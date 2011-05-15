/*
 * assert.c
 *
 * Johan Bevemyr.....Tue Nov 12 1991
 *                   Fri Jul  3 1992
 *
 *
 * Interface:      
 *
 *   match_term()
 *   compile_clause()
 *
 *
 * Implementation:
 *
 *   One way of representing an asserted clause would be to add code for
 * the predicate clause/2. For example, if foo(bar) :- true is added to the
 * database then code for clause(foo(bar),true) could be added. This way we
 * would use existing WAM-instructins for performing the head unification of
 * clause/2. A more efficient method is to write a specialized emulator that
 * only executes the head unification instructions needed by that clause/2. 
 *   A potential problem with this approach is that the Prolog compiler has to
 * be invoked to compile the new clause/2 clause each time a clause is asserted.
 * This is probably far to slow. It would be much better if we could write a
 * simple clause-compiler in C. However, this is not trivial since compiling
 * head unification is rather complicated, mainly due to register allocation. 
 *   Our solution is to design a new set of instructions which are easier to
 * compile to, and which does not require register allocation. This is achived
 * by performing the head unification in depth-first order, pushing "structure-
 * return-addresses" on a stack. A simple one-pass compiler can be written in C
 * for this instruction set.
 *
 * Potential problems: 
 *
 * 1. No heap overflow check is performed. An equivalent of the Require_Using
 *    instruction could be added to repair this problem.
 *
 * 2. Circular structures cannot be handled. The result is that compile_clause()
 *    loops. This problem can be *detected* by marking structures that have
 *    already seen. In order to handle circular structures an altogether
 *    different way of representing dynamic clauses is probably needed. 
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */

#include "luther.h"
#include "bignum.h"
#include "unify.h"


/*
 * Note: This file use the same macros as engine.c, but newer with threaded
 *       code emulation.
 */
#ifdef THREADED_CODE 
#undef THREADED_CODE
#endif

#include "engine.h"
#include "assert.h"

/*******************************************************************************
 *
 * Private (static) prototypes
 *
 */
static PROTOTYPE(int, compile_head, (TAGGED, int, int *, worker *));
static PROTOTYPE(int, compile_head_str, (TAGGED *, int, BOOL, int *, worker *));


/*******************************************************************************
 *
 *   Call match_term with a WAM environment (worker) and a pointer to the code
 * that describe the dynamic clause. 
 *
 * match_term() is called by luther_clause & luther_clause_noref to perform
 * the head unification of clause. It implements a simplified WAM emulator with
 * a slightly different instruction set (see above). 
 *
 *   We use a push-down stack to keep track of the 'return-structure-address'.
 * When a substructure is unified (read or written) the s-register
 * (next-structure) is pushed on the stack. When the last argument of the
 * substructure has been unified the old s value is poped from the stack.
 *
 * Note: The WAM environment is modified.
 * 
 */

LOCAL TAGGED pdstack[1024];


BOOL match_term(Arg,pc)
    Argdecl;
    register code *pc;
{
  register uword instruction;
  register TAGGED  k;			/* number of arguments in substructrue */
  register TAGGED *s;			/* argument-in-structure pointer       */
  register TAGGED *heap_top;		/* heap top pointer                    */
  register TAGGED *cont;		/* push down stack top pointer         */
  register TAGGED *areg = w->regs;	/* X-register pointer                  */ 
  
  
  /* The w->heap_top pointer is cashed in a register.
   */
  LoadCache(H);
  
  /*   Starting the emulator loop (in read mode). We use two different jump
   * tables (switch structures) to keep track of read and write mode.
   */
 read_start:
  {
    /*   Fetch the next instruction word. The opcode is stored in the lower 8
     * bits of the word. Register information is kept in the other bits.
     * Constants etc are stored in consecutive words.
     */

    instruction = Get_Code(pc);

    switch(Get_Instr(instruction)) {

    case A_GET_X_VARIABLE:
    a_get_x_variable:
      {
	register indx i,n;
	
	n = Get_Index_I_M(1,instruction);
	i = Get_Index_I(2,instruction);
	
	X(n) = X(i);
      }
      goto read_start;

    case A_GET_X_VALUE:
    a_get_x_value:
      {
	register indx i,n;
	
	n = Get_Index_I_M(1,instruction);
	i = Get_Index_I(2,instruction);
	
	Unify(X(n),X(i));
      }
      goto read_start;

    case A_GET_CONSTANT:
    a_get_constant:
      {
	register TAGGED c, Xi;
	register BOOL equal;
	register indx i;
	
	c = Get_Tagged(pc); 
	i = Get_Index_I(1,instruction);
	
	Xi = X(i);
	DerefLockSwitch(Xi, 
			{
			  Bind(Xi,c,{goto fail;});
			},
			{
			  if (IsBIG(Xi) && IsBIG(c))
			    equal = bignum_eq(GetBignum(Xi),GetBignum(c));
			  else
			    equal = Xi == c ? TRUE : FALSE;
			    
			  if (not(equal))
			    goto fail;
			});
      }
      goto read_start;

    case A_READ_LIST_TOP:
    a_read_list_top:
      {
	register indx i;
	register TAGGED Xi, lst;

	AInitCont;

	i = Get_Index_I_M(1,instruction);
	Xi = X(i);

	/*
	 *   If Xi is a variable then all substructures should written on
	 * the heap and Xi bound to a pointer to the topmost structure.
	 * If Xi is not a variable then it should be unfied with a list.
	 * This list does not appear in any other structure which means that
	 * the 'return-address' (s) does not have to be pushed on the stack
	 * as we unify the arguments of the list.
	 */
#if defined(PARALLEL)
	/*
	 *   In the parallel case the entire structure has to be built
	 * before Xi can be bound to it. Otherwise another processor might
	 * have access to an incomplete structure. We therefore push both
	 * the variable and the list pointer on the stack. These are unified
	 * when the emulator return to read mode (either by executing a *_up
	 * instruction or an unlock instruction).
	 */
	DerefLockSwitch(Xi,
			{
			  k = Get_Index_I(2,instruction);
			  Make_LST_S(H,s,lst);
			  PushCont(Xi);
			  PushCont(lst);
			  goto write_start;
			},
			{
			  if (IsLST(Xi))
			    {
			      s = GetCar(Xi);
			      goto read_start;
			    }
			  else
			    goto fail;
			});
#else  /* not PARALLEL */
	DerefLockSwitch(Xi,
			{
			  k = Get_Index_I(2,instruction);
			  Make_LST_S(H,s,lst);
			  Bind(Xi,lst,{goto fail;});
			  goto write_start;
			},
			{
			  if (IsLST(Xi))
			    {
			      s = GetCar(Xi);
			      goto read_start;
			    }
			  else
			    goto fail;
			});
#endif /* PARALLEL */
      }

    case A_READ_STRUCT_TOP:
    a_read_struct_top:
      {
	register TAGGED Xi, str, f;
	register indx i;

	AInitCont;

	i = Get_Index_I_M(1,instruction);
	f = Get_Functor(pc);
	
	Xi = X(i);

	/*
	 *   If Xi is a variable then all substructures should writen on the
	 * heap and Xi bound to a pointer to the topmost structure. If Xi is
	 * not a variable then it should be unfied with a list. This list does
	 * not appear in another which means that the 'return-address' (s) does 
	 * not have to be pushed on the stack as we unify the arguments of the
	 * structure.
	 */
#if defined(PARALLEL)
	/*
	 *   In the parallel case the entire structure has to be built before
	 * Xi can be bound to it. Otherwise another processor might have access
	 * to an incomplete structure. We therefore push both the variable and 
	 * the structure pointer on the stack. These are unified when the
	 * emulator return to read mode (either by executing a *_up or unlock
	 * instruction).
	 */
	DerefLockSwitch(Xi,
			{
			  register indx n;
			  k = Get_Index_I(2,instruction);
			  Make_STR_Alloc(H,s,str,f);
			  PushCont(Xi);
			  PushCont(str);
			  goto write_start;
			},
			{
			  if (IsSTR(Xi))
			    {
			      if (GetFunctor(Xi) == f)
				{
				  s = GetArg(Xi,0);
				  goto read_start;
				}
			      else
				goto fail;
			    }
			  else
			    goto fail;
			});
#else  /* not PARALLEL */
	DerefLockSwitch(Xi,
			{
			  k = Get_Index_I(2,instruction);
			  Make_STR_Alloc(H,s,str,f);
			  Bind(Xi,str,{goto fail;});
			  goto write_start;
			},
			{
			  if (IsSTR(Xi))
			    {
			      if (GetFunctor(Xi) == f)
				{
				  s = GetArg(Xi,0);
				  goto read_start;
				}
			      else
				goto fail;
			    }
			  else
			    goto fail;
			});
#endif /* PARALLEL */
      }

    case A_READ_LIST:
      {
	register TAGGED Ds, lst;
	  
	Ds = Ref(s);

	/* Move s to the next element in the structure.
	 */
	s += VARSIZE;

	/*
	 *   If the current structure argument (Ds) is a variable then the
	 * substructure should be constructed and Ds bound to a pointer to
	 * the topmost structure. This is done by entering write mode.
	 * The current value of s is pushed on the stack.
	 */
#if defined(PARALLEL)
	/*
	 *   In the parallel case the entire structure has to be built before
	 * Xi can be bound to it. Otherwise another processor might have access
	 * to an incomplete structure. We therefore push both the variable and 
	 * the structure pointer on the stack. These are unified when the
	 * emulator return to read mode (either by executing a *_up or unlock
	 * instruction).
	 */
	DerefLockSwitchHVA(Ds,
			   {
			     k = Get_Index_I(1,instruction);
			     PushCont(s);
			     Make_LST_S(H,s,lst);
			     PushCont(Ds);
			     PushCont(lst);
			     goto write_start;
			   },
			   {
			     if (IsLST(Ds))
			       {
				 PushCont(s);
				 s = GetCar(Ds);
				 goto read_start;
			       } 
			     else
			       goto fail;
			   });
#else  /* not PARALLEL */
	DerefLockSwitchHVA(Ds,
			   {
			     k = Get_Index_I(1,instruction);
			     PushCont(s);
			     Make_LST_S(H,s,lst);
			     Bind(Ds,lst, { goto fail; });
			     goto write_start;
			   },
			   {
			     if (IsLST(Ds))
			       {
				 PushCont(s);
				 s = GetCar(Ds);
				 goto read_start;
			       } 
			     else
			       goto fail;
			   });
#endif /* PARALLEL */
      }

    case A_READ_STRUCT:
      {
	register TAGGED f, Ds, str;

	f = Get_Functor(pc);

	/* We don't want to change the value of *s when we dereference
	 */
	Ds = Ref(s);

	/* Update the structure pointer (move it to the next argument).
	 */
	s += VARSIZE;

#if defined(PARALLEL)
	/* 
	 *   If *s is a variable then it should not be bound until we have
	 * constructed the structure it should be bound to. Therefore, we
	 * save it on the push-down stack.
	 */
	DerefLockSwitchHVA(Ds,
			   {
			     k = Get_Index_I(1,instruction);
			     PushCont(s);
			     Make_STR_Alloc(H,s,str,f);
			     PushCont(Ds);
			     PushCont(str);
			     goto write_start;
			   },
			   {
			     if (IsSTR(Ds))
			       {
				 PushCont(s);
				 if (GetFunctor(Ds) == f)
				   {
				     s = GetArg(Ds,0);
				     goto read_start;
				   }
			       }
			   });
#else  /* not PARALLEL */
	DerefLockSwitchHVA(Ds,
			   {
			     k = Get_Index_I(1,instruction);
			     PushCont(s);
			     Make_STR_Alloc(H,s,str,f);
			     Bind(Ds,str,{goto fail;});
			     goto write_start;
			   },
			   {
			     if (IsSTR(Ds))
			       {
				 PushCont(s);
				 if (GetFunctor(Ds) == f)
				   {
				     s = GetArg(Ds,0);
				     goto read_start;
				   }
			       }
			   });
#endif /* PARALLEL */			  
      }
      goto fail;

    case A_READ_LIST_TAIL:
      {
	register TAGGED Ds, lst;
	
	Ds = Ref(s);

	/* Move s to next argument.
	 */
	s += VARSIZE;

#if defined(PARALLEL)
	/* 
	 *   In the parallel case *s cannot be bound to a structure until it
	 * has been fully created (as above). Therefore it is save on the stack
	 * until such is the case.
	 *   It may seem that we loose some of the advantages of tail
	 * optimization since we have to save two pointers on the stack if *s
	 * is a variable. This is not to bad since this only happens once
	 * (during the transaction form read mode to write mode). 
	 */
	DerefLockSwitchHVA(Ds,
			   {
			     k = Get_Index_I(1,instruction);
			     Make_LST_S(H,s,lst);
			     PushCont(Ds);
			     PushCont(lst);
			     goto write_start;
			   },
			   {
			     if (IsLST(Ds))
			       {
				 s = GetCar(Ds);
				 goto read_start;
			       }
			     else
			       goto fail;
			   });
#else  /* not PARALLEL */
	DerefLockSwitchHVA(Ds,
			   {
			     k = Get_Index_I(1,instruction);
			     Make_LST_S(H,s,lst);
			     Bind(Ds,lst,{goto fail;});
			     goto write_start;
			   },
			   {
			     if (IsLST(Ds))
			       {
				 s = GetCar(Ds);
				 goto read_start;
			       }
			     else
			       goto fail;
			   });
#endif /* PARALLEL */
      }

    case A_READ_STRUCT_TAIL:
      {
	register TAGGED f, Ds, str;
	
	f = Get_Functor(pc);
	
	Ds = Ref(s);
	  
	/* Move s to the next argument in the structure
	 */
	s += VARSIZE;

#if defined(PARALLEL)
	/* 
	 *   In the parallel case *s cannot be bound to a structure until it
	 * has been fully created (as above). Therefore it is save on the stack
	 * until such is the case.
	 *   It may seem that we loose some of the advantages of tail
	 * optimization since we have to save two pointers on the stack if *s
	 * is a variable. This is not to bad since this only happens once
	 * (during the transaction form read mode to write mode). 
	 */
	DerefLockSwitchHVA(Ds,
			   {
			     k = Get_Index_I(1,instruction);
			     Make_STR_Alloc(H,s,str,f);
			     PushCont(Ds);
			     PushCont(str);
			     goto write_start;
			   },
			   {
			     if (IsSTR(Ds))
			       {
				 if (GetFunctor(Ds) == f)
				   {
				     s = GetArg(Ds,0);
				     goto read_start;
				   }
			       }
			   });
			  
#else  /* not PARALLEL */
	DerefLockSwitchHVA(Ds,
			   {
			     k = Get_Index_I(1,instruction);
			     Make_STR_Alloc(H,s,str,f);
			     Bind(Ds,str,{goto fail;});
			     goto write_start;
			   },
			   {
			     if (IsSTR(Ds))
			       {
				 if (GetFunctor(Ds) == f)
				   {
				     s = GetArg(Ds,0);
				     goto read_start;
				   }
			       }
			   });
#endif /* PARALLEL */			  
      }
      goto fail;

    case A_UNIFY_CONSTANT_UP:
      {
	register TAGGED c, Si;
	register BOOL equal;

	c = Get_Tagged(pc);
	
	Si = Ref(s);
	DerefLockSwitchHVA(Si,
			   {
			     Bind_Heap(Si,c,{goto fail;});
			   },
			   {
			     if (IsBIG(Si) && IsBIG(c))
			       equal = bignum_eq(GetBignum(Si),GetBignum(c));
			     else
			       equal = Si == c ? TRUE : FALSE;

			     if (not(equal))
			       goto fail;
			   });
			  
	s = PopCont;
      }
      goto read_start;

    case A_UNIFY_X_VARIABLE_UP:
      {
	register indx n;
	n = Get_Index_I(1,instruction);
      
	X(n) = Ref(s);
	s = PopCont;
      }
      goto read_start;

    case A_UNIFY_X_VALUE_UP:
      {
	register indx n;
	n = Get_Index_I(1,instruction);
	
	Unify(Ref(s),X(n));
	s = PopCont;
      }
      goto read_start;

    case A_UNIFY_X_LOCAL_VALUE_UP:
      {
	register indx n;
	n = Get_Index_I(1,instruction);
	
	Unify(X(n),Ref(s));
      }
      goto read_start;

    case A_UNIFY_VOID_UP:
      {
	s = PopCont;
      }
      goto read_start;

    case A_UNIFY_VOID:
      {
	s += Get_Index_I(1,instruction);
      }    
      goto read_start;

    case A_UNIFY_X_VARIABLE:
      {
	register indx n;
	n = Get_Index_I(1,instruction);
	
	X(n) = Ref(s);
	s += VARSIZE;
      }
      goto read_start;

    case A_UNIFY_X_VALUE:
      {
	register indx n;
	n = Get_Index_I(1,instruction);
	
	Unify(Ref(s),X(n));
	s += VARSIZE;
      }
      goto read_start;

    case A_UNIFY_X_LOCAL_VALUE:
      {
	register indx n;
	n = Get_Index_I(1,instruction);
	
	Unify(X(n),Ref(s));
	s += VARSIZE;
      }
      goto read_start;

    case A_UNIFY_CONSTANT:
      { 
	register TAGGED c, Si;
	register BOOL equal;
	
	c = Get_Tagged(pc);
	
	Si = Ref(s);
	s += VARSIZE;
	DerefLockSwitchHVA(Si,
			   {
			     Bind_Heap(Si,c,{goto fail;});
			   },
			   {
			     if (IsBIG(Si) && IsBIG(c))
			       equal = bignum_eq(GetBignum(Si),GetBignum(c));
			     else
			       equal = Si == c ? TRUE : FALSE;

			     if (not(equal))
			       goto fail;
			   });
      }
      goto read_start;

#if defined(PARALLEL)
    case A_UNLOCK:
      goto read_start;
#endif

    case A_DONE:
      {
	SaveCache(H);
      }
      return TRUE;

    default:
      FatalError("match_term: switch out of range");
    }
  }

 write_start:
  {
    instruction = Get_Code(pc);

    switch(Get_Instr(instruction)) {

    case A_GET_X_VARIABLE:
      goto a_get_x_variable;
    case A_GET_X_VALUE:
      goto a_get_x_value;
    case A_GET_CONSTANT:
      goto a_get_constant;
    case A_READ_LIST_TOP:
      goto a_read_list_top;
    case A_READ_STRUCT_TOP:
      goto a_read_struct_top;

    case A_READ_LIST:
      {
	PushCont(s+VARSIZE);
	Make_LST_S(H,s,*s);
      }
      goto write_start;

    case A_READ_STRUCT:
      {
	register TAGGED f;

	f = Get_Functor(pc);

	PushCont(s+VARSIZE);

	Make_STR_Alloc(H,s,*s,f);
      }
      goto write_start;

    case A_READ_LIST_TAIL:
      {
#if defined(PARALLEL)
	if (k <= 0)
	  {
	    register TAGGED x,y,lst;
	    x = (TAGGED) PopCont;
	    y = (TAGGED) PopCont;
	    Bind(y,x,{goto fail;});
	    InitHVA(s,x,w);
	    Make_LST_S(H,s,lst);
	    PushCont(x);
	    PushCont(lst);
	  }
	else 
#endif
	  {
	    Make_LST_S(H,s,*s);
	  }
      }
      goto write_start;

    case A_READ_STRUCT_TAIL:
      {
	register TAGGED f;

	f = Get_Functor(pc);

#if defined(PARALLEL)
	if (k <= 0)
	  {
	    register TAGGED x,y,str;
	    x = (TAGGED) PopCont;
	    y = (TAGGED) PopCont;
	    Bind(y,x,{goto fail;});
	    InitHVA(s,x,w);
	    Make_STR_Alloc(H,s,str,f);
	    PushCont(x);
	    PushCont(str);
	  }
	else 
#endif
	  {
	    Make_STR_Alloc(H,s,*s,f);
	  }
      }
      goto write_start;

    case A_UNIFY_CONSTANT_UP:
      { 
	register TAGGED c;
	  
	c = Get_Tagged(pc);
	  
	*(s) = c;

	ADispatchUp(k--);
      }
	
    case A_UNIFY_X_VARIABLE_UP:
      {
	register indx n;

	n = Get_Index_I(1,instruction);

	LoadHVA(s,X(n),w);

	ADispatchUp(k--);
      }

    case A_UNIFY_X_VALUE_UP:
      {
	register indx n;

	n = Get_Index_I(1,instruction);

	*(s) = X(n);

	ADispatchUp(k--);
      }

    case A_UNIFY_X_LOCAL_VALUE_UP:
      {
	register indx n;
	
	n = Get_Index_I(1,instruction);
	
	WriteLocalValueX(X(n),s);

	ADispatchUp(k--);
      }

    case A_UNIFY_VOID_UP:
      {
	register indx n;

	n = Get_Index_I(1,instruction); 

	do {CreateHVA(s,w);} while(--n);

	ADispatchUp(k--);
      }

    case A_UNIFY_VOID:
      {
	register indx n;

	n = Get_Index_I(1,instruction); 

	do {CreateHVA(s,w);} while(--n);
      }
      goto write_start;

    case A_UNIFY_X_VARIABLE:
      {
	register indx n;

	n = Get_Index_I(1,instruction);

	LoadHVA(s,X(n),w);
      }
      goto write_start;

    case A_UNIFY_X_VALUE:
      {
	register indx n;
	n = Get_Index_I(1,instruction);
	PushOnHeap(s,X(n));
      }
      goto write_start;

    case A_UNIFY_X_LOCAL_VALUE:
      {
	register indx n;
	
	n = Get_Index_I(1,instruction);
	
	WriteLocalValueX(X(n),s);
      }
      goto write_start;

    case A_UNIFY_CONSTANT:
      {
	register TAGGED c;
	
	c = Get_Tagged(pc);
	
	PushOnHeap(s,c);
      }
      goto write_start;

#if defined(PARALLEL)
    case A_UNLOCK:
      {
	register TAGGED x,y;
	x = (TAGGED) PopCont;
	y = (TAGGED) PopCont;
	Bind(y,x,{goto fail;});
      }
      goto write_start;
#endif

    case A_DONE:
      {
	SaveCache(H);
      }
      return TRUE;

    default:
      FatalError("match_term: switch out of range");
    }
  }

 fail:
  {
    SaveCache(H);
    return FALSE;
  }
}


/*******************************************************************************
 *
 *   compile_clause() takes a (TAGGED pointer to) Head and Body and creates
 * the corresponding code in the code area (thus modifying the codearea),
 * i.e. code which performs head unification of clause(Head, Body).
 *   On success, a pointer to the generated code is returned, otherwise NULL
 * is returned. The generated code is inteded to be emulated by the emulator
 * above (match_term()).
 *
 *   In order to detect that a variable has occured earlier in Head or Body
 * the 'gc-mark-bit' is set in each reached variable. Also, the variable is
 * (re)tagged as an integer. The integer value field holds the X-register that
 * the variable is stored in. Thus, if an gc-marked integer is encountered it
 * should be viewed as a variable seen previously, and which is stored in the
 * register indicated by the integer.
 *   To restore the variables after compilation all bindings are recorded on
 * the trail. These temporary variable bindings are undone by the UndoForward()
 * macro at the end of the compilation.
 *
 * Note: Used by luther_assert_delete_other, luther_asserta and luther_assertz.
 *
 */
code *compile_clause(Head,Body,w,Name,Module)
     TAGGED Head, Body,*Name,*Module;
     worker *w;
{
  u32 saved_trailtop_offset;
  code *start_of_pred;
  int freg;

  /*   First find out the name and module of the clause. If there is a module
   * then the module declaration should be stripped before code is generated.
   */
  *Module = w->global->current_module;

 start:

  if (IsSTR(Head))
    {
      *Name = GetFunctor(Head);
      if (*Name == functor_colon)
	{
	  DerefNLL(*Module,Ref(GetArg(Head,0)));
	  DerefNLL(Head,Ref(GetArg(Head,1)));
	  goto start;
	}
    }
  else if (IsATM(Head))
    {
      *Name = StoreFunctor(Head,0);
    }
  else
    {
      return NULL;
    }

  /* 
   * Save start of new code. It is returned as result.
   */

  if (w->global->code_current > w->global->code_limit)
    {
      extend_codespace(w);
    }

  start_of_pred = w->global->code_current;

  if (start_of_pred > w->global->code_end)
    {
      FatalError("code space exhausted");
    }

  /* 
   * We save the current trailtop. All bindings done during compilation
   * must be undone before we return.
   */

  saved_trailtop_offset = w->trail_top-w->trail_start;

  /* 
   * freg holds the first free register. Initially only X0 and X1 are
   * occupied.
   */

  freg = 2;

  /* 
   * Compile Head and Body
   */
  if (compile_head(Head, 0, &freg,w) == -1)
    {
      goto barf;
    }
  else if (compile_head(Body, 1, &freg,w) == -1)
    {
      goto barf;
    }

  EmitOp(A_DONE);

  /* 
   * Undo all bindings done to keep track of in which register 
   * a variable is stored 
   */

  UndoForward(w->trail_start + saved_trailtop_offset);
  return start_of_pred;

 barf:
  UndoForward(w->trail_start + saved_trailtop_offset);
  return (code *) NULL;
}

/***********************************************************************
 * This function performs the compilation. It differs from 
 * compile_head_str() in that A_READ_LIST_TOP and A_READ_STRUCT_TOP
 * are generated instead of A_READ_LIST and READ_STRUCT
 *
 * Returns: -1 (on failure)
 *           0 (on success)
 *
 * Modifies: the code area
 *
 */

static int compile_head(term, reg, freg,w)
     TAGGED term; /* The term to compile       */
     int reg;     /* The register it reside in */
     int *freg;   /* First unused register     */
     worker *w;   /* WAM environment (worker)  */
{

  DerefNLL(term,term);

  switch(TagOf(term))
    {
    case HVA:
#if defined(CONSTR)
    case CVA:
      Error("compile_head: can't compile CVA");
      return -1;
#endif
    case SVA:
      {
	indx n;

	/* 
	 * If we could be sure that this argument never occured again
	 * we could simply return, but since we might have clause(X,X)...
	 *
	 */
	n = NextFreeReg(freg);

	EmitOp2(A_GET_X_VARIABLE,n,reg);

	SetForward(term,n);

	return 0;
      }
    case NUM:
      if (IsForward(term))
	{
	  /* This is actually a variable that has been seen earlier.
           * Generate get_x_value to unify this term with the earlier
	   * occurance.
	   */
	  EmitOp2(A_GET_X_VALUE,reg,GetForward(term));
	  return 0;
	}
      else
	{
	  EmitOp1(A_GET_CONSTANT,reg);
	  EmitNumber(term);
	  return 0;
	}

    case BOX:
      if (IsFLT(term))
	{
	  EmitOp1(A_GET_CONSTANT,reg);
	  EmitUnsafeFLT(term);
	}
      else
	{
	  EmitOp1(A_GET_CONSTANT,reg);
	  EmitUnsafeBIG(term);
	}
      return 0;

    case ATM:
      EmitOp1(A_GET_CONSTANT,reg);
      EmitAtom(term);
      return 0;

    case LST:
      {
	code *this_instr = w->global->code_current;
	int nr_substructures;

	EmitOp1(A_READ_LIST_TOP,reg);
	
	nr_substructures = compile_head_str(GetCar(term),2,FALSE,freg,w);

	/* 
	 * Propagate error upward.
	 */
	if (nr_substructures == -1) return -1;

	ZapIndex(this_instr,2,nr_substructures);

#if defined(PARALLEL)
	EmitOp(A_UNLOCK);
#endif

	/* 
	 * Report success.
	 */
	return 0;
      }
	  
    case STR:
      {
	code *this_instr = w->global->code_current;
	int number_of_substr;
	TAGGED f;

	EmitOp1(A_READ_STRUCT_TOP,reg);
	f = GetFunctor(term);
	EmitFunctor(f);

	number_of_substr=compile_head_str(GetArg(term,0),ArityOf(f),FALSE,
					  freg,w);
	
	/* 
	 * Propagate error upward.
	 */
	if (number_of_substr == -1) return -1;

	ZapIndex(this_instr,2,number_of_substr);

#if defined(PARALLEL)
	/* In the parallel case we might need to unify a variable and a structure.
         */
	EmitOp(A_UNLOCK);
#endif

	/* 
	 * Return success.
	 */
	return 0;
      }
      
    case GEN:
      Error("compile_head: can't compile generic object");
      return -1;

    default:
      Error("compile_head: switch out of range");
      return -1;
    }
}


/**********************************************************************
 * This function generates code for unifying substructures. It returns
 * the number of substructures (*_up instructions) in the generated code.
 * Tail structures are not counted, e.g., foo(bar(bas(a)),a) would
 * return 1 since only bar/1 is a non-tail structure. 
 *
 * Returns: -1                      (failure)
 *          number of substructures (success)
 * 
 * Modifies: the code area
 *
 */

static int compile_head_str(term,arity,nontail,freg,w)
    TAGGED *term;  /* pointer to first argument of structure  */
    int arity;     /* number of arguments in structure        */
    BOOL nontail;  /* flag for compiling a tail argument      */
    int *freg;     /* first free register                     */
    worker *w;     /* WAM environment (worker)                */
{
    TAGGED t;              /* dereferenced argument   */
    int nr_substructures;  /* number of substructures */

    if (w->global->code_current > w->global->code_end)
      {
	FatalError("code space exhausted");
      }


    nr_substructures = 0;

    while((arity--) > 1)
      {
        DerefNLL(t,Ref(term));
	term += VARSIZE;

	switch(TagOf(t))
	  {
	  case HVA:
#if defined(CONSTR)
	    {
	      indx n;
	      n = NextFreeReg(freg);
	      EmitOp1(A_UNIFY_X_VARIABLE,n);
	      SetForward(t,n);
	      break;
	    }
	  case CVA:
	    Error("compile_head_str: cant compile CVA");  
	    return -1;
#endif
	  case SVA:
	    {
	      indx n;
	      n = NextFreeReg(freg);
	      EmitOp1(A_UNIFY_X_VARIABLE,n);
	      SetForward(t,n);
	      break;
	    }

	  case NUM:
	    if (IsForward(t))
	      {
		EmitOp1(A_UNIFY_X_VALUE,GetForward(t));
		break;
	      }
	    else
	      {
		EmitOp(A_UNIFY_CONSTANT);
		EmitNumber(t);
		break;
	      }

	  case BOX:
	    if (IsFLT(t))
	      {
		EmitOp(A_UNIFY_CONSTANT);
		EmitUnsafeFLT(t);
	      }
	    else
	      {
		EmitOp(A_UNIFY_CONSTANT);
		EmitUnsafeBIG(t);
	      }
	    break;

	  case ATM:
	    EmitOp(A_UNIFY_CONSTANT);
	    EmitAtom(t);
	    break;

	  case LST:
	    {
	      code *this_instr = w->global->code_current;
	      int substructures;

	      EmitOp(A_READ_LIST);

	      substructures = compile_head_str(GetCar(t),2,TRUE,freg,w);

	      /* 
	       * Propagate error upward.
	       */
	      if (substructures == -1) return -1;
	    
	      ZapIndex(this_instr,1,substructures);

	      nr_substructures += substructures + 1;
	    
	      break;
	    }

	  case STR:
	    {
	      code *this_instr = w->global->code_current;
	      int substructures;
	      TAGGED f;

	      EmitOp(A_READ_STRUCT);
	      f = GetFunctor(t);
	      EmitFunctor(f);

	     substructures = compile_head_str(GetArg(t,0),ArityOf(f),TRUE,
					      freg,w);

	      /* 
	       * Propagate error upward.
	       */
	      if (substructures == -1) return -1;
	    
	      ZapIndex(this_instr,1,substructures);

	      nr_substructures += substructures + 1;
	    
	      break;
	    }
	  case GEN:
	    Error("compile_head_str: can't compile generic objects");
	    return -1;

	  default:
	    Error("compile_head_str: no such term type");
	    return -1;
	  }
      }

    /* 
     * Compile the last argument of the structure. If this argument
     * is a structure there is no need to push a return address on 
     * the stack. And, if the current structure appeared as last 
     * argument there is no need to pop a structure (generate a *_up
     * instruction).
     */

    DerefNLL(t,Ref(term));
    
    switch(TagOf(t))
      {
      case HVA:
#if defined(CONSTR)
	{
	  indx n;
	  n = NextFreeReg(freg);

	  if (nontail==TRUE)
	    {
	      EmitOp1(A_UNIFY_X_VARIABLE_UP,n);
	    }
	  else
	    {
	      EmitOp1(A_UNIFY_X_VARIABLE,n);
	    }
	  SetForward(t,n);
	  break;
	}
      case CVA:
	Error("compile_head_str: can't compile CVA");
	return -1;
#endif
      case SVA:
	{
	  indx n;
	  n = NextFreeReg(freg);

	  if (nontail==TRUE)
	    {
	      EmitOp1(A_UNIFY_X_VARIABLE_UP,n);
	    }
	  else
	    {
	      EmitOp1(A_UNIFY_X_VARIABLE,n);
	    }
	  SetForward(t,n);
	  break;
	}

      case NUM:
	if (IsForward(t))
	  {
	    if (nontail==TRUE)
	      {
		EmitOp1(A_UNIFY_X_VALUE_UP,GetForward(t));
	      }
	    else
	      {
		EmitOp1(A_UNIFY_X_VALUE,GetForward(t));
	      }
	    break;
	  }
	else
	  {
	    if (nontail==TRUE)
	      {
		EmitOp(A_UNIFY_CONSTANT_UP);
	      }
	    else
	      {
		EmitOp(A_UNIFY_CONSTANT);
	      }
	    EmitNumber(t);
	    break;
	  }
	
      case BOX:
	{
	  if (nontail==TRUE)
	    {
	      EmitOp(A_UNIFY_CONSTANT_UP);
	    }
	  else
	    {
	      EmitOp(A_UNIFY_CONSTANT);
	    }

	  if (IsFLT(t))
	    {
	      EmitUnsafeFLT(t);
	    }
	  else
	    {
	      EmitUnsafeBIG(t);
	    }
	}
	break;
	
      case ATM:
	if (nontail==TRUE)
	  {
	    EmitOp(A_UNIFY_CONSTANT_UP);
	  }
	else
	  {
	    EmitOp(A_UNIFY_CONSTANT);
	  }
	EmitAtom(t);
	break;
	
      case LST:
	{
	  code *this_instr = w->global->code_current;
	  int substructures;
	  
	  EmitOp(A_READ_LIST_TAIL);
	  
	  substructures = compile_head_str(GetCar(t),2,nontail,freg,w);
	  
	  /* 
	   * Propagate error upward.
	   */
	  if (substructures == -1) return -1;
	  
	  ZapIndex(this_instr,1,substructures);
	  

	  /* 
	   * Do not add one in this case since we didn't push a 
	   * return pointer (this is a tail structure).
	   */
	  nr_substructures += substructures;
	  
	  break;
	}
	
      case STR:
	{
	  code *this_instr = w->global->code_current;
	  int substructures;
	  TAGGED f;
	  
	  EmitOp(A_READ_STRUCT_TAIL);
	  f = GetFunctor(t);
	  EmitFunctor(f);
	  
	  substructures = compile_head_str(GetArg(t,0),ArityOf(f),nontail,
					   freg,w);
	  
	  /* 
	   * Propagate error upward.
	   */
	  if (substructures == -1) return -1;
	  
	  ZapIndex(this_instr,1,substructures);
	  
	  /* 
	   * Do not add one in this case since we didn't push a 
	   * return pointer (this is a tail structure).
	   */
	  nr_substructures += substructures;
	  
	  break;
	}

      case GEN:
	Error("compile_head_str: can't compile generic objects");
	return -1;

      default:
	Error("compile_head_str: switch out of range");
	return -1;
      }

    return nr_substructures;
}
	
