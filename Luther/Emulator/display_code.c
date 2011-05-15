/*
 * display_code.c
 *
 * Johan Bevemyr.....Wed Jun  5 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"
#include "engine.h"
#include "display_code.h"
#include "inline.h"
#include "bignum.h"
#include "debug.h"


#define ResetIndex      index_nr = 1

#define PrintSmallIndex					\
{							\
  int index;						\
  Get_Small_Index(index,instruction,index_nr,pc);	\
  index_nr++;						\
  DB_Print2("%d",index);				\
}

#if defined(THREADED_CODE)
#define Get_Small_Index(To,I,N,P) { To = Get_Index(P); }
#else
#define Get_Small_Index(To,I,N,P)		\
{						\
  if (N > MAX_INDEX)				\
    {						\
      N = 1;					\
      I = Get_Tagged(P);			\
    }						\
  To = Get_Instr((I) >> N*8);			\
}
#endif


#define PrintLabel(pc)					\
{							\
  int label = Get_Label(pc);				\
  if (label == 0)					\
    {							\
      DB_Print1("fail");				\
    }							\
  else							\
    {							\
      DB_Print2("%#lx", (long)(pc - 1) + (long)label);	\
    }							\
}

#define PrintIndex   { int index = Get_Index(pc);			   \
			  DB_Print2("%lu",index);}
#define PrintIndex1  { int index = Get_Index_I(1,instruction);		   \
			  DB_Print2("%d)",index); }
#define PrintIndex1n { int index = Get_Index_I_M(1,instruction);	   \
			  DB_Print2("%d,",index); }
#define PrintIndex2  { int index = Get_Index_I(2,instruction);		   \
			  DB_Print2("%d)",index); }
#define PrintIndex2n { int index = Get_Index_I(2,instruction);		   \
			  DB_Print2("%d,",index); }

#define PrintConstant							\
{									\
  TAGGED con = Get_Tagged(pc);						\
									\
  switch (TagOf(con)) {							\
									\
  case ATM:								\
    {									\
      DB_Print2("'%s'",GetString(con,w));				\
    }									\
    break;								\
									\
  case NUM:								\
    {									\
      DB_Print2("%d",GetNumber(con));					\
    }									\
    break;								\
									\
  case BOX:								\
    {									\
      if (IsFLT(con))							\
	{								\
	  DB_Print2("%f",(float) GetFloat(con));			\
	}								\
      else								\
	{								\
	  print_bignum(currerr, GetBignum(con), DECIMAL);		\
	}								\
    }									\
    break;								\
									\
  default:								\
    {									\
      DB_Print1("error: not a constant!");				\
    }									\
  }									\
}

#define PrintStructure							\
{									\
  TAGGED f = Get_Functor(pc);						\
  DB_Print3("'%s'/%d", GetString(FunctorToAtom(f),w),ArityOf(f));	\
}

void display_code(pc,w)
    code *pc;
    worker *w;
{
  (void) display_code_inc(pc,w);
}

code *display_code_inc(pc,w)
    code   *pc;
    worker *w;
{
  TAGGED instruction;
  register int index_nr;
  
  DB_Print2("%#-10lx : ", (uword) pc);

  InstToOp(instruction, Get_Code(pc));

  switch(Get_Instr(instruction)) {

  case DUMMY_INSTRUCTION:
    {
      DB_Print1("dummy_instruction");
    }
    break;

  case SWITCH_ON_TERM:
    {
      DB_Print1("switch_on_term(");
      PrintLabel(pc); DB_Print1(",");
      PrintLabel(pc); DB_Print1(",");
      PrintLabel(pc); DB_Print1(",");
      PrintLabel(pc); DB_Print1(",");
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case SWITCH_ON_CONSTANT:
    {
      int i, nr = Get_Index(pc);
      DB_Print2("switch_on_constant(%d,[", nr);

      for (i = 0; i != nr; i++)
	{
	  DB_Print1("[");
	  PrintConstant; DB_Print1(","); PrintLabel(pc); 
	  DB_Print1("]");
	  if (i != nr - 1) DB_Print1(",");
	}
      DB_Print1("],");
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case SWITCH_ON_STRUCTURE:
    {
      int i, nr = Get_Index(pc);
      DB_Print2("switch_on_structure(%d,[", nr);

      for (i = 0; i != nr; i++)
	{
	  DB_Print1("[");
	  PrintStructure; DB_Print1(","); PrintLabel(pc); 
	  DB_Print1("]");
	  if (i != nr - 1) DB_Print1(",");
	}
      DB_Print1("],");
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;
      
  case TRY:
    {
      DB_Print1("try("); 
      PrintIndex1n;
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case RETRY:
    {
      DB_Print1("retry("); 
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case TRUST:
    {
      DB_Print1("trust("); 
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case TRY_ME_ELSE:
    {
      DB_Print1("try_me_else("); 
      PrintIndex1n; 
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case RETRY_ME_ELSE:
    {
      DB_Print1("retry_me_else("); 
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case TRUST_ME:
    {
      DB_Print1("trust_me");
    }
    break;
      
  case CHOICE_X:
    {
      DB_Print1("choice_x(");
      PrintIndex1;
    }
    break;

  case CHOICE_Y:
    {
      DB_Print1("choice_y("); 
      PrintIndex1;
    }
    break;

  case CUT:
    {
      DB_Print1("cut");
    }
    break;

  case CUT_X:
    {
      DB_Print1("cut_x("); 
      PrintIndex1;
    }
    break;

  case CUT_Y:
    {
      DB_Print1("cut_y("); 
      PrintIndex1;
    }
    break;

    case INLINE:
      {
	uword func = Get_Index_I(1,instruction);
	
	natural arity = GetInlineArity(func);
	
	DB_Print3("inline('%s'/%d,", GetInlineName(func), arity);
	PrintLabel(pc);
	DB_Print1(",[");
	
	while (arity--)
	  {
	    PrintIndex;
	    if (arity != 0) DB_Print1(",");
	  }
	
	DB_Print1("])");
      }
    break;

  case BUILTIN:
      {
	uword func = Get_Index_I(1,instruction);
	
	natural arity = GetInlineArity(func);
	
	DB_Print3("builtin('%s'/%d,[", GetInlineName(func), arity);
	
	while (arity--)
	  {
	    PrintIndex;
	    if (arity != 0) DB_Print1(",");
	  }
	
	DB_Print1("])");
      }
    break;

  case META_CALL:
    {
      DB_Print1("meta_call("); 
      PrintIndex1n;
      PrintIndex2;
      Inc_Index(pc);
    }
    break;

  case META_EXECUTE:
    {
      DB_Print1("meta_execute("); 
      PrintIndex1;
    }
    break;
      
  case REQUIRE:
    {
      DB_Print1("require(");
      PrintIndex;
      DB_Print1(")");
    }
    break;
      
  case REQUIRE_USING:
    {
      int n = Get_Index_I(1,instruction);
      
      DB_Print1("require_using(");
      PrintIndex; 
      DB_Print2(",%d)", n);
    }
    break;

  case ALLOCATE:
    {
      DB_Print1("allocate"); 
    }
    break;

  case ALLOCATE2:
    {
      DB_Print1("allocate2");
    }
    break;

  case DEALLOCATE:
    {
      DB_Print1("deallocate");
    }
    break;

  case INIT:
    {
      natural init_list_size = Get_Index_I(1,instruction);
	
      DB_Print2("init(%d,[", init_list_size);
	
      while (init_list_size--)
	{
	  int index = Get_Index(pc);
	  DB_Print2("%d", index);
	  if (init_list_size != 0) DB_Print1(",");
	}

      DB_Print1("])");
    }
    break;
      
  case CALL:
      {
	natural env_size = Get_Index_I(2,instruction);
	definition *def = Get_Definition(pc);
	
	DB_Print4("call('%s'/%d,%d)",
		  GetString(FunctorToAtom(def->name),w),
		  ArityOf(def->name), env_size);
      }
    break;

  case EXECUTE:
    {
      definition *def = Get_Definition(pc);

      DB_Print3("execute('%s'/%d)",
		GetString(FunctorToAtom(def->name),w),
		ArityOf(def->name));
    }
    break;

  case PROCEED:
    {
      DB_Print1("proceed");
    }
    break;

  case FAIL:
    {
      DB_Print1("fail");
    }
    break;
      
  case GET_X_VARIABLE:
    {
      DB_Print1("get_x_variable(");
      PrintIndex1n;
      PrintIndex2;
    }
    break;

  case GET_Y_VARIABLE:
    {
      DB_Print1("get_y_variable(");
      PrintIndex1n;
      PrintIndex2;
    }
    break;

  case GET_Y_FIRST_VALUE:
    {
      DB_Print1("get_y_first_value(");
      PrintIndex1n;
      PrintIndex2;
    }
    break;

  case GET_X_VALUE:
    {
      DB_Print1("get_x_value(");
      PrintIndex1n;
      PrintIndex2;
    }
    break;

  case GET_Y_VALUE:
    {
      DB_Print1("get_y_value(");
      PrintIndex1n;
      PrintIndex2;
    }
    break;

  case GET_CONSTANT:
    {
      DB_Print1("get_constant(");
      PrintConstant; DB_Print1(","); 
      PrintIndex1;
    }
    break;

  case GET_NIL:
    {
      DB_Print1("get_nil(");
      PrintIndex1;
    }
    break;

  case GET_STRUCTURE:
    {
      DB_Print1("get_structure(");
      PrintStructure; DB_Print1(","); 
      PrintIndex1;
    }
    break;

  case GET_LIST:
    {
      DB_Print1("get_list(");
      PrintIndex1;
    }
    break;

  case GET_CONSTANT_X0:
    {
      DB_Print1("get_constant_x0(");
      PrintConstant;
      DB_Print1(")");
    }
    break;

  case GET_NIL_X0:
    {
      DB_Print1("get_nil_x0");
    }
    break;

  case GET_STRUCTURE_X0:
    {
      DB_Print1("get_structure_x0(");
      PrintStructure;
      DB_Print1(")");
    }
    break;

  case GET_LIST_X0:
    {
      DB_Print1("get_list_x0");
    }
    break;
      
  case PUT_X_VOID:
    {
      DB_Print1("put_x_void(");
      PrintIndex1;
    }
    break;

  case PUT_Y_VOID:
    {
      DB_Print1("put_y_void(");
      PrintIndex1;
    }
    break;

  case PUT_X_VARIABLE:
    {
      DB_Print1("put_x_variable(");
      PrintIndex1n;
      PrintIndex2;
    }
    break;

  case PUT_Y_VARIABLE:
    {
      DB_Print1("put_y_variable(");
      PrintIndex1n;
      PrintIndex2;
    }
    break;

  case PUT_X_VALUE:
    {
      DB_Print1("put_x_value(");
      PrintIndex1n;
      PrintIndex2;
    }
    break;

  case PUT_Y_VALUE:
    {
      DB_Print1("put_y_value(");
      PrintIndex1n;
      PrintIndex2;
    }
    break;

  case PUT_X_UNSAFE_VALUE:
    {
      DB_Print1("put_x_unsafe_value(");
      PrintIndex1n;
      PrintIndex2;
    }
    break;

  case PUT_Y_UNSAFE_VALUE:
    {
      DB_Print1("put_y_unsafe_value(");
      PrintIndex1n;
      PrintIndex2;
    }
    break;

  case PUT_CONSTANT:
    {
      DB_Print1("put_constant(");
      PrintConstant; DB_Print1(",");
      PrintIndex1;
    }
    break;

  case PUT_NIL:
    {
      DB_Print1("put_nil(");
      PrintIndex1;
    }
    break;

  case PUT_STRUCTURE:
    {
      DB_Print1("put_structure(");
      PrintStructure; DB_Print1(",");
      PrintIndex1;
    }
    break;

  case PUT_LIST:
    {
      DB_Print1("put_list(");
      PrintIndex1;
    }
    break;
      
  case UNIFY_VOID:
    {
      DB_Print1("unify_void(");
      PrintIndex1;
    }
    break;

  case UNIFY_X_VARIABLE:
    {
      DB_Print1("unify_x_variable(");
      PrintIndex1;
    }
    break;

  case UNIFY_Y_VARIABLE:
    {
      DB_Print1("unify_y_variable(");
      PrintIndex1;
    }
    break;

  case UNIFY_Y_FIRST_VALUE:
    {
      DB_Print1("unify_y_first_value(");
      PrintIndex1;
    }
    break;

  case UNIFY_X_VALUE:
    {
      DB_Print1("unify_x_value(");
      PrintIndex1;
    }
    break;

  case UNIFY_Y_VALUE:
    {
      DB_Print1("unify_y_value(");
      PrintIndex1;
    }
    break;

  case UNIFY_X_LOCAL_VALUE:
    {
      DB_Print1("unify_x_local_value(");
      PrintIndex1;
    }
    break;

  case UNIFY_Y_LOCAL_VALUE:
    {
      DB_Print1("unify_y_local_value(");
      PrintIndex1;
    }
    break;

  case UNIFY_CONSTANT:
    {
      DB_Print1("unify_constant(");
      PrintConstant; 
      DB_Print1(")");
    }
    break;

  case UNIFY_NIL:
    {
      DB_Print1("unify_nil");
    }
    break;

  case UNIFY_STRUCTURE:
    {
      DB_Print1("unify_structure(");
      PrintStructure;
      DB_Print1(")");
    }
    break;

  case UNIFY_LIST:
    {
      DB_Print1("unify_list");
    }
    break;

#if defined(JUMP_CALL)
    case CJUMP:
      {
	int  env = Get_Index_I(2,instruction);
	definition *def = Get_Definition(pc);

	DB_Print3("cjump(%lx,%d)", def, env);
      }
    break;

    case EJUMP:
      {
	definition *def = Get_Definition(pc);

	DB_Print2("ejump(%lx)", def);
      }
    break;
#endif JUMP_CALL
      
#if defined(BOUNDED_Q)
  case ZEROP:
    {
      DB_Print1("zerop(");
      PrintIndex1n;
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case LISTP:
    {
      DB_Print1("listp(");
      PrintIndex1n;
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case DETERMINISTIC:
    {
      DB_Print1("deterministic(");
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;
      
  case ALLOCATE_STAR:
    {
      DB_Print1("allocate*(");
      PrintIndex1n;
    }
    break;

  case REPEAT:
    {
      DB_Print1("repeat(");
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case ITERATE_INT:
    {
      DB_Print1("iterate_int(");
      PrintIndex1n; 
      PrintIndex2n;
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case ITERATE_INT_STAR:
    {
      DB_Print1("iterate_int_star(");
      PrintIndex1n;
      PrintIndex2n;
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case ITERATE_LIST:
    {
      DB_Print1("iterate_list(");
      PrintIndex1n;
      PrintIndex2n;
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case ITERATE_LIST_STAR:
    {
      DB_Print1("iterate_list_star(");
      PrintIndex1n;
      PrintIndex2n;
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case ENSURE_LIST_TRY:
    {
      DB_Print1("ensure_list_try(");
      PrintIndex1n;
      PrintIndex2n;
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;

  case ENSURE_LIST_TRUST:
    {
      DB_Print1("ensure_list_trust");
    }
    break;
#endif /* BOUNDED_Q */
      
#if defined(PARALLEL_BQ)
  case SPAWN_LEFT_BQ:
    {
      ResetIndex;
      DB_Print1("spawn_left_bq(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintLabel(pc);
      DB_Print1(")");
    }
    break;
#endif /* PARALLEL_BQ */      

#if defined(JUMP_CODE)
    case JUMP:
      DB_Print1("jump ");
      PrintLabel(pc);
      break;
#endif
      
#if defined(REFORM)
  case GLOBAL_GET_X_VALUE:
    {
      DB_Print1("global_get_x_value(");
      PrintIndex1n;
      PrintIndex2;
    }
    break;

  case GLOBAL_GET_Y_VALUE:
    {
      DB_Print1("global_get_y_value(");
      PrintIndex1n;
      PrintIndex2;
    }
    break;

  case GLOBAL_GET_CONSTANT:
    {
      DB_Print1("global_get_constant(");
      PrintConstant; DB_Print1(","); 
      PrintIndex1;
    }
    break;

  case GLOBAL_GET_NIL:
    {
      DB_Print1("global_get_nil(");
      PrintIndex1;
    }
    break;

  case GLOBAL_GET_STRUCTURE:
    {
      DB_Print1("global_get_structure(");
      PrintStructure; DB_Print1(","); 
      PrintIndex1;
    }
    break;

  case GLOBAL_GET_LIST:
    {
      DB_Print1("global_get_list(");
      PrintIndex1;
    }
    break;

  case GLOBAL_GET_CONSTANT_X0:
    {
      DB_Print1("global_get_constant_x0(");
      PrintConstant;
      DB_Print1(")");
    }
    break;

  case GLOBAL_GET_NIL_X0:
    {
      DB_Print1("global_get_nil_x0");
    }
    break;

  case GLOBAL_GET_STRUCTURE_X0:
    {
      DB_Print1("global_get_structure_x0(");
      PrintStructure;
      DB_Print1(")");
    }
    break;

  case GLOBAL_GET_LIST_X0:
    {
      DB_Print1("global_get_list_x0");
    }
    break;
      
  case GLOBAL_UNIFY_X_VALUE:
    {
      DB_Print1("global_unify_x_value(");
      PrintIndex1;
    }
    break;

  case GLOBAL_UNIFY_Y_VALUE:
    {
      DB_Print1("global_unify_y_value(");
      PrintIndex1;
    }
    break;

  case GLOBAL_UNIFY_X_LOCAL_VALUE:
    {
      DB_Print1("global_unify_x_local_value(");
      PrintIndex1;
    }
    break;

  case GLOBAL_UNIFY_Y_LOCAL_VALUE:
    {
      DB_Print1("global_unify_y_local_value(");
      PrintIndex1;
    }
    break;

  case GLOBAL_UNIFY_CONSTANT:
    {
      DB_Print1("global_unify_constant(");
      PrintConstant; 
      DB_Print1(")");
    }
    break;

  case GLOBAL_UNIFY_NIL:
    {
      DB_Print1("global_unify_nil");
    }
    break;

  case GLOBAL_UNIFY_STRUCTURE:
    {
      DB_Print1("global_unify_structure(");
      PrintStructure;
      DB_Print1(")");
    }
    break;

  case GLOBAL_UNIFY_LIST:
    {
      DB_Print1("global_unify_list");
    }
    break;

  case BUILD_REC_POSLIST:
    {
      ResetIndex;
      DB_Print1("build_rec_poslist(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex;
      DB_Print1(")");
    }
    break;

  case BUILD_POSLIST:
    {
      ResetIndex;
      DB_Print1("build_poslist(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex;
      DB_Print1(")");
    }
    break;

  case BUILD_POSLIST_VALUE:
    {
      ResetIndex;
      DB_Print1("build_poslist_value(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex;
      DB_Print1(")");
    }
    break;

  case BUILD_NEGLIST:
    {
      ResetIndex;
      DB_Print1("build_neglist(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex;
      DB_Print1(")");
    }
    break;

  case BUILD_NEGLIST_VALUE:
    {
      ResetIndex;
      DB_Print1("build_neglist_value(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex;
      DB_Print1(")");
    }
    break;

  case BUILD_VARIABLES:
    {
      ResetIndex;
      DB_Print1("build_variables(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex;
      DB_Print1(")");
    }
    break;

  case PUT_NTH_HEAD:
    {
      ResetIndex;
      DB_Print1("put_nth_head(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintIndex;  DB_Print1(",");
      PrintSmallIndex; 
      DB_Print1(")");
    }
    break;

  case PUT_NTH_TAIL:
    {
      ResetIndex;
      DB_Print1("put_nth_tail(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintIndex;  DB_Print1(",");
      PrintSmallIndex;
      DB_Print1(")");
    }
    break;

  case PUT_GLOBAL_ARG:
    {
      ResetIndex;
      DB_Print1("put_global_arg(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex;
      DB_Print1(")");
    }
    break;

  case NIL_NTH_HEAD:
    {
      ResetIndex;
      DB_Print1("nil_nth_head(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintIndex;
      DB_Print1(")");
    }
    break;

  case UNIFY_NTH_HEAD:
    {
      ResetIndex;
      DB_Print1("unify_nth_head(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintIndex;
      DB_Print1(")");
    }
    break;

  case UNIFY_NTH_TAIL:
    {
      ResetIndex;
      DB_Print1("unify_nth_tail(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintIndex;
      DB_Print1(")");
    }
    break;

  case UNIFY_GLOBAL_ARG:
    {
      ResetIndex;
      DB_Print1("unify_global_arg(");
      PrintSmallIndex;
      DB_Print1(")");
    }
    break;

  case START_RIGHT_BODY:
    {
      ResetIndex;
      DB_Print1("start_right_body(");
      PrintSmallIndex; DB_Print1(",");
      PrintLabel(pc);
      {
	int env_size;
	int wait_list_size = Get_Index(pc);

	DB_Print2(",%d,[", wait_list_size);

	while (--wait_list_size)
	  {
	    PrintIndex;
	    DB_Print1(",");
	  }
	PrintIndex;
	DB_Print1("]");

	env_size = Get_Index(pc);
	if (env_size != NO_ENV_ID)
	  {
	    DB_Print2(",%d", env_size);
	  }
      }
      DB_Print1(")");
    }
    break;

  case START_LEFT_BODY:
    {
      DB_Print1("start_left_body(");
      PrintLabel(pc);
      {
	int env_size;
	int wait_list_size = Get_Index(pc);

	DB_Print2(",%d,[", wait_list_size);

	while (--wait_list_size)
	  {
	    PrintIndex;
	    DB_Print1(",");
	  }
	PrintIndex;
	DB_Print1("]");

	env_size = Get_Index(pc);
	if (env_size != NO_ENV_ID)
	  {
	    DB_Print2(",%d", env_size);
	  }
      }
      DB_Print1(")");
    }
    break;

  case INITIALIZE_RIGHT:
  {  
      ResetIndex;
      DB_Print1("initialize_right(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex;
      DB_Print1(")");
    }
    break;

  case INITIALIZE_LEFT:
    {
      ResetIndex;
      DB_Print1("initialize_left(");
      PrintSmallIndex;
      DB_Print1(")");
    }
    break;

  case SPAWN_RIGHT:
    {
      ResetIndex;
      DB_Print1("spawn_right(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; 
      DB_Print1(")");
    }
    break;

  case SPAWN_LEFT:
    {
      ResetIndex;
      DB_Print1("spawn_left(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; 
      DB_Print1(")");
    }
    break;

  case AWAIT_LEFTMOST:
    {
      DB_Print1("await_leftmost");
    }
    break;

  case AWAIT_NONVAR:
    {
      ResetIndex;
      DB_Print1("await_nonvar");
      PrintSmallIndex;
    }
    break;

  case AWAIT_STRICTLY_NONVAR:
    {
      ResetIndex;
      DB_Print1("await_strictly_nonvar(");
      PrintSmallIndex;
      DB_Print1(")");
    }
    break;

  case AWAIT_VARIABLE:
    {
      ResetIndex;
      DB_Print1("await_variable(");
      PrintSmallIndex;
      DB_Print1(")");
    }
    break;

  case AWAIT_NONVAR_UNIFY:
    {
      ResetIndex;
      DB_Print1("await_nonvar");
    }
    break;

  case AWAIT_STRICTLY_NONVAR_UNIFY:
    {
      ResetIndex;
      DB_Print1("await_strictly_nonvar");
    }
    break;

  case AWAIT_VARIABLE_UNIFY:
    {
      ResetIndex;
      DB_Print1("await_variable");
    }
    break;

  case PAR_BUILTIN:
    {
      uword func = Get_Index_I_M(1,instruction);
      natural arity = GetInlineArity(func);
      {
	natural wait_list_size = Get_Index_I(2,instruction);
		
	DB_Print4("builtin('%s'/%d,%d,[", GetInlineName(func), arity,
		  wait_list_size);
	
	while (--wait_list_size)
	  {
	    PrintIndex; DB_Print1(",");
	  }
	PrintIndex; DB_Print1("],");
      }
      PrintIndex; DB_Print1(",");
      {
	natural live_list_size = Get_Index(pc);

	DB_Print2("%d,[", live_list_size);

	while (--live_list_size)
	  {
	    PrintIndex; DB_Print1(",");
	  }
	PrintIndex; DB_Print1("],[");
      }

      while (--arity)		/* arity may not be zero (is this assumption ok?) */
	{
	  PrintIndex; DB_Print1(",");
	}
      PrintIndex; DB_Print1("])");
    }
    break;

  case PAR_INLINE:
      {
	uword func = Get_Index_I_M(1,instruction);
	natural arity = GetInlineArity(func);
	{
	  natural wait_list_size = Get_Index_I(2,instruction);
	
	  DB_Print4("inline('%s'/%d,%d,", GetInlineName(func), arity,
		    wait_list_size);

	  PrintLabel(pc); DB_Print1(",[");
	
	  while (--wait_list_size)
	    {
	      PrintIndex; DB_Print1(",");
	    }
	  PrintIndex; DB_Print1("],[");
	
	  while (--arity)	/* arity may not be zero (is this assumption ok?) */
	    {
	      PrintIndex; DB_Print1(",");
	    }
	  PrintIndex; DB_Print1("])");
	}
      }
    break;

  case LOCK_AND_GET_LIST:
    {
      ResetIndex;
      DB_Print1("lock_and_get_list(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; 
      DB_Print1(")");
    }
    break;

  case LOCK_AND_GET_STRUCTURE:
    {
      ResetIndex;
      DB_Print1("lock_and_get_structure(");
      PrintStructure;  DB_Print1(",");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; 
      DB_Print1(")");
    }
    break;

  case UNLOCK:
    {
      ResetIndex;
      DB_Print1("unlock(");
      PrintSmallIndex; DB_Print1(",");
      PrintSmallIndex; 
      DB_Print1(")");
    }
    break;
#endif /* REFORM */

  case HALT:
    {
      DB_Print1("halt");
    }
    break;
  
  default:
    {
      DB_Print1("display_code: no such instruction!");
    }
  }
  return pc;
}
