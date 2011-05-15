/*
 * builtin.c - a major portion of the C defined predicates.
 *
 * Johan Bevemyr.....Thu Jun  6 1991
 * Patric Hedlin.....Tue Jan 10 1995
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include <ctype.h>

#include "luther.h"
#include "engine.h"
#include "unify.h"
#include "think.h"
#include "debug.h"
#include "assert.h"
#include "bignum.h"
#include "builtin.h"


LOCAL int unique_name_index = 0;

LOCAL code *luther_clause_code;
LOCAL code *luther_clause_code_noref;
LOCAL code *luther_retry_choice_code;
LOCAL code *luther_retry_current_pred;

LOCAL TAGGED functor_d_ref;	/* '$ref'/2 */


/*******************************************************************************
 *
 * true - Succeeds.
 *
 */
BOOL luther_true(Arg)
     Argdecl;
{
  return TRUE;
}

/*******************************************************************************
 *
 * fail & false - Backtracks immediately.
 *
 */
BOOL luther_fail(Arg)
     Argdecl;
{
  return FALSE;
}


#define IsDigit(N,R) (R < 11 ? (isdigit(N) && ((N - '0') < radix))\
			     : (isdigit(N) ||\
				((N >= 'a') && (N < ('a'+radix-10)))))
#define CharOf(Num)  (Num < 10 ? Num + '0' : Num - 10 + 'a')

/*******************************************************************************
 *
 * number_chars(+CONSTANT,?CHARLIST,+RADIX)
 * number_chars(?CONSTANT,+CHARLIST,+RADIX)
 *
 *   The same as name(CONSTANT,CHARLIST), but CONSTANT is constrained to be a
 * number typed with radix RADIX.
 *
 * Example:
 *
 *	| ?- number_chars(12,L,2).
 *
 *	L = [49,49,48,48] ? 
 *
 *	| ?- number_chars(12,"1100",2).
 *
 *	| ?- number_chars(12,L,10).
 *
 *	L = [49,50] ? 
 *
 *	| ?- number_chars(12,"12",10).
 *
 *	| ?- number_chars(X,"01001101",2).
 *
 *	X = 77 ?
 *
 */
BOOL luther_number_chars(Arg)
     Argdecl;
{
  register TAGGED constant;
  register TAGGED charlist;
  register TAGGED radixval;

  register int radix;

  char temp[MAXATOMLEN];        /* termporary buffer for new string */
    
  DerefNLL(constant, Xw(0));
  DerefNLL(charlist, Xw(1));
  DerefNLL(radixval, Xw(2));
    

  /* Third argument must be an integer
   */
  if (not(IsNUM(radixval)))
    {
      BuiltinError("number_chars/3", " - illegal radix value", "");
    }
  else
    {
      radix = GetNumber(radixval);
    }
    
  /*   First check if "constant" is instantiated. If it is, construct a
   * list of characters and unify with "charlist".
   */
  if (IsNUM(constant))
    {
      register TAGGED newlist; /* new charlist */

      if (radix == 10)
	{
	  sprintf(temp, "%d", GetNumber(constant));
	}
      else
	{
	  /* We have to perform the radix conversion "manually".
	   */
	  
	  int sum = GetNumber(constant);   /* the number to convert */
	  int char_cnt = 0;	           /* number of characters in list */

	  /* We first calculate how many characters are needed.
	   */
	  if (sum < 0)
	    {
	      sum *= -1;
	      inc(char_cnt);

	      temp[0] = '-';
	    }

	  while (sum)
	    {
	      sum = sum / radix;
	      inc(char_cnt);
	    }

	  if (char_cnt == 0)
	    {
	      temp[0] = '0';
	      temp[1] = '\0';
	    }
	  else
	    {
	      /* Fill in end of string marker 
	       */
	      temp[char_cnt] = '\0';

	      /* The string can now be built in reverse order.
	       */
	      sum = GetNumber(constant);

	      if (sum < 0)
		{
		  sum *= -1;
		}

	      while (sum)
		{
		  register int radix_char;

		  dec(char_cnt);
		  radix_char = sum % radix;
		  temp[char_cnt] = CharOf(radix_char);
		  sum = sum / radix;
		}

	    }
	}
      /*
       * Finally make string into list and unify with "charlist".
       */
      newlist = make_string_list(w,temp);

      return unify(charlist,newlist,w);
    }
  else if (IsFLT(constant))
    {
      if (radix == 10)
	{
	  register TAGGED newlist;

	  sprintf(temp, "%f", GetFloat(constant));
	  newlist = make_string_list(w, temp);

	  return unify(charlist, newlist, w);
	}
      else
	{
	  /*   Floating point numbers can only be converted to 
	   * string using radix 10.
	   */
	  return FALSE;
	}
    }
  else if (IsBIG(constant))
    {
      register TAGGED newlist;

      make_string_of_bignum(GetBignum(constant), temp, radix);
      newlist = make_string_list(w, temp);

      return unify(charlist, newlist, w);
    }
  else if (IsLST(charlist))
    {
      register TAGGED chr, atm;

      register int ch;
      register int char_cnt = 0;          
      register int flag_flt = 0;
      register int flag_neg = 0;

      /* We first build a string of characters from the list.
       */
      while (IsLST(charlist))
	{
	  DerefNLL(chr, Ref(GetCar(charlist)));
	  
	  if (not(IsNUM(chr)))
	    return FALSE;
	  
	  if (char_cnt == MAXATOMLEN - 1)
	    BuiltinError("number_chars/3", " - atom exceeds maximum atom length", "");

	  ch = GetNumber(chr);

	  temp[char_cnt++] = (char) ch;

	  if (ch == '.' || ch == 'e' || ch == 'E')
	    flag_flt = 1;

	  DerefNLL(charlist, Ref(GetCdr(charlist)));
	}

      /* If the list is not nil terminated fail.
       */
      if (charlist != atom_nil)
	return FALSE;

      temp[char_cnt] = '\0';

      /* With radix 10 we let sscanf perform the conversion to integer/float.
       */
      if (flag_flt)
	{
	  if (radix == 10)
	    {
	      double tmpflt;

	      sscanf(temp, "%lf", &tmpflt);
	      atm = make_float(tmpflt, w);

	      return unify(constant, atm, w);
	    }
	  else
	    {
	      return FALSE;
	    }
	}
      else
	{
	  atm = make_number_of_string(w, temp, radix);

	  return unify(constant, atm, w);
	}
    }
  else
    return FALSE;
}

/*******************************************************************************
 *
 * atom_chars(+CONSTANT,?CHARLIST)
 * atom_chars(?CONSTANT,+CHARLIST)
 *
 *   The same as name(CONSTANT,CHARLIST) with the exception that CONSTANT is
 * constrained to be an atom.
 *
 */
BOOL luther_atom_chars(Arg)
     Argdecl;
{
  TAGGED constant;
  TAGGED charlist;

  DerefNLL(constant, Xw(0));
  DerefNLL(charlist, Xw(1));

  if (IsATM(constant))
    {
      TAGGED newlist = make_string_list(w, GetString(constant, w));

      return unify(charlist, newlist, w);
    }
  else if (IsLST(charlist))
    {
      TAGGED car_char;		/* next character in list         */
      TAGGED new_atom;		/* new atom constructed from list */

      register int ch;		/* character in list              */
      register int char_cnt;	/* number of characters in list   */

      char temp[MAXATOMLEN];	/* temporary buffer for string    */

      char_cnt = 0;

      /*   At this point we know that charlist is a list so there is no need
       * to verify this again. Therefore the do/while construct.
       */
      do {
	DerefNLL(car_char, Ref(GetCar(charlist)));

	if (not(IsNUM(car_char)))
	  return FALSE;

	temp[char_cnt++] = (char)(ch = GetNumber(car_char));

	if (char_cnt == MAXATOMLEN - 1)
	  BuiltinError("atom_chars/2", " - atom exceeds maximum atom length", "");

	DerefNLL(charlist, Ref(GetCdr(charlist)));

      } while (IsLST(charlist));

      if (charlist != atom_nil)
	return FALSE;

      temp[char_cnt] = '\0';
      new_atom = store_atom(temp, w);

      return unify(constant, new_atom, w);
    }
  else if (charlist == atom_nil)
    {
      return unify(constant, store_atom("", w), w);
    }

  return FALSE;
}

/*******************************************************************************
 *
 * name(+CONSTANT,?CHARLIST)
 * name(?CONSTANT,+CHARLIST)
 *
 *   If CONSTANT is an atom or number then CHARLIST is a list of the character
 * codes of the characters comprising the name of CONSTANT.
 *
 * Example:
 *
 *	| ?- name(product, L).
 *
 *	L = [112,114,111,100,117,99,116]
 *
 *	| ?- name(product, "product").
 *
 *	| ?- name(1976, L).
 *
 *	L = [49,57,55,54]
 *
 *	| ?- name('1976', L).
 *
 *	L = [49,57,55,54]
 *
 *	| ?- name((:-), L).
 *
 *	L = [58,45]
 *
 *   If CONSTANT is uninstantiated, CHARLIST must be instantiated to a list
 * of character codes. If CHARLIST can be interpreted as a number, CONSTANT
 * is unified with that number, otherwise with the atom whose name is CHARLIST.
 *
 * NOTE: The length of CHARLIST must be less than 512.
 *
 * Example:
 *
 *	| ?- name(X, [58,45]).
 *
 *	X = :-
 *
 *	| ?- name(X, ":-").
 *
 *	X = :-
 *
 *	| ?- name(X, [49,50,51]).
 *
 *	X = 123
 *
 *   Note that there are atoms for which name(CONSTANT,CHARLIST) is true,
 * but which will not be constructed if name/2 is called with CONSTANT
 * uninstantiated. One such atom is the atom `'1976''. It is recommended
 * that new programs use atom_chars/2 or number_chars/2, as these
 * predicates do not have this inconsistency.
 *
 */
BOOL luther_name(Arg)
     Argdecl;
{
  /*   Alternative implementation of name/2. First we try atom_chars/2,
   * alternatively, if number_chars/3 fail, we call number_chars/3 with
   * radix 10 (decimal).
   */
  Xw(2) = Make_Integer(10);

  if (luther_atom_chars(Arg))
    return TRUE;
  else
    return luther_number_chars(Arg);
}


/*******************************************************************************
 *
 * unique_name(?Prefix,?Suffix,-Name)
 *
 *   The resulting Name is unified with an unique atom which starts with Prefix
 * and ends with Suffix. If Prefix is a variable then it is unified with the
 * default prefix. Prefix becomes the new default prefix. If Suffix is a
 * variable it is left uninstantiated.
 * 
 * Example:
 *
 *   | ?- unique_name(Prefix,Suffix,Unique).
 *
 *   Prefix = '$new',
 *   Suffix = _1061ec,
 *   Unique = '$new0' ? 
 *
 *   | ?- unique_name(foo,Suffix,Unique).
 *        
 *   Suffix = _1061c8,
 *   Unique = foo1 ? 
 *
 *   | ?- unique_name(foo,bar,Unique).
 *
 *   Unique = foo2bar ? 
 *
 */
BOOL luther_unique_name(Arg)
     Argdecl;
{
  TAGGED prefix, suffix, name;
  TAGGED unique;

  DerefNLL(prefix, Xw(0));
  DerefNLL(suffix, Xw(1));
  DerefNLL(name,   Xw(2));

  if (IsVar(prefix))
    {
      Bind(prefix, default_prefix, return FALSE);
    }
  else if (IsATM(prefix))
    {
      default_prefix = prefix;
    }
  else
    {
      return FALSE;
    }

  if (IsVar(name))
    {
      int length;

      char buffer[MAXATOMLEN];	/* string containing new atom      */
      char number[50];		/* string representation of number */

    start:
      {
	sprintf(number, "%d", unique_name_index++);
	
	length = strlen(number);
	
	strncpy(buffer, GetString(prefix, w), MAXATOMLEN - length - 1);

	buffer[MAXATOMLEN - length] = '\0';

	strcat(buffer, number);
	
	if (IsATM(suffix))
	  {
	    strncat(buffer, GetString(suffix, w), MAXATOMLEN - 1);
	  }

	if (!(unique = atom_exist(buffer, w)))
	  {
	    goto start;
	  }
      }
      Bind(name, unique, return FALSE);

      return TRUE;
    }
  else
    {
      /*   If "name" already is bound to an atom then that atom 
       * already exist in the database and cannot be unique.
       */
      return FALSE;
    }
}


/*******************************************************************************
 *
 * Note: If the UNIX runtime environment does not contain a random function
 *       we substitute it with the less random, but more standard, rand().
 *
 */
#if ! defined(HAVE_RANDOM)
int random()
{
  return rand();
}

void srandom(seed)
     int seed;
{
  srand(seed);
}
#endif /* NEED_RANDOM */


/*******************************************************************************
 *
 * seed(+NUMBER) - The random generator is seeded with NUMBER.
 *
 */
BOOL luther_seed(Arg)
     Argdecl;
{
  register TAGGED seed;
  
  DerefNLL(seed, Xw(0));

  if (not(IsNUM(seed)))
    return FALSE;

  srandom(GetNumber(seed));

  return TRUE;
}

/*******************************************************************************
 *
 * random(?RANGE,?NUMBER)
 *
 *   Unifies NUMBER with a random number in the range 0-RANGE. If RANGE is
 * undefined then NUMBER is unified with a number in the range 0-MAXNUMBER.
 *
 */
BOOL luther_random(Arg)
     Argdecl;
{
  register TAGGED range;
  register TAGGED number;
  
  DerefNLL(range,  Xw(0));
  DerefNLL(number, Xw(1));

  if (IsVar(range))
    {
      return unify(number, Make_Integer(random()), w);
    }
  else if (IsNUM(range))
    {
      return unify(number, Make_Integer((random() % GetNumber(range)) + 1), w);
    }
  else if (IsBOX(range))
    {
      BuiltinError("random/2", " - illegal first argument - ", "(out of range)");
    }

  return FALSE;
}


/*******************************************************************************
 *
 * halt - Causes an irreversible exit from Prolog back to the shell. 
 *
 */
BOOL luther_halt(Arg)
     Argdecl;
{
  luther_exit(0);
  return FALSE;
}

/********************************************************************************
 *
 * version
 *
 *   Displays the introductory messages for all the component parts of the
 * current system.
 *
 */
BOOL luther_version(Arg)
     Argdecl;
{
  PL_Print4(currout, "Luther Reform Prolog (%s) version %s, %s.\n",
	    LUTHER_SUPPORT,
	    LUTHER_VERSION,
	    LUTHER_COMPILE_DATE);
  PL_Print1(currout, "Copyright (C) 1991 - 1995 Uppsala University.\n");
  PL_Print1(currout, "All rights reserved.\n");
  
  return TRUE;
}


/*******************************************************************************
 *
 * '$freeze'(?Variable,?Goal) - When the Variable is bound Goal is executed.
 *
 */
BOOL luther_freeze(Arg)
     Argdecl;
{
#if defined(CONSTR)
  register TAGGED variable;      /* variable to constrain */
  register TAGGED goal;          /* goal to suspend       */

  register TAGGED cons;

  DerefNLL(variable, Xw(0));
  DerefNLL(goal,     Xw(1));

  Make_LST(w->heap_top, cons);
  PushOnHeap(w->heap_top, goal);
  CreateHVA(w->heap_top, w);

  if (IsCVA(variable))
    {
      register TAGGED last_tail;

      LastTail(last_tail, GetCVAGoals(variable));

      Bind_HVA(last_tail, cons, return FALSE);

      return TRUE;
    }
  else
    {
      register TAGGED new_cva;
      
      CreateCVA(w->heap_top, new_cva, cons, w);

      return unify(variable, new_cva, w);
    }
#endif

  return FALSE;
}

/*******************************************************************************
 *
 * '$frozen'(-VARIABLE,?GOALLIST)
 *
 *   If some goal is blocked on the variable VARIABLE, that particular instance
 * is unified with GOALLIST. Otherwise, GOALLIST is unified with the atom `true'.
 * If VARIABLE is not a variable, this is interpreted as failure.
 * 
 * NOTE: This predicate is called by frozen/2 (defined in Library/freeze).
 *
 *       frozen(-VARIABLE,?CONSTRAINTS)
 *
 */
BOOL luther_frozen(Arg)
     Argdecl;
{
#if defined(CONSTR)
  register TAGGED variable;
  register TAGGED goallist;

  DerefNLL(variable, Xw(0));
  DerefNLL(goallist, Xw(1));

  if (IsCVA(variable))
    {
      return unify(goallist, GetCVAGoals(variable), w);
    }
  else
    {
      return unify(goallist, atom_table_tagged[ATOM_TRUE], w);
    }
#endif

  return FALSE;
}

/*******************************************************************************
 *
 * defrost(?VARIABLE)
 *
 *   If VARIABLE is a constrained variable its constraints are removed.
 *
 */
BOOL luther_defrost(Arg)
     Argdecl;
{
#if defined(CONSTR)
  TAGGED variable;
  
  DerefNLL(variable, Xw(0));

  if (IsCVA(variable))
    {
      TAGGED new_var;
      
      if (IsCondCVA(variable))
	{
	  PushOnTrail(w, variable);
	}

      LoadHVA(w->trail_top, new_var, w);
      SetVar(variable, new_var);
    }
#endif

  return TRUE;
}


/*******************************************************************************
 *
 * setarg(+argument, +compound term, ?replacement)
 *
 * Replace destructively "argument" in "compound term" by "replacement". 
 *
 *   The assignment is undone on backtracking. This operation is only safe if
 * there is no further use of the "old" value of the replaced argument. The use
 * of this predicate is discouraged, as the idea of destructive
 * replacement is alien to logic programming.
 *
 */
BOOL luther_setarg(Arg)
     Argdecl;
{
  TAGGED  argument, compound, replacer;
  TAGGED *location;
    
  DerefNLL(argument, Xw(0));
  DerefNLL(compound, Xw(1));
  DerefNLL(replacer, Xw(2));

  /* First some type checking. A better way is to perform this in Prolog.
   */
  if (not(IsNUM(argument)))
    BuiltinError("setarg/3", " - illegal first argument", " (not an integer)");

  if (IsLST(compound))
    {
      /*   Check that argument is within range (1 or 2) and calculate
       * which memory cell to zap (location).
       */
      switch (GetNumber(argument)) {

      case 1:
	{
	  location = GetCar(compound);
	}
	break;

      case 2:
	{
	  location = GetCdr(compound);
	}
	break;

      default:
	goto error;
      }
    }
  else if (IsSTR(compound))
    {
      /* Check that argument is within range (1..arity).
       */
      if (1 > GetNumber(argument) || GetNumber(argument) > GetArity(compound))
	goto error;

      location = GetArg(compound, GetNumber(argument) - 1);
    }
  else
    {
      BuiltinError("setarg/3", " - illegal second argument", " (not a compound term)");
    }
        
  /*   We can't have stack variables on the heap. If it is a stack variable
   * we have to globalize it.
   */
  if (IsSVA(replacer))
    {
      register TAGGED heap_var;

      LoadHVA(w->heap_top, heap_var, w);
      Bind_SVA(replacer, heap_var);
      replacer = heap_var;
    }

  /* Always trail setarg/3.
   */
  ValueTrail(w, location, *location);
  *location = replacer;

  return TRUE;

 error:
  {
    BuiltinError("setarg/3", " - illegal first argument", " (out of range)");
  }
}


/*******************************************************************************
 *
 * zaparg(+argument, +compound term, ?replacement)
 *
 * Replace destructively "argument" in "compound term" by "replacement". 
 *
 *   Like setarg/3 with the exception that the assignment is _not_ undone on
 * backtracking.
 *
 */
BOOL luther_zaparg(Arg)
     Argdecl;
{
  TAGGED  argument, compound, replacer;
  TAGGED *location;

  DerefNLL(argument, Xw(0));
  DerefNLL(compound, Xw(1));
  DerefNLL(replacer, Xw(2));

  /* First some type checking. A better way is to perform this in Prolog.
   */
  if (not(IsNUM(argument)))
    BuiltinError("zaparg/3", " - illegal first argument", " (not an integer)");

  if (IsLST(compound))
    {
      /*   Check that argument is within range (1 or 2) and calculate
       * which memory cell to zap (location).
       */
      switch (GetNumber(argument)) {

      case 1:
	{
	  location = GetCar(compound);
	}
	break;

      case 2:
	{
	  location = GetCdr(compound);
	}
	break;

      default:
	goto error;
      }
    }
  else if (IsSTR(compound))
    {
      /* Check that argument is within range (1..arity).
       */
      if (1 > GetNumber(argument) || GetNumber(argument) > GetArity(compound))
	goto error;

      location = GetArg(compound, GetNumber(argument) - 1);
    }
  else
    {
      BuiltinError("zaparg/3", " - illegal second argument", " (not a compound term)");
    }
        
  /*   We can't have stack variables on the heap. If it is a stack variable
   * we have to globalize it.
   */
  if (IsSVA(replacer))
    {
      register TAGGED heap_var;

      LoadHVA(w->heap_top, heap_var, w);
      Bind_SVA(replacer, heap_var);
      replacer = heap_var;
    }

  *location = replacer;

  return TRUE;

 error:
  {
    BuiltinError("zaparg/3", " - illegal first argument", " (out of range)");
  }
}


/*******************************************************************************
 *
 * '$atom_mode'(+ATOM,?MODE)
 *
 *   Unifies MODE with the mode of ATOM. The mode is used for determining
 * whether an atom need to be quoted or not.
 *
 *   MODE = 0x00      alpha
 *   MODE = 0x01      quote
 *   MODE = 0x02      other
 *   MODE = 0x04      punct      
 *
 */
BOOL luther_atom_mode(Arg)
     Argdecl;
{
  TAGGED term, mode;

  DerefNLL(term, Xw(0));
  DerefNLL(mode, Xw(1));

  if (IsATM(term))
    return unify(mode, GetAtomMode(term), w);
  else
    return FALSE;
}


/*******************************************************************************
 *
 * '$loading_mode'(?OldMode,+NewMode)
 *
 *
 * Note: loading_mode is set to 'consult' on initialization.
 *
 */
static TAGGED loading_mode;


BOOL luther_loading_mode(Arg)
     Argdecl;
{
  TAGGED old_mode, new_mode;
  
  DerefNLL(old_mode, Xw(0));

  if (not(unify(old_mode, loading_mode, w)))
    return FALSE;

  DerefNLL(new_mode, Xw(1));

  if (not(IsATM(new_mode)))
    return FALSE;

  loading_mode = new_mode;

  return TRUE;
}


/*******************************************************************************
 *
 * '$module'(?OldModule,+NewModule)
 *
 *   Unifies OldModule with the name of the current module and sets the current
 * module to NewModule.
 *
 */
BOOL luther_module(Arg)
     Argdecl;
{
  TAGGED old_module;
  TAGGED new_module;

  DerefNLL(old_module, Xw(0));

  /* First unify the current module with "old_module". 
   */
  if (not(unify(w->global->current_module, old_module, w)))
    {
      return FALSE;
    } 

  /* Currently the name of a module must be an atom.
   */
  DerefNLL(new_module, Xw(1));

  if (not(IsATM(new_module)))
    BuiltinError("'$module'/2", " - illegal second argument", " (bad module specification)");

  w->global->current_module = new_module;
    
  return TRUE;
}

/*
 * '$get_module'(?Module)
 *
 */
BOOL luther_get_module(Arg)
     Argdecl;
{
  TAGGED module;

  DerefNLL(module, Xw(0));

  return unify(w->global->current_module, module, w);
}

/*
 * '$set_module'(+Module)
 *
 */
BOOL luther_set_module(Arg)
     Argdecl;
{
  TAGGED module;

  DerefNLL(module, Xw(0));

  /* Currently the name of a module must be an atom.
   */
  if (not(IsATM(module)))
    BuiltinError("'$set_module'/1", " - illegal first argument", " (bad module specification)");

  w->global->current_module = module;
    
  return TRUE;
}

/*******************************************************************************
 *
 * '$public'(+Name,+Arity)
 *
 *       Exports the predicate Name/Arity from the current module.
 *
 */
BOOL luther_public(Arg)
     Argdecl;
{
  TAGGED name, arity;

  DerefNLL(name,  Xw(0));
  DerefNLL(arity, Xw(1));

  assert(IsATM(name) && IsNUM(arity));

  (void) make_public(StoreFunctor(name, GetNumber(arity)), w);

  return TRUE;
}

/*******************************************************************************
 *
 * '$dynamic'(+Name,+Arity)
 *
 */
BOOL luther_dynamic(Arg)
     Argdecl;
{
  TAGGED name, arity;

  TAGGED module = w->global->current_module;

  DerefNLL(name,  Xw(0));
  DerefNLL(arity, Xw(1));

 start:
  {
    if (IsSTR(name))
      {
	if (GetFunctor(name) == functor_colon)
	  {
	    DerefNLL(module, Ref(GetArg(name, 0)));
	    DerefNLL(name,   Ref(GetArg(name, 1)));

	    goto start;
	  }
      }
    else if (IsATM(name))
      {
	if (IsNUM(arity))
	  {
	    TAGGED functor = StoreFunctor(name, GetNumber(arity));

	    definition *def = get_definition_module(functor, module, w);

	    switch (def->enter_instruction) {

	    case ENTER_C:
	    case ENTER_EMULATED:
	      {
		if (def->module == module) 
		  {
		    /* Remove current definition and make it dynamic.
		     */
		    def->enter_instruction = ENTER_INTERPRETED;
		    def->entry_code.indexinfo = make_empty_index_table(w);
		  }
		else
		  {
		    return luther_error(E_NOT_REDEFINED, functor, w);
		  }
	      }
	      break;

	    case ENTER_TRUE_SPY:
	    case ENTER_FAKE_SPY:
	    case ENTER_INTERPRETED:
	      break;

	    case ENTER_UNDEFINED:
	      {
		def->entry_code.indexinfo = make_empty_index_table(w);
		def->enter_instruction = ENTER_INTERPRETED;
	      }
	    }
	    return TRUE;
	  }
      }
    return FALSE;
  }
}


/*******************************************************************************
 *
 * '$assert_delete_other'(+HEAD,+BODY)
 *
 *   Adds the clause  HEAD :- BODY  to the database while deleting all
 * existing clauses of the same predicate.
 *
 */
BOOL luther_assert_delete_other(Arg)
     Argdecl;
{
  TAGGED head, body;
  TAGGED name, module;

  code *clause_code;

  DerefNLL(head, Xw(0));
  DerefNLL(body, Xw(1));

  clause_code = compile_clause(head, body, w, &name, &module);

  if (clause_code == NULL)
    BuiltinError("'$assert_delete_other/2'", "unable to assert clause", "");

  /* Update the database deleting all existing clauses for the predicate.
   */
  store_dynamic_predicate(name, module, clause_code, w);

  return TRUE;
}

/*******************************************************************************
 *
 * '$asserta'(+HEAD,+BODY,?PREDICATEREF,?CLAUSEREF)
 *
 *   Add the clause  HEAD :- BODY  as the first clause of the corresponding
 * predicate in the database. Further, PREDICATEREF is unified with an internal
 * reference to the predicate and CLAUSEREF is unified with an internal ditto
 * to the added clause. 
 *
 */
BOOL luther_asserta(Arg)
     Argdecl;
{
  TAGGED head, body, pref, cref;
  TAGGED name, module;

  code *clause_code;

  DerefNLL(head, Xw(0));
  DerefNLL(body, Xw(1));
  DerefNLL(pref, Xw(2));
  DerefNLL(cref, Xw(3));

  /* Generate code for head unification of clause(head,body)
   */
  clause_code = compile_clause(head, body, w, &name, &module);

  if (clause_code == NULL)
    BuiltinError("'$asserta'/4", "unable to assert clause", "");

  add_first_dynamic_predicate(name, module, clause_code, pref, cref, w);

  return TRUE;
}

/*******************************************************************************
 *
 * `$assertz(+HEAD,+BODY,?PREDICATEREF,?CLAUSEREF)'
 *
 *   Add the clause  HEAD :- BODY  as last clause of the corresponding
 * predicate in the database. Further, PREDICATEREF is unified with an internal
 * reference to the predicate and CLAUSEREF is unified with an internal ditto
 * to the added clause. 
 *
 */
BOOL luther_assertz(Arg)
     Argdecl;
{
  TAGGED head, body, pref, cref;
  TAGGED name, module;

  code *clause_code;

  DerefNLL(head, Xw(0));
  DerefNLL(body, Xw(1));
  DerefNLL(pref, Xw(2));
  DerefNLL(cref, Xw(3));

  /* Generate code for head unification of clause(head,body)
   */
  clause_code = compile_clause(head, body, w, &name, &module);

  if (clause_code == NULL)
    BuiltinError("'$assertz'/4", "unable to assert clause", "");

  add_last_dynamic_predicate(name, module, clause_code, pref, cref, w);

  return TRUE;
}


/*******************************************************************************
 *
 * '$erase'(+PREF,+CREF)
 *
 *   The recorded item (or dynamic clause) whose implementation-defined
 * identifier is REF is effectively erased from the internal database or
 * interpreted program.
 *
 */
BOOL luther_erase(Arg)
     Argdecl;
{
  TAGGED pref;
  TAGGED cref;

  definition *def;

  DerefNLL(pref, Xw(0));
  DerefNLL(cref, Xw(1));

  def = (definition *)TermToPointer(pref);

  switch (def->enter_instruction) {

  case ENTER_C:
  case ENTER_EMULATED:
    return luther_error(E_PRED_NOT_DYN, (TAGGED)def, w);

  case ENTER_TRUE_SPY:
  case ENTER_FAKE_SPY:
  case ENTER_INTERPRETED:
    {
      remove_dynamic_clause(def->entry_code.indexinfo,
			    (retry_node *)TermToPointer(cref));
    }
    return TRUE;

  case ENTER_UNDEFINED:
    return luther_error(E_PRED_NOT_DEF, (TAGGED)def, w);
  }
}


/*******************************************************************************
 *
 * '$mgu_variables'(X,Y,VARLIST)
 *
 *   VARLIST is a list of all variables ...
 *
 */
BOOL luther_mgu_variables(Arg)
     Argdecl;
{
  TAGGED xvar, yvar, var_list;
  u32 saved_trailtop_offset;

  DerefNLL(xvar, Xw(0));
  DerefNLL(yvar, Xw(1));

  /* Assuming that a choice point have just been created.
   */
  saved_trailtop_offset = w->trail_top-w->trail_start;

  if (unify(xvar, yvar, w))
    {
      DerefNLL(var_list, Xw(2));

      if (w->trail_start+saved_trailtop_offset == w->trail_top)
	{
	  return unify(var_list, atom_nil, w);
	}
      else
	{
	  TAGGED *new_list, *trail;

	  trail = w->trail_start + saved_trailtop_offset;
	  new_list = w->heap_top;

	  /* Build var list.
	   */
	  while (trail < w->trail_top)
	    {
	      TAGGED bound_to;

	      PushOnHeap(w->heap_top, *trail);
	      PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE, LST));

	      bound_to = *TagToPointer(*trail);

	      if (IsVar(bound_to))
		{
		  TAGGED *current_trail = trail+1;

		  /*   Check if the variable is found later in the trail. If it
		   * is, it should not be included in the list a this point.
		   * We do not want duplicates in the list.
		   */
		  while (current_trail < w->trail_top)
		    {
		      if ((*current_trail == bound_to) ||
			 (*TagToPointer(*current_trail) == bound_to))
			goto found;
		    }
		  
		  PushOnHeap(w->heap_top,*TagToPointer(*trail));
		  PushOnHeap(w->heap_top,Tagify(w->heap_top+VARSIZE, LST));

		found:
		  ;
		}
	      trail++;
	    }

	  /* Nil terminate list.
	   */
	  *(w->heap_top-1) = atom_nil;
	  
	  /* Remove bindings created during unification.
	   */
	  Unwind_Trail(w->trail_start+saved_trailtop_offset);

	  /* globalize any SVAs
	   */
	  {
	    TAGGED *hp = new_list;

	    while (hp < w->heap_top)
	      {
		if (IsSVA(*hp))
		  {
		    Bind_SVA(*hp,Tagify(hp, HVA));
		    *hp = Tagify(hp, HVA);
		  }
		hp += 2*VARSIZE;
	      }
	  }
	  return unify(var_list, Tagify(new_list, LST), w);
	}
    }
  else
    {
      return FALSE;
    }
}


/*******************************************************************************
 *
 * '$clause'(?Head,?Body,?Ref, PREF, CREF)
 *
 * This predicate is used when implementing the following predicates:
 * 
 *   clause(:Head,?Body)
 *   clause(:Head,?Body,?Ref)
 *   clause(?Head,?Body,+Ref)
 *
 *   The clause (Head :- Body) exists in the current interpreted program, and is
 * uniquely identified by Ref. The predicate concerned must currently be dynamic.
 * At the time of call, either REF must be instantiated to a valid identifier, or
 * Head must be instantiated to an atom or a compound term. Thus `clause/3' may
 * have two different modes of use.
 *
 *   If PREF and CREF are 0 then this is a call to '$clause'/5 and not a
 * backtracking invocation of '$clause'/5.
 *
 * Note: clause/3 is defined as 
 *
 *       clause(Head,Body,Ref) :- '$clause'(Head, Body, Ref, 0, 0).
 *
 */
BOOL luther_clause(Arg)
     Argdecl;
{
  TAGGED head;
  TAGGED body = Xw(1);
  TAGGED ref  = Xw(2);

  definition *this_def;
  retry_node *this_cls;

  choicepoint *newchoice;

  /*
   *   The Body (Xw(1)) is just passed on to 'match_term' so there is no need
   * to dereference it here. If PREF is not zero then this is a backtracking
   * invocation of '$clause'/5 and clause should try to unify Head and Body
   * with the next clause of the predicate. This is done by restoring the saved
   * definition pointer from PREF and the current_clause pointer from CREF.
   *
   */
  if (GetNumber(Xw(3)) != 0)
    {  
      /* This is a backtracking invocation of '$clause'/5.
       */
      this_def = ((definition *)TermToPointer(Xw(3)));
      this_cls = ((retry_node *)TermToPointer(Xw(4)))->next;

      /*   We set newchoice to NULL in order to be able to detect, later on, that
       * this is a backtracking invocation of '$clause'/5.
       */
      newchoice = NULL;
      goto retry;
    }
  else 
    {
      DerefNLL(head, Xw(0));

      if (not(IsSTR(head) || IsATM(head)))
	return FALSE;
    }

  /* Find the definition of the predicate in the database.
   */
  {
    TAGGED goal_no_module;

    this_def = get_definition_term(head, w, &goal_no_module);

    head = Xw(0) = goal_no_module;
  }

  /* If the clause is not dynamic (interpreted or spied on) then generate error.
   */
  switch (this_def->enter_instruction) {

  case ENTER_C:
  case ENTER_EMULATED:
    return luther_error(E_CLAUSE_NOT_DYNAMIC, head, w);

  case ENTER_TRUE_SPY:
  case ENTER_FAKE_SPY:
  case ENTER_INTERPRETED:
    break;

  case ENTER_UNDEFINED:
    return FALSE;
  }

  /*   Get retry-node of clause. A retry node consist of a linked list of
   * clauses which should be tried.
   */
  this_cls = this_def->entry_code.indexinfo->var;

  /* The clause may have been deleted.
   */
  if (IsEmptyDynPred(this_cls))
    return FALSE;

  /*
   *   Now it is time to try and match the first clause with the Head and Body
   * of this call. Since matching may result in structures being built on the
   * heap and existing variables being bound we have to save some values.
   * We may just as well save them in a choice point since we are likely to
   * have to create one later anyway. However, there is no need to build a
   * complete choice point, we can fill in some parts later.
   *
   */
 try:				
  {
    /*   Reference passed to user. It has to be dereferenced later. We do it
     * here to avoid duplicating the code.
     */
    DerefNLL(ref, ref);		

    newchoice = (choicepoint *) Get_Local_Stack_Top;

    if (newchoice > (choicepoint *) w->stack_margin)
      {
	w->gc_info.arity = 5;
	w->gc_info.env_size = FrameSize(w->next_instr);

	shift_stack(w, (size_t)newchoice - (size_t)w->stack_margin);

	newchoice = (choicepoint *) Get_Local_Stack_Top;
      }
    
    newchoice->last_choice = w->choice;
    newchoice->generation = NEW_GEN;
    newchoice->global_top = w->heap_top;
    newchoice->trail_top = w->trail_top;
    newchoice->areg[0] = head;
    newchoice->areg[ARSIZE*1] = body;
#if defined(TIMESTAMP) || defined(UNBOUND)
    newchoice->timestamp = w->time;
    w->time += TIMEUNIT;
    w->uncond = w->time;
#else
    w->uncond = w->heap_top;
#endif /* TIMESTAMP || UNBOUND */

    w->choice = newchoice;
  }

  /*
   * Try until one matches or we run out of clauses.
   *
   * NOTE: This is where we enter when invoking '$clause'/5 on backtracking.
   */
 retry:				
  {
    /*   If there is only one more clause to try then we can remove the choice
     * point and fail if match_clause() fails. This is done at the trust label.
     */
    if (IsLastClause(this_cls))
      {
	goto trust;
      }
    else
      {
	/* There are potentially several matching clauses.
	 */
	if (match_term(w, this_cls->clause) == TRUE)
	  {
	    /*   The clause matched. We have to complete (or update) the
	     * current choice-point.
	     */
	    TAGGED new_ref;

	    if (newchoice != NULL)
	      {
		/*   Complete partial choice-point. This is not a backtracking
		 * invocation.
		 */
		newchoice->next_instr = w->next_instr;
		newchoice->next_clause = luther_clause_code;
		newchoice->cont_env = w->frame;
		newchoice->arity = 5;
		newchoice->areg[ARSIZE*2] = ref;
		newchoice->areg[ARSIZE*3] = PointerToTerm(this_def);
		newchoice->areg[ARSIZE*4] = PointerToTerm(this_cls);
	      }
	    else
	      {
		/*   We have backtracked into this choicepoint. Update the
		 * existing choicepoint.
		 */
		w->choice->areg[ARSIZE*3] = PointerToTerm(this_def);
		w->choice->areg[ARSIZE*4] = PointerToTerm(this_cls);
	      }

	    /* Build '$ref'(this_def, this_cls).
	     */
	    Make_STR(w->heap_top, new_ref, functor_d_ref);

	    PushOnHeap(w->heap_top, PointerToTerm(this_def));
	    PushOnHeap(w->heap_top, PointerToTerm(this_cls));
	  
	    /*   If Ref was bound during our last visit to '$clause'/5 its
	     * binding was trailed and we are now free to bind it again.
	     */
	    return unify(ref, new_ref, w);
	  }
	else
	  {
	    /*   The match failed. We have to undo all possible changes to
	     * the state (a restricted form of backtracking).
	     */
	    Unwind_Trail(w->choice->trail_top);

	    w->heap_top = w->choice->global_top;

	    Xw(0) = w->choice->areg[0];
	    Xw(1) = w->choice->areg[ARSIZE*1];

	    this_cls = this_cls->next;

	    /* Try the next clause of the predicate.
	     */
	    goto retry;
	  }
      }
  }

  /*   We have reached the last clause of the predicate. The choicepoint
   * must be deallocated.
   */
 trust:
  {
    w->choice = w->choice->last_choice;

#if defined(TIMESTAMP) || defined(UNBOUND)
# if defined(GENERATIONAL)
    if (w->choice->timestamp+TIMEUNIT < w->old_gen_uncond)
      w->uncond = w->old_gen_uncond;
    else
# endif
      w->uncond = w->choice->timestamp + TIMEUNIT;
#else
    w->uncond = w->choice->global_top;
#endif /* TIMESTAMP */
    
    if (match_term(w, this_cls->clause) == TRUE)
      {
	/*   The last clause unified with Head and Body, we thus build a '$ref'/2
	 * structure and return.
	 */
	TAGGED new_ref;

	/* Create '$ref'(this_def, this_cls)
	 */
	Make_STR(w->heap_top, new_ref, functor_d_ref);

	PushOnHeap(w->heap_top, PointerToTerm(this_def));
	PushOnHeap(w->heap_top, PointerToTerm(this_cls));

	return unify(ref, new_ref, w);
      }
    else
      {
	return FALSE; /* No match, fail. */
      }
  }
}

/*******************************************************************************
 *
 * '$clause'(?Head,?Body,+PREF,+CREF)
 *
 * Same as above but we are not interested in a reference to the clause.
 * 
 * Note: clause/2 is defined as
 *
 *       clause(Head, Body) :- '$clause'(Head, Body, 0, 0).
 *
 */
BOOL luther_clause_noref(Arg)
     Argdecl;
{
  TAGGED head;
  TAGGED body = Xw(1);

  definition *this_def;
  retry_node *this_cls;

  choicepoint *newchoice;

  /*
   *   The Body (Xw(1)) is just passed on to 'match_term' so there is no need
   * to dereference it here. If PREF is not zero then this is a backtracking
   * invocation of '$clause'/4 and clause should try to unify Head and Body
   * with the next clause of the predicate. This is done by restoring the saved
   * definition pointer from PREF and the current clause pointer from CREF.
   *
   */
  if (GetNumber(Xw(2)) != 0)
    {  
      /* This is a backtracking invocation of '$clause'/4.
       */
      this_def = ((definition *)TermToPointer(Xw(2)));
      this_cls = ((retry_node *)TermToPointer(Xw(3)))->next;

      /*   We set newchoice to NULL in order to later be able to detect
       * that this is a backtracked invocation of '$clause'/4.
       */
      newchoice = NULL;

      goto retry;
    }
  else
    {
      DerefNLL(head, Xw(0));

      if (not(IsSTR(head) || IsATM(head)))
	return FALSE;
    }

  /* Find the definition of the predicate in the database.
   */
  {
    TAGGED goal_no_module;

    this_def = get_definition_term(head, w, &goal_no_module);

    head = Xw(0) = goal_no_module;
  }

  /*   If the clause is not dynamic (interpreted or spied on) then
   * generate error.
   */
  switch (this_def->enter_instruction) {

  case ENTER_C:
  case ENTER_EMULATED:
    return luther_error(E_CLAUSE_NOT_DYNAMIC, head, w);

  case ENTER_TRUE_SPY:
  case ENTER_FAKE_SPY:
  case ENTER_INTERPRETED:
    break;

  case ENTER_UNDEFINED:
    return FALSE;
  }

  /*   Get retry-node of clause. A retry node consist of a linked list
   * of clauses which should be tried.
   */
  this_cls = this_def->entry_code.indexinfo->var;

  /* The clause may have been deleted.
   */
  if (IsEmptyDynPred(this_cls))
    return FALSE;

  /*
   *   Now it is time to try and match the first clause with the HEAD and BODY
   * of this call. Since matching may result in structures being built on the
   * heap and existing variables being bound we have to save some values.
   * We may just as well save them in a choice point since we are likely to
   * have to create one later anyway. However, there is no need to build a
   * complete choice point, we can fill in some parts later.
   *
   */
 try:
  {
    newchoice = (choicepoint *) Get_Local_Stack_Top;
    
    if (newchoice > (choicepoint *) w->stack_margin)
      {
	w->gc_info.arity = 4;
	w->gc_info.env_size = FrameSize(w->next_instr);

	shift_stack(w, (size_t)newchoice - (size_t)w->stack_margin);

	newchoice = (choicepoint *) Get_Local_Stack_Top;
      }
    
    newchoice->last_choice = w->choice;
    newchoice->generation = NEW_GEN;
    newchoice->global_top = w->heap_top;
    newchoice->trail_top = w->trail_top;
    newchoice->areg[0] = head;
    newchoice->areg[ARSIZE*1] = body;
#if defined(TIMESTAMP) || defined(UNBOUND)
    newchoice->timestamp = w->time;
    w->time += TIMEUNIT;
    w->uncond = w->time;
#else
    w->uncond = w->heap_top;
#endif /* TIMESTAMP || UNBOUND */*/

    w->choice = newchoice;
  }

  /*
   * Try until one matches or we run out of clauses.
   *
   * NOTE: This is where we enter when invoking '$clause'/4 on backtracking.
   */
 retry:
  {
    /*   If there is only one more clause to try then we can remove the choice
     * point and fail if match_clause() fails. This is done at the trust label.
     */
    if (IsLastClause(this_cls))
      {
	goto trust;
      }
    else
      {
	/* There are potentially several matching clauses.
	 */
	if (match_term(w, this_cls->clause) == TRUE)
	  {
	    /*   The clause matched. We have to complete (or update) the
	     * current choice point.
	     */
	    if (newchoice != NULL)
	      {
		/*   Complete a partial choicepoint. This is not a backtracking
		 * invocation.
		 */
		newchoice->next_instr = w->next_instr;
		newchoice->next_clause = luther_clause_code_noref;
		newchoice->cont_env = w->frame;
		newchoice->arity = 4;
		newchoice->areg[ARSIZE*2] = PointerToTerm(this_def);
		newchoice->areg[ARSIZE*3] = PointerToTerm(this_cls);
	      }
	    else
	      {
		/*   We have backtracked into this choicepoint. Update the
		 * existing choicepoint.
		 */
		w->choice->areg[ARSIZE*2] = PointerToTerm(this_def);
		w->choice->areg[ARSIZE*3] = PointerToTerm(this_cls);
	      }

	    return TRUE;
	  }
	else
	  {
	    /*   The match failed. We have to undo all possible changes to
	     * the state (a restricted form of backtracking).
	     */
	    Unwind_Trail(w->choice->trail_top);

	    w->heap_top = w->choice->global_top;

	    Xw(0) = w->choice->areg[0];
	    Xw(1) = w->choice->areg[ARSIZE*1];

	    this_cls = this_cls->next;

	    /* Try the next clause of the predicate.
	     */
	    goto retry;
	  }
      }
  }

  /*   We have reached the last clause of the predicate. The choicepoint
   * must thus be deallocated.
   */
 trust:
  {
    w->choice = w->choice->last_choice;

#if defined(TIMESTAMP) || defined(UNBOUND)
# if defined(GENERATIONAL)
    if (w->choice->timestamp+TIMEUNIT < w->old_gen_uncond)
      w->uncond = w->old_gen_uncond;
    else
# endif
      w->uncond = w->choice->timestamp + TIMEUNIT;
#else
    w->uncond = w->choice->global_top;
#endif    
    
    return match_term(w, this_cls->clause);
  }
}


/********************************************************************************
 *
 * '$predicate_property'(:HEAD,?PROPERTY)
 * '$predicate_property'(-HEAD,?PROPERTY)
 *
 *   HEAD is the most general goal for an existing predicate, possibly prefixed
 * by a module name, and PROPERTY is a property of that predicate, where the
 * possible properties are one of the atoms `built_in' (for built-in predicates)
 * or `compiled' or `interpreted' (for user defined predicates).
 *   This predicate can be used to enumerate all existing predicates and their
 * properties through backtracking.
 *
 * Note: This predicate is used in conjunction with current_predicate/2 to
 *       achive backtracking.
 *
 *       predicate_property(Name,Prop) :-
 *              current_predicate(_,Name),
 *              '$predicate_property'(Name,Prop).
 *
 */
BOOL luther_predicate_property(Arg)
     Argdecl;
{
  TAGGED head, property;

  TAGGED module = w->global->current_module;

  DerefNLL(head,     Xw(0));
  DerefNLL(property, Xw(1));
       
 start:
  {
    definition *def;

    TAGGED functor;

    switch (TagOf(head)) {

    case HVA:
#if defined(CONSTR)
    case CVA:
#endif
    case SVA:
    case NUM:
      return FALSE;

    case ATM:
      {
	functor = StoreFunctor(head, 0);
      }
      break;

    case BOX:
    case GEN:
      return FALSE;

    case LST:
      {
	functor = StoreFunctor(atom_table_tagged[ATOM_LIST], 2);
      }
      break;

    case STR:
      {
	functor = GetFunctor(head);

	if (functor == functor_colon)
	  {
	    DerefNLL(module, Ref(GetArg(head, 0)));
	    DerefNLL(head,   Ref(GetArg(head, 1)));

	    goto start;
	  }
      }
      break;
    }    
  
    def = get_definition_module(functor, module, w);

    switch (def->enter_instruction) {

    case ENTER_C:
      return unify(atom_table_tagged[ATOM_BUILT_IN], property, w);

    case ENTER_EMULATED:
      return unify(atom_table_tagged[ATOM_COMPILED], property, w);

    case ENTER_TRUE_SPY:
    case ENTER_FAKE_SPY:
    case ENTER_INTERPRETED:
      return unify(atom_table_tagged[ATOM_INTERPRETED], property, w);

    case ENTER_UNDEFINED:
      return FALSE;
    }
  }
}

/*******************************************************************************
 *
 * '$current_predicate'(?Name,?Head,+PREF,+CREF)
 *
 *   This predicate is called on backtracking if current_predicate/2 is
 * non-deterministic.
 *
 */
BOOL luther_current_predicate_aux(Arg)
     Argdecl;
{
  TAGGED name, head;
  TAGGED head_struct, functor;

  definition *def;

  DerefNLL(name, Xw(0));
  DerefNLL(head, Xw(1));
  
  def = (definition *)TermToPointer(Xw(3));


  /* First we build the structure to unify with HEAD.
   */
  {
    int arity = ArityOf(def->name);

    if (arity == 0)
      {
	head_struct = FunctorToAtom(def->name);
      }
    else
      {
	Make_STR(w->heap_top, head_struct, def->name);

	while (arity--)
	  CreateHVA(w->heap_top, w);
      }
  }

  /* Second, we find out if there are any more matching predicates in the database.
   */
  functor = FunctorToAtom(def->name);

  {
    database_iterator iterator;
    BOOL flag_chp = FALSE;

    iterator.index  = GetNumber(Xw(2));
    iterator.def    = def;
    iterator.module = Xw(4);

    /*   The only difference between the two cases when NAME is an atom and when
     * it is a variable is how to determine if there are more matching predicates
     * in the database.
     */
    if (IsVar(name))
      {
	if (next_pred_in_database(&iterator))
	  {
	    flag_chp = TRUE;
	  }
      }
    else /* IsATM(Name) */
      {
	while (next_pred_in_database(&iterator))
	  {
	    if (FunctorToAtom(iterator.def->name) == name)
	      {
		flag_chp = TRUE;
		break;
	      }
	  }
      }
    
    if (flag_chp == TRUE)
      {
	/* The choice point is stil needed.
	 */
	w->choice->areg[ARSIZE*2] = Make_Integer(iterator.index);
	w->choice->areg[ARSIZE*3] = PointerToTerm(iterator.def);
      }
    else
      {
	/* The choice point is not needed any longer.
	 */
	w->choice = w->choice->last_choice;
#if defined(TIMESTAMP) || defined(UNBOUND)
# if defined(GENERATIONAL)
	if (w->choice->timestamp+TIMEUNIT < w->old_gen_uncond)
	  w->uncond = w->old_gen_uncond;
	else
# endif
	  w->uncond = w->choice->timestamp+TIMEUNIT;
#else
	w->uncond = w->choice->global_top;
#endif /* TIMESTAMP | UNBOUND */
      }

    return (unify(name, functor, w) && unify(head, head_struct, w));
  }
}

/*******************************************************************************
 *
 * current_predicate(?Name,:Head)
 * current_predicate(?Name,-Head)
 *
 *   Name is the name of a user defined predicate, and Head is the most general
 * goal for that predicate, possibly prefixed by a module name. This predicate
 * can be used to enumerate all user defined predicates through backtracking.
 *
 */
BOOL luther_current_predicate(Arg)
     Argdecl;
{
  TAGGED name, head;

  TAGGED module = w->global->current_module;

  DerefNLL(name, Xw(0));
  DerefNLL(head, Xw(1));

 start:
  {
    if (IsATM(head))
      {
	/* If the HEAD is instantiated then only one unique predicate can match.
	 */
	definition *def = get_definition_module(StoreFunctor(head, 0), module, w);

	if (def->enter_instruction == ENTER_UNDEFINED) 
	  return FALSE;

	return unify(name, head, w);
      }
    else if (IsSTR(head))
      {
	definition *def;

	TAGGED functor = GetFunctor(head);

	/* Check for module:predicate
	 */
	if (functor == functor_colon)
	  {
	    DerefNLL(module, Ref(GetArg(head, 0)));
	    DerefNLL(head,   Ref(GetArg(head, 1)));

	    goto start;
	  }

	/* If the HEAD is instantiated then only one unique predicate can match.
	 */
	def = get_definition_module(functor, module, w);

	if (def->enter_instruction == ENTER_UNDEFINED) 
	  return FALSE;

	return unify(name, FunctorToAtom(functor), w);
      }
    else  if (IsVar(head))
      {
	/* We use a structure when iterating through the database.
	 */
	database_iterator iterator;

	/*   Initialize the database iterator and find the first entry of
	 * the database.
	 */
	init_database_iterator(&iterator, module);

	/*   If the name is instantiated then only predicates with that
	 * name (but with varying arity) should be matched. If NAME is
	 * uninstantiated we are satisfied with the initial predicate.
	 */
	if (IsATM(name))
	  {
	    forever
	      {
		if (FunctorToAtom(iterator.def->name) == name)
		  {
		    break;
		  }
		else
		  {
		    /*   Fetch the next entry in the database and return FALSE
		     * if it is empty.
		     */
		    if (next_pred_in_database(&iterator) == FALSE)
		      return FALSE;
		  }
	      }
	  }

	/*   Now we need to know if there are more matching predicates in the
	 * database. If there are we must create a choicepoint.
	 */
	{
	  BOOL flag_chp = FALSE;	/* true if choice point is needed */

	  /*
	   *   The functor (predicate name) is saved since we have to iterate
	   * through the database to see if there is another matching entry.
	   * If there is we have to build a choice point.
	   *
	   * Note: Iterating through the database destroys the contents of the
	   *       iterator structure.
	   *
	   */
	  TAGGED functor = iterator.def->name;
      
	  if (IsVar(name))
	    {
	      if (next_pred_in_database(&iterator))
		flag_chp = TRUE;
	    }
	  else /* IsATM(name) */
	    {
	      /*   Iterate until there are no more entries in the database or a
	       * matching entry is found.
	       */
	      while (next_pred_in_database(&iterator))
		{
		  if (FunctorToAtom(iterator.def->name) == name)
		    {
		      flag_chp = TRUE;
		      break;
		    }
		}
	    }
	
	  /* If choice point needed create choice point
	   */
	  if (flag_chp == TRUE)
	    {
	      /* There are more alternatives, we have to build a choicepoint.
	       */
	      choicepoint *newchoice; /* Pointer to new choice point. */
	    
	    
	      newchoice = (choicepoint *) Get_Local_Stack_Top;

	      if (newchoice > (choicepoint *) w->stack_margin)
		{
		  w->gc_info.arity = 2;
		  w->gc_info.env_size = FrameSize(w->next_instr);

		  shift_stack(w, (size_t)newchoice - (size_t)w->stack_margin);

		  newchoice = (choicepoint *) Get_Local_Stack_Top;
		}

	      newchoice->last_choice = w->choice;
	      newchoice->generation = NEW_GEN;
	      newchoice->global_top = w->heap_top;
	      newchoice->trail_top = w->trail_top;
	      newchoice->next_instr = w->next_instr;
	      newchoice->next_clause = luther_retry_current_pred;
	      newchoice->cont_env = w->frame;
	      newchoice->arity = 5;
	      newchoice->areg[ARSIZE*0] = name;
	      newchoice->areg[ARSIZE*1] = head;

	      /* The values of the iterator are saved in the choicepoint
	       */
	      newchoice->areg[ARSIZE*2] = Make_Integer(iterator.index);
	      newchoice->areg[ARSIZE*3] = PointerToTerm(iterator.def);
	      newchoice->areg[ARSIZE*4] = module;
#if defined(TIMESTAMP) || defined(UNBOUND)
	      newchoice->timestamp = w->time;
	      w->time += TIMEUNIT;
	      w->uncond = w->time;
#else
	      w->uncond = w->heap_top;
#endif /* TIMESTAMP */
	  
	      w->choice = newchoice;
	    }
	
	  {
	    TAGGED head_struct;

	    int arity = ArityOf(functor);
	  
	    if (arity == 0)
	      {
		head_struct = FunctorToAtom(functor);
	      }
	    else
	      {
		Make_STR(w->heap_top, head_struct, functor);

		while (arity--)
		  CreateHVA(w->heap_top, w);
	      }
	      
	    return (unify(head, head_struct, w) &&
		    unify(name, FunctorToAtom(functor), w));
	  }
	}
      }
    else
      {
	return FALSE;
      }
  }
}


/*******************************************************************************
 *
 * '$topchoice'(-ChoicePoint)
 *
 *   Unifies ChoicePoint with the term representation of the offset, relative
 * the local stack base, to the current choice point. This is not the same
 * thing as choice_x which refers to the call_choice register.
 *
 */
BOOL luther_topchoice(Arg)
     Argdecl;
{
  TAGGED X0;
  long offset = (TAGGED *)w->choice - w->stack_start;

  DerefNLL(X0, Xw(0));

  assert(IsVar(X0));

  return unify(X0, Make_Integer(offset), w);
}

/*******************************************************************************
 *
 *   The top loop choice point is saved in an internal variable in order to be
 * able to perform `abort' from the tracer and interrupt handler conveniently.
 *
 */
LOCAL TAGGED static_choice;

/*******************************************************************************
 *
 * '$save_choice'(+ChoicePoint) & '$load_choice'(-ChoicePoint)
 *
 */
BOOL luther_save_choice(Arg)
     Argdecl;
{
  TAGGED choice;

  DerefNLL(choice, Xw(0));

  static_choice = choice;
  
  return TRUE;
}

BOOL luther_load_choice(Arg)
     Argdecl;
{
  TAGGED choice;

  DerefNLL(choice, Xw(0));

  return unify(choice, static_choice, w);
}


/*******************************************************************************
 *
 * The following predicates are used by the meta interpreter.
 *
 */

/*******************************************************************************
 *
 * '$retry_choice'(N)
 *
 *   Creates a recognizable choice point and associate the number N with it.
 * This is used during program tracing when a goal is redone.
 *
 */
BOOL luther_retry_choice(Arg)
     Argdecl;
{
  TAGGED level;

  choicepoint *newchoice;

  DerefNLL(level, Xw(0));

  newchoice = (choicepoint *) Get_Local_Stack_Top;

    
  if (newchoice > (choicepoint *) w->stack_margin)
    {
      w->gc_info.arity = 1;
      w->gc_info.env_size = FrameSize(w->next_instr);

      shift_stack(w, (size_t)newchoice - (size_t)w->stack_margin);

      newchoice = (choicepoint *) Get_Local_Stack_Top;
    }
 
  newchoice->last_choice = w->choice;
  newchoice->generation = NEW_GEN;
  newchoice->global_top = w->heap_top;
  newchoice->trail_top = w->trail_top;
  newchoice->next_instr = w->next_instr;
  newchoice->next_clause = luther_retry_choice_code;
  newchoice->cont_env = w->frame;
  newchoice->arity = 1;
  newchoice->areg[0] = level;
#if defined(TIMESTAMP) || defined(UNBOUND)
  newchoice->timestamp = w->time;
  w->time += TIMEUNIT;
  w->uncond = w->time;
#else
  w->uncond = w->heap_top;
#endif
  w->choice = newchoice;

  return TRUE;
}

/*******************************************************************************
 *
 * '$retry_cut'(N)
 *
 *   This predicate removes all choice points more recent than the choice point
 * created by '$retry_choice'(N).
 *
 */
BOOL luther_retry_cut(Arg)
     Argdecl;
{
  TAGGED level;

  choicepoint *c = w->choice;

  DerefNLL(level, Xw(0));
    
  while (c != NULL)
    {
      if (c->next_clause == luther_retry_choice_code)
	{
	  if (c->areg[0] == level)
	    {
	      /* The proper choice point has been found, perform cut.
	       */
	      w->choice = c;
	      TidyTrail;

	      return TRUE;
	    }
	  else if (GetNumber(c->areg[0]) < GetNumber(level))
	    {
	      return FALSE;
	    }
	}
      c = c->last_choice;
    }

  return FALSE;
}


/*******************************************************************************
 *
 * '$set_spy'(+Goal)
 *
 * '$impose_spy'(+Goal)
 *
 * '$remove_spy'(+Goal)
 *
 *
 * Note: Spy points are not supported yet.
 *
 */
BOOL luther_set_spy(Arg)
     Argdecl;
{
  TAGGED goal;
  TAGGED functor;

  definition *def;

  DerefNLL(goal, Xw(0));

  if (IsSTR(goal))
    {
      functor = GetFunctor(goal);
      def = get_definition(functor, w);
    }
  else if (IsATM(goal))
    {
      functor = StoreFunctor(goal, 0);
      def = get_definition(functor, w);
    }
  else
    {
      return FALSE;
    }

  switch (def->enter_instruction) {

  case ENTER_C:
  case ENTER_EMULATED:
    return luther_error(E_PRED_NOT_DYN, (TAGGED)def, w);

  case ENTER_TRUE_SPY:
  case ENTER_FAKE_SPY:
  case ENTER_INTERPRETED:
    {
      def->enter_instruction = ENTER_TRUE_SPY;
    }
    return TRUE;

  case ENTER_UNDEFINED:
    return luther_error(E_PRED_NOT_DEF, (TAGGED)def, w);
  }
}

BOOL luther_impose_spy(Arg)
     Argdecl;
{
  TAGGED goal;
  TAGGED functor;

  definition *def;

  DerefNLL(goal, Xw(0));

  if (IsSTR(goal))
    {
      functor = GetFunctor(goal);
      def = get_definition(functor, w);
    }
  else if (IsATM(goal))
    {
      functor = StoreFunctor(goal, 0);
      def = get_definition(functor, w);
    }
  else
    {
      return FALSE;
    }

  assert(def->enter_instruction == ENTER_TRUE_SPY ||
	 def->enter_instruction == ENTER_INTERPRETED);

  def->enter_instruction = ENTER_FAKE_SPY;

  return TRUE;
}

BOOL luther_remove_spy(Arg)
     Argdecl;
{
  TAGGED goal;
  TAGGED functor;

  definition *def;

  DerefNLL(goal, Xw(0));

  if (IsSTR(goal))
    {
      functor = GetFunctor(goal);
      def = get_definition(functor, w);
    }
  else if (IsATM(goal))
    {
      functor = StoreFunctor(goal, 0);
      def = get_definition(functor, w);
    }
  else
    {
      return FALSE;
    }

  if (def->enter_instruction == ENTER_TRUE_SPY ||
      def->enter_instruction == ENTER_FAKE_SPY)
    {
      def->enter_instruction = ENTER_INTERPRETED;
    }
  return TRUE;
}


/*******************************************************************************
 *
 * '$get_trace_level'(-Level)
 *
 * '$set_inc_trace_level'(+Level) & '$set_dec_trace_level'(+Level)
 *
 *
 * Note: We use an internal variable to keep track of the trace level. 
 *
 */
LOCAL TAGGED trace_level;


BOOL luther_get_trace_level(Arg)
     Argdecl;
{
  TAGGED level;

  DerefNLL(level, Xw(0));

  return unify(level, trace_level,w);
}

BOOL luther_set_inc_trace_level(Arg)
     Argdecl;
{
  TAGGED level;

  DerefNLL(level, Xw(0));

  trace_level = Make_Integer(GetNumber(level) + 1);

  return TRUE;
}

BOOL luther_set_dec_trace_level(Arg)
     Argdecl;
{
  TAGGED level;

  DerefNLL(level, Xw(0));

  trace_level = level;

  return TRUE;
}


/*******************************************************************************
 *
 * '$trace' - true when in trace mode.
 *
 * '$trace_on' & '$trace_spy' & '$trace_off'
 *
 */
BOOL luther_trace(Arg)
     Argdecl;
{
  return (w->trace_mode == LUTHER_TRACE_ON) ? TRUE : FALSE;
}

/* '$trace_on' - set emulator trace mode on.
 */
BOOL luther_trace_on(Arg)
     Argdecl;
{
  w->trace_mode = LUTHER_TRACE_ON;
  return TRUE;
}

/* '$trace_spy' - set emulator trace mode spy.
 */
BOOL luther_trace_spy(Arg)
     Argdecl;
{
  w->trace_mode = LUTHER_TRACE_SPY;
  return TRUE;
}

/* '$trace_off' - set emulator trace mode off.
 */
BOOL luther_trace_off(Arg)
     Argdecl;
{
  w->trace_mode = LUTHER_TRACE_OFF;
  return TRUE;
}


/*******************************************************************************
 *
 * '$set_handler'
 *
 *   Initializes the internal variable used for recognizing on_exception
 * choice points.
 *
 */
BOOL luther_set_handler(Arg)
     Argdecl;
{
  w->global->luther_on_exception_clause = w->choice->next_clause;
  return TRUE;
}

/*******************************************************************************
 *
 * '$find_handler'(-ChoicePoint)
 *
 *   This predicate unifies ChoicePoint with a choice point created by a call
 * to on_exception/3. 
 *
 */
BOOL luther_find_handler(Arg)
     Argdecl;
{
  TAGGED choice;

  choicepoint *chpp = w->choice;

  DerefNLL(choice, Xw(0));
    
  while (chpp != NULL)
    {
      if (chpp->next_clause == w->global->luther_on_exception_clause)
	{
	  integer offset = (TAGGED *)chpp - w->stack_start;

	  return unify(choice, Make_Integer(offset), w);
	}
      chpp = chpp->last_choice;
    }

  return FALSE;
}


/*******************************************************************************
 *
 * garbage_collect -  Force garbage collection.
 *
 */
BOOL luther_garbage_collect(Arg)
     Argdecl;
{
  int gc_start_time = usertime();

  w->gc_info.arity = 0;
  w->gc_info.env_size = FrameSize(w->next_instr);

  garbage_collect(w);

  BarrierSynch(w,3); 

  /* Run_Parallel(W_GC_FINISH); */

  w->stats->heap_gc_time += usertime() - gc_start_time;

  return TRUE;
}


/*******************************************************************************
 *
 * struct_reduce(+Operator,+Array,?Result)
 *
 *   The Result is unified with the result of folding the Array using the
 * operaton Op ("+" or "*"). Array may be any structure containing valid
 * numbers (integers or floats).
 *
 */
BOOL luther_struct_reduce(Arg)
     Argdecl;
{
  TAGGED operator, array, result;

  TAGGED* array_slot;
  integer array_size;

  DerefNLL(operator, Xw(0));
  DerefNLL(array,    Xw(1));
  DerefNLL(result,   Xw(2));

  /*   Perform type check. The `operator' should be an atom and, the `array'
   * should be an array.
   */
  if (not(IsATM(operator) && IsSTR(array)))
    return FALSE;

  /* Initialize loop variables.
   */
  array_size = GetArity(array);
  array_slot = GetArg(array, 0);

  /* 
   *  o   operator == "+"   then sum all elements in the array. 
   *  o   operator == "*"   then multiply all elements in the array. 
   *
   *   If the array consists only of integers then the result should be an integer,
   * but if there is at least one floating point number then the result is a float.
   * At the end of the computation, the float flag should only be true if a float
   * has been encountered. 
   *
   */
  {
    BOOL float_flag = FALSE;
    TAGGED number;
    double value;

    if (operator == atom_table_tagged[ATOM_PLUS])
      {
	for (value = 0.0; array_size--; array_slot += VARSIZE)
	  {
	    DerefNLL(number, Ref(array_slot));

	    switch(TagOf(number)) {

	    case NUM:
	      {
		value += GetNumber(number);
	      }
	      break;

	    case BOX:
	      {
		if (IsFLT(number))
		  {
		    value += GetFloat(number);

		    float_flag = TRUE;
		  }
		else if (IsBIG(number))
		  {
		    value += bntof(GetBignum(number));
		  }
		else
		  {
		    return FALSE;
		  }
	      }
	      break;

	    default:
	      return FALSE;
	    }
	  }
      }
    else if (operator == atom_table_tagged[ATOM_TIMES])
      {
	for (value = 1.0 ; array_size--; array_slot += VARSIZE)
	  {
	    DerefNLL(number, Ref(array_slot));

	    switch(TagOf(number)) {

	    case NUM:
	      {
		value *= GetNumber(number);
	      }
	      break;

	    case BOX:
	      {
		if (IsFLT(number))
		  {
		    value *= GetFloat(number);

		    float_flag = TRUE;
		  }
		else if (IsBIG(number))
		  {
		    value *= bntof(GetBignum(number));
		  }
		else
		  {
		    return FALSE;
		  }
	      }
	      break;

	    default:
	      return FALSE;
	    }
	  }
      }

    /*
     *   If the `float_flag' is true then the calculated value should not be truncated
     * and the result should be a floating point number. Floating point numbers are
     * less precise and the representation consumes more heap space. If no floating
     * point number is required, we create an integer (big number if required).
     */
    if (float_flag)
      return unify(make_float(value, w), result, w);
    else if (value > (double)MAX_NUM)
      return unify(make_bignum_of_float(w, value), result, w);
    else
      return unify(Make_Integer((integer)value), result, w);
  }
}


/*******************************************************************************
 *
 * copy_term(?Term,?CopyOfTerm)
 *
 *   CopyOfTerm is an independent copy of Term, with new variables substituted
 * for all variables in Term.
 *
 */
BOOL luther_copy_term(Arg)
     Argdecl;
{
  register TAGGED term, copy;
  register TAGGED temp, *sp;

  DerefNLL(term, Xw(0));
  DerefNLL(copy, Xw(1));

  switch (TagOf(term)) {

  case HVA:
#if defined(CONSTR)
  case CVA:
#endif
  case SVA:
    {
      LoadHVA(w->heap_top, temp, w);
    }
    break;

  case NUM:
  case ATM:
  case BOX:
    {
      temp = term;
    }
    break;

  case GEN:
    {
      u32 trail_top_offset = w->trail_top - w->trail_start;
      TIMETYPE oldlimit;

#if defined(TIMESTAMP) || defined(UNBOUND)
      oldlimit = w->time;
      w->time += TIMEUNIT;
#else
      oldlimit = w->heap_top;
#endif

      temp = GetMethod(copy, term)(term, oldlimit, w);

      Unwind_Trail(w->trail_start + trail_top_offset);
    }
    break;

  case LST:
    {
      u32 trail_top_offset = w->trail_top - w->trail_start;
      TIMETYPE oldlimit;

#if defined(TIMESTAMP) || defined(UNBOUND)
      w->time += TIMEUNIT;
      oldlimit = w->time;
#else
      oldlimit = w->heap_top;
#endif

      Make_LST_S(w->heap_top, sp, temp);

      luther_copy_args(2, GetCar(term), sp, oldlimit, w);

      Unwind_Trail((w->trail_start+trail_top_offset));
    }
    break;

  case STR:
    {
      u32 trail_top_offset = w->trail_top - w->trail_start;
      TIMETYPE oldlimit;

#if defined(TIMESTAMP) || defined(UNBOUND)
      oldlimit = w->time;
      w->time += TIMEUNIT;
#else
      oldlimit = w->heap_top;
#endif

      Make_STR_Alloc(w->heap_top, sp, temp, GetFunctor(term));

      luther_copy_args(GetArity(term), GetArg(term, 0), sp, oldlimit, w);

      Unwind_Trail((w->trail_start + trail_top_offset));
    }
    break;
  }
  return unify(copy, temp, w);
}


#if defined(SEGMENTED_MEMORY)

static void check_size(size, w)
     long size;
     worker *w;
{
  /*   Check that we do not overwrite the end of the segment, we use a small
   * buffer of 10 word to be extra safe.
   */
  if (w->heap_top + size > (w->heap_end - 10))
    {
#ifdef GENERATIONAL
      (void) new_new_segment(w);
#else
      (void) new_segment(w);
#endif /* GENERATIONAL */
    }

  if (w->heap_top + size > (w->heap_end - 10))
    {
      FatalError("Structure do not fit into segment");
    }
}

#endif /* SEGMENTED_MEMORY */

 
/* Copy all sub arguments to a term
 */
void luther_copy_args(size, term, sp, oldlimit, w)
     register int size;
     register TAGGED *term;
     register TAGGED *sp;
     TIMETYPE oldlimit;
     worker *w;
{
  register TAGGED t;

  while (size--)
    {
      DerefNLL(t, Ref(term));

      switch (TagOf(t))
	{
	case HVA:	/* This must be changed in the parallell version */
	  {
	    TIMETYPE thistime;

	    thistime = GetHVATimeDer(t);

	    if (thistime > oldlimit) /* if already bound */
	      {
		*sp = t;
		sp += VARSIZE;
	      }
	    else
	      {
		register TAGGED var;

		LoadHVA(sp, var, w);

		AlwaysTrail(t);
		AlwaysBind(t, var);
	      }
	  }
	  break;

#if defined(CONSTR)
	case CVA:
#endif
	case SVA:
	  {
	    register TAGGED var;

	    LoadHVA(sp, var, w);

	    AlwaysTrail(t);
	    AlwaysBind(t, var);
	  }
	  break;

	case NUM:
	case ATM:
	case BOX:
	  {
	    *sp = t;
	    sp += VARSIZE;
	  }
	  break;

	case GEN:
	  {
	    *sp = GetMethod(copy, t)(t, oldlimit, w);
	    sp += VARSIZE;
	  }
	  break;

	case LST:
	  {
	    TAGGED *temp;

#if defined(SEGMENTED_MEMORY)
	    check_size(2*VARSIZE, w);
#endif
	    Make_LST_S(w->heap_top, temp, *sp);

	    sp += VARSIZE;
	    luther_copy_args(2, GetCar(t), temp, oldlimit, w);
	  }
	  break;

	case STR:
	  {
	    TAGGED *temp;
	    s32 size;
	    TAGGED func;

	    func = GetFunctor(t);

	    size = ArityOf(func) + 1;

#if defined(SEGMENTED_MEMORY)
	    check_size(size*VARSIZE, w);
#endif
	    Make_STR_Alloc(w->heap_top, temp, *sp, func);

	    sp += VARSIZE;

	    luther_copy_args(GetArity(t), GetArg(t,0), temp, oldlimit, w);
	  }
	  break;
	}
      term += VARSIZE;
    }
}


/*******************************************************************************
 *
 * worker_rest
 *
 *   Put worker in idle mode. If they were actively waiting for commands from
 * the sequential worker this command will make them wait on a system semaphore.
 * This is prefered since other processes can use the processor in that case.
 *
 */
BOOL luther_worker_rest(Arg)
     Argdecl;
{
#if defined(PARALLEL)
  if (w->global->workers_active == TRUE)
    {
      Run_Parallel(W_REST);
      w->global->workers_active = FALSE;
    }
#endif /* PARALLEL */

  return TRUE;
}

/*******************************************************************************
 *
 * active_workers(?Workers)           - Gets the number of active workers.
 * active_workers(?Workers,+Original) - Sets the number of active workers. 
 *
 */
BOOL luther_active_workers(Arg)
     Argdecl;
{
  register TAGGED workers;

  DerefNLL(workers,  Xw(0));

  if (w->pid != 0)
    return FALSE;
    
  return unify(workers, Make_Integer(w->global->active_workers), w);
}

BOOL luther_active_workers_org(Arg)
     Argdecl;
{
  register TAGGED workers, original;
  register int count;

  /* First, unify with max number of workers to allow:
   *    active_workers(X,X)
   */

  DerefNLL(original, Xw(1));

  if (unify(original, Make_Integer(orig_nr_of_workers), w) == FALSE)
      {
	  return FALSE;
      }

  /* Then deal with desired number of workers
   */

  DerefNLL(workers,  Xw(0));

  if (not(IsNUM(workers)))
    return FALSE;

  count = GetNumber(workers);

  if (count < 0 || count > orig_nr_of_workers)
    return FALSE;

  if (w->pid != 0)
    return FALSE;
    
  (void) luther_worker_rest(Arg);

  w->global->active_workers = count;

  return TRUE;
}

/*******************************************************************************
 *
 * scheduling(+Mode) - Mode can be either 'dynamic' or 'static'. 
 *
 */
BOOL luther_scheduling(Arg)
     Argdecl;
{
#if defined(PARALLEL)
  register TAGGED Method;

  DerefNLL(Method,Xw(0));

  if (!IsATM(Method)) return FALSE;

  if (Method = atom_table_tagged[ATOM_DYNAMIC])
    {
      w->global->scheduling = DYNAMIC;
      return TRUE;
    }
  else if (Method = atom_table_tagged[ATOM_STATIC])
    {
      w->global->scheduling = STATIC;
      return TRUE;
    }
  else
#endif
    return FALSE;
}


/*******************************************************************************
 *
 * wamdebug - Start WAM level debugger on sequential worker.
 *
 */
BOOL luther_debug(Arg)
     Argdecl;
{
#if defined(DEBUG)
  w->debug.debugflag = TRUE;
  w->debug.debugmode = DBG_CREEP;

  PL_Print1(currout,"{Debugger will first creep - showing everything}\n");
#else
  PL_Print1(currout,"{Debugger not available}\n");
#endif /* DEBUG */

  return TRUE;
}

/*******************************************************************************
 *
 * wamdebug(+Worker)
 *
 *   Start WAM level debugger on parallel worker number Worker.
 *
 */
BOOL luther_debug_par(Arg)
     Argdecl;
{
#if defined(PARALLEL)
#if defined(DEBUG)
  register TAGGED worker;

  DerefNLL(worker, Xw(0));

  if (not(IsNUM(worker)))
    return FALSE;

  if ((GetNumber(worker) < 1 ) || (GetNumber(worker) > w->global->active_workers))
    BuiltinError("wamdebug/1", " - worker not active", "");

  w->global->parallel_start.code = (code *)GetNumber(worker);

  Run_Parallel(W_DEBUG);
    
  PL_Print1(currout,"{Debugger will first creep - showing everything}\n");
#else
  PL_Print1(currout,"{Debugger not available}\n");
#endif /* DEBUG */
#endif /* PARALLEL */

  return TRUE;
}

/*******************************************************************************
 *
 * wamdebug_file(+Worker,+File)
 *
 *   Start WAM level debugger on parallel worker number Worker and direct
 * output to File.
 *
 */
BOOL luther_debug_par_file(Arg)
     Argdecl;
{
#if defined(PARALLEL)
#if defined(DEBUG)
  register TAGGED worker, file;

  DerefNLL(worker, Xw(0));
  DerefNLL(file,   Xw(1));

  if (not(IsNUM(worker) && IsATM(file)))
    return FALSE;

  if ((GetNumber(worker) < 1 ) || (GetNumber(worker) > w->global->active_workers))
    BuiltinError("wamdebug_file/2", " - worker not active", "");

  w->global->parallel_start.code = (code *) GetNumber(worker);
  w->global->parallel_start.vector = (TAGGED *) file;

  Run_Parallel(W_DEBUG_FILE);
    
  PL_Print1(currout,"{Debugger will first creep - showing everything}\n");
#else
  PL_Print1(currout,"{Debugger not available}\n");
#endif /* DEBUG */
#endif /* PARALLEL */

  return TRUE;
}

/*******************************************************************************
 *
 * wamdebug_socket(+Worker)
 *
 *   Start WAM level debugger on parallel worker number Worker, using a
 * separate socket.
 *
 */
BOOL luther_debug_par_socket(Arg)
     Argdecl;
{
#if defined(DEBUG)
#if defined(HAVE_SOCKETS)
  register TAGGED worker_num;

  DerefNLL(worker_num, Xw(0));

  if (not(IsNUM(worker_num)))
    return FALSE;

  if (GetNumber(worker_num) == 0)
    {
      start_debug_socket(0, w);
    }
#if defined(PARALLEL)
  else if ((GetNumber(worker_num) > 0 ) &&
	   (GetNumber(worker_num) <= w->global->active_workers))
    {
      w->global->parallel_start.code = (code *) GetNumber(worker_num);

      Run_Parallel(W_DEBUG_SOCKET);
    
      PL_Print1(currout,"{Debugger will first creep - showing everything}\n");
    }
#endif /* PARALLEL */
  else
    {
      BuiltinError("wamdebug_socket/1", " - worker not active", "");
    }
#endif /* HAVE_SOCKETS */
#else
  PL_Print1(currout,"{Debugger not available}\n");
#endif /* DEBUG */

  return TRUE;
}

/*******************************************************************************
 *
 * wamdebug_prolog - Start tracer at WAM level. It will not interact with user. 
 *
 */
BOOL luther_debug_prolog(Arg)
     Argdecl;
{
#if defined(DEBUG)
  debug_prolog_terms(w);
#endif
  return TRUE;
}

/*******************************************************************************
 *
 * wamnodebug - Turns of WAM level debugger.
 *
 */
BOOL luther_nodebug(Arg)
     Argdecl;
{
#if defined(DEBUG)
  w->debug.debugflag = FALSE;
#endif
  return TRUE;
}


/*******************************************************************************
 *
 * Initialize prolog flags.
 *
 */
void initialize_flags(w)
     worker *w;
{
  w->global->flags.gc_verbose   = FALSE;
  w->global->flags.load_verbose = FALSE;
}

/*******************************************************************************
 *
 * prolog_flag_gc(?OldValue, +NewValue)
 *
 *   Sets the gc_verbose flag to NewValue. OldValue is unified with the flags
 * old value.
 *
 */
BOOL luther_prolog_flag_gc_verbose(Arg)
     Argdecl;
{
  TAGGED old_value, new_value;

  DerefNLL(old_value, Xw(0));
  DerefNLL(new_value, Xw(1));

  if (not(IsATM(new_value)))
    return FALSE;

  if (w->global->flags.gc_verbose)
    unify(old_value, atom_table_tagged[ATOM_ON], w);
  else
    unify(old_value, atom_table_tagged[ATOM_OFF], w);

  if (new_value == atom_table_tagged[ATOM_ON])
    w->global->flags.gc_verbose = TRUE;
  else if (new_value == atom_table_tagged[ATOM_OFF])
    w->global->flags.gc_verbose = FALSE;
  else
    return FALSE;

  return TRUE;
}

/*******************************************************************************
 *
 * prolog_flag_load(?Oldvalue, +NewValue)
 *
 *   Sets the load_verbose flag to Newvalue. Oldvalue is unified with the flags
 * old value.
 *
 */
BOOL luther_prolog_flag_load_verbose(Arg)
     Argdecl;
{
  TAGGED old_value, new_value;

  DerefNLL(old_value, Xw(0));
  DerefNLL(new_value, Xw(1));

  if (not(IsATM(new_value)))
    return FALSE;

  if (w->global->flags.load_verbose)
    unify(old_value, atom_table_tagged[ATOM_ON], w);
  else
    unify(old_value, atom_table_tagged[ATOM_OFF], w);

  if (new_value == atom_table_tagged[ATOM_ON])
    w->global->flags.load_verbose = TRUE;
  else if (new_value == atom_table_tagged[ATOM_OFF])
    w->global->flags.load_verbose = FALSE;
  else
    return FALSE;

  return TRUE;
}

/*******************************************************************************
 *
 *
 *
 */
BOOL luther_processor_ids(Arg)
     Argdecl;
{
#if defined(ksr1)
  TAGGED worker_num, worker_pid;

  long thread_pid;
  long nr_of_worker;

  DerefNLL(worker_num, Xw(0));
  DerefNLL(worker_pid, Xw(1));

  if (not(IsNUM(worker_num)))
    return FALSE;

  nr_of_worker = GetNumber(worker_num);

  if (worker_nr > orig_nr_of_workers)
    return FALSE;

  if (pthread_getproc(child_pid[nr_of_worker], &thread_pid) == -1)
    {
      MajorError("unable to find thread pid");
    }
  
  return unify(worker_pid, MakeNumber(w, thread_pid), w);
#else
  return FALSE;
#endif
}

BOOL luther_processor_sort(Arg)
     Argdecl;
{
#if defined(ksr1)
  TAGGED worker_list;
  TAGGED car, cdr;

  integer length = 0;

  DerefNLL(worker_list, Xw(0));

  if (not(IsLST(worker_list)))
    return FALSE;

  cdr = worker_list;

  while (IsLST(cdr))
    {
      DerefNLL(car, Ref(GetCar(cdr)));

      if (not(IsNUM(car)))
	return FALSE;

      inc(length);
      
      DerefNLL(cdr, Ref(GetCdr(cdr)));
    }

  if (cdr != atom_nil)
    return FALSE;

  if (length != orig_nr_of_workers)
    return FALSE;

  BuiltinHeapCheck(length*VARSIZE, return FALSE);

  w->global->parallel_start.vector = w->heap_top;
  
  PushOnHeap1(w->heap_top, atom_nil); /* dummy to cover sequential worker */

  cdr = worker_list;

  while (IsLST(cdr))
    {
      DerefNLL(car, Ref(GetCar(cdr)));

      PushOnHeap1(w->heap_top, car);
      
      DerefNLL(cdr, Ref(GetCdr(cdr)));
    }

  Run_Parallel(W_REASSIGN);

  return TRUE;
#else
  return FALSE;
#endif
}


#if defined(THREADED_CODE)
#  define Store_Code(T)   Store_Tagged((code)(global_label_table[(int)(T)]))
#else
#  define Store_Code(T)   Store_Tagged(T)
#endif

#define Store_Tagged(T)				\
{						\
  *(w->global->code_current) = (TAGGED)(T);	\
    w->global->code_current++;			\
}

#define Init_Backtrackable(P,N,A)		\
{						\
  definition *def;				\
  TAGGED functor;				\
  TAGGED batom;					\
						\
  P = w->global->code_current;			\
  Store_Code(EXECUTE);	                        \
  batom = atom_table_tagged[N];			\
  functor = StoreFunctor(batom,A);		\
  def = get_definition(functor,w);		\
  Store_Tagged((TAGGED) def);			\
}    

void init_backtrackable_c(w)
    worker *w;
{
  Init_Backtrackable(luther_clause_code, ATOM_D_CLAUSE, 5);
  Init_Backtrackable(luther_clause_code_noref, ATOM_D_CLAUSE, 4);
  Init_Backtrackable(luther_retry_choice_code, ATOM_TRUE, 0);
  Init_Backtrackable(luther_retry_current_pred, ATOM_D_CURRENT_PREDICATE, 4);
}


void initialize_builtin(w)
     worker *w;
{
  loading_mode  = atom_table_tagged[ATOM_CONSULT];

  functor_d_ref = StoreFunctor(atom_table_tagged[ATOM_D_REF], 2);


  def_c_pred(ATOM_TRUE, luther_true, 0, PUBLIC, w);
  def_c_pred(ATOM_FAIL, luther_fail, 0, PUBLIC, w);
  def_c_pred(ATOM_FALSE, luther_fail, 0, PUBLIC, w);

  def_c_pred(ATOM_NUMBER_CHARS, luther_number_chars, 3, PUBLIC, w);
  def_c_pred(ATOM_ATOM_CHARS, luther_atom_chars, 2, PUBLIC, w);
  def_c_pred(ATOM_NAME, luther_name, 2, PUBLIC, w);

  def_c_pred(ATOM_UNIQUE_NAME, luther_unique_name, 3, PUBLIC, w);

  def_c_pred(ATOM_SEED, luther_seed, 1, PUBLIC, w);
  def_c_pred(ATOM_RANDOM, luther_random, 2, PUBLIC, w);

  def_c_pred(ATOM_HALT, luther_halt, 0, PUBLIC, w);
  def_c_pred(ATOM_VERSION, luther_version, 0, PUBLIC, w);

  def_c_pred(ATOM_D_FREEZE, luther_freeze, 2, PROLOG, w);
  def_c_pred(ATOM_D_FROZEN, luther_frozen, 2, PROLOG, w);
  def_c_pred(ATOM_DEFROST, luther_defrost, 1 ,PUBLIC, w);

  def_c_pred(ATOM_SETARG, luther_setarg, 3, PUBLIC, w);
  def_c_pred(ATOM_ZAPARG, luther_zaparg, 3, PUBLIC, w);

  def_c_pred(ATOM_D_ATOM_MODE, luther_atom_mode, 2, PROLOG, w);
  def_c_pred(ATOM_D_LOADING_MODE, luther_loading_mode, 2, PROLOG, w);

  def_c_pred(ATOM_D_MODULE, luther_module, 2, PROLOG, w);
  def_c_pred(ATOM_D_GET_MODULE, luther_get_module, 1, PROLOG, w);
  def_c_pred(ATOM_D_SET_MODULE, luther_set_module, 1, PROLOG, w);
  def_c_pred(ATOM_D_PUBLIC, luther_public, 2, PROLOG, w);
  def_c_pred(ATOM_D_DYNAMIC, luther_dynamic, 2, PROLOG, w);

  def_c_pred(ATOM_D_ASSERT_DELETE_OTHER, luther_assert_delete_other, 2, PROLOG, w);
  def_c_pred(ATOM_D_ASSERTA, luther_asserta, 4, PROLOG, w);
  def_c_pred(ATOM_D_ASSERTZ, luther_assertz, 4, PROLOG, w);
  def_c_pred(ATOM_D_ERASE, luther_erase, 2, PROLOG, w);

  def_c_pred(ATOM_D_MGU_VARIABLES, luther_mgu_variables, 3, PROLOG, w);

  def_c_pred(ATOM_D_CLAUSE, luther_clause, 5, PROLOG, w);
  def_c_pred(ATOM_D_CLAUSE, luther_clause_noref, 4, PROLOG, w);
  def_c_pred(ATOM_D_PREDICATE_PROPERTY, luther_predicate_property, 2, PROLOG, w);
  def_c_pred(ATOM_D_CURRENT_PREDICATE, luther_current_predicate_aux, 4, PROLOG, w);
  def_c_pred(ATOM_CURRENT_PREDICATE, luther_current_predicate, 2, PUBLIC, w);

  def_c_pred(ATOM_D_TOPCHOICE, luther_topchoice, 1, PROLOG, w);
  def_c_pred(ATOM_D_SAVE_CHOICE, luther_save_choice, 1, PROLOG, w);
  def_c_pred(ATOM_D_LOAD_CHOICE, luther_load_choice, 1, PROLOG, w);

  def_c_pred(ATOM_D_RETRY_CHOICE, luther_retry_choice, 1, PROLOG, w);
  def_c_pred(ATOM_D_RETRY_CUT, luther_retry_cut, 1, PROLOG, w);

  def_c_pred(ATOM_D_SET_SPY, luther_set_spy, 1, PROLOG, w);
  def_c_pred(ATOM_D_IMPOSE_SPY, luther_impose_spy, 1, PROLOG, w);
  def_c_pred(ATOM_D_REMOVE_SPY, luther_remove_spy, 1, PROLOG, w);

  def_c_pred(ATOM_D_GET_TRACE_LEVEL, luther_get_trace_level, 1, PROLOG, w);
  def_c_pred(ATOM_D_SET_INC_TRACE_LEVEL, luther_set_inc_trace_level, 1, PROLOG, w);
  def_c_pred(ATOM_D_SET_DEC_TRACE_LEVEL, luther_set_dec_trace_level, 1, PROLOG, w);

  def_c_pred(ATOM_D_TRACE, luther_trace, 0, PROLOG, w);
  def_c_pred(ATOM_D_TRACE_ON, luther_trace_on, 0, PROLOG, w);
  def_c_pred(ATOM_D_TRACE_SPY, luther_trace_spy, 0, PROLOG, w);
  def_c_pred(ATOM_D_TRACE_OFF, luther_trace_off, 0, PROLOG, w);

  def_c_pred(ATOM_D_SET_HANDLER, luther_set_handler, 0, PROLOG, w);
  def_c_pred(ATOM_D_FIND_HANDLER, luther_find_handler, 1, PROLOG, w);

  def_c_pred(ATOM_GARBAGE_COLLECT, luther_garbage_collect, 0, PUBLIC, w);

  def_c_pred(ATOM_STRUCT_REDUCE, luther_struct_reduce, 3, PUBLIC, w); 

  def_c_pred(ATOM_COPY_TERM, luther_copy_term, 2, PUBLIC, w);

  def_c_pred(ATOM_WORKER_REST, luther_worker_rest, 0, PUBLIC, w);
  def_c_pred(ATOM_ACTIVE_WORKERS, luther_active_workers, 1, PUBLIC, w);
  def_c_pred(ATOM_ACTIVE_WORKERS, luther_active_workers_org, 2, PUBLIC, w);
  def_c_pred(ATOM_SCHEDULE, luther_scheduling, 1, PUBLIC, w);

  def_c_pred(ATOM_WAMDEBUG, luther_debug, 0, PUBLIC, w);
  def_c_pred(ATOM_WAMDEBUG, luther_debug_par, 1, PUBLIC, w);
  def_c_pred(ATOM_WAMDEBUG_FILE, luther_debug_par_file, 2, PUBLIC, w);
  def_c_pred(ATOM_WAMDEBUG_SOCKET, luther_debug_par_socket, 1, PUBLIC, w);
  def_c_pred(ATOM_WAMDEBUG_PROLOG, luther_debug_prolog, 0, PUBLIC, w);
  def_c_pred(ATOM_WAMNODEBUG, luther_nodebug, 0, PUBLIC, w);

  def_c_pred(ATOM_PROLOG_FLAG_GC_VERBOSE, luther_prolog_flag_gc_verbose, 2, PUBLIC, w);
  def_c_pred(ATOM_PROLOG_FLAG_LOAD_VERBOSE, luther_prolog_flag_load_verbose, 2, PUBLIC, w);

  def_c_pred(ATOM_PROCESSOR_IDS, luther_processor_ids, 2, PUBLIC, w);
  def_c_pred(ATOM_PROCESSOR_SORT, luther_processor_sort, 1, PUBLIC, w);
}
