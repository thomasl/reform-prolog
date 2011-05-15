/*
 * inline.c 
 *
 * Johan Bevemyr.....Wed Jun  5 1991
 * Patric Hedlin.....Mon Jan 23 1995
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include <math.h>

#include "luther.h"
#include "inline.h"
#include "bignum.h"
#include "engine.h"
#include "initial.h"
#include "unify.h"
#include "array.h"
#include "math.h"



GLOBAL TAGGED float_inf, float_nan;


static CompareType compare_term PROTO((TAGGED, TAGGED, worker *));

static BOOL luther_compare PROTO((InArgproto));
static BOOL luther_functor PROTO((InArgproto));
static BOOL luther_arg PROTO((InArgproto));
static BOOL luther_univ PROTO((InArgproto));
static BOOL luther_inline_unify PROTO((InArgproto));

static BOOL luther_eq_univ PROTO((InArgproto));
static BOOL luther_ineq_univ PROTO((InArgproto));
static BOOL luther_lt_univ PROTO((InArgproto));
static BOOL luther_gt_univ PROTO((InArgproto));
static BOOL luther_le_univ PROTO((InArgproto));
static BOOL luther_ge_univ PROTO((InArgproto));

static BOOL luther_math_eq PROTO((InArgproto));
static BOOL luther_math_ineq PROTO((InArgproto));
static BOOL luther_lt PROTO((InArgproto));
static BOOL luther_gt PROTO((InArgproto));
static BOOL luther_le PROTO((InArgproto));
static BOOL luther_ge PROTO((InArgproto));

static BOOL luther_var PROTO((InArgproto));
static BOOL luther_atom PROTO((InArgproto));
static BOOL luther_atomic PROTO((InArgproto));
static BOOL luther_nonvar PROTO((InArgproto));
static BOOL luther_number PROTO((InArgproto));
static BOOL luther_float PROTO((InArgproto));
static BOOL luther_integer PROTO((InArgproto));
static BOOL luther_generic PROTO((InArgproto));

static BOOL luther_plus PROTO((InArgproto));
static BOOL luther_plus_1 PROTO((InArgproto));
static BOOL luther_minus PROTO((InArgproto));
static BOOL luther_minus_1 PROTO((InArgproto));
static BOOL luther_times PROTO((InArgproto));
static BOOL luther_div PROTO((InArgproto));
static BOOL luther_intdiv PROTO((InArgproto));
static BOOL luther_mod PROTO((InArgproto));
static BOOL luther_min PROTO((InArgproto));
static BOOL luther_max PROTO((InArgproto));
static BOOL luther_tointeger PROTO((InArgproto));
static BOOL luther_tofloat PROTO((InArgproto));
static BOOL luther_floor PROTO((InArgproto));
static BOOL luther_ceiling PROTO((InArgproto));
static BOOL luther_truncate PROTO((InArgproto));
static BOOL luther_round PROTO((InArgproto));

static BOOL luther_eval_math PROTO((InArgproto));

static BOOL luther_min PROTO((InArgproto));
static BOOL luther_max PROTO((InArgproto));
static BOOL luther_abs PROTO((InArgproto));
static BOOL luther_neg PROTO((InArgproto));
static BOOL luther_log PROTO((InArgproto));
static BOOL luther_exp PROTO((InArgproto));
static BOOL luther_pow PROTO((InArgproto));
static BOOL luther_sqrt PROTO((InArgproto));
static BOOL luther_cbrt PROTO((InArgproto));

static BOOL luther_sin PROTO((InArgproto));
static BOOL luther_cos PROTO((InArgproto));
static BOOL luther_tan PROTO((InArgproto));
static BOOL luther_asin PROTO((InArgproto));
static BOOL luther_acos PROTO((InArgproto));
static BOOL luther_atan PROTO((InArgproto));

static BOOL luther_bin_or PROTO((InArgproto));
static BOOL luther_bin_and PROTO((InArgproto));
static BOOL luther_bin_xor PROTO((InArgproto));
static BOOL luther_bin_not PROTO((InArgproto));
static BOOL luther_lshift PROTO((InArgproto));
static BOOL luther_rshift PROTO((InArgproto));
static BOOL luther_msb PROTO((InArgproto));
static BOOL luther_rng PROTO((InArgproto));

static BOOL luther_aref PROTO((InArgproto));

static BOOL luther_active PROTO((InArgproto));

static BOOL luther_save PROTO((InArgproto));

static BOOL luther_collect_plus PROTO((InArgproto));
static BOOL luther_collect_times PROTO((InArgproto));

/* */

LOCAL TAGGED functor_plus, functor_minus, functor_times, functor_div;
LOCAL TAGGED functor_intdiv, functor_mod, functor_integer, functor_float;
LOCAL TAGGED functor_floor, functor_ceiling, functor_truncate, functor_round;
LOCAL TAGGED functor_min, functor_max, functor_abs, functor_neg;
LOCAL TAGGED functor_log, functor_exp, functor_pow, functor_sqrt, functor_cbrt;
LOCAL TAGGED functor_sin, functor_cos, functor_tan;
LOCAL TAGGED functor_asin, functor_acos, functor_atan;
LOCAL TAGGED functor_bin_or, functor_bin_and, functor_bin_xor, functor_bin_not;
LOCAL TAGGED functor_lshift, functor_rshift, functor_msb, functor_rng;
LOCAL TAGGED functor_aref;


inline_entry inline_table[INLINE_TABLE_SIZE] = {
  {"$compare",		3, luther_compare,		I_PRED, 0},
  {"$functor",		3, luther_functor,		I_PRED, 0},
  {"$arg",		3, luther_arg,			I_PRED, 0},
  {"$univ",		2, luther_univ,			I_PRED, 0},
  {"$unify",		2, luther_inline_unify,		I_PRED, 0},

  {"$eq_univ",		2, luther_eq_univ,		I_PRED, 0},
  {"$ineq_univ",	2, luther_ineq_univ,		I_PRED, 0},
  {"$lt_univ",		2, luther_lt_univ,		I_PRED, 0},
  {"$gt_univ",		2, luther_gt_univ,		I_PRED, 0},
  {"$le_univ",		2, luther_le_univ,		I_PRED, 0},
  {"$ge_univ",		2, luther_ge_univ,		I_PRED, 0},

  {"$eq",		2, luther_math_eq,		I_PRED, 0},
  {"$ineq",		2, luther_math_ineq,		I_PRED, 0},
  {"$lt",		2, luther_lt,			I_PRED, 0},
  {"$gt",		2, luther_gt,			I_PRED, 0},
  {"$le",		2, luther_le,			I_PRED, 0},
  {"$ge",		2, luther_ge,			I_PRED, 0},

  {"$var",		1, luther_var,			I_PRED, 0},
  {"$atom",		1, luther_atom,			I_PRED, 0},
  {"$atomic",		1, luther_atomic,		I_PRED, 0},
  {"$nonvar",		1, luther_nonvar,		I_PRED, 0},
  {"$number",		1, luther_number,		I_PRED, 0},
  {"$float",		1, luther_float,		I_PRED, 0},
  {"$integer",		1, luther_integer,		I_PRED, 0},
  {"$generic",		1, luther_generic,		I_PRED, 0},

  {"$plus",		3, luther_plus,			I_FUNC, 1},
  {"$plus_1",		2, luther_plus_1,		I_FUNC, 1},
  {"$minus",		3, luther_minus,		I_FUNC, 1},
  {"$minus_1",		2, luther_minus_1,		I_FUNC, 1},
  {"$times",		3, luther_times,		I_FUNC, 1},
  {"$div",		3, luther_div,			I_FUNC, 1},
  {"$intdiv",		3, luther_intdiv,		I_FUNC, 1},
  {"$mod",		3, luther_mod,			I_FUNC, 1},
  {"$tointeger",	2, luther_tointeger,		I_FUNC, 1},
  {"$tofloat",		2, luther_tofloat,		I_FUNC, 1},
  {"$floor",		2, luther_floor,		I_FUNC, 1},
  {"$ceiling",		2, luther_ceiling,	        I_FUNC, 1},
  {"$truncate",		2, luther_truncate,	        I_FUNC, 1},
  {"$round",		2, luther_round,	        I_FUNC, 1},
  {"$eval_math",	2, luther_eval_math,		I_FUNC, 1},
  {"$min",		3, luther_min,			I_FUNC, 1},
  {"$max",		3, luther_max,			I_FUNC, 1},
  {"$abs",		2, luther_abs,			I_FUNC, 1},
  {"$neg",		2, luther_neg,			I_FUNC, 1},
  {"$log",		2, luther_log,			I_FUNC, 1},
  {"$exp",		2, luther_exp,			I_FUNC, 1},
  {"$pow",		3, luther_pow,			I_FUNC, 1},
  {"$sqrt",		2, luther_sqrt,		        I_FUNC, 1},
  {"$cbrt",		2, luther_cbrt,		        I_FUNC, 1},

  {"$sin",		2, luther_sin,			I_FUNC, 1},
  {"$cos",		2, luther_cos,			I_FUNC, 1},
  {"$tan",		2, luther_tan,			I_FUNC, 1},
  {"$asin",		2, luther_asin,			I_FUNC, 1},
  {"$acos",		2, luther_acos,			I_FUNC, 1},
  {"$atan",		2, luther_atan,			I_FUNC, 1},

  {"$b_or",		3, luther_bin_or,		I_FUNC, 1},
  {"$b_and",		3, luther_bin_and,		I_FUNC, 1},
  {"$b_xor",		3, luther_bin_xor,		I_FUNC, 1},
  {"$b_not",		2, luther_bin_not,	        I_FUNC, 1},
  {"$lshift",		3, luther_lshift,		I_FUNC, 1},
  {"$rshift",		3, luther_rshift,		I_FUNC, 1},
  {"$msb",		2, luther_msb,		        I_FUNC, 1},
  {"$rng",		4, luther_rng,		        I_FUNC, 1},

  {"$aref",		3, luther_aref,			I_FUNC, 1},

  {"$active",		1, luther_active,		I_FUNC, 1},

  {"$save",		1, luther_save,			I_PRED, 0},

  {"$collect_plus",	1, luther_collect_plus,		I_PRED, 0},
  {"$collect_times",	1, luther_collect_times,	I_PRED, 0},
};

/* compare/3 */
/* The order is as follows:

   Variables, ordered by age

   Integers, in numeric order

   Floats, in numeric order

   Atoms, in alphabetical order

   Compound terms, ordered first by arity, then by name of the
   principal functor, then by the arguments (in left-to-right order).
   Remember that a list is a structure with fuctor '.'.
   
   0 = Less
   1 = Greater

         H  C  S  N  A  B  G  L  S
         V  V  V  U  T  O  E  S  T
         A  A  A  M  M  X  N  T  R
   HVA { 2,13, 0, 0, 0, 0, 0, 0, 0 }
   CVA {13,13, 0, 0, 0, 0, 0, 0, 0 }
   SVA {15, 1, 3, 0, 0, 0, 0, 0, 0 }
   NUM { 1, 1, 1, 4, 0, 0,12, 0, 0 }
   ATM { 1, 1, 1, 1, 6, 1,12, 0, 0 }
   BOX { 1, 1, 1, 1, 0, 5,12, 0, 0 }
   GEN { 1, 1, 1,11,11,11,11,11,11 }
   LST { 1, 1, 1, 1, 1, 1,12, 7, 8 }
   STR { 1, 1, 1, 1, 1, 1,12, 9,10 }

 */           

#if defined(CONSTR)
#  define C(X,Y) X,Y
#else
#  define C(X,Y) Y
#endif

#if defined(UNBOUND)
#  define U(X,Y) X,Y
#else
#  define U(X,Y) X
#endif
  
LOCAL int compare_table[MAX_TAG][MAX_TAG] = {
    { 2, C(13, 0), 0, 0, 0, 0, 0, U( 0, 0)},
#if defined(CONSTR)
    {13, C(13, 0), 0, 0, 0, 0, 0, U( 0, 0)},
#endif
    {15, C( 1, 3), 0, 0, 0, 0, 0, U( 0, 0)},
    { 1, C( 1, 1), 4, 0, 0,12, 0, U( 0, 0)},
    { 1, C( 1, 1), 1, 6, 1,12, 0, U( 0, 0)},
    { 1, C( 1, 1), 1, 0, 5,12, 0, U( 0, 0)},
    { 1, C( 1, 1),11,11,11,11,11, U(11, 0)},
    { 1, C( 1, 1), 1, 1, 1,12, 7, U( 8, 0)},
    { 1, C( 1, 1), 1, 1, 1,12, 9, U(10, 0)}
#if defined(UNBOUND)
,   { 0, C( 0, 0), 0, 0, 0, 0, 0, U( 0, 0)}
#endif
};

           
CompareType compare_struct(term1,term2,arity,w)
    register TAGGED *term1, *term2;
    register int arity;
    worker *w;
{
  register TAGGED dt1, dt2;
  register CompareType rem;

  while (arity--)
    {
      DerefNLL(dt1,Ref(term1));
      DerefNLL(dt2,Ref(term2));
      rem = compare_term(dt1,dt2,w);
      if (rem != COMPARE_EQUAL)
	return rem;
      term1 += VARSIZE; term2 += VARSIZE;
    }
  return COMPARE_EQUAL;
}
	

/* compare_term(t1,t2,w)
 * 
 * Returns one of:  
 *
 *    COMPARE_LESS      = -1,
 *    COMPARE_EQUAL     =  0,
 *    COMPARE_GREATER   =  1,
 *    COMPARE_UNDEFINED =  2
 * 
 * Compares the terms t1 and t2. If copying gc is used, then
 * a variables position on the heap cannot be used for determining
 * its age. Instead a combination of timestamps and ordering numbers
 * are used.
 * 
 * When two variables are compared the one with the least timestamp
 * is less. If they have equal timestamps then one of them is bound
 * to a variable in the static area. The new variables is ordered
 * in the sence that it is preceeded by a special ordering object.
 *
 *        -------------------------
 *        | ATM | 01 | order nr   |
 *        -------------------------
 *        | UVA | timestamp       |
 *        -------------------------
 *
 * The ordering number is recognised by its ATM tag and its 01 subtag.
 * Only orderingnumbers have this subtag.
 *
 * The binding to the ordered variable is not trailed. Thus the ordered
 * variable has to be stored in a static area that is not deallocated 
 * on backtracking.
 */

static CompareType compare_term_wrapped(term1,term2,w)
    TAGGED term1, term2;
    worker *w;
{
  if (term1 == term2)
    return COMPARE_EQUAL;
  else
    {
      switch(compare_table[TagOf(term1)][TagOf(term2)]) {

      case 0:
	return COMPARE_LESS;

      case 1:
	return COMPARE_GREATER;

      case 2: /* HVA -- HVA */
	{
#if defined(COPY_GC)

	  /*   If we are using a copying collector we cannot use a variables
	   * location on the heap for deciding whether which is greather than
	   * another (variables may have switched their relative positions on
	   * the heap after a gc).
	   *   When a variable is compared it is migrated to a special area
	   * in which variables relative positions are maintained. The old
	   * variable is unconditionally bound (i.e. without trailing) to
	   * the new location. Its timestamp is preserved.
	   */
	  
	  /*   First, compare their timestamps. If they are equal then see
	   * if any variable has been put on the compared stack. If only one
	   * variable is on the CVS the that one is older. If both are on the
	   * stack then their relative position has to decide. If none has
	   * been put on the CVS then pick one for putting there and make
	   * that one older.
	   */
	  
	  uword x_time, y_time;

	  x_time = GetHVATime(*RemoveTag(term1,HVA));
	  y_time = GetHVATime(*RemoveTag(term2,HVA));

	  if (x_time < y_time)
	    return COMPARE_LESS;
	  else if (x_time > y_time)
	    return COMPARE_GREATER;
	  else
	    {
	      if (IsOrdered(term1))
		if (IsOrdered(term2))
		  if(GetOrderNr(term1) < GetOrderNr(term2))
		    return COMPARE_LESS;
		  else
		    return COMPARE_GREATER;
		else
		  return COMPARE_LESS;
	      else
		if (IsOrdered(term2))
		  return COMPARE_GREATER;
		else
		  {
		    /* put term1 on stack */
		    Order(term1);
		    return COMPARE_LESS;
		  }
	    }
#else /* not COPY_GC */
	  if (term1 < term2)
	    return COMPARE_LESS;
	  else
	    return COMPARE_GREATER;
#endif /* COPY_GC */
	}

      case 3:
	{
	  if (term1 < term2)
	    {
#ifdef COPY_GC	    
	      TAGGED tmp;

	      LoadHVA(w->heap_top,tmp,w);
	      Bind_SVA(term1,tmp);
	      Order(tmp);
#endif 
	      return COMPARE_LESS;
	    }
	  else
	    {
#ifdef COPY_GC	    
	      TAGGED tmp;

	      LoadHVA(w->heap_top,tmp,w);
	      Bind_SVA(term2,tmp);
	      Order(tmp);
#endif 
	      return COMPARE_GREATER;
	    }
	}

      case 4: /* NUM - NUM */
	{
	  if (GetNumber(term1) < GetNumber(term2))
	    return COMPARE_LESS;
	  if (GetNumber(term1) > GetNumber(term2))
	    return COMPARE_GREATER;
	}
	return COMPARE_UNDEFINED;

      case 5: /* BOX - BOX */
	{
	  if (IsBIG(term1))
	    {
	      if (IsBIG(term2))
		{
		  int res = bignum_cmp(GetBignum(term1), GetBignum(term2));

		  return res == 0 ? COMPARE_EQUAL :
		    (res < 0 ? COMPARE_LESS : COMPARE_GREATER);
		}
	      else if (IsFLT(term2))
		{
		  return COMPARE_LESS;
		}
	    }
	  else if (IsFLT(term1))
	    {
	      if (IsFLT(term2))
		{
		  double fval1 = GetFloat(term1);
		  double fval2 = GetFloat(term2);

		  return fval1 < fval2 ? COMPARE_LESS :
		    (fval1 > fval2 ? COMPARE_GREATER : COMPARE_EQUAL);
		}
	      else if (IsBIG(term2))
		{
		  return COMPARE_GREATER;
		}
	    }
	}

      case 6: /* ATM - ATM */
	{
	  int res = strcmp(GetString(term1,w),GetString(term2,w));
	  
	  if (res < 0)
	    return COMPARE_LESS;
	  if (res > 0)
	    return COMPARE_GREATER;
	}
	return COMPARE_UNDEFINED;

      case 7: /* LST - LST */
	return compare_struct(RemoveTag(term1,LST),RemoveTag(term2,LST),2,w);

      case 8: /* LST - STR */
	{
	  int res;
	  int arity = StructArity(RemoveTag(term2,STR));

	  if (arity < 2)
	    return COMPARE_GREATER;
	  if (arity > 2)
	    return COMPARE_LESS;
	  
	  res = strcmp(".",StructString(term2));

	  if (res < 0)
	    return COMPARE_LESS;
	  if (res > 0)
	    return COMPARE_GREATER;

	  Error("compare_term - structure looks like list but isn't!");
	}
	return COMPARE_UNDEFINED;

      case 9: /* STR - LST */
	{
	  int res;
	  int arity = StructArity(RemoveTag(term1,STR));

	  if (arity < 2)
	    return COMPARE_LESS;
	  if (arity > 2)
	    return COMPARE_GREATER;

	  res = strcmp(".",StructString(term1));

	  if (res < 0)
	    return COMPARE_GREATER;
	  if (res > 0)
	    return COMPARE_LESS;

	  Error("compare_term - structure looks like list but isn't!");
	}
	return COMPARE_UNDEFINED;

      case 10: /* STR - STR */
	{
	  int a1, a2, res;
	  a1 = StructArity(RemoveTag(term1,STR));
	  a2 = StructArity(RemoveTag(term2,STR));

	  if (a1 < a2)
	    return COMPARE_GREATER;
	  if (a1 > a2)
	    return COMPARE_GREATER;

	  if (GetFunctor(term1) == GetFunctor(term2))
	    return compare_struct(GetArg(term1,0),GetArg(term2,0), a1,w);

	  res = strcmp(StructString(term1),StructString(term2));

	  if (res < 0)
	    return COMPARE_LESS;
	  if (res > 0)
	    return COMPARE_GREATER;

	  Error("compare_term - same arity same name but term1 != term2");
	}
	return COMPARE_UNDEFINED;

      case 11: /* GEN - ANY (but variable) */
	return GetMethod(compare,term1)(term1,term2,w);

      case 12: /* ANY - GEN */
	{
	  CompareType res = GetMethod(compare,term2)(term2,term1,w);
	  
	  if (res == COMPARE_LESS)
	    return COMPARE_GREATER;
	  if (res == COMPARE_GREATER)
	    return COMPARE_LESS;
	  else
	    return res;
	}
	
#if defined(CONSTR)
      case 13: /* CVA - CVA */
	{
	  if (RemoveTag(term1,CVA) < RemoveTag(term2,CVA))
	    return COMPARE_LESS;
	  else
	    return COMPARE_GREATER;
	}
#endif /* CONSTR */

      case 14:
	{
#ifdef COPY_GC
	  if (!IsOrdered(term1))
	    Order(term1);
#endif
	}
	return COMPARE_LESS;

      case 15:
	{
#ifdef COPY_GC
	  if (!IsOrdered(term2))
	    Order(term2);
#endif
	}
	return COMPARE_GREATER;

      default:
	{
	  Error("compare_term - compare index table broken");
	}
	return COMPARE_UNDEFINED;

      }
    }
}

static CompareType compare_term(term1,term2,w)
    TAGGED term1, term2;
    worker *w;
{
  CompareType ret;

  ret = compare_term_wrapped(term1,term2,w);

  return ret;
}

static BOOL luther_compare(InArg)
     InArgdecl;
{
  TAGGED operator, term1, term2;
  CompareType result;

  DerefNLL(operator, Xw(regs[0]));
  DerefNLL(term1,    Xw(regs[1]));
  DerefNLL(term2,    Xw(regs[2]));

  result = compare_term(term1,term2,w);

  if(IsVar(operator))
    {
      switch(result)
	{
	case COMPARE_LESS:
	  Bind(operator, atom_less, { return luther_compare(InArg); });
	  return TRUE;
	case COMPARE_EQUAL:
	  Bind(operator, atom_equal,{ return luther_compare(InArg); });
	  return TRUE;
	case COMPARE_GREATER:
	  Bind(operator, atom_greater,{ return luther_compare(InArg); });
	  return TRUE;
	case COMPARE_UNDEFINED:
	  Drop_P_Lock(operator, operator);
	  return FALSE;
	}
    }
  else
    {
      switch(result)
	{
	case COMPARE_LESS:
	  if (operator == atom_less) return TRUE;
	  break;
	case COMPARE_EQUAL:
	  if (operator == atom_equal) return TRUE;
	  break;
	case COMPARE_GREATER:
	  if (operator == atom_greater) return TRUE;
	  break;
	case COMPARE_UNDEFINED:
	  return FALSE;
	}
      return FALSE;
    }

  return FALSE;
}


static BOOL luther_eq_univ(InArg)
     InArgdecl;
{
  TAGGED X0, X1;

  DerefNLL(X0,Xw(regs[0]));	/* Term1 */
  DerefNLL(X1,Xw(regs[1]));	/* Term2 */

  if (compare_term(X0,X1,w) == COMPARE_EQUAL) return TRUE;

  return FALSE;
}


static BOOL luther_ineq_univ(InArg)
     InArgdecl;
{
  TAGGED X0, X1;

  DerefNLL(X0,Xw(regs[0]));	/* Term1 */
  DerefNLL(X1,Xw(regs[1]));	/* Term2 */

  if (compare_term(X0,X1,w) != COMPARE_EQUAL) return TRUE;

  return FALSE;
}


/* @< */
static BOOL luther_lt_univ(InArg)
     InArgdecl;
{
  TAGGED X0, X1;

  DerefNLL(X0,Xw(regs[0]));	/* Term1 */
  DerefNLL(X1,Xw(regs[1]));	/* Term2 */

  if (compare_term(X0,X1,w) == COMPARE_LESS) return TRUE;

  return FALSE;
}


/* @> */
static BOOL luther_gt_univ(InArg)
     InArgdecl;
{
  TAGGED X0, X1;

  DerefNLL(X0,Xw(regs[0]));	/* Term1 */
  DerefNLL(X1,Xw(regs[1]));	/* Term2 */

  if (compare_term(X0,X1,w) == COMPARE_GREATER) return TRUE;

  return FALSE;
}


/* @=< */
static BOOL luther_le_univ(InArg)
     InArgdecl;
{
  TAGGED X0, X1;

  DerefNLL(X0,Xw(regs[0]));	/* Term1 */
  DerefNLL(X1,Xw(regs[1]));	/* Term2 */

  if (compare_term(X0,X1,w) != COMPARE_GREATER) return TRUE;

  return FALSE;
}


/* @>= */
static BOOL luther_ge_univ(InArg)
     InArgdecl;
{
  TAGGED X0, X1;

  DerefNLL(X0,Xw(regs[0]));	/* Term1 */
  DerefNLL(X1,Xw(regs[1]));	/* Term2 */

  if (compare_term(X0,X1,w) != COMPARE_LESS) return TRUE;

  return FALSE;
}


/* $univ(Term,List) (=..) */
static BOOL luther_univ(InArg)
     InArgdecl;
{
  TAGGED Term,List,Functor,Car,Cdr,Str;
  int arity;

  DerefNLL(Term, Xw(regs[0]));
  DerefNLL(List, Xw(regs[1]));

  switch(TagOf(Term))
    {
    case HVA:
#if defined(CONSTR)
    case CVA:
#endif
    case SVA:
      {
	if (!IsLST(List)) goto error;

	/* Check arguments */
	DerefNLL(Cdr, Ref(GetCdr(List)));

	for (arity = 0; IsLST(Cdr); arity++)
	  DerefNLL(Cdr,Ref(GetCdr(Cdr)));

	if (Cdr != atom_nil) goto error;
	
	DerefNLL(Functor, Ref(GetCar(List)));

	if ((arity == 0))
	  {
	    if (IsATM(Functor) || IsNumber(Functor))
	      return unify(Term,Functor,w);
	    else
	      return FALSE;
	  }
	else if (!IsATM(Functor)) 
	  goto error;
	
	/* check if list */
	if ((Functor == atom_list) && (arity == 2))
	  {
	    Make_LST(w->heap_top,Str);

	    DerefNLL(Cdr,Ref(GetCdr(List)));
	    DerefNLL(Car,Ref(GetCar(Cdr)));
	    DerefNLL(Cdr,Ref(GetCdr(Cdr)));
	    PushOnHeap(w->heap_top,Car);
	    DerefNLL(Car,Ref(GetCar(Cdr)));
	    PushOnHeap(w->heap_top,Car);
	    
	    return unify(Term,Str,w);
	  }
	else /* Make heap term. */
	  {
	    BuiltinHeapCheck(arity*VARSIZE+VARSIZE, return FALSE);

	    Make_STR(w->heap_top,Str,StoreFunctor(Functor,arity));
	    DerefNLL(Cdr,Ref(GetCdr(List)));

	    while (arity--)
	      {
		DerefNLL(Car,Ref(GetCar(Cdr)));
		PushOnHeap(w->heap_top,Car);
		DerefNLL(Cdr,Ref(GetCdr(Cdr)));
	      }

	    return unify(Term,Str,w);
	  }
      }
      break;
	
    case NUM:
    case ATM:
    case BOX:
      {
	Make_LST(w->heap_top,Str);

	PushOnHeap(w->heap_top,Term);
	PushOnHeap(w->heap_top,atom_nil);

	return unify(Str,List,w);
      }
      break;
	
    case LST:
      {
	Make_LST(w->heap_top,Str);

	PushOnHeap(w->heap_top, atom_list);
	PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));
	PushOnHeap(w->heap_top, Ref(GetCar(Term)));
	PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));
	PushOnHeap(w->heap_top, Ref(GetCdr(Term)));
	PushOnHeap(w->heap_top, atom_nil);

	return unify(Str,List,w);
      }
      break;
	
    case STR:
      {
	TAGGED *strarg;
	    
	strarg = GetArg(Term,0);
	arity  = GetArity(Term);
	    
	BuiltinHeapCheck(2*arity*VARSIZE,return FALSE);

	Make_LST(w->heap_top,Str);
	PushOnHeap(w->heap_top,GetSTRatom(Term));

	while (arity--)
	  {
	    PushOnHeap(w->heap_top,Tagify(w->heap_top+VARSIZE,LST));
	    PushOnHeap(w->heap_top,Ref(strarg));
	    strarg += VARSIZE;
	  }
	PushOnHeap(w->heap_top,atom_nil);
	  
	return unify(Str,List,w);
      }
      break;

    default:
      goto error;
    }

 error:
  Error("=../2: illegal arguments");
  return FALSE;
}


static BOOL luther_functor(InArg)
     InArgdecl;
{
  TAGGED X0, X1, X2;

  DerefNLL(X0, Xw(regs[0]));	/* Term */
  DerefNLL(X1, Xw(regs[1]));	/* Name */
  DerefNLL(X2, Xw(regs[2]));	/* Arity */

  switch(TagOf(X0)) {

  case HVA:
#if defined(CONSTR)
  case CVA:
#endif
  case SVA:
    {
      if ((IsATM(X1) || IsNUM(X1)) && IsNUM(X2))
	{
	  if (GetNumber(X2) == 0)
	    return unify(X0, X1, w);

	  if (X1 == atom_list && GetNumber(X2) == 2)
	    {
	      TAGGED funct = Tagify(w->heap_top, LST);

	      CreateHVA(w->heap_top, w);
	      CreateHVA(w->heap_top, w);

	      return unify(X0, funct, w);
	    }

	  if (IsATM(X1))
	    {
	      int arity = GetNumber(X2);
	      TAGGED funct;

	      BuiltinHeapCheck(arity*VARSIZE+VARSIZE, return FALSE);

	      funct = Tagify(w->heap_top, STR);

	      PushOnHeapF(w->heap_top, StoreFunctor(X1, arity));

	      while (arity--)
		CreateHVA(w->heap_top, w);

	      return unify(X0, funct, w);
	    }
	}
      else 
	{
	  return FALSE;
	}
    }

  case NUM:
  case ATM:
  case BOX:
    {
      if (unify(X1, X0, w))
	return unify(X2,Make_Integer(0), w);
      else
	return FALSE;
    }

  case GEN:
    return FALSE;

  case LST:
    {
      if (unify(X1, atom_list, w))
	return unify(X2,Make_Integer(2), w);
      else
	return FALSE;
    }

  case STR:
    {
      UNTAGGED x0 = (UNTAGGED) RemoveTag(X0, STR);

      if (unify(FunctorToAtom(Struct(x0)->functor), X1, w))
	return unify(Make_Integer(StructArity(x0)), X2, w);
      else
	return FALSE;
    }

  default:
    {
      Error("functor - no such term type");
      return FALSE;
    }
  }
}


static BOOL luther_arg(InArg)
     InArgdecl;
{
  TAGGED X0, X1, X2, Tmp;

  DerefNLL(X0,Xw(regs[0]));	/* ArgNo */
  DerefNLL(X1,Xw(regs[1]));	/* Term */
  DerefNLL(X2,Xw(regs[2]));	/* Arg */

  if (not(IsNUM(X0) && IsCompound(X1)))
    return FALSE;

  if (IsLST(X1))
    {
      switch (GetNumber(X0)) {

      case 1:
	{
	  DerefNLL(Tmp,Ref(GetCar(X1)));
	}
	return unify(Tmp,X2,w);

      case 2:
	{
	  DerefNLL(Tmp,Ref(GetCdr(X1)));
	}
	return unify(Tmp,X2,w);
      }
      return FALSE;
    }
  else
    {
      if ((GetNumber(X0) > StructArity(RemoveTag(X1,STR))) ||
	  (GetNumber(X0) <= 0))
	return FALSE;

      DerefNLL(Tmp,Ref(GetArg(X1,(GetNumber(X0)-1))));

      return unify(Tmp,X2,w);
    }
}


static BOOL luther_inline_unify(InArg)
     InArgdecl;
{
  TAGGED Xd,Yd;

  DerefNLL(Xd,Xw(regs[0]));
  DerefNLL(Yd,Xw(regs[1]));

  return unify(Xd,Yd,w);
}


static BOOL luther_active(InArg)
     InArgdecl;
{
#if defined(PARALLEL)
  Xw(regs[0]) = Make_Integer(w->global->active_workers);
  return TRUE;
#else
  return FALSE;
#endif /* PARALLEL */
}


static BOOL luther_save(InArg)
     InArgdecl;
{
#if defined(PARALLEL)
  register TAGGED saved;
  DerefNLL(saved,Xw(regs[0]));
  w->global->collect[w->pid-1] = saved;
  return TRUE;
#else
  return FALSE;
#endif /* PARALLEL */
}

static BOOL luther_collect_plus(InArg)
     InArgdecl;
{
#if defined(PARALLEL)
  register TAGGED saved, Result;
  register double sum;
  register integer i;

  for(sum = 0.0, i = 0 ; i < w->global->active_workers ; i++) 
    {
      saved = w->global->collect[i];
      if (IsNUM(saved))
	{
	  sum += (double) GetNumber(saved);
	}
      else if (IsFLT(saved))
	{
	  sum += GetFloat(saved);
	}
      else return FALSE;
    }

  DerefNLL(Result, Xw(regs[0]));
  
  if (sum == floor(sum))
    {
      return unify(Result, Make_Integer(((integer) sum)), w);
    }
  else
    {
      return unify(Result, make_float(sum, w), w);
    }
#else
  return FALSE;
#endif /* PARALLEL */
}

static BOOL luther_collect_times(InArg)
     InArgdecl;
{
#if defined(PARALLEL)
  register TAGGED saved, Result;
  register double sum;
  register integer i;

  for(sum = 1.0, i = 0 ; i < w->global->active_workers ; i++) 
    {
      saved = w->global->collect[i];
      if (IsNUM(saved))
	{
	  sum *= (double) GetNumber(saved);
	}
      else if (IsFLT(saved))
	{
	  sum *= GetFloat(saved);
	}
      else return FALSE;
    }

  DerefNLL(Result, Xw(regs[0]));
  
  if (sum == floor(sum))
    {
      return unify(Result, Make_Integer(((integer) sum)), w);
    }
  else
    {
      return unify(Result, make_float(sum, w), w);
    }
#else
  return FALSE;
#endif /* PARALLEL */
}


/**********************************************************************
 *
 * Functions for math comparing.
 *
 */

/* $eq */
static BOOL luther_math_eq(InArg)
     InArgdecl;
{
  EvalArithmeticComp(==, bignum_eq);
}

/* $ineq */
static BOOL luther_math_ineq(InArg)
     InArgdecl;
{
  EvalArithmeticComp(!=, bignum_ne);
}


/* Operators: < > =< >=
*/
static BOOL luther_lt(InArg)
     InArgdecl;
{
  EvalArithmeticComp(<, bignum_lt);
}

static BOOL luther_gt(InArg)
     InArgdecl;
{
  EvalArithmeticComp(>, bignum_gt);
}

static BOOL luther_le(InArg)
     InArgdecl;
{
  EvalArithmeticComp(<=, bignum_le);
}

static BOOL luther_ge(InArg)
     InArgdecl;
{
  EvalArithmeticComp(>=, bignum_ge);
}



/* $atom/1 */

static BOOL luther_atom(InArg)
     InArgdecl;
{
  TAGGED X0; /* X */

  DerefNLL(X0,Xw(regs[0]));

  return (IsATM(X0)) ? TRUE : FALSE;
}

/* $atomic/1 */

static BOOL luther_atomic(InArg)
     InArgdecl;
{
  TAGGED X0;

  DerefNLL(X0,Xw(regs[0]));

  return (IsATM(X0) || IsNumber(X0)) ? TRUE : FALSE;
}

/* $generic/1 */

static BOOL luther_generic(InArg)
     InArgdecl;
{
  TAGGED X0;

  DerefNLL(X0,Xw(regs[0]));

  return (IsGEN(X0)) ? TRUE : FALSE;
}

/* $integer/1 */

static BOOL luther_integer(InArg)
     InArgdecl;
{
  TAGGED X0;

  DerefNLL(X0,Xw(regs[0])); /* X */

  return (IsNUM(X0) || IsBIG(X0)) ? TRUE : FALSE;
}

/* $float/1 */

static BOOL luther_float(InArg)
     InArgdecl;
{
  TAGGED X0;

  DerefNLL(X0,Xw(regs[0])); /* X */

  return (IsFLT(X0)) ? TRUE : FALSE;
}

/* $number/1 */

static BOOL luther_number(InArg)
     InArgdecl;
{
  TAGGED X0;

  DerefNLL(X0,Xw(regs[0])); /* X */

  return (IsNumber(X0)) ? TRUE : FALSE;
}

/* $nonvar/1 */

static BOOL luther_nonvar(InArg)
     InArgdecl;
{
  TAGGED X0;

  DerefNLL(X0,Xw(regs[0])); /* X */

  return (!IsVar(X0)) ? TRUE : FALSE;
}

/* $var/1 */

static BOOL luther_var(InArg)
     InArgdecl;
{
  TAGGED X0;

  DerefNLL(X0,Xw(regs[0])); /* X */

  return (IsVar(X0)) ? TRUE : FALSE;
}

/*******************************************************************************
 *
 * Functions for math operations.
 *
 */

/* $plus/3 */

static BOOL luther_plus(InArg)
     InArgdecl;
{
  EvalBinaryArithmetic(+, bignum_add);
}

static BOOL luther_minus(InArg)
     InArgdecl;
{
  EvalBinaryArithmetic(-, bignum_sub);
}


/*
 *   Special case of "EvalBinaryArithmetic" in order to handle possible overflow
 * when multiplying two small integers.
 */
static BOOL luther_times(InArg)
     InArgdecl;
{
  register TAGGED X1,X2;

  DerefNLL(X1,Xw(regs[1]));	/* Expr1 */
  DerefNLL(X2,Xw(regs[2]));	/* Expr2 */

 start:
  {
    if (IsNUM(X1))
      {
	if (IsNUM(X2))
	  {
	    integer factor1 = GetNumber(X1);
	    integer factor2 = GetNumber(X2);
	    integer product = factor1 * factor2;

	    if (factor1 == 0 || (product/factor1 == factor2))
	      {
		Xw(regs[0]) = MakeNumber(w, product);
		return TRUE;
	      }
	    else
	      {
		X2 = NUM2BIG(w, X2); /* Convert and fall through to next case (IsBIG). */
	      }
	  }
	if (IsBIG(X2))
	  {
	    Xw(regs[0]) = bignum_mul(w, GetBignum(NUM2BIG(w,X1)), GetBignum(X2));
	    return TRUE;
	  }
	if (IsFLT(X2))
	  {
	    Xw(regs[0]) = make_float(GetNumber(X1) * GetFloat(X2), w);
	    return TRUE;
	  }

	goto eval;
      }
    if (IsBIG(X1))
      {
	if (IsBIG(X2))
	  {
	    Xw(regs[0]) = bignum_mul(w, GetBignum(X1), GetBignum(X2));
	    return TRUE;
	  }
	if (IsFLT(X2))
	  {
	    Xw(regs[0]) = bignum_mul(w, GetBignum(X1), GetBignum(FLT2BIG(w,X2)));
	    return TRUE;
	  }
	if (IsNUM(X2))
	  {
	    Xw(regs[0]) = bignum_mul(w, GetBignum(X1), GetBignum(NUM2BIG(w,X2)));
	    return TRUE;
	  }

	goto eval;
      }
    if (IsFLT(X1))
      {
	if (IsFLT(X2))
	  {
	    Xw(regs[0]) = make_float(GetFloat(X1) * GetFloat(X2), w);
	    return TRUE;
	  }
	if (IsNUM(X2))
	  {
	    Xw(regs[0]) = make_float(GetFloat(X1) * GetNumber(X2), w);
	    return TRUE;
	  }
	if (IsBIG(X2))
	  {
	    Xw(regs[0]) = bignum_mul(w, GetBignum(FLT2BIG(w,X1)), GetBignum(X2));
	    return TRUE;
	  }

	goto eval;
      }
  }
  MathEvalTerm(X1,start,error1);

 eval:
  MathEvalTerm(X2,start,error2);

 error1:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);

 error2:
  return luther_error(E_ILLEGAL_AR_EX,X2,w);
}


static BOOL luther_plus_1(InArg)
     InArgdecl;
{
    EvalUnaryPostArithmetic(+1, bignum_inc);
}

static BOOL luther_minus_1(InArg)
     InArgdecl;
{
    EvalUnaryPostArithmetic(-1, bignum_dec);
}


static BOOL luther_bin_not(InArg)
     InArgdecl;
{
    EvalUnaryLogic(~, bignum_not);
}

static BOOL luther_bin_and(InArg)
     InArgdecl;
{
    EvalBinaryLogic(&, bignum_and);
}

static BOOL luther_bin_or(InArg)
     InArgdecl;
{
    EvalBinaryLogic(|, bignum_ior);
}

static BOOL luther_bin_xor(InArg)
     InArgdecl;
{
    EvalBinaryLogic(^, bignum_xor);
}



static BOOL luther_div(InArg)
     InArgdecl;
{
  register TAGGED X1,X2;
  register double dividend, divisor;

  DerefNLL(X1,Xw(regs[1]));	/* Expr1 */
  DerefNLL(X2,Xw(regs[2]));	/* Expr2 */

 eval_second:
  {
    if (IsNUM(X2))
      {
	divisor = GetNumber(X2);
      }
    else if (IsFLT(X2))
      {
	divisor = GetFloat(X2);
      }
    else if (IsBIG(X2))
      {
	divisor = bntof(GetBignum(X2));
      }
    else
      {
	MathEvalTerm(X2,eval_second,error);
      }
  }

  if (divisor == 0.0)
    {
      goto error_zero;
    }

 eval_first:
  {
    if (IsNUM(X1))
      {
	dividend = GetNumber(X1);
      }
    else if (IsFLT(X1))
      {
	dividend = GetFloat(X1);
      }
    else if (IsBIG(X1))
      {
	dividend = bntof(GetBignum(X1));
      }
    else
      {
	MathEvalTerm(X1,eval_first,error);
      }
  }
  Xw(regs[0]) = make_float(dividend / divisor, w);
  return TRUE;

 error:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);

 error_zero:
  return luther_error(E_DIV_ZERO,0,w);
}


static BOOL luther_intdiv(InArg)
     InArgdecl;
{
  register TAGGED X1,X2;

  DerefNLL(X1,Xw(regs[1])); /* Expr1 */
  DerefNLL(X2,Xw(regs[2])); /* Expr2 */

 start:
  {
    if (IsNUM(X1)) {
      if (IsNUM(X2)) {
	if (GetNumber(X2) == 0) {
	  goto error_zero;
	}
	Xw(regs[0]) = Make_Integer(((integer) (GetNumber(X1) / GetNumber(X2))));
	return TRUE;
      }
      else if (IsBIG(X2)) {
	Xw(regs[0]) = Make_Integer(0); /* A BigNumber is always bigger than a Number. */
	return TRUE;
      }
      else if (IsFLT(X2)) {
	if (((integer) GetFloat(X2)) == 0 ) {
	  goto error_zero;
	}
	Xw(regs[0]) = Make_Integer(((integer) (GetNumber(X1) / ((integer) GetFloat(X2)))));
	return TRUE;
      }
      else
	goto eval_second;
    }
    else if (IsBIG(X1)) {
      if (IsBIG(X2)) {
	Xw(regs[0]) = bignum_div(w,GetBignum(X1),GetBignum(X2));
	return TRUE;
      }
      else if (IsFLT(X2)) {
	if (((integer) GetFloat(X2)) == 0 ) {
	  goto error_zero;
	}
	Xw(regs[0]) = bignum_div(w,GetBignum(X1),GetBignum(FLT2BIG(w,X2)));
	return TRUE;
      }
      else if (IsNUM(X2)) {
	if (GetNumber(X2) == 0) {
	  goto error_zero;
	}
	Xw(regs[0]) = bignum_div(w,GetBignum(X1),GetBignum(NUM2BIG(w,X2)));
	return TRUE;
      }
      else
	goto eval_second;
    }
    else  if (IsFLT(X1)) {
      if (IsFLT(X2)) {
	if (((integer) GetFloat(X2)) == 0 ) {
	  goto error_zero;
	}
	Xw(regs[0]) = Make_Integer(((integer) (GetFloat(X1) / ((integer) GetFloat(X2)))));
	return TRUE;
      }
      else if (IsBIG(X2)) {
	Xw(regs[0]) = bignum_div(w,GetBignum(FLT2BIG(w,X1)),GetBignum(X2));
	return TRUE;
      }
      else if (IsNUM(X2)) {
	if (GetNumber(X2) == 0) {
	  goto error_zero;
	}
	Xw(regs[0]) = Make_Integer(((integer) (GetFloat(X1) / GetNumber(X2))));
	return TRUE;
      }
      else
	goto eval_second;
    }
  }
 eval_first:
  MathEvalTerm(X1,start,error1);

 eval_second:
  MathEvalTerm(X2,start,error2);

 error1:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);

 error2:
  return luther_error(E_ILLEGAL_AR_EX,X2,w);

 error_zero:
  return luther_error(E_DIV_ZERO,0,w);
}


static BOOL luther_mod(InArg)
     InArgdecl;
{
  register TAGGED X1,X2;

  DerefNLL(X1,Xw(regs[1]));	/* Expr1 */
  DerefNLL(X2,Xw(regs[2]));	/* Expr2 */

 start:
  {
    if (IsNUM(X1)) {
      if (IsNUM(X2)) {
	Xw(regs[0]) = Make_Integer(((integer) (GetNumber(X1) % GetNumber(X2))));
	return TRUE;
      }
      if (IsBIG(X2)) {
	Xw(regs[0]) = bignum_mod(w, GetBignum(NUM2BIG(w,X1)), GetBignum(X2));
	return TRUE;
      }
      if (IsFLT(X2)) {
	Xw(regs[0]) = Make_Integer(((integer) (GetNumber(X1) % ((integer) GetFloat(X2)))));
	return TRUE;
      }
      goto eval_second;
    }
    if (IsBIG(X1)) {
      if (IsBIG(X2)) {
	Xw(regs[0]) = bignum_mod(w, GetBignum(X1), GetBignum(X2));
	return TRUE;
      }
      if (IsFLT(X2)) {
	Xw(regs[0]) = bignum_mod(w, GetBignum(X1), GetBignum(FLT2BIG(w,X2)));
	return TRUE;
      }
      if (IsNUM(X2)) {
	Xw(regs[0]) = bignum_mod(w, GetBignum(X1), GetBignum(NUM2BIG(w,X2)));
	return TRUE;
      }
      goto eval_second;
    }
    if (IsFLT(X1)) {
      if (IsFLT(X2)) {
	Xw(regs[0]) = MakeNumber(w, ((integer) GetFloat(X1)) % (integer) GetFloat(X2));
	return TRUE;
      }
      if (IsNUM(X2)) {
	Xw(regs[0]) = MakeNumber(w, ((integer) GetFloat(X1)) % GetNumber(X2));
	return TRUE;
      }
      if (IsBIG(X2)) {
	Xw(regs[0]) = bignum_mod(w, GetBignum(FLT2BIG(w,X1)), GetBignum(X2));
	return TRUE;
      }
      goto eval_second;
    }
  }
 eval_first:
  MathEvalTerm(X1,start,error1);

 eval_second:
  MathEvalTerm(X2,start,error2);

 error1:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);

 error2:
  return luther_error(E_ILLEGAL_AR_EX,X2,w);
}


static BOOL luther_min(InArg)
     InArgdecl;
{
  register TAGGED X1,X2;

  DerefNLL(X1,Xw(regs[1]));	/* Expr1 */
  DerefNLL(X2,Xw(regs[2]));	/* Expr2 */

 start:
  {
    if (IsNUM(X1)) {
      if (IsNUM(X2)) {
	Xw(regs[0]) = X1 < X2 ? X1 : X2;
	return TRUE;
      }
      if (IsBIG(X2)) {
	Xw(regs[0]) = X1;
	return TRUE;
      }
      if (IsFLT(X2)) {
	Xw(regs[0]) = ((double) GetNumber(X1)) <  GetFloat(X2) ? X1 : X2;
	return TRUE;
      }
      goto eval_second;
    }
    if (IsBIG(X1)) {
      if (IsBIG(X2)) {
	Xw(regs[0]) = bignum_lt(GetBignum(X1), GetBignum(X2)) ? X1 : X2;
	return TRUE;
      }
      if (IsFLT(X2)) {
	Xw(regs[0]) = bignum_lt(GetBignum(X1), GetBignum(FLT2BIG(w,X2))) ? X1 : X2;
	return TRUE;
      }
      if (IsNUM(X2)) {
	Xw(regs[0]) = X2;
	return TRUE;
      }
      goto eval_second;
    }
    if (IsFLT(X1)) {
      if (IsFLT(X2)) {
	Xw(regs[0]) = GetFloat(X1) < GetFloat(X2) ? X1 : X2;
	return TRUE;
      }
      if (IsNUM(X2)) {
	Xw(regs[0]) = GetFloat(X1) < (double) GetNumber(X2) ? X1 : X2;
	return TRUE;
      }
      if (IsBIG(X2)) {
	Xw(regs[0]) = bignum_lt(GetBignum(FLT2BIG(w,X1)), GetBignum(X2)) ? X1 : X2;
	return TRUE;
      }
      goto eval_second;
    }
  }
 eval_first:
  MathEvalTerm(X1,start,error1);

 eval_second:
  MathEvalTerm(X2,start,error2);

 error1:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);

 error2:
  return luther_error(E_ILLEGAL_AR_EX,X2,w);
}


static BOOL luther_max(InArg)
     InArgdecl;
{
  register TAGGED X1,X2;

  DerefNLL(X1,Xw(regs[1]));	/* Expr1 */
  DerefNLL(X2,Xw(regs[2]));	/* Expr2 */

 start:
  {
    if (IsNUM(X1)) {
      if (IsNUM(X2)) {
	Xw(regs[0]) = X1 > X2 ? X1 : X2;
	return TRUE;
      }
      if (IsBIG(X2)) {
	Xw(regs[0]) = X2;
	return TRUE;
      }
      if (IsFLT(X2)) {
	Xw(regs[0]) = ((double) GetNumber(X1)) >  GetFloat(X2) ? X1 : X2;
	return TRUE;
      }
      goto eval_second;
    }
    if (IsBIG(X1)) {
      if (IsBIG(X2)) {
	Xw(regs[0]) = bignum_gt(GetBignum(X1), GetBignum(X2)) ? X1 : X2;
	return TRUE;
      }
      if (IsFLT(X2)) {
	Xw(regs[0]) = bignum_gt(GetBignum(X1), GetBignum(FLT2BIG(w,X2))) ? X1 : X2;
	return TRUE;
      }
      if (IsNUM(X2)) {
	Xw(regs[0]) = X1;
	return TRUE;
      }
      goto eval_second;
    }
    if (IsFLT(X1)) {
      if (IsFLT(X2)) {
	Xw(regs[0]) = GetFloat(X1) > GetFloat(X2) ? X1 : X2;
	return TRUE;
      }
      if (IsNUM(X2)) {
	Xw(regs[0]) = GetFloat(X1) > (double) GetNumber(X2) ? X1 : X2;
	return TRUE;
      }
      if (IsBIG(X2)) {
	Xw(regs[0]) = bignum_gt(GetBignum(FLT2BIG(w,X1)), GetBignum(X2)) ? X1 : X2;
	return TRUE;
      }
      goto eval_second;
    }
  }
 eval_first:
  MathEvalTerm(X1,start,error1);

 eval_second:
  MathEvalTerm(X2,start,error2);

 error1:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);

 error2:
  return luther_error(E_ILLEGAL_AR_EX,X2,w);
}




static BOOL rshift_aux(InArg, rshift)
     InArgdecl;
     natural rshift;
{
  register TAGGED X1;

  DerefNLL(X1,Xw(regs[1]));	/* Expr1 */

 eval_first:
  {
    if (IsNUM(X1))
      {
	Xw(regs[0]) = Make_Integer((integer) (GetNumber(X1) >> rshift));
      }
    else if (IsBIG(X1))
      {
	Xw(regs[0]) = bignum_rsh(w, GetBignum(X1), rshift);
      }
    else if (IsFLT(X1))
      {
	Xw(regs[0]) = MakeNumber(w, ((integer) (((integer) GetFloat(X1)) >> rshift)));
      }
    else
      {
	MathEvalTerm(X1,eval_first,error);
      }

    return TRUE;
  }
 error:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);
}


static BOOL lshift_aux(InArg, lshift)
     InArgdecl;
     natural lshift;
{
  register TAGGED X1;

  DerefNLL(X1,Xw(regs[1]));	/* Expr1 */

 eval_first:
  {
    if (IsNUM(X1))
      {
	if (lshift < (WORDSIZE - INTSIZE))
	  Xw(regs[0]) = MakeNumber(w, (integer) (GetNumber(X1) << lshift));
	else
	  Xw(regs[0]) = bignum_lsh(w, GetBignum(NUM2BIG(w,X1)), lshift);
      }
    else if (IsBIG(X1))
      {
	Xw(regs[0]) = bignum_lsh(w, GetBignum(X1), lshift);
      }
    else if (IsFLT(X1))
      {
	if (lshift < (WORDSIZE - INTSIZE))
	  Xw(regs[0]) = MakeNumber(w, (integer) ((integer) GetFloat(X1) << lshift));
	else
	  Xw(regs[0]) = bignum_lsh(w, GetBignum(FLT2BIG(w,X1)), lshift);
      }
    else
      {
	MathEvalTerm(X1,eval_first,error);
      }

    return TRUE;
  }
 error:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);
}


static BOOL luther_rshift(InArg)
     InArgdecl;
{
  register TAGGED X2;
  register integer rshift;

  DerefNLL(X2,Xw(regs[2]));	/* Expr2 */

 eval_second:
  {
    if (IsNUM(X2))
      {
	rshift = GetNumber(X2);
      }
    else if (IsFLT(X2))
      {
	rshift = GetFloat(X2);
      }
    else if (IsBIG(X2))
      {
	Xw(regs[0]) = Make_Integer(0);
	return TRUE;
      }
    else
      {
	MathEvalTerm(X2,eval_second,error);
      }

    return pos(rshift) ? rshift_aux(InArg, rshift) : lshift_aux(InArg, -rshift);
  }
 error:
  return luther_error(E_ILLEGAL_AR_EX,X2,w);
}


static BOOL luther_lshift(InArg)
     InArgdecl;
{
  register TAGGED X2;
  register integer lshift;

  DerefNLL(X2,Xw(regs[2]));	/* Expr2 */

 eval_second:
  {
    if (IsNUM(X2))
      {
	lshift = GetNumber(X2);
      }
    else if (IsFLT(X2))
      {
	lshift = GetFloat(X2);
      }
    else if (IsBIG(X2))
      {
	Xw(regs[0]) = float_nan; /* Make_Integer(0); */
	return TRUE;
      }
    else
      {
	MathEvalTerm(X2,eval_second,error);
      }

    return pos(lshift) ? lshift_aux(InArg, lshift) : rshift_aux(InArg, -lshift);
  }
 error:
  return luther_error(E_ILLEGAL_AR_EX,X2,w);
}


static BOOL luther_pow(InArg)
     InArgdecl;
{
  register double exp;
  register TAGGED X1,X2;

  DerefNLL(X1,Xw(regs[1]));	/* Expr1 */
  DerefNLL(X2,Xw(regs[2]));	/* Expr2 */

 eval_second:
  {
    if (IsNUM(X2))
      {
	exp = GetNumber(X2);
      }
    else if (IsFLT(X2))
      {
	exp = GetFloat(X2);
      }
    else
      {
	MathEvalTerm(X2,eval_second,error);
      }
  }

 eval_first:
  {
    if (IsNUM(X1))
      {
	Xw(regs[0]) = make_float(pow((double) GetNumber(X1), exp), w);
      }
    else if (IsFLT(X1))
      {
	Xw(regs[0]) = make_float(pow(GetFloat(X1), exp), w);
      }
    else if (IsBIG(X1))
      {
	Xw(regs[0]) = bignum_pow(w, GetBignum(X1), (integer) exp);
      }
    else
      {
	MathEvalTerm(X1,eval_first,error);
      }
  }
  return TRUE;

 error:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);
}


static BOOL luther_neg(InArg)
     InArgdecl;
{
  TAGGED X1;

  DerefNLL(X1,Xw(regs[1]));	/* Expr */

 start:
  {
    if (IsNUM(X1))
      {
	Xw(regs[0]) = Make_Integer(GetNumber(X1) * -1);
      }
    else if (IsBIG(X1))
      {
	Xw(regs[0]) = bignum_neg(w ,GetBignum(X1));
      }
    else if (IsFLT(X1))
      {
	Xw(regs[0]) = make_float(GetFloat(X1) * -1.0, w);
      }
    else
      {
	MathEvalTerm(X1,start,error);
      }
  }
  return TRUE;

 error:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);
}


static BOOL luther_tointeger(InArg)
     InArgdecl;
{
  TAGGED X1;

  DerefNLL(X1,Xw(regs[1]));	/* Expr */

 start:
  {
    if (IsFLT(X1))
      {
	Xw(regs[0]) = MakeNumber(w, (integer) GetFloat(X1));
      }
    else if (IsNUM(X1) || IsBIG(X1))
      {
	Xw(regs[0]) = X1;
      }
    else
      {
	MathEvalTerm(X1,start,error);
      }
  }
  return TRUE;

 error:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);
}


static BOOL luther_floor(InArg)
     InArgdecl;
{
  TAGGED X1;

  DerefNLL(X1,Xw(regs[1]));	/* Expr */

 start:
  {
    if (IsFLT(X1))
      {
	Xw(regs[0]) = make_float(floor(GetFloat(X1)), w);
      }
    else if (IsNUM(X1) || IsBIG(X1))
      {
	Xw(regs[0]) = X1;
      }
    else
      {
	MathEvalTerm(X1,start,error);
      }
  }
  return TRUE;

 error:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);
}


static BOOL luther_ceiling(InArg)
     InArgdecl;
{
  TAGGED X1;

  DerefNLL(X1,Xw(regs[1]));	/* Expr */

 start:
  {
    if (IsFLT(X1))
      {
	Xw(regs[0]) = make_float(ceil(GetFloat(X1)), w);
      }
    else if (IsNUM(X1) || IsBIG(X1))
      {
	Xw(regs[0]) = X1;
      }
    else
      {
	MathEvalTerm(X1,start,error);
      }
  }
  return TRUE;

 error:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);
}


static BOOL luther_truncate(InArg)
     InArgdecl;
{
  TAGGED X1;

  DerefNLL(X1,Xw(regs[1]));	/* Expr */

 start:
  {
    if (IsFLT(X1))
      {
	double integral;
	double fraction = modf(GetFloat(X1), &integral);

	Xw(regs[0]) = make_float(integral, w);
      }
    else if (IsNUM(X1) || IsBIG(X1))
      {
	Xw(regs[0]) = X1;
      }
    else
      {
	MathEvalTerm(X1,start,error);
      }
  }
  return TRUE;

 error:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);
}


static BOOL luther_round(InArg)
     InArgdecl;
{
  TAGGED X1;

  DerefNLL(X1,Xw(regs[1]));	/* Expr */

 start:
  {
    if (IsFLT(X1))
      {
	double value = GetFloat(X1);
	double integral;
	double fraction = modf(value, &integral);

	if (fraction == 0.5)
	  {
	    integral = (fmod(integral, 2.0) == 1.0) ? integral + 1.0 : integral;

	    Xw(regs[0]) = make_float(integral, w);
	  }
	else
	  {
	    Xw(regs[0]) = make_float(floor(value + 0.5), w);
	  }
      }
    else if (IsNUM(X1) || IsBIG(X1))
      {
	Xw(regs[0]) = X1;
      }
    else
      {
	MathEvalTerm(X1,start,error);
      }
  }
  return TRUE;

 error:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);
}


static BOOL luther_msb(InArg)
     InArgdecl;
{
  TAGGED X1;
  integer number;

  DerefNLL(X1,Xw(regs[1]));	/* Expr */

 start:
  {
    if (IsNUM(X1))
      {
	number = GetNumber(X1);
      }
    else if (IsBIG(X1))
      {
	Xw(regs[0]) = bignum_msb(w, GetBignum(X1));
	return TRUE;
      }
    else if (IsFLT(X1))
      {
	number = GetFloat(X1);
      }
    else
      {
	MathEvalTerm(X1,start,error);
      }

    if (number < 0)
      {
	Xw(regs[0]) = Make_Integer(-1);
      }
    else
      {
	sword most_significant = 0;
	sword divider = WORDSIZE/2;


	/* Binary search for the most significant bit.
	 */

	while (divider > 0)
	  {
	    if (number >= (0x1L << (divider)))
	      {
		number >>= divider;
		most_significant += divider;
	      }
	    divider /= 2;
	  }

	Xw(regs[0]) = Make_Integer(most_significant);
      }
    return TRUE;
  }

 error:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);
}


static BOOL luther_rng(InArg)
     InArgdecl;
{
  integer msb, lsb; /* most and least significant bit in bit field. */

  register TAGGED X1,X2,X3;

  DerefNLL(X1,Xw(regs[1]));	/* Expr */
  DerefNLL(X2,Xw(regs[2]));	/* Index1 */
  DerefNLL(X3,Xw(regs[3]));	/* Index2 */

 eval_third:
  {
    if (IsNUM(X3))
      {
	lsb = GetNumber(X3);
      }
    else
      {
	MathEvalTerm(X3,eval_third,error);
      }
  }

 eval_second:
  {
    if (IsNUM(X2))
      {
	msb = GetNumber(X2);
      }
    else
      {
	MathEvalTerm(X2,eval_second,error);
      }
  }

  if (lsb <= msb)
    {
    eval_first:
      {
	if (IsNUM(X1))
	  {
	    Xw(regs[0]) = bignum_rng(w, GetBignum(NUM2BIG(w,X1)), msb, lsb);
	  }
	else if (IsBIG(X1))
	  {
	    Xw(regs[0]) = bignum_rng(w, GetBignum(X1), msb, lsb);
	  }
	else
	  {
	    MathEvalTerm(X1,eval_first,error);
	  }
      }
      return TRUE;
    }
 error:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);
}


static BOOL luther_tofloat(InArg)
     InArgdecl;
{
  TAGGED X1;

  DerefNLL(X1,Xw(regs[1]));	/* Expr */

 start:
  {
    if (IsNUM(X1))
      {
	Xw(regs[0]) = make_float((double) GetNumber(X1), w);
      }
    else if (IsBIG(X1))
      {
	Xw(regs[0]) = make_float(bntof(GetBignum(X1)), w);
      }
    else if (IsFLT(X1))
      {
	Xw(regs[0]) = X1;
      }
    else
      {
	MathEvalTerm(X1,start,error);
      }
  }
  return TRUE;

 error:
  return luther_error(E_ILLEGAL_AR_EX,X1,w);
}


static BOOL luther_exp(InArg)
     InArgdecl;
{
  EvalUnaryArithmeticFloat(exp);
}

static BOOL luther_log(InArg)
     InArgdecl;
{
  EvalUnaryArithmeticFloat(log);
}

static BOOL luther_sqrt(InArg)
     InArgdecl;
{
  EvalUnaryArithmeticFloat(sqrt);
}

static BOOL luther_cbrt(InArg)
     InArgdecl;
{
#if HAVE_CBRT
  EvalUnaryArithmeticFloat(cbrt);
#else
  Error("cbrt not supported on this architecture");
  return FALSE;
#endif
}

static BOOL luther_sin(InArg)
     InArgdecl;
{
  EvalUnaryArithmeticFloat(sin);
}

static BOOL luther_cos(InArg)
     InArgdecl;
{
  EvalUnaryArithmeticFloat(cos);
}

static BOOL luther_tan(InArg)
     InArgdecl;
{
  EvalUnaryArithmeticFloat(tan);
}

static BOOL luther_asin(InArg)
     InArgdecl;
{
  EvalUnaryArithmeticFloat(asin);
}

static BOOL luther_atan(InArg)
     InArgdecl;
{
  EvalUnaryArithmeticFloat(atan);
}

static BOOL luther_acos(InArg)
     InArgdecl;
{
  EvalUnaryArithmeticFloat(acos);
}

static BOOL luther_abs(InArg)
     InArgdecl;
{
  EvalUnaryArithmeticNumBigFlt(abs,bignum_abs,fabs);
}


/* '$aref'(Array,Index,Object)
 *
 * Note: inline function equivalent of array_arg/3.
 *
 */
static BOOL luther_aref(InArg)
     InArgdecl;
{
  TAGGED X0, X1, value;

  DerefNLL(X0, Xw(regs[0])); /* Array */
  DerefNLL(X1, Xw(regs[1])); /* Index */

  if (not(IsArray(X0)))
    goto error;

 start:
  {
    if (IsNUM(X1))
      {
	register integer index = GetNumber(X1);

	if (index < 0 || index >= GetNumber(GetArraySize(X0)))
	  return FALSE;

	DerefNLL(value, Ref(&GetArrayArg(X0, index)));

	Xw(regs[2]) = value;

	return TRUE;
      }
    else
      {
	MathEvalTerm(X1, start, error);
      }
  }

 error:
  return luther_error(E_ILLEGAL_AR_EX, X1, w);
}


static BOOL luther_eval_math(InArg)
     InArgdecl;
{
  TAGGED X1, newarg[3], Operator;
  integer newregs[3];

  DerefNLL(X1, Xw(regs[1])); /* Expr */

 start:
  {
    if (IsSTR(X1))
      {
	Operator = GetFunctor(X1);

	if (Operator == functor_plus)     EvalBinary(luther_plus);
	if (Operator == functor_minus)    EvalBinary(luther_minus);
	if (Operator == functor_times)    EvalBinary(luther_times);
	if (Operator == functor_div)      EvalBinary(luther_div);
	if (Operator == functor_intdiv)   EvalBinary(luther_intdiv);
	if (Operator == functor_mod)      EvalBinary(luther_mod);
	if (Operator == functor_rshift)   EvalBinary(luther_rshift);
	if (Operator == functor_lshift)   EvalBinary(luther_lshift);

	if (Operator == functor_min)      EvalBinary(luther_min);
	if (Operator == functor_max)      EvalBinary(luther_max);

	if (Operator == functor_abs)      EvalUnary(luther_abs);
	if (Operator == functor_exp)      EvalUnary(luther_exp);
	if (Operator == functor_log)      EvalUnary(luther_log);
	if (Operator == functor_sqrt)     EvalUnary(luther_sqrt);
	if (Operator == functor_cbrt)     EvalUnary(luther_cbrt);
	if (Operator == functor_sin)      EvalUnary(luther_sin);
	if (Operator == functor_cos)      EvalUnary(luther_cos);
	if (Operator == functor_tan)      EvalUnary(luther_tan);
	if (Operator == functor_asin)     EvalUnary(luther_asin);
	if (Operator == functor_acos)     EvalUnary(luther_acos);
	if (Operator == functor_atan)     EvalUnary(luther_atan);

	if (Operator == functor_bin_or)   EvalBinary(luther_bin_or);
	if (Operator == functor_bin_and)  EvalBinary(luther_bin_and);
	if (Operator == functor_bin_xor)  EvalBinary(luther_bin_xor);
	if (Operator == functor_bin_not)  EvalUnary(luther_bin_not);
	if (Operator == functor_pow)      EvalBinary(luther_pow);

	if (Operator == functor_integer)  EvalUnary(luther_tointeger);
	if (Operator == functor_float)    EvalUnary(luther_tofloat);
	if (Operator == functor_floor)    EvalUnary(luther_floor);
	if (Operator == functor_ceiling)  EvalUnary(luther_ceiling);
	if (Operator == functor_truncate) EvalUnary(luther_truncate);
	if (Operator == functor_round)    EvalUnary(luther_round);
	if (Operator == functor_neg)	  EvalUnary(luther_neg);
	if (Operator == functor_msb)      EvalUnary(luther_msb);
	if (Operator == functor_rng)      EvalTernary(luther_rng);

	if (Operator == functor_aref)     EvalBinary(luther_aref);
      }
    else if (IsNumber(X1))
      {
	Xw(regs[0]) = X1;

	return TRUE;
      }
#if defined(PARALLEL)
    else if (IsVar(X1))
      {
	X1 = luther_await_nonvar(w, X1);

	if (IsVar(X1))
	  goto error;
	else
	  goto start;
      }
#endif /* PARALLEL */
    else if (X1 == atom_inf)
      {
	Xw(regs[0]) = MakeInf(w);

	return TRUE;
      }
    else if (X1 == atom_nan)
      {
	Xw(regs[0]) = MakeNan(w);

	return TRUE;
      }
  }

 error:
  {
    return luther_error(E_ILLEGAL_AR_EX, X1, w);
  }
}

/* is/2 */

BOOL luther_is(Arg)
    Argdecl;
{
  integer newregs[2];

  /* We will have to fake a builtin call to eval_math 
   * i.e. builtin $eval_math 0 1
   */

  newregs[1] = 1; /* Expression */
  newregs[0] = 2; /* Result */

  if (luther_eval_math(w, newregs))
    {
      /* At this point the result has been zapped into Xw(2) 
       * by eval_math(). We unify it with the first arg to is/2.
       */

      DerefNLL(Xw(0),Xw(0));
      return unify(Xw(0), Xw(2), w);
    }

  return FALSE;
}


#if defined(ksr1)
#include <ksr/context.h>
#endif

void initialize_inline(w)
     worker *w;
{
#if defined(ksr1)
  long no_exceptions;

  no_exceptions = 0L;

  (void) iswpalle(&no_exceptions);
#endif

  float_inf = MakeConstInf(w);
  float_nan = MakeConstNan(w);

  functor_plus     = StoreFunctor(atom_table_tagged[ATOM_PLUS], 2);
  functor_minus    = StoreFunctor(atom_table_tagged[ATOM_MINUS], 2);
  functor_times    = StoreFunctor(atom_table_tagged[ATOM_TIMES], 2);
  functor_div      = StoreFunctor(atom_table_tagged[ATOM_DIV], 2);
  functor_intdiv   = StoreFunctor(atom_table_tagged[ATOM_INTDIV], 2);
  functor_mod      = StoreFunctor(atom_table_tagged[ATOM_MOD], 2);
  functor_integer  = StoreFunctor(atom_table_tagged[ATOM_INTEGER], 1);
  functor_float    = StoreFunctor(atom_table_tagged[ATOM_FLOAT], 1);
  functor_floor    = StoreFunctor(atom_table_tagged[ATOM_FLOOR], 1);
  functor_ceiling  = StoreFunctor(atom_table_tagged[ATOM_CEILING], 1);
  functor_truncate = StoreFunctor(atom_table_tagged[ATOM_TRUNCATE], 1);
  functor_round    = StoreFunctor(atom_table_tagged[ATOM_ROUND], 1);

  functor_min      = StoreFunctor(atom_table_tagged[ATOM_MIN], 2);
  functor_max      = StoreFunctor(atom_table_tagged[ATOM_MAX], 2);

  functor_abs      = StoreFunctor(atom_table_tagged[ATOM_ABS], 1);
  functor_neg      = StoreFunctor(atom_table_tagged[ATOM_MINUS], 1);
  functor_log      = StoreFunctor(atom_table_tagged[ATOM_LOG], 1);
  functor_exp      = StoreFunctor(atom_table_tagged[ATOM_EXP], 1);
  functor_pow      = StoreFunctor(atom_table_tagged[ATOM_EXP], 2);
  functor_sqrt     = StoreFunctor(atom_table_tagged[ATOM_SQRT], 1);
  functor_cbrt     = StoreFunctor(atom_table_tagged[ATOM_CBRT], 1);
  
  functor_sin      = StoreFunctor(atom_table_tagged[ATOM_SIN], 1);
  functor_cos      = StoreFunctor(atom_table_tagged[ATOM_COS], 1);
  functor_tan      = StoreFunctor(atom_table_tagged[ATOM_TAN], 1);
  functor_asin     = StoreFunctor(atom_table_tagged[ATOM_ASIN], 1);
  functor_acos     = StoreFunctor(atom_table_tagged[ATOM_ACOS], 1);
  functor_atan     = StoreFunctor(atom_table_tagged[ATOM_ATAN], 1);
  
  functor_bin_or   = StoreFunctor(atom_table_tagged[ATOM_B_OR], 2);
  functor_bin_and  = StoreFunctor(atom_table_tagged[ATOM_B_AND], 2);
  functor_bin_xor  = StoreFunctor(atom_table_tagged[ATOM_B_XOR], 2);
  functor_bin_not  = StoreFunctor(atom_table_tagged[ATOM_B_NOT], 1);
  functor_lshift   = StoreFunctor(atom_table_tagged[ATOM_LSHIFT], 2);
  functor_rshift   = StoreFunctor(atom_table_tagged[ATOM_RSHIFT], 2);
  functor_msb      = StoreFunctor(atom_table_tagged[ATOM_MSB], 1);
  functor_rng      = StoreFunctor(atom_table_tagged[ATOM_RNG], 3);
  
  functor_aref     = StoreFunctor(atom_table_tagged[ATOM_AREF], 2);
}
