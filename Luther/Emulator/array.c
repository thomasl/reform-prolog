/*
 * array.c - Adding arrays to Luther as generic objects.
 *
 * Johan Bevemyr.....Tue Apr  7 1992
 * Patric Hedlin.....Thu Jul  6 1995
 *
 *
 * Description:
 *
 *   This file is intended to illustrate how generic objects can be used and
 * defined. In this case a generic object is used for adding arrays to Prolog.
 * One should be aware that this file is not the full implementation; read.pl
 * tokenizer.pl and write.pl also have to be modified. Usually one also need
 * some builtin predicates, e.g. for constructing the generic object etc.
 *
 *
 * The layout of an array is:
 *
 *      header
 *      pointer to method table
 *      data[0] = size of array
 *      data[1] = element 1
 *      data[2] = element 2
 *        ...
 *      data[n] = element n
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */

#include <float.h>

#include "luther.h"
#include "engine.h"
#include "unify.h"
#include "array.h"
#include "inline.h"
#include "display.h"
#include "builtin.h"
#include "bignum.h"
#include "copy-gc-ali.h"


/*******************************************************************************
 *
 * Define the generic method interface.
 *
 */

static PROTOTYPE(BOOL,        (luther_array_unify),   (TAGGED this, TAGGED that, worker *));
static PROTOTYPE(void,        (luther_array_print),   (TAGGED this, FILE *, worker *));
static PROTOTYPE(void,        (luther_array_undo),    (TAGGED this));
static PROTOTYPE(TAGGED,      (luther_array_copy),    (TAGGED this, TIMETYPE, worker *));
static PROTOTYPE(void,        (luther_array_mark),    (TAGGED this, worker *));
static PROTOTYPE(TAGGED *,    (luther_array_collect), (TAGGED this, worker *));
static PROTOTYPE(CompareType, (luther_array_compare), (TAGGED this, TAGGED that, worker *));


GLOBAL method array_method_table = {
  luther_array_unify,
  luther_array_print,
  luther_array_undo,
  luther_array_copy,
  luther_array_mark,
  luther_array_collect,
  luther_array_compare,
};


extern TAGGED float_inf;


/*
 *   Unify two arrays, each element in array_1 is unified with the
 * corresponding element in array_2.
 *
 */
static BOOL luther_array_unify(array_1, array_2, w)
     TAGGED array_1;
     TAGGED array_2;
     worker *w; 
{
  int array_size;
  TAGGED *index_1, element_1; 
  TAGGED *index_2, element_2;

  /*
   * Type checking:
   *
   *   Since array_1 is known to be an array we only have to check array_2. 
   *
   *   Both array_1 and array_2 are assumed to be dereferenced since this
   * function should only be called from unify().
   *
   */
  if (not(IsArray(array_2)))
    return FALSE;

  /* Check that the arrays are equally sized.
   */
  if (GetArraySize(array_1) != GetArraySize(array_2))
    return FALSE;

  /* Initialize loop variables.
   */
  array_size = GetNumber(GetArraySize(array_1));

  index_1 = &(GetArrayArg(array_1, 1));
  index_2 = &(GetArrayArg(array_2, 1));

  while (array_size--)
    {
      DerefNLL(element_1, Ref(index_1));
      DerefNLL(element_2, Ref(index_2));

      if (unify(element_1, element_2, w) == FALSE)
	return FALSE;

      index_1 += VARSIZE;
      index_2 += VARSIZE;
    }

  return TRUE;
}


/*
 *   Printing an array. This is done by calling "display_term" for each element
 * in the array, it would be better to use write, but that would require us to
 * rewrite write.pl (so this will have to suffice for now).
 *
 */
static void luther_array_print(array, fp, w)
     TAGGED  array;
     FILE   *fp;
     worker *w;
{
  integer i;
  integer array_size = GetNumber(GetArraySize(array));

  PL_Print1(fp, "#<");

  /* If the array is empty print the end symbol.
   */
  if (array_size > 0)
    {
      for (i = 1; i < array_size; i++) 
	{
	  display_term(fp, Ref(&GetArrayArg(array, i)), w);
	
	  PL_Print1(fp, ",");
	}
      display_term(fp, Ref(&GetArrayArg(array, i)), w);
    }
  PL_Print1(fp, ">");
}


/*
 *
 */
static void luther_array_undo(t)
    TAGGED t;
{

}

/*
 * Copy an array as called by copy_term/2.
 *
 */
static TAGGED luther_array_copy(array, oldlimit, w)
     TAGGED array;
     TIMETYPE oldlimit;
     worker *w;
{
  TAGGED copy = Tagify(w->heap_top, GEN); /* Initialize a copy on the heap. */
  
  PushOnHeap(w->heap_top, GetArrayHeader(array));
  PushOnHeap(w->heap_top, ((TAGGED) &array_method_table));
  PushOnHeap(w->heap_top, GetArraySize(array));
  
  /* Allocate memory for the copy.
   */
  w->heap_top += GetNumber(GetArraySize(array)) * VARSIZE;
  
  /* Copy all elements of the array.
   */
  luther_copy_args(GetNumber(GetArraySize(array)),
		   &(GetArrayArg(array, 1)),
		   &(GetArrayArg(copy, 1)), oldlimit, w);
  
  return copy;
}


/*
 * mark(term, worker)
 *
 */
static void luther_array_mark(term, w)
     TAGGED term;
     worker *w;
{
#if defined(COPY_GC)
  generic *this = Generic(TagToPointer(term));

  integer  size = GetNumber(GetArraySize(term));

  assert(IsArray(term));

  Mark(this->header);

  while (size > 0)
    {
      live_mark_variable(this->data + size * VARSIZE, w);

      dec(size);
    }
#endif
}


/*
 * collect(term, worker)
 *
 */
static TAGGED *luther_array_collect(term, w)
     TAGGED term;
     worker *w;
{
#if defined(COPY_GC)
  TAGGED header;

  generic *copy;
  generic *this = Generic(TagToPointer(term));

  integer  size = GetNumber(GetArraySize(this));

  assert(IsArray(term));

  LockCell(this->header, header, return NULL);

  copy = (generic*) gc_check_size(sizeofArray(size), w);

  CopyHeapTop += sizeofArray(size);

  while (size > 0)
    {
      copy->data[size] = collect_variable(this->data[size * VARSIZE], w);

      dec(size);
    }

  copy->data[0] = this->data[0]; /* copy size field */
  copy->header  = FromOld(header);
  copy->method  = this->method;

  Forward(this->header, copy);

  return (TAGGED *)copy;
#else
  return NULL;
#endif
}


/*
 *
 */
static CompareType luther_array_compare(array_1, array_2, w)
     TAGGED array_1;
     TAGGED array_2;
     worker *w;
{
  if (not(IsGEN(array_2)))
    return COMPARE_GREATER;

  if (not(IsArray(array_2)))
    {
      if (GetGenTag(array_1) < GetGenTag(array_2))
	return COMPARE_LESS;
      else 
	return COMPARE_GREATER;
    }

  /* If both are arrays, then a longer array is greater than a shorter.
   */
  if (GetArraySize(array_1) < GetArraySize(array_2))
    {
      return COMPARE_LESS;
    }
  else if (GetArraySize(array_1) > GetArraySize(array_2))
    {
      return COMPARE_GREATER;
    }

  /* Both arrays are of equal size, we have to compare the elements of the arrays.
   */
  return compare_struct(&(GetArrayArg(array_1, 0)),
			&(GetArrayArg(array_2, 0)),
			GetNumber(GetArraySize(array_1)), w);
}


/*******************************************************************************
 * 
 * Public builtins supporting array objects.
 *
 *
 *   In order to use the arrays efficiently we have to add builtin predicates
 * for accessing the array.
 *
 */

static PROTOTYPE(BOOL, luther_array, (worker *));
static PROTOTYPE(BOOL, luther_array_arg, (worker *));
static PROTOTYPE(BOOL, luther_array_setarg, (worker *));
static PROTOTYPE(BOOL, luther_array_sizeof, (worker *));
static PROTOTYPE(BOOL, luther_array_reduce, (worker *));


/* array(Term)
 */
static BOOL luther_array(w)
     worker *w;
{
  TAGGED term;

  DerefNLL(term, Xw(0));

  return IsArray(term) ? TRUE : FALSE;
}


/* array_arg(ArgNo, Array, Object)
 *
 *   This function is used for accessing elements in arrays, compare to arg/3.
 *
 */
static BOOL luther_array_arg(w)
     worker *w;
{
  TAGGED argno, array, object;

  integer index;

  DerefNLL(argno, Xw(0));
  DerefNLL(array, Xw(1));

  /* Perform type check. */

  if (not(IsNUM(argno) && IsArray(array)))
    return FALSE;

  index = GetNumber(argno);

  if (index < 1)
    return FALSE;

  if (GetNumber(GetArraySize(array)) < index)
    return FALSE;

  DerefNLL(object, Xw(2));

  return unify(object, Ref(&GetArrayArg(array, index)), w);
}

/* array_setarg(ArgNo, Array, Object)
 *
 *   This function performs the equivalent of setarg for arrays.
 *
 */
static BOOL luther_array_setarg(w)
     worker *w;
{
  TAGGED argno, array, object;

  integer index;

  DerefNLL(argno, Xw(0));
  DerefNLL(array, Xw(1));

  /* Perform type check. */

  if (not(IsNUM(argno) && IsArray(array)))
    return FALSE;

  index = GetNumber(argno);

  if (index < 1)
    return FALSE;

  if (GetNumber(GetArraySize(array)) < index)
    return FALSE;

  DerefNLL(object, Xw(2));
    
#if (defined(TRAIL_ALL) | defined(UNBOUND))
  ValueTrail(w, BaseOffset(&(GetArrayArg(array, index))), GetArrayArg(array, index));
#else
  if (GetHVATime(BaseOffset(&(GetArrayArg(array, index)))) < w->uncond)
    ValueTrail(w, &(GetArrayArg(array, index)), GetArrayArg(array, index));
#endif

  GetArrayArg(array, index) = object;

  return TRUE;
}


/* array_sizeof(Array, Size)
 *
 *   This function is used for constructing arrays, or finding out the size of
 * an array (compare to functor/3).
 *
 */
static BOOL luther_array_sizeof(w)
     worker *w;
{
  TAGGED array, size;

  DerefNLL(array, Xw(0)); 
  DerefNLL(size,  Xw(1)); 

  if (IsVar(array))
    {
      /* Construct array of `size'. If is not a positive number return FALSE;
       */
      if (IsNUM(size) && (GetNumber(size) > 0))
	{
	  generic *array_object;
	  integer  array_size;

	  array_object = Generic(w->heap_top);

	  array_object->header  = 0x0;
	  array_object->method  = &array_method_table;
	  array_object->data[0] = size;

	  array_size = GetNumber(size);

	  w->heap_top += sizeofArray(0);

	  while (array_size--)
	    {
	      CreateHVA(w->heap_top, w);
	    }

	  return unify(array, Tagify(array_object, GEN), w);
	}
    }
  else if (IsArray(array))
    {
      return unify(size, GetArraySize(array), w);
    }

  return FALSE;
}


/* array_reduce(Operator, Array, Result)
 *
 *   This function implements efficient reduction of an array. The sum or the
 * product is calculated depending on the first argument (Operator = {+,*}).
 *
 */
static BOOL luther_array_reduce(Arg)
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
  if (not(IsATM(operator) && IsArray(array)))
    return FALSE;

  /* Initialize loop variables.
   */
  array_slot = &(GetArrayArg(array, 1));
  array_size = GetNumber(GetArraySize(array));

  /* 
   *  o   operator == "+"   then sum all elements in the array. 
   *  o   operator == "*"   then multiply all elements in the array. 
   *
   *   If the array consists only of integers then the result should be an integer,
   * but if there is at least one floating point number then the result is a float.
   * At the end of the computation, the float flag should only be true if a float
   * has been encountered. 
   */
  {
    TAGGED number;
    TAGGED total;
    double value;

    BOOL float_flag = FALSE;

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
      {
	total = make_float(value, w);
      }
    else if (value > (double)MAX_NUM)
      {
	if (value < DBL_MAX)
	  total = make_bignum_of_float(w, value);
	else
	  total = float_inf;
      }
    else
      {
	total = Make_Integer((integer)value);
      }

    return unify(total, result, w);
  }
}


void initialize_array(w)
     worker *w;
{
  def_c_pred(ATOM_ARRAY, luther_array, 1, PUBLIC, w);
  def_c_pred(ATOM_ARRAY_ARG, luther_array_arg, 3, PUBLIC, w);
  def_c_pred(ATOM_ARRAY_SETARG, luther_array_setarg, 3, PUBLIC, w);
  def_c_pred(ATOM_ARRAY_SIZEOF, luther_array_sizeof, 2, PUBLIC, w);
  def_c_pred(ATOM_ARRAY_REDUCE, luther_array_reduce, 3, PUBLIC, w); 
}
