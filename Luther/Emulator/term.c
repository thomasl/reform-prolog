/*
 * term.c
 *
 * Johan Bevemyr.....Sat May 25 1991
 * Patric Hedlin.....Wed Aug 17 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 


#include <math.h>

#include "luther.h"
#include "engine.h"
#include "unify.h"


/* Note: These functions only work as long as sizeof(double) is a multiple
 *       of sizeof(TAGGED).
 */
double get_float(t)
    TAGGED t;
{
  double res;
  register TAGGED *p = (TAGGED *) &res;
  register TAGGED *f = RemoveTag(t,BOX);
    
  /* p[0] = f[1];
   * p[1] = f[2];
   */

  {
    register int size = sizeof(double)/sizeof(TAGGED);

    if (size == 2)
      {
	p[0] = f[VARSIZE];
	p[1] = f[VARSIZE+1];
      }
    else
      while (size--)
	{
	  p[size] = f[size+VARSIZE];
	}
  }

  return res;
}


/*
 * Create a float on the heap.
 *
 * The example illustrates the layout when sizeof(double) = 2 * sizeof(TAGGED)
 *
 *   res[0] = MakeFloatHeapBoxMarker(FLOATSIZE)
 *   res[1] = flt[0]
 *   res[2] = flt[1]
 *   res[3] = MakeFloatHeapBoxMarker(FLOATSIZE)
 */
#if not(defined(ALIGNED_BOXES))
TAGGED make_float(value,w)
     double value;
     worker *w;
{
  int size = sizeof(double)/sizeof(TAGGED);

  register TAGGED *flt = (TAGGED *) &value;
  register TAGGED *res = (TAGGED *) w->heap_top;
  TAGGED box_mark = MakeFloatHeapBoxMarker(size/VARSIZE + 2);

#ifdef ALIGNED_BOXES
  res += 1;
  DoubleAlignedAddress(res);
  res -= 1;
#endif /* ALIGNED_BOXES */

  res[0] = res[size+VARSIZE] = box_mark;
  
  w->heap_top += size+2*VARSIZE;

  if(size == 2)
    {
      res[VARSIZE] = flt[0];
      res[VARSIZE+1] = flt[1];
    }
  else
    while (size--)
      {
	res[size+VARSIZE] = flt[size];
      }

  return Tagify(res, BOX);
}
#endif

/*
 * Create a float in the atom space.
 *
 * The example illustrates the layout when sizeof(double) = 2 * sizeof(TAGGED)
 *
 *   res[0] = MakeFloatAtomBoxMarker(FLOATSIZE);
 *   res[1] = flt[0];
 *   res[2] = flt[1];
 *   res[3] = MakeFloatAtomBoxMarker(FLOATSIZE);
 */
TAGGED make_atomspace_float(value,w)
    double value;
    worker *w;
{
  int size = sizeof(double)/sizeof(TAGGED);

  register TAGGED *flt = (TAGGED *) &value;
  register TAGGED *res = (TAGGED *) atom_alloc((1+size+2*VARSIZE) *
					       sizeof(TAGGED),w);

  TAGGED box_mark = MakeFloatAtomBoxMarker(size/VARSIZE + 2);

#ifdef ALIGNED_BOXES
  res += 1;
  DoubleAlignedAddress(res);
  res -= 1;
#endif /* ALIGNED_BOXES */

  res[0] = res[size+VARSIZE] = box_mark;

  w->heap_top += size+2*VARSIZE;

  if(size == 2)
    {
      res[VARSIZE] = flt[0];
      res[VARSIZE+1] = flt[1];
    }
  else
    while (size--)
      {
	res[size+VARSIZE] = flt[size];
      }

  return Tagify(res, BOX);
}


TAGGED make_string_list(w,str)
     char *str;
     worker *w;
{
  register TAGGED res;
    
  if (*str == '\0')
    return atom_nil;

  Make_LST(w->heap_top, res);
  PushOnHeap(w->heap_top, Make_Integer((int) *str));

  inc(str);

  while (*str != '\0')
    {
      PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));
      PushOnHeap(w->heap_top, Make_Integer((int) *str));
      inc(str);
    }
  PushOnHeap(w->heap_top,atom_nil);

  return res;
}

