/*
 * parallel.c
 *
 * Johan Bevemyr.....Wed Apr 14 1993
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"
#include "engine.h"
#include "unify.h"
#include "debug.h"
#include "inline.h"
#include "labelsort.h"
#include "initial.h"
#include "array.h"

double reduce_vector_plus(w)
    worker *w;
{
#ifdef PARALLEL
  register double sum;
  register s32 first, last, arraysize, mod, div;
  register TAGGED *array, number, *end;
  
  arraysize = GetNumber(GetArraySize(w->global->parallel_start.code));
  div = (s32) arraysize / w->global->active_workers;
  mod = arraysize % w->global->active_workers;
  first =  div * (w->pid-1) + (mod < w->pid ?  mod : (w->pid - 1));
  last = first + div + (w->pid <= mod ? 1 : 0);
  
  array = &(GetArrayArg((TAGGED) w->global->parallel_start.code,first));
  end   = &(GetArrayArg((TAGGED) w->global->parallel_start.code,last));
  
  for(sum = 0.0 ; array < end ; array += VARSIZE)
    {
      if(EventTrue(w->global->event_flag,GLOBAL_EVENT_FAIL)) return sum;
      DerefNLL(number, Ref(array));
      if(IsNUM(number))
	{
	  sum += (double) GetNumber(number);
	}
      else if (IsFLT(number))
	{
	  sum += GetFloat(number);
	}
      else
	{
	  Report_Global_Fail;
	  return sum;
	}
    }
  return sum;
#endif
}

double reduce_vector_times(w)
     worker *w;
{
#ifdef PARALLEL
  register double sum;
  register s32 first, last, arraysize, mod, div;
  register TAGGED *array, number, *end;
  
  arraysize = GetNumber(GetArraySize(w->global->parallel_start.code));
  div = (s32) arraysize / w->global->active_workers;
  mod = arraysize % w->global->active_workers;
  first =  div * (w->pid-1) + (mod < w->pid ?  mod : (w->pid - 1));
  last = first + div + (w->pid <= mod ? 1 : 0);
  
  array = &(GetArrayArg((TAGGED) w->global->parallel_start.code,first));
  end   = &(GetArrayArg((TAGGED) w->global->parallel_start.code,last));
  
  for(sum = 1.0 ; array < end ; array += VARSIZE)
    {
      if(EventTrue(w->global->event_flag,GLOBAL_EVENT_FAIL)) return sum;
      DerefNLL(number, Ref(array));
      if(IsNUM(number))
	{
	  sum *= (double) GetNumber(number);
	}
      else if (IsFLT(number))
	{
	  sum *= GetFloat(number);
	}
      else
	{
	  Report_Global_Fail;
	  return sum;
	}
    }
  return sum;
#endif /* PARALLEL */
}

TAGGED luther_await_nonvar(w,term)
     worker *w;
     TAGGED term;
{
#ifdef PARALLEL
  if(w->pid != 0)
    {
      IsNonVarOrLeftmost(term,w,return term);
    }
#endif /* PARALLEL */

  return term;
}
