/*
 * trail.h
 *
 * Johan Bevemyr.....Fri Nov  6 1992 
 * Patric Hedlin.....Fri Feb 17 1995
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef TRAIL_H
#define TRAIL_H

#define IsDoubleTrail(T) ((IsHVAorCVA(*T) && (HVATrailSize==2)) || \
			  IsValueTrail(T))

/* Using auto-increment is NOT an option. It is not defined in which 
 * order the arguments are evaluated
 */

#define PopFromTrail(T,V)   						   \
{									   \
  (T) -= 1;								   \
  V =  *(T);								   \
}

#define PushOnTrail(worker, value)					   \
{									   \
  TrailStat;								   \
									   \
  if (worker->trail_top == worker->trail_end)				   \
    {									   \
      shift_trail(worker);						   \
    }									   \
									   \
  *worker->trail_top = value;						   \
  worker->trail_top += 1;						   \
}

  
#if defined(TRAIL_ALL)
/*******************************************************************************
 *
 * Trail every binding.
 *
 */
#define Trail_GEN(V)       PushOnTrail(w,V)
#define Trail_HVA(V)       PushOnTrail(w,V)
#define Trail_SVA(V)       PushOnTrail(w,V)
#define Trail_CVA(V)       { WakeCVA; PushOnTrail(w,V);}

#else /* not TRAIL_ALL */

/**********************************************************************
 *
 * Trail conditional bindings only. 
 *
 */
#define Trail_GEN(V)       PushOnTrail(w,V)

#if defined(UNBOUND)
# define TrailHVASize 2
# define Trail_HVA(V,T)				\
{						\
  if (GetHVATime(T) < w->uncond)		\
    {						\
      PushOnTrail(w, T);			\
      PushOnTrail(w, V);			\
    }						\
}

# define Trail_CVA(V,T)				\
{						\
  WakeCVA;					\
  PushOnTrail(w, T);				\
  PushOnTrail(w, V);				\
}

#else /* not UNBOUND */
# define TrailHVASize 1
# define Trail_HVA(V)				\
{						\
  if (GetHVATime(V) < w->uncond)		\
    PushOnTrail(w, V);				\
}

# define Trail_CVA(V)				\
{						\
  WakeCVA;					\
  PushOnTrail(w, V);				\
}

#endif /* UNBOUND */

#if (defined(TIMESTAMP) & !defined(SHORT_SVA))
/*******************************************************************************
 *
 * Check conditional Stack variables using TIMESTAMP.
 *
 */
#define Trail_SVA(V)       { if(GetSVATime(V) < w->uncond) \
			       PushOnTrail(w, V); }
#else
#define Trail_SVA(V)       { if(GetSVATime(V) < (TAGGED *) w->choice) \
			       PushOnTrail(w, V); }
#endif /* TIMESTAMP ... */
#endif /* TRAIL_ALL */

/*******************************************************************************
 *
 *
 */
#define TidyValue(C,L)	((C)--)

#define TidySVA(C,L)				\
{						\
  if (TagToPointer(*(C)) > L)			\
    {                        			\
      *(C) = 0;              			\
    }                        			\
}


#if defined(UNBOUND)

#define AlwaysTrail(V)						\
{								\
  PushOnTrail(w,*TagToPointer(V));				\
  PushOnTrail(w, (TAGGED) BaseOffset(TagToPointer(V)));		\
}

/* Using auto-increment is NOT an option. It is not defined in which 
 * order the arguments are evaluated
 */

#define UnBind_HVA(V)							   \
{									   \
  *((TAGGED *) OffsetBase(*V)) = *((V)-1);				   \
  (V) -= 1;								   \
}

/* Using auto-increment is NOT an option. It is not defined in which 
 * order the arguments are evaluated
 */
#define UnBind_CVA(V)							   \
{									   \
  if (w->wake_count > 0)						   \
    {									   \
      w->wake_count--;							   \
      if (w->wake_count == 0)						   \
	{								   \
	  RemoveEvent(EVENT_WAKE);					   \
	}								   \
    }									   \
  *TagToPointer(*V) = *((V)-1);						   \
  (V) -= 1;								   \
}

#define TidyHVA(C,L)				\
{						\
  if (GetHVATime(*(--(C))) > L)			\
    {						\
      (C)[0] = 0;				\
      (C)[1] = 0;				\
    }						\
}

#define HVATrailSize 2
      
#else  /* not UNBOUND */

#define AlwaysTrail(V) 	    PushOnTrail(w,V)

#define UnBind_HVA(V)       (*((TAGGED *) OffsetBase(*V)) = (*V))

#define UnBind_CVA(V)				\
{						\
  if (w->wake_count > 0)			\
    {						\
      w->wake_count-=1;				\
      if (w->wake_count == 0)			\
	{					\
	  RemoveEvent(EVENT_WAKE);		\
	}					\
    }						\
  *TagToPointer(*V) = (*V);			\
}

#define TidyHVA(C,L)				\
{						\
  if (GetHVATime(*(C)) > L)			\
    {						\
      *(C) = 0;					\
    }						\
}

#define HVATrailSize 1

#endif /* UNBOUND */

/*******************************************************************************
 *
 * DeclareUncond
 *
 */
#if defined(TIMESTAMP) || defined(UNBOUND)

#define DeclareUncond(H,G,T)   register s32 H=T

#else

#define DeclareUncond(H,G,T)   register TAGGED *H=G

#endif  /* TIMESTAMP || UNBOUND */


/******************************************************************************/

#define UnBind_SVA(V)       *((TAGGED *) RemoveTag(*V,SVA)) = (*V)

#define IsValueTrail(T)     IsLST(*(T))

#define UndoValue(T)							   \
{									   \
  *TagToPointer(*T) = *((T)-1);						   \
  (T) -= 1;								   \
}

#define ValueTrail(worker, variable, value)	\
{						\
  PushOnTrail(worker, value);			\
  PushOnTrail(worker, Tagify(variable, LST));	\
}

/*******************************************************************************
 *
 * Usage: TidyTrail()
 *
 * Result: Removes all unnecessary trail entries. This have to be done
 *         otherwise the GC gets confused. (turnes ut not to be neccessary)
 *
 */

#define TidyTrail

/**********************************************************************
 *
 * Usage: Unwind_Trail("new trail top")
 *
 *
 *   All bindings recorded on the trail are undone. Entries tagged as NUM store
 * a saved heap_top value. This is used by the workers to restore the heap_top
 * when backtracking. Entries tagged with LST are value entries. The following
 * value contain the heap cell's previous value. Entries which are tagged STR
 * indicate that a parallel execution occured at this stage in the computation.
 *
 */

#define Unwind_Trail(T)							\
{									\
  register TAGGED *stop = (T);						\
									\
  while (w->trail_top > stop)						\
    {									\
      dec(w->trail_top);						\
									\
      switch (TagOf(*(w->trail_top))) {					\
									\
      case HVA:								\
	{								\
          UnBind_HVA(w->trail_top);					\
	}								\
	break;								\
									\
      CVA_CASE({							\
	UnBind_CVA(w->trail_top);					\
	break;								\
      });								\
									\
      case SVA:								\
	{								\
          UnBind_SVA(w->trail_top);					\
	}								\
	break;								\
									\
      case NUM: /* Only on workers, ignore here */			\
	break;								\
									\
      case BOX:								\
	{								\
	  FatalError("found BOX in trail");				\
	}								\
	break;								\
									\
      case ATM:								\
	{								\
	  Free_Segment((segment *) TagToPointer(*w->trail_top),w);	\
	}								\
	break;								\
									\
      case LST:								\
	{								\
          UndoValue(w->trail_top);					\
	}								\
	break;								\
									\
      case STR: /* Only in Parallel */					\
	{								\
	  UnwindWorkers;						\
	}								\
	break;								\
									\
      case GEN:								\
	{								\
          GetMethod(undo,*(w->trail_top))(*(w->trail_top));		\
	}								\
									\
      default: /* Catch UVA when admissable. */				\
	{								\
          UnBind_SVA(w->trail_top);					\
	}								\
      }									\
    }									\
}

/*******************************************************************************
 *
 * Usage: UnwindWorkers()
 * 
 *   Sleeping workers are activated by the sequential worker in order to unwind
 * their respective trail until they find a NUM tagged object.
 * 
 */ 
#if defined(PARALLEL)

#define UnwindWorkers							   \
{									   \
  Run_Parallel(W_BACKTRACK);						   \
}


/**********************************************************************
 *
 * Usage: TidyWorkers()
 * 
 *   Sleeping workers are activated by the sequential worker in order to tidy
 * their respective trail until they find a NUM tagged object. This operation
 * can be ignored since Worker_Tidy_Trail is a no-op.
 * 
 */ 
#define TidyWorkers(C)	/* Run_Parallel(W_TIDY) */

/**********************************************************************
 *
 * Usage: Worker_Unwind_Trail()
 * 
 *   Same as Unwind_Trail exept this version is used by sleeping workers.
 *
 * Note: How should we treat CVA:s.
 *
 * Note: SVA entries can be skipped since all environments have been 
 * deallocated.
 * 
 */ 
#define Worker_Unwind_Trail						   \
{									   \
  register TAGGED *stop = w->trail_start;				   \
									   \
  while (w->trail_top > stop)						   \
    {									   \
      dec(w->trail_top);						   \
									   \
      switch (TagOf(*(w->trail_top))) {					   \
									   \
      case HVA:								   \
	{								   \
	  UnBind_HVA(w->trail_top);					   \
	}								   \
	break;								   \
									   \
      CVA_CASE({	/* Is this the right thing to do? */		   \
	UnBind_CVA(w->trail_top);					   \
	break;								   \
      });								   \
									   \
      case SVA:								   \
	break;								   \
									   \
      case NUM: /* Only on workers */					   \
	{								   \
	  stop = w->trail_top;						   \
									   \
          w->heap_top = TagToPointer(*(w->trail_top));			   \
	}								   \
	break;								   \
									   \
      case BOX:								   \
	{								   \
	  FatalError("found BOX in trail");				   \
	}								   \
	break;								   \
									   \
      case ATM: /* Free segment */					   \
	{								   \
	  Free_Segment((segment *) TagToPointer(*w->trail_top),w);	   \
	}								   \
	break;								   \
									   \
      case LST:								   \
	{								   \
	  UndoValue(w->trail_top);					   \
	}								   \
	break;								   \
									   \
      case GEN:								   \
	{								   \
	  GetMethod(undo,*(w->trail_top))(*(w->trail_top));		   \
	}								   \
									   \
      default: /* Catch UVA when admissable. */				   \
	{								   \
          UnBind_SVA(w->trail_top);					   \
	}								   \
      }									   \
    }									   \
}


/*******************************************************************************
 *
 * Usage:  Worker_Tidy_Trail()
 *
 *   This operation is unnecessary since SVA entries are ignored by
 * Unwind_Trail anyway.
 *
 */
#define Worker_Tidy_Trail

#else /* PARALLEL */

#define UnwindWorkers
#define TidyWorkers

#endif /* PARALLEL */


#endif /* TRAIL_H */
