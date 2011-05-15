/*
 * copy-gc-ali.c
 *
 * Johan Bevemyr.....Fri Nov 25 1994
 *
 *
 * Implementation:
 *
 *   Based on the parallel copying algorithm described by Kairi-Ali 
 *   at SICS and a copying GC algorithm by Bevemyr and Lindgren (PLILP'94).
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"
#include "engine.h"
#include "think.h"
#include "unify.h"
#include "copy-gc-ali.h"


#if defined(COPY_GC) && defined(COPY_ALI)


/**********************************************************************
 *
 * Public primitives to support mark and collect phases in generic objects.
 *
 */
void live_mark_variable(slot, w)
     TAGGED* slot;
     worker* w;
{
  if (IsHeapTerm(*slot) || IsHeapBox(*slot) || IsGEN(*slot))
    {
      mark_variable(slot, w);
    }
}

TAGGED collect_variable(term, w)
     TAGGED  term;
     worker* w;
{
  if (IsHeapTerm(term) || IsHeapBox(term) || IsGEN(term))
    {
      return copy_variable(term, w);
    }
  else
    {
      return term;
    }
}


/**********************************************************************
 *
 * Garabage Collector:
 *
 */
void garbage_collect(w)
     worker *w;
{
  integer gc_start_time = usertime();
  natural initial_size;
  natural final_size;
  segment *used_segs;
  copy_state_t copy_state;

  /************************************************************
   * Pre statistics.
   */

  if (w->global->flags.gc_verbose == TRUE)
    {
      fprintf(stderr, "{verbose: Garbage Collection...}\n");
    }

  initial_size = used_segment_size_all(w);

  /************************************************************
   * Set up for GC
   */
    
#ifdef GENERATIONAL

  /*
   * Reset the new generation. Data will be bilt in this area
   * after GC.
   *
   */

  used_segs = w->new_used_segments;
  w->new_used_segments = NULL;
  (void) new_new_segment(w);

#else /* not GENERATIONAL */

  /*
   * Prepare a new area to copy the live data into
   *
   */

  used_segs = w->used_segments;
  w->used_segments = NULL;
  (void) new_segment(w);

#endif  /* GENERATIONAL */

  /******************************
   *
   * Perform garbage collection
   *
   */

#if 0
    w->trail_old_gen_top = w->trail_start;
#endif

  mark_copy(w, &copy_state);

  /****************************************************************
   * free old segments
   */

  free_new_seg_list(w, used_segs);
  free_new_seg_list(w, w->global->compare_seg);

  /* Restore the non-backtrackable memory area
   */

  w->global->compare_seg = NULL;
  allocate_static_segment(w);
 
#ifdef GENERATIONAL

  /****************************************************************
   * check for global collection
   */

  if (w->global->used_seg > w->global->free_seg)
    {
      TAGGED *new_gen_limit;

      fprintf(stderr, "{global collection}\n");

      used_segs = w->used_segments;
      w->used_segments = NULL;
      (void) new_segment(w);
      
      new_gen_limit = w->global->new_gen_limit;

      w->trail_old_gen_top = w->trail_start;
      w->global->new_gen_limit = (TAGGED *) w->global->globalstart;

      mark_copy(w, &copy_state);
      
      w->global->new_gen_limit = new_gen_limit;

      free_seg_list(w, used_segs);

      if (w->global->free_seg < w->global->used_seg+1)
	{
	  FatalError("Prolog out of memory");
	}
    }
#else
  if (w->global->free_seg < w->global->used_seg+1)
    {
      FatalError("Prolog out of memory");
    }

  if (w->heap_top > w->heap_margin)
    {
      (void) new_segment(w);
    }
#endif

#ifdef GENERATIONAL

  w->time += TIMEUNIT;
  w->uncond = w->time;
  w->old_gen_uncond = w->uncond;
  w->trail_old_gen_top = w->trail_top;

#endif

  /*****************************************************************
   * Report statistics
   */

  final_size = used_segment_size_all(w);

  if (w->global->flags.gc_verbose == TRUE)
    {
      fprintf(stderr, "{verbose: Garbage Collection...\n");
      fprintf(stderr, "\t reclaimed: %ld bytes\n", 
	      (integer) initial_size - (integer) final_size);
      fprintf(stderr, "\t survived:  %ld bytes}\n",
	      (integer) final_size);
    }

  w->stats->heap_gc_nr++;
  w->stats->heap_gc_bytes += initial_size - final_size;
  w->stats->heap_gc_time += usertime() - gc_start_time;
}

choicepoint *find_least_old(choice, trail)
     choicepoint *choice;
     TAGGED *trail;
{
  while (choice != NULL && choice->trail_top >= trail)
    {
      choice = choice->last_choice;
    }

  return choice;
}

/**********************************************************************
 * COLLECT GARBAGE
 */

static void mark_copy(w, copy_state)
     worker *w;
     copy_state_t *copy_state;
{
#ifdef GENERATIONAL
  copy_state->least_old = find_least_old(w->choice, w->trail_old_gen_top);
#else
  copy_state->least_old = (choicepoint *) w->stack_start;
#endif
  /****************************************
   * Mark
   */

  copy_state->wake_count = w->wake_count;
  {
    int gc_mark_time = usertime();

    GC_Start_Parallel(W_GC_MARK);

    mark(w, copy_state);

    GC_Stop_Parallel;

#if not(defined(EARLY_RESET))
    GC_Start_Parallel(W_GC_MARK_LATE);

#ifdef GENERATIONAL
    mark_trail_late(w, w->trail_top-1, w->trail_old_gen_top, copy_state);
#else
    mark_trail_late(w, w->trail_top-1, w->trail_start, copy_state);
#endif

    GC_Stop_Parallel;
#endif /* not EARLY_RESET */

    /******************************
     * Mark stats
     */

    w->stats->heap_gc_mark_time += usertime() - gc_mark_time;
  }

  /****************************************
   * Copy
   */

  copy_state->wake_count = w->wake_count;

  {
    int gc_copy_time = usertime();

    GC_Start_Parallel(W_GC_COPY);

    copy(w, copy_state);

#ifdef GENERATIONAL
    copy_compare_area(w, copy_state);
#endif
    GC_Stop_Parallel;


    /******************************
     * Copy stats
     */

    w->stats->heap_gc_copy_time += usertime() - gc_copy_time;
  }

  update_choicepoints(w, copy_state);

  GC_Run_Parallel(W_GC_UPDATECP);

  return;
}

/************************************************************************
 * Marking                                                              *
 * We mark all cells residing in live structures (lists, cva-lists and  *
 * structures). This is done so that the copying algorithm may detect   *
 * that a cell recide inside a live structure and copy the entire       *
 * surrounding structure.                                               *
 ***********************************************************************/

static void mark(w, copy_state)
     worker *w;
     copy_state_t *copy_state;
{
    mark_registers(w);
    mark_environments(w->frame, w, w->gc_info.env_size, copy_state);
    mark_choicepoints(w, copy_state);

#ifdef GENERATIONAL
    mark_compare_area(w, copy_state);
#endif
}

static void mark_registers(w)
     worker *w;
{
  register int arity = w->gc_info.arity;
  register TAGGED *areg = w->regs;
  
  while (arity--)
    {
      if (IsHeapTerm(X(arity)) || IsHeapBox(X(arity)) || IsGEN(X(arity)))
	{
	  *w->trail_top = X(arity);
	  mark_variable(w->trail_top, w);
	}
    }
}

static void mark_environments(frame, w, size, copy_state)
     register environment *frame;
     worker *w;
     int size;
     copy_state_t *copy_state;
{
  BOOL visited_flag = FALSE;

  while ((frame != NULL) && (visited_flag == FALSE) &&
	 (frame > (environment *) copy_state->least_old))
    {
      while (size--)
	{
	  if (!IsMarked(Yf(size)))
	    {
	      switch(TagOf(Yf(size)))
		{
		case HVA:
#ifdef CONSTR 
		case CVA:
#endif /* CONSTR */
		  mark_variable(&(Yf(size)), w);
		  break;

		case SVA:
		  break;

		case NUM:
		case ATM:
		  break;

		case BOX:
		case GEN:
		  mark_variable(&(Yf(size)), w);
		  break;

		case LST:
		case STR:
		  mark_variable(&(Yf(size)), w);
		  break;
#ifdef UNBOUND
		case UVA:
		  break;

#endif /* UNBOUND */
		default:
		  Error("mark_environment: tag is out of range");
		}
	      Mark(Yf(size));
	    }
	  else
	    visited_flag = TRUE;
	}
      size = FrameSize(frame->next_instr);
      frame = frame->cont_env;
    }
}


/*******************************************************************************
 *
 * In Parallel:
 * 
 *   We would like to perform 'early-reset' in the same way as it is performed
 * sequentially. However, this would result in barrier synchronization at each
 * choicepoint. The reason is that in order to reset an unmarked variable we
 * must ensure that all data created after the current choicepoint have been
 * marked. That is, all workers have marked the data they can reach from roots
 * allocated after the current choicepoint.
 *
 * An alternative method:
 *
 *   When a worker finds an unmarked variable which is a candidate for
 * 'early-reset' it waits until all other workers have marked their live data
 * above this point. For this to be more efficient than barrier synchronization
 * candidates for 'early-reset' would have to be rare. If this is the case then
 * the optimization could be removed all together.
 *
 *   The advantage of performing 'early-reset' when compiling the compiler
 * (a program that allocates 195 MB and results in a large number of GCs)
 * is very small ...
 *
 *   Increase in marking time:
 *   Increase in live data:
 *   Decrease in reset cells:
 *
 */
#ifdef EARLY_RESET

static TAGGED *mark_trail(w, t, stop, copy_state)
     worker *w;
     TAGGED *t, *stop;
     copy_state_t *copy_state;
{
  while (t >= stop)
    {
      if (IsNewSegmentMarker(*t))
	{
	  ResetEntry(t);
	}
      else if ((IsValueTrail(t) || IsHVAorCVA(*t)) &&
	       !IsMarked(*TagToPointer(*t)))
	{
	  if (IsValueTrail(t))	/* Value trail entry */
	    {  
	      ResetValueCell(t);
	    }
	  else if (IsHVA(*t))
	    {
	      ResetHVA(t);
	    }
#ifdef CONSTR
	  else if (IsCVA(*t))
	    {
	      if (copy_state->wake_count > 1)
		{
		  mark_variable(t, w);
		  t -= TrailHVASize;
		  copy_state->wake_count--;
		}
	      else
		{
		  ResetHVA(t);
		}
	    }
#endif /* CONSTR */
	}
      else if (IsValueTrail(t))	/* Value trail entry */
	{  
	  mark_variable(t-1, w);
	  t -= 2;
	}
      else if (IsHVA(*t))
	{
	  t -= TrailHVASize;
	}
#ifdef CONSTR
      else if (IsCVA(*t))
	{
	  wake_count--;
	  t -= TrailHVASize;
	}
#endif /* CONSTR */
      else
	{
	  t--;
	}
    }
  return t;
}

#else /* not EARLY_RESET */

static TAGGED *mark_trail(w, t, stop, copy_state)
     worker *w;
     TAGGED *t, *stop;
     copy_state_t *copy_state;
{
  while (t >= stop)
    {
      if (IsNewSegmentMarker(*t))
	{
	  SkipEntry(t);
	}
      else if ((IsValueTrail(t) || IsHVAorCVA(*t)) &&
	       !IsMarked(*TagToPointer(*t)))
	{
	  if (InOldGen(*t))
	    {
	      TAGGED *old_gen_term;

	      old_gen_term = TagToPointer(*t);
	      if ((IsHeapTerm(*old_gen_term) || IsHeapBox(*old_gen_term)) &&
		 InNewGen(*old_gen_term))
		{
		  *w->trail_top = *old_gen_term;
		  mark_variable(w->trail_top, w);
		}

	      if (IsValueTrail(t))
		{
		  if ((IsHeapTerm(*(t-1)) || IsHeapBox(*(t-1))) &&
		     InNewGen(*(t-1)))
		    {
		      *w->trail_top = *(t-1);
		      mark_variable(w->trail_top, w);
		    }
		  SkipValueCell(t);
		}
	      else
		{
		  SkipHVA(t);
		}
	    }
	  else if (IsValueTrail(t))	/* Value trail entry */
	    {  
	      SkipValueCell(t);
	    }
	  else if (IsHVA(*t))
	    {
	      SkipHVA(t);
	    }
#ifdef CONSTR
	  else if (IsCVA(*t))
	    {
	      if (copy_state->wake_count > 1)
		copy_state->wake_count--;

	      SkipHVA(t);
	    }
#endif /* CONSTR */
	}
      else if (IsValueTrail(t))	/* Value trail entry */
	{  
	  mark_variable(t-1, w);
	  t -= 2;
	}
      else if (IsHVA(*t))
	{
	  t -= TrailHVASize;
	}
#ifdef CONSTR
      else if (IsCVA(*t))
	{
	  copy_state->wake_count--;
	  t -= TrailHVASize;
	}
#endif /* CONSTR */
#if defined(GENERATIONAL)
      else if (IsSVA(*t))
	{
	  TAGGED *sva_pos;

	  sva_pos = TagToPointer(*t);

	  if (sva_pos < (TAGGED *) copy_state->least_old)
	    {

	      if ((IsHeapTerm(*sva_pos) || IsHeapBox(*sva_pos)) &&
		  InNewGen(*sva_pos))
		{
		  *w->trail_top = *sva_pos;
		  mark_variable(w->trail_top, w);
		}
	    }
	  t--;
	}
#endif /* GENERATIONAL */
      else
	{
	  t--;
	}
    }
  return t;
}

static TAGGED *mark_trail_late(w, t, stop, copy_state)
     worker *w;
     TAGGED *t, *stop;
     copy_state_t *copy_state;
{
  while (t >= stop)
    {
      if (IsNewSegmentMarker(*t))
	{
	  ResetEntry(t);
	}
      else if ((IsValueTrail(t) || IsHVAorCVA(*t)) &&
	       !IsMarked(*TagToPointer(*t)) && InNewGen(*t))
	{
	  if (IsValueTrail(t))	/* Value trail entry */
	    {  
	      ResetValueCell(t);
	    }
	  else if (IsHVA(*t))
	    {
	      ResetHVA(t);
	    }
#ifdef CONSTR
	  else if (IsCVA(*t))
	    {
	      if (copy_state->wake_count > 1)
		copy_state->wake_count--;

	      ResetHVA(t);
	    }
#endif /* CONSTR */
	}
      else if (IsValueTrail(t))	/* Value trail entry */
	{  
	  if (InNewGen(*t) &&
	     (IsHeapTerm(*(t-1)) || IsHeapBox(*(t-1))) &&
	     InNewGen(*(t-1)))
	    {
	      mark_variable(t-1, w);
	    }
	  SkipValueCell(t);
	}
      else if (IsHVA(*t))
	{
	  SkipHVA(t);
	}
#ifdef CONSTR
      else if (IsCVA(*t))
	{
	  copy_state->wake_count--;
	  SkipHVA(t);
	}
#endif /* CONSTR */
      else
	{
	  t--;
	}
    }
  return t;
}

#endif

#ifdef GENERATIONAL
/* Since compare-bindinga are not trailed crossgenerational pointers
 * are not recorded in the usual way (on the trail). Therefore we have
 * to search the compared-variable are for such pointers.
 */

static void mark_compare_area(w, copy_state)
     register worker *w;
     copy_state_t *copy_state;
{
  TAGGED *current;
  segment *seg;
  
  *w->global->compare_top = (TAGGED) NULL; /* watch dog */

  seg = w->global->compare_seg;

  while (seg != NULL)
    {
      current = &(seg->data[0]);

      while (*current != (TAGGED) NULL)
	{
	  if (InOldGen(*current))
	    {
	      if (*TagToPointer(*current) == Tagify((current+2*VARSIZE), HVA) &&
		 !IsMarked(*(current+2*VARSIZE)))
		{
		  mark_variable((current+2*VARSIZE), w);
		}
	    }
	  current += 3*VARSIZE;
	}
      seg = seg->next;
    }
}
#endif /* GENERATIONAL */

static void mark_choicepoints(w, copy_state)
     register worker *w;
     copy_state_t *copy_state;
{
  register int i;
  register TAGGED *areg, *t = w->trail_top-1;
  register choicepoint *choice = w->choice;
  
  copy_state->trailcells_deleted = 0;
  
#ifdef GENERATIONAL
  while (choice != NULL && (choice >= copy_state->least_old))
#else
  while (choice != NULL)
#endif /* GENERATIONAL */
    {
      t = mark_trail(w, t, choice->trail_top, copy_state);
      mark_environments(choice->cont_env, w, FrameSize(choice->next_instr),
			copy_state);
      i = choice->arity;
      areg = choice->areg;
      while (i--)
	{
	  if (IsHeapTerm(X(i)) || IsHeapBox(X(i)) || IsGEN(X(i)))
	    {
	      mark_variable(&(X(i)), w);
	    }
	}
      choice = choice->last_choice;
    }

#ifdef GENERATIONAL
  /* Finally, mark all cross generational entries and value 
   * entries entered after last GC but before next choice point
   */

  t = mark_trail(w, t, w->trail_old_gen_top, copy_state);
#endif /* GENERATIONAL */

  copy_state->last_copied_trail_entry = t;

}

/*******************************************************************************
 *
 *   This part implements the copying algorithm described by Khayri A. M. Ali,
 * in his paper "A Paralle Copying Garbage Collection Scheme for Shared-memory
 * Multiprocessors". The algorithm has been extended to cope with the term
 * representation that Prolog use.
 *   Instead of using pointer reversal for traversing the structure to mark we
 * use an open coded recursive procedure in which the stack is temporarily
 * stored in the empty area (the area we are going to copy the live data to).
 *
 * Note:  All internal structure cells, except the first in each structrue,
 *        should be marked with F after this pass.
 *
 *
 * Algorithm:
 *
 *   Cells are marked with M as they are passed through. This is done in order
 * to detect circular references. 
 *
 *   The variable `remaining_args' is used for detecting whether the current
 * cell is internal to a structure. When backing up this is needed for
 * deciding whether to mark the next cell or to continue to back up.
 *   When a structure (cva, list, or struct) is entered the `remaining_args'
 * variable is set to the number of arguments in the structure. The first
 * element is marked. Note that it is not marked with F. Subsequent arguments
 * to the structure are marked with F when backing up. If an argument is
 * already marked with F, it is skipped.
 * 
 *   One way of parallelising this scheme is to let a worker start marking
 * unmarked structure arguments. They are then marked with F and consequently
 * skipped by other workers.
 *
 */
static void mark_variable(start, w)
     TAGGED *start;
     worker *w;
{
  TAGGED *stack_top;      /* Variables for book keeping of the */
  TAGGED *stack_start;    /* traversal stack.                  */
  TAGGED *stack_end;
  segment *stack;
  TAGGED *current;        /* Current cell to mark              */
  natural remaining_args; /* Remaining arguments in current structure */

  if (InOldGen(*start)) return;

  /* Init stack
   */

  InitStack(stack_top, stack_start, stack_end, stack);

  remaining_args = 0;
  current = start;

 forward:
  {
    CheckStackOverflow(stack_top, stack_start, stack_end, stack); 

    if (IsMarked(*current)) goto backward;

    SafeMark(*current, goto backward);

    switch(TagOf(*current))
      {
      case HVA:
	{
	  if (InOldGen(*current)) goto backward;

	  /* We have to mark to catch circular references 
	   */
	  SavePosition(stack_top, remaining_args, current);

	  remaining_args = 0;
	  current = TagToPointer(*current);
	}
	goto forward;

#ifdef CONSTR
      case CVA:
	{
	  TAGGED *next;

	  if (InOldGen(*current)) goto backward;

	  next = TagToPointer(*current);

	  if (IsMarkedF(*(next+VARSIZE))) goto backward;

	  SavePosition(stack_top, remaining_args, current);

	  remaining_args = 1;
	  current = next;
	}
	goto forward;

#endif /* CONSTR */

      case SVA:
	{
	  FatalError("mark_variable: stack variable in heap");
	}
	break;

      case NUM:
      case ATM:
	goto backward;

      case BOX:
	{      
	  if (not(InOldGen(*current)))
	    Mark(*TagToPointer(*current));
	}
	goto backward;

      case GEN:
	{
	  if (not(InOldGen(*current)))
	    GetMethod(mark, *current)(*current, w);
	}
	goto backward;

      case LST:
	{
	  TAGGED *next;

	  if (InOldGen(*current)) goto backward;

	  next = TagToPointer(*current);

	  if (IsMarkedF(*(next+VARSIZE))) goto backward;

	  SavePosition(stack_top, remaining_args, current);

	  remaining_args = 1;
	  current = next;
	}
	goto forward;
    
      case STR:
	{ 
	  TAGGED *next;
	  int arity, i;

	  if (InOldGen(*current)) goto backward;

	  next = TagToPointer(*current);

	  if (IsMarkedF(*(next+VARSIZE))) goto backward;

	  arity = StructArity(next);

	  if (arity == 0)
	    {
	      Error("mark_variable: structure arity is zero");
	      return;
	    }

	  SavePosition(stack_top, remaining_args, current);

	  remaining_args = arity;
	  current = next;
	}
	goto forward;
    
#ifdef UNBOUND
      case UVA:
	goto backward;
#endif /* UNBOUND */
      default:
	Error("mark_variable: tag is out of range");
      }
  }

  /*   Unwind until the stack is empty or an entry is popped where
   * `remaining_args' is non-zero (!= 0);
   */
 backward:
  {
    if (remaining_args == 0)
      {
	if (StackEmpty(stack_top, stack_start)) 
	  {
	    if (LastStack(stack_top)) /* done */
	      {
		return;
	      }
	    else
	      {
		PopStack(stack_top, stack_start, stack_end, stack); 
	      }
	  }

	current = PopTAGGED(stack_top);
	remaining_args = PopSize(stack_top);

	goto backward;
      }
    else
      {
	remaining_args -= 1;
	current += VARSIZE;

	if (IsMarkedF(*current))
	  {
	    goto backward;
	  }
	else
	  {
	    SafeMarkF(*current, goto backward);
	    goto forward;
	  }
      }
  }
}

/***********************************************************************
 * Finding data to copy (from registers, choicepoints and environments *
 ***********************************************************************/

static void copy(w, copy_state)
     worker *w;
     copy_state_t *copy_state;
{
  copy_registers(w);
  copy_environments(w->frame, w, w->gc_info.env_size, copy_state);
  copy_choicepoints(w, copy_state);
}

static void copy_registers(w)
     worker *w;
{
  register int arity = w->gc_info.arity;
  register TAGGED *areg = w->regs;
  
  while (arity--)
    {
      if (IsHeapTerm(X(arity)) || IsHeapBox(X(arity)))
	{
	  X(arity) = copy_variable(X(arity), w);
	}
    }
}

static void copy_environments(frame, w, size, copy_state)
     register environment *frame;
     worker *w;
     int size;
     copy_state_t *copy_state;
{
  BOOL visited_flag = FALSE;

  while ((frame != NULL) && (visited_flag == FALSE) &&
	 (frame > (environment *) copy_state->least_old))
    {
      while (size--)
	{
	  if (IsMarked(Yf(size)))
	    {
	      switch(TagOf(Yf(size)))
		{
		case HVA:
#ifdef CONSTR 
		case CVA:
#endif /* CONSTR */
		  Yf(size) = copy_variable(Yf(size), w);
		  break;

		case SVA:
		  break;

		case NUM:
		case ATM:
		  break;

		case BOX:
		  {
		    if (IsHeapBox(Yf(size)))
		      {
			Yf(size) = copy_variable(Yf(size), w);
		      }
		  }
		  break;

		case GEN:
		case LST:
		case STR:
		  Yf(size) = copy_variable(Yf(size), w);
		  break;

#ifdef UNBOUND 
		case UVA:
		  break;
#endif /* UNBOUND */

		default:
		  Error("copy_environment: tag is out of range");
		}
	      UnMark(Yf(size));
	    }
	  else
	    visited_flag = TRUE;
	}
      size = FrameSize(frame->next_instr);
      frame = frame->cont_env;
    }
}


static TAGGED *copy_trail(w, t, stop, copy_state)
     worker *w;
     TAGGED *t, *stop;
     copy_state_t *copy_state;
{
  while (t >= stop)
    {
#if defined(GENERATIONAL) 
      if ((*t == 0) || IsNUM(*t) || IsSTR(*t))
	{
	  t -= 1;
	}
      else if (IsSVA(*t))
	{
	  TAGGED *sva_pos;

	  sva_pos = TagToPointer(*t);

	  if (sva_pos < (TAGGED *) copy_state->least_old)
	    {
	      if ((IsHeapTerm(*sva_pos) || IsHeapBox(*sva_pos)) &&
		  InNewGen(*sva_pos))
		{
		  *sva_pos = copy_variable(*sva_pos, w);
		}
	    }

	  t -= 1;
	}
#else /* not GENERATIONAL */
      if ((*t == 0) || IsSVA(*t) || IsNUM(*t) || IsSTR(*t))
	{
	  t -= 1;
	}
#endif /* GENERATIONAL */
      else if (IsATM(*t))
	{
	  Error("copy_trail: found atom (ATM) while copying trail");
	  t -= 1;
	}
      else
	{
	  if (IsHVA(*t) && IsMarked(*TagToPointer(*t)))
	    {
#ifdef EARLY_RESET 
	      /* In this case there is an error */
	      Error("copy_trail: found uncoppied variable (HVA) in trail");
	      ResetHVA(t);
#else /* not EARLY_RESET */
	      CopyHVA(t);
#endif /* EARLY_RESET */
	    }
	  else if (IsValueTrail(t) && IsMarked(*TagToPointer(*t)))
	    {
	      CopyValueCell(t);
	    }
	  else
	    {
	      if (InNewGen(*t))
		{
		  /* If we get this far, then we know that the 
		   * trail entry has been copied, otherwise it would 
		   * have been zeroed 
		   */
		  register TAGGED *old = TagToPointer(*t);
		  register uword tag = TagOf(*t);

		  *t = Tagify(GetNewLocation(*old), tag);

		  if (IsValueTrail(t))
		    {
		      t -= 1;
		      if (IsHeapTerm(*t) || IsHeapBox(*t))
			*t = copy_variable(*t, w);
		      t -= 1;
		    }
		  else
		    {
		      t -= TrailHVASize;
		    }
		}
	      else /* HVA or ValueTrail in old gen */
		{
		  TAGGED *old_gen_term;

		  old_gen_term = TagToPointer(*t);

		  if ((IsHeapTerm(*old_gen_term) || IsHeapBox(*old_gen_term))
		      && InNewGen(*old_gen_term))
		    {
		      *old_gen_term = copy_variable(*old_gen_term, w);
		    }

		  if (IsValueTrail(t))
		    {
		      if ((IsHeapTerm(*(t-1)) || IsHeapBox(*(t-1)))
			  && InNewGen(*(t-1)))
			{
			  *(t-1) = copy_variable(*(t-1), w);
			}
		      SkipValueCell(t);
		    }
		  else
		    {
		      SkipHVA(t);
		    }
		}
	    }
	}
    }
  return t;
}

#ifdef GENERATIONAL
/* Since compare-bindinga are not trailed crossgenerational pointers
 * are not recorded in the usual way (on the trail). Therefore we have
 * to search the compared-variable are for such pointers.
 */

static void copy_compare_area(w, copy_state)
     register worker *w;
     copy_state_t *copy_state;
{
  TAGGED *current;
  segment *seg;
  
  seg = w->global->compare_seg;

  while (seg != NULL)
    {
      current = &(seg->data[0]);

      while (*current != (TAGGED) NULL)
	{
	  if (InOldGen(*current) &&
	     *TagToPointer(*current) == Tagify((current+2*VARSIZE), HVA))
	    {
	      TAGGED *old_term = TagToPointer(*current);

	      *old_term = copy_variable(*old_term, w);
	    }
	  current += 3*VARSIZE;
	}
      seg = seg->next;
    }
}
#endif /* GENERATIONAL */


static void copy_choicepoints(w, copy_state)
     register worker *w;
     copy_state_t *copy_state;
{
  register int i;
  register TAGGED *areg, *t = w->trail_top-1;
  register choicepoint *choice = w->choice;
  register TAGGED *old;
  
#ifdef GENERATIONAL
  while ((choice != NULL) && (choice >= copy_state->least_old))
#else
  while (choice != NULL)
#endif
    {
      t = copy_trail(w, t, choice->trail_top, copy_state);
      copy_environments(choice->cont_env, w,
			FrameSize(choice->next_instr), copy_state);
      i = choice->arity;
      areg = choice->areg;

      while (i--)
	{
	  if (IsHeapTerm(X(i)) || IsHeapBox(X(i)))
	    {
	      X(i) = copy_variable(X(i), w);
	    }
	}
      choice = choice->last_choice;
    }
#ifdef GENERATIONAL 
  /* copy_trail between last choice point and trail_old_gen_top
   */
  t = copy_trail(w, t, w->trail_old_gen_top, copy_state);
#endif /* GENERATIONAL */

  copy_state->last_copied_trail_entry = t;
}


/**********************************************************************
 *
 * copy_variable() returns a tagged pointer to the copied object. 
 *
 */

inline static TAGGED *check_size(size, w)
     integer size;
     worker *w;
{
  /* Check that we do not overwrite the end of the segment, we
   * use a small buffer of 10 word to be extra safe
   */

#ifdef GENERATIONAL
  if (w->old_heap_top+size > (w->old_heap_end-10))
    {
      (void) new_segment(w);
    }

  if (w->old_heap_top+size > (w->old_heap_end-10))
    {
      FatalError("Structure do not fit into segment");
    }

  return w->old_heap_top;
#else
  if (w->heap_top+size > (w->heap_end-10))
    {
      (void) new_segment(w);
    }

  if (w->heap_top+size > (w->heap_end-10))
    {
      FatalError("Structure do not fit into segment");
    }

  return w->heap_top;
#endif
}

/* Note: gc_check_size() resides here since check_size is inlined.
 */
TAGGED *gc_check_size(size, w)
     integer size;
     worker *w;
{
  return check_size(size, w);
}


inline static TAGGED *copy_hva(term, w)
     TAGGED term;
     worker *w;
{
  TAGGED *new;
  TAGGED *old;
  int size;

  old = TagToPointer(term);

  /*   Check if the cell reside in a live structure, in that case the entire
   * structure has to be copied.
   */
  if (InOldStruct(old)) 
    {
      TAGGED old0; /* the contents of old[0] */
      int dist, i;

      /* Calculate the distance to the head of the structure.
       *
       * Find head of struct.
       */
      for (dist = 0; IsMarkedF(*old); old -= VARSIZE, dist += VARSIZE);

      /* Find size of structure size = VARSIZE already set.
       */
      for (size = VARSIZE; IsMarkedF(old[size]); size += VARSIZE);

      /*   We lock cells to ensure that no other worker may copy the same term
       * while we are. If another worker manage to copy the cell ahead of us,
       * we then return NULL to signal this.
       */
      LockCell(old[0], old0, return NULL);

      new = check_size(size, w);
      
      new[0] = FromOld(old0);
      Forward(old[0], &(new[0]));

      if (VARSIZE == 2)
	new[1] = old[1];

      /* Copy the structure.
       */
      for (i = VARSIZE; i < size; i += VARSIZE)
	{
	  new[i] = FromOldArg(old[i]);
	  Forward(old[i], &(new[i]));

	  if (VARSIZE == 2)
	    new[i+1] = old[i+1];
	}

      /*   Mark the original cell so that it can be found when all
       * arguments have been copied
       */
      if (dist != 0)
	{
	  MarkF(new[0]);
	  MarkF(new[dist]);
	}
    }
  else
    {
      TAGGED old0; /* the contents of old[0] */

      LockCell(old[0], old0, return NULL);

      /* The variable does not reside in a live structure. 
       */
      if (IsOrdered(term))
	{
	  size = 2*VARSIZE;
	  new = check_size(size, w);

	  new[0] = *(old-VARSIZE);
	  
	  new[VARSIZE] = FromOld(old0);

	  Forward(old[0], (new+VARSIZE));

	  if (VARSIZE == 2)
	    new[VARSIZE+1] = *(old+1);

	  MarkF(new[0]);
	  MarkF(new[VARSIZE]);
	}
      else
	{
	  size = VARSIZE;

	  new = check_size(size, w);

	  new[0] = FromOld(old0);
	  Forward(old[0], new);

	  if (VARSIZE == 2)
	    new[1] = old[1];
	}
    } 

  Mark(new[0]);
  new += size;

  CopyHeapTop = new;

  return (new-VARSIZE);
}


inline static TAGGED *copy_cva(term, w)
     TAGGED term;
     worker *w;
{
  TAGGED *new;
  TAGGED *old;
  TAGGED old0; /* the contents of old[0] */

  new = check_size(VARSIZE*2, w);
  old = TagToPointer(term);

  /*   We lock cells to ensure that no other worker may copy the same term
   * while we are. If another worker manage to copy the cell ahead of us,
   * we then return NULL to signal this.
   */
  LockCell(old[0], old0, return NULL);
	
  new[0] = FromOld(old0);
  Forward(old[0], &(new[0])); 

  new[VARSIZE] = FromOldArg(old[VARSIZE]);
  Forward(old[VARSIZE], &(new[VARSIZE]));
	
  if (VARSIZE == 2)
    {
      new[1] = old[1];
      new[3] = old[3];
    }

  Mark(new[0]);
  new += 2*VARSIZE;
  CopyHeapTop = new;

  return (new-VARSIZE);
}


inline static TAGGED *copy_box(term, w)
     TAGGED term;
     worker *w;
{
  TAGGED *new, *ret;
  TAGGED *old;
  TAGGED old0;     /* the contents of old[0] */
  integer i, size;

  old = TagToPointer(term);

  /*   We lock cells to ensure that no other worker may copy the same term
   * while we are. If another worker manage to copy the cell ahead of us,
   * we then return NULL to signal this.
   */
  LockCell(old[0], old0, return NULL);

  size = GetBoxSize(old0);

  ret = new = check_size(VARSIZE*size, w);
	
  new[0] = FromOld(old0);

  for (i = 1; i < size*VARSIZE; i++) /* copy term */
    {
      new[i] = old[i];
    }

  Forward(old[0], &(new[0])); /* update oldspace */

  new += size*VARSIZE;
  CopyHeapTop = new;

  return ret;
}


inline static TAGGED *copy_internal_list(old, w)
     TAGGED old[];
     worker *w;
{
  TAGGED *new;
  TAGGED old0; /* the contents of old[0] */
  int dist, i, size;

  /* Find head of struct.
   */
  for (dist = 0; IsMarkedF(*old); old -= VARSIZE, dist += VARSIZE);

  /* Find size of structure.
   */
  for (size = VARSIZE; IsMarkedF(old[size]); size += VARSIZE);

  /*   We lock cells to ensure that no other worker may copy the same term
   * while we are. If another worker manage to copy the cell ahead of us,
   * we then return NULL to signal this.
   */
  LockCell(old[0], old0, return NULL);
  
  new = check_size(size, w);

  new[0] = FromOld(old0);
  Forward(old[0], &(new[0]));

  if (VARSIZE == 2)
    new[1] = old[1];

  /* Copy the structure.
   */
  for (i = VARSIZE; i < size; i += VARSIZE)
    {
      new[i] = FromOldArg(old[i]);
      Forward(old[i], &(new[i]));

      if (VARSIZE == 2)
	new[i+1] = old[i+1];
    }
  
  if (dist != 0)
    {
      MarkF(new[0]);
      MarkF(new[dist]);
    }

  Mark(new[0]);
  new += size;

  CopyHeapTop = new;

  return (new-VARSIZE);
}


inline static TAGGED *copy_list(term, w)
     TAGGED term;
     worker *w;
{
  TAGGED *new;
  TAGGED *old;

  old = TagToPointer(term);

  if (IsMarkedF(old[0]))
    {
      return copy_internal_list(old, w);
    }
  else
    {
      TAGGED old0; /* the contents of old[0] */

      /*   We lock cells to ensure that no other worker may copy the same term
       * while we are. If another worker manage to copy the cell ahead of us,
       * we then return NULL to signal this.
       */
      LockCell(old[0], old0, return NULL);

      new = check_size(VARSIZE*2, w);
	
      new[0] = FromOld(old0);
      Forward(old[0], &(new[0])); 

      new[VARSIZE] = FromOldArg(old[VARSIZE]);
      Forward(old[VARSIZE], &(new[VARSIZE]));
	
      if (VARSIZE == 2)
	{
	  new[1] = old[1];
	  new[3] = old[3];
	}

      Mark(new[0]);
      new += 2*VARSIZE;
      CopyHeapTop = new;

      return (new-VARSIZE);
    }
}


inline static TAGGED *copy_struct(term, w)
     TAGGED term;
     worker *w;
{
  TAGGED *new;
  TAGGED *old;
  TAGGED old0; /* the contents of old[0] */
  int i, size;

  size = ArityOf(GetFunctor(term));

  size = (size+1)*VARSIZE;

  new = check_size(size, w);
  old = TagToPointer(term);

  /*   We lock cells to ensure that no other worker may copy the same term
   * while we are. If another worker manage to copy the cell ahead of us,
   * we then return NULL to signal this.
   */
  LockCell(old[0], old0, return NULL);

  new[0] = FromOld(old0);     /* copy term       */
  Forward(old[0], &(new[0])); /* update oldspace */

  if (VARSIZE == 2)
    new[1] = old[1];

  for (i = VARSIZE; i < size; i += VARSIZE)
    {
      new[i] = FromOldArg(old[i]); /* copy term       */
      Forward(old[i], &(new[i]));  /* update oldspace */

      if (VARSIZE == 2)
	new[i+1] = old[i+1];
    }

  Mark(new[0]);			/* mark first cell */
  CopyHeapTop = new;
  CopyHeapTop += size;

  return (CopyHeapTop-VARSIZE);
}

inline static TAGGED *copy_term(term, w)
     TAGGED term;
     worker *w;
{
  switch(TagOf(term))
    {
    case HVA:
      return copy_hva(term, w);

#ifdef CONSTR
    case CVA:
      return copy_cva(term, w);
#endif  /* CONSTR */

    case SVA:
      Error("copy_term: found stack variable in heap");
      break;

    case NUM:
    case ATM:
      Error("copy_term: attempt to copy a non-heap term");
      break;

    case BOX:
      return copy_box(term, w);

    case GEN:
      Error("copy_term: attempt to copy a non-heap term");
      break;

    case LST:
      return copy_list(term, w);

    case STR:
      return copy_struct(term, w);

#ifdef UNBOUND
    case UVA:
      Error("copy_term: attempt to copy a non-heap term");
      break;
#endif /* UNBOUND */
    }
}

/*
 * Note: We assume that all internal live variables have been marked
 *
 * Note: We need a separate case to handle generic objects (GEN) since we
 *       do not want to scan the internals.
 */
static TAGGED copy_variable(start, w)
     TAGGED start;
     worker *w;
{
  TAGGED result;
  TAGGED *next, tag, *old;

  tag = TagOf(start);
  old = TagToPointer(start);

  if (not(IsHeapObj(start)) && not(IsHeapBox(start)))
    {
      UnMark(start);
      return start;
    }

  if (InOldGen(start))
    {
      result = start;
    }
  else if (IsCopied(*old))
    {
      TAGGED new_location;

      WaitUntilCopied(start, new_location);
      
      result = Tagify(GetNewLoc(new_location), tag);
    }
  else if (IsHeapTerm(start))
    {
      /* First we copy the root */

      next = copy_term(start, w);

      result = scan(next, tag, w);
    }
  else if (IsHeapBox(start))
    {
      next = copy_box(start, w);

      result = Tagify(next, BOX);
    }
  else if (IsGEN(start))
    {
      next = GetMethod(collect, start)(start, w);

      result = Tagify(next, GEN);
    }
  else
    {
      Error("copy-gc-ali: nothing to copy...");

      result = start;
    }

  return result;
}

/**************************************************/

static TAGGED scan(next, tag, w)
     TAGGED *next, tag;
     worker *w;
{
  TAGGED *prev;
#ifndef NDEBUG
  int debugcounter = 0;
#endif /* NDEBUG */

  prev = TagToPointer(NULL);              /* dummy start value */

 forward:
  {
#ifndef NDEBUG
    debugcounter++;
#endif /* NDEBUG */

    switch(TagOf(*next))
      {
      case HVA:
	{
	  CheckCopied(*next);
	  CopyTerm(copy_hva);
	}
	/* continue to forward or backward */
		
#ifdef CONSTR
      case CVA:
	{
	  CheckCopied(*next);
	  CopyTerm(copy_cva);
	}
	/* continue to forward or backward */
#endif /* CONSTR */

      case SVA:
	{
	  FatalError("Stack reference from heap");
	}

      case NUM:
      case ATM:
	goto backward;

      case BOX:
	{
	  if (IsHeapBox(*next))
	    {
	      TAGGED *new;

	      CheckCopied(*next);

	      /*   If another worker copies the box ahead of us,
	       * the copy_box method should return NULL.
	       */
	      new = copy_box(*next, w);
	      
	      if (new != NULL)
		{
		  TAGGED gcbits = GetGCBits(*next);
		
		  *next = SetGCBits(Tagify(new, BOX), gcbits);
		}
	      else
		{
		  GetCopied(*next);
		}
	    }
	}
	goto backward;

      case GEN:
	{
	  TAGGED *new;

	  CheckCopied(*next);

	  /*   If another worker collects the object ahead of us,
	   * the collect method should return NULL.
	   */
	  new = GetMethod(collect, *next)(*next, w);
	  
	  if (new != NULL)
	    {
	      TAGGED gcbits = GetGCBits(*next);

	      *next = SetGCBits(Tagify(new, GEN), gcbits);
	    }
	  else
	    {
	      GetCopied(*next);
	    }
	}
	goto backward;

      case LST:
	{
	  CheckCopied(*next);
	  CopyTerm(copy_list);
	}
	/* continue to forward or backward */

      case STR:
	{
	  CheckCopied(*next);
	  CopyTerm(copy_struct);
	}
	/* continue to forward or backward */

#ifdef UNBOUND
      case UVA:
	goto backward;
#endif /* UNBOUND */

#ifndef NDEBUG
      default:
	{
	  FatalError("Illegal tag in gc-scan");
	}
#endif 
	break;
      }
  }

 backward:
  {
    if (!IsMarked(*next))	/* not last */
      {
	next = next - VARSIZE;

	goto forward;
      }
    else			/* last in structure */
      {
	UnMark(*next);

	if (IsMarkedF(*next))	/* was entered from the inside */
	  {
	    UnMarkF(*next);

	    do {
	      next += VARSIZE;
	    } while (!(IsMarkedF(*next)));
	  
	    UnMarkF(*next);
	  }

	if (prev == TagToPointer(NULL))	/* done */
	  {
	    return Tagify(next, tag);
	  }
	else 
	  {
	    TAGGED oldprev;
	    TAGGED gcbits;
	  
	    oldprev = *prev;
	    gcbits = GetGCBits(oldprev);

	    *prev = SetGCBits(Tagify(next, tag), gcbits);
	    next = prev;

	    prev = TagToPointer(oldprev);
	    tag = TagOf(oldprev);

	    goto backward;
	  }
      }
  }
}

/**********************************************************************/
 
static void update_choicepoints(w, copy_state)
     worker *w;
     copy_state_t *copy_state;
{
  register choicepoint *choice = w->choice;
  TAGGED *t = w->trail_top-1;
  TAGGED *new = w->heap_top;
  
  while ((choice != NULL) && (choice >= copy_state->least_old))
    {
      while (t >= choice->trail_top)
	{
	  if (*t == 0)
	    copy_state->trailcells_deleted--;
	  t--;
	}
      choice->trail_top -= copy_state->trailcells_deleted;
      choice->global_top = new;
      choice = choice->last_choice;
    }

#ifndef NDEBUG
  if (copy_state->trailcells_deleted != 0)
    Error("`trailcells_deleted' != 0");
#endif

  /* compact trail */

  {
    register TAGGED *dest, *current;

#if GENERATIONAL
    dest = w->trail_old_gen_top;
#else
    dest = w->trail_start;
#endif

    for (current = dest;
	 current != w->trail_top;
	 current++)
      if (*current != 0)
	{
	  *dest = *current;
	  dest++;
	}
    w->trail_top = dest;
  }
}


/*******************************************************************************
 *
 * Support for parallel garabage collection.
 *
 */

#ifdef PARALLEL

BOOL worker_garbage_collect(w, from_event_loop)
     worker *w;
     BOOL from_event_loop;
{
  char *heap_gc_start;
  int gc_start_time;
  double gc_start_walltime;
  segment *used_segs;
  copy_state_t copy_state;

#ifdef GENERATIONAL
  copy_state.least_old = find_least_old(w->choice, w->trail_old_gen_top);
#else
  copy_state.least_old = (choicepoint *) w->stack_start;
#endif

  /**********************************************************************
   * 
   * pre statistics 
   *
   */

  gc_start_time = usertime();
  gc_start_walltime = walltime();
  heap_gc_start = (char *) w->heap_top;

  /**********************************************************************
   *  GC
   */

  copy_state.wake_count = w->wake_count;

  if (from_event_loop) goto if_done;

 wait_for_work:
  {
    w->gc_info.done[w->pid-1] = TRUE;
  }
  
 wait_first_time:
  {
    synchronize(w);
  }
  
 if_done:
  {
    /* Decode command from sequential worker */

    switch(w->global->parallel_start.type)
      {
      case W_GC_MARK:
	{
	  /* Some more initialization */
#ifdef GENERATIONAL
	  used_segs = w->new_used_segments;
	  w->new_used_segments = NULL;
	  (void) new_new_segment(w);
#else
	  used_segs = w->used_segments;
	  w->used_segments = NULL;
	  (void) new_segment(w);
#endif

	  /* GC_Sequentialize_Low_To_High; */
	  mark(w, &copy_state);
	  worker_mark_rest_trail(w, &copy_state);
	}
	goto wait_for_work;

      case W_GC_MARK_LATE:
	{
#if not(defined(EARLY_RESET))
#  ifdef GENERATIONAL 
	  mark_trail_late(w, w->trail_top-1, w->trail_old_gen_top, &copy_state);
#  else
	  mark_trail_late(w, w->trail_top-1, w->trail_start, &copy_state);
#  endif
#endif
	}
	goto wait_for_work;

      case W_GC_COPY:
	{
	  GC_Sequentialize_Low_To_High; 
	  copy(w, &copy_state);
	  worker_copy_rest_trail(w, &copy_state);
	}
	goto wait_for_work;

      case W_GC_UPDATECP:
	{
	  GC_Sequentialize_Low_To_High;
	  worker_update_choicepoints(w, &copy_state);

	  /* Almost done, free old blocks */
      
	  while (used_segs != NULL)
	    {
	      segment *seg;

	      seg = used_segs;
	      used_segs = seg->next;
#ifdef GENERATIONAL
	      new_return_segment(seg, w);
#else
	      return_segment(seg, w);
#endif GENERATIONAL
	    }

	  /* Note: No memory check is necessary here, the sequential worker
	   *       takes care of that.
	   */
#ifdef GENERATIONAL
	  if (w->global->used_seg > w->global->free_seg)
	    {
	      Error("global garbage collection is required");
	    }
#else
	  if (w->heap_top > w->heap_margin)
	    {
	      (void) new_segment(w);
	    }
#endif
	}
	goto wait_for_work;
      
      case W_GC_FINISH:
	{
	  worker_finish(w);
	}
	goto done;

      default:
	{
	  FatalError("No such command");
	}
      }
  }

  /**********************************************************************
   * 
   * done
   *
   */
 done:
  {
    return TRUE;
  }
}

static void worker_finish(w)
     worker *w;
{
#if defined(UNBOUND) || defined(TIMESTAMP)
#else
  w->uncond = w->choice->global_top;
#endif /* UNBOUND */
}

/**********************************************************************/

static void worker_mark_rest_trail(w, copy_state)
     worker *w;
     copy_state_t *copy_state;
{
  TAGGED *t;

  t = copy_state->last_copied_trail_entry;

#ifndef GENERATIONAL
  (void) mark_trail(w, t, w->trail_start, copy_state);
#endif 

  return;
}

static void worker_copy_rest_trail(w, copy_state)
     worker *w;
     copy_state_t *copy_state;
{
  TAGGED *t;

  t = copy_state->last_copied_trail_entry;

#ifndef GENERATIONAL
  (void) copy_trail(w, t, w->trail_start, copy_state);
#endif 

  return;
}

/**********************************************************************/
 
static void worker_update_choicepoints(w, copy_state)
     worker *w;
     copy_state_t *copy_state;
{
  TAGGED *t = w->trail_top-1;
  TAGGED *new = w->heap_top;
  choicepoint *choice;
  choice = w->choice;
  
  while ((choice != NULL) && (choice >= copy_state->least_old))
    {
      while (t >= choice->trail_top)
	{
	  if (*t == 0)
	    copy_state->trailcells_deleted--;
	  t--;
	}
      choice->trail_top -= copy_state->trailcells_deleted;
      choice->global_top = new;
      choice = choice->last_choice;
    }

  while (t >= w->trail_start)
    {
      /* We do not have to count the number of deleted trailcells
       * at this point since no choice point is updated.
       */
      if (IsNUM(*t))
	{
	  *t = Tagify(new, NUM);
	}
      else if (IsValueTrail(t))
	{
	  t--;
	}
      t--;
    }

  /* compact trail */

  {
    register TAGGED *dest, *current;

#ifdef GENERATIONAL
    dest = w->trail_old_gen_top;
#else
    dest = w->trail_start;
#endif 

    for (current = dest;
	 current != w->trail_top;
	 current++)
      if (*current != 0)
	{
	  *dest = *current;
	  dest++;
	}
    w->trail_top = dest;
  }
}

#endif /* PARALLEL */
#endif /* COPY_GC  && COPY_ALI */
