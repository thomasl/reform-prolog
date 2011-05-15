/*
 * gc.c
 *
 * Johan Bevemyr.....Mon Oct 21 1991
 *                   Fri Jan  7 1994  (parallel execution)
 *
 *
 * Description:
 *
 *   This garbage collector is an implementation of the algorithm described
 * in "Carbage Collection for Prolog Based on WAM" by K. Appleby, M. Carlsson,
 * S. Haridi and D. Sahlin, Communications of the AMC, June 1988, Volume 31,
 * Number 6.
 *
 *   It has been extended to garbage collect the Parallel implementation of
 * Reform Prolog.
 *
 *
 * Addition for boxed objects:
 *
 *   Objects such as bit-strings and floating point numbers cannot be marked
 * using bits of the value field. Such objects are represented as follows:
 *
 *      ---------------------------------------------------------
 *      | ATM | Box Flag |  Object Size   | Stat Flag | GC-bits |     
 *      ---------------------------------------------------------
 *      |                   Object                              |
 *      ---------------------------------------------------------
 *      | ATM | Box Flag |  Object Size   | Stat Flag | GC-bits |
 *      ---------------------------------------------------------
 *
 *	When marking only the first word of the box is marked.
 *
 *	The main issues concerning floating point objects are:
 *
 *	1. There is no space left for gc-bits in the float.
 *	2. The float has to be double aligned (we fix this by 
 *	   moving the float into a double aligned word whenever 
 *	   we need to access it's value).
 *	3. There must be a way of differentiate between permanent
 *	   (floats refered to from the code area) and temporary floats
 *	   (floats created during execution). 
 *	
 *
 * Additions for garbage collecting the parallel machine:
 *
 *   There are two cases.
 *
 *	1. GC is performed during sequential execution.
 *
 *	2. GC is performed during parallel execution.
 *
 *	  The main differences are that during sequential execution
 *	only the sequential worker has roots to live data (apart from
 *	value trail entries on the trail of each worker).
 *
 *	  During a sequential gc interrupt the parallel workers
 *	are activated to perform three different operations using
 *	three different flaggs.
 *
 *	W_GC_MARK	Each worker marks its live data, updates its
 *                      choicepoints (none) and compacts its trail.
 *
 *	W_GC_MARK_TRAIL Each worker should mark value trail references.
 *
 *	W_GC_COMP_TRAIL Each worker should mark value trail references.
 *
 *      W_GC_SWEEP	Each worker sweeps its registers, trail, environments
 *                      and choicepoints.
 *
 *	W_GC_UP         Each worker performs the upward compacting phase.
 *
 *	W_GC_DOWN       Each worker performs the downward compacting phase.
 *
 *	W_GC_FINISH       Each worker restore its state.
 *
 *	  A potential problem is that there may be references from
 *	the local stack of one worker into the heap of another 
 *	worker. Such references cannot be put into reallocation
 *	chains in the usual way since the local stack is local to
 *	each worker. Possible solutions:
 *
 *	1. The local stacks are made public. 
 *
 *	2. Heap references are moved into a global area during GC
 *	   and then moved back.
 *
 *	3. All external references are kept on a special stack a la
 *         Crammonds algorithm(*). (This is unelegant and unefficient
 *         to implement.)
 *
 *	We chose the first apporach in this implementation.
 *
 *	  Copying GC does not have this problem. The problem with
 *	copying GC is that segmented memory is required, but segmented
 *	memory is desired for mark-sweep GC as well.
 *
 *
 *	(*) Crammond, J. A Garbage Collection Algorithm for Shared
 *	Memory Parallel Processors, Technical Report (?), Dept. of
 *	Computing, Imperial College, London.
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
#include "think.h"

#if ! defined(COPY_GC) && ! defined(COPY_GC_GEN)

static void marking_phase PROTO((worker *));
static void mark_registers PROTO((worker *));
static void mark_all_environments PROTO((worker *));
static void mark_environments PROTO((environment *, worker *, int));
static void mark_choicepoints PROTO((worker *));
static void mark_variable PROTO((TAGGED *));
static void update_choicepoints PROTO((worker *));
static void compact_trail PROTO((worker *));

static void compacting_phase PROTO((worker *));

static void sweep_registers PROTO((worker *));
static void sweep_trail PROTO((worker *));
static void sweep_all_environments PROTO((worker *));
static void sweep_environments PROTO((environment *, worker *, int));
static void sweep_choicepoints PROTO((worker *));

static void compact_heap PROTO((worker *));
static void intoReallocChain PROTO((TAGGED *, TAGGED *));
static void updateReallocChain PROTO((TAGGED *, TAGGED *));

static void compact_heap_up PROTO((worker *));
static void compact_heap_down PROTO((worker *));
static s32 calculate_total_marked PROTO((worker *));
static TAGGED *mark_trail PROTO((worker*, TAGGED *, TAGGED *));

int total_marked;
int trailcells_deleted;
u32 wake_count;

void garbage_collect(w)
    worker *w;                 			/* sequential worker       */
{
  char *heap_gc_start;
  int gc_start_time;
  double gc_start_walltime;
  u32 worker_gc_bytes_before, worker_gc_bytes_after;

    
  /**********************************************************************
   * 
   * pre statistics 
   *
   */
  gc_start_time = usertime();
  gc_start_walltime = walltime();

  heap_gc_start = (char *) w->heap_top;
  total_marked = 0;
  trailcells_deleted = 0;
  wake_count = w->wake_count;
  

  if (w->global->flags.gc_verbose == TRUE)
    {
#if defined(PARALLEL)
      register int i;
      
      worker_gc_bytes_before = 0;

      for (i = 1; i <= w->global->active_workers; i++)
	worker_gc_bytes_before += w[i].stats->heap_gc_bytes;
#endif /* PARALLEL */

      fprintf(stderr, "{verbose: Garbage Collection...}\n");
    }
  
  /**********************************************************************/

  marking_phase(w);
  
  compacting_phase(w);

    

#if defined(UNBOUND) || defined(TIMESTAMP)
#else
  w->uncond = w->choice->global_top;
#endif /* UNBOUND */

  /**********************************************************************
   * 
   * post statistics 
   *
   */
  w->stats->heap_gc_nr++;
  w->stats->heap_gc_bytes += (int) (heap_gc_start - (char *) w->heap_top);
  w->stats->user_gcwalltime += walltime() - gc_start_walltime;
  w->stats->heap_gc_time += usertime() - gc_start_time; 

  if (w->global->flags.gc_verbose == TRUE)
    {
#if defined(PARALLEL)
      register int i;
      
      worker_gc_bytes_after = 0;

      for (i = 1; i <= w->global->active_workers; i++)
	worker_gc_bytes_after += w[i].stats->heap_gc_bytes;
      
      fprintf(stderr, "{verbose: Garbage Collection...worker reclaimed %lu bytes}\n", 
	      ((u32) worker_gc_bytes_after) -	
	      ((u32) worker_gc_bytes_before));
#endif /* PARALLEL */
      fprintf(stderr, "{verbose: Garbage Collection...reclaimed %lu bytes}\n",
	      ((u32) heap_gc_start) -
	      ((u32) w->heap_top));
    }

  /**********************************************************************
   * 
   * heap margin check:
   *
   */
  if (w->heap_top > w->heap_margin)
    {
      FatalError("Prolog out of memory");
    }

  return;
}

/************************************************************************
 * Mark live data using a version of the Deutsch-Schorr-Waite algorithm *
 * Schorr, H. and Waite, W. "An efficient machine-independent procedure *
 * for garbage collection in various list structures" Commun. ACM, 10(8)*
 * 501-506, Aug. 1967.                                                  *
 ***********************************************************************/

static void marking_phase(w)
    worker *w;
{
  int gc_mark_time = usertime();

  mark_registers(w);
  mark_all_environments(w);

  /* This should be done before update_choicepoints is performed
   * since all choice point on the parallel workers must be more
   * recent than any choice point in the sequential worker.
   * (Choice points can only exist on the stacks of parallel
   * workers if a parallel execution is currently in progress.)
   */

  GC_Run_Parallel(W_GC_MARK);

  mark_choicepoints(w);

  update_choicepoints(w);

  compact_trail(w);

  /*   The order in which trails are compacted is not significant but it has
   * to be done after update_choicepoint since update_choicepoint may invoke
   * mark_trail on the parallel workers.
   */

  GC_Run_Parallel(W_GC_COMP_TRAIL);

  w->stats->heap_gc_mark_time += usertime() - gc_mark_time;
}

static void mark_registers(w)
    worker *w;
{
  register size_t arity = w->gc_info.arity;
  register TAGGED *areg = w->regs;
  
  while (arity--)
    {
      if (IsHeapTerm(X(arity)) || IsHeapBox(X(arity)))
	{
	  *w->trail_top = X(arity);
	  mark_variable(w->trail_top);
	}
    }
}

static void mark_all_environments(w)
     worker *w;
{
  mark_environments(w->frame, w, w->gc_info.env_size);
}

static void mark_environments(frame,w,size)
    register environment *frame;
    worker *w;
    register int size;
{
  while (frame != NULL)
    {
      while (size--)
	{
	  register TAGGED term = Yf(size);

	  switch (TagOf(Yf(size))) {

	  case HVA:
#if defined(CONSTR)
	  case CVA:
#endif /* CONSTR */
	    {
	      if (IsMarked(term))
		return;		/* This environment has already been marked. */
	      else
		mark_variable(&(Yf(size)));
	    }
	    break;

	  case SVA:
	    break;

	  case NUM:
	  case ATM:
	    break;

	  case BOX:
	  case GEN:
	    {
	      if (IsMarked(term))
		return;		/* This environment has already been marked. */
	      else
		mark_variable(&(Yf(size)));
	    }
	    break;


	  case LST:
	  case STR:
	    {
	      if (IsMarked(term))
		return;		/* This environment has already been marked. */
	      else
		mark_variable(&(Yf(size)));
	    }
	    break;

#if defined(UNBOUND)
	    case UVA:
	      break;
#endif /* UNBOUND */

	  default:
	    Error("mark_environment -- TagOf out of range");
	  }
	}
      size = FrameSize(frame->next_instr);
      frame = frame->cont_env;
    }
}


static void mark_choicepoints(w)
    register worker *w;
{
  register int i;
  register TAGGED *areg, *t = w->trail_top-1;
  register choicepoint *choice = w->choice;
  
  trailcells_deleted = 0;
  
  while (choice != NULL)
    {
      t = mark_trail(w,t,choice->trail_top);

      mark_environments(choice->cont_env,w,FrameSize(choice->next_instr));
      i = choice->arity;
      areg = choice->areg;
      while (i--)
	{
	  if (IsHeapTerm(X(i)) || IsHeapBox(X(i)))
	    {
	      mark_variable(&(X(i)));
	    }
	}
      choice = choice->last_choice;
    }

  w->gc_info.trail = t;
}


static TAGGED *mark_trail(w,t,stop)
     worker *w;
     register TAGGED *t, *stop;
{
  while (t >= stop)
    {

#if defined(PARALLEL)
      if (IsSTR(*t)) /* parallel execution has occured */
	{
	  GC_Run_Parallel(W_GC_MARK_TRAIL);
	}
      else 
#endif /* PARALLEL */

	if ((IsValueTrail(t) || IsHVAorCVA(*t)) &&
	    !IsMarked(*TagToPointer(*t)))
	  {
	    if (IsValueTrail(t))	/* Value trail entry */
	      {  
		Reset(*t);
		*t-- = 0;
		*t = 0;
		trailcells_deleted += 2;
	      }
	    else if (IsHVA(*t))
	      {
		Reset(*t);
		*t = 0;
		trailcells_deleted++;
#if defined(UNBOUND)
		trailcells_deleted++;
		*t--;
		*t = 0;
#endif /* UNBOUND */
	      }
#if defined(CONSTR)
	    else if (IsCVA(*t))
	      {
		if (wake_count > 1)
		  wake_count--;
		else
		  {
		    Reset(*t);
		    *t = 0;
		    trailcells_deleted++;
#if defined(UNBOUND)
		    trailcells_deleted++;
		    *t--;
		    *t = 0;
#endif /* UNBOUND */
		  }
	      }
#endif /* CONSTR */
	  }
	else
	  if (IsValueTrail(t))	/* Value trail entry */
	    {  
	      t--;
	      mark_variable(t);
	    }
#if defined(UNBOUND)
	  else if (IsHVA(*t))
	    {
	      t--;
	    }
#endif /* UNBOUND */
#if defined(CONSTR)
	  else if (IsCVA(*t))
	    {
	      wake_count--;
#if defined(UNBOUND)
	      t--;
#endif /* UNBOUND */
	    }
#endif /* CONSTR */
      t--;
    }
  return t;
}

static void mark_variable(start)
    TAGGED *start;
{
  register TAGGED *current, *next;

  current = start;
  next = TagToPointer(*current);

  MarkF(*current);

  goto first_forward;

 forward:
  {
    if (IsMarked(*current)) goto backward;

    total_marked++;
  }

 first_forward:
  {  
    Mark(*current);

    switch(TagOf(*current)) {

    case HVA:
      {
	if (IsForM(*next))
	  goto backward;
	Reverse(current,next);
      }
      goto forward;

#if defined(CONSTR)
    case CVA:
      {
	if ((IsMarkedF(*(next+VARSIZE))) ||
	    (IsMarked(*next) && IsMarked(*(next+VARSIZE))))
	  goto backward;

	next += VARSIZE;
	MarkF(*(next));
      
	Reverse(current, next);
      }
      goto forward;

#endif /* CONSTR */

    case SVA:
      {
	FatalError("mark_variable -- found stack variable in heap");
      }
      break;

    case NUM:
    case ATM:
      goto backward;

    case BOX:
      { 
	if (IsDynBox(*next))
	  /* mark box */
	  if (!IsMarked(*next))
	    {
	      Mark(*next);
	      total_marked += GetBoxSize(*next);
	    }
      }
      goto backward;

    case GEN:
      {
	FatalError("mark_variable -- found unsupported tag (GEN)");
      }
      goto backward;

    case LST:
      {
	if ((IsMarkedF(*(next+VARSIZE))) ||
	    (IsMarked(*next) && IsMarked(*(next+VARSIZE))))
	  goto backward;

	next += VARSIZE;
	MarkF(*(next));
      
	Reverse(current, next);
      }
      goto forward;
    
    case STR:
      { 
	register int arity;

	if (IsMarked(*next) || IsMarkedF(*(next+VARSIZE)))
	  goto backward;

	/* To take care of the functor */
	Mark(*next);
	total_marked++;

	arity = StructArity(next);

	if (arity == 0)
	  {
	    Error("mark_variable: Structure arity is zero");
	    return;
	  }

	while (arity--)
	  {
	    next += VARSIZE;
	    MarkF(*(next));
	  }

	Reverse(current, next);
      }
      goto forward;
    
#if defined(UNBOUND)
    case UVA:
      goto backward;

#endif /* UNBOUND */

    default:
      {
	FatalError("mark_variable -- found illegal tag");
      }
    }
  }

 backward:
  {
    if (!IsMarkedF(*current))
      {
	Undo(current,next);
	goto backward;
      }
    else
      {
	UnMarkF(*current);

	if (current == start) return;

	Advance(current, next);

	goto forward;
      }
  }
}

  
static void update_choicepoints(w)
    worker *w;
{
  register choicepoint *choice = w->choice;
  TAGGED *t = w->trail_top-1;
  
  while (choice != NULL)
    {
      while (t >= choice->trail_top)
	{
	  if (*t == 0)
	    trailcells_deleted--;
	  t--;
	}
      choice->trail_top -= trailcells_deleted;
      choice = choice->last_choice;
    }
}


static void compact_trail(w)
    worker *w;
{
  register TAGGED *current;
  register TAGGED *target = w->trail_start;

  for (current = target; current != w->trail_top; current++)
    if (*current != 0)
      {
	*target++ = *current;
      }
  w->trail_top = target;
}


/**********************************************************************     
 * The Compaction Phase                                               *
 * This code implements a version of Morris' algorithm.               *
 * Morris, F.L. "A Time- and Space- Efficient Garbage Compaction      *
 * Algorithm", Commun. ACM, 21(8) 662-665, Aug. 1978.                 *
 **********************************************************************/

static void compacting_phase(w)
    worker *w;
{
  int gc_copy_time = usertime();

  sweep_registers(w);
  sweep_trail(w);
  sweep_all_environments(w);
  sweep_choicepoints(w);

  GC_Run_Parallel(W_GC_SWEEP);
  
  compact_heap(w);
  
  w->stats->heap_gc_copy_time += usertime() - gc_copy_time;
}


static void sweep_registers(w)
    worker *w;
{
  register size_t arity = w->gc_info.arity;
  register TAGGED *areg = w->regs;
    
  while (arity--)
    {
      if (IsHeapTerm(X(arity)) || IsHeapBoxM(X(arity)))
	{
	  intoReallocChain(TagToPointer(X(arity)),&(X(arity)));
	}
    }
}

static void sweep_trail(w)
    worker *w;
{
  register TAGGED *current;

  for (current = w->trail_top-1; current >= w->trail_start; current--)
    {
      switch(TagOf(*current)) {

      case HVA:
#if defined(CONSTR)
      case CVA:
#endif /* CONSTR */
	{
	  intoReallocChain(TagToPointer(*current),current);
#if defined(UNBOUND)
	  current--;
#endif /* UNBOUND */
	}
	break;

      case SVA:
	break;

      case NUM:   /* Saved heap top on parallel workers */
	{
	  if (!IsMarked(*TagToPointer(*current)))
	    {
	      *TagToPointer(*current) = atom_nil;
	      Mark(*TagToPointer(*current));
	      if (w->heap_top == TagToPointer(*current))
		{
		  w->heap_top += VARSIZE;
		}
	      total_marked++;
	    }
	  intoReallocChain(TagToPointer(*current),current);
	}
	break;

      case GEN: /* unsupported (trapped in mark_variable) */
	break;

      case LST:   /* Value Trail */
	{
	  intoReallocChain(TagToPointer(*current),current);
	  current--;
	  if (IsHeapTerm(*current) || IsHeapBoxM(*current))
	    {
	      intoReallocChain(TagToPointer(*current),current);
	    }
	}
	break;

      case STR:
	break;

      default:
	break;
      }
    }
}


static void sweep_all_environments(w)
     worker *w;
{
  sweep_environments(w->frame, w, w->gc_info.env_size);
  return;
}

static void sweep_environments(frame, w, size)
    register environment *frame;
    worker *w;
    register int size;
{
  while (frame != NULL)
    {
      while (size--)
	{
	  if (IsHeapTerm(Yf(size)))
	    {
	      if (!IsMarked(Yf(size)))
		return;
	      else
		{
		  UnMark(Yf(size));
		  intoReallocChain(TagToPointer(Yf(size)),&(Yf(size)));
		}
	    }
	  else if (IsHeapBoxM(Yf(size)))
	    {
	      UnMark(Yf(size));
	      intoReallocChain(TagToPointer(Yf(size)),&(Yf(size)));
	    }
	}
      size = FrameSize(frame->next_instr);
      frame = frame->cont_env;
    }
}


static void sweep_choicepoints(w)
    worker *w;
{
  register choicepoint *choice = w->choice;
  int arity;

  while (choice != NULL)
    {
      sweep_environments(choice->cont_env,w,FrameSize(choice->next_instr));
      arity = choice->arity;

      while (arity--)
	{
	  UnMark(Xc(arity));
	  if (IsHeapTerm(Xc(arity)) || IsHeapBoxM(Xc(arity)))
	    {
	      intoReallocChain(TagToPointer(Xc(arity)),&(Xc(arity)));
	    }
	}

      if (!IsMarked(*(choice->global_top)))
	{
	  *(choice->global_top) = atom_nil;
	  Mark(*(choice->global_top));
	  if (w->heap_top == choice->global_top)
	    {
	      w->heap_top += VARSIZE;
	    }
	  total_marked++;
	}

      intoReallocChain(choice->global_top,((TAGGED *) &(choice->global_top)));
      choice = choice->last_choice;
    }
}


static void compact_heap(w)
    worker *w;
{
  total_marked = calculate_total_marked(w);

  GC_Run_Parallel(W_GC_UP);

  compact_heap_up(w);

  compact_heap_down(w);

  GC_Run_Parallel(W_GC_DOWN);
}


static void compact_heap_up(w)
     worker *w;
{
  register TAGGED *current = w->heap_top - VARSIZE; 
  register TAGGED *target = w->heap_start + (total_marked - 1) * VARSIZE;

  /* the upward phase */

  while (current >= w->heap_start)
    {
      if (IsMarked(*current))
	{
	  updateReallocChain(current, target);

	  if (IsHeapTerm(*current) || IsHeapBoxM(*current))
	    {
	      if (TagToPointer(*current) < current)
		{
		  intoReallocChain(TagToPointer(*current), current);
		}
	      else
		{
		  if (current == TagToPointer(*current))
		    *current = PutValue(target, *current);
		}
	    }
	  current -= VARSIZE;
	  target -= VARSIZE;
	}
      else if (IsBox(*current)) /* Boxed object */
	{
	  register s32 size = GetBoxSize(*current);
	  current -= size*VARSIZE;

	  if (IsMarked(*(current+VARSIZE)))
	    {
	      target -= size*VARSIZE;
	      updateReallocChain(current+VARSIZE, target+VARSIZE);
	    }
	}
      else
	current -= VARSIZE;
    }
  assert(target+VARSIZE == w->heap_start); /* safety test */
}


static void compact_heap_down(w)
     worker *w;
{
  register TAGGED *current = w->heap_start;
  register TAGGED *target = w->heap_start;

  /* the downward phase */

  while (current < w->heap_top)
    {
      if (IsMarked(*current))
	{
	  updateReallocChain(current, target);
	  if ((IsHeapTerm(*current) || IsHeapBoxM(*current)) &&
	      TagToPointer(*current) > current)
	    {
	      intoReallocChain(TagToPointer(*current), target);
	      *target = SetTag(*target, TagOf(*current));
	      UnMark(*target);             /* this may be uneccessary */
	      target += VARSIZE;
	      current += VARSIZE;
	    }
	  else if (IsBox(*current))
	    {
	      register s32 i, size = GetBoxSize(*current);

	      for (i = 0; i < size*VARSIZE; i++)
		{
		  target[i] = current[i];
		}
	      UnMark(*target);
	      target += size*VARSIZE;
	      current += size*VARSIZE;
	    }
	  else
	    {
	      *target = *current;
	      UnMarkF(*target);
	      UnMark(*target);
	      target += VARSIZE;
	      current += VARSIZE;
	    }
	}
      else if (IsBox(*current)) /* Boxed object */
	{
	  current += GetBoxSize(*current)*VARSIZE;
	}
      else
	{
	  current += VARSIZE;
	}
    }
  w->heap_top = target;
}


static void intoReallocChain(J,C)
    TAGGED *J,*C;
{
  *(C) = PutValueFirst(*(J),*(C));
  *(J) = PutValueFirst(((uword)(C) | GC_FWD_MASK),*(J));
}

static void updateReallocChain(current, target)
     TAGGED *current;
     TAGGED *target;
{
  register TAGGED *j;

  while (IsMarkedF(*current))
    {
      j = TagToPointer(*current);
      *current = PutValueFirst(*j, *current);
      *j = PutValue(target, *j);
      UnMarkF(*j);
    }
}


static s32 calculate_total_marked(w)
     worker *w;
{

#if defined(PARALLEL)
  /*   When garbage collecting several workers we have to calculate
   * 'total_marked' separately for each heap. This is due to the fact
   * that a variable chain might span over a number of heaps.
   * Thus, the mark_variable procedure might have marked variables in
   * other heaps
   */
  register TAGGED *current = w->heap_start;

  total_marked = 0;

  /* calculate 'total_marked' */

  while (current < w->heap_top)
    {
      if (IsMarked(*current))
	{
	  if (IsBox(*current))
	    {
	      register int size = GetBoxSize(*current);
	      total_marked += size;
	      current += size;
	    }
	  else
	    {
	      total_marked++;
	      current += VARSIZE;
	    }
	}
      else if (IsBox(*current))
	{
	  current += GetBoxSize(*current) * VARSIZE;
	}
      else
	{
	  current += VARSIZE;
	}
    }
#endif /* PARALLEL */

  return total_marked;
}


/**********************************************************************
 **********************************************************************
 *                                                                    *
 * Support for parallel mark-sweep GC                                 *
 * These extensions are all ours :-)                                  *
 *                                                                    *
 **********************************************************************
 **********************************************************************/

#if defined(PARALLEL)

static void worker_finish PROTO((worker *));
static void worker_mark_trail PROTO((worker *));

BOOL worker_garbage_collect(w,is_done)
     worker *w;
     BOOL is_done;
{
  char *heap_gc_start;
  int gc_start_time;
  double gc_start_walltime;

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

  wake_count = w->wake_count;

  if (is_done) goto if_done;

 wait_for_work:
  {
    w->gc_info.done[w->pid-1] = TRUE;
    DropDone(1,w);
  }

 wait_first_time:
  {
    SpinGrabPrivateSema(w);
  }

 if_done:
  {
    /* Decode command from sequential worker */

    switch (w->global->parallel_start.type) {

    case W_GC_MARK:
      {
	GC_Sequentialize_Low_To_High;
	mark_registers(w);
	mark_all_environments(w);
	mark_choicepoints(w);
	update_choicepoints(w);
      }
      goto wait_for_work;

    case W_GC_MARK_TRAIL:
      {
	GC_Sequentialize_Low_To_High;
	worker_mark_trail(w);
      }
      goto wait_for_work;

    case W_GC_COMP_TRAIL:
      {
	compact_trail(w);
      }
      goto wait_for_work;

    case W_GC_SWEEP:
      {
	GC_Sequentialize_Low_To_High;
	sweep_registers(w);
	sweep_trail(w);
	sweep_all_environments(w);
	sweep_choicepoints(w);
      }
      goto wait_for_work;

    case W_GC_UP:
      {
	GC_Sequentialize_High_To_Low;
	total_marked = calculate_total_marked(w);
	compact_heap_up(w);
      }
      goto wait_for_work;

    case W_GC_DOWN:
      {
	GC_Sequentialize_Low_To_High;
	compact_heap_down(w);

	/* post statistics */
	w->stats->heap_gc_nr++;
	w->stats->heap_gc_bytes += (((u32)heap_gc_start) - ((u32)w->heap_top));
	w->stats->user_gcwalltime += walltime() - gc_start_walltime;
	w->stats->heap_gc_time += usertime() - gc_start_time;
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
    if (w->heap_top > w->heap_margin)
      {
	Error("Worker out of memory");
	return FALSE;
      }
    return TRUE;
  }
}


static void worker_finish(w)
     worker *w;
{
#if ! defined(UNBOUND) && ! defined(TIMESTAMP)
  w->uncond = w->choice->global_top;
#endif
}


static void worker_mark_trail(w)
     worker *w;
{
  register TAGGED *t = w->gc_info.trail;
  register TAGGED *stop = w->trail_start;

  while (t >= stop)
    {
      if (!IsMarked(*TagToPointer(*t)))
	{
	  if (IsValueTrail(t))	/* Value trail entry */
	    {  
	      Reset(*t);
	      *t-- = 0;
	      *t = 0;
	      trailcells_deleted += 2;
	    }
	  else if (IsHVA(*t))
	    {
	      Reset(*t);
	      *t = 0;
	      trailcells_deleted++;
#if defined(UNBOUND)
	      trailcells_deleted++;
	      *t--;
	      *t = 0;
#endif /* UNBOUND */
	    }
	}
      else
	if (IsValueTrail(t))	/* Value trail entry */
	  {  
	    t--;
	    mark_variable(t);
	  }
#if defined(UNBOUND)
	else if (IsHVA(*t))
	  {
	    t--;
	  }
#endif /* UNBOUND */
      t--;
    }

  w->gc_info.trail = t;

  return;
}

#endif /* PARALLEL */

#endif /* COPY_GC */

