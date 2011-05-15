/*    -*- C -*- 
 *    File:	 new-copy-gc.c  (~jb/Reform/Luther/Emulator/new-copy-gc.c)
 *    Author:	 Johan Bevemyr
 *    Created:	 Thu Nov 16 17:13:30 1995
 *    Purpose:   
 */ 

#include "luther.h"
#include "engine.h"
#include "think.h"
#include "unify.h"
#include "new-copy-gc.h"


#if defined(COPY_GC) && defined(COPY_ALI)

#define DoAll(CODE)							   \
if(SeqWorker(w)) {							   \
  int i;								   \
  for(i=0 ; i <= w->global->active_workers ; i++)			   \
    {									   \
      CODE;								   \
    }									   \
}

void gc_PushChoicePoint PROTO((worker *));
void gc_PopChoicePoint PROTO((worker *));
void gc_tidy_trail PROTO((worker *));
void gc_mark  PROTO((worker *));
void gc_copy  PROTO((worker *)); 
void gc_update  PROTO((worker *)); 
void gc_clean  PROTO((worker *)); 
void gc_return_new_gen_memory PROTO((worker *));

TAGGED *gc_tidy_trail_seg PROTO((worker *, choicepoint *, TAGGED *,
				 TAGGED *));

void    gc_mark_choicepoints PROTO((worker *));
void    gc_mark_compare_area PROTO((worker *));
void    gc_mark_environments PROTO((worker *, environment *, int));
TAGGED *gc_mark_trail PROTO((worker *, TAGGED *, TAGGED *));
void    gc_mark_variable PROTO((worker *, TAGGED *));
void    gc_mark_var PROTO((worker *, int, TAGGED *));

void    gc_copy_choicepoints PROTO((worker *));
void    gc_copy_compare_area PROTO((worker *));
void    gc_copy_environments PROTO((worker *, environment *, int));
TAGGED *gc_copy_trail PROTO((worker *, TAGGED *, TAGGED *, s32));
static TAGGED  gc_copy_variable PROTO((worker *, TAGGED));

static void scan(TAGGED *, TAGGED *, TAGGED, worker *);


/**********************************************************************
 *
 * Public primitives to support mark and collect phases in generic objects.
 *
 */
void live_mark_variable(slot, w)
     TAGGED* slot;
     worker* w;
{
/*
  if (IsHeapTerm(*slot) || IsHeapBox(*slot) || IsGEN(*slot))
    {
      gc_mark_variable(w,slot);
    }
 */
}

TAGGED collect_variable(term, w)
     TAGGED  term;
     worker* w;
{
/*
  if (IsHeapTerm(term) || IsHeapBox(term) || IsGEN(term))
    {
      return gc_copy_variable(w,term);
    }
  else
    {
      return term;
    }
 */
}

/**********************************************************************
 *
 * New implementation of generational gc. Fully symmetric.
 *
 */

#if 0
void garbage_collect(w)
     worker *w;
{
  integer gc_start_time = usertime();
  long initial_size, final_size;
  double gc_start_walltime = walltime();

  int i;

  /* Let the sequential worker care about statistics */

  InitBarrier(w);
  InitMarkBuffer(w);

  /* to make sure that the end is properly marked */
  PushOnHeap(w->heap_top, (TAGGED)NULL);

  if (SeqWorker(w))
    {
      /* Start all sleeping workers */
      Start_Parallel(W_GC_MARK);

      if (w->global->flags.gc_verbose == TRUE)
	{
	  fprintf(stderr, "{verbose: Garbage Collection...}\n");
	}

      initial_size = used_segment_size_all(w);
    }

  /* Create a choice point containing pointers to the roots found
   * in registers, environments, and choice points
   */

  gc_PushChoicePoint(w);

  /************************************************************
   * First, remove all redundant SVA trail entries created after
   * the last GC. These may point to deallocated data and must
   * be removed.
   */

  gc_tidy_trail(w);

  /************************************************************
   * Second, mark all live data.
   */
  w->stats->heap_gc_pre_time += walltime()-gc_start_walltime;

  w->gc_info.load->markdone = FALSE;
  w->gc_info.load->copydone = FALSE;
  
  BarrierSynch(w,0); /***** no marking until all in gc *******/

  DoAll(
        {
	  double start = walltime();

	  gc_mark(&(w[i]));
	  SearchMarkWork(&(w[i]));

	  w[i].stats->heap_gc_mark_time += walltime()-start;

	  gc_return_new_gen_memory(&(w[i]));
	});

  DoAll(
        {
	  double start = walltime();


	  gc_copy(&(w[i]));
	  SearchCopyWork(&(w[i]));

	  w[i].stats->heap_gc_copy_time += walltime()-start;

	  w[i].stats->heap_gc_post_time -= walltime();

	  gc_update(&(w[i]));
	  w[i].stats->heap_gc_post_time += walltime();
	});


  BarrierSynch(w,1); /***** no check size until all copied dealloced *****/

  /**********************************************************************
   **********************************************************************
   *
   * Check if global GC is needed
   *
   */
  if (w->global->used_seg+2*new_gen_size >= w->global->free_seg)
    {
      segment *used_segs;
      
      {
	choicepoint *ch;
	
	for(ch = w->choice ; ch != NULL ; ch = ch->last_choice)
	  ch->generation = NEW_GEN;
	
      }

      if (SeqWorker(w) && w->global->flags.gc_verbose == TRUE)
	{
	  fprintf(stderr, "{verbose: global collection}\n");
	}

      /* to make sure that the end is properly marked */
      PushOnHeap(w->old_heap_top, (TAGGED)NULL);

      w->gc_info.load->markdone = FALSE;
      w->gc_info.load->copydone = FALSE;

      used_segs = w->used_segments;
      w->used_segments = NULL;
      (void) new_segment(w);
      
      w->trail_old_gen_top = w->trail_start;
      w->global->new_gen_limit = (TAGGED *) w->global->globalstart;

      gc_tidy_trail(w);

      /**********************************************************************/
      BarrierSynch(w,0); /***** no marking until all in gc *******/
      /* fprintf(stderr,"%d marking\n",w->pid); */
      {
	double start = walltime();

	gc_mark(w);
	SearchMarkWork(w);

	w->stats->heap_gc_mark_time += walltime()-start;
      }
      /* fprintf(stderr,"%d marking done\n",w->pid); */

      /**********************************************************************/
      BarrierSynch(w,1); /***** no copying until all marked ******/
      /* fprintf(stderr,"%d copying\n",w->pid); */
      {
	double start = walltime();

	gc_copy(w);
	SearchCopyWork(w);
	w->stats->heap_gc_copy_time += walltime()-start;
      }
      /* fprintf(stderr,"%d copying done\n",w->pid); */

      BarrierSynch(w,2); /***** no check size until all dealloced ******/

      w->stats->heap_gc_post_time -= walltime();

      free_seg_list(w, used_segs);

      gc_update(w);

      BarrierSynch(w,2); /***** no check size until all dealloced ******/

      w->global->new_gen_limit = w->global->new_gen_limit_orig;

      if (SeqWorker(w))
	{
	  if (1.3*(w->global->used_seg+2*new_gen_size) > w->global->free_seg)
	    {
	      extend_heap(w);
	    }
	}
      w->stats->heap_gc_post_time += walltime();
    }

  w->stats->heap_gc_post_time -= walltime();

  gc_PopChoicePoint(w);

  gc_clean(w);

  w->stats->heap_gc_post_time += walltime();

  if (SeqWorker(w))
    {
      final_size = used_segment_size_all(w);

      if (w->global->flags.gc_verbose == TRUE)
	{
	  fprintf(stderr, "{verbose: reclaimed %ld bytes}\n",
		  initial_size-final_size);
	}

      w->stats->heap_gc_nr++;
      w->stats->heap_gc_bytes += initial_size - final_size;
    }

  w->stats->heap_gc_walltime += walltime() - gc_start_walltime;
  w->stats->heap_gc_time += usertime() - gc_start_time;

  return;
}

#else

void garbage_collect(w)
     worker *w;
{
  integer gc_start_time = usertime();
  long initial_size, final_size;
  double gc_start_walltime = walltime();

  int i;

  /* Let the sequential worker care about statistics */

  InitBarrier(w);

#if defined(SINGLE_LOAD_STACK)
  if (SeqWorker(w))
#endif
    InitMarkBuffer(w);

  /* to make sure that the end is properly marked */
  PushOnHeap(w->heap_top, (TAGGED)NULL);

  if (SeqWorker(w))
    {
      /* Start all sleeping workers */
      Start_Parallel(W_GC_MARK);

      if (w->global->flags.gc_verbose == TRUE)
	{
	  fprintf(stderr, "{verbose: Garbage Collection...}\n");
	}

      initial_size = used_segment_size_all(w);
    }

  /* Create a choice point containing pointers to the roots found
   * in registers, environments, and choice points
   */

  gc_PushChoicePoint(w);

  /************************************************************
   * First, remove all redundant SVA trail entries created after
   * the last GC. These may point to deallocated data and must
   * be removed.
   */

  gc_tidy_trail(w);

  /************************************************************
   * Second, mark all live data.
   */
  w->stats->heap_gc_pre_time += walltime()-gc_start_walltime;

  w->gc_info.load->markdone = FALSE;
  w->gc_info.load->copydone = FALSE;
  
  BarrierSynch(w,0); /***** no marking until all in gc *******/
  {
    double start = walltime();

    gc_mark(w);
    SearchMarkWork(w);

    w->stats->heap_gc_mark_time += walltime()-start;
  }

  gc_return_new_gen_memory(w);

  BarrierSynch(w,1); /***** no copying until all marked ******/
  {
    double start = walltime();

    gc_copy(w);
    SearchCopyWork(w);
    /* fprintf(stderr,"%d done copying at %f\n",w->pid,walltime()); */

    w->stats->heap_gc_copy_time += walltime()-start;
  }

  w->stats->heap_gc_post_time -= walltime();
  gc_update(w);
  w->stats->heap_gc_post_time += walltime();

  BarrierSynch(w,2); /***** no check size until all copied dealloced *****/


  /**********************************************************************
   **********************************************************************
   *
   * Check if global GC is needed
   *
   */
  if (w->global->used_seg+2*new_gen_size >= w->global->free_seg)
    {
      segment *used_segs;
      
      {
	choicepoint *ch;
	
	for(ch = w->choice ; ch != NULL ; ch = ch->last_choice)
	  ch->generation = NEW_GEN;
	
      }

      if (SeqWorker(w) && w->global->flags.gc_verbose == TRUE)
	{
	  fprintf(stderr, "{verbose: global collection}\n");
	}

      /* to make sure that the end is properly marked */
      PushOnHeap(w->old_heap_top, (TAGGED)NULL);

      w->gc_info.load->markdone = FALSE;
      w->gc_info.load->copydone = FALSE;

      used_segs = w->used_segments;
      w->used_segments = NULL;
      (void) new_segment(w);
      
      w->trail_old_gen_top = w->trail_start;
      w->global->new_gen_limit = (TAGGED *) w->global->globalstart;

      gc_tidy_trail(w);

      /**********************************************************************/
      BarrierSynch(w,0); /***** no marking until all in gc *******/
      /* fprintf(stderr,"%d marking\n",w->pid); */
      {
	double start = walltime();

	gc_mark(w);
	SearchMarkWork(w);

	w->stats->heap_gc_mark_time += walltime()-start;
      }
      /* fprintf(stderr,"%d marking done\n",w->pid); */

      /**********************************************************************/
      BarrierSynch(w,1); /***** no copying until all marked ******/
      /* fprintf(stderr,"%d copying\n",w->pid); */
      {
	double start = walltime();

	gc_copy(w);
	SearchCopyWork(w);
	w->stats->heap_gc_copy_time += walltime()-start;
      }
      /* fprintf(stderr,"%d copying done\n",w->pid); */

      BarrierSynch(w,2); /***** no check size until all dealloced ******/

      w->stats->heap_gc_post_time -= walltime();

      free_seg_list(w, used_segs);

      gc_update(w);

      BarrierSynch(w,3); /***** no check size until all dealloced ******/

      w->global->new_gen_limit = w->global->new_gen_limit_orig;

      if (SeqWorker(w))
	{
	  if (1.3*(w->global->used_seg+2*new_gen_size) > w->global->free_seg)
	    {
	      extend_heap(w);
	    }
	}
      w->stats->heap_gc_post_time += walltime();
    }

  w->stats->heap_gc_post_time -= walltime();

  gc_PopChoicePoint(w);

  gc_clean(w);

  if (SeqWorker(w))
    {
      final_size = used_segment_size_all(w);

      if (w->global->flags.gc_verbose == TRUE)
	{
	  fprintf(stderr, "{verbose: reclaimed %ld bytes}\n",
		  initial_size-final_size);
	}

      w->stats->heap_gc_nr++;
      w->stats->heap_gc_bytes += initial_size - final_size;
    }

  w->stats->heap_gc_walltime += walltime() - gc_start_walltime;
  w->stats->heap_gc_time += usertime() - gc_start_time;

  w->stats->heap_gc_post_time += walltime();

  return;

}
#endif


/**********************************************************************
 *
 * Name: gc_tidy_trail
 * 
 * Desc: Removes trail entries belonging to dead environments.
 *       Also, as a side effect, finds the youngest old choice point
 *       (the one protecting all old environments below).
 *
 */

void gc_tidy_trail(w)
     worker *w;
{
  TAGGED *trail, *gen_stop, *stop;
  choicepoint *cp;
  

  trail = w->trail_top;
  cp = w->choice;
  gen_stop = w->trail_old_gen_top;

  while((cp != NULL) && IsNewCp(cp))
    {
      stop = max(cp->trail_top, gen_stop);
      trail = gc_tidy_trail_seg(w,cp,trail,stop);
				
      cp = cp->last_choice;
    }

  /**************************************************
   * Finally, tidy the part added after the last
   * surviving choice point, but before any new
   * cp was created.
   *
   * In the parallel worker this may be trail entries
   * added by other iterations since the stack is cleared
   * after each iteration.
   */

  if (cp == NULL)
    stop = gen_stop;
  else
    stop = max(cp->trail_top, gen_stop);

  (void) gc_tidy_trail_seg(w,cp,trail,stop);
  
  w->gc_info.last_safe = cp;

  return;
}

TAGGED *gc_tidy_trail_seg(w,cp,trail,stop)
     worker *w;
     choicepoint *cp;
     TAGGED *trail, *stop;
{

  while ((trail > stop))
    {
      dec(trail);
      if (IsSVA(*trail))
	{
	  if(TagToPointer(*trail) > (TAGGED *) cp)
	    {
	      *trail = (TAGGED) 0;
	    }
	}
      else if (IsDoubleTrail(trail))
	{
	  dec(trail);
	}
    }
   return trail;
}

/**********************************************************************
 *====================================================================*
 * Marking phase                                                      *
 **********************************************************************/

void gc_mark(w)
     worker *w;
{
  gc_mark_choicepoints(w);
  gc_mark_compare_area(w);

  return;
}

void gc_mark_choicepoints(w)
     worker *w;
{
  choicepoint *choice;
  TAGGED *trailtop;

  trailtop = w->trail_top;
  choice = w->choice;

  while((choice != NULL) && IsNewCp(choice))
    {
      int arity;
      TAGGED *areg;     /* enables the X(i) notation  */

      trailtop = gc_mark_trail(w, trailtop, choice->trail_top);
      gc_mark_environments(w,choice->cont_env,FrameSize(choice->next_instr));

      arity = choice->arity;
      areg = choice->areg;

      while(arity--)
	{
	  if (IsHeapObj(X(arity)))
	    {
	      gc_mark_variable(w,&X(arity));
	      /* Is this needed ? */
	      UnMark(X(arity));
	    }
	}
      
      choice = choice->last_choice;
    }

  (void) gc_mark_trail(w, trailtop, w->trail_old_gen_top);

  return;
}

/**********************************************************************
 * Name: gc_mark_environmens
 *
 * Desc: mark all roots accessible from all unsafe environments.
 */

void gc_mark_environments(w,frame,size)
     worker *w;
     environment *frame;
     int size;
{
  BOOL visited = FALSE;

  while ((frame != NULL) && (visited == FALSE) && UnProtected(frame))
    {
      while(size--)
	{
	  if (!IsMarked(Yf(size)))
	    {
	      if(IsHeapObj(Yf(size)))
		{
		  gc_mark_variable(w,&Yf(size));
		}

	      Mark(Yf(size));
	    }
	  else
	    {
	      visited = TRUE;
	    }
	}
      size = FrameSize(frame->next_instr);
      frame = frame->cont_env;
    }

  return;
}
/**********************************************************************
 * Name: gc_mark_compare_area
 *
 * Desc: mark all roots from the compare area.
 *
 * Since compare-bindinga are not trailed crossgenerational pointers
 * are not recorded in the usual way (on the trail). Therefore we have
 * to search the compared-variable are for such pointers.
 */

void gc_mark_compare_area(w)
     register worker *w;
{
  TAGGED *current;
  segment *seg;
  
  /* Install sentinel (watch dog) */
  *w->global->compare_top = (TAGGED) NULL; 

  seg = w->global->compare_seg;

  while (seg != NULL)
    {
      current = &(seg->data[0]);

      while (*current != (TAGGED) NULL)
	{
	  /* All terms accessible from the new generation are 
	   * automatically marked if live. Ordered old generation 
	   * variables need to be marked.
	   */

	  if (InOldGen(*current) && not(IsMarked(*(current+2*VARSIZE))))
	    {
	      gc_mark_variable(w,(current+2*VARSIZE));
	    }
	  current += 3*VARSIZE;
	}
      seg = seg->next;
    }
}


/**********************************************************************
 * Name: gc_mark_trail
 * 
 * Desc: mark all roots found in the new part of the trail.
 */

TAGGED *gc_mark_trail(w, top, stop)
     worker *w;
     TAGGED *top, *stop;
{
  /* Dont look at the topmost element
   */

  top--;

  while (top >= stop)
    {
      if (*top == ((TAGGED) NULL))
	{
	  SkipEntry(top);
	}
      else if ((IsValueTrail(top) || IsHVAorCVA(*top)) &&
	       not(IsMarked(*TagToPointer(*top))) &&
	       InOldGen(*top))
	{
	  TAGGED *old_gen_term;

	  old_gen_term = TagToPointer(*top);

	  if ((IsHeapTerm(*old_gen_term) || IsHeapBox(*old_gen_term)) &&
	      InNewGen(*old_gen_term))
	    {
	      *w->trail_top = *old_gen_term;
	      gc_mark_variable(w,w->trail_top);
	    }
	  
	  if (IsValueTrail(top))
	    {
	      if ((IsHeapTerm(*(top-1)) || IsHeapBox(*(top-1))) &&
		  InNewGen(*(top-1)))
		{
		  *w->trail_top = *(top-1);
		  gc_mark_variable(w,w->trail_top);
		}
	      SkipValueCell(top);
	    }
	  else
	    {
	      SkipHVA(top);
	    }
	}
      else if (IsValueTrail(top))	/* Value trail entry */
	{  
	  gc_mark_variable(w,top-1);
	  SkipValueCell(top);	
	}
      else if (IsHVAorCVA(*top))
	{
	  SkipHVA(top);
	}
      else if (IsSVA(*top))
	{
	  TAGGED *sva_pos;

	  sva_pos = TagToPointer(*top);

	  /* All unprotected environments are scanned later and their 
	   * potential references into the new generation are
	   * updated then. At this point we want to deal with
	   * changes in protected environments. 
	   */
	  /* 
	   * My guess is that it does not hurt to always mark...
	   * 
	   *  if (ProtectedSVA(sva_pos)) 
	   *  {
	   *
	   */

	  if ((IsHeapTerm(*sva_pos) || IsHeapBox(*sva_pos)) &&
	      InNewGen(*sva_pos))
	    {
	      *w->trail_top = *sva_pos;
	      gc_mark_variable(w,w->trail_top);
	    }

	  /* 
	   * }
	   */

	  SkipSVA(top);
	}
      else if (IsNewSegmentMarker(*top))
	{
	  ResetEntry(top);
	  SkipEntry(top);
	}
      else if (TRUE)
	{
	  /* Worker and sequential ParCall */
	  SkipEntry(top);
	}
    }

  top += 1;

  return top;
}

/**********************************************************************
 *
 *   Instead of using pointer reversal for traversing the structure to mark we
 * use an open coded recursive procedure in which the stack is temporarily
 * stored in the empty area (the area we are going to copy the live data 
 * into).
 *
 * Note:  All internal structure cells, except the first in each structrue,
 *        should be marked with F after this pass. All live cells are marked
 *        with M. No data is marked with F but not M. This combination is
 *        used to denote forward pointers.
 *
 * Algorithm:
 *
 *   Cells are marked with M as they are passed through. This is done in order
 * to detect circular references. 
 *
 *   The variable `remaining_args' is used for detecting whether the
 * current cell is internal to a structure. When backing up, this
 * knowledge is needed for deciding whether to mark the next cell or
 * to continue to back up.  When a structure (cva, list, or struct) is
 * entered the `remaining_args' variable is set to the number of
 * arguments in the structure. The first element is marked. Note that
 * it is not marked with F. Subsequent arguments to the structure are
 * marked with F when backing up. If an argument is already marked
 * with F, it is skipped.
 * 
 *   One way of parallelising this scheme is to let a worker start marking
 * unmarked structure arguments. They are then marked with F and consequently
 * skipped by other workers.
 *
 */

void gc_mark_variable(w,start)
     TAGGED *start;
     worker *w;
{
  if (!InOldGen(*start))
    gc_mark_var(w,0,start);
  
  return;
}

void gc_mark_var(w,rem,start)
     TAGGED *start;
     int rem;
     worker *w;
{
  TAGGED *stack_top;      /* Variables for book keeping of the */
  TAGGED *stack_start;    /* traversal stack.                  */
  TAGGED *stack_end;
  segment *stack;
  TAGGED *current;        /* Current cell to mark              */
  natural remaining_args; /* Remaining arguments in current structure */
  register int depth = 0;

  /* Init stack
   */

  InitStack(stack_top, stack_start, stack_end, stack);

  remaining_args = rem;
  current = start;

  /* If stolen work, proceed backward */
  if (remaining_args != 0)
    goto backward;

 forward:
  {
    CheckStackOverflow(stack_top, stack_start, stack_end, stack); 

    if (IsMarked(*current)) goto backward;

    SafeMark(*current, {/*fprintf(stderr,"%d: safe mark clash\n",w->pid)*/ });
    w->stats->heap_gc_marked += 1;

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

	  if (IsMarkedFM(*(next+VARSIZE))) goto backward;

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
	  /* This may result in static boxes being marked. We don't 
	   * care...
	   */
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

	  /* The CDR-argument is marked when backing up
	   * from following the CDR-cell.
	   */

	  if (InOldGen(*current)) goto backward;

	  next = TagToPointer(*current);

	  if (IsMarkedFM(*(next+VARSIZE))) goto backward;

	  SavePosition(stack_top, remaining_args, current);

	  remaining_args = 1;
	  current = next;
	}
	goto forward;
    
      case STR:
	{ 
	  TAGGED *next;
	  int arity, i;

	  /* Structure arguments are marked when backing up
	   * in the structure.
	   */

	  if (InOldGen(*current)) goto backward;

	  next = TagToPointer(*current);

	  if (IsMarkedFM(*(next+VARSIZE))) goto backward;

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
	goto backward;
      }
  }

  /*   Unwind until 
   *
   *     1. the stack is empty or 
   *     2. an entry is popped where `remaining_args' positive
   *
   */
 backward:
  {
    if (remaining_args > 0)
      {
	remaining_args -= 1;
	current += VARSIZE;

	if (IsMarkedFM(*current))
	  {
	    goto backward;
	  }
	else
	  {
	    SafeMarkF(*current, goto backward);
	    goto forward;
	  }
      }
    else
      {
	if (StackEmpty(stack_top, stack_start)) 
	  {
	    if (LastStack(stack_top)) /* done */
	      {
		RemoveStack(stack_top, stack_start, stack_end, stack);
		return;
	      }
	    else
	      {
		PopStack(stack_top, stack_start, stack_end, stack);
		goto backward;
	      }
	  }
	else
	  {
	    RestorePosition(stack_top,current,remaining_args);
	    goto backward;
	  }
      }
  }
}

/**********************************************************************
 **********************************************************************
 * Copying phase                                                      *
 *                                                                    *
 *                                                                    *
 *                                                                    *
 *                                                                    *
 *                                                                    *
 *                                                                    *
 *                                                                    *
 *                                                                    *
 **********************************************************************/

void    gc_copy_choicepoints PROTO((worker *));
void    gc_copy_compare_area PROTO((worker *));
void    gc_copy_environments PROTO((worker *, environment *, int));
TAGGED *gc_copy_trail PROTO((worker *, TAGGED *, TAGGED *, s32));

inline static TAGGED *check_size PROTO((worker *, integer));

inline static TAGGED *check_size(w,size)
     integer size;
     worker *w;
{
  /* Check that we do not overwrite the end of the segment, we
   * use a small buffer of 10 word to be extra safe
   */

  if (w->old_heap_top+size > (w->old_heap_end-10))
    {
#if 0
      if (w->global->flags.gc_verbose == TRUE)
	{
	  fprintf(stderr,"wasting %ld \n",
		  ((long) w->old_heap_end) - ((long) w->old_heap_top));
	}
#endif
      (void) new_segment(w);
    }

  if (w->old_heap_top+size > (w->old_heap_end-10))
    {
      FatalError("Structure do not fit into segment");
    }

  return w->old_heap_top;
}

/* Note: gc_check_size() resides here since check_size is inlined.
 */
inline TAGGED *gc_check_size(size, w)
     integer size;
     worker *w;
{
  return check_size(w,size);
}

/**********************************************************************
 * Name: gc_copy()
 */


void gc_copy(w)
     worker *w;
{
  gc_copy_choicepoints(w);
  gc_copy_compare_area(w);

  return;
}

/**********************************************************************
 * Name: gc_copy_choicepoints
 * 
 * Desc: copy all unprotected data reachable from the chain of choice
 * pointe. Environments, trail entreis and saved argument registers are
 * copied.
 */

void gc_copy_choicepoints(w)
     worker *w;
{
  choicepoint *choice;
  TAGGED *trailtop;

  trailtop = w->trail_top;
  choice = w->choice;

  while((choice != NULL) && IsNewCp(choice))
    {
      int arity;
      TAGGED *areg;     /* enables the X(i) notation  */

      trailtop = gc_copy_trail(w, trailtop, choice->trail_top,
			       choice->timestamp);
      gc_copy_environments(w,choice->cont_env,FrameSize(choice->next_instr));

      arity = choice->arity;
      areg = choice->areg;

      while(arity--)
	{
	  if (IsHeapObj(X(arity)))
	    {
	      X(arity) = gc_copy_variable(w,X(arity));
	    }
	}
      
      choice = choice->last_choice;
    }

  (void) gc_copy_trail(w, trailtop, w->trail_old_gen_top,
		       (choice == NULL ? 0x7fffffff : choice->timestamp));

  return;
}

/**********************************************************************
 * Name: gc_copy_environments
 *
 * Desc: copy all data reachable from unprotected environments
 */

void gc_copy_environments(w, frame, size)
     worker *w;
     environment *frame;
     int size;
{
  BOOL visited = FALSE;

  while ((frame!=NULL) && (visited==FALSE) && UnProtected(frame))
    {
      while(size--)
	{
	  if (IsMarked(Yf(size)))
	    {
	      if(IsHeapObj(Yf(size)))
		{
		  Yf(size) = gc_copy_variable(w,Yf(size));
		}
	      UnMark(Yf(size));
	    }
	  else
	    {
	      visited = TRUE;
	    }
	}
      size = FrameSize(frame->next_instr);
      frame = frame->cont_env;
    }

  return;
}


/**********************************************************************
 * Since compare-bindinga are not trailed crossgenerational pointers
 * are not recorded in the usual way (on the trail). Therefore we have
 * to search the compared-variable are for such pointers.
 */

void gc_copy_compare_area(w)
     register worker *w;
{
  TAGGED *current;
  segment *seg;
  
  seg = w->global->compare_seg;

  while (seg != NULL)
    {
      current = &(seg->data[0]);

      while (*current != (TAGGED) NULL)
	{
	  TAGGED comvar = *current;

	  if (not(IsCopied(comvar)) && InOldGen(comvar) &&
	     *TagToPointer(comvar) == Tagify((current+2*VARSIZE), HVA))
	    {
	      TAGGED *old_term = TagToPointer(comvar);

	      *old_term = gc_copy_variable(w,*old_term);
	    }
	  current += 3*VARSIZE;
	}
      seg = seg->next;
    }
}


/**********************************************************************
 * Name: gc_copy_trail
 *
 * Desc: copy all data reachable from the trail
 *
 */

TAGGED *gc_copy_trail(w, top, stop, uncond)
     worker *w;
     TAGGED *top, *stop;
     s32 uncond;
{
  top -= 1;
  while (top >= stop)
    {
      if ((*top == ((TAGGED) NULL)) || IsParCallTrail(top))
	{
	  SkipEntry(top);
	}
      else if (IsSVA(*top))
	{
	  TAGGED *sva_pos;

	  sva_pos = TagToPointer(*top);

	  if (ProtectedSVA(sva_pos))
	    {
	      if ((IsHeapTerm(*sva_pos) || IsHeapBox(*sva_pos)) &&
		  InNewGen(*sva_pos))
		{
		  *sva_pos = gc_copy_variable(w,*sva_pos);
		}
	    }

	  SkipSVA(top);
	}
      else if (IsHVA(*top))
	{
	  if (InNewGen(*top))
	    {
	      TAGGED *heap_pos = TagToPointer(*top);

	      /* Let's tidy the trail while we are at it (saving 
	       * ourselves some work in the process).
	       */

	      if (VARSIZE == 1)
		{
		  if (GetHVATime(*(top-1)) > uncond)
		    {
		      ResetHVA(top);
		      SkipHVA(top);
		      continue;
		    }
		}
	      else
		{
		  if (GetHVATime(*top) > uncond)
		    {
		      ResetHVA(top);
		      SkipHVA(top);
		      continue;
		    }
		}

	      if (IsLive(*heap_pos))
		{
		  CopyHVA(top);
		}
	      else if (IsCopied(*heap_pos))
		{
		  UpdateTrailForward(top); 
		}
	      else
		{
		  ResetHVA(top);
		}

	      SkipHVA(top);
	    }
	  else			/* In old genertion */
	    {
	      TAGGED *old_gen_term;

	      old_gen_term = TagToPointer(*top);
	      
	      *old_gen_term = gc_copy_variable(w,*old_gen_term);
	      
	      /* If this trail entry was only there to signal a
	       * cross generational binding, then remove it.
	       */
	      
	      if (VARSIZE == 1)
		{
		  if (GetHVATime(*(top-1)) > uncond)
		    {
		      ResetHVA(top);
		    }
		}
	      else
		{
		  if (GetHVATime(*top) > uncond)
		    {
		      ResetHVA(top);
		    }
		}

	      SkipHVA(top);
	    }
	}
      else if (IsValueTrail(top))
	{
	  if (InNewGen(*top))
	    {
	      TAGGED *heap_pos = TagToPointer(*top);

	      if (IsLive(*heap_pos))
		{
		  CopyValue(top);
		}
	      else if (IsCopied(*heap_pos))
		{
		  UpdateTrailForward(top);
		}
	      else
		{
		  ResetValue(top);
		  SkipValueCell(top);
		  continue;
		}
	    }
	  else			/* In old generation */
	    {
	      TAGGED *old_gen_term;
	      
	      old_gen_term = TagToPointer(*top);

	      *old_gen_term = gc_copy_variable(w,*old_gen_term);
	    }

	  *(top-1) = gc_copy_variable(w,*(top-1));

	  SkipValueCell(top);
	}
      else if (IsNUM(*top)) /* start of parallel phase on worker */
	{
	  /* Start of parallel execution */
	  *top = Tagify(w->heap_top,NUM);
	  SkipEntry(top);
	}
      else
	{
	  SkipEntry(top);
	}
    }
  top += 1;
  return top;
}

/**********************************************************************
 * copy prototypes
 */

inline static TAGGED *copy_hva(TAGGED, TAGGED **, worker *);
inline static TAGGED *copy_cva(TAGGED, TAGGED **, worker *);
inline static TAGGED *copy_box(TAGGED, TAGGED **, worker *);
inline static TAGGED *copy_internal_list(TAGGED [], TAGGED **, worker *);
inline static TAGGED *copy_list(TAGGED, TAGGED **, worker *);
inline static TAGGED *copy_struct(TAGGED, TAGGED **, worker *);
inline static TAGGED *copy_term(TAGGED, TAGGED **, worker *);

/**********************************************************************
 * Name: copy_hva()
 * 
 * Desc: Copies the given heap variable and the surrounding structure
 *       if it is an internal cell.
 * 
 * Returns: A tagged pointer to the first of the copied cells, OR if
 *          the cell has already been copied GC_LOCKVALUE.
 */

inline static TAGGED *copy_hva(term, copy, w)
     TAGGED term, **copy;
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

      /* Calculate the distance to the head of the structure and 
       * find head of live structure.
       */
      for (dist = 0; IsInternal(*old); old -= VARSIZE, dist += VARSIZE);

      /* Find size of structure size = VARSIZE already set.
       */
      for (size = VARSIZE; IsInternal(old[size]); size += VARSIZE);

      /*   We lock cells to ensure that no other worker may copy the same term
       * while we are. If another worker manage to copy the cell ahead of us,
       * we then return GC_LOCKVALUE to signal this.
       */

      LockCell(old[0], old0, return (TAGGED *) GC_LOCKVALUE);

      new = check_size(w,size);
      
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

      *copy = &(new[dist]); /* location of copy of original cell */
    }
  else
    {
      TAGGED old0; /* the contents of old[0] */

      LockCell(old[0], old0, return (TAGGED *) GC_LOCKVALUE);

      /* The variable does not reside in a live structure.
       * Check if it is ordered.
       */
      if (IsOrdered(term))
	{
	  /* fprintf(stderr,"%d locked %lx\n",w->pid,&(old[0])); */

	  size = 2*VARSIZE;
	  new = check_size(w,size);

	  new[0] = *(old-VARSIZE);
	  
	  new[VARSIZE] = FromOld(old0);

	  Forward(old[0], (new+VARSIZE));

	  if (VARSIZE == 2)
	    new[VARSIZE+1] = *(old+1);

	  MarkF(new[0]);
	  MarkF(new[VARSIZE]);
	  
	  *copy = &(new[VARSIZE]);
	}
      else
	{
	  size = VARSIZE;

	  new = check_size(w,size);

	  new[0] = FromOld(old0);
	  Forward(old[0], new);

	  if (VARSIZE == 2)
	    new[1] = old[1];
	  
	  *copy = new;
	}
    } 

  Mark(new[0]);
  new += size;

  CopyHeapTop = new;

  return (new-VARSIZE);
}


inline static TAGGED *copy_cva(term, copy, w)
     TAGGED term, **copy;
     worker *w;
{
  TAGGED *new;
  TAGGED *old;
  TAGGED old0; /* the contents of old[0] */

  new = check_size(w,VARSIZE*2);
  old = TagToPointer(term);

  /*   We lock cells to ensure that no other worker may copy the same term
   * while we are. If another worker manage to copy the cell ahead of us,
   * we then return GC_LOCKVALUE to signal this.
   */
  LockCell(old[0], old0, return (TAGGED *) GC_LOCKVALUE);
	
  new[0] = FromOld(old0);
  Forward(old[0], &(new[0])); 

  new[VARSIZE] = FromOldArg(old[VARSIZE]);
  Forward(old[VARSIZE], &(new[VARSIZE]));
	
  if (VARSIZE == 2)
    {
      new[1] = old[1];
      new[3] = old[3];
    }

  *copy = new;       /* location of copy of original cell */

  Mark(new[0]);      /* Mark first cell */
  new += 2*VARSIZE;
  CopyHeapTop = new;

  return (new-VARSIZE);
}


inline static TAGGED *copy_box(term, copy, w)
     TAGGED term, **copy;
     worker *w;
{
  TAGGED *new, *ret;
  TAGGED *old;
  TAGGED old0;     /* the contents of old[0] */
  integer i, size;

  old = TagToPointer(term);

  /*   We lock cells to ensure that no other worker may copy the same term
   * while we are. If another worker manage to copy the cell ahead of us,
   * we then return GC_LOCKVALUE to signal this.
   */
  LockCell(old[0], old0, return (TAGGED *)  GC_LOCKVALUE);

  size = GetBoxSize(old0);

  ret = check_size(w,VARSIZE*size);

#if defined(ALIGNED_BOXES)
  ret += 1;
  DoubleAlignedAddress(ret);
  ret -= 1;
#endif

  new = ret;
	
  new[0] = FromOld(old0);

  for (i = 1; i < size*VARSIZE; i++) /* copy term */
    {
      new[i] = old[i];
    }

  Forward(old[0], &(new[0])); /* update oldspace */

  new += size*VARSIZE;
  CopyHeapTop = new;

  *copy = ret; /* location of copy of original cell */

  return ret;
}


inline static TAGGED *copy_internal_list(old, copy, w)
     TAGGED old[], **copy;
     worker *w;
{
  TAGGED *new;
  TAGGED old0; /* the contents of old[0] */
  int dist, i, size;

  /* Find head of struct.
   */
  for (dist = 0; IsInternal(*old); old -= VARSIZE, dist += VARSIZE);

  /* Find size of structure.
   */
  for (size = VARSIZE; IsInternal(old[size]); size += VARSIZE);

  /*   We lock cells to ensure that no other worker may copy the same term
   * while we are. If another worker manage to copy the cell ahead of us,
   * we then return GC_LOCKVALUE to signal this.
   */
  LockCell(old[0], old0, return (TAGGED *) GC_LOCKVALUE);
  
  new = check_size(w,size);

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

  *copy = &(new[dist]); /* location of copy of original cell */

  Mark(new[0]);
  new += size;

  CopyHeapTop = new;

  return (new-VARSIZE);
}


inline static TAGGED *copy_list(term, copy, w)
     TAGGED term, **copy;
     worker *w;
{
  TAGGED *new;
  TAGGED *old;

  old = TagToPointer(term);

  if (IsInternal(old[0]))
    {
      return copy_internal_list(old, copy, w);
    }
  else
    {
      TAGGED old0; /* the contents of old[0] */

      /*   We lock cells to ensure that no other worker may copy the same term
       * while we are. If another worker manage to copy the cell ahead of us,
       * we then return GC_LOCKVALUE to signal this.
       */
      LockCell(old[0], old0, return (TAGGED *) GC_LOCKVALUE);

      new = check_size(w,VARSIZE*2);
	
      new[0] = FromOld(old0);
      Forward(old[0], &(new[0])); 

      new[VARSIZE] = FromOldArg(old[VARSIZE]);
      Forward(old[VARSIZE], &(new[VARSIZE]));
	
      if (VARSIZE == 2)
	{
	  new[1] = old[1];
	  new[3] = old[3];
	}

      *copy = new; /* location of copy of original cell */

      Mark(new[0]);
      new += 2*VARSIZE;
      CopyHeapTop = new;


      return (new-VARSIZE);
    }
}


inline static TAGGED *copy_struct(term, copy, w)
     TAGGED term, **copy;
     worker *w;
{
  TAGGED *new;
  TAGGED *old;
  TAGGED old0; /* the contents of old[0] */
  int i, size;

  old = TagToPointer(term);

  /*   We lock cells to ensure that no other worker may copy the same term
   * while we are. If another worker manage to copy the cell ahead of us,
   * we then return GC_LOCKVALUE to signal this.
   */
  LockCell(old[0], old0, return (TAGGED *) GC_LOCKVALUE);

  size = (ArityOf(old0)+1)*VARSIZE;
  new  = check_size(w,size);

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

  *copy = new; /* location of copy of original cell */

  return (CopyHeapTop-VARSIZE);
}

inline static TAGGED *copy_term(term, copy, w)
     TAGGED term, **copy;
     worker *w;
{
  switch(TagOf(term))
    {
    case HVA:
      return copy_hva(term, copy, w);

#ifdef CONSTR
    case CVA:
      return copy_cva(term, copy, w);
#endif  /* CONSTR */

    case SVA:
      Error("copy_term: found stack variable in heap");
      break;

    case NUM:
    case ATM:
      Error("copy_term: attempt to copy a non-heap term");
      break;

    case BOX:
      return copy_box(term, copy, w);

    case GEN:
      Error("copy_term: attempt to copy a non-heap term");
      break;

    case LST:
      return copy_list(term, copy, w);

    case STR:
      return copy_struct(term, copy, w);

#ifdef UNBOUND
    case UVA:
      Error("copy_term: attempt to copy a non-heap term");
      break;
#endif /* UNBOUND */
    }
}


/**********************************************************************
 * Name: gc_copy_variable
 *
 * Desc: copies a term tree to the new area.
 */


/*
 * Note: We assume that all internal live variables have been marked
 * 
 *       Forwarded data is marked with F but not M (dead data is not
 *       marked at all, but we should not encounter any such data).
 *
 * Note: We need a separate case to handle generic objects (GEN) since we
 *       do not want to scan the internals.
 */
static TAGGED gc_copy_variable(w,start)
     TAGGED start;
     worker *w;
{
  TAGGED result;
  TAGGED *next, tag, *old;

  tag = TagOf(start);
  old = TagToPointer(start);

  /* Check if we got intermediate value */
  if (IsCopied(start))
    {
      TAGGED *new_location;

      WaitUntilCopied(start, new_location);

      return Tagify(new_location,tag);
    }
  else if (not(IsHeapObj(start)) && not(IsHeapBox(start)))
    {
      UnMark(start);
      return start;
    }
  
  if (InOldGen(start))
    {
      UnMark(start);
      result = start;
    }
  else if (IsCopied(*old))
    {
      TAGGED *new_location;

      WaitUntilCopied(start, new_location);
      
      result = Tagify(new_location, tag);
    }
  else if (IsHeapTerm(start))
    {
      TAGGED *copy;
      /* First we copy the root */

      next = copy_term(start, &copy, w);

      if (next != (TAGGED *) GC_LOCKVALUE)
	{
	  scan(next, TagToPointer(NULL), tag, w);
	  result = Tagify(copy,tag);
	}
      else
	{
	  TAGGED *new_location;

	  WaitUntilCopied(start, new_location);

	  result = Tagify(new_location, tag);
	}
    }
  else if (IsHeapBox(start))
    {
      TAGGED *copy;

      next = copy_box(start, &copy, w);

      if (next != (TAGGED *) GC_LOCKVALUE)
	{
	  result = Tagify(next, BOX);
	}
      else
	{
	  TAGGED *new_location;

	  WaitUntilCopied(start, new_location);

	  result = Tagify(new_location, tag);
	}
    }
  else if (IsGEN(start))
    {
      next = GetMethod(collect, start)(start, w);

      if (next != (TAGGED *) GC_LOCKVALUE)
	{
	  result = Tagify(next, GEN);
	}
      else
	{
	  TAGGED *new_location;

	  WaitUntilCopied(start, new_location);

	  result = Tagify(new_location, tag);
	}
    }
  else
    {
      /* Error("copy-gc-ali: nothing to copy..."); */

      UnMark(start);
      result = start;
    }

  return result;
}

/**************************************************/

static void scan(next, prev, tag, w)
     TAGGED *next, *prev, tag;
     worker *w;
{
  register int depth = 0;

#ifndef NDEBUG
  register int debugcounter = 0;
#endif /* NDEBUG */

  /* If stolen chain, continue backward  */
  if (prev != TagToPointer(NULL))
    goto backward;

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
	  CheckCopied(*next);

	  if (IsHeapBox(*next))
	    {
	      TAGGED *new, *copy;

	      /* If another worker copies the box ahead of us,
	       * the copy_box method should return GC_LOCKVALUE.
	       */
	      new = copy_box(*next, &copy, w);
	      
	      if (new != (TAGGED *) GC_LOCKVALUE)
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
	   * the collect method should return GC_LOCKVALUE.
	   */
	  new = GetMethod(collect, *next)(*next, w);
	  
	  if (new != (TAGGED *) GC_LOCKVALUE)
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
	    return;
	  }
	else 
	  {
	    TAGGED oldprev;
	    TAGGED gcbits;
	  
	    DecDepth;

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



/**********************************************************************
 * Name: gc_update
 * 
 * Desc: Updates the information stored in the choice points and 
 *       compacts the trail at the same time.
 */
 
void gc_update(w)
     worker *w;
{
  register choicepoint *choice = w->choice;
  TAGGED *t = w->trail_top-1;
  integer trailcells_deleted;

  /* Scan trail to find the number of deleted trail cells. */

  for (t = w->trail_old_gen_top, trailcells_deleted = 0 ;
       t != w->trail_top ; t++)
    {
      if (*t == 0)
	trailcells_deleted++;
    }

  t = w->trail_top - 1;
  
  while ((choice != NULL) && IsNewCp(choice))
    {
      while (t >= choice->trail_top)
	{
	  if (*t == 0)
	    trailcells_deleted--;
	  t--;
	}
      
      choice->generation = OLD_GEN;
      choice->trail_top -= trailcells_deleted;
      choice->global_top = w->heap_top;
      choice = choice->last_choice;
    }

  {
    TAGGED *stop;

    stop = max(w->trail_old_gen_top,w->trail_start);

    while(t >= stop)
      {
	if (*t == 0)
	  trailcells_deleted--;

	t--;
      }
  }
  

#ifndef NDEBUG
  if (trailcells_deleted != 0)
    Error("`trailcells_deleted' != 0");
#endif

  /* compact trail */

  {
    register TAGGED *dest, *current;

    dest = w->trail_old_gen_top;

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

  return;
}


/**********************************************************************
 * Push a choice point on the stack to have as starting point.
 *
 */

void gc_PushChoicePoint(w)
     worker *w;
{
  int i, arity;
  choicepoint *newchoice;
  code *sizecode;
  
  arity = w->gc_info.arity;

  /* We assume that it will fit on the stack without shifting. This is 
   * safe since we have a stack margin of 1 KBYTES
   */ 

  newchoice = (choicepoint *) Get_Local_Stack_Top;

  newchoice->trail_top = w->trail_top;
  newchoice->global_top = w->heap_top;
  newchoice->last_choice = w->choice;
  newchoice->cont_env = w->frame;
  newchoice->next_clause = NULL;
  newchoice->arity = arity;
  newchoice->generation = NEW_GEN;

  for(i=0 ; i < arity ; i++)
    {
      newchoice->areg[ARSIZE*i] = Xw(i);
    }

  newchoice->timestamp = w->time;
  w->time += TIMEUNIT;
  w->uncond = w->time;

  w->choice = newchoice;

  /* We have to create a call instruction with the proper
   * environmentsize.
   */

  sizecode = (code *) Get_Local_Stack_Top;

  sizecode += 2;
  
  StoreFrameSize(sizecode,w->gc_info.env_size); 

  w->choice->next_instr = sizecode;

  return;
}

/**********************************************************************/

void gc_PopChoicePoint(w)
     worker *w;
{
  int i, arity;
  choicepoint *newchoice;

  newchoice = w->choice;

  arity = newchoice->arity;
  newchoice = w->choice;

  for(i=0 ; i < arity ; i++)
    {
      Xw(i) = newchoice->areg[ARSIZE*i];
    }
 
  w->trail_top = newchoice->trail_top;

  w->choice = newchoice->last_choice;

  return;
}

/**********************************************************************
 * Name: gc_return_new_gen_memory
 *
 * Desc: return new generations memory to the free list.
 */

void gc_return_new_gen_memory(w)
     worker *w;
{
  /* Keep last segment */
  segment *seg;

  seg = w->new_used_segments;

  if (seg->next != NULL)
    {
      segment *prev = seg;

      while(prev->next->next != NULL)
	{
	  prev = prev->next;
	}

      seg = prev->next;
      prev->next = NULL;

      free_new_seg_list(w, w->new_used_segments);

      w->new_used_segments = seg;
    }

  w->heap_top = w->heap_start = &(seg->data[0]);
  w->heap_end = ((TAGGED *) seg) + seg->size;
  w->heap_margin = w->heap_end - (margin_size/sizeof(TAGGED));

  if (SeqWorker(w))
    {
      free_new_seg_list(w, w->global->compare_seg);
      w->global->compare_seg=NULL;
      allocate_static_segment(w);
    }
}


/**********************************************************************
 * Name: gc_clean
 *
 * Desc: Performs cleanup operations.
 */

void gc_clean(w)
     worker *w;
{
#if defined(TIMESTAMP) || defined(UNBOUND)
  register int i;
  
  for(i = w->global->active_workers ;
      i >= 0 ;
      i--)
    {
      if (w[i-(w->pid)].time > w->time)
	{
	  w->time = w[i-(w->pid)].time;
	}
    }
#endif	

  w->time += TIMEUNIT;
  w->uncond = w->time;
  w->old_gen_uncond = w->uncond;
  w->trail_old_gen_top = w->trail_top;
}

/**********************************************************************
 * Name: worker_garbage_collect
 *
 * Desc: entrypoint for worker garbage collector
 *
 */

BOOL worker_garbage_collect(w, from_event_loop)
     worker *w;
     BOOL from_event_loop;
{
  if (!from_event_loop)
    {
      synchronize(w);
    }

  garbage_collect(w);

  BarrierSynch(w,3); /****************************************/

  return TRUE;
}

 


#endif /* COPY_GC  && COPY_ALI */
