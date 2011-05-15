/*
 * gc.h - Support macros for the garbage collector.
 *
 * Johan Bevemyr.....Mon Oct 28 1991
 * Patric Hedlin.....Thu Jan 19 1995
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef GC_H
#define GC_H


#define GC_MRK_MASK	LIVEMARKMASK
#define GC_FWD_MASK	LINKMARKMASK
#define GC_FMK_MASK	FULLMARKMASK
#define GC_PTR_MASK	POINTERMASK

#define SafeMarking(T,AlreadyMarkedCode,Bits)				   \
{									   \
  TAGGED assumed_val = (T);						   \
  TAGGED old_val;							   \
  TAGGED new_val = assumed_val | Bits;					   \
  TAGGED *Pos = &(T);							   \
  KSR1(DECLARE_SWAP_VAL);						   \
									   \
  old_val = swap_il(Pos,new_val);					   \
									   \
  if ((old_val != assumed_val) && (old_val != new_val))			   \
    {									   \
      (void) swap_il(Pos,(new_val|old_val));			           \
      if ((new_val|old_val)&0x3 != 0x3)                                    \
	fprintf(stderr,"%d: Safe marking failed\n",w->pid);		   \
      AlreadyMarkedCode;						   \
    }									   \
}

#define SafeUnMarking(T,AlreadyMarkedCode,Bits)				   \
{									   \
  TAGGED new_val = (T) & (~Bits);					   \
  TAGGED *Pos = &(T);							   \
  KSR1(DECLARE_SWAP_VAL);                                                  \
  									   \
  if(swap_il(Pos,new_val) == new_val)					   \
    {									   \
      AlreadyMarkedCode;						   \
    }									   \
}

#ifdef PARALLEL

#  define SafeMark(T,M)	   SafeMarking(T,M,GC_MRK_MASK)
#  define SafeMarkF(T,M)   SafeMarking(T,M,GC_FWD_MASK)

#  define SafeUnMark(T,M)  SafeUnMarking(T,M,GC_MRK_MASK)
#  define SafeUnMarkF(T,M) SafeUnMarking(T,M,GC_FWD_MASK)

#else

#  define SafeMark(T,M)    Mark(T)
#  define SafeMarkF(T,M)   MarkF(T)

#  define SafeUnMark(T,M)  UnMark(T)
#  define SafeUnMarkF(T,M) UnMarkF(T)

#endif

#define Mark(T)		 (T) |= GC_MRK_MASK
#define MarkF(T)	 (T) |= GC_FWD_MASK
			 
#define UnMark(T)	 (T) &= (~GC_MRK_MASK)
#define UnMarkF(T)	 (T) &= (~GC_FWD_MASK)
			 
#define IsMarked(T)	 ((uword)(T) & GC_MRK_MASK)
#define IsMarkedF(T)	 ((uword)(T) & GC_FWD_MASK)
#define IsMarkedFonly(T) HasOnlyLinkMark(T)
#define IsForM(T)	 ((uword)(T) & GC_FMK_MASK)
#define IsMarkedFM(T)	 (((uword)(T) & GC_FMK_MASK) == GC_FMK_MASK)
			 
#define GetBits(T)	 ClrMask((uword)(T), GC_PTR_MASK)
#define SetBits(T,B)	 ((TAGGED) SetMask(BaseOffset(T), B))
			 
#define GetGCBits(T)	 GetFullMark(T)
#define SetGCBits(T,B)	 ((TAGGED) SetMask(T,B))
			 
#define AddMark(T)	 ((uword)(T) | GC_MRK_MASK)
#define AddMarkF(T)	 ((uword)(T) | GC_FWD_MASK)
#define AddMarkFM(T)	 AddMark(AddMarkF(T))
			 
#define RemoveMark(T)	 ((TAGGED) (((uword)(T) - GC_MRK_MASK)))
#define RemoveMarkF(T)	 ((TAGGED) (((uword)(T) - GC_FWD_MASK)))
#define RemoveMarkFM(T)	 RemoveMark(RemoveMarkF(T))
			 
#define PutMark(T)	 ((TAGGED) (((uword)(T) + GC_MRK_MASK)))
			 
#define PutValue(X,Y)	 (((uword)(X) &  GC_PTR_MASK) |	\
			  ((uword)(Y) & ~GC_PTR_MASK))

#define PutValueFirst(X,Y)  (((uword)(X) &  (GC_PTR_MASK | GC_FWD_MASK)) | \
			     ((uword)(Y) & ~(GC_PTR_MASK | GC_FWD_MASK)))

#define Reset(V)	*TagToPointer(V) = (V)

#define Reverse(C,N)							   \
{									   \
  register TAGGED *temp = TagToPointer(*(N));				   \
									   \
  *(N) = PutValue(C,*(N));						   \
  (C) = (N);								   \
  (N) = temp;								   \
}
                         
#define Undo(C,N)          Reverse(N,C)

#define Advance(C,N)							   \
{									   \
  register TAGGED *temp = TagToPointer(*(C));				   \
									   \
  *(C) = PutValue(N,*(C));						   \
  (C) -= VARSIZE;							   \
  (N) = TagToPointer(*(C));						   \
  *(C) = PutValue(temp,*(C));						   \
}


/**********************************************************************
 * GC checks
 */

#ifdef SEGMENTED_MEMORY
# ifdef GENERATIONAL
#  define TryAllocate(FAILCODE)						   \
{									   \
  TAGGED seg = (TAGGED) NULL;						   \
									   \
  /* try allocate a new generation segment */				   \
  if (w->global->new_free_seg > 2)					   \
    {									   \
      seg = new_new_segment(w);						   \
    }									   \
									   \
  if (seg != (TAGGED) NULL)						   \
    {									   \
      PushOnTrail(w,Tagify(seg,ATM));					   \
    }									   \
  else									   \
    {									   \
      FAILCODE;								   \
    }									   \
}
# else /* not GENERATIONAL */
#  define TryAllocate(FAILCODE)						   \
{									   \
  if (w->global->free_seg > w->global->used_seg+1)			   \
    {									   \
      TAGGED seg;							   \
      /* allocate a new segment */					   \
      seg = new_segment(w);						   \
      PushOnTrail(w,Tagify(seg,ATM));					   \
    }									   \
  else									   \
    {									   \
      FAILCODE;								   \
    }									   \
}
# endif /* GENERATIONAL */
#else /* not SEGMENTED_MEMORY */
# define TryAllocate(FAILCODE) FAILCODE
#endif /* SEGMENTED_MEMORY */


#ifdef PARALLEL 
#define GC_If_Needed(W,A)						   \
{									   \
  if (H > (W)->heap_margin)						   \
    {									   \
      SaveCache(H);							   \
      TryAllocate(							   \
		  {							   \
		    W->gc_info.arity = A;				   \
		    W->gc_info.env_size = FrameSize(W->next_instr);	   \
		  							   \
		    if (SeqWorker(w))					   \
		      {							   \
			garbage_collect(W);				   \
			/* Run_Parallel(W_GC_FINISH); */		   \
			BarrierSynch(w,3); 				   \
			ActivateWorkersStop(w);                            \
		      }							   \
		    else						   \
		      {							   \
			AddGlobalEvent(GLOBAL_EVENT_GC);		   \
									   \
			if (not(worker_garbage_collect(W, FALSE)))	   \
			  goto global_fail;				   \
		      }							   \
		  }							   \
		  );							   \
      LoadCache(H);							   \
    }									   \
}

#define GC_If_Needed_Execute(W, DEF) GC_If_Needed(W,ArityOf(DEF->name))

#define GC_If_Needed_Require(W, A)					   \
{									   \
  GC_If_Needed(W,A);							   \
									   \
  if (EventTrue(w->global->event_flag,GLOBAL_EVENT_GC))			   \
    {									   \
      if (w->pid != 0)							   \
        {								   \
          SaveCache(H);							   \
									   \
          W->gc_info.arity = A;						   \
          W->gc_info.env_size = FrameSize(W->next_instr);		   \
									   \
          if (not(worker_garbage_collect(W, FALSE)))			   \
	    goto global_fail;						   \
									   \
	  LoadCache(H);							   \
        }								   \
    }									   \
}

#else /* Not PARALLEL */

#define GC_If_Needed_Execute(W, A) GC_If_Needed(W, ArityOf(A->name))
#define GC_If_Needed_Require(W, A) GC_If_Needed(W, A)

#define GC_If_Needed(W, A)						   \
{									   \
  if (H > (W)->heap_margin)						   \
    {									   \
      SaveCache(H);							   \
									   \
      TryAllocate({							   \
		    W->gc_info.arity = A;				   \
		    W->gc_info.env_size = FrameSize(W->next_instr);	   \
		    							   \
		    garbage_collect(W);					   \
		    BarrierSynch(w,3); /* Run_Parallel(W_GC_FINISH); */	   \
		  });							   \
									   \
      LoadCache(H);							   \
    }									   \
}

#endif /* Not PARALLEL */


#ifdef PARALLEL

#define GC_Sequentialize_Low_To_High					   \
{									   \
  register volatile BOOL *prev;						   \
									   \
  prev = &(w->gc_info.done[w->pid-2]);					   \
									   \
  if (w->pid != 1)							   \
    while(!*prev);							   \
}

#define GC_Sequentialize_High_To_Low					   \
{									   \
  register volatile BOOL *prev;						   \
									   \
  prev = &(w->gc_info.done[w->pid]);					   \
									   \
  if (w->pid != w->global->active_workers)				   \
    while(!*prev);							   \
}

#define GC_Run_Parallel(C)						   \
{									   \
  register int i;							   \
									   \
  for (i = 0; i < w->global->active_workers; i++)			   \
    w->gc_info.done[i] = FALSE;						   \
									   \
  Run_Parallel(C);							   \
}

#define GC_Start_Parallel(C)						   \
{									   \
  register int i;							   \
									   \
  for (i = 0; i < w->global->active_workers; i++)			   \
    w->gc_info.done[i] = FALSE;						   \
									   \
  Start_Parallel(C);							   \
}

#define GC_Stop_Parallel Stop_Parallel

#define GC_Run_Parallel_NoWait(C)					   \
{									   \
  register int i;							   \
									   \
  for (i = 0; i < w->global->active_workers; i++)			   \
    w->gc_info.done[i] = FALSE;						   \
									   \
  Run_Parallel_NoWait(C);						   \
}
#else /* Not PARALLEL */

#define GC_Run_Parallel(C)
#define GC_Start_Parallel(C)
#define GC_Stop_Parallel

#endif /* Not PARALLEL */


extern void garbage_collect PROTO((worker *));
extern BOOL worker_garbage_collect PROTO((worker *, BOOL));


#endif /* GC_H */
