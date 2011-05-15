/*    -*- C -*- 
 *    File:	 new-copy-gc.h  (~jb/Reform/Luther/Emulator/new-copy-gc.h)
 *    Author:	 Johan Bevemyr
 *    Created:	 Mon Dec 18 17:18:55 1995
 *    Purpose:   
 */ 


#define SHARE_MARKING 1 
#define SHARE_COPYING 1

#define PUSHDEPTH  1
#define SPLITDEPTH 1

#ifndef NEW_COPY_GC_H
#define NEW_COPY_GC_H

#define IsNewCp(cp)            (cp->generation == NEW_GEN)
#define IsParCallTrail(T)      (IsSTR(*(T)))
#define UnProtected(FRAME)     ((FRAME) > ((environment *) w->gc_info.last_safe))

#define ProtectedSVA(STACKVAR) ((STACKVAR) < ((TAGGED *) w->gc_info.last_safe))

#define InOldGen(T)            (TagToPointer(T) > w->global->new_gen_limit)
#define CopyHeapTop            w->old_heap_top

#define InNewGen(T)            (!InOldGen(T))
#define IsLive(T)              (IsMarked(T))

#define SkipValueCell(T)       (T) -= 2
#define SkipHVA(T)             (T) -= TrailHVASize
#define SkipSVA(T)             (T) -= 1
#define SkipEntry(T)           (T) -= 1


/**********************************************************************/

/************************************************************
 * Reset macros
 */

#if (TrailHVASize == 2)
#  define ResetHVA(T) ResetValue(T)
#else                                       
   /* assume TrailHVASize == 1 */
#  define ResetHVA(T) ResetCell(T)
#endif 

#define ResetCell(T) ResetEntry(T)

#define ResetEntry(T)							   \
{									   \
  *(T) = 0;								   \
}

#define ResetValue(T)							   \
{									   \
  *(T) = 0;								   \
  *(T-1) = 0;								   \
}

/************************************************************
 * Copy macros
 */

#define CopyHVA(T)							   \
{									   \
  *(T) = gc_copy_variable(w,*T);					   \
}

#define CopyValue(T)							   \
{									   \
  uword utf_tag = TagOf(*(T));						   \
  TAGGED utf_copied_value;						   \
									   \
  utf_copied_value = gc_copy_variable(w,Tagify(TagToPointer(*(T)),HVA));   \
									   \
  *(T) = Tagify(TagToPointer(utf_copied_value),utf_tag);		   \
									   \
}


/**********************************************************************
 * Stack support.
 * 
 * The stack is used when marking live data. The unused segments (the
 * ones we are going to copy the live data to) are used to keep the
 * stack. This guarantees that the stack is large enough.
 *
 */

#define InitStack(T,S,E,Stack)						   \
{									   \
  PushOnHeap(w->old_heap_top, w->old_heap_start);			   \
  PushOnHeap(w->old_heap_top, w->old_heap_end);				   \
  PushOnHeap(w->old_heap_top, 0); /* sentinel */			   \
  (T) = (S) = w->old_heap_top;						   \
  (E) =     w->old_heap_margin;						   \
  (Stack) = w->used_segments;						   \
}

#define RemoveStack(T,S,E,Stack)					   \
{									   \
  TAGGED sentinel;							   \
  w->old_heap_top = (T);						   \
  w->old_heap_margin = (E);						   \
  PopFromHeap(w->old_heap_top,sentinel);				   \
  PopFromHeap(w->old_heap_top,(TAGGED) w->old_heap_end);		   \
  PopFromHeap(w->old_heap_top,(TAGGED) w->old_heap_start);		   \
}

#define CheckStackOverflow(T,S,E,Stack)					   \
{									   \
  if((T) > (E)) /* allocate new stack */				   \
    {									   \
      segment *new;							   \
									   \
      w->old_heap_top = (T);       /* new segment pushes NULL on top */	   \
      new = (segment *) new_segment(w);					   \
									   \
      PushOnHeap(w->old_heap_top,Stack);				   \
      PushOnHeap(w->old_heap_top,T);					   \
      PushOnHeap(w->old_heap_top,S);					   \
      PushOnHeap(w->old_heap_top,E);					   \
									   \
      Stack = new;							   \
      (T) = (S) = w->old_heap_top;					   \
      (E) = w->old_heap_margin;						   \
    }									   \
}

#define PopStack(T,S,E,Stack)						   \
{									   \
  segment *old;								   \
  TAGGED *new_heap_top;							   \
									   \
  old = Stack;								   \
									   \
  PopFromHeap(w->old_heap_top,(TAGGED)E);				   \
  PopFromHeap(w->old_heap_top,(TAGGED)S);				   \
  PopFromHeap(w->old_heap_top,(TAGGED)T);				   \
  PopFromHeap(w->old_heap_top,(TAGGED)Stack);				   \
									   \
  free_segment(old,w);							   \
}

						 

#define SavePosition(S,R,C)						   \
{									   \
  IncDepth;                                                                \
  if(R != 0)								   \
    {									   \
      if (!(Share(w) && AddMarkBuffer(w,R,C,{})))                          \
	{								   \
	  PushSize(S,R);						   \
	  PushTAGGED(S,C);						   \
	}								   \
    }									   \
}

#define RestorePosition(S,CURR,REM)					   \
{									   \
  CURR = PopTAGGED(S);							   \
  REM = PopSize(S);							   \
  /* RemoveMarkBufferTail(w,REM,CURR); */				   \
}


#define PushTAGGED(S,T)    {*(S) = (TAGGED) (T);(S)++;}
#define PushSize(S,T)      {*(S) = (TAGGED) (T);(S)++;}

#define PopTAGGED(S)       ((TAGGED *) *(--(S)))
#define PopSize(S)         ((natural) *(--(S)))

#define StackEmpty(S,E)    ((S) == (E))
#define LastStack(T)       (*((T)-VARSIZE) == 0)


/*****************************************************************
 * Some usefull macros for getting copied cells new locations
 */

#define CheckCopied(NEXT)						   \
{									   \
  if (InOldGen(NEXT))							   \
    {									   \
      goto backward;							   \
    }									   \
  else if (IsCopiedCell(NEXT))						   \
    {									   \
      GetCopied(NEXT); 						           \
      goto backward;							   \
    }									   \
}

#define GetCopied(NEXT)							   \
{									   \
  TAGGED bits;								   \
  TAGGED *new_loc;                                                         \
									   \
  WaitUntilCopied(NEXT,new_loc); 					   \
									   \
  bits = GetBits(NEXT);							   \
  NEXT = SetBits(new_loc,bits);				                   \
}

#define CopyTerm(COPY_FUNC)						   \
{									   \
  TAGGED oldnext;							   \
  TAGGED *newnext;							   \
  TAGGED *copy;								   \
									   \
  oldnext = *next;							   \
  newnext = COPY_FUNC(*next, &copy, w);					   \
									   \
  if (newnext != (TAGGED *) GC_LOCKVALUE)				   \
    {									   \
      GoForward(w,copy,prev,next,tag);                                     \
									   \
      goto forward;							   \
    }									   \
  else									   \
    {									   \
      /* get new location */						   \
									   \
      GetCopied(*next);							   \
      goto backward;							   \
    }									   \
}

/**********************************************************************
 * Forwarding macros
 */

#define GetNewLocation(T)  ((TAGGED *) RemoveMarkF(T))
#define FromOld(T)         RemoveMark(T)
#define FromOldArg(T)      RemoveMarkFM(T)

#define IsCopied(T)        Forwarded(T)
#define IsCopiedCell(T)    (IsCopied(*TagToPointer(T)))
#define GetNewLoc(T)       ((TAGGED) GetNewLocation(T))
#define InOldStruct(T)     (IsInternal(*T) || IsInternal(*((T)+VARSIZE)))
#define IsInternal(T)      IsMarkedFM(T)

#define Forwarded(X)       (IsMarkedF(X) && !IsMarked(X))

#define UpdateTrailForward(T)						   \
{									   \
  uword utf_tag = TagOf(*(T));						   \
  TAGGED *utf_loc;							   \
									   \
  WaitUntilCopied(*(T),utf_loc);					   \
  									   \
  *(T) = Tagify(utf_loc,utf_tag);					   \
									   \
  /* *(T) = Tagify(GetNewLocation(*TagToPointer(*(T))),utf_tag); */	   \
}


# define Forward(O,T)      						   \
{									   \
  w->stats->heap_gc_copied++;						   \
  (O) = (TAGGED) AddMarkF(T);						   \
}

# define GC_LOCKVALUE    GC_FWD_MASK

#ifdef PARALLEL 

# define LockCell(Cell,CellVal,FailCode)				   \
{									   \
  KSR1(DECLARE_SWAP_VAL);						   \
									   \
  CellVal = swap_il(&(Cell),GC_LOCKVALUE);				   \
  if (CellVal == GC_LOCKVALUE)						   \
    {									   \
      FailCode;								   \
    }									   \
  else if (IsCopied(CellVal))						   \
    {									   \
      Cell = CellVal;							   \
      FailCode;								   \
    }									   \
}


# define WaitUntilCopied(NEXT,NEW_LOC)					   \
{									   \
  volatile TAGGED *pos;							   \
  TAGGED wuc_new_loc;							   \
  int i;								   \
  pos = TagToPointer(NEXT);						   \
  while(!IsCopied(*pos)); 						   \
  while((wuc_new_loc = *pos) == (TAGGED) GC_LOCKVALUE);			   \
  NEW_LOC = GetNewLocation(wuc_new_loc);				   \
}

#else /* not PARALLEL */

# define LockCell(Cell,CellVal,FailCode)				   \
{									   \
  CellVal = Cell;							   \
}
# define WaitUntilCopied(NEXT,NEW_LOC) 					   \
    NEW_LOC = GetNewLocation(*TagToPointer(NEXT))

#endif /* PARALLEL */


/**********************************************************************
 * Load distribution support.
 * 
 * We use bounded buffers of a size 2^n for cheap modulo calculation.
 * 
 */


#if 1

#define InitMarkBuffer(W)						   \
{									   \
  AcquireLock(W->gc_info.load->lock);					   \
  W->gc_info.load->stop = 0;			                           \
  ReleaseLock(W->gc_info.load->lock);					   \
}

#define BufferFull(W)  ((W)->gc_info.load->stop >= LOADBUFFERSIZE)
#define BufferEmpty(W) ((W)->gc_info.load->stop == 0)

#define AddMarkBuffer(W,SIZE,ARG,AMB_CODE)				   \
({									   \
  BOOL amb_ret = TRUE;							   \
  s32 amb_index;							   \
									   \
  AcquireLock((W)->gc_info.load->lock);					   \
									   \
  /* Check if buffer is full */						   \
  if (!BufferFull(W))							   \
    {									   \
      /* Add new entry */						   \
      amb_index = (W)->gc_info.load->stop;				   \
      (W)->gc_info.load->buf[amb_index].size = SIZE;			   \
      (W)->gc_info.load->buf[amb_index].arg = ARG;			   \
      (W)->gc_info.load->stop += 1;					   \
      {									   \
         AMB_CODE;							   \
      }									   \
    }									   \
  else									   \
    {									   \
      amb_ret = FALSE;							   \
    }									   \
  									   \
  ReleaseLock((W)->gc_info.load->lock);					   \
									   \
  amb_ret;								   \
})

#define RemoveMarkBufferHead(W,SIZE,ARG)				   \
{									   \
  int rmb_index;							   \
									   \
  AcquireLock((W)->gc_info.load->lock);					   \
									   \
  if (!BufferEmpty(W))							   \
    {									   \
      (W)->gc_info.load->stop -= 1;					   \
      rmb_index = (W)->gc_info.load->stop;				   \
      SIZE = (W)->gc_info.load->buf[rmb_index].size;			   \
      ARG =  (W)->gc_info.load->buf[rmb_index].arg;			   \
    }									   \
  else									   \
    {									   \
      SIZE = 0;								   \
    }									   \
									   \
  ReleaseLock((W)->gc_info.load->lock);					   \
}

#define RemoveMarkBufferTail(W,SIZE,ARG) RemoveMarkBufferHead(W,SIZE,ARG)


/**********************************************************************
 * Sharing Marking work
 */

#if defined(SHARE_MARKING)
#if defined(SINGLE_LOAD_STACK) /* single stack */
#define SearchMarkWork(W)						   \
{									   \
  int    smw_size;							   \
  TAGGED *smw_arg;							   \
  BOOL   found_work = TRUE;						   \
									   \
  forever								   \
    {									   \
      if (!BufferEmpty(W))						   \
	{								   \
	  RemoveMarkBufferHead(W,smw_size,smw_arg);			   \
	  if (smw_size != 0)						   \
	    {								   \
	      gc_mark_var(W,smw_size,smw_arg);				   \
	    }								   \
	  continue;							   \
	}								   \
      else								   \
	break;								   \
    }									   \
}

#else /* multiple stacks */

#define SearchMarkWork(W)						   \
{									   \
  /* First, find a worker with something to work with */		   \
									   \
  BOOL   found_work;							   \
  int    smw_size;							   \
  TAGGED *smw_arg;							   \
  worker *smw_w = &((W)[0-((W)->pid)]);					   \
									   \
  do									   \
    {									   \
      int smw_i;							   \
									   \
      /* Try the own stack first */					   \
      if (!BufferEmpty(W))						   \
	{								   \
	  RemoveMarkBufferHead(w,smw_size,smw_arg);			   \
	  if (smw_size != 0)						   \
	    {								   \
	      found_work = TRUE;					   \
	      gc_mark_var(w,smw_size,smw_arg);				   \
	      continue;							   \
	    }								   \
	}								   \
									   \
      found_work=FALSE;							   \
									   \
      for(smw_i = 0 ; 							   \
	  smw_i <= (W)->global->active_workers  ;			   \
	  smw_i ++)							   \
	{								   \
	  if (!BufferEmpty(&(smw_w[smw_i])))				   \
	    {								   \
	      RemoveMarkBufferHead(&(smw_w[smw_i]),smw_size,smw_arg);	   \
	      if (smw_size != 0)					   \
		{							   \
                  (W)->gc_info.load->markdone = FALSE;			   \
		  found_work = TRUE;					   \
		  gc_mark_var(w,smw_size,smw_arg);			   \
                  break;                                                   \
		}							   \
	    }								   \
	  else if (smw_w[smw_i].gc_info.load->markdone == FALSE)	   \
	    {								   \
	       found_work = TRUE;					   \
	    }								   \
	}								   \
      (W)->gc_info.load->markdone = TRUE;				   \
    }									   \
  while (found_work != FALSE);						   \
}

#endif /* single stack */


#define Share(W)          ((depth >= PUSHDEPTH) && !BufferFull(W) &&       \
			   (ResetDepth,TRUE))
#define IncDepth          depth += 1
#define ResetDepth        depth = 0


#else /* no Shared marking */

#define SearchMarkWork(W)
#define Share(W)           FALSE
#define IncDepth          

#endif /* Shared marking */

#else /* no Sharing support */

# define MarkBufferFull(W) TRUE
# define BufferFull(W) TRUE
# define InitMarkBuffer(W)
# define AddMarkBuffer(W,SIZE,ARG,CODE) FALSE
# define RemoveMarkBufferHead(W,SIZE,ARG)
# define RemoveMarkBufferTail(W,SIZE,ARG)
# define SearchMarkWork(W)
#endif /* Sharing support */

#if defined(SHARE_COPYING) /* Share copying */
#if defined(SINGLE_LOAD_STACK)

# define SearchCopyWork(W)						   \
{									   \
  /* First, find a worker with something to work with */		   \
									   \
  int    scw_size;							   \
  TAGGED *scw_arg;							   \
									   \
  forever								   \
    {									   \
      if (!BufferEmpty(W))						   \
	{								   \
	  RemoveMarkBufferHead(W,scw_size,scw_arg);			   \
									   \
	  if (scw_size != 0)						   \
	    {								   \
	      TAGGED *scw_next, *scw_prev, scw_tag;			   \
									   \
	      /* fprintf(stderr,"%d: grabbing copy work\n",w->pid); */	   \
	      scw_prev = (TAGGED *) scw_arg;				   \
	      scw_next = TagToPointer(scw_size);			   \
	      scw_tag  = TagOf(scw_size);				   \
									   \
	      scan(scw_next,scw_prev,scw_tag,W);			   \
	    }								   \
	  continue;							   \
	}								   \
      else								   \
	break;								   \
    }									   \
}

#else /* several load stacks */

# define SearchCopyWork(W)						   \
{									   \
  /* First, find a worker with something to work with */		   \
									   \
  BOOL   found_work;							   \
  int    scw_size;							   \
  TAGGED *scw_arg;							   \
  worker *scw_w = &((W)[0-((W)->pid)]);					   \
									   \
  do									   \
    {									   \
      int scw_i;							   \
									   \
      if (!BufferEmpty(W))						   \
	{								   \
	  RemoveMarkBufferHead(W,scw_size,scw_arg);			   \
									   \
	  if (scw_size != 0)						   \
	    {								   \
	      TAGGED *scw_next, *scw_prev, scw_tag;			   \
									   \
              found_work=TRUE;                                             \
									   \
	      scw_prev = (TAGGED *) scw_arg;				   \
	      scw_next = TagToPointer(scw_size);			   \
	      scw_tag  = TagOf(scw_size);				   \
									   \
	      scan(scw_next,scw_prev,scw_tag,w);			   \
									   \
	      continue;							   \
	    }								   \
	}								   \
									   \
      found_work=FALSE;							   \
									   \
      for(scw_i = 0 ;							   \
	  scw_i <= (W)->global->active_workers ;			   \
	  scw_i ++)							   \
	{								   \
	  if (!BufferEmpty(&(scw_w[scw_i])))				   \
	    {								   \
	      RemoveMarkBufferHead(&(scw_w[scw_i]),scw_size,scw_arg);	   \
									   \
	      if (scw_size != 0)					   \
		{							   \
		  TAGGED *scw_next, *scw_prev, scw_tag;			   \
									   \
                  (W)->gc_info.load->copydone = FALSE;			   \
		  found_work = TRUE;					   \
									   \
                  /* fprintf(stderr,"%d: grabbing copy work\n",w->pid); */ \
		  scw_prev = (TAGGED *) scw_arg;			   \
		  scw_next = TagToPointer(scw_size);			   \
		  scw_tag  = TagOf(scw_size);				   \
									   \
		  scan(scw_next,scw_prev,scw_tag,w);			   \
		  break;                                                   \
 		}							   \
	    }								   \
	  else if (scw_w[scw_i].gc_info.load->copydone == FALSE)	   \
	    {								   \
	      found_work = TRUE;					   \
	    }								   \
	}								   \
      (W)->gc_info.load->copydone = TRUE;				   \
    }									   \
  while (found_work != FALSE);						   \
}

#endif /* SINGLE_LOAD_STACK */

# define AddCopyWork(W,PREV,COPY,CODE) AddMarkBuffer(W,COPY,PREV,CODE)

# define GoForward(w,copy,prev,next,tag)				   \
{									   \
  TAGGED gcbits;							   \
									   \
  depth += 1;								   \
									   \
  gcbits = GetGCBits(oldnext);						   \
									   \
  if (((depth == SPLITDEPTH) ? (depth=0,TRUE) : FALSE) &&		   \
      !BufferFull(w) && (prev != NULL) &&				   \
      AddCopyWork(w,prev,Tagify(next,tag),				   \
		  {							   \
		    tag = TagOf(oldnext);				   \
		    *next = SetGCBits(Tagify(copy,tag),gcbits);		   \
		  }))							   \
    {									   \
      prev = TagToPointer(NULL);				           \
      next = newnext;					                   \
    }									   \
  else									   \
    {									   \
      *next = SetGCBits(Tagify(prev,tag),gcbits);			   \
      tag = TagOf(oldnext);						   \
      prev  = next;							   \
      next  = newnext;							   \
    }									   \
}

# define DecDepth  /* depth -= 1 */

#else /* no Shared copying */

# define SearchCopyWork(W)
# define AddCopyWork(W,Y,Z,CODE)
# define GoForward(w,copy,prev,next,tag)				   \
{									   \
  TAGGED gcbits;							   \
									   \
  gcbits = GetGCBits(oldnext);						   \
									   \
  *next = SetGCBits(Tagify(prev,tag),gcbits);			           \
  tag = TagOf(oldnext);						           \
  prev  = next;							           \
  next  = newnext;							   \
}

# define DecDepth 

#endif /* Shared copying */
#endif /* NEW_COPY_GC_H */

