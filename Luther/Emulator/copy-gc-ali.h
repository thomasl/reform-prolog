/*
 * copy-gc-ali.h
 *
 * Johan Bevemyr.....Fri Apr  7 1995
 *
 *
 * Implementation:
 *
 *   Based on the copying algorithm described by Kairi-Ali at SICS.
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

/* #define EARLY_RESET 1 */

#ifndef COPY_GC_ALI_H
#define COPY_GC_ALI_H

#if defined(COPY_GC) && defined(COPY_ALI)

/**********************************************************************/

typedef struct {
  integer trailcells_deleted;
  natural wake_count;
  TAGGED *last_copied_trail_entry;
  choicepoint *least_old;
} copy_state_t;

/**********************************************************************
 * Forwarding support 
 */

#define GetNewLocation(T)  ((TAGGED *) (T))
#define FromOld(T)         RemoveMark(T)
#define FromOldArg(T)      RemoveMarkFM(T)

#ifdef PARALLEL 

# define Forward(O,T)      (O) = (TAGGED) (T);

# define LockCell(Cell,CellVal,FailCode)				   \
{									   \
  KSR1(DECLARE_SWAP_VAL);						   \
									   \
  CellVal = swap_il(&(Cell),NULL);					   \
  if((CellVal == (TAGGED) NULL) || !IsMarked(CellVal))			   \
    {									   \
      FailCode;								   \
    }									   \
}

# define WaitUntilCopied(NEXT,NEW_LOC)					   \
{									   \
  volatile TAGGED *pos;							   \
  									   \
  pos = TagToPointer(NEXT);						   \
  while((NEW_LOC = *pos) == (TAGGED) NULL);				   \
}

#else /* not PARALLEL */

# define Forward(O,T)      (O) = (TAGGED) (T)

# define LockCell(Cell,CellVal,FailCode)				   \
{									   \
  CellVal = Cell;							   \
}
# define WaitUntilCopied(NEXT,NEW_LOC) NEW_LOC = *TagToPointer(NEXT)

#endif /* PARALLEL */

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
  (T) = (S) = w->heap_top;						   \
  (E) = w->heap_margin;							   \
  (Stack) = w->used_segments;						   \
}

#define CheckStackOverflow(T,S,E,Stack)					   \
{									   \
  if((T) > (E)) /* allocate new stack */				   \
    {									   \
      segment *new;							   \
      TAGGED *old_heap_top;						   \
									   \
      old_heap_top = w->heap_top;					   \
      w->heap_top = (T);       /* new segment push NULL on top */	   \
      new = (segment *) new_segment(w);					   \
									   \
      PushOnHeap(w->heap_top,old_heap_top);				   \
      PushOnHeap(w->heap_top,Stack);					   \
      PushOnHeap(w->heap_top,T);					   \
      PushOnHeap(w->heap_top,S);					   \
      PushOnHeap(w->heap_top,E);					   \
									   \
      Stack = new;							   \
      (T) = (S) = w->heap_top;						   \
      (E) = w->heap_margin;						   \
    }									   \
}

#define PopStack(T,S,E,Stack)						   \
{									   \
  segment *old;								   \
  TAGGED *new_heap_top;							   \
									   \
  old = Stack;								   \
									   \
  PopFromHeap(w->heap_top,(TAGGED)E);					   \
  PopFromHeap(w->heap_top,(TAGGED)S);					   \
  PopFromHeap(w->heap_top,(TAGGED)T);					   \
  PopFromHeap(w->heap_top,(TAGGED)Stack);				   \
  PopFromHeap(w->heap_top,(TAGGED)new_heap_top);		           \
									   \
  w->heap_top = new_heap_top;						   \
									   \
  free_segment(old,w);							   \
}

#define SavePosition(S,R,C)						   \
{									   \
  if(R != 0)								   \
    {									   \
      PushSize(S,R);							   \
      PushTAGGED(S,C);							   \
    }									   \
}

#define PushTAGGED(S,T)    {*(S) = (TAGGED) (T);S++;}
#define PushSize(S,T)      {*(S) = (TAGGED) (T);S++;}

#define PopTAGGED(S)       ((TAGGED *) *(--S))
#define PopSize(S)         ((natural) *(--S))

#define StackEmpty(S,E)    (S == E)
#define LastStack(T)       (*(T-1) == 0)

/**********************************************************************
 * Support for early reset (a trail related optimization).
 */

/* reset cell but keep GC-mark-bits (the cell is still in a live struct)
 */

#define ResetEntry(T)							   \
{									   \
  *(T) = 0;								   \
  (T) -= 1;								   \
  copy_state->trailcells_deleted += 1;					   \
}

#define ResetCell(V)							   \
{									   \
  register TAGGED rc_var;						   \
  register uword fmark;							   \
									   \
  rc_var = *(V);							   \
  fmark = GetMask(*TagToPointer(rc_var), GC_FWD_MASK);			   \
  *TagToPointer(rc_var) = SetMask((rc_var), fmark);			   \
									   \
  *(V) = 0;								   \
  V -= 1;								   \
  copy_state->trailcells_deleted += 1;					   \
}

#define CopyHVA(V)						           \
{									   \
  *(V) = copy_variable(*(V),w);						   \
  V -= TrailHVASize;							   \
}

#define CopyValueCell(V)						   \
{									   \
  TAGGED tag;								   \
  TAGGED *pos;								   \
									   \
  tag = TagOf(*(V));							   \
  pos = TagToPointer(*(V));						   \
									   \
  *(V) = Tagify(TagToPointer(copy_variable(Tagify(pos,HVA),w)),tag);	   \
									   \
  if(IsHeapTerm(*(V-1)) || IsHeapBox(*(V-1)))				   \
    {									   \
      *(V-1) = copy_variable(*(V-1),w);					   \
    }									   \
									   \
  SkipValueCell(V);							   \
}

#define ResetValueCell(V)						   \
{									   \
  register TAGGED rvc_var, rvc_val;					   \
  register uword fmark;							   \
									   \
  rvc_var = *(V);							   \
  rvc_val = *(V-1);							   \
									   \
  fmark = GetMask(*TagToPointer(rvc_var), GC_FWD_MASK);			   \
  *TagToPointer(rvc_var) = SetMask((rvc_val), fmark);			   \
									   \
  *(V) = *((V)-1) = 0;							   \
  V -= 2;								   \
  copy_state->trailcells_deleted += 2;					   \
}

#ifdef UNBOUND

#  define ResetHVA(T) ResetValueCell(T)
#  define TrailHVASize 2

#else /* not UNBOUND */

#  define ResetHVA(T) ResetCell(T)
#  define TrailHVASize 1

#endif /* UNBOUND */

#if not(defined(EARLY_RESET))

#define SkipValueCell(T)  (T) -= 2
#define SkipHVA(T)       (T) -= TrailHVASize
#define SkipSVA(T)       (T) -= 1
#define SkipEntry(T)     (T) -= 1

#endif /* EARLY_RESET */

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
      GetCopied(NEXT);							   \
      goto backward;							   \
    }									   \
}

#define GetCopied(NEXT)							   \
{									   \
  TAGGED bits;								   \
  TAGGED new_loc;                                                          \
									   \
  WaitUntilCopied(NEXT,new_loc);					   \
									   \
  bits = GetBits(NEXT);							   \
  NEXT = SetBits(GetNewLoc(new_loc),bits);				   \
}

#define CopyTerm(COPY_FUNC)						   \
{									   \
  TAGGED oldnext;							   \
  TAGGED *newnext;							   \
									   \
  oldnext = *next;							   \
  newnext = COPY_FUNC(*next, w);					   \
									   \
  if (newnext != NULL)							   \
    {									   \
      TAGGED gcbits;							   \
									   \
      gcbits = GetGCBits(oldnext);					   \
      *next = SetGCBits(Tagify(prev,tag),gcbits);			   \
									   \
      tag = TagOf(oldnext);						   \
      prev = next;							   \
									   \
      next = newnext;							   \
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
 * Generational GC macros
 **********************************************************************/

#ifdef GENERATIONAL

# define InOldGen(T) (TagToPointer(T) > w->global->new_gen_limit)
# define CopyHeapTop w->old_heap_top

#else  /* not GENERATIONAL */

# define InOldGen(T) FALSE
# define CopyHeapTop w->heap_top

#endif /* GENERATIONAL */

# define InNewGen(T) (!InOldGen(T))

/**********************************************************************
 * Misc support macros
 **********************************************************************/

#define IsCopied(T)        (!IsMarked(T))
#define IsCopiedCell(T)    (!IsMarked(*TagToPointer(T)))
#define GetNewLoc(T)       ((TAGGED) T)
#define InOldStruct(T)     (IsMarkedF(*T) || IsMarkedF(*(T+VARSIZE)))


/*
 * Public prototypes.
 *
 */
void live_mark_variable PROTO((TAGGED *, worker *));
TAGGED collect_variable PROTO((TAGGED, worker *));
TAGGED *gc_check_size PROTO((integer, worker *));


/*
 * Private prototypes
 *
 */
static void mark PROTO((worker *,copy_state_t *));
static void mark_variable PROTO((TAGGED *, worker *));
static void mark_registers PROTO((worker *));
static void mark_environments PROTO((environment *,worker *,int,copy_state_t *));
static void mark_choicepoints PROTO((worker *, copy_state_t *));
static TAGGED *mark_trail PROTO((worker *,TAGGED *,TAGGED *, copy_state_t *));

#ifdef GENERATIONAL
static void mark_compare_area PROTO((worker *, copy_state_t *));
static void copy_compare_area PROTO((worker *, copy_state_t *));
#endif

#if not(defined(EARLY_RESET))
static TAGGED *mark_trail_late  PROTO((worker *,TAGGED *,TAGGED *,copy_state_t *));
#endif

static void copy PROTO((worker *, copy_state_t *));
static TAGGED copy_variable PROTO((TAGGED, worker *));
static void copy_registers PROTO((worker *));
static void copy_environments PROTO((environment *, worker *, int, copy_state_t *));
static void copy_choicepoints PROTO((worker *, copy_state_t *));
static TAGGED *copy_trail PROTO((worker *,TAGGED *, TAGGED *, copy_state_t *));
static void update_choicepoints PROTO((worker *, copy_state_t *));

static TAGGED scan PROTO((TAGGED *, TAGGED, worker *));
static TAGGED *check_size PROTO((integer, worker *));
static TAGGED *copy_term PROTO((TAGGED, worker *));
static TAGGED *copy_hva PROTO((TAGGED, worker *));
static TAGGED *copy_cva PROTO((TAGGED, worker *));
static TAGGED *copy_box PROTO((TAGGED, worker *));
static TAGGED *copy_list PROTO((TAGGED, worker *));
static TAGGED *copy_struct PROTO((TAGGED, worker *));

static void mark_copy PROTO((worker *, copy_state_t *));

#ifdef PARALLEL

static void worker_finish PROTO((worker *));
static void worker_mark_rest_trail PROTO((worker *, copy_state_t *));
static void worker_copy_rest_trail PROTO((worker *, copy_state_t *));
static void worker_update_choicepoints PROTO((worker *, copy_state_t *));

#endif /* PARALLEL */

#endif /* defined(COPY_GC) && defined(COPY_ALI) */
#endif /* not COPY_GC_ALI_H */
