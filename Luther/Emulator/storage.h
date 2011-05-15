/*
 * storage.h
 *
 * Johan Bevemyr.....Sat May 25 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef STORAGE_H
#define STORAGE_H


#if defined(__GNUC__)
#  include <malloc.h>
#endif

#if defined(ALIGN_MALLOC)
#  define Malloc(bytes)		aligned_malloc(bytes)

PROTOTYPE(char*, aligned_malloc, (long));

#else  /* not ALIGN_MALLOC */
#  define Malloc(bytes)		malloc(bytes)
#endif


extern char *getenv();

#define GETENV(value, name, default)			\
{							\
  char *cp;						\
							\
  value = (cp = getenv(name)) ? atoi(cp) : default;	\
}


#define AlignSize(size, alignment) (((size) + alignment-1) & ~(alignment-1))

#define AlignDown(size, alignment) (((size) & ~(alignment-1)))


#define AlignAddress(addr, alignment) AlignSize((uword)addr, alignment)


#define SingleAlignedSize(size) (size = AlignSize(size, sizeof(uword)))

#define DoubleAlignedSize(size) (size = AlignSize(size, sizeof(double)))


#define SingleAlignedAddress(addr) ((uword)addr = AlignAddress(addr, sizeof(uword)))

#define DoubleAlignedAddress(addr) ((uword)addr = AlignAddress(addr, sizeof(double)))


#if defined(TIMESTAMP)

#define InitHVA(S,V,W)				\
{						\
  V = *(S) = (TAGGED) BaseOffset(S);		\
  *((S)+1) = (W)->time;				\
  HVAInitStat(W);				\
}

#define InitCVA(S,V,W)				\
{						\
  V = *(S) = Tagify(S,CVA);			\
  *((S)+1) = (W)->time;				\
  HVAInitStat(W);				\
}

#if defined(SHORT_SVA)
#  define LoadSVA(V,W)             { V = Tagify((TAGGED) &(V),SVA); }
#else
#  define LoadSVA(V,W)				\
{						\
  V = Tagify((TAGGED) &(V),SVA);		\
  *((&(V))+1) = (W)->time;			\
  SVAInitStat(W);				\
}
#endif

#else /* not TIMESTAMP */

#if defined(UNBOUND)

#  define InitHVA(S,V,W)			\
{						\
  *(S) = Tagify((W)->time,UVA);			\
  V = (TAGGED) BaseOffset(S);			\
  HVAInitStat(W);				\
}

#  define InitCVA(S,V,W)			\
{						\
  V = *(S) = Tagify(S,CVA);			\
  *((S)+1) = (W)->time;				\
  HVAInitStat(W);				\
}

#else
#  define InitHVA(S,V,W)	{ V = *(S) = (TAGGED) BaseOffset(S); }
#  define InitCVA(S,V,W)	{ V = *(S) = Tagify(S,CVA); }
#endif

#define LoadSVA(V,W)		{ V = Tagify((TAGGED) &(V),SVA); }

#endif /* TIMESTAMP */


/* */


#define LoadHVA(S,V,W)		{ InitHVA(S,V,W); (S) += VARSIZE; }

#define CreateCVA(S,To,Goal,W)			\
{						\
  InitCVA(S,To,W); (S)+=VARSIZE;		\
  PushOnHeap(S,Goal);				\
}

#if defined(UNBOUND)
#define CreateHVA(S,W)           { register TAGGED _x;	\
				   InitHVA(S,_x,W); (S) += VARSIZE; }
#else
#define CreateHVA(S,W)           { InitHVA(S,*S,W); (S) += VARSIZE; }
#endif

#define Make_LST(H,L)            { L = Tagify(H,LST); }

#define Make_LST_S(H,S,To)       { To = Tagify(H,LST);                    \
                                   S = H;                                 \
			           H += 2*VARSIZE; }

#define Make_STR(H,S,F)          { S = Tagify(H,STR); PushOnHeapF(H,F); }

#define Make_STR_Alloc(H,S,To,F) { To = Tagify(H,STR);                    \
			           PushOnHeapF(H,F);                      \
			           S = H;                                 \
			           H += ArityOf(F)*VARSIZE;               \
			         }
			   
#define Make_Vector_Head(H,To,Size)					   \
{									   \
  TAGGED Functor = StoreFunctor(atom_table_tagged[ATOM_D_ARRAY],Size*2);   \
  Make_STR(H,To,Functor);						   \
}

#define Patch_Vector_Size(Vector,Size)					   \
{									   \
  TAGGED *Vec = TagToPointer(Vector);					   \
  *Vec = StoreFunctor(atom_table_tagged[ATOM_D_ARRAY],Size*2);		   \
}


#define PopOnHeap(H,T)           { *(H) = (TAGGED)(T); (H) -= VARSIZE; }
#define PushOnHeap(H,T)          { *(H) = (TAGGED)(T); (H) += VARSIZE; }
#define PushOnHeap1(H,T)         { *(H) = (TAGGED)(T); (H) += 1; }

#define PopFromHeap(H,T)         { (H) -= VARSIZE; (T) = *(H); }

#define PushOnHeapF(H,T)         { *(H) = (TAGGED)(T); (H) += FUNCSIZE; }

#define IsInHeap(T)              ((TagToPointer(T) > w->heap_start) &&    \
		                  (TagToPointer(T) < w->heap_end))

/**********************************************************************
 * Stack initialization
 */

#if defined(TIMESTAMP) || defined(UNBOUND)

#define InitTime(W)				\
{						\
  InitTimeLite(W);				\
  W->choice->timestamp = W->uncond;		\
}

#define InitTimeLite(W)				\
{						\
  W->time = (W-W->pid)->time+TIMEUNIT;		\
  W->uncond = (W-W->pid)->uncond;		\
}

#else

#define InitTime(W)

#endif


#define StackAndTimeInit(W)			\
{						\
  LightStackInit(W);				\
						\
  /* current_module = module_user; */		\
						\
  InitTime(W);					\
}

#define LightStackInit(W)					\
{								\
  /* reinitialize stack */					\
								\
  W->choice = (choicepoint *) W->stack_start;			\
  W->choice->trail_top = W->trail_top;				\
  W->choice->global_top = W->heap_top;				\
  W->choice->last_choice = (choicepoint *) NULL;		\
  W->choice->generation = NEW_GEN;                              \
  W->choice->cont_env = (environment *) NULL;			\
  W->choice->arity = 0;						\
								\
  W->frame = (environment *) (((char *) (W->stack_start)) +	\
			sizeof(choicepoint) - sizeof(TAGGED));	\
  W->frame->cont_env = (environment *) NULL;			\
}


#if defined(SEGMENTED_MEMORY)

#if defined(GENERATIONAL)

#define BuiltinHeapCheck(AllocSize,FailCode)				   \
{									   \
  if (w->heap_end < (w->heap_top + (AllocSize)))			   \
    {									   \
      TAGGED seg;							   \
      seg = new_new_segment(w);						   \
									   \
      if (seg != (TAGGED) NULL)						   \
	{								   \
	  PushOnTrail(w, Tagify(seg,ATM));				   \
	  if (w->heap_margin < (w->heap_top + (AllocSize)))		   \
	    {								   \
	      Error("Structure to large");				   \
	      FailCode;							   \
	    }								   \
	}								   \
      else								   \
	{								   \
	  Error("Out of memory");					   \
	  FailCode;							   \
	}								   \
    }									   \
}

#define HeapCheck(AllocSize,FailCode)					   \
{									   \
  if (w->heap_end < (H + (AllocSize)))					   \
    {									   \
      TAGGED seg;							   \
      SaveCache(H);							   \
									   \
      seg = new_new_segment(w);						   \
      									   \
      LoadCache(H);							   \
									   \
      if (seg != (TAGGED) NULL)						   \
	{								   \
	  LoadCache(H);							   \
	  PushOnTrail(w, Tagify(seg,ATM));				   \
	  if (w->heap_margin < (H + (AllocSize)))			   \
	    {								   \
	      Error("Structure to large");				   \
	      FailCode;							   \
	    }								   \
	}								   \
      else								   \
	{								   \
	  Error("Out of memory");					   \
	  FailCode;							   \
	}								   \
    }									   \
}
#else /* not GENERATIONAL */
#define BuiltinHeapCheck(AllocSize,FailCode)			\
{								\
  if (w->heap_end < (w->heap_top + (AllocSize)))		\
    {								\
      if (w->global->free_seg > w->global->used_seg+1)		\
	{							\
	  TAGGED seg;						\
	  seg = new_segment(w);					\
	  PushOnTrail(w, Tagify(seg,ATM));			\
	  if (w->heap_margin < (w->heap_top + (AllocSize)))	\
	    {							\
	      Error("Structure to large");			\
	      FailCode;						\
	    }							\
	}							\
      else							\
	{							\
	  Error("Out of memory");				\
	  FailCode;						\
	}							\
    }								\
}

#define HeapCheck(AllocSize,FailCode)			\
{							\
  if (w->heap_end < (H + (AllocSize)))			\
    {							\
      if (w->global->free_seg > w->global->used_seg+1)	\
	{						\
	  TAGGED seg;					\
	  SaveCache(H);					\
	  seg = new_segment(w);				\
	  LoadCache(H);					\
	  PushOnTrail(w, Tagify(seg,ATM));		\
	  if (w->heap_margin < (H + (AllocSize)))	\
	    {						\
	      Error("Structure to large");		\
	      FailCode;					\
	    }						\
	}						\
      else						\
	{						\
	  Error("Out of memory");			\
	  FailCode;					\
	}						\
    }							\
}
#endif /* GENERATIONAL */
#else /* not SEGMENTED_MEMORY */

#define BuiltinHeapCheck(AllocSize,FailCode)		\
{							\
  if (w->heap_end < (w->heap_top + (AllocSize)))	\
    {							\
      Error("Out of memory");				\
      FailCode;						\
    }							\
}

#define HeapCheck(AllocSize,FailCode)		\
{						\
  if (w->heap_end < (H + (AllocSize)))		\
    {						\
      Error("Out of memory");			\
      FailCode;					\
    }						\
}

#endif /* SEGMENTED_MEMORY */


/**********************************************************************
 * functions declared in storage.c 
 */

extern void init_localstack PROTO((long, worker *));
extern void reset_backpatchheap PROTO((worker *));
extern heap *atom_alloc PROTO((unsigned long,worker *));
extern heap *aligned_atom_alloc PROTO((unsigned long,worker *));
extern heap *patch_alloc PROTO((unsigned long, worker *));
extern void init_temporary_registers PROTO((long, worker *));
extern worker *init_global_memory PROTO((int));
extern void init_local_memory PROTO((worker *));

static worker *init_worker_global_storage PROTO((char *, int));

extern TAGGED *mem_search PROTO((TAGGED, TAGGED *,TAGGED *));

#if defined(PARALLEL)
extern long worker_size;
#endif

extern long total_size;

extern long margin_size;

extern u32 get_usedheap PROTO((worker *));
extern u32 get_freeheap PROTO((worker *));
extern u32 get_totalheap PROTO((worker *));
extern void extend_heap PROTO((worker *));
extern void extend_codespace PROTO((worker *));


#endif /* STORAGE_H */
