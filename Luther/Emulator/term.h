/*
 * term.h  
 *
 * Johan Bevemyr.....Sat May 25 1991
 * Patric Hedlin.....Fri Jan 13 1995
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef TERM_H
#define TERM_H

/*******************************************************************************
 *
 * Tagged objects.
 *
 */
typedef uword TAGGED;
typedef uword UNTAGGED;

typedef enum {
    HVA = 0, 
#if defined(CONSTR)
    CVA,
#endif /* CONSTR */    
    SVA,     
    NUM,     
    ATM,     
    BOX,     
    GEN,
    LST,     
    STR,     
#if defined(UNBOUND)
    UVA,
#endif /* UNBOUND */
    MAX_TAG
} tagvalue;


typedef enum {
    FLT = 0,
    BIG = 1
} boxvalue;


#if ! defined(BITS_PER_BYTE)
#  define BITS_PER_BYTE		8
#endif

#if ! defined(BITS_PER_WORD)
#  define BITS_PER_WORD		BITS_PER_BYTE * SIZEOF_UNSIGNED_LONG_INT 
#endif

#if ! defined(BYTES_PER_WORD)
#  define BYTES_PER_WORD	(BITS_PER_WORD / BITS_PER_BYTE)
#endif

#if defined(LOWTAGS)
#  define FUNCSIZE	2
#  define VARSIZE	2
#  define SVASIZE	2
#  define ARSIZE	2
#else /* if ! defined(LOWTAGS) -> HIGHTAGS */
#  define FUNCSIZE	1 
#  define VARSIZE	1
#  define SVASIZE	1
#  define ARSIZE	1
#endif

#if defined(CONSTR) || defined(UNBOUND)
#  define TAGBITS	4
#else
#  define TAGBITS	3
#endif /* (CONSTR | UNBOUND) */


#define WORDSIZE	BITS_PER_WORD
#define MARKBITS	2

#define TOPBIT          (0x1L << WORDSIZE-1)


#if defined(LOWTAGS)

#  define MARKOFFSET		(WORDSIZE - MARKBITS)
#  define TAGOFFSET		0
#  define TAGMASK		((0x1L << TAGBITS) - 1)

#  define P_TagOf(T)		(((TAGGED)(T)) & TAGMASK)
#  define TagToPointer(T)	((TAGGED *) OffsetBase((((s32) (T)) | TAGMASK) - TAGMASK))
#  define Tagify(P,T)		((TAGGED) (BaseOffset((u32) (P)) | ((u32) (T))))
#  define RemoveTag(P,T)	((TAGGED *) OffsetBase((((s32) (P))-(T))))

#else /* Not defined(LOWTAGS) -> HIGHTAGS */

#  define MARKOFFSET		0
#  define TAGOFFSET		(WORDSIZE - TAGBITS)
#  define TAGMASK		((u32) (((s32) TOPBIT) >> (TAGBITS-1)))

#  define P_TagOf(T)		(((TAGGED)(T)) >> TAGOFFSET)
#  define TagToPointer(T)	((TAGGED *) OffsetBase(((u32) POINTERMASK)&((u32) (T))))
#  define Tagify(P,T)		((TAGGED) (BaseOffset((u32) (P)) | (((u32) (T)) << TAGOFFSET)))
#  define RemoveTag(P,T)	TagToPointer(P)

#endif


#define TagOf(T)		((tagvalue) P_TagOf(T))
#define BoxTagOf(T)		((boxvalue) S_TagOf(*TagToPointer(T)))

#define SetTag(P,T)		Tagify(((u32) (P) & ~TAGMASK), T)

#define PointerToTerm(P)	Tagify(P, NUM)
#define TermToPointer(T)	RemoveTag(T, NUM)


/* */

#define SetMask(word, mask)	(((uword) word) |  mask)

#define GetMask(word, mask)	(((uword) word) &  mask)

#define ClrMask(word, mask)	(((uword) word) & ~mask)


/* We use two mark bits for liveness and (forward) linking purposes.
 */
#define LIVEMARKMASK	(0x1L << MARKOFFSET) /* 0000..00001 (using low marks) */
#define LINKMARKMASK	(0x2L << MARKOFFSET) /* 0000..00010 (using low marks) */
#define FULLMARKMASK	(0x3L << MARKOFFSET) /* 0000..00011 (using low marks) */

#define SetLiveMark(term)	SetMask(term, LIVEMARKMASK)
#define SetLinkMark(term)	SetMask(term, LINKMARKMASK)
#define SetFullMark(term)	SetMask(term, FULLMARKMASK)

#define GetLiveMark(term)	GetMask(term, LIVEMARKMASK)
#define GetLinkMark(term)	GetMask(term, LINKMARKMASK)
#define GetFullMark(term)	GetMask(term, FULLMARKMASK)

#define ClrLiveMark(term)	ClrMask(term, LIVEMARKMASK)
#define ClrLinkMark(term)	ClrMask(term, LINKMARKMASK)
#define ClrFullMark(term)	ClrMask(term, FULLMARKMASK)

#define HasLiveMark(term)	(GetLiveMark(term) == LIVEMARKMASK)
#define HasLinkMark(term)	(GetLinkMark(term) == LINKMARKMASK)
#define HasFullMark(term)	(GetFullMark(term) == FULLMARKMASK)
#define HasOnlyLinkMark(term)   (GetFullMark(term) == LINKMARKMASK)

#define POINTERMASK	(~(TAGMASK | FULLMARKMASK))


#if defined(MALLOC_BASE)
#  define OffsetBase(T)	(((uword) (T)) + ((uword) MALLOC_BASE))
#  define BaseOffset(T)	(((uword) (T)) & POINTERMASK)
#else
#  define OffsetBase(T)	(T)
#  define BaseOffset(T)	(T)
#endif


#define MakeHVA(addr)	Tagify((TAGGED) addr, HVA)
#define MakeCVA(addr)	Tagify((TAGGED) addr, CVA)
#define MakeSVA(addr)	Tagify((TAGGED) addr, SVA)


#define IsHVA(V)	(TagOf(V) == HVA)
#define IsCVA(V)	(TagOf(V) == CVA)
#define IsSVA(V)	(TagOf(V) == SVA)
#define IsLCK(V)	(TagOf(V) == LCK)
#define IsNUM(V)	(TagOf(V) == NUM)
#define IsBOX(V)	(TagOf(V) == BOX)
#define IsATM(V)	(TagOf(V) == ATM)
#define IsLST(V)	(TagOf(V) == LST)
#define IsSTR(V)	(TagOf(V) == STR)
#define IsGEN(V)	(TagOf(V) == GEN)
#define IsUVA(V)	(TagOf(V) == UVA)

#define IsFLT(V)         (IsBOX(V) && BoxTagOf(V) == FLT)
#define IsBIG(V)         (IsBOX(V) && BoxTagOf(V) == BIG)
                         
#define IsNonVar(V)      (TagOf(V) > SVA)
#define IsHVAorCVA(V)    (TagOf(V) < SVA)

#define HVA_CASE(X)      case HVA: X
#define SVA_CASE(X)      case SVA: X
#define NUM_CASE(X)      case NUM: X
#define BOX_CASE(X)      case BOX: X
#define ATM_CASE(X)      case ATM: X
#define LST_CASE(X)      case LST: X
#define STR_CASE(X)      case STR: X
#define GEN_CASE(X)      case GEN: X


#if defined(CONSTR)
#  define IsCondCVA(X)   (GetCVATime(X) < w->uncond)
#  define CVA_CASE(X)    case CVA: X
#else
#  define CVA_CASE(X)  
#endif


#define Switch_Tag(X,H,C,S,N,F,A,L,T,G,B) \
{                                         \
  switch(TagOf(X))                        \
    {                                     \
      HVA_CASE(H);                        \
      CVA_CASE(C);                        \
      SVA_CASE(S);                        \
      NUM_CASE(N);                        \
      BOX_CASE(F);                        \
      ATM_CASE(A);                        \
      LST_CASE(L);                        \
      STR_CASE(T);                        \
      GEN_CASE(G);                        \
    default:                              \
      B;                                  \
    }                                     \
}

#define IsVar(V)	(TagOf(V) < NUM)
#define IsUnbound(V)	(V == *TagToPointer(V))

#define IsNumber(A)	(IsNUM(A) || IsBIG(A) || IsFLT(A))
#define IsCompound(A)	(IsLST(A) || IsSTR(A))


/*******************************************************************************
 *
 *   Variables (nonstack) are heap terms, so are lists and structures. However,
 * generic objects are not treated as heap objects. Instead generic objects are
 * treated as opaque objects (like atoms and boxes) even though they may contain
 * heap objects them self.
 *
 */

#if defined(UNBOUND)
#  define IsHeapTerm(V)   ((TagOf(V) < SVA) || ((TagOf(V) > GEN) && (TagOf(V) != UVA)))
#  define IsHeapObj(V)    ((TagOf(V) < SVA) || ((TagOf(V) > ATM) && (TagOf(V) != UVA)))
#else
#  define IsHeapTerm(V)   ((TagOf(V) < SVA) || (TagOf(V) > GEN))
#  define IsHeapObj(V)    ((TagOf(V) < SVA) || (TagOf(V) > ATM))
#endif


#define IsHeapBox(V)     (IsBOX(V) && IsDynBox(*TagToPointer(V)))
#define IsHeapBoxM(V)    (IsBOX(V) && IsMarked(*TagToPointer(V)))
                         
#define Var(V)           ((TAGGED *) (V))
#define Atom(T)          ((atom) (T))
#define Choice(T)        ((choicepoint *) (T))
#define Generic(G)       ((generic *) (G))
                         
#define GetCar(L)        RemoveTag(L,LST)
#define GetCdr(L)        (RemoveTag(L,LST)+VARSIZE)
                         

#if defined(TIMESTAMP)
#  undef VARSIZE
#  undef FUNCSIZE

#  define VARSIZE          2
#  define FUNCSIZE         2
#  define TIMEUNIT         1

#  define GetHVATime(T)    ((s32) *(((TAGGED *) OffsetBase(T))+1))
#  define GetCVATime(T)    ((s32) *(RemoveTag(T,CVA)+1))
#  define GetHVATimeDer(T) GetHVATime(T)

#  if defined(SHORT_SVA)
#    define GetSVATime(T)  RemoveTag(T,SVA)
#  else
#    undef SVASIZE
#    define SVASIZE        2
#    define GetSVATime(T)  ((s32) *(RemoveTag(T,SVA)+1))
#  endif

#else  /* Not defined(TIMESTAMP) */

#  if defined(UNBOUND)
#    define TIMEUNIT       4
#    define GetHVATime(T)     ((s32) (BaseOffset(RemoveTag(T,UVA))))
#    define GetCVATime(T)     ((s32) (BaseOffset(RemoveTag(T,CVA))))
#    define GetHVATimeDer(T)  GetHVATime(*((TAGGED *) OffsetBase(T)))
#  else
#    define GetHVATime(T)     ((TAGGED *) OffsetBase(T))
#    define GetCVATime(T)     RemoveTag(T,CVA)
#    define GetHVATimeDer(T)  GetHVATime(T)
#  endif

#  define GetSVATime(T)    RemoveTag(T,SVA)

#endif /* TIMESTAMP */

#if defined(TIMESTAMP) || defined(UNBOUND)
# define TIMETYPE s32
#else
# define TIMETYPE TAGGED *
#endif


#if defined(UNBOUND)
#  define Ref(X)           ((TAGGED) BaseOffset(X))
#  define DRef(X)          (IsUVA(*(X)) ? Ref(X):*(X))
#else
#  define Ref(X)           *(X)
#endif


/*******************************************************************************
 *
 * Constrained variables.
 * 
 *       ---------
 *  n:  |  CVA n  |
 *      |---------|
 *      |  Goals  |-> List of goals to wake when CVA is bound
 *       ---------
 *
 */
#define GetCVAGoals(V) *(RemoveTag(V,CVA)+VARSIZE)

#define WakeCVA					\
{						\
  if (w->wake_count == 0)			\
    {						\
      AddEvent(EVENT_WAKE);			\
    }						\
  w->wake_count++;				\
}


/*******************************************************************************
 *
 * Recursion levels.
 *
 */
#define IsLeftmost(F,W)				\
{						\
  if (W->direction == LEFT_BODY)		\
    {						\
      IsFirstLeft(F,W);				\
    }						\
  else						\
    {						\
      IsFirstRight(F,W);			\
    }						\
}
  
#define IsFirstLeft(F,W)					\
{								\
  register int if_i;						\
  register s32 if_lev = w->global->level[w->pid-1];		\
  F = TRUE;							\
  for (if_i = 0; if_i < w->global->active_workers; if_i++)	\
    if (w->global->level[if_i] < if_lev)			\
      {								\
	F = FALSE;						\
	break;							\
      }								\
}

#define IsFirstRight(F,W)					\
{								\
  register int if_i;						\
  register s32 if_lev = w->global->level[w->pid-1];		\
  F = TRUE;							\
  for (if_i = 0; if_i < w->global->active_workers; if_i++)	\
    if (w->global->level[if_i] > if_lev)			\
      {								\
	F = FALSE;						\
	break;							\
      }								\
}


#define IsNonVarOrLeftmost(TERM,W,FAIL_CODE)				\
{									\
  TAGGED invol_term = (TERM);						\
									\
  if (IsHVA(invol_term))						\
    {									\
      register BOOL is_first;	/* Wait until first or bound. */	\
									\
      AwaitStat(W);							\
									\
      forever								\
	{								\
	  IsLeftmost(is_first, W);					\
									\
	  DerefNLL(invol_term, invol_term);				\
									\
	  if ((!IsHVA(invol_term)) || is_first)				\
	    break;							\
									\
	  if (EventTrue(W->global->event_flag, GLOBAL_EVENT_FAIL))	\
	    {								\
	      FAIL_CODE;						\
	    }								\
									\
	  AwaitCountStat(W);						\
	}								\
    }									\
  TERM = invol_term;							\
}


/*******************************************************************************
 *
 * Atoms
 *
 */
typedef struct atom_bucket
{
  TAGGED a;
  struct atom_bucket *next;
} atom_bucket;

struct atom_s
{
  TAGGED mode;
  char *pname;
};

typedef struct atom_s *atom;

#define GetAtomMode(A)    get_mode((atom)RemoveTag(A,ATM),w)
#define GetString(A,W)    get_string((atom)RemoveTag(A,ATM),W)

#define AtomHasSpecial(T) (GetAtomMode(T) == Make_Integer(1))
#define AtomHasSquote(T)  (GetAtomMode(T) == Make_Integer(1))

#define OffsetAtom(A)     Atom(((s32)(A))+((s32) w->global->atomoffset))
#define DeOffsetAtom(A)   Atom(((s32)(A))-((s32) w->global->atomoffset))


/*******************************************************************************
 *
 * Functors
 *
 */
typedef struct functor_bucket
{
  TAGGED name, arity;
  struct functor_bucket *next;
} functor_bucket;

#define Func(T)              ((functor_bucket *) (T))

#define ArityOf(F)           GetNumber(Func(TermToPointer(F))->arity)
#define HashOf(F)            (Func(TermToPointer(F))->hash_value)

#define FunctorToAtom(F)     (Func(TermToPointer(F))->name)
#define StoreFunctor(F,A)    store_functor(F,Make_Integer(A),w)
#define StoreFunctorW(F,A,W) store_functor(F,Make_Integer(A),W)


/*******************************************************************************
 *
 * Structures
 *
 */
struct structure_s
{
  TAGGED functor;
#if (FUNCSIZE == 2)
  TAGGED dummy;
#endif
  TAGGED arg[ANY];
};

typedef struct structure_s *structure;

#define Struct(T)       ((structure) (T))

#define StructArity(S)  ArityOf(Struct(S)->functor)
#define StructString(S) GetString(FunctorToAtom(GetFunctor(S)),w)

#define GetArg(S,I)     GetUntArg(RemoveTag(S,STR),I)
#define GetUntArg(S,I)  (&(Struct(S)->arg[(I)*VARSIZE]))
#define GetFunctor(S)   (Struct(RemoveTag(S,STR))->functor)
#define GetArity(S)     (ArityOf(GetFunctor(S)))
#define GetSTRatom(S)   FunctorToAtom(GetFunctor(S))

#define GetNthHead(V,I) (RemoveTag(V,STR)+VARSIZE+2*I*VARSIZE)
#define GetNthTail(V,I) (RemoveTag(V,STR)+VARSIZE+2*I*VARSIZE+VARSIZE)


/*******************************************************************************
 *
 * Integers
 *
 */
#if defined(LOWTAGS)

#  define TOPMASK         (~FULLMARKMASK)
#  define GetNumber(N)    ((sword) (((sword) ((N)<<MARKBITS))>>(TAGBITS+MARKBITS)))
#  define Make_Integer(N) Tagify(ClearTop(((u32) (N)) << TAGBITS), NUM)

#else /* Not defined(LOWTAGS) -> HIGHTAGS */

#  define TOPMASK         (~TAGMASK)
#  define GetNumber(N)    ((sword) (((sword)((N)<<TAGBITS))>>(TAGBITS+MARKBITS)))
#  define Make_Integer(N) Tagify(ClearTop(((u32) (N)) << MARKBITS), NUM)

#endif /* LOWTAGS */


#define INTSIZE		(WORDSIZE - (TAGBITS + MARKBITS))

#define MAX_NUM		((0x1L << (INTSIZE - 2)) - 1)
#define MIN_NUM		-(MAX_NUM + 1)

#define Is_Integer(x)	(MAX_NUM >= x && x >= MIN_NUM)
#define MakeNumber(w,x)	(Is_Integer(x) ? Make_Integer(x) : make_bignum_of_int(w,x))

#define ClearTop(N)     (((u32) (N)) & TOPMASK)

#define Make_Atomspace_Integer(w,str) make_atomspace_number_of_string(w,str,DECIMAL)


/*******************************************************************************
 *
 * Boxed objects (layout showing high tagged words).
 *
 *   TAGGED (start marker)   [ATM TAG][BOX TAG][SIZE][SEC TAG][DYN TAG][GC]
 *    ...
 *   opaque
 *    ...
 *   TAGGED (end marker)     [ATM TAG][BOX TAG][SIZE][SEC TAG][DYN TAG][GC]
 *
 *                                     BOX TAG = 10
 *
 *   Observe that only the 1 is checked in order to decide that it is a
 *   box. The 0 is assumed to be present (in other words only 00, 10 and 01 
 *   are legal values of the BOX TAG.
 * 
 *   BOX TAG = 00  (regular atom)
 *             10  (boxed object)
 *             01  (variable ordering value)
 */

#define BOXBITS 2
#define DYNBITS 1
#define SECBITS 1

#if defined(LOWTAGS)

#  define BOXMASK	(((u32) TOPBIT) >> MARKBITS)
#  define DYNMASK	(((u32) 0x1L) << TAGBITS)
#  define SECMASK	(((u32) 0x1L) << (DYNBITS + TAGBITS))
#  define S_TagOf(box)	((box & SECMASK) >> (DYNBITS + TAGBITS))
#  define GetBoxSize(T)	((((u32) (T)) << (MARKBITS + BOXBITS)) >> \
				(MARKBITS + BOXBITS + SECBITS + DYNBITS + TAGBITS))
#  define MakeBoxSize(S,TAG)	Tagify((((((u32) (S)) << SECBITS) | TAG) << (DYNBITS + TAGBITS)), ATM)

#else /* Not defined(LOWTAGS) -> HIGHTAGS */

#  define BOXMASK	(((u32) TOPBIT) >> TAGBITS)
#  define DYNMASK	(((u32) 0x1L) << MARKBITS)
#  define SECMASK	(((u32) 0x1L) << (DYNBITS + MARKBITS))
#  define S_TagOf(box)	((box & SECMASK) >> (DYNBITS + MARKBITS))
#  define GetBoxSize(T)	((((u32) (T)) << (TAGBITS + BOXBITS)) >> \
				(TAGBITS + BOXBITS + SECBITS + DYNBITS + MARKBITS))
#  define MakeBoxSize(S,TAG)	Tagify((((((u32) (S)) << SECBITS) | TAG) << (DYNBITS + MARKBITS)), ATM)
#endif


#define IsBox(T)		(IsATM(T) && (((u32) (T)) & BOXMASK))

#define IsDynBox(T)		(!IsStatBox(T))
#define IsStatBox(T)		(IsBox(T) && (((u32) (T)) & DYNMASK))
#define MakeDynBoxSize(S,TAG)	(MakeBoxSize(S,TAG) | BOXMASK)
#define MakeStatBoxSize(S,TAG)	(MakeBoxSize(S,TAG) | DYNMASK | BOXMASK)

#define BOX_FRAME_SIZE	2
#define BOX_OBJ_OFFSET	1

/*******************************************************************************
 *
 * An ordered ATM.
 *
 *   This object is put after an ordered HVA. It contains the ordering number. 
 *
 *          [ATM][BOX TAG][ORDERNR][GC]
 *
 *          BOX TAG = 01
 *
 * Note: These definitions are only relevant when a copying garbage collector
 *       is used.
 */

#define ORDEREDTAG      (((u32) TOPBIT) >> (TAGBITS+1))

#if 1
#define Next(T)         *(RemoveTag(T,HVA)-VARSIZE)
#else
#define Next(T)         *(RemoveTag(T,HVA)+VARSIZE)
#endif

#define IsOrdered(T)    (IsATM(Next(T)) && (Next(T) & ORDEREDTAG))

#define GetOrderNr(T)   ((((uword) Next(T)) << (TAGBITS + BOXBITS)) >>     \
			 (TAGBITS + BOXBITS + MARKBITS))

#define MakeOrderNr(Nr) (Tagify((((uword) (Nr))<<MARKBITS),ATM) | ORDEREDTAG)

#if defined(UNBOUND) || defined(TIMESTAMP)

# ifdef GENERATIONAL
#  define Order(T)							   \
{									   \
  TAGGED *pocterm = (TAGGED *) OffsetBase(T);				   \
  TAGGED *poctop = w->global->compare_top;				   \
  uword ordernr;							   \
									   \
  if (poctop+3*VARSIZE > w->global->compare_end)			   \
    {									   \
      *poctop = (TAGGED) NULL;						   \
      allocate_static_segment(w);					   \
      poctop = w->global->compare_top;					   \
    }									   \
									   \
  NewOrderNr(ordernr);							   \
									   \
  *(poctop) = (T);							   \
									   \
  *(poctop+VARSIZE) = MakeOrderNr(ordernr);				   \
									   \
  *(poctop+2*VARSIZE) = *pocterm;					   \
									   \
  if(VARSIZE==2)							   \
    *(poctop+2*VARSIZE+1) = *(pocterm+1);				   \
									   \
  *pocterm = Tagify((poctop+2*VARSIZE),HVA);				   \
									   \
  w->global->compare_top = poctop + 3*VARSIZE;				   \
}
# else /* not GENERATIONAL */
#  define Order(T)							   \
{									   \
  TAGGED *pocterm = (TAGGED *) OffsetBase(T);				   \
  TAGGED *poctop = w->global->compare_top;				   \
  uword ordernr;							   \
									   \
  if (poctop+2*VARSIZE > w->global->compare_end)			   \
    {									   \
      allocate_static_segment(w);					   \
      poctop = w->global->compare_top;					   \
    }									   \
									   \
  NewOrderNr(ordernr);							   \
									   \
  *(poctop) = MakeOrderNr(ordernr);					   \
									   \
  *(poctop+VARSIZE) = *pocterm;						   \
									   \
  if(VARSIZE==2)							   \
    *(poctop+VARSIZE+1) = *(pocterm+1);					   \
									   \
  *pocterm = Tagify((poctop+VARSIZE),HVA);				   \
									   \
  w->global->compare_top = poctop + 2*VARSIZE;				   \
}
# endif /* GENERATIONAL */
#else

#define Order(T)							   \
{									   \
  TAGGED *pocterm = (TAGGED *) OffsetBase(T);				   \
  TAGGED *poctop = w->global->compare_top;				   \
  uword ordernr;							   \
									   \
  if (poctop+2*VARSIZE > w->global->compare_end)			   \
    {									   \
      allocate_static_segment(w);					   \
      poctop = w->global->compare_top;					   \
    }									   \
									   \
  NewOrderNr(ordernr);							   \
									   \
  *(poctop) = MakeOrderNr(ordernr);					   \
									   \
  *(poctop+VARSIZE) = Tagify((poctop+VARSIZE),HVA);			   \
									   \
  *pocterm = Tagify((poctop+VARSIZE),HVA);				   \
									   \
  if(VARSIZE==2)							   \
    *(poctop+1+VARSIZE) = *(pocterm+1+VARSIZE);				   \
									   \
  w->global->compare_top = poctop + 2*VARSIZE;				   \
}
#endif /* UNBOUND */

/* This is potentially unsafe:
 * In parallel we need exclusive access to the ordrnr counter.
 * Also, it is theoretically possible that the counter overflows.
 */

#define NewOrderNr(O)							   \
{									   \
  O = w->global->ordernr;						   \
  w->global->ordernr++;							   \
}



/**********************************************************************
 *
 * Floating point numbers.
 *
 *  TAGGED (start marker)   [ATM TAG][BOX TAG][SIZE][SEC TAG][DYN TAG][GC]
 *  double (part 1)
 *  double (part 2)
 *  TAGGED (end marker)     [ATM TAG][BOX TAG][SIZE][SEC TAG][DYN TAG][GC]
 *
 *  with SEC TAG = FLT
 *
 */
#define FLOATSIZE	sizeof(double)

#if ALIGNED_BOXES
#define GetFloat(t)	*((double *) (RemoveTag(t,BOX)+VARSIZE))
#else
#define GetFloat(t)	get_float(t)
#endif

#define MakeFloatHeapBoxMarker(Size)	MakeDynBoxSize(Size, FLT)
#define MakeFloatAtomBoxMarker(Size)	MakeStatBoxSize(Size, FLT)

#if defined(IEEE_FLOATING_POINT)
#  define MakeInf(w)		make_float(1.0/0.0, w)
#  define MakeNan(w)		make_float(0.0/0.0, w)

#  define MakeConstInf(w)	make_atomspace_float(1.0/0.0, w)
#  define MakeConstNan(w)	make_atomspace_float(0.0/0.0, w)

#else /* not IEEE_FLOATING_POINT - these need adjustment to each platform. */

#  define MakeInf(w)		make_float(DBL_MAX + 1.0, w)
#  define MakeNan(w)		make_float(-0.0, w)

#  define MakeConstInf(w)	make_atomspace_float(DBL_MAX + 1.0, w)
#  define MakeConstNan(w)	make_atomspace_float(-0.0, w)
#endif

#if defined ALIGNED_BOXES
#define make_float(value,w)						   \
({									   \
  double mf_value = (value);						   \
									   \
  int size = sizeof(double)/sizeof(TAGGED);				   \
									   \
  register TAGGED *flt = (TAGGED *) &mf_value;				   \
  register TAGGED *res = (TAGGED *) w->heap_top;			   \
  TAGGED box_mark = MakeFloatHeapBoxMarker(size/VARSIZE + 2);		   \
									   \
  res += 1;								   \
  DoubleAlignedAddress(res);						   \
  res -= 1;								   \
									   \
  res[0] = res[size+VARSIZE] = box_mark;				   \
  									   \
  w->heap_top += size+2*VARSIZE;					   \
									   \
  if(size == 2)								   \
    {									   \
      res[VARSIZE] = flt[0];						   \
      res[VARSIZE+1] = flt[1];						   \
    }									   \
  else									   \
    while (size--)							   \
      {									   \
	res[size+VARSIZE] = flt[size];					   \
      }									   \
									   \
  Tagify(res, BOX);							   \
})

#endif


/**********************************************************************
 *
 * Big (integer) numbers.
 *
 *  TAGGED (start marker)   [ATM TAG][BOX TAG][SIZE][SEC TAG][DYN TAG][GC]
 *  big number structure (of variable size)
 *  TAGGED (end marker)     [ATM TAG][BOX TAG][SIZE][SEC TAG][DYN TAG][GC]
 *
 *  with SEC TAG = BIG
 *
 */
#if defined(DEBUG)
#  define GetBignum(t)	get_bignum(t)
#else
#  define GetBignum(t)	((BigNumber*)(RemoveTag(t, BOX) + BOX_OBJ_OFFSET))
#endif

#define MakeBignumHeapBoxMarker(Size)	MakeDynBoxSize(Size, BIG)
#define MakeBignumAtomBoxMarker(Size)	MakeStatBoxSize(Size, BIG)

#define NUM2BIG(w,t)	make_bignum_of_int(w,GetNumber(t))
#define FLT2BIG(w,t)	make_bignum_of_int(w,(integer)GetFloat(t))

#define LONG_INT_WORDS	sizeof(TAGGED)/sizeof(long int)


/*******************************************************************************
 *
 * Additional type declarations.
 *
 */
typedef sword code;
typedef sword indx;


struct list_s
{
  TAGGED car;
  TAGGED cdr;
};

typedef struct list_s *list;

#define LastTail(Tail,Head)			\
{						\
  register TAGGED Cdr = (Head);			\
						\
  forever					\
    {						\
      DerefNLL(Cdr,Cdr);			\
      if (IsLST(Cdr))				\
	{					\
	  Cdr = Ref(GetCdr(Cdr));		\
	  continue;				\
	}					\
      else					\
	break;					\
    }						\
						\
  Tail = Cdr;					\
}


/*******************************************************************************
 *
 * Worker support.
 *
 */

#include "worker.h"


/*******************************************************************************
 *
 * Generic objects.
 *
 */
#define GetGenTag(G)   (Generic(RemoveTag(G,GEN))->method)
#define GetMethod(M,G) (Generic(RemoveTag(G,GEN))->method->M)

typedef struct {
  PROTOTYPE(BOOL,        (*unify),   (TAGGED this, TAGGED that, worker *));
  PROTOTYPE(void,        (*print),   (TAGGED this, FILE *, worker *));
  PROTOTYPE(void,        (*undo),    (TAGGED this));
  PROTOTYPE(TAGGED,      (*copy),    (TAGGED this, TIMETYPE, worker *));
  PROTOTYPE(void,        (*mark),    (TAGGED this, worker *));
  PROTOTYPE(TAGGED *,    (*collect), (TAGGED this, worker *));
  PROTOTYPE(CompareType, (*compare), (TAGGED this, TAGGED that, worker *));
} method;

typedef struct {
    TAGGED  header;
    method *method;
    TAGGED  data[ANY];
} generic;


/*******************************************************************************
 *
 * Functions defined in term.c 
 *
 */
extern double get_float PROTO((TAGGED));
/* extern TAGGED make_float PROTO((double, worker *)); */
extern TAGGED make_atomspace_float PROTO((double, worker *));
extern TAGGED make_string_list PROTO((worker *, char *));


#endif /* TERM_H */
