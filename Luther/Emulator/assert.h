/*
 * assert.h
 *
 * Johan Bevemyr.....Mon Nov 18 1991
 *
 *
 * Description:
 *
 *   Compile dynamic clauses and match clauses against claueses in the
 * database.
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef ASSERT_H
#define ASSERT_H


extern code *compile_clause PROTO((TAGGED, TAGGED, worker *,TAGGED *,TAGGED *));
extern BOOL match_term PROTO((Argproto,code *));

/**********************************************************************
 **********************************************************************
 **********************************************************************
 *         Macro definitions used in assert.c                         *
 **********************************************************************
 **********************************************************************
 **********************************************************************/

#if defined(PARALLEL)

#define ADispatchUp(K)				\
{						\
  if ((K) > 0)					\
    {						\
      s = PopCont;				\
      goto write_start;				\
    }						\
  else						\
    {						\
      register TAGGED x = (TAGGED) PopCont;	\
      register TAGGED y = (TAGGED) PopCont;	\
						\
      Bind(y,x,{goto fail;});			\
      s = PopCont;				\
      goto read_start;				\
    }						\
}

#else /* not PARALLEL */

#define ADispatchUp(K)				\
{						\
  s = PopCont;					\
						\
  if ((K) > 0)					\
    {						\
      goto write_start;				\
    }						\
  else						\
    {						\
      goto read_start;				\
    }						\
}

#endif /* PARALLEL */


#define AInitCont	{ cont = pdstack; }
#define PushCont(C)	{ *(cont++) = (TAGGED) (C); }
#define PopCont		((TAGGED *) (*(--cont)))

#define IsForward(T)	IsMarked(T)
#define GetForward(T)	GetNumber(T)


#if defined(UNBOUND)

#define SetForward(V,N)				\
{						\
  PushOnTrail(w,*(TagToPointer(V)));		\
  PushOnTrail(w,V);				\
  *TagToPointer(V) = Make_Integer(N);		\
  Mark(*TagToPointer(V));			\
}

#else /* not UNBOUND */

#define SetForward(V,N)				\
{						\
  PushOnTrail(w,V);				\
  *TagToPointer(V) = Make_Integer(N);		\
  Mark(*TagToPointer(V));			\
}

#endif


#define UndoForward(T)	Unwind_Trail(T)

#define NextFreeReg(R)	(*(R))++

#define EmitTagged(T)				\
{						\
  *(w->global->code_current) = (T);		\
    w->global->code_current++;			\
}


#define EmitOp(O)	EmitTagged((TAGGED) (O))
#define EmitOp1(O,A)	EmitTagged((TAGGED) ((uword)(O) | ((A) << INDEXOFFSET)))
#define EmitOp2(O,A,B)	EmitTagged((TAGGED) ((uword)(O) | ((A) << INDEXOFFSET) | \
				                          ((B) << (INDEXOFFSET * 2))))

#define ZapIndex(To,I,V)	(*(To) |= ((V) << (INDEXOFFSET * (I))))

#define EmitNumber(N)	EmitTagged(N)
#define EmitAtom(A)	EmitTagged(A)
#define EmitFunctor(F)	EmitTagged(F)


#define EmitUnsafeFLT(F)					\
{								\
  TAGGED stmp = (F);						\
								\
  if (IsInHeap(stmp))						\
    {								\
      TAGGED ttmp = make_atomspace_float(GetFloat(stmp),w);	\
      EmitTagged(ttmp);						\
    }								\
  else								\
    {								\
      EmitTagged(stmp);						\
    }								\
}


#define EmitUnsafeBIG(N)						\
{									\
  TAGGED stmp = (N);							\
									\
  if (IsInHeap(stmp))							\
    {									\
      TAGGED ttmp = make_atomspace_bignum(w,BN_SIZE(GetBignum(stmp)));	\
      make_bignum_copy(GetBignum(stmp),GetBignum(ttmp));		\
      EmitTagged(ttmp);							\
    }									\
  else									\
    {									\
      EmitTagged(stmp);							\
    }									\
}


#endif /* ASSERT_H */
