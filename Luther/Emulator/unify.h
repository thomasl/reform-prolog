/*
 * unify.h
 *
 * Johan Bevemyr.....Thu May 23 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef UNIFY_H
#define UNIFY_H

#include "bind.h"
#include "trail.h"
#include "deref.h"

/**********************************************************************
 *
 *
 */

#if defined(CONSTR)
#  define RefHVAorCVA(To,X) RefVAR(To,X)
#else
#  define RefHVAorCVA(To,X) RefHVA(To,X)
#endif

/**********************************************************************
 *
 */
#define RefVAR(To, X) To = *TagToPointer(X)
#define RefHVA(To, X) To = *Var(OffsetBase(X))

#define IsBoundHVA(X) (((TAGGED) X) != *OffsetBase(X))

/**********************************************************************
 *
 */

#define Unify(X,Y)				\
{						\
  register TAGGED tmp1, tmp2;			\
  DerefNLL(tmp1,X);				\
  DerefNLL(tmp2,Y);				\
  if (!unify(tmp1,tmp2,w)) goto fail;		\
} 
 
#define Local_Unify(X,Y)			\
{						\
  register TAGGED tmp1, tmp2;			\
  DerefNLL(tmp1,X);				\
  DerefNLL(tmp2,Y);				\
  if (!local_unify(tmp1,tmp2,w)) goto fail;	\
} 
 
#define UnifyPop(X,Y)				\
{						\
  register TAGGED tmp1, tmp2;			\
  DerefNLL(tmp1,X);				\
  DerefNLL(tmp2,Y);				\
  if (!unify(tmp1,tmp2,w)) goto fail;		\
  s = PopCont;					\
} 
 
#define Unify1(X,Y)				\
{						\
  register TAGGED tmp1;				\
  DerefNLL(tmp1,X);				\
  if (!unify(tmp1,Y,w)) goto fail;		\
}

/**********************************************************************
 *
 */

#define IsUnsafe(V)  (((environment *) RemoveTag(V,SVA)) > w->frame)

#define RefStackUnsafe(H,To,X)			\
{						\
  register TAGGED tmp1;				\
						\
  DerefNLL(tmp1,X);				\
						\
  if (IsSVA(tmp1) && IsUnsafe(tmp1))		\
    {						\
      LoadHVA(H,To,w);				\
      Bind_SVA(tmp1, To);			\
    }						\
  else						\
    {						\
      To = tmp1;				\
    }						\
}


#define WriteLocalValueY(X,H)			  \
{						  \
  register TAGGED tmp1,tmp2;			  \
						  \
  DerefNLL(tmp1,X);				  \
						  \
  if (IsSVA(tmp1))				  \
    {						  \
      LoadHVA(H,tmp2,w);			  \
      Bind_SVA(tmp1,tmp2);			  \
    }						  \
  else						  \
    {						  \
      PushOnHeap(H,tmp1);			  \
    }						  \
}


#define WriteLocalValueX(X,H)			\
{						\
  register TAGGED tmp1,tmp2;			\
						\
  DerefNLL(tmp1,X);				\
						\
  if (IsSVA(tmp1))				\
    {						\
      LoadHVA(H,tmp2,w);			\
      Bind_SVA(tmp1,tmp2);			\
      X = tmp2;				        \
    }						\
  else						\
    {						\
      PushOnHeap(H,tmp1)			\
      X = tmp1;				        \
    }						\
}


#define GlobalizeRegisterY(X,H)			  \
{						  \
  register TAGGED tmp1,tmp2;			  \
						  \
  DerefNLL(tmp1,X);				  \
						  \
  if (IsSVA(tmp1))				  \
    {						  \
      LoadHVA(H,tmp2,w);			  \
      Bind_SVA(tmp1,tmp2);			  \
    }						  \
  else						  \
    {						  \
    }						  \
}

#define GlobalizeRegisterX(X,H)			\
{						\
  register TAGGED tmp1,tmp2;			\
						\
  DerefNLL(tmp1,X);				\
						\
  if (IsSVA(tmp1))				\
    {						\
      LoadHVA(H,tmp2,w);			\
      Bind_SVA(tmp1,tmp2);			\
      X = tmp2;					\
    }						\
  else						\
    {						\
      X = tmp1;					\
    }						\
}
  

/**********************************************************************
 *
 */
extern BOOL unify PROTO((TAGGED, TAGGED, worker *));
extern BOOL local_unify PROTO((TAGGED, TAGGED, worker *));
extern BOOL unify_deref PROTO((TAGGED, TAGGED, worker *));


#endif /* UNIFY_H */
