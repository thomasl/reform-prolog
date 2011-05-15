/*
 * bind.h
 *
 * Johan Bevemyr.....Fri Nov  6 1992
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef BIND_H
#define BIND_H

#if defined(CONSTR)

#define Bind(To,Val,FAILCODE)			\
{						\
  register TAGGED rTo = (To);			\
  register TAGGED rVal = (Val);			\
						\
  if (IsHVA(rTo))				\
    {						\
      Bind_HVA(rTo,rVal,FAILCODE);		\
    }						\
  else if (IsSVA(rTo))				\
    {						\
      Bind_SVA(rTo,rVal);			\
    }						\
  else /* must be CVA */			\
    {						\
      Bind_CVA(rTo,rVal,FAILCODE);		\
    }						\
}

#define Bind_Heap(To,Val,FAILCODE)		\
{						\
  register TAGGED rTo = (To);			\
  register TAGGED rVal = (Val);			\
						\
  if (IsHVA(rTo))				\
    {						\
      Bind_HVA(rTo,rVal,FAILCODE);		\
    }						\
  else /* must be CVA */			\
    {						\
      Bind_CVA(rTo,rVal,FAILCODE);		\
    }						\
}

#define Local_Bind(To,Val)			\
{						\
  register TAGGED rTo = (To);			\
  register TAGGED rVal = (Val);			\
						\
  if (IsHVA(rTo))				\
    {						\
      Local_Bind_HVA(rTo,rVal);			\
    }						\
  else if (IsSVA(rTo))				\
    {						\
      Bind_SVA(rTo,rVal);			\
    }						\
  else /* must be CVA */			\
    {						\
      Local_Bind_CVA(rTo,rVal);			\
    }						\
}

#define Local_Bind_Heap(To,Val)			\
{						\
  register TAGGED rTo = (To);			\
  register TAGGED rVal = (Val);			\
						\
  if (IsHVA(rTo))				\
    {						\
      Local_Bind_HVA(rTo,rVal);			\
    }						\
  else /* must be CVA */			\
    {						\
      Local_Bind_CVA(rTo,rVal);			\
    }						\
}

#else  /* not CONSTR */

#define Bind(To,Val,FAILCODE)			\
{						\
  register TAGGED rTo = (To);			\
  register TAGGED rVal = (Val);			\
						\
  if (IsHVA(rTo))				\
    {						\
      Bind_HVA(rTo,rVal,FAILCODE);		\
    }						\
  else /* must be SVA */			\
    {						\
      Bind_SVA(rTo,rVal);			\
    }						\
}

#define Bind_Heap(To,Val,FAILCODE)		\
{						\
  register TAGGED rTo = (To);			\
  register TAGGED rVal = (Val);			\
						\
  Bind_HVA(rTo,rVal,FAILCODE);			\
}

#define Local_Bind(To,Val)			\
{						\
  register TAGGED rTo = (To);			\
  register TAGGED rVal = (Val);			\
						\
  if (IsHVA(rTo))				\
    {						\
      Local_Bind_HVA(rTo,rVal);			\
    }						\
  else /* must be SVA */			\
    {						\
      Bind_SVA(rTo,rVal);			\
    }						\
}

#define Local_Bind_Heap(To,Val)			\
{						\
  register TAGGED rTo = (To);			\
  register TAGGED rVal = (Val);			\
						\
  Local_Bind_HVA(rTo,rVal);			\
}

#endif /* CONSTR */


/**********************************************************************
 *
 * Atomic bind operation using SWAP
 *
 */

#if defined(SWAP_BIND)
#if defined(UNBOUND)

#define Bind_HVA(To,Val,FAILCODE)			\
{							\
  register TAGGED *Pos = (TAGGED *) OffsetBase(To);	\
  register TAGGED NVal = (Val);				\
  register TAGGED OVal;					\
							\
  KSR1(DECLARE_SWAP_VAL);				\
							\
  SwapBindStat;						\
							\
  if (!IsUVA(OVal = swap_il(Pos,NVal)))			\
    {							\
      SwapFailStat(w)					\
      if(!unify_deref(OVal, NVal,w))			\
        {						\
          FAILCODE;					\
        }						\
    }							\
  else							\
    {							\
      Trail_HVA(To,OVal);				\
    }							\
}

#define Bind_CVA(To,Val,FAILCODE)		\
{						\
  register TAGGED *Pos = TagToPointer(To);	\
  register TAGGED NVal = (Val);			\
  register TAGGED OVal;				\
						\
  KSR1(DECLARE_SWAP_VAL);			\
						\
  SwapBindStat;					\
						\
  if (!IsCVA(OVal = swap_il(Pos,NVal)))		\
    {						\
      SwapFailStat(w)				\
      if (!unify_deref(OVal, NVal,w))		\
	{					\
	  FAILCODE;				\
	}					\
    }						\
  else						\
    {						\
      Trail_CVA(To,OVal);			\
    }						\
}

#else /* not UNBOUND */

#define Bind_HVA(To,Val,FAILCODE)			\
{							\
  register TAGGED *Pos = (TAGGED *) OffsetBase(To);	\
  register TAGGED NVal = (Val);				\
  register TAGGED OVal;					\
							\
  KSR1(DECLARE_SWAP_VAL);				\
							\
  SwapBindStat;						\
							\
  if ((OVal = swap_il(Pos,NVal)) != To)			\
    {							\
      SwapFailStat(w)					\
      if (!unify_deref(OVal, NVal,w))			\
	{						\
	  FAILCODE;					\
	}						\
    }							\
  else							\
    {							\
      Trail_HVA(To);					\
    }							\
}

#define Bind_CVA(To,Val,FAILCODE)		\
{						\
  register TAGGED *Pos = TagToPointer(To);	\
  register TAGGED NVal = (Val);			\
  register TAGGED OVal;				\
						\
  KSR1(DECLARE_SWAP_VAL);			\
						\
  SwapBindStat;					\
						\
  if ((OVal = swap_il(Pos,NVal)) != To)		\
    {						\
      SwapFailStat(w)				\
      if (!unify_deref(OVal, NVal,w))		\
	{					\
	  FAILCODE;				\
	}					\
    }						\
  else						\
    {						\
      Trail_CVA(To);				\
    }						\
}

#endif /* UNBOUND */

#else  /* not SWAP_BIND */

#define Bind_HVA(To, Val, FAILCODE)  Local_Bind_HVA(To,Val)
#define Bind_CVA(To, Val, FAILCODE)  Local_Bind_CVA(To,Val)

#endif /* SWAP_BIND */

#if defined(UNBOUND)

#define Local_Bind_HVA(To, Val)				\
{							\
  TAGGED *lbh_pos = ((TAGGED *) OffsetBase(To));	\
  Trail_HVA(To,*lbh_pos);				\
  *lbh_pos = (Val);					\
}

#define Local_Bind_CVA(To, Val)			\
{						\
  TAGGED *lbh_pos = TagToPointer(To);		\
  Trail_CVA(To,*lbh_pos);			\
  *lbh_pos = (Val);				\
}

#else /* not UNBOUND */

#define Local_Bind_HVA(To, Val)			\
{						\
  Trail_HVA(To);				\
  *((TAGGED *) OffsetBase(To)) = (Val);		\
}

#define Local_Bind_CVA(To, Val)			\
{						\
  Trail_CVA(To);				\
  *TagToPointer(To) = (Val);			\
}

#endif /* UNBOUND */


#define Bind_SVA(To, Val)			\
{						\
  Trail_SVA(To);				\
  *TagToPointer(To) = (Val);			\
}

#define AlwaysBind(X,Y)     SetVar(X,Y)
#define SetHVA(X,Y)         *((TAGGED *) OffsetBase(X)) = Y
#define SetVar(X,Y)         *(TagToPointer(X)) = (Y);


#endif /* BIND_H */
