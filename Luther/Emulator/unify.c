/*
 * unify.c 
 *
 * Johan Bevemyr.....Sat May 25 1991
 *
 *
 * Implementation:
 *
 *   We use a table (as described below) to determine what action to take
 * when we try to unify two terms.
 *
 *   0: Fail unification
 *   1: Bind younger variable to older
 *   2: Bind SVA to HVA
 *   3: Bind HVA to constant or structure
 *   4: Bind SVA to HVA
 *   5: Bind younger variable to older
 *   6: Bind HVA to constant or structure
 *   7: Unify numbers
 *   8: Unify boxed numbers (floats or bignums)
 *   9: Unify lists
 *  10: Unify args of two structures 
 *  11: Unify a generic object with anything but a variable
 *  12: Unify a generic object with anything but a variable or another generic object.
 *  13: Bind CVA to constant or CVA
 *  14: Bind CVA to constant
 *  15: Bind younger CVA to older CVA
 *
 *         H  C  S  N  A  B  G  L  S
 *         V  V  V  U  T  O  E  S  T
 *         A  A  A  M  M  X  N  T  R
 *   HVA { 1, 3, 2, 3, 3, 3, 3, 3, 3 }
 *   CVA { 6,15, 2,13,13,13,13,13,13 }
 *   SVA { 4,14, 5, 4, 4, 4, 4, 4, 4 }
 *   NUM { 6,14, 2, 7, 0, 0,12, 0, 0 }
 *   ATM { 6,14, 2, 0, 0, 0,12, 0, 0 }
 *   BOX { 6,14, 2, 0, 0, 8,12, 0, 0 }
 *   GEN { 6,14, 2,11,11,11,11,11,11 }
 *   LST { 6,14, 2, 0, 0, 0,12, 9, 0 }
 *   STR { 6,14, 2, 0, 0, 0,12, 0,10 }
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"
#include "unify.h"
#include "bignum.h"
#include "engine.h"


#if defined(CONSTR)
#  define C(X,Y) X,Y
#else
#  define C(X,Y) Y
#endif

#if defined(UNBOUND)
#  define U(X,Y) X,Y
#else
#  define U(X,Y) X
#endif

int unifytable[MAX_TAG][MAX_TAG] = {
  { 1, C( 3, 2), 3, 3, 3, 3, 3, U( 3, 0)},
#if defined(CONSTR)
  { 6, C(15, 2),13,13,13,13,13, U(13, 0)},
#endif
  { 4, C(14, 5), 4, 4, 4, 4, 4, U( 4, 0)},
  { 6, C(14, 2), 7, 0, 0,12, 0, U( 0, 0)},
  { 6, C(14, 2), 0, 0, 0,12, 0, U( 0, 0)},
  { 6, C(14, 2), 0, 0, 8,12, 0, U( 0, 0)},
  { 6, C(14, 2),11,11,11,11,11, U(11, 0)},
  { 6, C(14, 2), 0, 0, 0,12, 9, U( 0, 0)},
  { 6, C(14, 2), 0, 0, 0,12, 0, U(10, 0)}
#if defined(UNBOUND)
, { 0, C( 0, 0), 0, 0, 0, 0, 0, U( 0, 0)}
#endif 
};

static BOOL unify_structure(s1, s2, arity,w)
    TAGGED *s1, *s2;
    int arity;
    worker *w;
{
    TAGGED t1, t2;

    while (arity--)
      {
	DerefNLL(t1, Ref(s1)); s1 += VARSIZE;
	DerefNLL(t2, Ref(s2)); s2 += VARSIZE;

	if (!unify(t1,t2,w)) return FALSE;
      }
    return TRUE;
}

BOOL unify(x,y,w)
    register TAGGED x, y;
    worker *w;
{
  register TAGGED x_time, y_time;
  
 start:
  if (x != y)
    {
      switch (unifytable[TagOf(x)][TagOf(y)])
	{
	case 0:			/* unification failed */
	  return FALSE;

	case 1:			/* HVA - HVA */
#ifdef UNBOUND
	  x_time = *RemoveTag(x,HVA);
	  y_time = *RemoveTag(y,HVA);

	  if (!(IsUVA(x_time) && IsUVA(y_time))) unify_deref(x,y,w);
	    
	  y_time = (TAGGED) GetHVATime(y_time);
	  x_time = (TAGGED) GetHVATime(x_time);
#else  /* not UNBOUND */
	  y_time = (TAGGED) GetHVATime(y);
	  x_time = (TAGGED) GetHVATime(x);
#endif /* UNBOUND */
	  if (x_time > y_time)
	    {
	      Bind_HVA(x,y,{return FALSE;});
	    } 
	  else if (x_time < y_time)
	    {
	      Bind_HVA(y,x,{return FALSE;});
	    }
#ifdef COPY_GC
	  else if (IsOrdered(x))
	    if (IsOrdered(y))
	      if (GetOrderNr(x) > GetOrderNr(y))
		{
		  Bind_HVA(x,y,{return FALSE;});
		}
	      else
		{
		  Bind_HVA(y,x,{return FALSE;});
		}
	    else
	      {
		Bind_HVA(y,x,{return FALSE;});
	      }
	  else  
	    {
	      /* Now, y is ordered or neither x nor y is ordered,
	       * in any case, we bind x to y.
	       */

	      Bind_HVA(x,y,{return FALSE;});
	    }
	  break;
#else	      
	  else if (x > y)	/* this code is redundant if UVAs are not used */
	    {
	      Bind_HVA(x,y,{return FALSE;});
	    }
	  else
	    {
	      Bind_HVA(y,x,{return FALSE;});
	    }		
	  break;
#endif

	case 2:			/* HVA \/ Constant - SVA */
	  {
	    Bind_SVA(y,x);
	  }
	  break;

	case 3:			/* HVA - Constant */
	  {
	    Bind_HVA(x,y,{return FALSE;});
	  }
	  break;

	case 4:			/* SVA - HVA \/ Constant */
	  {
	    Bind_SVA(x,y);
	  }
	  break;

	case 5:			/* SVA - SVA */
	  {
	    if (GetSVATime(x) > GetSVATime(y))
	      {
		Bind_SVA(x,y);
	      }
	    else
	      {
		Bind_SVA(y,x);
	      }
	  }
	  break;

	case 6:			/* Constant - HVA */
	  {
	    Bind_HVA(y,x,{return FALSE;});
	  }
	  break;

	case 7:			/* NUM - NUM */
	  return FALSE;

	case 8:			/* BOX - BOX */
	  {
	    if (IsFLT(x) && IsFLT(y))
	      {
		if (GetFloat(x) == GetFloat(y))
		  break;
		else
		  return FALSE;
	      }

	    if (IsBIG(x) && IsBIG(y))
	      {
		if (bignum_eq(GetBignum(x), GetBignum(y)))
		  break;
		else
		  return FALSE;
	      }
	  }
	  return FALSE;

	case 9:			/* LST - LST */
	  return unify_structure(GetCar(x),GetCar(y), 2, w);

	case 10:		/* STR - STR */
	  { 
	    register UNTAGGED xu, yu;

	    xu = (UNTAGGED) RemoveTag(x,STR);
	    yu = (UNTAGGED) RemoveTag(y,STR);
	    
	    if (Struct(xu)->functor == Struct(yu)->functor)
	      {
		return unify_structure(GetUntArg(xu,0), GetUntArg(yu,0),
				       StructArity(xu), w);
	      }
	    else
	      return FALSE;
	  }

	case 11:		/* GEN - CON \/ GEN */
	  return GetMethod(unify,x)(x,y,w);

	case 12:		/* GEN - CON */
	  return GetMethod(unify,y)(y,x,w);

#ifdef CONSTR
	case 13:		/* CVA - CON \/ CVA */
	  {
	    Bind_CVA(x,y,{return FALSE;});
	  }
	  break;

	case 14:		/* constant - CVA */
	  {
	    Bind_CVA(y,x,{return FALSE;});
	  }
	  break;

	case 15:		/* CVA - CVA */
	  {
#ifdef UNBOUND
	    x_time = *RemoveTag(x,CVA);
	    y_time = *RemoveTag(y,CVA);

	    if (!(IsUVA(x_time) && IsUVA(y_time))) unify_deref(x,y,w);
	    
	    y_time = (TAGGED) GetCVATime(y_time);
	    x_time = (TAGGED) GetCVATime(x_time);
#else  /* not UNBOUND */
	    y_time = (TAGGED) GetCVATime(y);
	    x_time = (TAGGED) GetCVATime(x);
#endif /* UNBOUND */
	    if (x_time > y_time)
	      {
		Bind_CVA(x,y,{return FALSE;});
	      } 
	    else if (x_time < y_time)
	      {
		Bind_CVA(y,x,{return FALSE;});
	      }
	    else if (x > y) /* this code is redundant if UVAs are not used */
	      {
		Bind_CVA(x,y,{return FALSE;});
	      }
	    else
	      {
		Bind_CVA(y,x,{return FALSE;});
	      }		
	    break;
	  }
#endif /* CONSTR */

	default:
	  {
	    Error("unify - Out of range in unify");
	  }
	  return TRUE;
	}
      return TRUE;
    }
  return TRUE;
}

BOOL local_unify(x,y,w)
    register TAGGED x, y;
    worker *w;
{
  register TAGGED x_time, y_time;
  
 start:
  if (x!=y)
    {
      switch(unifytable[TagOf(x)][TagOf(y)])
	{
	case 0:			/* unification failed */
	  return FALSE;

	case 1:			/* HVA - HVA */
#ifdef UNBOUND
	  x_time = *RemoveTag(x,HVA);
	  y_time = *RemoveTag(y,HVA);

	  if (!(IsUVA(x_time) && IsUVA(y_time))) unify_deref(x,y,w);
	    
	  y_time = (TAGGED) GetHVATime(y_time);
	  x_time = (TAGGED) GetHVATime(x_time);
#else  /* not UNBOUND */
	  y_time = (TAGGED) GetHVATime(y);
	  x_time = (TAGGED) GetHVATime(x);
#endif /* UNBOUND */
	  if (x_time > y_time)
	    {
	      Local_Bind_HVA(x,y);
	    } 
	  else if (x_time < y_time)
	    {
	      Local_Bind_HVA(y,x);
	    }
#ifdef COPY_GC
	  else if (IsOrdered(x))
	    if (IsOrdered(y))
	      if (GetOrderNr(x) > GetOrderNr(y))
		{
		  Local_Bind_HVA(x,y);
		}
	      else
		{
		  Local_Bind_HVA(y,x);
		}
	    else
	      {
		Local_Bind_HVA(y,x);
	      }
	  else  
	    {
	      /* Now, y is ordered or neither x nor y is ordered,
	       * in any case, we bind x to y.
	       */

	      Local_Bind_HVA(x,y);
	    }
	  break;
#else	      
	  else if (x > y)      /* this code is redundant if UVAs are not used */
	    {
	      Local_Bind_HVA(x,y);
	    }
	  else
	    {
	      Local_Bind_HVA(y,x);
	    }		
	  break;
#endif

	case 2:			/* HVA \/ constant - SVA */
	  {
	    Bind_SVA(y,x);
	  }
	  break;

	case 3:			/* HVA - Constant */
	  {
	    Local_Bind_HVA(x,y);
	  }
	  break;

	case 4:			/* SVA - HVA \/ constant */
	  {
	    Bind_SVA(x,y);
	  }
	  break;

	case 5:			/* SVA - SVA */
	  {
	    if (GetSVATime(x) > GetSVATime(y))
	      {
		Bind_SVA(x,y);
	      }
	    else
	      {
		Bind_SVA(y,x);
	      }
	    break;
	  }

	case 6:			/* Constant - HVA */
	  {
	    Local_Bind_HVA(y,x);
	  }
	  break;

	case 7:			/* NUM - NUM */
	  return FALSE;

	case 8:			/* BOX - BOX */
	  {
	    if (IsFLT(x) && IsFLT(y))
	      {
		if (GetFloat(x) == GetFloat(y))
		  break;
		else
		  return FALSE;
	      }

	    if (IsBIG(x) && IsBIG(y))
	      {
		if (bignum_eq(GetBignum(x), GetBignum(y)))
		  break;
		else
		  return FALSE;
	      }
	  }
	  return FALSE;

	case 9:			/* LST - LST */
	  return unify_structure(GetCar(x),GetCar(y), 2, w);

	case 10:		/* STR - STR */
	  { 
	    register UNTAGGED xu, yu;

	    xu = (UNTAGGED) RemoveTag(x,STR);
	    yu = (UNTAGGED) RemoveTag(y,STR);
	    
	    if (Struct(xu)->functor == Struct(yu)->functor)
	      {
		return unify_structure(GetUntArg(xu,0), GetUntArg(yu,0),
				       StructArity(xu), w);
	      }
	    else
	      return FALSE;
	  }

	case 11:		/* GEN - CON \/ GEN */
	  return GetMethod(unify,x)(x,y,w);

	case 12:		/* GEN - CON */
	  return GetMethod(unify,y)(y,x,w);

#ifdef CONSTR
	case 13:		/* CVA - constant \/ CVA */
	  {
	    Local_Bind_CVA(x,y);
	  }
	  break;

	case 14:		/* constant - CVA */
	  {
	    Local_Bind_CVA(y,x);
	  }
	  break;

	case 15:		/* CVA - CVA */
	  {
#ifdef UNBOUND
	    x_time = *RemoveTag(x,CVA);
	    y_time = *RemoveTag(y,CVA);

	    if (!(IsUVA(x_time) && IsUVA(y_time))) unify_deref(x,y,w);
	    
	    y_time = (TAGGED) GetCVATime(y_time);
	    x_time = (TAGGED) GetCVATime(x_time);
#else  /* not UNBOUND */
	    y_time = (TAGGED) GetCVATime(y);
	    x_time = (TAGGED) GetCVATime(x);
#endif /* UNBOUND */
	    if (x_time > y_time)
	      {
		Local_Bind_CVA(x,y);
	      } 
	    else if (x_time < y_time)
	      {
		Local_Bind_CVA(y,x);
	      }
	    else if (x > y) /* this code is redundant if UVAs are not used */
	      {
		Local_Bind_CVA(x,y);
	      }
	    else
	      {
		Local_Bind_CVA(y,x);
	      }		
	    break;
	  }
#endif /* CONSTR */
	default:
	  {
	    Error("unify - Out of range in unify");
	  }
	  return TRUE;
	}
      return TRUE;
    }
  return TRUE;
}

BOOL unify_deref(x,y,w)
    TAGGED x, y;
    worker *w;
{
    DerefNLL(x,x);
    DerefNLL(y,y);

    return unify(x,y,w);
}
