/*
 * deref.h - Dereferencing macros.
 *
 * Johan Bevemyr.....Fri Nov  6 1992
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef DEREF_H
#define DEREF_H


#if defined(UNBOUND)

#define Deref(Xderefed, X)				\
{                     					\
  register TAGGED d_v, d_vp;                    	\
                                                	\
  d_v = (X);						\
							\
  forever                                		\
    {                                         		\
      if (IsVar(d_v))                         		\
	{                                     		\
	  RefVAR(d_vp,d_v);                   		\
	  if ((d_v != d_vp) && (!IsUVA(d_vp)))		\
	    {                                 		\
	      d_v = d_vp;                     		\
	      continue;                       		\
	    }                                 		\
	}                                     		\
      break;                                  		\
    }                                         		\
  Xderefed = d_v;                             		\
}

#define DerefHVA(Xderefed, X)			\
{						\
  register TAGGED d_v, d_vp;			\
						\
  d_v = (X);					\
						\
  forever					\
    {						\
      if (IsHVA(d_v))				\
	{					\
	  RefHVA(d_vp,d_v);			\
	  if (!IsUVA(d_vp))			\
	    {					\
	      d_v = d_vp;			\
	      continue;				\
	    }					\
	}					\
      break;					\
    }						\
  Xderefed = d_v;				\
}

#else /* not UNBOUND */

#if defined(SEQUENT)

#define Deref(Xderefed, X)			\
{                           			\
  register TAGGED d_v, d_vp;			\
                              			\
  d_v = (X);                			\
						\
  forever					\
    {        					\
      if (IsVar(d_v))				\
	{           				\
	  RefVAR(d_vp,d_v);			\
	  if (d_v == d_vp)  			\
	    break;         			\
	}                  			\
      else					\
	{					\
	  break;             			\
	}					\
      d_v = d_vp;          			\
    }                      			\
  Xderefed = d_v;          			\
}

#define DerefHVA(Xderefed, X)			\
{						\
  register TAGGED d_v, d_vp;			\
						\
  d_v = (X);					\
						\
  forever					\
    {						\
      if(IsHVAofCVA(d_v))			\
	{					\
	  RefHVAorCVA(d_vp,d_v);		\
	  if(d_v == d_vp)			\
	    break;				\
	}					\
      else					\
	break;					\
      d_v = d_vp;				\
    }						\
  Xderefed = d_v;				\
}

#else /* not SEQUENT */

#define Deref(Xderefed, X)			\
{						\
  register TAGGED d_v = (X), d_vp;		\
  						\
  if (IsVar(d_v))				\
    do {					\
      DerefStat;				\
      RefVAR(d_vp,d_v);				\
    } while(d_v != d_vp && IsVar(d_v = d_vp));	\
  Xderefed = d_v;				\
}

#define DerefHVA(Xderefed, X)				\
{							\
  register TAGGED d_v = (X), d_vp;			\
							\
  if (IsHVAorCVA(d_v))					\
    do {						\
      DerefStat;					\
      RefHVAorCVA(d_vp,d_v);				\
    } while(d_v != d_vp && IsHVAorCVA(d_v = d_vp));	\
  Xderefed = d_v;					\
}

#endif /* SEQUENT */
#endif /* UNBOUND */

#define DerefNLL(Xderefed,X) Deref(Xderefed,X)

#if defined(UNBOUND)

#define DerefLockSwitch(d_v, Vcode, Ccode)	\
{						\
  register TAGGED d_vp;				\
						\
  forever					\
    {						\
      if (IsVar(d_v))				\
	{					\
	  RefVAR(d_vp,d_v);			\
	  if ((d_v == d_vp) || (IsUVA(d_vp)))	\
	    {					\
	      Vcode;				\
	      break;				\
	    }					\
	}					\
      else					\
	{					\
	  Ccode;				\
	  break;				\
	}					\
      d_v = d_vp;				\
    }						\
}
    
#define DerefLockSwitchHVA(d_v, Vcode, Ccode)	\
{                                            	\
  register TAGGED d_vp = d_v;                	\
                                             	\
  forever                                	\
    {                                        	\
      if (IsVar(d_v))                         	\
	{                                    	\
	  RefHVA(d_vp,d_v);                  	\
	  if (IsUVA(d_vp))                    	\
	    {                                	\
	      Vcode;                         	\
	      break;                         	\
	    }                                	\
	}                                    	\
      else                                   	\
	{                                    	\
	  Ccode;                             	\
	  break;                             	\
	}                                    	\
      d_v = d_vp;                            	\
    }                                        	\
}

#else /* not UNBOUND */

#define DerefLockSwitch(d_v, Vcode, Ccode)	\
{						\
  register TAGGED d_vp;				\
						\
  forever					\
    {						\
      if (IsVar(d_v))				\
	{					\
	  DerefStat;				\
	  RefVAR(d_vp,d_v);			\
	  if (d_v == d_vp)			\
	    {					\
	      Vcode;				\
	      break;				\
	    }					\
	}					\
      else					\
	{					\
	  Ccode;				\
	  break;				\
	}					\
      d_v = d_vp;				\
    }						\
}
    
#define DerefLockSwitchHVA(d_v, Vcode, Ccode)	\
{                                            	\
  register TAGGED d_vp;                      	\
                                             	\
  forever                                	\
    {                                        	\
      if (IsHVAorCVA(d_v))                    	\
	{                                    	\
	  DerefStat;                         	\
	  RefHVAorCVA(d_vp,d_v);             	\
	  if (d_v == d_vp)			\
	    {                  			\
	      Vcode;                           	\
	      break;                           	\
	    }					\
	}                                    	\
      else                                   	\
	{                                    	\
	  Ccode;                             	\
	  break;                             	\
	}                                    	\
      d_v = d_vp;                            	\
    }                                        	\
}

#endif  /* UNBOUND */


#endif /* DEREF_H */
