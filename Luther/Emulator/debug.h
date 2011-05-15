/*
 * debug.h
 *
 * Johan Bevemyr.....Wed Jun  5 1991
 * Patric Hedlin.....Thu Dec  8 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef DEBUG_H
#define DEBUG_H


#define MAXSPYPOINTS   100
#define MAXBREAKPOINTS 100

#define DEBUG_PROMPT   "debug"

enum debugflags {
  DBG_SKIP,
  DBG_LEAP,
  DBG_TRACE,
  DBG_CREEP,
  DBG_CALL_LEAP,
  DBG_CALL_TRACE
};

#if defined(DEBUG)

#if defined(PREFETCH)

#define Pre_DisplayInstr(rwmc,C)		\
{						\
  if (w->debug.debugflag)			\
    {						\
      SaveCache(H);				\
      if (!debug(pc-(C==0?1:0),w,rwmc)) return;	\
      LoadCache(H);				\
    }						\
}

#define DisplayFail(rwmc)			\
{						\
  if (w->debug.debugflag)			\
    {						\
      SaveCache(H);				\
      if (!debug_fail(pc-1,w,rwmc)) return;	\
      LoadCache(H);				\
    }						\
}
#else /* not PREFETCH */

#define DisplayFail(rwmc)			\
{						\
  if (w->debug.debugflag)			\
    {						\
      SaveCache(H);				\
      if (!debug_fail(pc,w,rwmc)) return;	\
      LoadCache(H);				\
    }						\
}

#endif /* PREFETCH */


#define DisplayInstr(rwmc)			\
{						\
  if (w->debug.debugflag == TRUE)		\
    {						\
      SaveCache(H);				\
      if (!debug(pc,w,rwmc)) return;		\
      LoadCache(H);				\
    }						\
}

extern void init_debugger PROTO((worker *,int));
extern int debug_fail PROTO((code *, worker *, char *));
extern int debug PROTO((code *,worker *, char *));
extern void debug_prolog_terms PROTO((worker *));

#else /* not DEBUG */

#define DisplayFail(F)
#define DisplayInstr(F) 
#define Pre_DisplayFail(F,C)
#define Pre_DisplayInstr(F,C) 

#endif /* DEBUG */


#if defined(DEBUG)

#define DB_Print1(S)            FD_Print1(w->debug.out,S)
#define DB_Print2(S1,S2)        FD_Print2(w->debug.out,S1,S2)
#define DB_Print3(S1,S2,S3)     FD_Print3(w->debug.out,S1,S2,S3) 
#define DB_Print4(S1,S2,S3,S4)  FD_Print4(w->debug.out,S1,S2,S3,S4)

#else /* not DEBUG */

#define DB_Print1(S)            PL_Print1(stderr,S)
#define DB_Print2(S1,S2)        PL_Print2(stderr,S1,S2)
#define DB_Print3(S1,S2,S3)     PL_Print3(stderr,S1,S2,S3) 
#define DB_Print4(S1,S2,S3,S4)  PL_Print4(stderr,S1,S2,S3,S4)

#endif /* DEBUG */


#endif /* DEBUG_H */
