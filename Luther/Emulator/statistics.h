/*
 * statistics.h
 *
 * Johan Bevemyr.....Mon Jun 10 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef STATISTICS_H
#define STATISTICS_H


#if defined(STATISTICS)
#  define BuiltinCallStatistics w->stats->builtin_calls += 1
#  define CallStatistics        w->stats->calls += 1
#  define FailStatistics        w->stats->fails += 1

#  if defined(TIMESTAMP)
#    define HVAInitStat(W)     (W)->stats->hva_init_count += 1     
#    define SVAInitStat(W)     (W)->stats->sva_init_count += 1     
#  endif

#  if defined(SWAP_BIND)
#    define SwapFailStat(W)    (W)->stats->swap_fail += 1
#  endif

#else /* not STATISTICS */

#  define BuiltinCallStatistics 
#  define CallStatistics 
#  define FailStatistics 
#  define HVAInitStat(W)
#  define SVAInitStat(W)
#  define LCKGrabStat(W)
#  define LCKAttStat(W)
#  define SwapFailStat(W)

#endif /* STATISTICS */


#if defined(TRAILSTAT)
#  define TrailStat             w->stats->trail_stat += 1
#  define DerefStat             w->stats->deref_stat += 1
#  define RestoreStat(A)        w->stats->restore_stat += (A)
#  define SwapBindStat          w->stats->swap_stat += 1
#  define TidyStat(I)           w->stats->tidy_stat += (I)

#  define InstrStat(I)				\
{						\
  w->stats->instr_stat += 1;			\
  w->stats->instr_prof[Get_Op(I)] += 1;		\
}

#else /* not TRAILSTAT */

#  define TrailStat
#  define DerefStat
#  define RestoreStat(A)
#  define InstrStat(I)
#  define SwapBindStat
#  define TidyStat(I)

#endif /* TRAILSTAT */


#if defined(STAT_AWAIT)
#  define AwaitStat(W)          (W)->stats->await += 1;
#  define AwaitCountStat(W)     (W)->stats->await_count += 1;
#else /* not STAT_AWAIT */
#  define AwaitStat(W)
#  define AwaitCountStat(W)
#endif /* STAT_AWAIT */

extern BOOL luther_statistics PROTO((Argproto));
extern BOOL luther_statistics_n PROTO((Argproto));
extern BOOL luther_statistics_runtime PROTO((Argproto));
extern BOOL luther_statistics_gctime PROTO((Argproto));
extern BOOL luther_statistics_gcnr PROTO((Argproto));
extern BOOL luther_statistics_gcbytes PROTO((Argproto));
extern BOOL luther_statistics_walltime PROTO((Argproto));
extern BOOL luther_statistics_parallel_runtime PROTO((Argproto));
extern BOOL luther_statistics_memory PROTO((Argproto));
extern BOOL luther_statistics_global PROTO((Argproto));
extern BOOL luther_statistics_local PROTO((Argproto));
extern BOOL luther_statistics_trail PROTO((Argproto));
extern BOOL luther_statistics_deref PROTO((Argproto));
extern BOOL luther_statistics_restore PROTO((Argproto));
extern BOOL luther_statistics_instr PROTO((Argproto));
extern BOOL luther_statistics_tidy PROTO((Argproto));
extern BOOL luther_statistics_swap PROTO((Argproto));
extern BOOL luther_statistics_code PROTO((Argproto));
extern BOOL luther_statistics_atom PROTO((Argproto));
extern BOOL luther_statistics_instr_prof PROTO((Argproto));
extern BOOL luther_pmon PROTO((Argproto));
extern BOOL luther_pmon_print PROTO((Argproto));

void init_statistics PROTO((worker *w));


#endif /* STATISTICS_H */
