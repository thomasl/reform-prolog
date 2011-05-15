/*
 * parallel.h
 *
 * Johan Bevemyr.....Wed Apr 14 1993
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef PARALLEL_H
#define PARALLEL_H


#if defined(PARALLEL)

#define Report_Global_Fail AddGlobalEvent(GLOBAL_EVENT_FAIL)

#else /* not PARALLEL */

#define Report_Global_Fail 

#endif /* PARALLEL */


/**********************************************************************
 *
 * Macros for activating workers
 *
 */

#define ActivateWorkers(W)						   \
{									   \
  double starttime = walltime();					   \
									   \
  ActivateWorkersStart(W);						   \
  ActivateWorkersStop(W);						   \
									   \
  (W)->stats->in_parallel_walltime += walltime()-starttime;		   \
}

#define ActivateWorkersGC(W,G)						   \
{									   \
  double starttime = walltime();					   \
									   \
  ActivateWorkersStart(W);						   \
									   \
  while(1)								   \
    {									   \
      ActivateWorkersStop(W);						   \
      G;								   \
    }									   \
									   \
  (W)->stats->in_parallel_walltime += walltime()-starttime;		   \
}

#define ActivateWorkersNoWait(W)					   \
{									   \
  double starttime = walltime();					   \
									   \
  ActivateWorkersStart(W);						   \
									   \
  (W)->stats->in_parallel_walltime += walltime()-starttime;		   \
}

/**********************************************************************
 * synch method specific code
 */

#ifdef ACTIVE_SYNCH
#define ActivateWorkersStart(W)						   \
{									   \
  if(W->global->workers_active == FALSE)				   \
    {									   \
      DropPrivateSema(W,(W)->global->active_workers);			   \
      w->global->workers_active = TRUE;					   \
    }									   \
  else									   \
    {									   \
      SpinDropPrivateSema(W,(W)->global->active_workers);		   \
    }									   \
}

#define ActivateWorkersStop(W)						   \
{									   \
  GrabDone((W)->global->active_workers,W);				   \
}

#else /* not ACTIVE_SYNCH */

#if defined(USE_KSR_THREADS)

#define ActivateWorkersStart(W)						   \
{									   \
  pthread_barrier_checkin(W->global->worker_barrier, W->pid);		   \
}

#define ActivateWorkersStop(W)						   \
{									   \
  pthread_barrier_checkout(W->global->worker_barrier, W->pid);	           \
}

#else /* not USE_KSR_THREADS */

#define ActivateWorkersStart(W)						   \
{									   \
  DropPrivateSema(W,(W)->global->active_workers);			   \
}

#define ActivateWorkersStop(W)						   \
{									   \
  GrabSemaphore(SEMA_SDONE,(W)->global->active_workers,W);		   \
}

#endif /* USE_KSR_THREADS */
#endif /* ACTIVE_SYNCH */

#if defined(PARALLEL)

#define Run_Parallel(C)							   \
{									   \
  w->global->parallel_start.type = C;					   \
  ActivateWorkers(w);							   \
}

#define Start_Parallel(C)						   \
{									   \
  w->global->parallel_start.type = C;					   \
  ActivateWorkersStart(w);						   \
}

#define Stop_Parallel							   \
{									   \
  ActivateWorkersStop(w);						   \
}

#define Run_Parallel_NoWait(C)						   \
{									   \
  w->global->parallel_start.type = C;					   \
  ActivateWorkersNoWait(w);						   \
}

#else /* not PARALLEL */

#define Run_Parallel(C)
#define Start_Parallel(C)
#define Stop_Parallel

#endif /* PARALLEL */

extern TAGGED luther_await_nonvar PROTO((worker *, TAGGED));


#endif /* PARALLEL_H */
