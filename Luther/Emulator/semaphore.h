/*
 * semaphore.h
 *
 * Johan Bevemyr.....Wed Aug 21 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef SEMAPHORE_H
#define SEMAPHORE_H

/**********************************************************************/

typedef enum {
  SEMA_SDONE,
  SEMA_ONE,
  SEMA_TWO,
  MAX_SEM_NR
} semaphores;

/* MAX_SPIN_SEM_NR is set in constants.h 
 */

typedef enum {
  SEMA_DONE,
  SEMA_EVENT,
  SEMA_ALLOCATE_SEGMENT
} spin_semaphores;

typedef enum {
  SYNCH_FIRST,
  SYNCH_ACTIVE,
  SYNCH_SLEEP
} synch_t;


/* 
 * There are tree alternatives. 
 *
 *   1) Do not use semaphores
 *   2) Use system semaphores and spin-lock semaphores
 *   3) Use only spin-lock semaphores
 *
 */

#if defined(SEMAPHORE)  

#define GrabPrivateSema(W) GrabPrivSemaphore((W->pid)-1,1,W)

#define DropPrivateSema(W,I)						   \
{									   \
  int i = (I);								   \
  while (i--)								   \
    {									   \
      DropPrivSemaphore(i,1,W);				                   \
    }									   \
}

/**********************************************************************
 * Have system semaphores.
 */ 

# if defined(HAVE_SEMAPHORE)
#  if defined(USE_THREADS)
    
#   define GrabPrivSemaphore(S,N,W) GrabSemaphore((S)+MAX_SEM_NR,N,W)
#   define DropPrivSemaphore(S,N,W) DropSemaphore((S)+MAX_SEM_NR,N,W)
#   define GrabSemaphore(S,N,W)						   \
    {									   \
      int gs_i = (N);							   \
      int gs_s = (S);							   \
    									   \
      while(gs_i--)							   \
        {								   \
    	 if (Thread_Sem_Wait(&(sem[gs_s])) < 0)				   \
    	   Error("Grabbing semaphore");					   \
        }								   \
    }
#   define DropSemaphore(S,N,W)						   \
    {									   \
      int ds_i = (N);							   \
      int ds_s = (S);							   \
    									   \
      while(ds_i--)							   \
        {								   \
    	 if (Thread_Sem_Post(&(sem[ds_s])) < 0)				   \
    	   Error("Dropping semaphore");					   \
        }								   \
    } 
#  else /* not USE_PTHREADS */
#   include <sys/types.h>
#   include <sys/ipc.h>
#   include <sys/sem.h>
    
    extern int sem;
    
    /**************************************************
     *
     * Support macros for system semaphores
     *
     */
    
#   define GrabPrivSemaphore(S,N,W) GrabSemaphore((S)+MAX_SEM_NR,N,W)
#   define DropPrivSemaphore(S,N,W) DropSemaphore((S)+MAX_SEM_NR,N,W)
#   define GrabSemaphore(S,N,W) { FooSemaphore(S,-1*(N),W); } 
#   define DropSemaphore(S,N,W) { FooSemaphore(S,N,W); } 
#   define FooSemaphore(S,N,W)						   \
    {									   \
      sdropop[0].sem_num = S;						   \
      sdropop[0].sem_op = N;						   \
    									   \
      forever								   \
        {								   \
          if (semop(sem, sdropop, 1)==-1)				   \
    	{								   \
    	  if (errno == EINTR)						   \
    	    continue;							   \
    	  FatalError("Worker couldn't drop/grab semaphore");		   \
    	}								   \
          else								   \
    	break;								   \
        }								   \
    }									   \
    
    extern struct sembuf sdropop[1];
    
#  endif /* USE_THREADS */
# else /* not HAVE_SEMAPHORE */
#  define GrabPrivSemaphore(S,N,W)					   \
   {									   \
     SpinGrabSemaphore(W->global->pid_sema[S],N,W);			   \
   }
   
#  define DropPrivSemaphore(S,N,W)					   \
   {									   \
     SpinDropSemaphore(W->global->pid_sema[S],N,W);			   \
   }
   
#  define GrabSemaphore(S,N,W)						   \
   {									   \
     SpinGrabSemaphore(W->global->semaphores[S],N,W);			   \
   }
#  define DropSemaphore(S,N,W)						   \
   {									   \
     SpinDropSemaphore(W->global->semaphores[S],N,W);			   \
   }
# endif /* HAVE_SEMAPHORE */

/**********************************************************************
 * Spin lock semaphores
 * These are used for active waiting.
 */

#define AcquireLock(LOCK)						   \
{									   \
  TAGGED al_dummy;							   \
									   \
  w->stats->hva_init_count += 1;					   \
  SpinLockSemaphore(al_dummy,&(LOCK));					   \
}

#define ReleaseLock(LOCK) LOCK = 0

#define SPIN_SEMA_LOCK 0xffffffff

#define SpinLockSemaphore(Val,Sem)					   \
{									   \
  register volatile u32 *sl_pos = Sem;					   \
									   \
  KSR1(DECLARE_SWAP_VAL);						   \
									   \
  forever								   \
    {									   \
      if ((Val = swap_il(sl_pos,SPIN_SEMA_LOCK)) != SPIN_SEMA_LOCK)	   \
	{								   \
	  break;							   \
	}								   \
      while (*sl_pos == SPIN_SEMA_LOCK);				   \
    }									   \
}

#define SpinGrabSemaphore(S,N,W)					   \
{									   \
  register u32 s_val;							   \
  register volatile u32 *s_pos;						   \
									   \
  s_pos = &(S);								   \
									   \
  forever								   \
    {									   \
      while ((s_val = *s_pos) == SPIN_SEMA_LOCK);			   \
									   \
      if (s_val >= N)							   \
        {								   \
          SpinLockSemaphore(s_val,s_pos);				   \
          if (s_val >= N)						   \
            {								   \
	      *s_pos = s_val - N;					   \
	      break;							   \
            }								   \
          else								   \
            {								   \
	      *s_pos = s_val;						   \
            }								   \
        }								   \
    }									   \
}

#define SpinDropSemaphore(S,N,W)					   \
{									   \
  register u32 s_val;							   \
  register u32 *s_pos;							   \
									   \
  s_pos = &(S);								   \
  SpinLockSemaphore(s_val,s_pos);					   \
  *s_pos = s_val + N;							   \
}

/**********************************************************************
 * Spin semaphore interface
 */

#define SpinDropSema(S,N,W) SpinDropSemaphore(w->global->semaphores[S],N,W)
#define SpinGrabSema(S,N,W) SpinGrabSemaphore(w->global->semaphores[S],N,W)

#define SpinGrabPrivateSema(W)						   \
{									   \
  SpinGrabSemaphore(W->global->pid_sema[(W->pid)-1],1,W);		   \
}

#define SpinDropPrivateSema(W,I)					   \
{									   \
   int sdp_i = (I);							   \
   while (sdp_i--)							   \
     {									   \
       SpinDropSemaphore(W->global->pid_sema[sdp_i],1,W);		   \
     }									   \
}

#else /* SEMAPHORES */

#define GrabPrivateSema(W)
#define DropPrivateSema(W,I)
#define SpinGrabSemaphore(S,N,W)   
#define SpinDropSemaphore(S,N,W) 
#define SpinGrabSema(S,N,W)   
#define SpinDropSema(S,N,W) 
#define GrabSemaphore(W)
#define DropSemaphore(W)
#define AcquireLock(LOCK)
#define ReleaseLock(LOCK)

#endif /* SEMAPHORES */

#if defined(SYNCH_ACTIVE)
#  define DropDone(I,W) SpinDropSema(SEMA_DONE,I,W)
#  define GrabDone(I,W) SpinGrabSema(SEMA_DONE,I,W)
#else
#  define DropDone(I,W) DropSemaphore(SEMA_SDONE,I,W)
#  define GrabDone(I,W) GrabSemaphore(SEMA_SDONE,I,W)
#endif /* SYNCH_ACTIVE */

extern void init_semaphores PROTO((worker *, s32));
extern void init_spin_semaphores PROTO((worker *, s32));
extern void remove_semaphores PROTO((void));
extern void synchronize PROTO((worker *));


#endif /* SEMAPHORE_H */

/**********************************************************************
 * Barrier synchronization support
 *
 */
#ifdef PARALLEL
#if 1 /* !defined(HAVE_SEMAPHORE) */
# define InitBarrier(W)							   \
{									   \
  w->barrier_count = 1;							   \
									   \
  if (SeqWorker(W))							   \
    {									   \
      w->global->barrier=0;						   \
      w->global->barrier_wait=0;					   \
    }									   \
}
/* Add one to barrier. If the current worker was the last to reach
 * the barrier it resetes the barrier and flags the other workers by
 * encrementing barrier_wait.
 */

# define BarrierSynch(W,StatC)						   \
{									   \
  s32 head_count;							   \
  double start = walltime();						   \
									   \
  SpinLockSemaphore(head_count,&(w->global->barrier));			   \
									   \
  /* not counting our selves */						   \
  if (head_count == w->global->active_workers)				   \
    {									   \
      w->global->barrier = 0;						   \
      w->global->barrier_wait += 1;					   \
      w->barrier_count += 1;						   \
    }									   \
  else									   \
    {									   \
      w->global->barrier = head_count+1;				   \
      while(w->global->barrier_wait != w->barrier_count);		   \
      w->barrier_count += 1;						   \
    }									   \
  									   \
  W->stats->heap_gc_wait_time[StatC] += walltime()-start;		   \
}

# else /* HAVE_SEMAPHORE */

# define InitBarrier(W)							   \
{									   \
  w->barrier_count = 1;							   \
									   \
  if (SeqWorker(W))							   \
    {									   \
      w->global->barrier=0;						   \
    }									   \
}
/* Add one to barrier. If the current worker was the last to reach
 * the barrier it resetes the barrier and flags the other workers by
 * encrementing barrier_wait.
 */

# define BarrierSynch(W,StatC)						   \
{									   \
  s32 head_count;							   \
  double start = walltime();						   \
									   \
  SpinLockSemaphore(head_count,&(w->global->barrier));			   \
									   \
  /* not counting our selves */						   \
  if (head_count == w->global->active_workers)				   \
    {									   \
      w->global->barrier = 0;						   \
      w->barrier_count += 1;						   \
									   \
      DropSemaphore((w->barrier_count & 1 ? SEMA_ONE : SEMA_TWO),	   \
		    w->global->active_workers,W);			   \
    }									   \
  else									   \
    {									   \
      w->global->barrier = head_count+1;				   \
      w->barrier_count += 1;						   \
									   \
      GrabSemaphore((w->barrier_count & 1 ? SEMA_ONE : SEMA_TWO),1,W);	   \
    }									   \
									   \
  W->stats->heap_gc_wait_time[StatC] += walltime()-start;		   \
}
#endif
#else /* not PARALLEL */
# define InitBarrier(W)
# define BarrierSynch(W,C)
#endif /* PARALLEL */
 
