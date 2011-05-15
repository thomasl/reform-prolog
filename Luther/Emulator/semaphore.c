/*
 * semaphore.c
 *
 * Johan Bevemyr......Wed Aug 21 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"


#if defined(SEMAPHORE)

/**********************************************************************
 * 
 */

void init_spin_semaphores(w,n)
    worker *w;
    s32 n;
{
  register int i;

  for (i = 0; i < n; i++)
    {
      w->global->pid_sema[i] = 0;
    }

  for (i = 0; i < MAX_SPIN_SEM_NR; i++)
    {
      w->global->semaphores[i] = 0;
    }

  w->global->semaphores[SEMA_EVENT] = 1;
  w->global->semaphores[SEMA_ALLOCATE_SEGMENT] = 1;

}

/**********************************************************************
 * 
 */

#if defined(HAVE_SEMAPHORE)
# if defined(USE_THREADS)

Thread_Sem_Type *sem;

void init_semaphores(w,n)
    worker *w;
    s32 n;
{
  int i;

  sem = (Thread_Sem_Type *) malloc(sizeof(Thread_Sem_Type)*(MAX_SEM_NR+n));

  if (sem == NULL)
    {
      FatalError("Cannot allocate semaphores");
    }

  for(i=0 ; i < MAX_SEM_NR+n ; i++)
    {
      if (Thread_Sem_Init(&sem[i],0) < 0)
	{
	  FatalError("Cannot initialize semaphore");
	}
    }

  return;
}
# else /* not USE_THREADS */

int    sem;
struct sembuf sdropop[1];

void init_semaphores(w,n)
    worker *w;
    s32 n;
{
  /*   In case this is a SYSTEM V installation the sumun union is not
   * defined, we thus define our own version to work across unix systems.
   */
  union semun_arg { long val; struct semid_ds *buf; ushort *array; } arg;

  int i;
  
  sdropop[0].sem_num = 0;
  sdropop[0].sem_op = 1;
  sdropop[0].sem_flg = 0;

  if((sem = semget(IPC_PRIVATE, MAX_SEM_NR+n, 200)) == -1)
    {
      perror("semget couldn't initialize semaphores");
      luther_exit(0);
    }

  arg.val = 0;
  
  for(i = 0; i != MAX_SEM_NR+n ; i++)
    {
      if(semctl(sem, i, SETVAL, arg) == -1)
	{
	  perror("semctl couldn't initialize semaphores");
	  luther_exit(0);
	}
    }
}
# endif /* USE_THREADS */
#else /* not HAVE_SEMAPHORE */
# if defined(USE_KSR_THREADS)

pthread_barrier_t work_barr

void init_semaphores(w,n)
    worker *w;
    s32 n;
{
  w->global->worker_barrier = &work_barr;

  pthread_barrier_init(w->global->worker_barrier,
		       pthread_barrierattr_default,
		       n+1);
}
# else

void init_semaphores(w,n)
     worker *w;
     s32 n;
{
}

# endif /* USE_KSR_THREADS */


#endif /* HAVE_SEMAPHORE */


/**********************************************************************
 * 
 */

void remove_semaphores()
{
#if defined(HAVE_SEMAPHORE)
# if defined(USE_THREADS)
  int i;

  for(i=0 ; i < MAX_SEM_NR+orig_nr_of_workers ; i++)
    {
      Thread_Sem_Dest(&sem[i]);
    }
# else /* not USE_THREADS */
  if(semctl(sem, 0, IPC_RMID, 0) == -1)
    {
      perror("semctl remove semaphores");
    }
# endif /* USE_THREADS */
#endif
}

#else /* SEMAPHORE */

/**********************************************************************
 * 
 */

void init_semaphores(w,n)
    worker *w;
    s32 n;
{}

/**********************************************************************
 * 
 */

void init_spin_semaphores(w,n)
    worker *w;
    s32 n;
{}

/**********************************************************************
 * 
 */

void remove_semaphores()
{}

#endif /* SEMAPHORE */

#if defined(PARALLEL)
/* We have two alternatives:
 *
 * 1. Use system semaphores for all barrier synchronization.
 *    This may be rather slow and unacceptable when executing
 *    fine grained parallelism. 
 *
 * 2. Use user implemented semaphores for barrier synchronization
 *    during execution of a goal. System semaphores are used when
 *    the top loop is reached. In theory this approach has the 
 *    advantage that we can handle fine grained parallelism and
 *    still let others use the machine when we do not need it. However,
 *    a side effect of this is that the sequential worker will
 *    occupy a processor while waiting for the parallel workers to
 *    complete their work, thus the efficiency goes down.
 */


#if defined(ACTIVE_SYNCH)

void synchronize(w)
     worker *w;
{
 start:

  switch (w->synch_state)
    {
    case SYNCH_FIRST:
      GrabPrivateSema(w);

      w->synch_state = SYNCH_ACTIVE;

      break;

    case SYNCH_ACTIVE:
      /* The sequential worker must wait until all co-workers are done.
       * Therefore each co-worker signals the semaphore SEMA_DONE when
       * done. 
       */
      DropDone(1,w);

      SpinGrabPrivateSema(w);

      break;

    case SYNCH_SLEEP:
      /* The sequential worker must wait until all co-workers are done.
       * Therefore each co-worker signals the semaphore SEMA_DONE when
       * done. 
       */
      DropDone(1,w);

      GrabPrivateSema(w);

      w->synch_state = SYNCH_ACTIVE;

      break;
    }

  return;
}
#else /* not ACTIVE_SYNCH */

#if defined(USE_KSR_THREADS)
void synchronize(w)
     worker *w;
{
 start:
  switch(w->synch_state)
    {
    case SYNCH_ACTIVE:
    case SYNCH_SLEEP:
      pthread_barrier_checkout(w->global->worker_barrier,w->pid);

    case SYNCH_FIRST:
      pthread_barrier_checkin(w->global->worker_barrier,w->pid);
      w->synch_state = SYNCH_SLEEP;

      if (w->pid > w->global->active_workers)
	goto start;
      else
	break;
    }

  return;
}    

#else /* not USE_KSR_THREADS */

void synchronize(w)
     worker *w;
{
  switch(w->synch_state)
    {
    case SYNCH_ACTIVE:

    case SYNCH_SLEEP:
      DropDone(1,w);

    case SYNCH_FIRST:
      GrabPrivateSema(w);
      w->synch_state = SYNCH_SLEEP;
      break;
    }

  return;
}

#endif /* USE_KSR_THREADS */
#endif /* ACTIVE_SYNCH */
#else /* not PARALLEL */

void synchronize(w)
     worker *w;
{
  return;
}
#endif /* PARALLEL */
