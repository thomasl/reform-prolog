/*    -*- C -*- 
 *    File:	 threads.h  (~jb/Reform/Luther/Emulator/threads.h)
 *    Author:	 Johan Bevemyr
 *    Created:	 Sat Mar  9 17:09:10 1996
 *    Copyright:	(c)Johan Bevemyr
 *    Purpose:   Interface to different thread packages.
 */ 

#ifndef THREADS_H
#define THREADS_H


/*
 *   Supplied macros:
 *
 *      Thread_Create(PidLocation, StartFunction, StartFunctionArgs)
 *      Thread_Cancel(ThreadID)
 *
 *      Thread_Sem_Wait(Semaphore)
 *      Thread_Sem_Post(Semaphore)
 *      Thread_Sem_Init(Semaphore)
 *      
 */

/**********************************************************************
 * KSR threads (a variant of posix threads)
 */
 
#if defined(USE_KSR_THREADS)
# include <pthread.h>
# define USE_THREADS
# define CHILD_PID_TYPE pthread_t
# undef  SHARED_MEMORY

# define Thread_Create(PidLocation,StartFunc,StartArgs)			   \
                pthread_create(PidLocation, pthread_attr_default,	   \
			       StartFunc, StartArgs)
  
# define Thread_Cancel(TID) (void) pthread_cancel(TID)
# define Thread_Self()      pthread_self()

#endif /* USE_KSR_THREADS */

#if defined(USE_PTHREADS)
/**********************************************************************
 * Posix threads 
 */

# include <pthread.h>
# include <semaphore.h> 

# define USE_THREADS
# define CHILD_PID_TYPE pthread_t

# undef  SHARED_MEMORY

# define Thread_Create(PidLocation,StartFunc,StartArgs)			   \
{									   \
  int tc_ret;								   \
  pthread_attr_t attr;							   \
									   \
  pthread_attr_init(&attr);						   \
  pthread_attr_setscope(&attr,PTHREAD_SCOPE_SYSTEM);			   \
									   \
  tc_ret = pthread_create(PidLocation, NULL, StartFunc, StartArgs);	   \
									   \
  tc_ret;								   \
}

# define Thread_Cancel(TID) (void) pthread_cancel(TID)
# define Thread_Self()      pthread_self()

# define Thread_Sem_Type                sem_t
# define Thread_Sem_Wait(Semaphore)     sem_wait(Semaphore)
# define Thread_Sem_Post(Semaphore)     sem_post(Semaphore)
# define Thread_Sem_Init(Semaphore,Val) sem_init(Semaphore,1,Val)
# define Thread_Sem_Dest(Semaphore)     sem_destroy(Semaphore)

extern sem_t *sem;
    
#endif /* USE_PTHREADS */

#if defined(USE_SOLARIS_THREADS)
/**********************************************************************
 * SOLARIS threads 
 */

# include <thread.h>
# include <synch.h>

# define USE_THREADS
# define CHILD_PID_TYPE thread_t

# undef  SHARED_MEMORY

# define Thread_Create(PidLocation,StartFunc,StartArgs)			   \
            thr_create(NULL,(int) NULL,StartFunc,StartArgs,THR_BOUND,PidLocation)
# define Thread_Cancel(TID) thr_kill(TID,SIGKILL)
# define Thread_Self()      thr_self()

# define Thread_Sem_Type                sema_t
# define Thread_Sem_Wait(Semaphore)     sema_wait(Semaphore)
# define Thread_Sem_Post(Semaphore)     sema_post(Semaphore)
# define Thread_Sem_Init(Semaphore,Val) sema_init(Semaphore,Val,(int) NULL,NULL)
# define Thread_Sem_Dest(Semaphore)     sema_destroy(Semaphore)

extern sema_t *sem;

#endif /* USE_SOLARIS_THREADS */

#endif /* THREADS_H */
