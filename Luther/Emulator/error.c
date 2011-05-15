/*
 * error.c
 *
 * Johan Bevemyr.....Tue Jun  4 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include <signal.h>

#include "luther.h"
#include "display.h"

#if defined(unix)
int sys_nerr;
int errno;
#endif

#if defined(PARALLEL)

CHILD_PID_TYPE *child_pid;
int nr_children;

#endif /* PARALLEL */

#if defined(THINK_C)
#  include <SegLoad.h>
#endif

void luther_exit(e_code)
    int e_code;
{
#if defined(PARALLEL)
    fprintf(stderr, "died\n");

    if (worker_nr == 0)
      {
	int i;

	for (i = 1; i <= nr_children; i++)
	  {
#if defined(USE_THREADS)
	    Thread_Cancel(child_pid[i]);
#else
	    (void) kill(child_pid[i],SIGKILL);
#endif
	  }
      }
    else
      {
#if defined(USE_THREADS)
	Thread_Cancel(child_pid[0]);
#else
	(void) kill(child_pid[0], SIGUSR1);
	exit(e_code);
#endif
      }
#endif /* PARALLEL */

    remove_semaphores();
#if defined(SHARED_MEMORY)
    remove_shared_memory();
#endif

    if (stderr != currerr)
      fclose(currerr);

#if defined(THINK_C)
    ExitToShell();
#else
#  if defined(unix) && defined(DEBUG)
     (void) kill(getpid(), SIGTRAP); /* Break to debugger if any. */
#  else
     exit(e_code);
#  endif
#endif
}


BOOL luther_error(error,arg,w)
     int	error;
     TAGGED	arg;
     worker	*w;
{
  switch (error) {

  case E_PUT_ARG:
    {
      PL_Print1(stderr,"{Error: illegal arithmetic expression}\n");
      PL_Print1(stderr,"{Error: put(");
      display_term(stderr,arg,w);
      PL_Print1(stderr,") - illegal argumnet}\n");
    }
    break;

  case E_OPEN_FILE:
    {
      PL_Print2(stderr,"{Error: unable to open file '%s'}\n", GetString(arg,w));
    }
    break;

  case E_POPEN_FILE:
    {
      PL_Print2(stderr,"{Error: unable to run command '%s'}\n",
		GetString(arg,w));
    }
    break;

  case E_FILE_SPEC:
    {
      PL_Print1(stderr,"{Error: ");
      display_term(stderr,arg,w);
      PL_Print1(stderr," - invalid file specification}\n");
    }
    break;

  case E_NR_FILES:
    {
      PL_Print1(stderr,"{Error: unable open more files}\n");
    }
    break;

  case E_ILLEGAL_GOAL:
    {
      PL_Print1(stderr,"{Error: ");
      display_term(stderr,arg,w);
      PL_Print1(stderr," - illegal goal}\n");
    }
    break;

  case E_PRED_NOT_DYN:
    {  
      PL_Print4(stderr,"{Warning: The predicate %s:%s/%d is not dynamic}\n",
		GetString((((definition *) arg)->module),w),
		GetString(FunctorToAtom(((definition *) arg)->name),w),
		ArityOf(((definition *) arg)->name));
    }
    break;

  case E_PRED_NOT_DEF:
    {  
      PL_Print4(stderr,"{Warning: The predicate %s:%s/%d is undefined}\n",
		GetString((((definition *) arg)->module),w),
		GetString(FunctorToAtom(((definition *) arg)->name),w),
		ArityOf(((definition *) arg)->name));
    }
    break;

  case E_ILLEGAL_STREAM:
    {
      PL_Print1(stderr,"{error: ");
      display_term(stderr,arg,w);
      PL_Print1(stderr," - illegal stream specifier}\n");
    }
    break;

  case E_ILLEGAL_IN_STREAM:
    {
      PL_Print1(stderr,"{error: ");
      display_term(stderr,arg,w);
      PL_Print1(stderr," - illegal in stream specifier}\n");
    }
    break;

  case E_ILLEGAL_OUT_STREAM:
    {
      PL_Print1(stderr,"{error: ");
      display_term(stderr,arg,w);
      PL_Print1(stderr," - illegal out stream specifier}\n");
    }
    break;

  case E_ILLEGAL_AR_EX:
    {
#ifdef PARALLEL
      PL_Print2(stderr,"{Error<%d>: illegal argument ",w->pid);
      display_term(stderr,arg,w);
      PL_Print1(stderr," in arithmetic expression}\n");
#else
      PL_Print1(stderr,"{Error: illegal argument ");
      display_term(stderr,arg,w);
      PL_Print1(stderr," in arithmetic expression}\n");
#endif
    }
    break;

  case E_DIV_ZERO:
    {
      PL_Print1(stderr,"{error: division by zero}\n");
    }
    break;

  case E_CLAUSE_NOT_DYNAMIC:
    {
      PL_Print1(stderr,"{Error: predicate ");
      display_term(stderr,arg,w);
      PL_Print1(stderr," is not dynamic}\n");
    }
    break;

  case E_INSTANTIATION_ERROR:
    {
      PL_Print1(stderr,"{Error: instantiation error}\n");
    }
    break;

  case E_NOT_REDEFINED:
    {
      PL_Print3(stderr,"{Warning: %s/%d - not redefined}\n",
		GetString(FunctorToAtom(arg),w),
		ArityOf(arg));
    }
    break;

  default:
    PL_Print1(stderr,"Error: luther_error, no such error type\n");
  }
  return FALSE;
}

