/*
 * signal.c - Luther signal handling.
 *
 * Johan Bevemyr.....Tue Jun 29 1993
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"
#include "debug.h"
#include "signal.h"
#include "engine.h"

static int is_in_interrupt = 0;

#define BUFSIZE 255
#define SH_Print1(String) { sprintf(tmpout,String);writestring(0,tmpout);}

static char readbuffer[BUFSIZE];
static int c_count, c_max;

/**********************************************************************
 * Name: 
 * 
 * Desc: 
 */

void init_my_getchar()
{
  c_count = c_max = 0;
}

/**********************************************************************
 * Name: 
 * 
 * Desc: 
 */

int my_getchar(in)
     int in;
{
  if (c_count == c_max)
    {
      c_max = read(in,readbuffer,BUFSIZE);

      if (c_max <= 0)
	{
	  /* end of file has been reached, exit */
	  luther_exit(0);
	}

      c_count = 1;
      return (int) readbuffer[0];
    }
  else
    {
      return (int) readbuffer[c_count++];
    }
}

/**********************************************************************
 * Name: 
 * 
 * Desc: 
 */

void readstring(in, x)
     int in;
     char *x;
{
   int i = 0;

   init_my_getchar();

   x[0] = my_getchar(in);
   while((x[i] != '\n') && (x[i] != EOF) && (i < 255)) {
	if (x[i] == 8) {
	   if (i>0) i--;
	} else {
	   i++;
	}
	x[i] = my_getchar(in);
   }
   x[++i] = 0;
}

/**********************************************************************
 * Name: 
 * 
 * Desc: 
 */

void writestring(out, x)
     int out;
     char *x;
{
  register int size;

  write(out, x, strlen(x));
}

/**********************************************************************
 * Name: 
 * 
 * Desc: 
 */

void install_sigvec(sig,new)
     int sig;
     void (*new)();
{
#if defined(HAVE_SIGVEC)
  struct sigvec newsig;

  bzero((char *) &newsig, sizeof(newsig));
  
# if defined(SEQUENT)
  newsig.sv_handler = (int (*)()) new;
# else
  newsig.sv_handler = new;
# endif /* SEQUENT */

  sigvec(sig, &newsig, NULL);

#else /* No sigvec() available. */
  (void) signal(sig,new);
#endif
}

/**********************************************************************
 * Name: 
 * 
 * Desc: 
 */

void reinstall_sigvec(sig,new)
     int sig;
     void (*new)();
{
#if !defined(HAVE_SIGVEC)
  (void) signal(sig,new);
#endif
}

/**********************************************************************
 * Name: 
 * 
 * Desc: 
 */

void interrupt_handler(sig)
     int sig;
{
  register worker *w = worker_set;
  char tmp[255], tmpout[255];

  if (is_in_interrupt == 1)
    {
      return;
    }
  else
    {
      is_in_interrupt = 1;
    }

  /* If not sequential worker */
#ifdef PARALLEL

#if defined(USE_THREADS)
  worker_nr = 0;
#endif

  if (worker_nr != 0) 
    {
      if (w->global->debugsig == worker_nr)
	{
	  SH_Print1("{The debugger will first creep -- showing everything}\n");
# if defined(DEBUG)
	  w->debug.debugflag = TRUE;
	  w->debug.debugmode = DBG_CREEP;
# endif
	  w->global->debugsig = 0;	  
	}
	  
      is_in_interrupt = 0;
    }
  else
#endif /* PARALLEL */
    {
    start:
      /* Write and read to and from file identifiers (0 and 1) */
      SH_Print1("\nProlog interruption (h for help)? ");
      readstring(1,tmp);
      
      switch (tmp[0])
	{
	case 'a':
	  {
	    AddEvent(EVENT_INTERRUPT);
	    SH_Print1("{Execution will abort at next call}\n");
	    is_in_interrupt = 0;
	  }
	  break;

	case 'c':
	  {
	    is_in_interrupt = 0;
	  }
	  break;

	case 'd':
	  {
#if defined(DEBUG)
	    int i = atoi(&(tmp[1]));

	    if (i == 0)
	      {
		SH_Print1("{The debugger will first creep}\n");
		w->debug.debugflag = TRUE;
		w->debug.debugmode = DBG_CREEP;
	      }
#ifdef PARALLEL
	    else if (i > w->global->active_workers)
	      {
		SH_Print1("No such worker\n");
		goto error;
	      }
	    else /* Signal worker */
	      {
		w->global->debugsig = i;
# if defined(USE_THREADS)
		Thread_Cancel(child_pid[i]);
# else /* not USE_THREADS */
		(void) kill(child_pid[i],SIGINT);
# endif/* USE_THREADS */
	      }
#endif /* PARALLEL */
#endif /* DEBUG */
	    is_in_interrupt = 0;
	  }
	  break;

	case 'e':
	  luther_exit(1);
	  break;

	case 't':
	  {
	    w->trace_mode = LUTHER_TRACE_ON;
	    is_in_interrupt = 0;
	  }
	  break;

	case '?':
	case 'h':
	default:
	error:
	  {
	    SH_Print1("\nProlog interrupt options: \n");
	    SH_Print1("    a         abort        - abort execution\n");
	    SH_Print1("    c         continue     - do nothing\n");
#if defined(PARALLEL)
	    SH_Print1("    d <nr>    debug        - enter wam-debugger for worker <nr>\n");
#else
	    SH_Print1("    d         debug        - enter wam-debugger\n");
#endif
	    SH_Print1("    e         exit         - cause exit\n");
	    SH_Print1("    h         help         - get this list\n");
	    SH_Print1("    t         trace        - enter tracer\n");
	  }
	  goto start;
	}
    }

  reinstall_sigvec(SIGINT, interrupt_handler);

  return;
}

/**********************************************************************
 * Name: 
 * 
 * Desc: 
 */

BOOL in_usr1_handler = FALSE;

void usr1_interrupt_handler(sig)
     int sig;
{
  if (in_usr1_handler == FALSE)
    in_usr1_handler = TRUE;
  else
    return;

  luther_exit(0);
}

/**********************************************************************
 * Name: 
 * 
 * Desc: 
 */

static BOOL in_signal_handler = FALSE;

void signal_handler(sig)
     int sig;
{
  if (in_signal_handler)
    luther_exit(0);

  in_signal_handler = TRUE;

  switch (sig) {

  case SIGILL:
    FatalError("illegal instruction");
    
  case SIGFPE:
    FatalError("arithmetic exception");

  case SIGBUS:
    FatalError("bus error");

  case SIGSEGV:
    FatalError("segmentation violation");

  default:  
    FatalError("unknown interrupt");
  }    
}
