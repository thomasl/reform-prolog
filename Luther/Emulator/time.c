/*
 * time.c 
 *
 * Johan Bevemyr.....Mon Jun 10 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 * 
 */ 

#include "luther.h"
#include "initial.h"


#if defined(HAVE_GETRUSAGE)

#include <sys/time.h>
#include <sys/types.h>
#include <sys/resource.h>

long usertime()
{
  struct rusage t;

  if (getrusage(RUSAGE_SELF,&t) != 0)
    {
      SystemError("getrusage error");
      return 0;
    }
  return t.ru_utime.tv_sec * 1000 + t.ru_utime.tv_usec / 1000;
}
    
long systime()
{
  struct rusage t;

  if (getrusage(RUSAGE_SELF,&t) != 0)
    {
      SystemError("getrusage error");
      return 0;
    }
  return t.ru_stime.tv_sec * 1000 + t.ru_stime.tv_usec / 1000;
}

double walltime()
{
  struct timeval tp;
  struct timezone tzp;

  gettimeofday(&tp,&tzp);
  return (double) tp.tv_sec +(double) tp.tv_usec/1.0e6;
}

long pagefaults_no_io()
{
  struct rusage t;

  if (getrusage(RUSAGE_SELF,&t) != 0)
    {
      SystemError("getrusage error");
      return 0;
    }
  return t.ru_minflt;
}

long pagefaults_ph_io()
{
  struct rusage t;

  if (getrusage(RUSAGE_SELF,&t) != 0)
    {
      SystemError("getrusage error");
      return 0;
    }
  return t.ru_majflt;
}

#else /* No getrusage() available, use times() */

#if defined(HAVE_TIMES)

#include <sys/time.h>
#include <sys/times.h>

long usertime()
{
  struct tms buffer;

  if (times(&buffer) == -1)
    {
      Error("usertime: can't read user time");
      return 0;
    }
  return (long) (buffer.tms_utime / ticks_per_msecond);
}

long systime()
{
  struct tms buffer;

  if (times(&buffer) == -1)
    {
	Error("systime: can't read system time");
	return 0;
    }

    return (buffer.tms_stime / ticks_per_msecond);
}

double walltime()
{
  struct timeval tp;
  struct timezone tzp;

  gettimeofday(&tp,&tzp);
  return (double) tp.tv_sec +(double) tp.tv_usec/1.0e6;
}

long pagefaults_no_io()
{
  return 0;
}

long pagefaults_ph_io()
{
  return 0;
}

#endif /* HAVE_TIMES */

#endif /* HAVE_GETRUSAGE */

