/*
 * error.h
 *
 * Johan Bevemyr.....Sun Jun  2 1991
 * Patric Hedlin.....Mon Sep 26 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef ERROR_H
#define ERROR_H


#if defined(unix)
#  include <errno.h>
#  include <sys/errno.h>
#endif

extern int sys_nerr;
extern int errno;


#define Error(msg) fprintf(stderr, "{Error: %s}\n", msg)

#define MinorError(msg) fprintf(stderr, "{Error: %s...}\n", msg)

#define MajorError(msg) return (fprintf(stderr, "{Error: %s}\n", msg), FALSE)

#define FatalError(msg) fprintf(stderr, "<<Fatal Error: %s>>\n", msg), luther_exit(-1)

#define SystemError(msg) fprintf(stderr, "<<System Error: %s>>\n", msg)


#define BuiltinError(name, pmsg, smsg)					\
{									\
  return fprintf(stderr, "{Error: %s%s%s}\n", name, pmsg, smsg), FALSE;	\
}


enum errortypes {
    E_PUT_ARG,
    E_FILE_SPEC,
    E_OPEN_FILE,
    E_POPEN_FILE,
    E_NR_FILES,
    E_ILLEGAL_GOAL,
    E_PRED_NOT_DYN,
    E_PRED_NOT_DEF,
    E_ILLEGAL_STREAM,
    E_ILLEGAL_IN_STREAM,
    E_ILLEGAL_OUT_STREAM,
    E_ILLEGAL_AR_EX,
    E_DIV_ZERO,
    E_CLAUSE_NOT_DYNAMIC,
    E_INSTANTIATION_ERROR,
    E_NOT_REDEFINED
};


/* functions declared in error.c */

extern void luther_exit PROTO((int));
extern BOOL luther_error PROTO((int, TAGGED, worker *));

extern int nr_children;

#if defined(PARALLEL)
# ifndef CHILD_PID_TYPE
#   define CHILD_PID_TYPE int
# endif

 extern CHILD_PID_TYPE *child_pid;

#endif /* PARALLEL */


#endif /* ERROR_H */
