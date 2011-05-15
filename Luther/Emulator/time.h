/*
 * time.h
 *
 * Johan Bevemyr.....Mon Jun 10 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef TIME_H
#define TIME_H


extern long usertime PROTO((void));
extern long systime PROTO((void));
extern long pagefaults_no_io PROTO((void));
extern long pagefaults_ph_io PROTO((void));
extern double walltime PROTO((void));


#endif /* TIME_H */
