/*
 * signal.h
 *
 * Johan Bevemyr.....Tue Jun 29 1993
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef SIGNAL_H
#define SIGNAL_H


extern void install_sigvec PROTO((int, void (*func)()));
extern void init_my_getchar PROTO((void));
extern int my_getchar PROTO((int));
extern void readstring PROTO((int, char *));
extern void writestring PROTO((int, char *));

extern void interrupt_handler PROTO((int));
extern void usr1_interrupt_handler PROTO((int));

extern void signal_handler PROTO((int));


#endif /* SIGNAL_H */
