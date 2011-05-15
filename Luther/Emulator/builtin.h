/*
 * builtin.h
 *
 * Johan Bevemyr.....Wed Jun  5 1991
 * Patric Hedlin.....Tue Jan 10 1995
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef BUILTIN_H
#define BUILTIN_H


PROTOTYPE(void, luther_copy_args, (int, TAGGED *, TAGGED *, TIMETYPE, worker *));


PROTOTYPE(void, initialize_flags, (worker *));
PROTOTYPE(void, init_backtrackable_c, (worker *));


#endif /* BUILTIN_H */
