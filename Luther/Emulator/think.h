/*
 * think.h
 *
 * Johan Bevemyr.....Thu Sep  3 1992
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef THINK_H
#define THINK_H


#if defined(THINK_C)

#define ArgCnt	3


PROTOTYPE(long, usertime, (void));
PROTOTYPE(long, systime,  (void));

PROTOTYPE(char*, get_application_name, (char *));

#endif


#endif /* THINK_H */
