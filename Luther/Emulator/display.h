/*
 * display.h
 *
 * Johan Bevemyr.....Tue Jun  4 1991
 * Patric Hedlin.....Wed Aug 24 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef DISPLAY_H
#define DISPLAY_H


PROTOTYPE(void, display_term,    (FILE *, TAGGED term, worker *));
PROTOTYPE(void, display_term_fd, (int fd, TAGGED term, worker *));

PROTOTYPE(void, write_term,    (FILE *, TAGGED, worker *));
PROTOTYPE(void, write_float,   (FILE *, TAGGED number, integer format, integer precision, worker *));
PROTOTYPE(void, write_radix,   (FILE *, TAGGED number, integer format, integer radix, worker *));
PROTOTYPE(void, write_integer, (FILE *, TAGGED number, integer format, integer precision, worker *));


#endif /* DISPLAY_H */
