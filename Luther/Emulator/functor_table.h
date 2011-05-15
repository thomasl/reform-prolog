/*
 * functor_table.h
 *
 * Johan Bevemyr.....Tue Feb  8 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef FUNCTOR_TABLE_H
#define FUNCTOR_TABLE_H


extern functor_bucket **functortable;

extern TAGGED store_functor PROTO((TAGGED, TAGGED, worker *));
extern void init_functortable PROTO((worker *));


#endif /* FUNCTOR_TABLE_H */
