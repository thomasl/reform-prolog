/*
 * labelsort.h
 *
 * Johan Bevemyr.....Wed Feb 19 1992
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef LABELSORT_H
#define LABELSORT_H


typedef struct labelt {
  TAGGED constant;
  s32 offset;
} labelt;

extern void labelsort PROTO((code *, int));
    

#endif /* LABELSORT_H */
