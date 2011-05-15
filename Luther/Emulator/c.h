/*
 * c.h
 *
 * Johan Bevemyr.....Fri Jun  7 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef C_H
#define C_H


#define ANY	1	/* Number of elements in dummy array. */


#if ! defined(__STDC__)
#  if ! defined(NULL)
#    define NULL 0
#  endif
#endif

#if ! defined(__GNUC__)
#  define inline
#endif

#if defined(ANSI_C)
#  define PROTO(fpl)	fpl

#  define PROTOTYPE(type, name, fpl) type name fpl
#else
#  define const
#  define signed
#  define volatile

#  define PROTO(fpl)	()
#  define PROTOTYPE(type, name, fpl) type name ()
#endif


typedef signed short int s16;
typedef unsigned short int u16;

typedef   signed long int sword;
typedef unsigned long int uword;

typedef sword s32;
typedef uword u32;


#endif /* C_H */
