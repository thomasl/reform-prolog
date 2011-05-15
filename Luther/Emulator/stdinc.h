/*
 * stdinc.h - frequently used macro definitions and types.
 * 
 * Patric Hedlin.....Mon Jul  4 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */

#ifndef STDINC_H
#define STDINC_H


#if defined(NDEBUG)
#  define _DEBUG_(code)
#else
#  define  DEBUG
#  define _DEBUG_(code)	code
#endif


#define STRCMP_EQUAL	0


/* Scope definitions. */
/*
#define PUBLIC
#define PRIVATE	static
*/

#define GLOBAL
#define LOCAL	static

#define EXTERN	extern


/* Cosmetic macro definitions. */

#define PROCESS     void
#define PROCEDURE   void

#define forever	for(;;)


#if ! defined(NIL)
#  define NIL	0
#endif

#define	VOID	(0xffffffff)

#define FAILURE	0       /* indicates failure (used as false). */
#define SUCCESS	1       /* indicates success (used as true).  */


#define not(expr)	(!(expr))

#define inc(value)	(++value)
#define dec(value)	(--value)

#define neg(expr)	(((expr) < 0) ? TRUE : FALSE)
#define pos(expr)	(((expr) > 0) ? TRUE : FALSE)

#if ! defined(abs)
#define abs(expr)	(neg(expr) ? -(expr) : (expr))
#endif

#define sgn(expr)	(neg(expr) ? -1 : (pos(expr) ? 1 : 0))

#define min(x, y)	(((x) < (y)) ? (x) : (y))
#define max(x, y)	(((x) > (y)) ? (x) : (y))

#define odd(x)		(((x) & 1) == 0 ? FALSE : TRUE)
#define even(x)		(((x) & 1) == 0 ? TRUE : FALSE)

#define pot(x)		(((x) & (x - 1)) == 0 ? TRUE : FALSE)


/* Semi-Standard type definitions. */

typedef unsigned char byte;

typedef   signed long int integer;
typedef unsigned long int natural;

typedef void *opaque;


#if defined(FALSE)

typedef int BOOL;

#else
/*
typedef enum { FALSE = 0, false = 0, TRUE = 1, true = 1 } boolean;
*/
typedef enum { FALSE = 0, TRUE = 1 } BOOL;

#endif


typedef enum {
  COMPARE_LESS      = -1,
  COMPARE_EQUAL     =  0,
  COMPARE_GREATER   =  1,
  COMPARE_UNDEFINED =  2
} CompareType;


#endif /* STDINC_H */
