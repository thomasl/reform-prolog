/*
 * system.sun.h
 *
 * Johan Bevemyr.....Tue Jul 21 1992
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 
#ifndef SYSTEM_SUN_H
#define SYSTEM_SUN_H

#define MAXSEMOPS 20

/* The code for swam_il() is copied from muse.14.06 by Roland Karlsson at SICS.
 */

#if defined(__GNUC__)
#  if defined(SEMILOCK)
#    define swap_il(addr,reg) ({ uword _ret; _ret = (uword *) *addr; *addr = reg; _ret; })
#  else
#    define swap_il(addr,reg)					          \
({ uword _ret;							          \
   asm volatile ("swap %1,%0"					          \
	: "=r" (_ret), "=m" (*(addr))	/* Output %0,%1 */	          \
	: "m"  (*(addr)), "0" (reg));	/* Input (%2),%0 */	          \
   _ret;							          \
})                                                              

#  endif
#endif


extern char *mmap_global PROTO((long));
extern void  remove_shared_memory PROTO((void));
#endif /* SYSTEM_SUN_H */
