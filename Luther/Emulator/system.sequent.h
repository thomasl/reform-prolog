/*
 * system.sequent.h
 *
 * Johan Bevemyr.....Mon Oct  5 1992
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 
#ifndef SYSTEM_SEQUENT_H
#define SYSTEM_SEQUENT_H

#define MAXSEMOPS 10

/* The code for swap_il() is copied from muse.14.06 by Roland Karlsson at SICS.
 */

#ifdef __GNUC__
#  ifdef SEMILOCK
#    define swap_il(addr,reg) ({ uword _ret; _ret = (uword *) *addr; *addr = reg; _ret; })
#  else
#    define swap_il(addr,reg)					\
({								\
   uword _ret;							\
   asm volatile ("xchgl %0,%1"					\
	: "=q" (_ret), "=m" (*(addr))	/* Output %0,%1 */	\
	: "m"  (*(addr)), "0"  (reg));	/* Input (%2),%0 */	\
   _ret;							\
})

#  endif
#endif


extern char* mmap_global PROTO((long));

#endif /* SYSTEM_SEQUENT_H */
