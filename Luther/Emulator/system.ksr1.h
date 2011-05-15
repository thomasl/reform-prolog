/*
 * system.ksr1.h
 *
 * Johan Bevemyr.....Tue Aug  9 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef SYSTEM_KSR_H
#define SYSTEM_KSR_H

#define MAXSEMOPS 20


#define LockAddress(addr) _gspwt((TAGGED *) AlignDown((uword)addr, 128L))
#define FreeAddress(addr) _rsp((TAGGED *) AlignDown((uword)addr, 128L))


#if defined(__GNUC__)
#  if defined(SEMILOCK)
#    define swap_il(addr,reg) ({ uword _ret; _ret = (uword *) *addr; *addr = reg; _ret; })
#  else
#    define swap_il(addr,reg) /* nothing here, yet */
#  endif
#else
#if 0
#  define SWAP_VAL        _swap_val
#  define DECLARE_SWAP_VAL register TAGGED SWAP_VAL

   extern TAGGED swap_il PROTO((TAGGED *, TAGGED));
#else
#endif
#  define SWAP_VAL        _swap_val
#  define DECLARE_SWAP_VAL register TAGGED SWAP_VAL

#  define swap_il(addr,reg) ( LockAddress(addr), SWAP_VAL = *addr, *addr = reg, \
			      FreeAddress(addr), SWAP_VAL )
#endif


extern char *mmap_global PROTO((long));
extern void  remove_shared_memory PROTO((void));

#endif /* SYSTEM_KSR_H */
