/*
 * Copyright 1988 MIPS Computer Systems Inc.  All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of MIPS not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  MIPS makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty or support of any kind.
 */

/*
 * Tournament barrier without reset.
 *
 * Adapted from an algorithm presented in
 *	Two Algorithms for Barrier Synchronization
 *	Debra Hensgen, Raphael Finkel, and Udi Manber
 *	International Journal of Parallel Programming, Vol 17, No 1, 1988
 * This version is considerably simpler than the cited algorithm,
 * because the need for reset is eliminated by using counters
 * instead of flags.
 *
 * Earl Killian, 6 November 1989
 */

/*
 * Processor 0 competes with processors 1, 2, 4, 8, ...
 * Processor 1 competes with processor 0.
 * Processor 2 competes with processors 3 and 0.
 * Processor 3 competes with processor 2.
 * Processor 4 competes with processor 5, 6, and 0.
 * .
 * .
 * .
 * Processor 0 wins and everyone else loses in this tournament.
 *
 * The use of counters eliminates the need to reset the barrier.
 * Requires N shared writes.  Takes only O(log2(N)) steps, but
 * executes in O(N) time when the writes are serialized.
 * Serialization occurs on a bus, or even in a parallel interconnect
 * at the processor <-> interconnect boundary when large cache blocks
 * cause processors to see more than log2(N) writes.
 *
 * When a write-invalidate protocol is used, or write-update where
 * update hits are expensive, it may be useful to put each counter
 * into a separate cache block.  This can be accomplished by increasing
 * the BLOCKSIZE define below.
 */

#define BLOCKSIZE 1
/*shared*/ static volatile unsigned barrier_count[MAXPROCS][BLOCKSIZE];

static void
tbarrier (unsigned id, unsigned nprocessors)
{
  register unsigned b, t;

  t = barrier_count[id][0] + 1;
  b = 1;
  /* while there is someone to compete with and we are winner */
  /* (we're the winner if bit is clear, loser if bit is set) */
  while ((id | b) < nprocessors && (id & b) == 0) {
    /* we will compete and win -- wait for loser to admit defeat */
    do ; while (barrier_count[id | b][0] != t);
    b <<= 1;
  } 
  sync ();	/* all writes before barrier must now be complete */
  barrier_count[id][0] = t;	/* shared write to admit defeat
				   or claim championship */
  /* wait for champion to announce */
  do ; while (barrier_count[0][0] != t);
}

