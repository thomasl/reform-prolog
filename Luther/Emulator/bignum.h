/*
 * bignum.h
 *
 * Patric Hedlin.....Fri Jun 10 1994
 *
 *
 * Description:
 *
 *   The Big Number implementation is devided into a multi layer package
 * in order to isolate tasks such as memory management and conformance to
 * external libraries.
 *   The "bignum"-layer is the actual interface to the bignum package, used
 * by the arithmetic package in Luther. The interface is responsible for
 * memory management (in accordance with single assignment).
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */

#ifndef BIGNUM_H
#define BIGNUM_H


#include "luther.h"

#include "bnlib.h"


#define BIG_STR_SIZE 1024


#define bntof(x)	bn_get_flt(x)
#define ftobn(w,v)	make_bignum_of_float(w,v)

/*
 * Prototypes of bignum functions.
 *
 */
PROTOTYPE(BigNumber *, get_bignum, (TAGGED));

PROTOTYPE(TAGGED, make_bignum, (worker *, size_t));
PROTOTYPE(TAGGED, make_atomspace_bignum, (worker *, size_t));

PROTOTYPE(int, make_bignum_copy, (BigNumber *, BigNumber *));

PROTOTYPE(TAGGED, bignum_check, (const TAGGED));

PROTOTYPE(TAGGED, make_bignum_of_int, (worker *, integer));
PROTOTYPE(TAGGED, make_bignum_of_float, (worker *, double));

PROTOTYPE(TAGGED, make_bignum_of_string, (worker *, const char *, const natural base));
PROTOTYPE(TAGGED, make_atomspace_bignum_of_string, (worker *, const char *, const natural base));

PROTOTYPE(TAGGED, make_number_of_string, (worker *, char *, const natural base));
PROTOTYPE(TAGGED, make_atomspace_number_of_string, (worker *, char *, const natural base));

PROTOTYPE(void, make_string_of_bignum, (const BigNumber *, char *string, const natural base));

PROTOTYPE(void, print_bignum, (FILE *, const BigNumber*, const natural base));

PROTOTYPE(BOOL, bignum_eq, (BigNumber *, BigNumber *));
PROTOTYPE(BOOL, bignum_ne, (BigNumber *, BigNumber *));
PROTOTYPE(BOOL, bignum_lt, (BigNumber *, BigNumber *));
PROTOTYPE(BOOL, bignum_gt, (BigNumber *, BigNumber *));
PROTOTYPE(BOOL, bignum_le, (BigNumber *, BigNumber *));
PROTOTYPE(BOOL, bignum_ge, (BigNumber *, BigNumber *));

PROTOTYPE(int, bignum_cmp, (BigNumber *, BigNumber *));

PROTOTYPE(TAGGED, bignum_abs, (worker *, BigNumber *));
PROTOTYPE(TAGGED, bignum_neg, (worker *, BigNumber *));

PROTOTYPE(TAGGED, bignum_inc, (worker *, BigNumber *));
PROTOTYPE(TAGGED, bignum_dec, (worker *, BigNumber *));

PROTOTYPE(TAGGED, bignum_add, (worker *, BigNumber *, BigNumber *));
PROTOTYPE(TAGGED, bignum_sub, (worker *, BigNumber *, BigNumber *));
PROTOTYPE(TAGGED, bignum_mul, (worker *, BigNumber *, BigNumber *));
PROTOTYPE(TAGGED, bignum_div, (worker *, BigNumber *, BigNumber *));
PROTOTYPE(TAGGED, bignum_mod, (worker *, BigNumber *, BigNumber *));
PROTOTYPE(TAGGED, bignum_pow, (worker *, BigNumber *, integer));

PROTOTYPE(TAGGED, bignum_not, (worker *, BigNumber *));
PROTOTYPE(TAGGED, bignum_and, (worker *, BigNumber *, BigNumber *));
PROTOTYPE(TAGGED, bignum_ior, (worker *, BigNumber *, BigNumber *));
PROTOTYPE(TAGGED, bignum_xor, (worker *, BigNumber *, BigNumber *));

PROTOTYPE(TAGGED, bignum_msb, (worker *, BigNumber *));

PROTOTYPE(TAGGED, bignum_rng, (worker *, BigNumber *, natural, natural));

PROTOTYPE(TAGGED, bignum_rsh, (worker *, BigNumber *, integer));
PROTOTYPE(TAGGED, bignum_lsh, (worker *, BigNumber *, integer));


#endif /* BIGNUM_H */
