/*
 * bnlib.h - Big Number Library header.
 *
 * Patric Hedlin.....Fri Jun 10 1994
 *
 *
 * Description:
 *
 *   The "bnlib"-layer supports Big Number functionality and hiding of
 * any underlying package (we are using portions of the GMP library at
 * the moment) used in conjunction with it. 
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */

#ifndef BNLIB_H
#define BNLIB_H


#include "luther.h"

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"


#define LIMB_MAX	((unsigned)LIMB_MASK)
#define LIMB_MASK	(-1L)

#define BITS_PER_LIMB   BITS_PER_MP_LIMB
#define BYTES_PER_LIMB  BYTES_PER_MP_LIMB

#define BN_SIZE(x)	(abs(x->size))
#define BN_DATA(x)	(x->data)

/*
 *   sizeofBigNumber(limbs) is used in order to calculate the memory
 * requirements (i.e. the size of a big number structure) measured in
 * machine _words_ (according to BYTES_PER_WORD).
 */
#define sizeofBigNumber(limbs)	((sizeof(BigNumber) + (sizeof(bn_limb) * (limbs - ANY))) / BYTES_PER_WORD)


#define BN_COPY(target, source, count)		\
{						\
  signed int i;					\
						\
  for (i = count - 1; i >= 0; i--)		\
    (target)[i] = (source)[i];			\
}


#define BN_CLEAR(target, count)			\
{						\
  signed int i;					\
						\
  for (i = count - 1; i >= 0; i--)		\
    (target)[i] = 0;				\
}


typedef mp_size bn_size;
typedef mp_limb bn_limb;

typedef mp_ptr bn_ptr;
typedef mp_srcptr bn_ref;


/*
 * BigNumber Representation:
 *
 * o size	abs(size) is the number of limbs (words) used in the 'data' field,
 * 		we use neg(size) in order to indicate a negative number.
 *
 * o data[]	is the array of limbs used to hold the actual number (ordered from
 *              least to most significant limb), using unsigned storage.
 */
typedef struct {

  bn_size size;
  bn_limb data[ANY];

} BigNumber;


enum NumericBase { BINARY = 2, OCTAL = 7, DECIMAL = 10, HEXADECIMAL = 16 };


/*
 * prototypes of public primitives.
 *
 */
void bn_clr PROTO((BigNumber *x));

void bn_set_int PROTO((BigNumber *x, integer value));
integer bn_get_int PROTO((const BigNumber *x));

void bn_set_flt PROTO((BigNumber *x, double value));
double bn_get_flt PROTO((const BigNumber *x));

void bn_set_str PROTO((BigNumber *x, const char *string, const natural base));
void bn_get_str PROTO((const BigNumber *x, char *string, const natural base));

int bn_cmp PROTO((const BigNumber *u, const BigNumber *v));
int bn_cmp_int PROTO((const BigNumber *u, const integer v));

void bn_abs PROTO((BigNumber *r, const BigNumber *x));
void bn_neg PROTO((BigNumber *r, const BigNumber *x));

void bn_add PROTO((BigNumber *r, const BigNumber *x, const BigNumber *y));
void bn_sub PROTO((BigNumber *r, const BigNumber *x, const BigNumber *y));
void bn_mul PROTO((BigNumber *r, const BigNumber *x, const BigNumber *y));
void bn_div PROTO((BigNumber *q, const BigNumber *num, const BigNumber *den));
void bn_mod PROTO((BigNumber *r, const BigNumber *num, const BigNumber *den));

void bn_pow PROTO((BigNumber *r, const BigNumber *base, const integer exp));

void bn_not PROTO((BigNumber *r, const BigNumber *u));

void bn_and PROTO((BigNumber *r, const BigNumber *u, const BigNumber *v));
void bn_ior PROTO((BigNumber *r, const BigNumber *u, const BigNumber *v));
void bn_xor PROTO((BigNumber *r, const BigNumber *u, const BigNumber *v));

integer bn_msb PROTO((const BigNumber *u));

void bn_rng PROTO((BigNumber *r, const BigNumber *u, const natural i, const natural j));

void bn_rsh PROTO((BigNumber *r, const BigNumber *u, const natural i));
void bn_lsh PROTO((BigNumber *r, const BigNumber *u, const natural i));

natural approx_string_size_number PROTO((const char *string, const natural base));
natural approx_number_size_string PROTO((const BigNumber *number, const natural base));


#endif /* BNLIB_H */
