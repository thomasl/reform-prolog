/*
 * bignum.c
 *
 * Patric Hedlin.....Fri Jun 10 1994
 *
 *
 * Implementation:
 *
 *   The "bignum"-layer is built ontop of the "bnlib"-layer, hiding memory
 * management issues from the underlying layer. This layer also handles
 * Big Number verification, i.e. range checking and down sizing when the
 * machine representation is admissible.
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */

#include "bignum.h"


#if defined(DEBUG)
BigNumber* get_bignum(t)
    TAGGED t;
{
  assert(IsBIG(t));

  return (BigNumber*)(RemoveTag(t, BOX) + BOX_OBJ_OFFSET);
}
#endif


/*
 * Create a big number (bignum) on the heap or in the constant area.
 *
 *   [1] MakeBignumBoxMarker(sizeofBignum(Size))
 *   [+] Bignum (of variable size of one or more limbs)
 *   [n] MakeBignumBoxMarker(sizeofBignum(Size))
 *
 * Note: At the present we check heap allocation relative the "heap_end"
 *       instead of the "heap_margin". The reason for this is that we cannot
 *       perform any garbage collection in this context.
 *
 * Note: The size of a big number (i.e. its structure) is calculated in
 *       words, we thus have to adjust the size into bytes when allocating
 *       storage in the constant area (since atom_alloc() uses byte allocation).
 */
TAGGED make_bignum(w, words)
     worker *w;
     size_t words;
{
  const int size = sizeofBigNumber(words) + BOX_FRAME_SIZE;

  TAGGED *boxp = w->heap_top;
  BigNumber *bigp = (BigNumber *)(boxp + BOX_OBJ_OFFSET);

  TAGGED box_mark = MakeBignumHeapBoxMarker(size);

  boxp[0] = boxp[size - 1] = box_mark;
  
  w->heap_top += size;

  if (w->heap_top > w->heap_end)
    FatalError("Unable to allocate big number on heap");

  bigp->size = words;

  bn_clr(bigp);

  return Tagify(boxp, BOX);
}


TAGGED make_atomspace_bignum(w, words)
     worker *w;
     size_t words;
{
  const int size = sizeofBigNumber(words) + BOX_FRAME_SIZE;

  TAGGED *boxp = (TAGGED *)atom_alloc(size * BYTES_PER_WORD, w);
  BigNumber *bigp = (BigNumber *)(boxp + BOX_OBJ_OFFSET);

  TAGGED box_mark = MakeBignumAtomBoxMarker(size);

  boxp[0] = boxp[size - 1] = box_mark;
  
  bigp->size = words;

  bn_clr(bigp);

  return Tagify(boxp, BOX);
}


int make_bignum_copy(x, y)
     BigNumber *x;
     BigNumber *y;
{
  bn_ptr xp = BN_DATA(x);
  bn_ptr yp = BN_DATA(y);

  bn_size xsize = BN_SIZE(x);

  assert(BN_SIZE(x) == BN_SIZE(y));

  BN_COPY(yp, xp, xsize);

  return (y->size = x->size);
}


/*******************************************************************************
 *
 * Big Number Verification (range checking and down sizing).
 *
 */
TAGGED bignum_check(x)
     const TAGGED x;
{
  BigNumber *bn = GetBignum(x);

  if (bn_cmp_int(bn, MAX_NUM) <= 0 && bn_cmp_int(bn, MIN_NUM) >= 0)
    return Make_Integer(bn_get_int(bn));
  else
    return x;
}


/*******************************************************************************
 *
 * Big Number Conversion Functions.
 *
 *  We also include a general number conversion, producing a tagged big number
 * or a plain number (tagged small integer), as necessary.
 *
 */
TAGGED make_bignum_of_int(w, value)
    worker *w;
    integer value;
{
  TAGGED bignum = make_bignum(w, LONG_INT_WORDS);

  bn_set_int(GetBignum(bignum), value);

  return bignum;
}


TAGGED make_bignum_of_float(w, value)
    worker *w;
    double value;
{
  size_t i;
  double quote;

  /* Note: We do not handle Inf and Nan as is.
   */
  for (quote = value, i = 0; quote > 1.0; i++)
    quote = quote / (double)LIMB_MAX;

  if (i > 0)
    {
      TAGGED bignum = make_bignum(w, i + 1);

      bn_set_flt(GetBignum(bignum), value);

      return bignum;
    }
  else
    return Make_Integer(0);
}


/*
 * Convert String into Big Number.
 *
 */
TAGGED make_bignum_of_string(w, string, base)
     worker *w;
     const char *string;
     const natural base;
{
  const natural size = approx_string_size_number(string, base);

  TAGGED bignum = make_bignum(w, size);

  bn_set_str(GetBignum(bignum), string, base);

  return bignum;
}


TAGGED make_atomspace_bignum_of_string(w, string, base)
     worker *w;
     const char *string;
     const natural base;
{
  const natural size = approx_string_size_number(string, base);

  TAGGED bignum = make_atomspace_bignum(w, size);

  bn_set_str(GetBignum(bignum), string, base);

  return bignum;
}


/*
 * Convert String into a tagged number (either a small integer or a Big Number).
 *
 */
TAGGED make_number_of_string(w, string, base)
     worker *w;
     char *string;
     const natural base;
{
  register int i,j;
  register long int val, sum = 0;
  register char *strp;

  int sign;

  if (string[0] == '-')
    {
      sign = -1;
      strp = string + 1;
    }
  else
    {
      sign = 1;
      strp = string;
    }

  j = strlen(strp);

  for (i = 0; j > 0; i++, j--)
    {
      val = (int) strp[i];
      sum = sum * base + (val <= '9' ? (val - '0') : (val - 'a' + 10));

      if (sum > MAX_NUM) /* check for overflow in small NUM-ber */
	break;
    }

  if (j == 0) /* Loop terminated without overflow. */
    return Make_Integer(sign * sum);
  else
    return make_bignum_of_string(w, string, base);
}


TAGGED make_atomspace_number_of_string(w, string, base)
     worker *w;
     char *string;
     const natural base;
{
  register int i,j;
  register long int val, sum = 0;
  register char *strp;

  int sign;

  if (string[0] == '-')
    {
      sign = -1;
      strp = string + 1;
    }
  else
    {
      sign = 1;
      strp = string;
    }

  j = strlen(strp);

  for (i = 0; j > 0; i++, j--)
    {
      val = (int) strp[i];
      sum = sum * base + (val <= '9' ? (val - '0') : (val - 'a' + 10));

      if (sum > MAX_NUM) /* check for overflow in small NUM-ber */
	break;
    }

  if (j == 0) /* Loop terminated without overflow. */
    return Make_Integer(sign * sum);
  else
    return make_atomspace_bignum_of_string(w, string, base);
}


/*
 * Convert Big Number into String using a given base.
 *
 */
void make_string_of_bignum(x, string, base)
     const BigNumber *x;
     char *string;
     const natural base;
{
  assert(1 < base && base < 38);

  bn_get_str(x, string, base);
}


/*
 * Print a Big Number using a given base.
 *
 */
void print_bignum(fp, x, base)
     FILE *fp;
     const BigNumber *x;
     const natural base;
{
  assert(1 < base && base < 38);
  {
    static char bignum_string[BIG_STR_SIZE];

    int size = approx_number_size_string(x, base);

    char *buffer = size < BIG_STR_SIZE ? bignum_string : (char *)alloca(size);

    bn_get_str(x, buffer, base);

    fprintf(fp, "%s", buffer);
  }
}

/*
 * Print a Big Number using a given base.
 *
 */
void print_bignum_fd(fd, x, base)
     int fd;
     const BigNumber *x;
     const natural base;
{
  assert(1 < base && base < 38);
  {
    static char bignum_string[BIG_STR_SIZE];

    int size = approx_number_size_string(x, base);

    char *buffer = size < BIG_STR_SIZE ? bignum_string : (char *)alloca(size);

    bn_get_str(x, buffer, base);

    FD_Print2(fd, "%s", buffer);
  }
}


/*******************************************************************************
 *
 * Big Number Library wrapper functions.
 *
 */

BOOL bignum_eq(x, y)
     BigNumber *x;
     BigNumber *y;
{
  return bn_cmp(x, y) == 0 ? TRUE : FALSE;
}


BOOL bignum_ne(x, y)
     BigNumber *x;
     BigNumber *y;
{
  return bn_cmp(x, y) != 0 ? TRUE : FALSE;
}


BOOL bignum_lt(x, y)
     BigNumber *x;
     BigNumber *y;
{
  return bn_cmp(x, y) < 0 ? TRUE : FALSE;
}


BOOL bignum_gt(x, y)
     BigNumber *x;
     BigNumber *y;
{
  return bn_cmp(x, y) > 0 ? TRUE : FALSE;
}


BOOL bignum_le(x, y)
     BigNumber *x;
     BigNumber *y;
{
  return bn_cmp(x, y) <= 0 ? TRUE : FALSE;
}


BOOL bignum_ge(x, y)
     BigNumber *x;
     BigNumber *y;
{
  return bn_cmp(x, y) >= 0 ? TRUE : FALSE;
}


int bignum_cmp(x, y)
     BigNumber *x;
     BigNumber *y;
{
  return bn_cmp(x, y);
}


/*******************************************************************************
 *
 * Arithmetic Functions.
 *
 */

TAGGED bignum_abs(w, x)
     worker *w;
     BigNumber *x;
{
  const size_t res_size = BN_SIZE(x);

  TAGGED res = make_bignum(w, res_size);

  bn_abs(GetBignum(res), x);

  return res;
}


TAGGED bignum_neg(w, x)
     worker *w;
     BigNumber *x;
{
  const size_t res_size = BN_SIZE(x);

  TAGGED res = make_bignum(w, res_size);

  bn_neg(GetBignum(res), x);

  return bignum_check(res);
}


TAGGED bignum_inc(w, x)
     worker *w;
     BigNumber *x;
{
  return bignum_add(w, x, GetBignum(make_bignum_of_int(w, 1)));
}


TAGGED bignum_dec(w, x)
     worker *w;
     BigNumber *x;
{
  return bignum_sub(w, x, GetBignum(make_bignum_of_int(w, 1)));
}


TAGGED bignum_add(w, x, y)
     worker *w;
     BigNumber *x;
     BigNumber *y;
{
  const size_t res_size = max(BN_SIZE(x), BN_SIZE(y)) + 1;

  TAGGED res = make_bignum(w, res_size);

  bn_add(GetBignum(res), x, y);

  return bignum_check(res);
}


TAGGED bignum_sub(w, x, y)
     worker *w;
     BigNumber *x;
     BigNumber *y;
{
  const size_t res_size = max(BN_SIZE(x), BN_SIZE(y)) + 1;

  TAGGED res = make_bignum(w, res_size);

  bn_sub(GetBignum(res), x, y);

  return bignum_check(res);
}


TAGGED bignum_mul(w, x, y)
     worker *w;
     BigNumber *x;
     BigNumber *y;
{
  const size_t res_size = BN_SIZE(x) + BN_SIZE(y);

  TAGGED res = make_bignum(w, res_size);

  bn_mul(GetBignum(res), x, y);

  return bignum_check(res);
}


TAGGED bignum_div(w, x, y)
     worker *w;
     BigNumber *x;
     BigNumber *y;
{
  const size_t res_size = BN_SIZE(x) - BN_SIZE(y) + 1;

  if (res_size > 0)
    {
      TAGGED res = make_bignum(w, res_size);

      bn_div(GetBignum(res), x, y);

      return bignum_check(res);
    }
  else
    {
      return Make_Integer(0);
    }
}


TAGGED bignum_mod(w, x, y)
     worker *w;
     BigNumber *x;
     BigNumber *y;
{
  const size_t res_size = BN_SIZE(y) + 1;

  TAGGED res = make_bignum(w, res_size);

  bn_mod(GetBignum(res), x, y);

  return bignum_check(res);
}


TAGGED bignum_pow(w, bas, exp)
     worker *w;
     BigNumber *bas;
     integer exp;
{
  if (exp == 0)
    {
      return make_bignum_of_int(w, 1);
    }
  else if (exp < 0)
    {
      return make_bignum_of_int(w, 0);
    }
  else /* if (exp > 0) */
    {
      int bit_cnt;
      bn_ref bp = BN_DATA(bas);
      bn_size bas_size = BN_SIZE(bas);

      /*   Count the number of leading zero bits of the base's most
       * significant limb in order to calculate (we actually use an
       * over-estimate) the space requirements of the result.
       */
      count_leading_zeros(bit_cnt, bp[bas_size - 1]);
      {
	const size_t res_size = bas_size * exp - bit_cnt * exp / BITS_PER_LIMB;

	TAGGED res = make_bignum(w, res_size);

	assert(res_size > 0);

	bn_pow(GetBignum(res), bas, exp);

	return bignum_check(res);
      }
    }
}


/*******************************************************************************
 *
 * Arithmetic Logic Functions.
 *
 */

TAGGED bignum_not(w, x)
     worker *w;
     BigNumber *x;
{
  const size_t res_size = BN_SIZE(x) + 1;

  TAGGED res = make_bignum(w, res_size);

  bn_not(GetBignum(res), x);

  return bignum_check(res);
}


TAGGED bignum_and(w, x, y)
     worker *w;
     BigNumber *x;
     BigNumber *y;
{
  const size_t res_size = max(BN_SIZE(x), BN_SIZE(y)) + 1;

  TAGGED res = make_bignum(w, res_size);

  bn_and(GetBignum(res), x, y);

  return bignum_check(res);
}


TAGGED bignum_ior(w, x, y)
     worker *w;
     BigNumber *x;
     BigNumber *y;
{
  const size_t res_size = max(BN_SIZE(x), BN_SIZE(y));

  TAGGED res = make_bignum(w, res_size);

  bn_ior(GetBignum(res), x, y);

  return bignum_check(res);
}


TAGGED bignum_xor(w, x, y)
     worker *w;
     BigNumber *x;
     BigNumber *y;
{
  const size_t res_size = max(BN_SIZE(x),BN_SIZE(y));

  TAGGED res = make_bignum(w, res_size);

  bn_xor(GetBignum(res), x, y);

  return bignum_check(res);
}


TAGGED bignum_msb(w, x)
     worker *w;
     BigNumber *x;
{
  return Make_Integer(bn_msb(x));
}


TAGGED bignum_rng(w, x, i, j)
     worker *w;
     BigNumber *x;
     natural i;
     natural j;
{
  const size_t res_size = (i - j) / BITS_PER_LIMB + 1;

  TAGGED res = make_bignum(w, res_size);

  bn_rng(GetBignum(res), x, i, j);

  return bignum_check(res);
}


TAGGED bignum_rsh(w, x, i)
     worker *w;
     BigNumber *x;
     integer i;
{
  if (neg(i))
    {
      return bignum_lsh(w, x, -i);
    }
  else
    {
      const size_t res_size = BN_SIZE(x) - (i / BITS_PER_LIMB);

      TAGGED res = make_bignum(w, res_size);

      bn_rsh(GetBignum(res), x, i);

      return bignum_check(res);
    }
}


TAGGED bignum_lsh(w, x, i)
     worker *w;
     BigNumber *x;
     integer i;
{
  if (neg(i))
    {
      return bignum_rsh(w, x, -i);
    }
  else
    {
      const size_t res_size = BN_SIZE(x) + (i / BITS_PER_LIMB) + 1;

      TAGGED res = make_bignum(w, res_size);

      bn_lsh(GetBignum(res), x, i);

      return bignum_check(res);
    }
}

