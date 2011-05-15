/*
 * bnlib.c - Big Number Library.
 *
 * Patric Hedlin.....Fri Jun 10 1994
 *
 *
 * Implementation:
 *
 *   The "bnlib"-layer is built ontop of the low-level routines found
 * in the GMP library, hiding differences in the representation.
 *
 * 
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */

#include <math.h>

#include "bnlib.h"


/*
 * Prototypes of private (static) functions.
 */
static integer canonize PROTO((BigNumber *, bn_size));


const int char_per_word[] = {
  0,  0, 32, 20, 16, 13, 12, 11, 10, 10,  9,  9,  8,
      8,  8,  8,  8,  7,  7,  7,  7,  7,  7,  7,  6,
      6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6
};


void bn_clr(x)
     BigNumber *x;
{
  BN_CLEAR(BN_DATA(x), BN_SIZE(x));
}


void bn_set_int(x, value)
     BigNumber *x;
     integer value;
{
  assert(BN_SIZE(x) == 1);

  if (value >= 0)
    {
      x->data[0] = value;
      x->size = (value == 0 ? 0 : 1);
    }
  else
    {
      x->data[0] = -value;
      x->size = -1;
    }
}


integer bn_get_int(x)
     const BigNumber *x;
{
  assert(BN_SIZE(x) <= 1);

  return x->size == 0 ? 0 : (x->size > 0 ? x->data[0] : -x->data[0]);
}


/* Temporary kludge.
*/
void bn_set_flt(x, value)
     BigNumber *x;
     double value;
{
  static char buffer[1024];

  sprintf(buffer, "%0.0f", value);

  bn_set_str(x, buffer, DECIMAL);
}


double bn_get_flt(x)
     const BigNumber *x;
{
  const double limb_base = pow(2, BITS_PER_LIMB);

  register bn_ref xp = BN_DATA(x);
  register bn_size i;
  register double fval = 0.0;

  for (i = BN_SIZE(x) - 1; i >= 0; i--)
    fval = fval * limb_base + (double)xp[i];

  return fval;
}


void bn_set_str(x, string, base)
     BigNumber *x;
     const char *string;
     const natural base;
{
  MP_INT mp_int;

  mp_int.alloc = BN_SIZE(x);
  mp_int.size  = BN_SIZE(x);
  mp_int.d     = BN_DATA(x);

  _mpz_set_str(&mp_int, string, base);

  x->size = sgn(x->size) * canonize(x, BN_SIZE(x));
}


void bn_get_str(x, string, base)
     const BigNumber *x;
     char *string;
     const natural base;
{
  MP_INT mp_int;

  mp_int.alloc = BN_SIZE(x);
  mp_int.size  = x->size;
  mp_int.d     = BN_DATA(x);

  _mpz_get_str(string, base, &mp_int);
}



int bn_cmp(u, v)
     const BigNumber *u;
     const BigNumber *v;
{
  bn_size usize = u->size;
  bn_size vsize = v->size;
  bn_size i = abs(usize);
  bn_limb ui; 
  bn_limb vi; 

  if (usize != vsize)
    return usize - vsize;

  if (usize == 0)
    return 0;

  do {
    dec(i);
    ui = BN_DATA(u)[i];
    vi = BN_DATA(v)[i];
  } while (ui == vi && i > 0);

  return (ui == vi) ? 0 : ((ui < vi) || (usize < 0) ? -(i + 1) : (i + 1));
}


int bn_cmp_int(u, v)
     const BigNumber *u;
     const integer v;
{
  bn_size usize = BN_SIZE(u);

  if (usize == 0)
    return v < 0 ? 1 : -1;

  if (usize != 1)
    return sgn(u->size);
  else {
    bn_limb uval = BN_DATA(u)[0];

    if (v < 0) {
      bn_limb ival = -v;

      return neg(u->size) ? (uval == ival ? 0 : ((uval < ival) ? 1 : -1)) :  1;
    }
    else {
      bn_limb ival =  v;

      return pos(u->size) ? (uval == ival ? 0 : ((uval < ival) ? -1 : 1)) : -1;
    }
  }
}


/*
 * absolution and negation.
 *
 */
void bn_abs(r, x)
     BigNumber *r;
     const BigNumber *x;
{
  assert(BN_SIZE(r) == BN_SIZE(x));

  BN_COPY(BN_DATA(r), BN_DATA(x), BN_SIZE(x));
}


void bn_neg(r, x)
     BigNumber *r;
     const BigNumber *x;
{
  assert(BN_SIZE(r) == BN_SIZE(x));

  r->size = -x->size;

  BN_COPY(BN_DATA(r), BN_DATA(x), BN_SIZE(x));
}


/*
 * addition
 *
 */
void bn_add(r, x, y)
     BigNumber *r;
     const BigNumber *x;
     const BigNumber *y;
{
  bn_size xsize = BN_SIZE(x);
  bn_size ysize = BN_SIZE(y);

  assert(BN_SIZE(r) == max(BN_SIZE(x), BN_SIZE(y)) + 1);

  if (xsize < ysize)
    bn_add(r, y, x);
  else
    {
      bn_ptr rp = BN_DATA(r);
      bn_ref xp = BN_DATA(x);
      bn_ref yp = BN_DATA(y);

      bn_size rsize = xsize + 1;

      bn_size xflag = x->size;
      bn_size yflag = y->size;

      if (xflag >= 0)
	{
	  if (yflag >= 0)
	    {
	      rsize = mpn_add(rp, xp, xsize, yp, ysize);
	      if (rsize != 0)
		rp[xsize] = 1;
	      rsize = rsize + xsize;
	    }
	  else
	    {
	      /*   The signs are different. Determine which operand to
	       * subtract from which.
	       */
	      if (xsize == ysize && mpn_cmp(xp, yp, xsize) < 0)
		rsize = -(xsize + mpn_sub(rp, yp, xsize, xp, xsize));
	      else
		rsize = (xsize + mpn_sub(rp, xp, xsize, yp, ysize));
	    }
	}
      else
	{
	  if (yflag >= 0)
	    {
	      /*   The signs are different. Determine which operand to
	       * subtract from which.
	       */
	      if (xsize == ysize && mpn_cmp(xp, yp, xsize) < 0)
		rsize = (xsize + mpn_sub(rp, yp, xsize, xp, xsize));
	      else
		rsize = -(xsize + mpn_sub(rp, xp, xsize, yp, ysize));
	    }
	  else
	    {
	      rsize = mpn_add(rp, xp, xsize, yp, ysize);
	      if (rsize != 0)
		rp[xsize] = 1;
	      rsize = -(rsize + xsize);
	    }
	}

      r->size = rsize;
    }
}


/*
 * subtraction
 *
 */
void bn_sub(r, x, y)
     BigNumber *r;
     const BigNumber *x;
     const BigNumber *y;
{
  bn_size xsize = BN_SIZE(x);
  bn_size ysize = BN_SIZE(y);

  assert(BN_SIZE(r) == max(BN_SIZE(x), BN_SIZE(y)) + 1);

  if (xsize < ysize)
    bn_sub(r, y, x);
  else
    {
      bn_ptr rp = BN_DATA(r);
      bn_ref xp = BN_DATA(x);
      bn_ref yp = BN_DATA(y);

      bn_size rsize = xsize + 1;

      bn_size xflag =  x->size;
      bn_size yflag = -y->size;   /* The "-" makes the difference from bn_add */

      if (xflag >= 0)
	{
	  if (yflag >= 0)
	    {
	      rsize = mpn_add(rp, xp, xsize, yp, ysize);
	      if (rsize != 0)
		rp[xsize] = 1;
	      rsize = rsize + xsize;
	    }
	  else
	    {
	      /*   The signs are different. Determine which operand to
	       * subtract from which.
	       */
	      if (xsize == ysize && mpn_cmp(xp, yp, xsize) < 0)
		rsize = -(xsize + mpn_sub(rp, yp, xsize, xp, xsize));
	      else
		rsize = xsize + mpn_sub(rp, xp, xsize, yp, ysize);
	    }
	}
      else
	{
	  if (yflag >= 0)
	    {
	      /*   The signs are different. Determine which operand to
	       * subtract from which.
	       */
	      if (xsize == ysize && mpn_cmp(xp, yp, xsize) < 0)
		rsize = xsize + mpn_sub(rp, yp, xsize, xp, xsize);
	      else
		rsize = -(xsize + mpn_sub(rp, xp, xsize, yp, ysize));
	    }
	  else
	    {
	      rsize = mpn_add(rp, xp, xsize, yp, ysize);
	      if (rsize != 0)
		rp[xsize] = 1;
	      rsize = -(rsize + xsize);
	    }
	}

      r->size = rsize;
    }
}


/*
 * multiplication
 *
 */
void bn_mul(r, x, y)
     BigNumber *r;
     const BigNumber *x;
     const BigNumber *y;
{
  bn_size xsize = BN_SIZE(x);
  bn_size ysize = BN_SIZE(y);

  bn_size sign_product = x->size ^ y->size;

  assert(BN_SIZE(r) == BN_SIZE(x) + BN_SIZE(y));

  if (xsize < ysize)
    bn_mul(r, y, x);
  else
    {
      bn_ptr rp = BN_DATA(r);
      bn_ref xp = BN_DATA(x);
      bn_ref yp = BN_DATA(y);

      bn_size rsize;

      rsize = mpn_mul(rp, xp, xsize, yp, ysize);
      r->size = sign_product < 0 ? -rsize : rsize;
    }
}


/*
 * division
 *
 * Quotient = Numerator / Denominator
 *
 */
void bn_div(quot, num, den)
     BigNumber *quot;
     const BigNumber *num;
     const BigNumber *den;
{
  bn_ref np = BN_DATA(num);
  bn_ref dp = BN_DATA(den);
  bn_ptr qp = BN_DATA(quot);

  bn_size nsize = BN_SIZE(num);
  bn_size dsize = BN_SIZE(den);
  bn_size qsize = BN_SIZE(quot);
  bn_size rsize = nsize + 1;

  bn_size sign_quotient = num->size ^ den->size;
  unsigned normalization_steps;

  bn_ptr rp = (bn_ptr) alloca(rsize * BYTES_PER_LIMB);

  assert(qsize > 0);
  assert(qsize == nsize - dsize + 1);

  count_leading_zeros(normalization_steps, dp[dsize - 1]);

  /* Normalize the denominator and the numerator.
   */
  if (normalization_steps != 0)
    {
      bn_limb ndigit;
      bn_ptr tp = (bn_ptr) alloca(dsize * BYTES_PER_LIMB);

      /*   Shift up the denominator setting the most significant bit of
       * the most significant word. Use temporary storage not to clobber
       * the original contents of the denominator.
       */
      (void) mpn_lshift(tp, dp, dsize, normalization_steps);

      /*   Shift up the numerator, possibly introducing a new most
       * significant word. Move the shifted numerator in the remainder
       * meanwhile.
       */
      ndigit = mpn_lshift(rp, np, nsize, normalization_steps);

      if (ndigit != 0)
	rp[nsize] = ndigit;
      else
	rsize = nsize;

      qsize = rsize - dsize + mpn_div(qp, rp, rsize, tp, dsize);
    }
  else
    {
      /* The denominator is already normalized, as required.
       */
      BN_COPY(rp, np, nsize);	/* Move the numerator to the remainder. */

      rsize = nsize;
      qsize = rsize - dsize + mpn_div(qp, rp, rsize, dp, dsize);
    }

  /*   Normalize the quotient. We may have at most one leading
   * zero-word, so no loop is needed.
   */
  if (qsize > 0)
    if (qp[qsize - 1] == 0)
      dec(qsize);
  
  quot->size = (sign_quotient < 0) ? -qsize : qsize;
}


/*
 * modulation
 *
 * Remainder = Numerator mod Denominator
 *
 */
void bn_mod(rem, num, den)
     BigNumber *rem;
     const BigNumber *num;
     const BigNumber *den;
{
  bn_ptr rp = BN_DATA(rem);
  bn_ref np = BN_DATA(num);
  bn_ref dp = BN_DATA(den);

  bn_size rsize = BN_SIZE(rem);
  bn_size nsize = BN_SIZE(num);
  bn_size dsize = BN_SIZE(den);
  bn_size qsize = nsize - dsize + 1;

  bn_size sign_remainder = num->size;
  unsigned normalization_steps;

  assert(rsize == dsize + 1);

  if (qsize <= 0)
    {
      rem->size = num->size;

      BN_COPY(rp, np, nsize);
    }
  else
    {
      bn_ptr qp = (bn_ptr) alloca(qsize * BYTES_PER_LIMB);

      count_leading_zeros(normalization_steps, dp[dsize - 1]);
      
      /*   Normalize the denominator, i.e. make its most significant bit set
       * by shifting it NORMALIZATION_STEPS bits to the left. Also shift the
       * numerator the same number of steps (to keep the quotient the same).
       */
      if (normalization_steps != 0)
	{
	  bn_limb ndigit;
	  bn_ptr tp = (bn_ptr) alloca(dsize * BYTES_PER_LIMB);

	  /*   Shift up the denominator setting the most significant bit of
	   * the most significant word. Use temporary storage not to clobber
	   * the original contents of the denominator.
	   */
	  (void) mpn_lshift(tp, dp, dsize, normalization_steps);

	  /*   Shift up the numerator, possibly introducing a new most
	   * significant word. Move the shifted numerator in the remainder
	   * meanwhile.
	   */
	  ndigit = mpn_lshift(rp, np, nsize, normalization_steps);

	  if (ndigit != 0)
	    rp[nsize] = ndigit;
	  else
	    rsize = nsize;

	  (void) mpn_div(qp, rp, rsize, tp, dsize);
	}
      else
	{
	  BN_COPY(rp, np, nsize);

	  rsize = nsize;

	  (void) mpn_div(qp, rp, rsize, dp, dsize);
	}

      /* We no longer need that extra limb (which contains rubbish after the division).
       */
      rsize = dsize;

      /*   Normalize the remainder. We may have at most one leading
       * zero-word, so no loop is needed.
       */
      if (rsize > 0)
	if (rp[rsize - 1] == 0)
	  dec(rsize);

      if (normalization_steps > 0)
	rsize = mpn_rshift(rp, rp, rsize, normalization_steps);

      rem->size = (sign_remainder < 0) ? -rsize : rsize;
    }
}


/*
 * power : Result = Base ^ Exponent  ( exp(Base,Exponent) )
 *
 */
void bn_pow(r, b, exp)
     BigNumber *r;
     const BigNumber *b;
     const integer exp;
{
  int i,bit_cnt;

  bn_size rsize;
  bn_size bsize = BN_SIZE(b);

  bn_ptr rp = BN_DATA(r);
  bn_ref bp = BN_DATA(b);

  bn_ptr tp, xp;

  assert(exp > 0);
  assert(bsize > 0);

# ifndef NDEBUG
  count_leading_zeros(bit_cnt, bp[bsize - 1]);

  assert(BN_SIZE(r) == bsize * exp - bit_cnt * exp / BITS_PER_LIMB);
# endif

  /*   We use two areas alternately to hold the input and recieve the product
   * of the multiplication (using mpn_mul). We thus have to allocate an extra
   * buffer used as temporary storage.
   *   This scheme is needed in order to meet the requirements of mpn_mul
   * (the product space may not be the same as any of the operands).
   */
  tp = (bn_ptr) alloca(BN_SIZE(r) * BYTES_PER_LIMB);

  BN_COPY(rp, bp, bsize);

  rsize = bsize;

  count_leading_zeros(bit_cnt, exp);

  for (i = BITS_PER_LIMB - bit_cnt - 2; i >= 0; i--)
    {
      rsize = mpn_mul(tp, rp, rsize, rp, rsize);
      xp = tp; tp = rp; rp = xp;

      if ((exp & ((bn_limb) 1 << i)) != 0)
	{
	  rsize = mpn_mul(tp, rp, rsize, bp, bsize);
	  xp = tp; tp = rp; rp = xp;
	}
    }
  BN_COPY(BN_DATA(r), rp, rsize);
  r->size = even(exp) || b->size >= 0 ? rsize : -rsize;
}


/*******************************************************************************
 *
 * Logical (bitwise) Functions.
 *
 * NOTE: We use negative numbers to represent infinite precision.
 *
 */

/*
 * logical not : Result is \(Operand')
 *
 */
void bn_not(r, u)
     BigNumber *r;
     const BigNumber *u;
{
  bn_ptr rp = BN_DATA(r);
  bn_ref up = BN_DATA(u);

  bn_size usize = u->size;
  bn_limb one = 1;

  assert(BN_SIZE(r) == BN_SIZE(u) + 1);

  if (usize >= 0)
    {
      bn_limb carry;

      /* using -x = ~x + 1  ->  ~x = -(x + 1).
       */
      if ((carry = mpn_add(rp, up, usize, &one, 1)) != 0)
	{
	  rp[usize++] = carry;
	}
      r->size = -usize;
    }
  else /* usize < 0 */
    {
      usize = -usize;

      /* using -x = ~(x - 1)  ->  ~(-x) = ~~(x - 1) = x - 1.
       */
      usize = usize + mpn_sub(rp, up, usize, &one, 1);

      r->size = usize;
    }
}


/*
 * logical and : Result is Operand' /\ Operand''
 *
 */
void bn_and(r, u, v)
     BigNumber *r;
     const BigNumber *u;
     const BigNumber *v;
{
  bn_size i;

  bn_size rsize;
  bn_size usize = u->size;
  bn_size vsize = v->size;

  assert(BN_SIZE(r) == max(BN_SIZE(u), BN_SIZE(v)) + 1);

  if (usize >= 0)
    {
      if (vsize >= 0)
	{
	  bn_ptr rp = BN_DATA(r);
	  bn_ref up = BN_DATA(u);
	  bn_ref vp = BN_DATA(v);

	  rsize = min(abs(usize), abs(vsize));

	  for (i = rsize - 1; i >= 0; i--)
	    rp[i] = up[i] & vp[i];

	  r->size = canonize(r, rsize);
	}
      else /* vsize < 0 */
	{
	  bn_and(r, v, u);
	}
    }
  else /* usize < 0 */
    {
      if (vsize < 0)
	{
	  bn_ptr rp = BN_DATA(r);
	  bn_ref up = BN_DATA(u);
	  bn_ref vp = BN_DATA(v);

	  /* Both operands are negative (so is the result):
	   *
	   *   -((-u) & (-v)) = -(~(u - 1) & ~(v - 1))
	   *                  = ~(~(u - 1) & ~(v - 1)) + 1
	   *                  = ((u - 1) | (v - 1)) + 1
	   */
	  usize = -usize;
	  vsize = -vsize;
	  {
	    bn_limb one = 1;
	    bn_limb carry;

	    bn_ptr tup = (bn_ptr) alloca(usize * BYTES_PER_LIMB);
	    bn_ptr tvp = (bn_ptr) alloca(vsize * BYTES_PER_LIMB);

	    usize = usize + mpn_sub(tup, up, usize, &one, 1);
	    vsize = vsize + mpn_sub(tvp, vp, vsize, &one, 1);

	    if (vsize < usize)
	      {
		BN_COPY(rp + vsize, tup + vsize, usize - vsize);

		for (i = vsize - 1; i >= 0; i--)
		  rp[i] = tup[i] | tvp[i];

		rsize = usize;
	      }
	    else
	      {
		BN_COPY(rp + usize, tvp + usize, vsize - usize);

		for (i = usize - 1; i >= 0; i--)
		  rp[i] = tup[i] | tvp[i];

		rsize = vsize;
	      }

	    if (rsize > 0)
	      {
		if((carry = mpn_add(rp, rp, rsize, &one, 1)) != 0)
		  {
		    rp[rsize++] = carry;
		  }
	      }
	    else
	      {
		rp[0] = 1;
		rsize = 1;
	      }
	  }
	  r->size = -rsize;
	}
      else /* 0 >= vsize */
	{
	  bn_ptr rp = BN_DATA(r);
	  bn_ref up = BN_DATA(u);
	  bn_ref vp = BN_DATA(v);

	  /* First operand is negative, second is positive (so is the result):
	   *
	   *   -u & v = ~(u - 1) & v
	   */
	  usize = -usize;
	  {
	    bn_limb one = 1;

	    bn_ptr tup = (bn_ptr) alloca(usize * BYTES_PER_LIMB);

	    usize = usize + mpn_sub(tup, up, usize, &one, 1);

	    if (usize < vsize)
	      {
		/*   The result has the same size as v, since v is normalized
		 * and longer than the ones-extended u.
		 */
		BN_COPY(rp + usize, vp + usize, vsize - usize);

		for (i = usize - 1; i >= 0; i--)
		  rp[i] = ~tup[i] & vp[i];

		rsize = vsize;
	      }
	    else
	      {
		for (i = vsize - 1; i >= 0; i--)
		  rp[i] = ~tup[i] & vp[i];

		rsize = vsize;
	      }
	  }
	  r->size = canonize(r, rsize);
	}
    }
}


/*
 * logical inclusive or : Result is Operand' \/ Operand''
 *
 */
void bn_ior(r, u, v)
     BigNumber *r;
     const BigNumber *u;
     const BigNumber *v;
{
  bn_size i;

  bn_size rsize;
  bn_size usize = u->size;
  bn_size vsize = v->size;

  assert(BN_SIZE(r) == max(BN_SIZE(u), BN_SIZE(v)));

  if (usize >= 0)
    {
      if (vsize >= 0)
	{
	  bn_ptr rp = BN_DATA(r);
	  bn_ref up = BN_DATA(u);
	  bn_ref vp = BN_DATA(v);

	  if (vsize < usize)
	    {
	      BN_COPY(rp + vsize, up + vsize, usize - vsize);

	      for (i = vsize - 1; i >= 0; i--)
		rp[i] = up[i] | vp[i];

	      rsize = usize;
	    }
	  else
	    {
	      BN_COPY(rp + usize, vp + usize, vsize - usize);

	      for (i = usize - 1; i >= 0; i--)
		rp[i] = up[i] | vp[i];

	      rsize = vsize;
	    }
	  r->size = rsize;
	}
      else /* vsize < 0 */
	{
	  bn_ior(r, v, u);
	}
    }
  else /* usize < 0 */
    {
      if (vsize < 0)
	{
	  bn_ptr rp = BN_DATA(r);
	  bn_ref up = BN_DATA(u);
	  bn_ref vp = BN_DATA(v);

	  /* Both operands are negative (so is the result):
	   *
	   *   -((-u) | (-v)) = -(~(u - 1) | ~(v - 1))
	   *                  = ~(~(u - 1) | ~(v - 1)) + 1
	   *                  = ((u - 1) & (v - 1)) + 1
	   */
	  usize = -usize;
	  vsize = -vsize;

	  rsize = min(usize, vsize);
	  {
	    bn_limb one = 1;
	    bn_limb carry;

	    bn_ptr tup = (bn_ptr) alloca(usize * BYTES_PER_LIMB);
	    bn_ptr tvp = (bn_ptr) alloca(vsize * BYTES_PER_LIMB);

	    usize = usize + mpn_sub(tup, up, usize, &one, 1);
	    vsize = vsize + mpn_sub(tvp, vp, vsize, &one, 1);

	    for (i = rsize - 1; i >= 0; i--)
	      rp[i] = tup[i] & tvp[i];

	    rsize = canonize(r, rsize);

	    if (rsize > 0)
	      {
		if((carry = mpn_add(rp, rp, rsize, &one, 1)) != 0)
		  {
		    rp[rsize++] = carry;
		  }
	      }
	    else
	      {
		rp[0] = 1;
		rsize = 1;
	      }
	  }
	  r->size = -rsize;
	}
      else /* vsize > 0 */
	{
	  bn_ptr rp = BN_DATA(r);
	  bn_ref up = BN_DATA(u);
	  bn_ref vp = BN_DATA(v);

	  /* Second operand is positive, first is negative (so is the result):
	   *
	   *   -((-u) | v) = -(~(u - 1) | v)
	   *               = ~(~(u - 1) | v) + 1
	   *               = ((u - 1) & ~v) + 1
	   */
	  usize = -usize;
	  {
	    bn_limb one = 1;
	    bn_limb carry;

	    bn_ptr tup = (bn_ptr) alloca(usize * BYTES_PER_LIMB);

	    usize = usize + mpn_sub(tup, up, usize, &one, 1);

	    if (usize <= vsize)
	      {
		rsize = vsize;
	      }
	    else
	      {
		rsize = usize;

		/* Copy the part of u that stretches above v, to r.
		 */
		BN_COPY(rp + vsize, up + vsize, usize - vsize);
	      }

	    for (i = rsize - 1; i >= 0; i--)
	      rp[i] = tup[i] & ~vp[i];

	    if (rsize > 0)
	      {
		if ((carry = mpn_add(rp, rp, rsize, &one, 1)) != 0)
		  {
		    rp[rsize++] = carry;
		  }
	      }
	    else
	      {
		rp[0] = 1;
		rsize = 1;
	      }
	  }
	  r->size = -rsize;
	}
    }
}


/*
 * logical exclusive or : Result is Operand' # Operand''
 *
 */
void bn_xor(r, u, v)
     BigNumber *r;
     const BigNumber *u;
     const BigNumber *v;
{
  bn_size i;

  bn_size rsize;
  bn_size usize = u->size;
  bn_size vsize = v->size;

  assert(BN_SIZE(r) == max(BN_SIZE(u), BN_SIZE(v)));

  if (usize >= 0)
    {
      if (vsize >= 0)
	{
	  bn_ptr rp = BN_DATA(r);
	  bn_ref up = BN_DATA(u);
	  bn_ref vp = BN_DATA(v);

	  if (vsize < usize)
	    {
	      for (i = usize - 1; i >= vsize; i--)
		rp[i] = up[i] ^ 0;

	      for (i = vsize - 1; i >= 0; i--)
		rp[i] = up[i] ^ vp[i];

	      rsize = usize;
	    }
	  else
	    {
	      for (i = vsize - 1; i >= usize; i--)
		rp[i] = 0 ^ vp[i];

	      for (i = usize - 1; i >= 0; i--)
		rp[i] = up[i] ^ vp[i];

	      rsize = vsize;
	    }
	  r->size = rsize;
	}
      else /* vsize < 0 */
	{
	  bn_xor(r, v, u);
	}
    }
  else /* usize < 0 */
    {
      if (vsize < 0)
	{
	  bn_ptr rp = BN_DATA(r);
	  bn_ref up = BN_DATA(u);
	  bn_ref vp = BN_DATA(v);

	  /* Both operands are negative (the result is positive):
	   *
	   *   (-u) ^ (-v) = ~(u - 1) ^ ~(v - 1)
	   */
	  usize = -usize;
	  vsize = -vsize;

	  rsize = min(usize, vsize);
	  {
	    bn_limb one = 1;

	    bn_ptr tup = (bn_ptr) alloca(usize * BYTES_PER_LIMB);
	    bn_ptr tvp = (bn_ptr) alloca(vsize * BYTES_PER_LIMB);

	    usize = usize + mpn_sub(tup, up, usize, &one, 1);
	    vsize = vsize + mpn_sub(tvp, vp, vsize, &one, 1);

	    for (i = rsize - 1; i >= 0; i--)
	      rp[i] = ~up[i] ^ ~vp[i];

	    rsize = canonize(r, rsize);
	  }
	  r->size = rsize;
	}
      else /* vsize > 0 */
	{
	  bn_ptr rp = BN_DATA(r);
	  bn_ref up = BN_DATA(u);
	  bn_ref vp = BN_DATA(v);

	  /* Second operand is positive, first is negative (so is the result):
	   *
	   *   -((-u) ^ v) = -(~(u - 1) ^ v)
	   *               = ~(~(u - 1) ^ v) + 1
	   */
	  usize = -usize;
	  {
	    bn_limb one = 1;
	    bn_limb carry;

	    bn_ptr tup = (bn_ptr) alloca(usize * BYTES_PER_LIMB);

	    usize = usize + mpn_sub(tup, up, usize, &one, 1);

	    if (vsize < usize)
	      {
		for (i = usize - 1; i >= vsize; i--)
		  rp[i] = ~(~tup[i] ^ 0);
		
		for (i = vsize - 1; i >= 0; i--)
		  rp[i] = ~(~tup[i] ^ vp[i]);

		rsize = canonize(r, usize);
	      }
	    else
	      {
		for (i = vsize - 1; i >= usize; i--)
		  rp[i] = ~(LIMB_MASK ^ vp[i]);

		for (i = usize - 1; i >= 0; i--)
		  rp[i] = ~(~tup[i] ^ vp[i]);
		
		rsize = canonize(r, vsize);
	      }

	    if (rsize > 0)
	      {
		if ((carry = mpn_add(rp, rp, rsize, &one, 1)) != 0)
		  {
		    rp[rsize++] = carry;
		  }
	      }
	    else
	      {
		rp[0] = 1;
		r->size = 1;
	      }
	  }
	  r->size = -rsize;
	}
    }
}


/*
 * Calculate most significant bit (msb).
 *
 * NOTE: bits are counted starting with bit zero. Negative numbers represent
 *       infinite precision (approximated with a negative (-1) msb).
 */
integer bn_msb(u)
     const BigNumber *u;
{
  if (u->size < 0)
    {
      return -1;
    }
  else
    {
      bn_size count;
      bn_size usize = BN_SIZE(u);

      count_leading_zeros(count, BN_DATA(u)[usize - 1]);

      assert(count < WORDSIZE);

      return (BITS_PER_LIMB - (count + 1)) + (usize - 1) * BITS_PER_LIMB;
    }
}


/*
 * Calculate the bit range [i,j] as a down shifted number.
 *
 * NOTE: bits are counted starting with bit zero. Negative numbers representing
 *       infinite precision is not handled as yet.
 */
void bn_rng(r, u, i, j)
     BigNumber *r;
     const BigNumber *u;
     const natural i;
     const natural j; /* superflous */
{
  if (pos(u->size))
    {
      bn_ptr rp = BN_DATA(r);
      bn_ptr up = BN_DATA(u);

      assert(r->size == (i - j) / BITS_PER_LIMB + 1);

      r->size = mpn_rshift(rp, up + (j / BITS_PER_LIMB), r->size, j % BITS_PER_LIMB);
    }
  else
    {
      r->size = 0;
    }
}


/*
 * Arithmetic right shift : Result is Operand' >> Operand''
 *
 */
void bn_rsh(r, u, i)
     BigNumber *r;
     const BigNumber *u;
     const natural i;
{
  bn_size usize = BN_SIZE(u);

  if (usize == 0)
    {
      r->size = 0;
    }
  else
    {
      bn_ptr rp = BN_DATA(r);
      bn_ptr up = BN_DATA(u);

      int sign = sgn(u->size);

      bn_size limb_cnt = i / BITS_PER_LIMB;

      assert(BN_SIZE(r) == usize - limb_cnt);

      r->size = sign * mpn_rshift(rp, up + limb_cnt, usize - limb_cnt, i % BITS_PER_LIMB);
    }
}


/*
 * Arithmetic left shift : Result is Operand' << Operand''
 *
 */
void bn_lsh(r, u, i)
     BigNumber *r;
     const BigNumber *u;
     const natural i;
{
  bn_size rsize = BN_SIZE(r);
  bn_size usize = BN_SIZE(u);

  if (usize == 0)
    {
      r->size = 0;
    }
  else
    {
      bn_ptr rp = BN_DATA(r);
      bn_ptr up = BN_DATA(u);

      int sign = sgn(u->size);

      bn_size limb_cnt = i / BITS_PER_LIMB;
      bn_limb msl;

      assert(rsize == usize + limb_cnt + 1);

      msl = mpn_lshift(rp + limb_cnt, up, usize, i % BITS_PER_LIMB);

      if (msl != 0)
	rp[rsize - 1] = msl;
      else
	dec(rsize);

      /* Clear lower end of Big Number.
       */
      BN_CLEAR(rp, limb_cnt);

      r->size = sign * rsize;
    }
}



/*******************************************************************************
 *
 * Support Functions.
 *
 * o approx_string_size_number(string, base)
 *
 *     Approximate the size of a big number (measured in limbs) represented as
 *   a string, using the actual base.
 *
 *
 * o approx_number_size_string(number, base)
 *
 *     Approximate the size of a string (measured in bytes) needed to represent
 *  a big number, using the actual base.
 *
 */
natural approx_string_size_number(string, base)
     const char *string;
     const natural base;
{
  return strlen(string) / char_per_word[base] + 1;
}

natural approx_number_size_string(number, base)
     const BigNumber *number;
     const natural base;
{
  return BYTES_PER_LIMB * BN_SIZE(number) * char_per_word[base] + 1;
}


/*******************************************************************************
 *
 * Private Functions.
 *
 */
static integer canonize(x, i)
     BigNumber *x;
     bn_size i;
{
  bn_ref xp = BN_DATA(x);

  while(i > 0 && xp[i - 1] == 0)
    dec(i);

  return i;
}
