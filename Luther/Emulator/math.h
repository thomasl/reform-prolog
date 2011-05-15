/*
 * math.h
 *
 * Johan Bevemyr.....Thu Feb 10 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef MATH_H
#define MATH_H


#if defined(PARALLEL)

#define MathEvalTerm(X,Done,Barf)					   \
{									   \
  if (IsSTR(X))								   \
    {									   \
      TAGGED a[2], *save;						   \
      s32 r[2];								   \
      save = w->regs;							   \
      w->regs = a;							   \
      a[1] = X; r[0] = 0; r[1] = 1;					   \
      if (luther_eval_math(w,r))					   \
	{								   \
	  X = a[0];							   \
	  w->regs = save;						   \
	  goto Done;							   \
	}								   \
      else								   \
	{								   \
	  w->regs = save;						   \
	  return FALSE;							   \
	}								   \
    }									   \
  else if (IsVar(X))							   \
    {									   \
      X = luther_await_nonvar(w,X);					   \
      /*								   \
       * At this point we should actually make sure that X is a 	   \
       * number, but we rely on the caller to handle this.		   \
       */								   \
      if (IsVar(X))							   \
	goto Barf;							   \
      else								   \
	goto Done;							   \
    }									   \
  else if (X == atom_inf)						   \
    {									   \
      X = float_inf; goto Done;						   \
    }									   \
  else if (X == atom_nan)						   \
    {									   \
      X = float_nan; goto Done;						   \
    }									   \
  else goto Barf;							   \
}

#else /* not PARALLEL */

#define MathEvalTerm(X,Done,Barf)					   \
{									   \
  if (IsSTR(X))								   \
    {									   \
      TAGGED a[2], *save;						   \
      s32 r[2];								   \
      save = w->regs;							   \
      w->regs = a;							   \
      a[1] = X; r[0] = 0; r[1] = 1;					   \
      if (luther_eval_math(w,r))					   \
	{								   \
	  X = a[0];							   \
	  w->regs = save; goto Done;					   \
	}								   \
      else								   \
	{								   \
	  w->regs = save;						   \
	  return FALSE;							   \
	}								   \
    }									   \
  else if (X == atom_inf)						   \
    {									   \
      X = float_inf; goto Done;						   \
    }									   \
  else if (X == atom_nan)						   \
    {									   \
      X = float_nan; goto Done;						   \
    }									   \
  else goto Barf;							   \
}
#endif  /* PARALLEL */


#define EvalArithmeticComp(Op,BigOp)					   \
{									   \
  TAGGED X0, X1;							   \
									   \
  DerefNLL(X0,Xw(regs[0]));	/* Expr1 */				   \
  DerefNLL(X1,Xw(regs[1]));	/* Expr2 */				   \
									   \
 start:									   \
   {									   \
     if (IsNUM(X0)) {							   \
       if (IsNUM(X1))							   \
	 return (GetNumber(X0) Op GetNumber(X1)) ? TRUE : FALSE;	   \
       if (IsBIG(X1))							   \
	 return BigOp(GetBignum(NUM2BIG(w,X0)), GetBignum(X1)) ?	   \
	   TRUE : FALSE;						   \
       if (IsFLT(X1))							   \
	 return (GetNumber(X0) Op GetFloat(X1)) ? TRUE : FALSE;		   \
									   \
       goto eval;							   \
     }									   \
     if (IsBIG(X0)) {							   \
       if (IsBIG(X1))							   \
	 return BigOp(GetBignum(X0), GetBignum(X1));			   \
       if (IsFLT(X1))							   \
	 return (bntof(GetBignum(X0)) Op GetFloat(X1)) ? TRUE : FALSE;	   \
       if (IsNUM(X1))							   \
	 return BigOp(GetBignum(X0), GetBignum(NUM2BIG(w,X1))) ?	   \
	   TRUE : FALSE;						   \
									   \
       goto eval;							   \
     }									   \
     if (IsFLT(X0)) {							   \
       if (IsFLT(X1))							   \
	 return (GetFloat(X0) Op GetFloat(X1)) ? TRUE : FALSE;		   \
       if (IsNUM(X1))							   \
	 return (GetFloat(X0) Op GetNumber(X1)) ? TRUE : FALSE;		   \
       if (IsBIG(X1))							   \
	 return (GetFloat(X0) Op bntof(GetBignum(X1))) ? TRUE : FALSE;	   \
									   \
       goto eval;							   \
     }									   \
   }									   \
  MathEvalTerm(X0,start,error1);					   \
									   \
 eval:									   \
  MathEvalTerm(X1,start,error2);					   \
									   \
 error1:								   \
  return luther_error(E_ILLEGAL_AR_EX,X0,w);				   \
									   \
 error2:								   \
  return luther_error(E_ILLEGAL_AR_EX,X1,w);				   \
}


#define EvalBinaryArithmetic(Op,BigOp)					   \
{									   \
  register TAGGED X1,X2;						   \
									   \
  DerefNLL(X1,Xw(regs[1]));	/* Expr1 */				   \
  DerefNLL(X2,Xw(regs[2]));	/* Expr2 */				   \
									   \
 start:									   \
  {									   \
    if (IsNUM(X1)) {							   \
      if (IsNUM(X2)) {							   \
	Xw(regs[0]) = MakeNumber(w, GetNumber(X1) Op GetNumber(X2));	   \
	return TRUE;							   \
      }									   \
      if (IsBIG(X2)) {							   \
	Xw(regs[0]) = BigOp(w, GetBignum(NUM2BIG(w,X1)), GetBignum(X2));   \
	return TRUE;							   \
      }									   \
      if (IsFLT(X2)) {							   \
	Xw(regs[0]) = make_float(GetNumber(X1) Op GetFloat(X2), w);	   \
	return TRUE;							   \
      }									   \
									   \
      goto eval;							   \
    }									   \
    if (IsBIG(X1)) {							   \
      if (IsBIG(X2)) {							   \
	Xw(regs[0]) = BigOp(w, GetBignum(X1), GetBignum(X2));		   \
	return TRUE;							   \
      }									   \
      if (IsFLT(X2)) {							   \
	Xw(regs[0]) = BigOp(w, GetBignum(X1), GetBignum(FLT2BIG(w,X2)));   \
	return TRUE;							   \
      }									   \
      if (IsNUM(X2)) {							   \
	Xw(regs[0]) = BigOp(w, GetBignum(X1), GetBignum(NUM2BIG(w,X2)));   \
	return TRUE;							   \
      }									   \
									   \
      goto eval;							   \
    }									   \
    if (IsFLT(X1)) {							   \
      if (IsFLT(X2)) {							   \
	Xw(regs[0]) = make_float(GetFloat(X1) Op GetFloat(X2), w);	   \
	return TRUE;							   \
      }									   \
      if (IsNUM(X2)) {							   \
	Xw(regs[0]) = make_float(GetFloat(X1) Op GetNumber(X2), w);	   \
	return TRUE;							   \
      }									   \
      if (IsBIG(X2)) {							   \
	Xw(regs[0]) = BigOp(w, GetBignum(FLT2BIG(w,X1)),GetBignum(X2));	   \
	return TRUE;							   \
      }									   \
									   \
      goto eval;							   \
    }									   \
  }									   \
  MathEvalTerm(X1,start,error1);					   \
									   \
 eval:									   \
  MathEvalTerm(X2,start,error2);					   \
									   \
 error1:								   \
  return luther_error(E_ILLEGAL_AR_EX,X1,w);				   \
									   \
 error2:								   \
  return luther_error(E_ILLEGAL_AR_EX,X2,w);				   \
}


#define EvalUnaryPostArithmetic(Op, BigOp)		\
{							\
  register TAGGED X1;					\
							\
  DerefNLL(X1,Xw(regs[1]));	/* Expr */		\
							\
 start:							\
  {							\
    if (IsNUM(X1)) {					\
      Xw(regs[0]) = MakeNumber(w, GetNumber(X1) Op);	\
      return TRUE;					\
    }							\
    if (IsBIG(X1)) {					\
      Xw(regs[0]) = BigOp(w, GetBignum(X1));		\
      return TRUE;					\
    }							\
    if (IsFLT(X1)) {					\
      Xw(regs[0]) = make_float(GetFloat(X1) Op, w);	\
      return TRUE;					\
    }							\
							\
    MathEvalTerm(X1,start,error);			\
  }							\
							\
 error:							\
  return luther_error(E_ILLEGAL_AR_EX,X1,w);		\
}


#define EvalUnaryArithmeticFloat(Op)				\
{								\
  register TAGGED X1;						\
								\
  DerefNLL(X1,Xw(regs[1]));	/* Expr */			\
								\
 start:								\
  {								\
    if (IsFLT(X1)) {						\
      Xw(regs[0]) = make_float(Op(GetFloat(X1)), w);		\
      return TRUE;						\
    }								\
    if (IsNUM(X1)) {						\
      Xw(regs[0]) = make_float(Op((double) GetNumber(X1)), w);	\
      return TRUE;						\
    }								\
    if (IsBIG(X1)) {						\
      Xw(regs[0]) = make_float(Op(bntof(GetBignum(X1))), w);    \
      return TRUE;						\
    }								\
								\
    MathEvalTerm(X1,start,error);				\
  }								\
								\
 error:								\
  return luther_error(E_ILLEGAL_AR_EX,X1,w);			\
}


#define EvalUnaryLogic(Op, BigOp)			\
{							\
  register TAGGED X1;					\
							\
  DerefNLL(X1,Xw(regs[1]));     /* Expr */		\
							\
 start:							\
  {							\
    if (IsNUM(X1)) {					\
      Xw(regs[0]) = Make_Integer(Op GetNumber(X1));	\
      return TRUE;					\
    }							\
    if (IsBIG(X1)) {					\
      Xw(regs[0]) = BigOp(w, GetBignum(X1));		\
      return TRUE;					\
    }							\
							\
    MathEvalTerm(X1,start,error);			\
  }							\
							\
 error:							\
  return luther_error(E_ILLEGAL_AR_EX,X1,w);		\
}


#define EvalBinaryLogic(Op, BigOp)					   \
{									   \
  register TAGGED X1,X2;						   \
									   \
  DerefNLL(X1,Xw(regs[1]));	/* Expr1 */				   \
  DerefNLL(X2,Xw(regs[2]));	/* Expr2 */				   \
									   \
 start:									   \
  {									   \
    if (IsNUM(X1)) {							   \
      if (IsNUM(X2)) {							   \
	Xw(regs[0]) = Make_Integer(GetNumber(X1) Op GetNumber(X2));	   \
	return TRUE;							   \
      }									   \
      if (IsBIG(X2)) {							   \
	Xw(regs[0]) = BigOp(w, GetBignum(NUM2BIG(w,X1)), GetBignum(X2));   \
	return TRUE;							   \
      }									   \
									   \
      goto eval;							   \
    }									   \
    if (IsBIG(X1)) {							   \
      if (IsBIG(X2)) {							   \
	Xw(regs[0]) = BigOp(w, GetBignum(X1), GetBignum(X2));		   \
	return TRUE;							   \
      }									   \
      if (IsNUM(X2)) {							   \
	Xw(regs[0]) = BigOp(w, GetBignum(X1), GetBignum(NUM2BIG(w,X2)));   \
	return TRUE;							   \
      }									   \
									   \
      goto eval;							   \
    }									   \
  }									   \
  MathEvalTerm(X1,start,error1);					   \
									   \
 eval:									   \
  MathEvalTerm(X2,start,error2);					   \
									   \
 error1:								   \
  return luther_error(E_ILLEGAL_AR_EX,X1,w);				   \
									   \
 error2:								   \
  return luther_error(E_ILLEGAL_AR_EX,X2,w);				   \
}


#define EvalTernary(Fun)			\
{						\
  TAGGED *save;					\
						\
  save = w->regs;				\
  w->regs = newarg;				\
						\
  newarg[1] = Ref(GetArg(X1,0));		\
  newarg[2] = Ref(GetArg(X1,1));		\
  newarg[3] = Ref(GetArg(X1,2));		\
						\
  newregs[0] = 0;				\
  newregs[1] = 1;				\
  newregs[2] = 2;				\
  newregs[3] = 3;				\
						\
  if (Fun(w,newregs))				\
    {						\
      w->regs = save;				\
      Xw(regs[0]) = newarg[0];			\
      return TRUE;				\
    }						\
  else						\
    {						\
      w->regs = save;				\
    }						\
  return FALSE;					\
}


#define EvalBinary(Fun)				\
{						\
  TAGGED *save;					\
						\
  save = w->regs;				\
  w->regs = newarg;				\
						\
  newarg[1] = Ref(GetArg(X1,0));		\
  newarg[2] = Ref(GetArg(X1,1));		\
						\
  newregs[0] = 0;				\
  newregs[1] = 1;				\
  newregs[2] = 2;				\
						\
  if (Fun(w,newregs))				\
    {						\
      w->regs = save;				\
      Xw(regs[0]) = newarg[0];			\
      return TRUE;				\
    }						\
  else						\
    {						\
      w->regs = save;				\
    }						\
  return FALSE;					\
}


#define EvalUnary(Fun)				\
{						\
  TAGGED *save;					\
						\
  save = w->regs;				\
  w->regs = newarg;				\
						\
  newarg[1] = Ref(GetArg(X1,0));		\
						\
  newregs[0] = 0;				\
  newregs[1] = 1;				\
						\
  if (Fun(w,newregs))				\
    {						\
      w->regs = save;				\
      Xw(regs[0]) = newarg[0];			\
      return TRUE;				\
    }						\
  else						\
    {						\
      w->regs = save;				\
    }						\
  return FALSE;					\
}


#define EvalUnaryArithmeticNumBigFlt(NumOp,BigOp,FltOp)			\
{									\
  register TAGGED X1;							\
									\
  DerefNLL(X1,Xw(regs[1]));	/* Expr */				\
									\
 start:									\
  {									\
    if (IsNUM(X1)) {							\
      Xw(regs[0]) = MakeNumber(w, NumOp(GetNumber(X1)));		\
      return TRUE;							\
    }									\
    if (IsBIG(X1)) {							\
      Xw(regs[0]) = BigOp(w, GetBignum(X1));				\
      return TRUE;							\
    }									\
    if (IsFLT(X1)) {							\
      Xw(regs[0]) = make_float(FltOp((double) GetFloat(X1)), w);	\
      return TRUE;							\
    }									\
									\
    MathEvalTerm(X1,start,error);					\
  }									\
									\
 error:									\
  return luther_error(E_ILLEGAL_AR_EX,X1,w);				\
}


#endif /* MATH_H */
