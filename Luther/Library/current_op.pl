%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  current_op.pl
%
%  Patric Hedlin.....Mon Jun 27 1994
%
%
% Description:
%
%
/*
:- module prolog.
*/

:- public current_op/3, op/3.
	


current_op(A,B,C) :- '$clause'(prolog:current_op_def(A,B,C),_,0,0).


op(Precedence, OpType, Name) :-
	( integer(Precedence), 0 =< Precedence, Precedence =< 1200 ->
	    ( atom(OpType), valid_op_type(OpType) ->
		( assert_op(Name, OpType, Precedence) ->
		    true
		; true,
		    illegal_arg(3, op(Precedence,OpType,Name))
		)
	    ; true,
	        illegal_arg(2, op(Precedence,OpType,Name))
	    )
	; true,
	    illegal_arg(1, op(Precedence,OpType,Name))
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Private predicates.
%

valid_op_type( fy).
valid_op_type( fx).
valid_op_type(yfx).
valid_op_type(xfy).
valid_op_type(xfx).
valid_op_type(yf ).
valid_op_type(xf ).


assert_op(      [],_OpType,_Prec) :- !.

assert_op([Op|Ops], OpType, Prec) :- !,
	assert_op_aux(Op, OpType, Prec),
	assert_op(Ops, OpType, Prec).

assert_op(Op, OpType, Prec) :- atom(Op),
	assert_op_aux(Op, OpType, Prec).

assert_op_aux(Op, OpType, Prec) :- atom(Op), Prec > 0, !,
	( clause(prolog:current_op_def(Op, OpType, _), _) ->
	    retract(prolog:current_op_def(Op, OpType, _)) ; true
	),
	assertz(prolog:current_op_def(Op, OpType, Prec)).

assert_op_aux(Op, OpType, Prec) :- atom(Op), Prec =:= 0,
	( retract(prolog:current_op_def(Op, OpType, _)) -> true ; true ).


%%%
%%  Reader support.
%

current_infixop(Op, xfx, LoPrec, OpPrec, RoPrec) :- current_op(Op, xfx, OpPrec), !,
	LoPrec is OpPrec - 1,
	RoPrec is LoPrec.

current_infixop(Op, xfy, LoPrec, OpPrec, OpPrec) :- current_op(Op, xfy, OpPrec), !,
	LoPrec is OpPrec - 1.

current_infixop(Op, yfx, OpPrec, OpPrec, RoPrec) :- current_op(Op, yfx, OpPrec), !,
	RoPrec is OpPrec - 1.


current_prefixop(Op, fy, OpPrec, OpPrec) :- current_op(Op, fy, OpPrec), !.

current_prefixop(Op, fx, OpPrec, RoPrec) :- current_op(Op, fx, OpPrec), !,
	RoPrec is OpPrec - 1.


current_postfixop(Op, yf, OpPrec, OpPrec) :- current_op(Op, yf, OpPrec), !.

current_postfixop(Op, xf, LoPrec, OpPrec) :- current_op(Op, xf, OpPrec), !,
	LoPrec is OpPrec - 1.


%%%
%%  Initial set of operators.
%

initialize_operators :-
	op(1200, xfx, [ :-, -->]),
	op(1200,  fx, [ :-, ?-]),
	op(1150,  fx, [mode, public, dynamic, multifile, block, meta_predicate]),
	op(1100, xfy, [ ; ]),
	op(1050, xfy, [ -> ]),
	op(1000, xfy, [',']),
	op( 900,  fy, [ \+ ]),
	op( 700, xfx, [ =, =.., ==, \==, @<, @>, @=<, @>=, =:=, =\=, <, =<, >, >=, is]),
	op( 550, xfy, [ : ]),
	op( 500, yfx, [ +, -, /\, \/, # ]),
	op( 500,  fx, [ +, - ]),
	op( 400, yfx, [ *, /, //, <<, >> ]),
	op( 300, xfx, [mod]),
	op( 300,  fy, [ ~ ]),
	op( 200, xfy, [ ^ ]). 
