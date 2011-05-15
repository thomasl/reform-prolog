%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  inline.pl
%
%  Patric Hedlin.....Mon Jul 4 1994
%
%
% Description:
%
%    Inlineable built-in calls which are handled by the compiler.
%
/*
:- module prolog.
*/

:- public
	compare/3,
	functor/3,
	arg/3,

	'=..'/2, '='/2,

	'=='/2, '\=='/2, '@<'/2, '@>'/2, '@=<'/2, '@>='/2,

	'=:='/2, '=\='/2, '<'/2, '>'/2, '=<'/2, '>='/2,

	var/1,
	atom/1,
	atomic/1,
	nonvar/1,
	number/1,
	float/1,
	integer/1,
	generic/1,

	par_reduce_plus/2,
	par_reduce_times/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


compare(X,Y,Z) :- compare(X,Y,Z).

functor(X,Y,Z) :- functor(X,Y,Z).

arg(X,Y,Z) :- arg(X,Y,Z).


X =.. Y :- X =.. Y.

X = Y :- X = Y.


X == Y :- X == Y.

X \== Y :- X \== Y.

X @< Y :- X @< Y.

X @> Y :- X @> Y.

X @=< Y :- X @=< Y.

X @>= Y :- X @>= Y.


X =:= Y :- X =:= Y.

X =\= Y :- X =\= Y.

X < Y :- X < Y.

X > Y :- X > Y.

X =< Y :- X =< Y.

X >= Y :- X >= Y.


var(X) :- var(X).

atom(X) :- atom(X).

atomic(X) :- atomic(X).

nonvar(X) :- nonvar(X).

number(X) :- number(X).

float(X) :- float(X).

integer(X) :- integer(X).

generic(X) :- generic(X).


par_reduce_plus(X,Y) :- par_reduce_plus(X,Y).

par_reduce_times(X,Y) :- par_reduce_times(X,Y).
