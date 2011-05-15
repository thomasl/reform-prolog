%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  inlineable.pl
%
%
%   Add new inlineable predicates to this list.
%
%


inlineable(true) :- !.
inlineable(fail) :- !.
inlineable(false) :- !.

inlineable('C'(_,_,_)).

inlineable(compare(_,_,_)).

inlineable(functor(_,_,_)).
inlineable(arg(_,_,_)).
inlineable((_=.._)).

inlineable((_=_)).

inlineable(var(_)).
inlineable(atom(_)).
inlineable(atomic(_)).
inlineable(nonvar(_)).
inlineable(number(_)).
inlineable(float(_)).
inlineable(integer(_)).
inlineable(generic(_)).

inlineable((_==_)).
inlineable((_\==_)).
inlineable((_@<_)).
inlineable((_@>_)).
inlineable((_@=<_)).
inlineable((_@>=_)).

inlineable((_=:=_)).
inlineable((_=\=_)).
inlineable((_<_)).
inlineable((_>_)).
inlineable((_=<_)).
inlineable((_>=_)).

inlineable('$plus'(_,_,_)).
inlineable('$plus_1'(_,_)).
inlineable('$minus'(_,_,_)).
inlineable('$minus_1'(_,_)).
inlineable('$times'(_,_,_)).
inlineable('$div'(_,_,_)).
inlineable('$intdiv'(_,_,_)).
inlineable('$mod'(_,_,_)).
inlineable('$min'(_,_,_)).
inlineable('$max'(_,_,_)).
inlineable('$abs'(_,_)).
inlineable('$neg'(_,_)).
inlineable('$log'(_,_)).
inlineable('$exp'(_,_)).
inlineable('$pow'(_,_,_)).
inlineable('$sqrt'(_,_)).
inlineable('$cbrt'(_,_)).

inlineable('$tointeger'(_,_)).
inlineable('$tofloat'(_,_)).
inlineable('$floor'(_,_)).
inlineable('$ceiling'(_,_)).
inlineable('$truncate'(_,_)).
inlineable('$round'(_,_)).

inlineable('$sin'(_,_)).
inlineable('$cos'(_,_)).
inlineable('$tan'(_,_)).
inlineable('$asin'(_,_)).
inlineable('$acos'(_,_)).
inlineable('$atan'(_,_)).

inlineable('$b_or'(_,_,_)).
inlineable('$b_and'(_,_,_)).
inlineable('$b_xor'(_,_,_)).
inlineable('$b_not'(_,_)).
inlineable('$lshift'(_,_,_)).
inlineable('$rshift'(_,_,_)).
inlineable('$msb'(_,_)).
inlineable('$rng'(_,_,_,_)).

inlineable('$aref'(_,_,_)).

/*******************************************************************************
inlineable((_ is _)).    % Currently, is/2 is defined in Prolog.
*******************************************************************************/

%%% Semi-inlineable primitives (introduced by the compiler).

inlineable('$choice'(_)).
inlineable('$cut'(_)).
inlineable('$early_cut').
inlineable('$early_cut'(_)).

inlineable('$det').
inlineable('$nondet').
inlineable('$initially_det').
inlineable('$initially_nondet').
inlineable('$get_level'(_)).
inlineable('$set_size'(_)).
