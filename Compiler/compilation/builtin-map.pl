%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%		     NAMES OF BUILTIN OPERATIONS
%
% builtin_map maps primitives to their names. (Note that this is somewhat
% redundant, since these names are then mapped to numbers! Anyway, it's
% easier to read and 'backwards compatible' :-)
%
% This is very much simpler than the treatment of sequential inline.pl !
%

builtin_map(compare,	3, X) :- !, X = '$compare'/3.

builtin_map(functor,	3, X) :- !, X = '$functor'/3.
builtin_map(arg,	3, X) :- !, X = '$arg'/3.
builtin_map('=..',	2, X) :- !, X = '$univ'/2.

builtin_map('=',	2, X) :- !, X = '$unify'/2.
builtin_map('$unify',	2, X) :- !, X = '$unify'/2.

builtin_map(var,	1, X) :- !, X = '$var'/1.
builtin_map(atom,	1, X) :- !, X = '$atom'/1.
builtin_map(atomic,	1, X) :- !, X = '$atomic'/1.
builtin_map(nonvar,	1, X) :- !, X = '$nonvar'/1.
builtin_map(number,	1, X) :- !, X = '$number'/1.
builtin_map(float,	1, X) :- !, X = '$float'/1.
builtin_map(integer,	1, X) :- !, X = '$integer'/1.
builtin_map(generic,	1, X) :- !, X = '$generic'/1.

builtin_map('==',	2, X) :- !, X = '$eq_univ'/2.
builtin_map('\==',	2, X) :- !, X = '$ineq_univ'/2.
builtin_map('@<',	2, X) :- !, X = '$lt_univ'/2.
builtin_map('@>',	2, X) :- !, X = '$gt_univ'/2.
builtin_map('@=<',	2, X) :- !, X = '$le_univ'/2.
builtin_map('@>=',	2, X) :- !, X = '$ge_univ'/2.

builtin_map('=:=',	2, X) :- !, X = '$eq'/2.
builtin_map('=\=',	2, X) :- !, X = '$ineq'/2.
builtin_map('<',	2, X) :- !, X = '$lt'/2.
builtin_map('>',	2, X) :- !, X = '$gt'/2.
builtin_map('=<',	2, X) :- !, X = '$le'/2.
builtin_map('>=',	2, X) :- !, X = '$ge'/2.

builtin_map('$plus',	3, X) :- !, X = '$plus'/3.
builtin_map('$plus_1',	2, X) :- !, X = '$plus_1'/2.
builtin_map('$minus',	3, X) :- !, X = '$minus'/3.
builtin_map('$minus_1',	2, X) :- !, X = '$minus_1'/2.
builtin_map('$times',	3, X) :- !, X = '$times'/3.
builtin_map('$div',	3, X) :- !, X = '$div'/3.
builtin_map('$intdiv',	3, X) :- !, X = '$intdiv'/3.
builtin_map('$mod',	3, X) :- !, X = '$mod'/3.
builtin_map('$min',	3, X) :- !, X = '$min'/3.
builtin_map('$max',	3, X) :- !, X = '$max'/3.
builtin_map('$abs',	2, X) :- !, X = '$abs'/2.
builtin_map('$neg',	2, X) :- !, X = '$neg'/2.
builtin_map('$log',	2, X) :- !, X = '$log'/2.
builtin_map('$exp',	2, X) :- !, X = '$exp'/2.
builtin_map('$pow',	3, X) :- !, X = '$pow'/3.
builtin_map('$sqrt',	2, X) :- !, X = '$sqrt'/2.
builtin_map('$cbrt',	2, X) :- !, X = '$cbrt'/2.

builtin_map('$tointeger', 2, X) :- !, X = '$tointeger'/2.
builtin_map('$tofloat',   2, X) :- !, X = '$tofloat'/2.
builtin_map('$floor',	  2, X) :- !, X = '$floor'/2.
builtin_map('$ceiling',   2, X) :- !, X = '$ceiling'/2.
builtin_map('$truncate',  2, X) :- !, X = '$truncate'/2.
builtin_map('$round',     2, X) :- !, X = '$round'/2.

builtin_map('$sin',	2, X) :- !, X = '$sin'/2.
builtin_map('$cos',	2, X) :- !, X = '$cos'/2.
builtin_map('$tan',	2, X) :- !, X = '$tan'/2.
builtin_map('$asin',	2, X) :- !, X = '$asin'/2.
builtin_map('$acos',	2, X) :- !, X = '$acos'/2.
builtin_map('$atan',	2, X) :- !, X = '$atan'/2.

builtin_map('$b_or',	3, X) :- !, X = '$b_or'/3.
builtin_map('$b_and',	3, X) :- !, X = '$b_and'/3.
builtin_map('$b_xor',	3, X) :- !, X = '$b_xor'/3.
builtin_map('$b_not',	2, X) :- !, X = '$b_not'/2.
builtin_map('$lshift',	3, X) :- !, X = '$lshift'/3.
builtin_map('$rshift',	3, X) :- !, X = '$rshift'/3.
builtin_map('$msb',	2, X) :- !, X = '$msb'/2.
builtin_map('$rng',	4, X) :- !, X = '$rng'/4.

builtin_map('$aref',	2, X) :- !, X = '$aref'/2.

builtin_map(P,N,_) :-
	error('builtin_map unrecognized primitive: ~q',[P/N]).
