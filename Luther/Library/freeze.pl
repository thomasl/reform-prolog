%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  freeze.pl
%
%  Johan Bevemyr.....Wed Feb 9 1994
%
%
% Description:   
%
% 
/*
:- module prolog.
*/

:- public
	freeze/2,
	frozen/2,
	call_residue/2,
	dif/2,
	mgu/2.


freeze(Var, Goal) :-
	( var(Var) ->
	    '$freeze'(Var, prolog:freeze(Var,Goal))
	; true,
	    call(Goal)
	).

%

frozen(Var, Goal) :-
	'$frozen'(Var, GoalList),
	fix_frozen_list(GoalList, Goal).


fix_frozen_list(true, true).
fix_frozen_list([G|Gs], Goal) :-
	fix_frozen_list_aux([G|Gs], Goal).

	
fix_frozen_list_aux([G|Gs], Goal) :- var(Gs), !,
	Goal = G.
fix_frozen_list_aux([G|Gs], Goal) :- 
	Goal = (G, Gn),
	fix_frozen(Gs, Gn).

%

call_residue(Goal,Varsgoals) :-	nyi('call_residue/2').

%

dif(X,Y) :- '$mgu_variables'(X, Y, Mgu), !,
	Mgu \== [],
	dif_list(Mgu, dif(X,Y)).

dif(_,_).

%

dif_list(    [],_Goal).
dif_list([V|Vs], Goal) :-
	'$frozen'(V, GoalList),
	( member_chk(Goal, GoalList) ->
	    true
	; true,
	    '$freeze'(V, Goal)
	),
	dif_list(Vs, Goal).

%

mgu(X,Y,Z) :- '$mgu_variables'(X,Y,Z).
mgu(_,_,_) :- fail.
