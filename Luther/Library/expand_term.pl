%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  expand_term.pl
%
%  Patric Hedlin.....Mon Jan 2 1995
%
%
% Description:
%
%
/*
:- module prolog.
*/

:- public
	phrase/2,
	phrase/3,
	expand_term/2.


phrase(DCG, List):-
	phrase(DCG, List, []).


phrase(DCG, List, Hdl):-
	expand_atom_dcg(DCG, Goal, List, Hdl),
	call(Goal).


%%%
%%
%
expand_term(Term, Expansion) :- var(Term), !,
	Term = Expansion.

expand_term(Term, Expansion) :-
	'USERCALL'(current_predicate(_,term_expansion(_,_))),
	'USERCALL'(term_expansion(Term, Temp)),
	!,
	expand_term_internal(Temp, Expansion).

expand_term(Term, Expansion) :-
	expand_term_internal(Term, Expansion).


%%%
%% Note: We allow a list of clauses to be expanded (as produced by the user
%        defined term_expansion/2).
%
expand_term_internal(Term, Expansion) :- var(Term), !,
	warning('variable clause from term_expansion/2 (in expand_term/2)').

expand_term_internal((H --> B), (Hx :- Bx)) :- !,
	nonvar(H),
	nonvar(B),
	expand_term_dcg(H, B, Hx, Bx).

expand_term_internal((Hx :- B), (Hx :- Bx)) :- !,
	expand_term_body(B, Bx).

expand_term_internal([], []) :- !.

expand_term_internal([X|Xs], [Y|Ys]) :- !,
	expand_term_internal(X, Y),
	expand_term_internal(Xs, Xs).

expand_term_internal(Term, Term).


%%%
%%  Expanding DCG rules.
%
expand_term_dcg(LHS, RHS, Head, Body) :-
	expand_head_dcg(LHS, Head, Si, Sx),
	expand_body_dcg(RHS, Body, Si, Sx).
	

%%%
%%
%
expand_head_dcg((Head,List), Hx, Si, Sx) :- !,
	closed_list_dcg(List),
	dlist(List, Sj, Sx),
	expand_atom_dcg(Head, Hx, Si, Sj).

expand_head_dcg(Head, Hx, Si, Sx) :-
	expand_atom_dcg(Head, Hx, Si, Sx).


%%%
%%
%
expand_body_dcg(Body, Goal, Si, Sx) :-
	expand_case_dcg(Body, Goal, Si, Sx).


%%%
%%  expand_each_dcg(+Term, -Goal, :StreamIn,-StreamOut)
%
%
expand_each_dcg(Term, Goal, Si, Sn) :- var(Term), !, Goal = Term,
	Sn = Si.

expand_each_dcg((P,Q), Goal, Si, Sn) :- !, Goal = (Px,Qx),
	expand_each_dcg(P, Px, Si, Sj),
	expand_each_dcg(Q, Qx, Sj, Sn).

expand_each_dcg((P;Q), Goal, Si, Sn) :- !, Goal = (Px;Qx),
	expand_case_dcg(P, Px, Si, Sn),
	expand_case_dcg(Q, Qx, Si, Sn).

expand_each_dcg((P->T), Goal, Si, Sn) :- !, Goal = (Px->Tx),
	expand_each_dcg(P, Px, Si, Sj),
	expand_case_dcg(T, Tx, Sj, Sn).

expand_each_dcg(    [], Goal, Si, Sn) :- !, Goal = (Sn = Si).

expand_each_dcg([X|Xs], Goal, Si, Sn) :- !, Goal = (Si = DL),
	closed_list_dcg(Xs),
	dlist([X|Xs], DL, Sn).

expand_each_dcg('!', Goal, Si, Sn) :- !, Goal = '!',
	Sn = Si.

expand_each_dcg({Term}, Goal, Si, Sn) :- !,
	( var(Term) ->
	    Goal = call(Term)
	; true,
	    Goal = Term
	),
	Sn = Si.

expand_each_dcg(Atom, Goal, Si, Sn) :-
	expand_atom_dcg(Atom, Goal, Si, Sn).


%%%
%%
%
expand_case_dcg(Case, Goal, Si, Sn) :-
	expand_each_dcg(Case, Temp, Si, Sj),
	( Si == Sj ->
	    linear_conj((Temp, Sn = Si), Goal)
	; true,
	    Sn = Sj,
	    Goal = Temp
	).


%%%
%%
%
expand_atom_dcg(Term, Goal, Si, Sj) :-
	functor(Term, F, N),
	N1 is N + 1,
	N2 is N + 2,
	functor(Goal, F, N2),
	arg(N1, Goal, Si),
	arg(N2, Goal, Sj),
	copy_args_dcg(N, Term, Goal).

%

dlist(   []) --> [].
dlist([X|Y]) --> [X], dlist(Y).

%

copy_args_dcg(0,_Term,_Goal) :- !.

copy_args_dcg(N, Term, Goal) :-
	arg(N, Term, X),
	arg(N, Goal, X),
	M is N - 1,
	copy_args_dcg(M, Term, Goal).

%

closed_list_dcg(List) :- nonvar(List),
	( List = [] ->
	    true
	; List = [_|Tail],
	    closed_list_dcg(Tail)
	).


%%%
%%
%
expand_term_body(X, Goal) :- var(X), !,
	Goal = call(X).

expand_term_body((P,Q), (Px,Qx)) :- !,
	expand_term_body(P, Px),
	expand_term_body(Q, Qx).

expand_term_body((P;Q), (Px;Qx)) :- !,
	expand_term_body(P, Px),
	expand_term_body(Q, Qx).

expand_term_body((P->T), (Px->Tx)) :- !,
	expand_term_body(P, Px),
	expand_term_body(T, Tx).

expand_term_body((\+ P), (\+ Px)) :- !,
	expand_term_body(P, Px).

expand_term_body(Goal, Goal).


%%%
%%  linear_conj(+Conjunction, -LinearDitto) &
%   linear_conj(+Conjunction, -LinearDitto, ?Handle)
%
linear_conj((P,Q), Conj) :- !,
	linear_conj(Q, Rest),
	linear_conj(P, Conj, Rest).

linear_conj(P,P).


linear_conj((P,Q), Conj, Rest) :- !,
	linear_conj(Q, Next, Rest),
	linear_conj(P, Conj, Next).

linear_conj(P, (P,Rest), Rest).
