%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  listing.pl
%
%  Patric Hedlin.....Mon Jun 27 1994
%
%
% Description:
%
%   listing(?Predicates) is supposed to write the predicates out so that they
% can be read back in exactly as they are, provided the operator declarations
% haven't changed. So it has to use writeq.
%
%
/*
:- module prolog.
*/

:- public
	listing/0,
	listing/1,
	listing/2,
	listery/0,
	portray_clause/1,
	portray_clause/2.



listing :-
	predicate_property(Predicate, interpreted),
	portray_predicate(Predicate),
	fail.

listing.


listing(Signature) :- var(Signature), !,
	listing.

listing(Signature) :-
	predicate_signature(Signature, Module, Functor, Arity),
	predicate_property(Module:Head, interpreted),
	functor(Head, Functor, Arity),
	portray_predicate(Module:Head),
	fail.

listing(_).


listing(Stream, Signature) :- with_default_output(Stream, listing(Signature)).


%%%
%%  Show the contents of the current load image.
%
listery :-
	predicate_property(Predicate, Property),
	functor(Predicate, P, N),
	format('~w~t: ~q/~d~n', [Property,P,N]),
	fail.

listery.


%%%
%%  Portray a single clause.
%
portray_clause((H :- B)) :- !,
	portray_clause_aux(H, B).

portray_clause(H) :-
	portray_clause_aux(H, true).


portray_clause(Stream, Clause) :-
	with_default_output(Stream, portray_clause(Clause)).


portray_clause_aux(H, B) :- nonvar(H),
	prettyvars((H,B)),
	portray_head(H),
	( B \== true ->
	    format('~w~n', [':-']),
	    portray_body(B, 8)
	; true ),
	format('~w~n', ['.']).
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Private predicates.
%

%%%
%%  predicate_signature(+Signature, ?Module, ?Functor, ?Arity)
%
predicate_signature(S, M, F, A) :-
	( S = M:N ->
	    ( N = F/A -> true ; N = F )
	; S = F/A ->
	    current_module(M)
	; atom(S) ->
	    S = F,
	    current_module(M)
	).

	
%%%
%%  Portray a single predicate (given its signature).
%
portray_predicate(Head) :-
	clause(Head, Body),
	portray_clause_aux(Head, Body),
	fail.

portray_predicate(_) :- nl.



portray_head(H) :- writeq(H).


portray_body(X,_Indent) :- var(X), !,
	sys_error('portray_body/2: var arg').

portray_body((Ci , Cj), Indent) :- !,
	portray_body(Ci, Indent),
	format('~w~n', [',']),
	portray_body(Cj, Indent).

portray_body((Ci ; Cj), Indent) :- !,
	IndentNext is Indent + 2,
	format('~*c~w ', [Indent,0' ,'(']),
	portray_body_aux(Ci, IndentNext),
	format('~n~*c~w ', [Indent,0' ,';']),
	portray_body_aux(Cj, IndentNext),
	format('~n~*c~w', [Indent,0' ,')']).

portray_body((Ci -> Cj), Indent) :- !,
	portray_body(Ci, Indent),
	format(' ~w~n', ['->']),
	portray_body(Cj, Indent).

portray_body(C, Indent) :- format('~*c~q', [Indent,0' ,C]).


portray_body_aux((Ci , Cj), Indent) :- !,
	portray_body_aux(Ci, Indent),
	format('~w~n', [',']),
	portray_body(Cj, Indent).

portray_body_aux((Ci ; Cj), Indent) :- !,
	IndentNext is Indent + 2,
	format('~w ', ['(']),
	portray_body_aux(Ci, IndentNext),
	format('~n~*c~w ', [Indent,0' ,';']),
	portray_body_aux(Cj, IndentNext),
	format('~n~*c~w', [Indent,0' ,')']).

portray_body_aux((Ci -> Cj), Indent) :- !,
	portray_body_aux(Ci, Indent),
	format(' ~w~n', ['->']),
	portray_body(Cj, Indent).

portray_body_aux(C,_Indent) :- writeq(C).


%%%
%%  We use a pretty formater (instead of numbervars/3) in order to print
% singelton (anonymous) variables using underscore.
%
prettyvars(Term) :-
	prettyvars(Term, VarList, []),
	keysort(VarList, SortedVarList),
	singlevars(SortedVarList, 0).


prettyvars( Term ) --> { var(Term) }, !, [Term-[]].

prettyvars( Term ) --> { atomic(Term) }, !, [].

prettyvars([X|Xs]) --> !,
	prettyvars(X),
	prettyvars(Xs).

prettyvars( Term ) --> { Term =.. [_Functor|Arguments] },
	prettyvars(Arguments).



singlevars(      [],_M).

singlevars([U,V|Vs], M) :- U == V, !,
	U = '$VAR'(M)-[],
	N is M+1,
	singlevars(Vs, U, N).

singlevars(['$VAR'('_')-[]|Vs], M) :-
	singlevars(Vs, M).


singlevars([V|Vs], U, M) :- V == U, !,
	singlevars(Vs, U, M).

singlevars(   Vs ,_U, M) :-
	singlevars(Vs, M).

