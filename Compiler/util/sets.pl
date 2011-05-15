
:- ensure_loaded(basics).

% Simple implementation of sets.

union(X1,X2,X) :-
	append(X1,X2,X).

%

intersect([],_,[]).

intersect([X|Xs],Ys,Zs) :-
	( soft_member(X,Ys) ->
	  Zs = [X|Zs0]
	; Zs = Zs0
	),
	intersect(Xs,Ys,Zs0).

%

diff([],_,[]).

diff([X|Xs],Ys,Zs) :-
	( soft_member(X,Ys) ->
	  Zs = Zs0
	; Zs = [X|Zs0]
	),
	diff(Xs,Ys,Zs0).

% Constructing the powerset is a 2^n complexity operation -- since
% that many elements are produced! (It is probably better done after
% some thinking.)

powerset([],[[]]).

powerset([X|Xs],Ys) :-
	powerset(Xs,Zs),
	append_with_prepend(Zs,Zs,X,Ys).

%

append_with_prepend([],Ys,_,Ys).

append_with_prepend([X|Xs],Ys,E,[[E|X]|Zs]) :-
	append_with_prepend(Xs,Ys,E,Zs).

%
