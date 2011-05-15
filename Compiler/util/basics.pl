
% Basic predicates employed here and there.


max(X,Y,Z) :- ( X < Y -> Z = Y ; Z = X ).

min(X,Y,Z) :- ( X > Y -> Z = Y ; Z = X ).


append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).


reverse(Xs,Ys) :- reverse(Xs,[],Ys).

reverse([],Ys,Ys).
reverse([X|Xs],Ys,Zs) :- reverse(Xs,[X|Ys],Zs).


member(X,[Y|_]) :- X = Y.
member(X,[_|Ys]) :- member(X,Ys).


% soft_member/2

soft_member(X,[Y|_]) :- X == Y.
soft_member(X,[_|Ys]) :- soft_member(X,Ys).


% list_to_conj/[23]

list_to_conj([],true).
list_to_conj([X|Xs],Conj) :-
	list_to_conj(Xs,X,Conj).

list_to_conj([],X,X).
list_to_conj([X|Xs],Y,(Y,Ys)) :- list_to_conj(Xs,X,Ys).

%

l_to_conj([],Conj,Conj).
l_to_conj([X|Xs],(X,TailConj),Rest) :-
	l_to_conj(Xs,TailConj,Rest).


% conj_to_list/[23]

conj_to_list(Conj,Xs) :- conj_to_list(Conj,Xs,[]).

conj_to_list((X,Xs)) --> !,
	conj_to_list(X),
	conj_to_list(Xs).
conj_to_list(X) --> [X].


% flatten_formula/3

flatten_formula(Formula,FlatFormula) :-
	flatten_formula(Formula,Xs,[]),
	list_to_conj(Xs,FlatFormula).

flatten_formula((P,Q)) --> !,
	flatten_formula(P),
	flatten_formula(Q).

flatten_formula((P->Q)) --> !,
	{ flatten_formula(P,A),
	  flatten_formula(Q,B)
	},
	[(A->B)].

flatten_formula((P;Q)) --> !,
	{ flatten_formula(P,A),
	  flatten_formula(Q,B)
	},
	[(A;B)].

flatten_formula(\+(P)) --> !,
	{ flatten_formula(P,A)
	},
	[\+(A)].

flatten_formula(X) --> [X].

%

dlist(    []) --> [].
dlist([X|Xs]) --> [X], dlist(Xs).


% max_in_list/[23]

max_in_list([X|Xs], Max) :-
	max_in_list(Xs, Max, X).

max_in_list(    [], Max, Max).
max_in_list([X|Xs], Max, Tmp) :- X > Tmp, !,
	max_in_list(Xs, Max, X).
max_in_list([_|Xs], Max, Tmp) :-
	max_in_list(Xs, Max, Tmp).


% reduce any multiple items in +SourceList, producing -TargetList.

reduce_from_list(    [],     []).
reduce_from_list([X|Xs], [X|Zs]) :-
	delete_all_from_list(Xs, X, Ys),
	reduce_from_list(Ys, Zs).


% delete_all succeeds when all +Items are deleted from +SourceList, producing -TargetList.

delete_all_from_list(    [],_Item, []).
delete_all_from_list([X|Xs], Item, Ys) :- X == Item, !,
	delete_all_from_list(Xs, Item, Ys).
delete_all_from_list([X|Xs], Item, Ys) :-
	Ys = [X|Zs],
	delete_all_from_list(Xs, Item, Zs).


% delete succeeds when +Item can be removed from the +SourceList, producing -TargetList.

delete_from_list([X|Xs], Item, Ys) :- X == Item, !,
	Ys = Xs.
delete_from_list([X|Xs], Item, [X|Ys]) :-
	delete_from_list(Xs, Item, Ys).

%

repeat(0,_Item, Xs) :- !,
	Xs = [].
repeat(N, Item, Xs) :- N > 0, 
	M is N - 1,
	Xs = [Item|Ys],
	repeat(M, Item, Ys).

%

flatten(List, Flat) :-
	flatten(List, Flat, []).

flatten(    []) --> !, [].
flatten([X|Xs]) --> !,
	flatten(X),
	flatten(Xs).
flatten(Object) --> [Object].



select(X, [X|_]).
select(X, [_|Xs]) :-
	select(X, Xs).

