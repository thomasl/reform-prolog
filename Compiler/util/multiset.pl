%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  multiset.pl
%
%
% Description:
%
%

:- ensure_loaded(basics).


multiset_union(Xs, Ys, Union) :-
	append(Xs, Ys, Union).


%%%
%%  multiset_intersection(+MultiSet, +MultiSet, ?Intersection)
%
multiset_intersection(    [],_Ys, []).

multiset_intersection([X|Xs], Ys, Zs) :-
	( delete_from_list(Ys, X, Left) -> %%% Remove if found.
	    Zs = [X|Rest]
	; true,
	    Zs = Rest,
	    Ys = Left
	),
	multiset_intersection(Xs, Left, Rest).


%%%
%%  multiset_difference(+MultiSet, +MultiSet, ?Difference)
%
multiset_difference(    [], Ys, Ys).

multiset_difference([X|Xs], Ys, Zs) :-
	multiset_delete_one(Ys, X, Left),
	multiset_difference(Xs, Left, Zs).


%%%
%%  multiset_delete(+MultiSet, +Item, -MultiSet)
%
multiset_delete(    [],_Item, []).

multiset_delete([X|Xs], Item, Ys) :-
	( X == Item ->
	    multiset_delete(Xs, Item, Ys)
	; true,
	    Ys = [X|Rest],
	    multiset_delete(Xs, Item, Rest)
	).


%%%
%%  multiset_delete_one(+MultiSet, +Item, -MultiSet)
%
multiset_delete_one(    [],_Item, []).

multiset_delete_one([X|Xs], Item, Ys) :-
	( X == Item ->
	    Ys = Xs
	; true,
	    Ys = [X|Rest],
	    multiset_delete_one(Xs, Item, Rest)
	).
