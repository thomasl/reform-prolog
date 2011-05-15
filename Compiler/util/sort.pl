%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  sort.pl - implementation of quick sort
%
%
% Note: Provided in case no sort is available at the target system.
%

sort(List, Sort) :- sort(List, Sort, []).


%%%
%%  sort(+List, ?SortedList, ?Handle)
%
%
sort(    [], Hdl, Hdl).

sort([X|Xs], Out, Hdl) :-
	partition(Xs, X, Sml, Big),
	sort(Sml, Out, Sdl),
	Sdl = [X|Bdl],
	sort(Big, Bdl, Hdl).


%%%
%%  partition(+List, +Median, -Smaller, -Bigger)
%
%
partition(    [],_M,  [],  []).

partition([X|Xs], M, Sml, Big) :- X @< M, !,
	Sml = [X|Ss],
	partition(Xs, M, Ss, Big).

partition([X|Xs], M, Sml, Big) :- X @> M, !,
	Big = [X|Bs],
	partition(Xs, M, Sml, Bs).

partition([X|Xs], M, Sml, Big) :- %%% X == M,
	partition(Xs, M, Sml, Big).

