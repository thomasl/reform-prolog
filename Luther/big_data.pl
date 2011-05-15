main :- 
	build_big_data(190000,D),
	garbage_collect,
	statistics,
	statistics(gctime,_),
	statistics(gcnr,_),
	statistics(gcbytes,_),
	statistics(runtime,_),
	big2_gc(10,D),
	statistics(runtime,[_,R|_]),
	statistics(gctime,[_,G|_]),
	statistics(gcnr,[_,N|_]),
	statistics(gcbytes,[_,B|_]),
        format("total gc time ~w~n",[G]),
        format("total number of gc ~w~n",[N]),
        format("total collected bytes ~w~n",[B]),
	format("total run time ~w~n",[R]).

big(0,_,_) :- !.
big(_,X,Y) :- big_data(X,Y,_).
big(N,X,Y) :- 
%	collect_garbage,
	N2 is N - 1,
	big(N2,X,Y).

big_data(0,_,_) :- !, fail.
big_data(N,S,_) :- 
	N2 is N - 1,
	build_big_data(S,D),
%	collect_garbage,
	big_data(N2,S,D).


build_big_data(0,[]) :- !.
build_big_data(N,[T]) :- 
	N2 is N - 1,
	build_big_data(N2,T).


big2_gc(0,_) :-!.
big2_gc(N,D) :-
	garbage_collect,
	N2 is N - 1,
	big2_gc(N2,D).
