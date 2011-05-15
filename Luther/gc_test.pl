
test :-
    run_list([1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0]).

test(N) :-
    large_list(N,List),
    run_list(List).

:- parallel([run_list/1]).

run_list([]).
run_list([X|Xs]) :-
    large_list(10000,Y),
    run_list(Xs).

large_list(0,X) :- !, X = [].
large_list(N,[1|T]) :-
    N > 0,
    N2 is N - 1,
    large_list(N2,T).

