
test :-
    run_list(10).

run_list(N) :-
    make_list(N,L),
    run(L,NL).

make_list(0,X) :- !, X = [].
make_list(N,[1|Xs]) :- N > 0, N2 is N - 1, make_list(N2, Xs).

% :- parallel([run/2]).

run([],[]).
run([X|Xs],[Y|Ys]) :-
    make_tree(10000,Y),
    run(Xs,Ys).

make_tree(D,Tree) :-
    D < 2, !,
    Tree = leaf.

make_tree(N,tree(node,L,R,node)) :-
    N > 1,
    N2 is N / 2,
    make_tree(N2, L),
    make_tree(N2, R).

