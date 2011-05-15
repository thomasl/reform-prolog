%----------------------------------------
% Sequence matching (sequences has length 32 in the test example)
% Matches N random sequences against a random target sequence.

% :- ensure_loaded(utilities).

test_seq :-
	read(N),
	integer(N),
	test_seq(N,Res,Time, 17, _Seed),
	format('time ~q for result ~n',[Time]).

% test_seq(+N, -Res, -Time):

test_seq(N, Res, Time, Seed0, Seed2) :-
    random_sequence(32, Side, Seed0, Seed1),
    random_sequences(N, 32, Tops, Seed1, Seed2),
	statistics(instr,[SeqA, ParA]),
    statistics(walltime, [T1|_]),
    match_sequences(Tops, Side, Res),
    statistics(walltime, [T2|_]),
	statistics(instr,[SeqB, ParB]),
	Instr is (SeqB + ParB) - (SeqA + ParA),
    Time is T2-T1,
	format('map: ~w instructions executed~n',[Instr]).

/**************************************************/

:- parallel([match_sequences/3]).

match_sequences([], _Side, _Tree).
match_sequences([Top|Tops], Side, Tree) :-
    match_two_seq(Side, Top, none, entry(0,0,0,0), Sim),
    put_in_tree(Tree, Sim),
    match_sequences(Tops, Side, Tree).

match_two_seq([], _, _, Res, Res).
match_two_seq([S|Side], Top, Prev, _, Res) :-
    match(Top, S, Prev, Row, entry(0,0,0,0), entry(0,0,0,0), L),
    match_two_seq(Side, Top, Row, L, Res).

match([], _, _, [], _, L, L).
match([T|Top], S, none, [E|Row], Zero, W, L) :- !,
    match_entry(T, S, Zero, Zero, W, E),
    match(Top, S, none, Row, Zero, E, L).
match([T|Top], S, [N|Prev], [E|Row], NW, W, L) :- 
    match_entry(T, S, NW, N, W, E),
    match(Top, S, Prev, Row, N, E, L).

match_entry(T, S, Entry1, Entry2, Entry3, Entry4) :-
    Entry1 = entry(NWmax,NWnw,_,_),
    Entry2 = entry(Nmax,Nnw,Nn,_),
    Entry3 = entry(Wmax,Wnw,_,Ww),
    Entry4 = entry(Emax,Enw,En,Ew),
    alpha_beta_penalty(Wnw, Ww, Ew),
    alpha_beta_penalty(Nnw,Nn,En),
    match_weights(T, S, Weight),
    Enw1 is NWnw + Weight,
    maxl([0,Ew,En], Enw1, Enw),
    maxl([NWmax,Wmax,Nmax], Enw, Emax).

alpha_beta_penalty(X, Y, Z) :-
    X1 is X - 4,            % alpha (indel) penalty 
    Y1 is Y - 1,            % beta (extension) penalty 
    max(Y1, X1, Z).

maxl([], X, X).
maxl([Y|R], X, Z) :- 
    max(X, Y, X1), 
    maxl(R, X1, Z).

max(X, Y, Z) :- X<Y, !, Z = Y.
max(X, Y, X).

% Dummy weights

match_weights(X, X, 1) :- !.
match_weights(_,_,0).

put_in_tree(X,Y) :- 
    var(X), !, 
    X = t(_, Y, _).
put_in_tree(t(_, X, _), Y) :- 
    X == Y, !.
put_in_tree(t(L, Y, _R), X) :-
    X @< Y, !,
    put_in_tree(L, X).
put_in_tree(t(_L, _Y, R), X) :-
    put_in_tree(R, X).

/**************************************************/

random_sequence(0, [], Seed0, Seed1) :- !, Seed0 = Seed1.
random_sequence(N, [X|Xs], Seed0, Seed2) :-
    random(10, X, Seed0, Seed1),
    N1 is N-1,
    random_sequence(N1, Xs, Seed1, Seed2).

random_sequences(0, _, [], Seed0, Seed1) :- !, Seed0 = Seed1.
random_sequences(N, I, [X|Xs], Seed0, Seed2) :-
    random_sequence(I, X, Seed0, Seed1),
    N1 is N-1,
    random_sequences(N1, I, Xs, Seed1, Seed2).

% Declarative random-function.

random(Limit, N, S, S1) :-
   N is (S mod Limit)+1,
   S1 is (125*S+1) mod 4096.
