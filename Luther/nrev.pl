/*
 * nrev.pl
 *
 */

run :- run_list(10).

run_list(N) :-
	run_list(N,List),
	run(List).

run_list(0,X) :- !, X = [].
run_list(N,[N|Ns]) :- N2 is N - 1, run_list(N2,Ns).

run(List) :-
	length(List, N),
%	statistics(instr,[SeqA, ParA]),
%	statistics(walltime, [T1 | _]),
	statistics(runtime, [T1 | _]),
	nrev(List, _),
	statistics(runtime, [T2 | _]),
%	statistics(walltime, [T2 | _]),
%	statistics(instr,[SeqB, ParB]),
	Time is T2 - T1,
%	Instr is (SeqB + ParB) - (SeqA + ParA),
 	format('Reversed list of ~q elements (time ~q s)~n', [N, Time]),
	format('map: ~w instructions executed~n',[Instr]).

% :- parallel([nrev/2]).

nrev(    [], []).
nrev([X|Xs],  Y) :- nrev(Xs, Z), append(Z, [X], Y).

append(    [],  X,      X).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
