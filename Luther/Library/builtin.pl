%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  builtin.pl
%
%  Patric Hedlin.....Wed Jul 6 1994
%
%
% Description:
%
%   Definitions of builtin predicates in Luther Reform Prolog.
%
%
/*
:- module prolog.
*/

:- public
	statistics/2,
	repeat/0,
	number_chars/2,
	length/2,
	numbervars/3,

	assert/1,
	assert/2,
	assertz/1,
	assertz/2,
	asserta/1,
	asserta/2,

	recordz/3,
	recorda/3,
	recorded/3,

	erase/1,

	retract/1,
	retractall/1,

	current_key/2,
	predicate_property/2,

	clause/2,
	clause/3,

	abort/0,

	compile/1,
	fcompile/1,

	prolog_flag/2,
	prolog_flag/3.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


statistics(runtime,  X) :- '$statistics_runtime'(X).
statistics(walltime, X) :- '$statistics_walltime'(X).
statistics(parallel, X) :- '$statistics_parallel_runtime'(X).

statistics(gcnr,    X) :- '$statistics_gcnr'(X).
statistics(gctime,  X) :- '$statistics_gctime'(X).
statistics(gcbytes, X) :- '$statistics_gcbytes'(X).

statistics(trail,  X) :- '$statistics_trail'(X).
statistics(local,  X) :- '$statistics_local'(X).
statistics(global, X) :- '$statistics_global'(X).
statistics(memory, X) :- '$statistics_memory'(X).

statistics(deref,   X) :- '$statistics_deref'(X).
statistics(restore, X) :- '$statistics_restore'(X).

statistics(instr,      X) :- '$statistics_instr'(X).
statistics(instr_prof, X) :- '$statistics_instr_prof'(X).

statistics(tidytrail, X) :- '$statistics_tidy'(X).

statistics(swapbind, X) :- '$statistics_swap'(X).

statistics(code, X) :- '$statistics_code'(X).
statistics(atom, X) :- '$statistics_atom'(X).

%

repeat.
repeat :- repeat.

%

number_chars(N,C) :- number_chars(N,C,10).


%%%
%%  length(+List,?Lenght)
%
length(X, N) :- var(N), !,
	length_var(X, 0, N).

length(X, N) :-
	length_num(X, 0, N).


length_var(   [], N, N).
length_var([_|T], M, N) :-
	K is M + 1,
	length_var(T, K, N).


length_num(   [], N, N) :- !.
length_num([_|T], M, N) :- N > M,
	K is M + 1,
	length_num(T, K, N).


%%%
%%  numbervars(?Term,+Count,?Count')
%
numbervars(Term, M, N) :-
	( integer(M) ->
	    numbervars_aux(Term, M, N)
	; true,
	    illegal_arg(2, numbervars(Term,M,N))
	).

numbervars_aux(Term, M, N) :-
	( var(Term) ->
	    Term = '$VAR'(M),
	    N is M + 1
	; atomic(Term) ->
	    N = M
	; Term = [Head|Tail] ->
	    numbervars_aux(Head, M, Mx),
	    numbervars_aux(Tail, Mx, N)
	; Term = '$VAR'(Count), integer(Count) ->
	    ( M =:= Count ->
	        N is M + 1
	    ; true,
	        N = M
	    )
	; true,
	    Term =.. [_Functor|ArgList],
	    numbervars_aux(ArgList, M, N)
	).


%%%
%%  Dynamic predicates.
%
%
assert(Cls) :- assertz(Cls,_Ref).

assert(Cls, Ref) :- assertz(Cls, Ref).

%

assertz(Cls) :- assertz(Cls,_Ref).


assertz((Head :- Body), Ref) :- !, var(Ref),
	Ref = '$ref'(P,R),
	'$assertz'(Head, Body, P, R).

assertz(Head, Ref) :- var(Ref),
	Ref = '$ref'(P,R),
	'$assertz'(Head, true, P, R).


asserta(Cls) :- asserta(Cls,_Ref).


asserta((Head :- Body), Ref) :- !, var(Ref),
	Ref = '$ref'(P,R),
	'$asserta'(Head, Body, P, R).

asserta(Head, Ref) :- var(Ref),
	Ref = '$ref'(P,R),
	'$asserta'(Head, true, P, R).

%

assert_delete_other((Head :- Body)) :- !,
	'$assert_delete_other'(Head, Body).

assert_delete_other(Head) :-
	'$assert_delete_other'(Head, true).


%%%%%

recordz(Key, Term, Ref) :-
	assertz(prolog:'$$RECORD$$'(Key, Term), Ref).

recorda(Key, Term, Ref) :-
	asserta(prolog:'$$RECORD$$'(Key, Term), Ref).

recorded(Key, Term, Ref) :-
	clause(prolog:'$$RECORD$$'(Key, Term), true, Ref).


erase('$ref'(P,R)) :- '$erase'(P,R).


%%%%%

retract((Head :- Body)) :- !,
	clause(Head, Body, Ref),
	erase(Ref).

retract(Head) :-
	clause(Head, true, Ref),
	erase(Ref).


retractall(Head) :-
	retract(Head),
	fail.

retractall(_Head).


%%%%%

current_key(Key, Term) :-
	clause(prolog:'$$RECORD$$'(Key, Term), true).


predicate_property(Name, Property) :-
	current_predicate(_, Name),
	'$predicate_property'(Name, Property).


%%%%%

clause(Head, Body) :- '$clause'(Head, Body, 0, 0).

clause(Head, Body, Ref) :- '$clause'(Head, Body, Ref, 0, 0).


%%%%%

abort :-
	'$display'('{Execution aborted}'), nl,
	'$load_choice'(C),
	'$cut'(C),
	fail.


%%%
%%   Compiler entrys (resembling SICStus Prolog) invoking sequential
%  compilation (only available if the compiler is loaded).
%
compile(File) :-
	'USERCALL'(rcompile(File, [-analyse, output('/tmp/a.wam')])),
	load('/tmp/a.wam').

fcompile(File) :-
	'USERCALL'(rcompile(File, [-analyse, output_separate])).


%

prolog_flag(Flag,New) :- prolog_flag(Flag,_,New).


prolog_flag(  gc_verbose, Old, New) :- prolog_flag_gc_verbose(Old, New).
prolog_flag(load_verbose, Old, New) :- prolog_flag_load_verbose(Old, New).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%   Non-standard builtins (not documented).
%

eq(X, X).

hd(X, [X|_]).

tl(X, [_|X]).


member(Object, List) :-
	( var(Object) ->
	    member_var(Object, List)
	; true,
	    member_chk(Object, List)
	).

member_var(X, [H|_]) :- X = H.
member_var(X, [_|T]) :-
	member_var(X, T).

member_chk(X,  LST ) :- var(LST), !, fail.
member_chk(X, [H|T]) :-
	( X == H ->
	    true
	; true,
	    member_chk(X, T)
	).


append(    [], Ys, Ys).
append([X|Xs], Ys, Zs) :- Zs = [X|Zr],
	append(Xs, Ys, Zr).


split_list(0, List, Prefix, Suffix) :- !,
	Prefix = [],
	Suffix = List.
split_list(N,[H|T], Prefix, Suffix) :-
	Prefix = [H|Rest],
	M is N - 1,
	split_list(M, T, Rest, Suffix).


