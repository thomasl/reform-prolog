%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  interpret.pl - Meta interpreter for Luther Prolog.
%
%  Johan Bevemyr.....Mon Nov 25 1991
%  Patric Hedlin.....Tue Feb 28 1995
%
%
% Description:   
%
%
/*
:- module prolog.
*/

:- public
	enter_choice/1,
	enter_tracer/1,
	spy/1,
	nospy/1,
	trace/0,
	notrace/0,

	call/1,	','/2, ';'/2, '->'/2, '^'/2, '\+'/1, '!'/1.



%%%
%%  enter_choice(+ChoicePoint) & enter_tracer(+Query)
%
%   These predicates are exported (made public) in order to support the
% interactive shell (top loop).
%
%
enter_choice(Choice) :-
	'$save_choice'(Choice),
	'$set_inc_trace_level'(0),
	( '$trace' ->
	    report('~w', [trace])
	; true ).


enter_tracer(Query) :-
	'$topchoice'(Choice),
	trace_body(Query, Choice).


%%%
%%  spy(Predicate) & nospy(Predicate)
%
spy(P/N) :-
	atom(P),
	integer(N),
	functor(Goal, P, N),
	'$set_spy'(Goal),
	'$trace_spy'.

nospy(P/N) :-
	atom(P),
	integer(N),
	functor(Goal, P, N),
	'$remove_spy'(Goal).


%%%
%%  trace & notrace
%
trace :- '$trace_on'.

notrace :- '$trace_off'.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%   Waking frozen goals.
%

wake(   [], Goal) :- !,
	call(Goal).

wake([H|T], Goal) :-
	call(H), wake(T, Goal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%   Some stuff so that we can do proper 'call(G)'.
%
% Note: These definitions are not used by the meta interpreter.
%

call(Goal) :- call(Goal).

%%
%  Entry point for event generated goals.
%

(Goal1 , Goal2) :-
	'$choice'(C),
	trace_body(Goal1, C),
	trace_body(Goal2, C).

(Goal1 ; Goal2) :-
	'$choice'(C),
	trace_disjunction(Goal1, Goal2, C).

(Goal1 -> Goal2) :-
	'$choice'(C),
	trace_body(Goal1, C),
	!,
	trace_body(Goal2, C).
	
(_ ^ Goal) :-
	'$choice'(C),
	trace_body(Goal, C).

\+ Goal :-
	(call(Goal) -> fail ; true).
	
!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Entry point for calls to interpreted predicates.
%

'$interpret_goal'(Goal) :-
	'$choice'(C),
	'$clause'(Goal, Body, 0, 0),
	'$interpret_body'(Body, C).
%

'$interpret_body'((Goal1 , Goal2), C) :- !,
	'$interpret_body'(Goal1, C),
	'$interpret_body'(Goal2, C).

'$interpret_body'((Goal1 ; Goal2), C) :- !,
	'$interpret_disj'(Goal1, Goal2, C).

'$interpret_body'((Goal1 -> Goal2), C) :- !,
	'$interpret_body'(Goal1, C), 
	!,
	'$interpret_body'(Goal2, C).

'$interpret_body'(_ ^ Goal, C) :- !,
	'$interpret_body'(Goal, C).

'$interpret_body'((\+ Goal), C) :- !,
	( '$interpret_body'(Goal, C) -> fail ; true ).

'$interpret_body'('!', C) :- !,
	'$cut'(C).

'$interpret_body'(Goal, C) :-
	call(Goal).

%

'$interpret_disj'( Goal1,_Goal2, C) :-
	'$choice'(D),
	'$interpret_cond'(Goal1, C, D).

'$interpret_disj'(_Goal1, Goal2, C) :-
	'$interpret_body'(Goal2, C).

	
'$interpret_cond'((Goal1 -> Goal2), C, D) :- !,
	'$interpret_body'(Goal1, C), 
	'$cut'(D),
	'$interpret_body'(Goal2, C).

'$interpret_cond'(Goal, C,_D) :-
	'$interpret_body'(Goal, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Entry point for calls to interpreted predicates (in trace mode).
%
%
% Note: '$retry_choice'/1
%
%   The predicate '$retry_choice'/1 is defined as a backtrackable C-predicate
% (in builtin.c), it is used to build a choice-point which we can cut to if we
% want to redo the current clause later. The builtin predicate '$retry_cut'(N)
% searches the choice-point chain for a '$retry_choice'(Y) choice-point (where
% Y = N) and performs a cut to the choice-point found.
%
% '$retry_choice'(_).
% '$retry_choice'(N) :- '$retry_choice'(N).
%	

'$interpret_goal_spy'(Goal) :-
	'$choice'(D),
	'$trace_on',
	'$get_trace_level'(N),
	'$retry_choice'(N),
	'$set_inc_trace_level'(N),
	trace_port(Goal, N, D).


'$interpret_goal_trace'(Goal) :-
	'$choice'(C),
	trace_body(Goal, C).

%%%

'$interpret_goal_trace_internal'(Goal) :-
	'$choice'(C),
	'$clause'(Goal, Body, 0, 0),
	trace_body(Body, C).

%

trace_body(Goal,_C) :- var(Goal), !, 
	instantiation_error(Goal).

trace_body((Goal1 , Goal2), C) :- !,
	trace_body(Goal1, C),
	trace_body(Goal2, C).

trace_body((Goal1 ; Goal2), C) :- !,
	trace_disj(Goal1, Goal2, C).

trace_body((Goal1 -> Goal2), C) :- !,
	trace_body(Goal1, C), 
	!,
	trace_body(Goal2, C).

trace_body(_ ^ Goal, C) :- !,
	trace_body(Goal, C).

trace_body((\+ Goal), C) :- !,
	( trace_body(Goal, C) -> fail ; true ).

trace_body('!', C) :- !,
	'$cut'(C).

trace_body('notrace', C) :- !,
	notrace.

trace_body(true,_C) :- !.

trace_body(Goal,_C) :-
	'$choice'(D),
	( '$trace' ->
	    '$get_trace_level'(N),
	    '$retry_choice'(N),
	    '$set_inc_trace_level'(N),
	    trace_port(Goal, N, D)
	; true,
	    call(Goal)
	).

%

trace_disj( Goal1,_Goal2, C) :-
	'$choice'(D),
	trace_cond(Goal1, C, D).

trace_disj(_Goal1, Goal2, C) :-
	trace_body(Goal2, C).


trace_cond((Goal1 -> Goal2), C, D) :- !,
	trace_body(Goal1, C), 
	'$cut'(D),
	trace_body(Goal2, C).

trace_cond(Goal, C,_D) :-
	trace_body(Goal, C).

%

trace_port(Goal, Level, D) :-
	format('~d  ~w:  ~w ? ', [Level,'Call',Goal]),
	trace_read_command(Command),
	trace_call(Command, Goal, Level, D).

%

trace_read_command(Mode) :-
	repeat,
	ttyget0(Ch),
	trace_read(Ch, Mode), !.

%

trace_read( -1, abort) :- !.
trace_read( 10, creep) :- !.
trace_read(0'+, add_spy) :- !, ttyskip(10).
trace_read(0'-, remove_spy) :- !, ttyskip(10).
trace_read(0'a, abort) :- !, ttyskip(10).
trace_read(0'c, creep) :- !, ttyskip(10).
trace_read(0'f, fail) :- !, ttyskip(10).
trace_read(0'h, help) :- !, ttyskip(10).
trace_read(0'l, leap) :- !, ttyskip(10).
trace_read(0'n, notrace) :- !, ttyskip(10).
trace_read(0'r, Mode) :- !, trace_read_retry(Mode).
trace_read(0's, skip) :- !, ttyskip(10).
trace_read(0'w, wamdebug) :- !, ttyskip(10).
trace_read(0'@, command) :- !, ttyskip(10).
trace_read(0'?, help) :- !, ttyskip(10).

%

trace_read_retry(retry(N)) :- tty_read_int(N).

trace_read_retry(retry).


tty_read_int(N) :-
	'$ttygetch0'(Typ, Ch),
	tty_read_int(Typ, Ch, CharList),
	!,
	number_chars(N, CharList).


tty_read_int(00, 10, []) :- !.

tty_read_int(00, -1,_Cs) :- !,
	report('Execution aborted'),
	'$load_choice'(C),
	'$cut'(C),
	fail.

tty_read_int(00, _ , Cs) :- !,
	'$ttygetch0'(Typ, Ch),
	tty_read_int(Typ, Ch, Cs).

tty_read_int(30, Di, [Di|Cs]) :- !,
	'$ttygetch0'(Typ, Ch),
	tty_read_int(Typ, Ch, Cs).

tty_read_int(_, _, []) :- ttyskip(10).

%

trace_call(add_spy, Goal, Level, D) :-
	( '$impose_spy'(Goal) ->
	    functor(Goal, Name, Arity),
	    report('spy point set on ~q/~d', [Name,Arity])
	; true ),
	trace_port(Goal, Level, D).

trace_call(remove_spy, Goal, Level, D) :-
	( '$remove_spy'(Goal) ->
	    functor(Goal, Name, Arity),
	    report('spy point removed from ~q/~d', [Name,Arity])
	; true ),
	trace_port(Goal, Level, D).

trace_call(abort, _,_,_) :-
	report('Execution aborted'),
	'$load_choice'(C),
	'$cut'(C),
	fail.


trace_call(creep, true,_Level,_D) :- !.

trace_call(creep, Goal, Level, D) :-
	( predicate_property(Goal, interpreted) ->
	  ( '$interpret_goal_trace_internal'(Goal) ->
	      report_success(Goal, Level)
	  ; true,
	      report_failure(Goal, Level, D)
	  )
	; call(Goal) ->
	    report_success(Goal, Level)
	; true,
	    report_failure(Goal, Level, D)
	).

trace_call(fail, Goal, Level, D) :-
	report_failure(Goal, Level, D).

trace_call(leap, Goal, Level, D) :-
	( '$trace_spy', call(Goal) ->
	    report_success(Goal, Level)
	; true,
	    report_failure(Goal, Level, D)
	).

trace_call(notrace, Goal,_Level, D) :-
	'$trace_off',
	'$cut'(D),              % remove last retry choice-point
	call(Goal).

trace_call(retry(Nr), Goal, Level, D) :-
	( '$retry_cut'(Nr),
	  fail
	; report('unable to find goal (~d) in order to retry', [Nr]),
	  trace_port(Goal, Level, D)
	).

trace_call(skip, Goal, Level, D) :-
	( '$trace_off', call(Goal) ->
	    '$trace_on',
	    report_success(Goal, Level)
	; true,
	    '$trace_on',
	    report_failure(Goal, Level, D)
	).

trace_call(wamdebug, Goal, Level, D) :-
	wamdebug,
	trace_port(Goal, Level, D).

trace_call(command, Goal, Level, D) :-
	read(Command),
	'$trace_off',
	( call(Command) -> Res = yes ; Res = no ), report(Res),
	'$trace_on',
	trace_port(Goal, Level, D).

trace_call(help, Goal, Level, D) :-
	'$display'('    +       add spy-point'),nl,
	'$display'('    -       remove spy-point'),nl,
	'$display'('    a       abort'),nl,
	'$display'('    c       creep (<cr>)'),nl,
	'$display'('    f       fail'),nl,
	'$display'('    l       leap'),nl,
	'$display'('    n       trace off'),nl,
	'$display'('    r       retry'),nl,
	'$display'('    r <nr>  retry <nr>'),nl,
	'$display'('    s       skip'),nl,
	'$display'('    w       enter wam debugger'),nl,
	'$display'('    @       enter command'),nl,
	'$display'('    ?       print help'),nl,
	'$display'('    h       print help'),nl,
	trace_port(Goal,Level,D).

%

report_success( Goal, Level) :-
	( '$trace' ->
	    format('~d  ~w:  ~p~n', [Level,'Exit',Goal])
	; true ).


report_failure( Goal, Level, D) :-
	( '$trace' ->
	    format('~d  ~w:  ~p~n', [Level,'Fail',Goal])
	; true ),
	'$set_dec_trace_level'(Level),
	'$cut'(D),
	fail.


instantiation_error(X) :-
	error('illegal instantiation: ~w', [X]).
