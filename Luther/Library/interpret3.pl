% -*- Prolog -*- 
%    File:	interpret.pl 
%    Author:	Johan Bevemyr
%    Created:	Mon Nov 25 14:09:02 1991
%    Purpose:   Meta interpreters for Luther Prolog.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% waking frozen goals
%

wake([],G) :- !, call(G).
wake([H|T],G) :- call(H), wake(T,G).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Some stuff so that we can do proper 'call(G)'. OBS! these definitions
% are not used by the meta interpreter.
%

call(Goal) :- call(Goal).

% entry point for eventgenerated goals

(Goal1,Goal2) :-
	'$choice'(C),
	trace_body(Goal1, C),
	trace_body(Goal2, C).

(Goal1 ; Goal2) :-
	'$choice'(C),
	trace_disjunction(Goal1,Goal2,C).

(Goal1 -> Goal2) :-
	'$choice'(C),
	trace_body(Goal1,C),
	!, trace_body(Goal2,C).
	
(_ ^ Goal) :-
	'$choice'(C),
	trace_body(Goal,C).

\+ Goal :-
	(call(Goal) -> fail ; true).
	
!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% entry point for calls to interpreted predicates

'$interpret_goal'(Goal) :-
	'$choice'(C),
	'$clause'(Goal,Body,0,0),
	'$interpret_body'(Body,C).
%

'$interpret_body'((Goal1,Goal2),C) :-
	!, '$interpret_body'(Goal1,C),
	'$interpret_body'(Goal2,C).

'$interpret_body'((Goal1 ; Goal2),C) :-
	!,'$inter_body1'(Goal1, Goal2, C).

'$interpret_body'((Goal1 -> Goal2),C) :-
	!, '$interpret_body'(Goal1,C), 
	!, '$interpret_body'(Goal2,C).

'$interpret_body'(_^Goal,C) :-
	!, '$interpret_body'(Goal,C).

'$interpret_body'((\+ Goal),C) :-
	!, ('$interpret_body'(Goal,C) -> fail ; true).

'$interpret_body'('!',C) :-
	!, '$cut'(C).

'$interpret_body'(Goal,C) :-
	call(Goal).

%

'$interpret_body'((Goal1 -> Goal2),C,D) :-
	!,
	'$interpret_body'(Goal1,C), 
	'$cut'(D),
	'$interpret_body'(Goal2,C).

'$interpret_body'(Goal,C,_) :- '$interpret_body'(Goal,C).

%

'$inter_body1'(G1,_, C) :- '$choice'(D),'$interpret_body'(G1,C,D).
'$inter_body1'(_,G2, C) :- '$interpret_body'(G2,C).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% entry point for calls to interpreted predicates

'$interpret_goal_trace'(Goal) :-
	'$choice'(C),
	'$clause'(Goal,Body,0,0),
	trace_body(Body,C).
%

'$interpret_goal_spy'(Goal) :-
	'$choice'(D),
	trace,
	'$get_trace_level'(N),
	'$retry_choice'(N),
	'$set_inc_trace_level'(N),
	trace_port(Goal,N,D).

%

trace_body(X,Y) :- 
	var(X), !, 
	instantiation_error(X).

trace_body((Goal1,Goal2),C) :-
	!, trace_body(Goal1,C),
	trace_body(Goal2,C).

trace_body((Goal1 ; Goal2),C) :-
	!,trace_disjunction(Goal1, Goal2, C).

trace_body((Goal1 -> Goal2),C) :-
	!, trace_body(Goal1,C), 
	!, trace_body(Goal2,C).

trace_body(_^Goal,C) :-
	!, trace_body(Goal,C).

trace_body((\+ Goal),C) :-
	!, (trace_body(Goal,C) -> fail ; true).

trace_body('!',C) :-
	!, '$cut'(C).

trace_body('notrace',C) :-
	!, notrace.

trace_body(Goal,_) :-
	'$choice'(D),
	'$trace',
	'$cut'(D),                    % ! 
	'$get_trace_level'(N),
	'$retry_choice'(N),
	'$set_inc_trace_level'(N),
	trace_port(Goal,N,D).

trace_body(Goal,C) :- call(Goal).

%
% This is defined as a backtrackable C-predicate in builtin.c.
% '$retry_choice' is used to build a choicepoint which we can
% cut to if we want to redo the current clause later. The builtin
% predicate '$retry_cut'(N) searches the choicepoint-chain for
% a '$retry_choice'(Y) choicepoint with Y == N and cuts to that 
% one.
%
% '$retry_choice'(_).
% '$retry_choice'(N) :- '$retry_choice'(N).
%	

%

trace_disjunction(G1,_, C) :-
	'$choice'(D),
	trace_if_then_else(G1,C,D).

trace_disjunction(_,G2, C) :- trace_body(G2,C).

% 

trace_if_then_else((Goal1 -> Goal2),C,D) :-
	!, trace_body(Goal1,C), 
	'$cut'(D),
	trace_body(Goal2,C).

trace_if_then_else(Goal,C,_) :- trace_body(Goal,C).

%

trace_port(Goal,Level,D) :-
	'$display'(Level), '$display'(' Call: '), '$display'(Goal), '$display'(' ? '),
	trace_read_command(Command),
	trace_call(Command,Goal,Level,D).

%

trace_read_command(Mode) :-
	ttyget0(M),
	trace_read(M,Mode).

%

trace_read(0'+, add_spy) :- !, ttyskip(10).
trace_read(0'-, remove_spy) :- !, ttyskip(10).
trace_read(0'a, abort) :- !, ttyskip(10).
trace_read(0'c, creep) :- !, ttyskip(10).
trace_read(10,  creep) :- !.
trace_read(0'f, fail) :- !, ttyskip(10).
trace_read(0'l, leap) :- !, ttyskip(10).
trace_read(0'n, traceoff) :- !, ttyskip(10).
trace_read(0'r, Mode) :- !, trace_read_retry(Mode).
trace_read(0's, skip) :- !, ttyskip(10).
trace_read(0'?, help)   :- !, ttyskip(10).
trace_read(0'h, help)   :- !, ttyskip(10).
trace_read(-1,end_of_file) :- halt.

%

trace_read_retry(retry(N)) :-
	'$ttygetch0'(Ch, Typ),
	tty_read_int(Ch,Typ,L),
	!,
	number_chars(N,L,10).


trace_read_retry(retry).

%

tty_read_int(-1,_,_) :- !, halt.

tty_read_int(10, 00,[]) :- !.

tty_read_int(_ , 00, L) :-
	!, '$ttygetch0'(Ch, Typ),
	tty_read_int(Ch,Typ,L).

tty_read_int(D,  30, [D|L]) :-
	!, '$ttygetch0'(Ch, Typ),
	tty_read_int(Ch,Typ,L).

tty_read_int(_, _, []) :-
	ttyskip(10).

%

trace_call(creep,Goal,Level,D) :-
	(   call(Goal) ->
	    report_success(Goal,Level,D) ;
	    report_failure(Goal,Level,D)
	).

trace_call(retry(Nr),Goal,Level,D) :-
	(   '$retry_cut'(Nr), fail ;
	    '$display'('{cannot find goal to retry}'),nl,
	    trace_port(Goal,Level,D)
	).

trace_call(skip,Goal,_,D) :-
	(   notrace, call(Goal) ->
	    trace, report_success(Goal,Level,D) ;
	    trace, report_failure(Goal,Level,D)
	).

trace_call(leap,Goal,_,D) :-
	(   notrace, call(Goal) ->
	    report_success(Goal,Level,D) ;
	    report_failure(Goal,Level,D)
	).

trace_call(add_spy,Goal,Level,D) :-
	'$set_spy'(Goal),
	trace_port(Goal,Level,D).

trace_call(remove_spy,Goal,Level,D) :-
	'$remove_spy'(Goal),
	trace_port(Goal,Level,D).

trace_call(traceoff, Goal,_,D) :-
	notrace,
	'$cut'(D),              % remove last retry choicepoint
	call(Goal).

trace_call(abort, _,_,_) :-
	'$display'('{ Execution aborted }'),nl,
	'$get_saved_choice'(C),
	'$cut'(C),
	fail.

trace_call(fail, Goal, Level, D) :-
	report_failure(Goal,Level,D).


trace_call(help, Goal, Level, D) :-
	'$display'('    +       add spypoint'),nl,
	'$display'('    -       remove spypoint'),nl,
	'$display'('    a       abort'),nl,
	'$display'('    c       creep (<cr>)'),nl,
	'$display'('    f       fail'),nl,
	'$display'('    l       leap'),nl,
	'$display'('    n       trace off'),nl,
	'$display'('    r       retry'),nl,
	'$display'('    r <nr>  retry <nr>'),nl,
	'$display'('    s       skip'),nl,
	'$display'('    ?       print help'),nl,
	'$display'('    h       print help'),nl,
	trace_port(Goal,Level,D).

%

report_success(G,Level,D) :- rep_suc(G,Level).

report_success(G,Level,D) :- report_failure(G,Level,D).

%

rep_suc(G,Level) :-
	'$trace', !,
	'$display'(Level), '$display'(' Exit: '), '$display'(G), nl.

rep_suc(_,_).
	
%

report_failure(G,Level,D) :-
	'$trace',!,
	'$display'(Level), '$display'(' Fail: '), '$display'(G), nl,
	'$set_dec_trace_level'(Level),
	'$cut'(D), fail.

report_failure(_,Level,D) :-
	'$set_dec_trace_level'(Level),
	'$cut'(D), fail.

%

instantiation_error(X) :-
	format(user_error,'{INSTANTIATION ERROR: ~w}').
