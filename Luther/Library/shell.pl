%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  shell.pl
%
%  Patric Hedlin.....Mon Jul 4 1994
%
%
% Description:
%
%   This is where we define the user (debug) shell (i.e. the interface to the
% interactive linker, loader and debugger).
%
%

:- module(user).



shell :- shell_top_loop.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  The top loop and query.
%

shell_top_loop :- 
	repeat,
	worker_rest,
	current_module(X),
	( X == user ->
	    true
	; true,
	    report('~w', [X])
	),
	on_exception(EXC, shell_top_query, (format('{caught exception: ~q}~n', [EXC]), fail)).


shell_top_query :-
	'$choice'(Choice),
	enter_choice(Choice),
	shell_read_query(Query, Variables),
	enter_tracer(Query),
	worker_rest,
	shell_display_solution(Variables),
	shell_request_solution(Variables),
	format('~n~w~n', [yes]),
	!,
	fail.

shell_top_query :-
	format('~n~w~n', [no]),
	fail.


%

shell_display_solution([]).

shell_display_solution([Name = Value|Vs]) :-
	shell_display_solution(Vs, Name, Value).


shell_display_solution([], Name, Value) :- !,
	format('~n~s = ~q.', [Name,Value]).

shell_display_solution(Vs, Name, Value) :-
	format('~n~s = ~q,', [Name,Value]),
	shell_display_solution(Vs).


shell_request_solution([]) :- !.

shell_request_solution(_) :-
	shell_read_next(Ch), Ch \== 0';.

%

shell_read_query(Query, VarList) :-
	prompt(Prompt, '| ?- '), read_term(Query, [variables(VarList)]),
	prompt(_VOID_, Prompt).


shell_read_next(Ch) :-
	prompt(Prompt, '  ?  '), shell_read_next_char(Ch),
	prompt(_VOID_, Prompt).


shell_read_next_char(Ch) :- get0(Ch),
	( Ch == 10 -> true ; skip(10) ).

