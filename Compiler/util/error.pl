%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%			    ERROR MESSAGES
%
%   User, System and Debug (warning and error) messages are reported using
% this mechanism.
%
% 

error(String) :-
	usr_message('Error: ', String),
	fail.

error(String, ArgList) :-
	usr_message('Error: ', String, ArgList),
	fail.


warning(String) :-
	usr_message('Warning: ', String).

warning(String, ArgList) :-
	usr_message('Warning: ', String, ArgList).


report(String) :-
	usr_message('', String).

report(String, ArgList) :-
	usr_message('', String, ArgList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% System messages.
%
sys_error(String) :-
	sys_message('ERROR: ', String),
	fail.

sys_error(String, ArgList) :-
	sys_message('ERROR: ', String, ArgList),
	fail.


sys_warning(String) :-
	sys_message('WARNING: ', String).

sys_warning(String, ArgList) :-
	sys_message('WARNING: ', String, ArgList).


sys_report(String) :-
	sys_message('', String).

sys_report(String, ArgList) :-
	sys_message('', String, ArgList).


nyi :-
	sys_error('Procedure not yet implemented').

nyi(X) :-
	sys_error('Procedure ~q not yet implemented',[X]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Formated message print out.
%
usr_message(Label, String) :-
	format(user_error, '{~w', [Label]),
	format(user_error,String, []),
	format(user_error, '}~n', []).

usr_message(Label, String, ArgList) :-
	format(user_error, '{~w', [Label]),
	format(user_error, String, ArgList),
	format(user_error, '}~n', []).


sys_message(Label, String) :-
	format(user_error, '<<~w', [Label]),
	format(user_error,String, []),
	format(user_error, '>>~n', []).

sys_message(Label, String, ArgList) :-
	format(user_error, '<<~w', [Label]),
	format(user_error, String, ArgList),
	format(user_error, '>>~n', []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 
%
note(N,Msg) :- note(N,Msg,[]).

note(N,Msg,Args) :-
	(verbosity(M) -> (M > N -> format(Msg,Args) ; true) ; true).


notify(String) :- notify(String,[]).

notify(String,Xs) :-
	(verbosity(_) -> format(user_error,String,Xs) ; true).


:- dynamic verbosity/1.

set_verbose :-
	( verbosity(_) ->
	  true
	; assert(verbosity(1))
	).

set_verbosity(N) :-
	( verbosity(_) ->
	  retract(verbosity(_)), assert(verbosity(N))
	; assert(verbosity(N))
	).

set_quiet :-
	( verbosity(_) ->
	  retract(verbosity(_))
	; true
	).
