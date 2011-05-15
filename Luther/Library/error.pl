%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  error.pl
%
%  Patric Hedlin.....Tue Jul 5 1994
%
%
% Description:
%
%
/*
:- module prolog.
*/

:- public
/*	error/1,
	error/2,
	warning/1,
	warning/2,
	report/1,
	report/2,
*/	mode_error/4.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  User messages.
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
%%
%  System messages.
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
%%
%  Formated message print out.
%
usr_message(Label, String) :-
	format(user_error, '{~w', [Label]),
	format(user_error, String, []),
	format(user_error, '}~n', []).

usr_message(Label, String, ArgList) :-
	format(user_error, '{~w', [Label]),
	format(user_error, String, ArgList),
	format(user_error, '}~n', []).


sys_message(Label, String) :-
	format(user_error, '<<~w', [Label]),
	format(user_error, String, []),
	format(user_error, '>>~n', []).

sys_message(Label, String, ArgList) :-
	format(user_error, '<<~w', [Label]),
	format(user_error, String, ArgList),
	format(user_error, '>>~n', []).


%%
%   mode_error(+Item, +Mode, +Functor, +Arity)
%
mode_error(Item, Mode, Functor, Arity) :-
	error('illegal mode in ~q/~d, expected ~w, found ~q', [Functor,Arity,Mode,Item]).


%%
%   illegal_arg(+N, +Term)
%
illegal_arg(N, Term) :- N < 1, !,
	sys_error('illegal usage of illegal_arg(_, ~q)', [Term]).

illegal_arg(N, Term) :- N > 0,
	pretty_arg(N, Arg),
	error('illegal ~w argument in ~q', [Arg, Term]).


pretty_arg(1,   first) :- !.
pretty_arg(2,  second) :- !.
pretty_arg(3,   third) :- !.
pretty_arg(4,  fourth) :- !.
pretty_arg(5,   fifth) :- !.
pretty_arg(6,   sixth) :- !.
pretty_arg(7, seventh) :- !.
pretty_arg(8,  eighth) :- !.
pretty_arg(9,  nineth) :- !.

pretty_arg(N, N:th).
