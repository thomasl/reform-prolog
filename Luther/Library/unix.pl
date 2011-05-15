%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  unix.pl - unix interface
%
%  Johan Bevemyr.....Sat Feb 12 1994
%  Patric Hedlin.....Tue Feb 28 1995
%
%
% Description:   
%
%
/*
:- module prolog.
*/

:- public unix/1.



unix(access(Path,Mode)) :-
	atom(Path),
	atom(Mode),
	!,
	'$unix_access'(Path, Mode).

unix(stat_type(Path,Type)) :- atom(Path), !,
	'$unix_stat_type'(Path, Type).

unix(stat_time(Path,Atime,Mtime,Ctime)) :- atom(Path), !,
	'$unix_stat_time'(Path, Atime, Mtime, Ctime).

unix(cd) :- !,
	'$unix_cd'('$HOME').

unix(cd(Path)) :- atom(Path), !,
	'$unix_cd'(Path).

unix(popen(Command,Mode,Stream)) :-
	atom(Command), !,
	(
	    atom(Mode) ->
	    '$unix_popen'(Command,Mode,Stream)
	;
	    illegal_arg(2, unix(Command,Mode,Stream))
	).

unix(X) :- illegal_arg(1, unix(X)).
