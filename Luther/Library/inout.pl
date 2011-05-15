%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  inout.pl
%
%  Patric Hedlin.....Mon Jul 4 1994
%
%
% Description:
%
%
/*
:- module prolog.
*/

:- public
	end_of_file/0,
	current_input/1,
	current_output/1.



end_of_file :- halt.


current_input(Stream) :-
	get_input(Stream).

current_output(Stream) :-
	get_output(Stream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Private predicates.
%

with_default_input(Default, Goal) :-
	get_input(Current),
	set_input(Default),
	( call(Goal) ->
	    set_input(Current) 
	; true,
	    set_input(Current), fail
	).


with_default_output(Default, Goal) :-
	get_output(Current),
	set_output(Default),
	( call(Goal) ->
	    set_output(Current) 
	; true,
	    set_output(Current), fail
	).


