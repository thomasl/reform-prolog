%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  exception.pl - Exception handling in Luther Reform Prolog.
%
%  Johan Bevemyr.....Sun Feb 6 1994
%
%
% Description:
%
%   Set handler sets the internal variable 'luther_on_exception_clause' to the
% next clause of the current choicepoint. This is used by '$find_handler' when
% searching the chain of choice-points to find a choice-point which has its next
% clause set to 'luther_on_exception_clause'. 
%
%
/*
:- module prolog.
*/

:- public
	on_exception/3,
	raise_exception/1.



on_exception(_,ProtectedGoal,_) :-
	call(ProtectedGoal), !.

on_exception(Pattern,_,Handler) :-
	( clause('$$EXCEPTION PATTERN$$'(Trigger),_) ->
	    ( Trigger = Pattern ->
	        !,
		retract('$$EXCEPTION PATTERN$$'(Pattern)),
		call(Handler)
	    ; true,
		'$find_handler'(Choice), 
		'$cut'(Choice), 
		fail
	    )
	; true,
	    !,
	    fail
	).


raise_exception(Pattern) :-
	assert('$$EXCEPTION PATTERN$$'(Pattern)),
	'$find_handler'(Choice),
	'$cut'(Choice),
	fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Private predicates.
%

initialize_exceptions :-
	on_exception(dummy, '$set_handler', fail).
