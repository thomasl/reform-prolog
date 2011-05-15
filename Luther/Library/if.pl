%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  if.pl - conditional with backtracking.
%
%  Patric Hedlin.....Mon Dec 19 1994
%
%
% Description:
%
%   This is an implementation of the if/3 predicate found in SICStus Prolog.
% Compared to the ordinary '->' (imlication) there is a slight difference in
% the treatment of the Condition. Using if/3 will allow the Condition to cause
% exhaustive search in the TrueBrach (as opposed to the ordinary '->' that will
% commit to the first solution in the Condition).
%
% Note: The Condition may not contain the cut operator.
%
/*
:- module prolog.
*/

:- public if/3.



if(Condition, TrueBranch, FailBranch) :-
	Guard = if(fail),
	( call(Condition), zaparg(1, Guard, true),
	  call(TrueBranch)
	; Guard = if(fail),
	  call(FailBranch)
	).

	  