%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  vars.pl
%
%
% Description:
%
% 
%

:- ensure_loaded(basics).


%%%
%%  vars(?Term, -VariableList)
%
% Note: kept for compatibility reasons.
%
vars(Term, Vars) :-
	 vars_find(Term, Vars).


%%%
%%  vars_find(?Term, -VariableList) &
%	vars_find(?Term, -VariableList, -Handle)
%
%   Collect (or find) all variables in a term.
%
vars_find(Term, Vars) :-
	 vars_find(Term, Vars, []).


vars_find(Term, Out, Hdl) :- var(Term), !,
	Out = [Term|Hdl].

vars_find(Term, Out, Hdl) :- atomic(Term), !,
	Out = Hdl.

vars_find(Term, Out, Hdl) :-
	Term =.. [_|Args],
	vars_find_list(Args, Out, Hdl).


vars_find_list(   [], Out, Out).

vars_find_list([H|T], Out, Tdl) :-
	vars_find(H, Out, Hdl),
	vars_find_list(T, Hdl, Tdl).


%%%
%%  vars_sort(?Term, -SortedVariableList)
%
vars_sort(Term, Sort) :-
	vars_find(Term, Vars),
	sort(Vars, Sort).


vars_join(SetV, SetU, Join) :-
	append(SetV, SetU, Join).

vars_join(SetV, SetU, SetW, Join) :-
	vars_join(SetV, SetU, Temp),
	vars_join(SetW, Temp, Join).
