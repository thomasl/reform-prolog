%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  inline.pl - a naive inline preprocessor.
%
%
% Description:
%
%   Inline expansion is performed in a separate (optional) pass through the
% clause database. Inlining is applied to "simple enough" predicates, focusing
% on expanding facts.
%   We assume that clauses in the Clause Database has been preprocessed so
% that no syntactic sugar remain and that all clauses are uniformly stored,
% using the format: (Head :- Body) where a body is a linear conjunction.
%   Since we assume that syntactic sugar has been removed, all disjunctions
% are supposedly represented as multiple clauses, and we may thus test for
% the "simple enough" criteria by looking at the number of clauses in a
% predicate. A single clause is "simple enough" if it does not contain any
% deep cut (neck and shallow cuts are removed).
%
% Note: We do not make any attempt to remove inline predicates from the
%       clause database.
%

inline_clause_database(CDB, NewCDB) :-
	list_dict(CDB, ClauseList),
	empty_dict(EmptyDict),
	find_inlineables(ClauseList, EmptyDict, InlineDict),
	expand_inlineables(ClauseList, InlineDict, EmptyDict, NewCDB).


find_inlineables([], InlineDict, InlineDict).

find_inlineables([(P/N,info(_CompileMode, Clauses,_Directives,_File))|Xs], IDi, IDx) :-
	( single_clause(Clauses, Head, Body), safe_inline(Body, Safe) ->
	    insert(P/N, (Head :- Safe), IDi, IDj)
	; true,
	    IDj = IDi
	),
	find_inlineables(Xs, IDj, IDx).


expand_inlineables([],_ID, CDB, CDB).

expand_inlineables([(P/N,info(CompileMode,Clauses,Directives,File))|Xs], ID, CDBi, CDBx) :-
	inline_expand_clauses(Clauses, ID, Expanded),
	insert(P/N, info(CompileMode,Expanded,Directives,File), CDBi, CDBj),
	expand_inlineables(Xs, ID, CDBj, CDBx).


inline_expand_clauses([],_ID, []).

inline_expand_clauses([Clause|Cs], ID, [Expanded|Es]) :-
	inline_expand_clause(Clause, ID, Expanded),
	inline_expand_clauses(Cs, ID, Es).


inline_expand_clause((Head :- Body), ID, (Head :- NewBody)) :-
	inline_expand_conj(Body, ID, ExpBody),
	linear_conj(ExpBody, NewBody).

inline_expand_conj((Pi,Qi), ID, (Pj,Qj)) :- !,
	inline_expand_goal(Pi, ID, Pj),
	inline_expand_conj(Qi, ID, Qj).

inline_expand_conj(Pi, ID, Pj) :-
	inline_expand_goal(Pi, ID, Pj).


inline_expand_goal(Pi, ID, Pj) :-
	functor(Pi, P, N),
	( lookup(P/N, ID, Clause) ->
	    copy_term(Clause, Copy),
	    inline_expand_copy(Copy, ID, Pi, Pj)
	; true,
	    Pj = Pi
	).


inline_expand_copy((Head :- Body), ID, Goal, InlGoal) :-
	Head =.. [_|HeadList],
	Goal =.. [_|GoalList],
	inline_expand_head_list(HeadList, GoalList, Epilogue, []),
	inline_expand_conj(Body, ID, InlBody),
	list_to_conj(Epilogue, PreBody),
	InlGoal = (PreBody, InlBody).


inline_expand_head_list([], []) --> [].

inline_expand_head_list([X|Xs], [Y|Ys]) --> [X = Y],
	inline_expand_head_list(Xs, Ys).


%%%
%%  safe_inline(+Body, -SafeBody)
%
%   Given the body of an inlineable clause (a single clause predicate),
% produce a inline-safe body suitable for inlining. This process removes
% 'ghost-primitives' introduced by the previous expansion (during loading).
%
safe_inline('$choice'(_),    true) :- !.
safe_inline('$early_cut',    true) :- !.
safe_inline('$early_cut'(_), true) :- !.
safe_inline(      '$cut'(_),_Void) :- !, fail.

safe_inline((Pi,Qi), (Pj,Qj)) :- !,
	safe_inline(Pi, Pj),
	safe_inline(Qi, Qj).

safe_inline(P, P).


single_clause([(Head :- Body)], Head, Body).


%%%
%%  Test predicate.
%
portray_clause_database(CDB) :-
	list_dict_values(CDB, List),
	portray_clause_database_aux(List).

portray_clause_database_aux([]).
portray_clause_database_aux([Info|Rest]) :-
	Info = info(_CompileMode, Clauses,_Directives,_File),
	portray_clauses(Clauses, user_output), nl,
	portray_clause_database_aux(Rest).
