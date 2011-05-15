%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  expander.pl
%
%
%  Patric Hedlin.....5/12 - 94
%
%
% Description:
%
%   This version of clause expansion will not just expand clauses in order
% to remove syntactic sugar, it will also introduce primitives to represent
% different situations of "cut". These primitives are used by the (reform)
% compiler in order to generate the appropriate code.
%
% The primitives introduced by the expander are the following:
%
%	$choice(X)	representing the (possible) creation of a choice point
%	$cut(X)		representing a deep cut
%	$early_cut	representing a neck cut (in a case construct)
%	$early_cut(X)	representing a shallow cut 
%
%   The arithmetic mode determines whether clauses containing is/2 etc.
% should be tranlated into internal (compiler supported) primitives or not.
% If the arithmetic expansion should be performed, arithmetic mode should be
% set to 'expand_arithmetic' or, if expansion should not be performed,
% 'intact_arithmetic'.
%
%

:- ensure_loaded('../util/vars').
:- ensure_loaded('../util/basics').
:- ensure_loaded('../util/multiset').

:- ensure_loaded('../definitions/primitives').
:- ensure_loaded('../definitions/inlineable').

:- ensure_loaded(arithmetic).


:- dynamic '$name_counter'/1.


'$name_counter'(0).


default_arithmetic_mode(expand_arithmetic).



%%%
%%  expand_clause(+OldClause, -NewClause) --> SpawnedClauses,Handle
%   expand_clause(+OldClause, -NewClause, +Mode) --> SpawnedClauses,Handle
%
expand_clause(Clause, Expand) -->
	{
          default_arithmetic_mode(Mode)
        },
	expand_clause(Clause, Expand, Mode).


expand_clause((Head :- Body), (Head :- NewBody), Mode) -->
	{
	  vars_find(Head, VSL),
          create_base_name(Head, BaseName),
	  pre_expand_body(Body, PreBody, Mode)
	},
	expand_body(PreBody, ExpBody, VSL,[],_VS, 0,_Cs, BaseName),
	{
	  linear_conj(ExpBody, NewBody)
	}.


%%%
%%  expand_body(+Body, -Term, +/+/-VarSet, +/-Count, +BaseName) --> SpawnedClauses,Handle
%
%   Given a Body, produce an expanded Term, using as input; a set of variables
% occuring in the body prior to (i.e. left of) the current (sub) body (we refer
% to these variables as left occurences, thus the short hand VSL) and, a set of
% variables occuring after (i.e. right of, thus VSR) the current (sub) body.
% The synthesized set of variables (thus VSS) represent all variables present
% in the current expression.
%   When expanding an if-then-else construct we have to identify a possible case
% construct in order to generate a flat expansion, i.e. a single spwned predicate.
% This is also the case when expanding multiple disjunctions. 
%   We also have to build the parameter list of the new head in such a way that
% indexing is supported if possible.
%
% The three sets of variables are used as follows:
%
%   o  The VSL contains the variables to the left (prior) of the current expression.
%
%   o  The VSR contains the variables to the right (post-) of the current expression.
%
%   o  The VSS contains the variables in the current expression.
%
%   When creating a new (internal) head used to represent the expanded expression,
% the formal parameter list is built from the set: (VSL \/ VSR) /\ VSP   (where \/
% is the union and, /\ is the intersection).
%
%   There are two (special) cases which need special attention when expanding the
% syntactic sugar:
%
%  1) if-then-else constructs may in fact be case constructs
%
%  2) disjunctions may be multiple
%
% In the above cases we generate a "flat" expansion, i.e. a single predicate that
% contains multiple choices (and possibly cuts).
%
%
% Note (on adding modules): call(X) should have module identification.
%
%
expand_body(X, Term, VSL,VSR,VSS, Ni,Ns,_BaseName) --> { var(X) }, !,
	{
	  Term = call(X),
	  vars_join(VSR, [X|VSL], VSS),
	  Ns = Ni
	}.

expand_body((P,Q), Term, VSL,VSR,VSS, Ni,Ns, BaseName) --> !,
	{
	  vars_find(P, VSl, VSL)
	},
	expand_body(Q, Qx, VSl,VSR,VSQ, Ni,Nx, BaseName),
	{
	  vars_join(VSQ, VSR, VSr)
	},
	expand_body(P, Px, VSL,VSr,VSP, Nx,Ns, BaseName),
	{
	  Term = (Px, Qx),
	  vars_join(VSP, VSQ, VSS)
        }.

expand_body((P->T), Head, VSL,VSR,VSS, Ni,Ns, BaseName) --> !, [Clause],
	{
	  vars_find(P, VSl, VSL)
	},
	expand_body(T, Ti, VSl,VSR,VST, Ni,Nj, BaseName),
	{
	  vars_join(VST, VSR, VSr)
	},
	expand_body(P, Pi, VSL,VSr,VSP, Nj,Nk, BaseName),
	{
	  vars_join(VSP, VST, VSS),

	  parameter_set(VSL,VSR,VSS, PS),

	  internal_head([], PS, Nk,Ns, BaseName, Head),
          internal_body(Pi, Ti, Body),

	  copy_term((Head :- Body), Clause)
	}.

expand_body((P->T;F), Head, VSL,VSR,VSS, Ni,Ns, BaseName) --> !, [Clause],
	{
          indexing_set(P, IS),
	  vars_find((P,T,F), VSi), %%% Naive computation of the parameter set.
	  parameter_set(VSL,VSR,VSi, PS)
	},
	expand_case(F, IS,PS, VSL,VSR,_VSF, Ni,Nj, BaseName, Head),
	{
	  vars_find(P, VSl, VSL)
	},
	expand_body(T, Ti, VSl,VSR,VST, Nj,Nk, BaseName),
	{
	  vars_join(VST, VSR, VSr)
	},
	expand_body(P, Pi, VSL,VSr,_VSP, Nk,Ns, BaseName),
	{
	  VSS = PS,

	  internal_body(Pi, Ti, Body),

	  copy_term((Head :- Body), Clause)
	}.

expand_body((P;Q), Head, VSL,VSR,VSS, Ni,Ns, BaseName) --> !, [Clause],
	{
	  indexing_set(P, IS),
	  vars_find((P;Q), VSi), %%% Naive computation of the parameter set.
	  parameter_set(VSL,VSR,VSi, PS)
	},
	expand_disj(Q, IS,PS, VSL,VSR,_VSQ, Ni,Nj, BaseName, Head),
	expand_body(P, Pi, VSL,VSR,_VSP, Nj,Ns, BaseName),
	{
	  VSS = PS,

	  internal_body(Pi, Body),

	  copy_term((Head :- Body), Clause)
	}.

expand_body(P, Goal, VSL,VSR,VSS, Ni,Ns, BaseName) -->
	{ vars_find(P, VSS) },
	( { meta_primitive(P) } ->
	    ( { explicit_meta_call(P) } ->
		{
	          meta_expand(P, Goal, MetaGoals)
		},
		expand_meta_goal(MetaGoals, VSL,VSR, Ni,Ns, BaseName)
	    ; { true,
		  Goal = P, Ns = Ni }
	    )
	; { true,
	      Goal = P, Ni = Ns }
	).


%%%
%%  expand_case(+Expr, +IndexSet,+ParamSet, +/+/-VarSet, +/-Count,
%                                +BaseName, -Head) --> SpawnedClauses,Handle
%
%   Expanding a possible case construct includes collecting a set of possible
% indexing variables (this set is referred to as the indexing set, thus IS).
% The expansion generates a number of 'internal' bodies (one for each case)
% while collecting the indexing set. The indexing and parameter sets are used
% when building the parameter list to the 'internal' head (i.e. the Head of
% the spawned predicate).
%   The VarSet triple is the same as described above (in expand_body).
%
%
expand_case((P->T), IS,PS, VSL,VSR,VSS, Ni,Ns, BaseName, Head) --> !, [Clause],
	{
	  vars_find(P, VSl, VSL)
	},
	expand_body(T, Ti, VSl,VSR,VST, Ni,Nj, BaseName),
	{
	  vars_join(VST, VSR, VSr)
	},
	expand_body(P, Pi, VSL,VSr,VSP, Nj,Nk, BaseName),
	{
	  vars_join(VSP, VST, VSS),

	  indexing_set(P, IndexSet, IS),

	  internal_head(IndexSet, PS, Nk,Ns, BaseName, Head),
          internal_body(Pi, Ti, Body),

	  copy_term((Head :- Body), Clause)
	}.

expand_case((P->T;F), IS,PS, VSL,VSR,VSS, Ni,Ns, BaseName, Head) --> !, [Clause],
	{
          indexing_set(P, IndexSet, IS)
	},
	expand_case(F, IndexSet,PS, VSL,VSR,VSF, Ni,Nj, BaseName, Head),
	{
	  vars_find(P, VSl, VSL)
	},
	expand_body(T, Ti, VSl,VSR,VST, Nj,Nk, BaseName),
	{
	  vars_join(VST, VSR, VSr)
	},
	expand_body(P, Pi, VSL,VSr,VSP, Nk,Ns, BaseName),
	{
	  vars_join(VSP, VST, VSF, VSS),

	  internal_body(Pi, Ti, Body),

	  copy_term((Head :- Body), Clause)
	}.

expand_case(P, IS,PS, VSL,VSR,VSS, Ni,Ns, BaseName, Head) --> [Clause],
	expand_body(P, Pi, VSL,VSR,VSP, Ni,Nj, BaseName),
	{
	  VSS = VSP,

	  internal_head(IS, PS, Nj,Ns, BaseName, Head),
	  internal_body(Pi, Body),

	  copy_term((Head :- Body), Clause)
	}.


%%%
%%  expand_disj(+Expr, +IndexSet,+ParamSet, +/+/-VarSet, +/-Count,
%                                +BaseName, -Head) --> SpawnedClauses,Handle
%
%   Expanding a possibly multiple disjunction includes collecting a set of possible
% indexing variables (this set is referred to as the indexing set, thus IS).
% This is much the same as above (expand_case), only here we give special treatment 
% to multiple dijunctions instead of conditions.
%
%
expand_disj((P;Q), IS,PS, VSL,VSR,VSS, Ni,Ns, BaseName, Head) --> !, [Clause],
	{
          indexing_set(P, IndexSet, IS)
	},
	expand_disj(Q, IndexSet,PS, VSL,VSR,VSQ, Ni,Nj, BaseName, Head),
	expand_body(P, Pi, VSL,VSR,VSP, Nj,Ns, BaseName),
	{
	  vars_join(VSP, VSQ, VSS),

	  internal_body(Pi, Body),

	  copy_term((Head :- Body), Clause)
	}.

expand_disj(P, IS,PS, VSL,VSR,VSS, Ni,Ns, BaseName, Head) --> [Clause],
	expand_body(P, Pi, VSL,VSR,VSP, Ni,Nj, BaseName),
	{
	  VSS = VSP,

	  internal_head(IS, PS, Nj,Ns, BaseName, Head),
	  internal_body(Pi, Body),

	  copy_term((Head :- Body), Clause)
	}.


%%%
%%  expand_meta_goal(+Goals, +/+VarSet, +/-Count, +BaseName)
%
%   Given a list of meta goal specifications, expand each goal, producing a
% spawned predicate. The specifications has the following format:
%
%       meta_goal(+Goal, -Head, +VarSet)
%
% where +Goal is the goal to be expanded, -Head is a reference to the meta
% predicate where +Goal should be replaced and, +VarSet is a set of variables
% occuring in the meta predicate prior to (i.e. left of) the +Goal.
%
%
expand_meta_goal([],_VSL,_VSR, Nx,Nx,_BaseName) --> [].

expand_meta_goal([meta_goal(Goal,Head,VS)|Rest], VSL,VSR, Ni,Ns, BaseName) --> [Clause],
	{
	  vars_join(VS, VSL, VSl)
	},
	expand_body(Goal, Body, VSl,VSR,VSS, Ni,Nj, BaseName),
	{
	  parameter_set(VSl,VSR,VSS, PS),

	  internal_head([], PS, Nj,Nk, BaseName, Head),

	  copy_term((Head :- Body), Clause)
	},
	expand_meta_goal(Rest, VSL,VSR, Nk,Ns, BaseName).


explicit_meta_call(call(Goal)) :- nonvar(Goal).
explicit_meta_call(freeze(_,Goal)) :- nonvar(Goal).
explicit_meta_call(bagof(_,Goal,_)) :- nonvar(Goal).
explicit_meta_call(setof(_,Goal,_)) :- nonvar(Goal).
explicit_meta_call(findall(_,Goal,_)) :- nonvar(Goal).
explicit_meta_call(on_exception(_,Goal, Handler)) :- nonvar(Goal), nonvar(Handler).


%%%
%%  meta_expand(+MetaPredicate, -MetaPredicate, -MetaGoalSpecification)
%
%
meta_expand(call(Goal), call(Head), [meta_goal(Goal,Head,[])]).

meta_expand(freeze(X,Goal), freeze(X,Head), [meta_goal(Goal,Head,VSL)]):-
	vars_find(X, VSL).

meta_expand(bagof(X,Goal,Y), bagof(X,Head,Y), [meta_goal(Goal,Head,VSL)]) :-
	vars_find(X, VSL).

meta_expand(setof(X,Goal,Y), setof(X,Head,Y), [meta_goal(Goal,Head,VSL)]) :-
	vars_find(X, VSL).

meta_expand(findall(X,Goal,Y), findall(X,Head,Y), [meta_goal(Goal,Head,VSL)]) :-
	vars_find(X, VSL).

meta_expand(on_exception(Pat,Goal,Handler), on_exception(Pat,Head,NewHandler),
  	         [meta_goal(Goal,Head,[]),meta_goal(Handler,NewHandler,VSL)]) :-
	vars_find(Pat, VSL).


%%%
%%  pre_expand_body(+Body, -Term, +Mode) &
%   pre_expand_body(+Body, -Term, ?CutRef, -CutVal, +/-State, +Mode)
%
%   As a first step when expanding a clause we perform a pre expansion phase
% in order to expand or/and introduce; choice point representation, different
% cut primitives and, if requested, inlineable arithmetic.
%   When expanding explicit cut operators, we have to keep track of the level
% to which a cut is in effect. This is accomplished using a reference (CutRef)
% to the fictive choice point ('$choice'(CutRef)). We also use a value (CutVal)
% in order to detect the presence of explicit cut operators in conditions.
%   Depending on what may have happend to the local stack and the trail, we
% should generate different cut primitives to reflect this (this also applies
% to introduced cut primitives in if-then-else constructs). For this reason we
% use a State pair to record the following:
%
%   o  in_call    Represents that we have just invoked a predicate.
%
%   o  pre_call   Represents that we have entered a predicate, but no call has
%                 yet been made.
%
%   o  post_call  Represents that we have entered a predicate and a call has
%                 been made.
%
%   This information is used when we introduce a cut primitive (se create_cut/3
% bellow), both explicit and implicit ones (i.e. when adorning if-then-else
% constructs with cut information).
%
%
pre_expand_body(Body, Term, Mode) :-
	Term = ('$choice'(CutRef), NewBody),
	pre_expand_body(Body, NewBody, CutRef,_CutVal, in_call,_S, Mode).


pre_expand_body(Body, Term,_CutRef,_CutVal, Si,Ss,_Mode) :- var(Body), !,
	Term = Body, Ss = Si.

pre_expand_body((P,Q), Term, CutRef, CutVal, Si,Ss, Mode) :- !,
	pre_expand_body(P, Pi, CutRef, CutP, Si,Sj, Mode),
	pre_expand_body(Q, Qi, CutRef, CutQ, Sj,Ss, Mode),
	Term = (Pi,Qi),
	propagate_cut(CutVal, CutP, CutQ).

pre_expand_body((P->T), Term, CutRef, CutVal,_Si,Ss, Mode) :- !,
	pre_expand_body(P, Pi, CutRef, CutP, in_call,Sj, Mode),
	pre_expand_body(T, Ti, CutRef, CutT, Sj,_Sl, Mode),
	Ss = post_call,
	Term = (Pi->(Primitive,Ti)),
	internal_cut(Sj,_CutInternal, CutP, Primitive),
	propagate_cut(CutVal, CutP, CutT).

pre_expand_body((P->T;F), Term, CutRef, CutVal,_Si,Ss, Mode) :- !,
	pre_expand_body(P, Pi, CutRef, CutP, in_call,Sj, Mode),
	pre_expand_body(T, Ti, CutRef, CutT, Sj,_Sk, Mode),
	pre_expand_body(F, Fi, CutRef, CutF, in_call,_Sl, Mode),
	Ss = post_call,
	Term = (Pi->(Primitive,Ti);Fi),
	internal_cut(Sj,_CutInternal, CutP, Primitive),
	propagate_cut(CutVal, CutP, CutT, CutF).

pre_expand_body((P;Q), Term, CutRef, CutVal,_Si,Ss, Mode) :- !,
	pre_expand_body(P, Pi, CutRef, CutP, in_call,Sj, Mode),
	pre_expand_body(Q, Qi, CutRef, CutQ, Sj,_Sk, Mode),
	Ss = post_call,
	Term = (Pi;Qi),
	propagate_cut(CutVal, CutP, CutQ).

pre_expand_body('!', Primitive, CutRef, CutVal, Sx,Sx,_Mode) :- !,
	CutVal = true,
	create_cut(Sx, CutRef, Primitive).

pre_expand_body(\+(P), Term, CutRef, CutVal, Si,Ss, Mode) :- !,
	pre_expand_body((P->false;true), Term, CutRef, CutVal, Si,Ss, Mode).

pre_expand_body(P, Term,_CutRef,_CutVal,_Si,Ss, Mode) :-
	( arithmetic_goal(P) ->
	    ( Mode == expand_arithmetic ->
		Ss = pre_call,
		expand_arithmetic_goal(P, List, []),
		list_to_conj(List, Term)
	    ; Mode == intact_arithmetic ->
	        Ss = post_call, Term = P
	    ; true,
	        sys_error('pre_expand_body/7: invalid arithmetic mode ~q',[Mode])
	    )
	; inlineable(P) ->
	    Ss = pre_call, Term = P
	; meta_primitive(P) ->
	    Ss = post_call,
	    ( explicit_meta_call(P) ->
		meta_expand(P, Term, MetaGoal),
		pre_expand_meta_goal(MetaGoal, Mode)
	    ; true,
	        Term = P
	    )
	; true,
	    Ss = post_call, Term = P
	).


%%%
%%  pre_expand_meta_goal(+MetaGoal, +Mode)
%
pre_expand_meta_goal([],_Mode).

pre_expand_meta_goal([meta_goal(Goal,Term,_VSL)|Rest], Mode) :-
	pre_expand_body(Goal, Term, Mode),
	pre_expand_meta_goal(Rest, Mode).



%%%
%%  internal_head(+IndexSet, +ParameterSet, +/-Count, +BaseName, -Head)
%
internal_head(IndexSet, ParameterSet, Ni,Ns, BaseName, Head) :-
	parameter_list(IndexSet, ParameterSet, ParameterList),
	internal_name(BaseName, Ni,Ns, Name),
	Head =.. [Name|ParameterList].


internal_name(BaseName, M,N, Name) :-
	N is M + 1,
	number_chars(M, MChar),
	append(BaseName, MChar, CharName),
	atom_chars(Name, CharName).


%%%
%%  internal_body(+Default, -Body) &
%   internal_body(+Condition, +TrueBranch, -Body)
%
internal_body(Pi, Body) :-
	linear_conj(('$choice'(_CutRef), Pi), Body).

internal_body(Pi, Ti, Body) :-
	choice_ref(Ti, CutRef),
	linear_conj(('$choice'(CutRef), Pi, Ti), Body).


%%%
%%  parameter_set(+/+/+VarSet, -ParamSet) &
%   parameter_list(+IndexSet, +ParamSet, -ParamList)
%
parameter_set(VSL,VSR,VSP, ParameterSet) :-
	multiset_union(VSL, VSR, VSU),
	multiset_intersection(VSU, VSP, ParameterSet).


parameter_list(IndexSet, ParameterSet, ParameterList) :-
	indexing_cnt(IndexSet, IndexCnt),
	indexing_arg_list(IndexCnt, ParameterSet, ParameterList).

%%%
%%  indexing_arg_list(+IndexCnt, +ParameterSet, -ParameterList)
%
%   Based on the index count (i.e. a list of possible index variables, sorted
% in order of frequency in which they occur in the gathered conditions) and,
% a (multi) set of variables use by the interface, produce a formal parameter
% list which enables indexing.
%
indexing_arg_list(IndexCnt, ParameterSet, ParameterList) :-
	select(_N-U, IndexCnt),
	delete_from_list(ParameterSet, U, Rest),
	!,
	reduce_from_list([U|Rest], ParameterList).
	
indexing_arg_list(_IndexCnt, ParameterSet, ParameterList) :-
	reduce_from_list(ParameterSet, ParameterList).


%%%
%%  indexing_set(+Condition, -IndexSet) &
%   indexing_set(+Condition, -IndexSet, ?Handle)
%
%   Compute the (local) indexing set in a condition as the variables found in
% the leftmost conjunct.
%
% Note: This fairly naive implementation of the indexing set computation should
%       perhaps be replaced by a more accurate ditto.
%
indexing_set(P, IndexSet) :-
	indexing_set(P, IndexSet, []).


indexing_set((P,_Q), IndexSet, Hdl) :- !,
	vars_find(P, IndexSet, Hdl).

indexing_set(P, IndexSet, Hdl) :-
	vars_find(P, IndexSet, Hdl).


%%%
%%  indexing_cnt(+IndexSet, -IndexCnt)
%
indexing_cnt(IndexSet, IndexCnt) :-
	indexing_cnt(IndexSet, IndexCnt, []).


indexing_cnt([], IndexCnt, IndexRaw) :-
	keysort(IndexRaw, IndexTmp),
	reverse(IndexTmp, IndexCnt).

indexing_cnt([X|Xs], IndexCnt, IndexRaw) :-
	indexing_inc(IndexRaw, X, IndexInc),
	indexing_cnt(Xs, IndexCnt, IndexInc).


indexing_inc([], V, [1-V]).

indexing_inc([M-U|Xs], V, Ys) :- U == V, !,
	N is M + 1,
	Ys = [N-U|Xs].

indexing_inc([X|Xs], V, [X|Ys]) :-
	indexing_inc(Xs, V, Ys).
	

%%%
%%  linear_conj(+Conjunction, -LinearDitto) &
%   linear_conj(+Conjunction, -LinearDitto, ?Handle)
%
linear_conj((P,Q), Conj) :- !,
	linear_conj(Q, Rest),
	linear_conj(P, Conj, Rest).

linear_conj(P,P).


linear_conj((P,Q), Conj, Rest) :- !,
	linear_conj(Q, Next, Rest),
	linear_conj(P, Conj, Next).

linear_conj(P, (P,Rest), Rest).


%%%
%%  create_base_name(+Head, -BaseName) & name_id(-ID)
%
create_base_name(Head, BaseName) :-
	functor(Head, P, N),
	name_id(ID),
	atom_chars(P, PChar),
	number_chars(N, NChar),
	flatten(["$",PChar,"_",NChar,"_",ID,"_"], BaseName).

name_id(ID) :-
	'$name_counter'(C),
	number_chars(C, ID),
	retract('$name_counter'(_)),
	D is C + 1,
	asserta('$name_counter'(D)).


%%%
%%  internal_cut(+State, ?CutRef, ?CutVal, -Primitive)
%
internal_cut(State, CutRef, CutVal, Primitive) :-
	( CutVal == true ->
	    warning('cut in condition to if-then-else')	; true ),
	create_cut(State, CutRef, Primitive).


create_cut(  in_call,_CutRef, '$early_cut').
create_cut( pre_call, CutRef, '$early_cut'(CutRef)).
create_cut(post_call, CutRef,       '$cut'(CutRef)).


choice_ref(('$early_cut',        _Conj),_CutRef).
choice_ref(('$early_cut'(CutRef),_Conj), CutRef).
choice_ref((      '$cut'(CutRef),_Conj), CutRef).


propagate_cut(Cut, Cut, Cut).

propagate_cut(Cut, Cut, Cut, Cut).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Test predicates.
%
%
expand_file(SourceFile, TargetFile) :-
	open(SourceFile, read, Rd),
	open(TargetFile, write, Wr),
	default_arithmetic_mode(Mode),
	( expand_stream(Rd, Mode, Wr) -> true ; true ),
	close(Rd),
	close(Wr).

expand_file_keep_arith(SourceFile, TargetFile) :-
	open(SourceFile, read, Rd),
	open(TargetFile, write, Wr),
	( expand_stream(Rd, intact_arithmetic, Wr) -> true ; true ),
	close(Rd),
	close(Wr).

expand_stream(Rd, Mode, Wr) :-
	read(Rd, X),
	( X == end_of_file ->
	    true
	; X = (:- _) ->
	    expand_stream(Rd, Mode, Wr)
	; expand_term(X, TmpX),
	    ( TmpX = (_ :- _) ->
		NewX = TmpX
	    ; true,
	        NewX = (TmpX :- true)
	    ),
	    expand_clause(NewX, NewCls, Mode, Spawns,[]),
	    portray_clauses([NewCls|Spawns], Wr),
	    expand_stream(Rd, Mode, Wr)
	).


portray_clauses(    [],_Wr).
portray_clauses([X|Xs], Wr) :-
	portray_clause(Wr, X),
	portray_clauses(Xs, Wr).

