%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  listlattice.pl
%
%   Coding the lattice explicitly.
%
%   Lattice elements are coded as numbers and least upper bound and
% unification then encoded as table lookup. There are nineteen possible
% types, in two lattices (the unification lattice and the upper bound
% lattice).
%
% Elements of the domain are:
%
%  bot
%  nil
%  gnd
%  nv(P,S,L)
%  any(P,S,L)
%  free(C,P,L)
%  free_nil(C,P,L)
%  list_g_n
%  list_nv_n(P,S,L)
%  list_a_n(P,S,L)
%  list_f_n(P,S,L)
%  list_g_fn(C,P,L)
%  list_nv_fn(P_hd,S_hd,L_hd,C,P,L)
%  list_a_fn(P_hd,S_hd,L_hd,C,P,L)
%  list_f_fn(P_hd,S_hd,L_hd,C,P,L)
%  list_g_f(C,P,L)
%  list_nv_f(P_hd,S_hd,L_hd,C,P,L)
%  list_a_f(P_hd,S_hd,L_hd,C,P,L)
%  list_f_f(P_hd,S_hd,L_hd,C,P,L)
%
%
% Call patterns are one of:
%
%   det(T1,...,Tn)        Call is determinate (and was so on entering call)
%   nondet(T1,...,Tn)     Call is nondet (and is so 'deeply')
%   shallow(T1,...,Tn)    Call is nondet due to choicept pushed by indexing
%
% Success patterns are one of
%
%   bot                               Call failed
%   shr(Sharing,Instantiation,Tuple)  Call succeeded with result ...
%    where
%    Sharing represents certain aliases as a list of bitvectors;
%    Instantiation is a bitvector (2 bits per arg) telling us what
%      arguments were instantiated during clause;
%    Tuple is a 'call pattern' representing types + det on exit.
%
% An environment is a vector
%   env(OrigDet,CurrDet,Inst,???,T1,...,Tn)
%      where
%      OrigDet is the determinacy prior to the call
%      CurrDet is the determinacy at the current call point
%      Inst is a bitvector (2 bits per entry) of instantiation info
%      ???
%      T1,...,Tn is the type info for variables X1,...,Xn. (They
%      appear as $VAR(4), $VAR(5) and so on.)
%

:- ensure_loaded('../util/sets').
:- ensure_loaded('../util/error').
:- ensure_loaded('../util/basics').
:- ensure_loaded('../definitions/primitives').
:- ensure_loaded(portray).
:- ensure_loaded(absindex).
:- ensure_loaded('recursion-parallel').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%   Initially, each variable is unaliased and local. (Note that the
% unaliasedness is not visible from the variable's POV, but it will not bind
% anyone else when instantiated.)
%
% initial_value(-Init)
%    Init is the value assigned to a variable before the clause has been
%    entered.
%
initial_value(uninit).

initial_environment(Size, Env) :-
	fill_list(Size, List),
	Env =.. [e,det,det|List].


fill_list(0, List) :- !,
	List = [].
fill_list(N, List) :- N > 0,
	initial_value(Item),
	List = [Item|Rest],
	M is N-1,
	fill_list(M, Rest).


fill_list_with(0,_Item, List) :- !,
	List = [].
fill_list_with(N, Item, List) :- N > 0,
	List = [Item|Rest],
	M is N-1,
	fill_list_with(M, Item, Rest).


clauses_and_envs(Goal,CDB,CsEs) :-
	functor(Goal,P,N),
	lookup(P/N,CDB,info(CompileMode,IxStruct,Directives,File)),
	copy_term(IxStruct,IxStructCopy),
	CsEs = info(CompileMode,IxStructCopy,Directives,File).


pre_initial_env(Env,A0) :-
	functor(Env,_,EnvArity),
	first_var_entry_of_env(Var_Entry),
	Size is EnvArity - (Var_Entry-1),
	empty_env(Size,A0).


initial_i_state(((H :- B),E),Cp,H,B,A1) :-
	copy_term(Cp,Pat),
	pre_initial_env(E,A0),
	( Pat = bot ->
	  A0 = A1,
	  set_comp_state(A1,failed) % shouldn't even get here, rilly.
	; Pat = shr(Shr,_IrrelevantInst,Tup) ->
	  install_aliases(Shr,Tup),
	  H =.. [_|Xs],
	  Tup =.. [D|AbsXs],
	  initialize_det(D,A0),
	  empty_arg_inst(Empty),
	  independence_of_args(AbsXs,ArgIndep),
	  initialize_i_state_args(Xs,AbsXs,ArgIndep,1,Empty,Inst,A0,A1),
	  set_arg_inst_state(A1,Inst)
	; Pat =.. [D|AbsXs],
	  H =.. [_|Xs],
	  initialize_det(D,A0),
	  empty_arg_inst(Empty),
	  independence_of_args(AbsXs,ArgIndep),
	  initialize_i_state_args(Xs,AbsXs,ArgIndep,1,Empty,Inst,A0,A1),
	  set_arg_inst_state(A1,Inst)
	).

%%%
%% independence_of_args( , )
%
%   Compute whether a given argument is independent of the other arguments at
% the time of the call. If this is the case, the linearity information can be
% used. Otherwise, it cannot.
%
independence_of_args(Xs,Al) :-
	collect_al(Xs,PA,[]),
	determine_aliasing_sharing(PA,[],Al).

%% Firstly.
%
%  Collect alias variables as a list:
%
%    <alias vars arg 1>, fence, <alias vars arg2>, fence, ...
%
collect_al([]) --> [].
collect_al([X|Xs]) -->
	poss_aliases_of(X),
	[fence],
	collect_al(Xs).

poss_aliases_of(det) --> !, [].
poss_aliases_of(nondet) --> !, [].
poss_aliases_of(X) -->
	( { simple_type(X,_,Attr) } ->
	  poss_al_attr(Attr)
	; { list_type(X,_,Hd_a,_,Tl_a) } ->
	  poss_al_attr(Hd_a),
	  poss_al_attr(Tl_a)
	).

poss_al_attr(none) --> [].
poss_al_attr(a(P,_,_)) --> [P].
poss_al_attr(b(_,P,_)) --> [P].

%% Secondly.
%
%   Determine aliasing sharing by taking the variables previous to a fence,
% and seeing if they also occur beyond it or after it. If so, the argument
% is dependent. Otherwise, it is independent.
%
% Note: We use bitvectors to avoid the quadratic behavior ...
%
determine_aliasing_sharing([],_Prev,Ind) :- !, Ind = [].

determine_aliasing_sharing(AliasList,Prev,IndepList) :-
	take_to_fence(AliasList,BeforeFence,AfterFence),
	independent_of(BeforeFence,AfterFence,Prev,NewPrev,YesNo),
	( YesNo == yes ->
	    IndepList = [indep|RestInd]
	; IndepList = [dep|RestInd]
	),
	determine_aliasing_sharing(AfterFence,NewPrev,RestInd).

take_to_fence([X|Xs],Before,After) :-
	( var(X) ->
	    Before = [X|B],
	    take_to_fence(Xs,B,After)
	; X == fence,
	    Before = [], After = Xs
	).


independent_of(Xs,After,Prev,NewPrev,YesNo) :-
	intersect(Xs,After,Intersect),
	add_to_prev(Intersect,NewPrev,Prev),
	intersect(Xs,Prev,PrevIntersect),
	( ( PrevIntersect = [], Intersect = [] ) ->
	  YesNo = yes
	; YesNo = no
	).

%

add_to_prev([]) --> [].
add_to_prev([X|Xs]) --> [X], add_to_prev(Xs).


%%%
%%  The determinacy in the environment is found by examining the entry
% determinacy. 'det' means computation was and is deterministic, 'shallow'
% that current indexing pushed a chpt but otherwise things are det. 'nondet'
% is nondeterministic outside (and inside hence).
%
initialize_det(det,A) :-
	set_orig_det(A,det),
	set_curr_det(A,det).
initialize_det(shallow,A) :-
	set_orig_det(A,det),
	set_curr_det(A,nondet).
initialize_det(nondet,A) :-
	set_orig_det(A,nondet),
	set_curr_det(A,nondet).

%

initialize_i_state_args([],[],[],_N,Inst,Inst,A,A).

initialize_i_state_args([X|Xs],[AbsX|AbsXs],[IndX|InXs],N,Inst0,Inst2,A0,A2) :-
	initialize_i_state_arg(X,AbsX,IndX,N,Inst0,Inst1,A0,A1),
	M is N+1,
	initialize_i_state_args(Xs,AbsXs,InXs,M,Inst1,Inst2,A1,A2).


%%%
%%  Quite similar to update_i_state_arg/term_1 The sole difference is that each
% argument is checked if the actual parameter is instantiated. This occurs e.g.
% if the parameter is free(...) and the head term matched is a constant, but will
% not show up on exit unfortunately.
%

initialize_i_state_arg(X,AbsX,IndX,N,Inst0,Inst1,A0,A1) :-
	actual_param_is_inst(X,AbsX,N,A0,Inst0,Inst1),
	initialize_i_state_term_1(X,AbsX,IndX,A0,A1).


initialize_i_state_term_1(Term,_AbsX,_IndX,A0,A1) :-
	atomic(Term),!,
	A0 = A1.

initialize_i_state_term_1('$VAR'(N),AbsX,_IndX,A0,A1) :-
	initialize_env(N,AbsX,A0,A1).

initialize_i_state_term_1([X|Xs],AbsX,IndX,A0,A2) :- !,
	( list_type(AbsX,Hd_id,Hd_attr,_,_) ->
	  list_head_subinst(IndX,Hd_id,Hd_attr,AbsT),
	  initialize_i_state_term_1(X,AbsT,IndX,A0,A1),
	  initialize_i_state_term_1(Xs,AbsX,IndX,A1,A2)
	; subinst(AbsX,IndX,SubAbsX),
	  initialize_i_state_term_1(X,SubAbsX,IndX,A0,A1),
	  subinst(AbsX,IndX,SubAbsXs),
	  initialize_i_state_term_1(Xs,SubAbsXs,IndX,A1,A2)
	).

initialize_i_state_term_1(PX,AbsX,IndX,A0,A1) :-
	PX =.. [_|Xs],
	initialize_i_state_subterms_1(Xs,AbsX,IndX,A0,A1).


initialize_i_state_subterms_1([],_AbsX,_IndX,A,A).

initialize_i_state_subterms_1([X|Xs],AbsX,IndX,A0,A2) :-
	subinst(AbsX,IndX,SubAbsX),
	initialize_i_state_term_1(X,SubAbsX,IndX,A0,A1),
	initialize_i_state_subterms_1(Xs,AbsX,IndX,A1,A2).


%%%
%%  The type of the head of the list depends on the following:
%
% o if the list does not share with another argument, then:
%   - if the list sharing is linear, the elt is linear + unaliased
%   - if the list sharing is indlist, elt is nonlinear + unaliased
%   - otherwise, elt is nonlinear + aliased (i.e. a 'boring' subinst)
%
%   The "no sharing with another argument" means the possible alias variable
% appearing can be "split". If there _is_ sharing, the alias cannot be split
% since that would incorrectly lose aliasing information:
%
% Arg1(P) --- Free(P)
%
% split Arg1(P) -> SubArg1(_), instantiate SubArg1(_) -> Free(P) remains 'free',
% which is incorrect (it may have been instantiated, so it must become 'any').
%
%   A list may have 'free' or 'other' elements. If 'free', we must convert the
% attribute from a(_,_,_) to b(_,_,_) since the free type requires that. Hence,
% the explicit cases.
%

list_head_subinst(IndX,Hd_id,Hd_attr,AbsT) :-
	  cnv_lub(Hd_id,Full_Hd_id),
	  ( Hd_attr = a(P,S,L) ; Hd_attr = none ),
	  ( IndX = indep ->
	    ( linear(S) ->
	      ( Full_Hd_id = free ->
		simple_type(AbsT,Full_Hd_id,b(_,_,L))
	      ; make_linear(S0),
		simple_type(AbsT,Full_Hd_id,a(_,S0,L))
	      )
	    ; indlist(S) ->
	      ( Full_Hd_id = free ->
		simple_type(AbsT,Full_Hd_id,b(_,_,L))
	      ; make_nonlinear(S0),
		simple_type(AbsT,Full_Hd_id,a(_,S0,L))
	      )
	    ; nonlinear(S) ->
	      ( Full_Hd_id = free ->
		simple_type(AbsT,Full_Hd_id,b(_,P,L))
	      ; simple_type(AbsT,Full_Hd_id,Hd_attr)
	      )
	    )
	  ; ( Full_Hd_id = free ->
	      simple_type(AbsT,Full_Hd_id,b(_,P,L))
	    ; simple_type(AbsT,Full_Hd_id,Hd_attr)
	    )
	  ).

%

initialize_env(N,AbsX,A0,A1) :-
	A0 = A1,
	arg(N,A0,AbsN),
	norm(AbsX,_,NormX),
	norm(AbsN,_,NormN),
	( AbsN = uninit ->
	  setarg(N,A0,AbsX)
	; absu_t(NormX,NormN,AbsT),
	  norm(AbsN,Inst0,_),
	  norm(AbsT,Inst1,AbsType),
	  NewInst is Inst0 \/ Inst1,
	  update_inst(N,NewInst,A0),
	  setarg(N,A0,AbsType)
	).


%%%
%%  The following predicate checks for whether the actual parameter was
% instantiated by the call. If this is the case, the instantiation is added to
% Inst0, producing Inst1.
%

max_inst(N,Max) :- Max is 1 << (2*N)-1.

actual_param_is_inst(Term,AbsX,ArgN,_Env,Inst0,Inst1) :-
	atomic(Term),!,
	( possibly_variable_type(AbsX) ->
	  arg_is_inst(ArgN,Inst0,Inst1)
	; Inst0 = Inst1
	).

actual_param_is_inst('$VAR'(N),AbsX,ArgN,Env,Inst0,Inst1) :- !,
	lookup_env(N,Env,AbsN),
	( ( possibly_variable_type(AbsX),
	    possibly_bound_type(AbsN) ) ->
	  arg_is_inst(ArgN,Inst0,Inst1)
	; Inst0 = Inst1
	).

actual_param_is_inst(Term,AbsX,ArgN,Env,Inst0,Inst1) :-
	( ground_type(AbsX) ->
	  Inst0 = Inst1
	; possibly_variable_type(AbsX) ->
	  arg_is_inst(ArgN,Inst0,Inst1)
	; simple_bound_type(AbsX,nv,_) ->
	  Term =.. [_|Xs],
	  ( subterms_are_free(Xs,Env) ->
	    Inst0 = Inst1
	  ; arg_is_inst(ArgN,Inst0,Inst1)
	  )
	; Inst0 = Inst1                        % default case
	).

%

possibly_variable_type(AbsX) :-
	norm(AbsX,_,NormAbsX),
	( simple_free_type(NormAbsX,_,_) ->
	  true
	; simple_bound_type(NormAbsX,T_id,_) ->
	  T_id = any
	; list_type(NormAbsX,_,_,Tl_id,_),
	  Tl_id \== n
	).

possibly_bound_type(AbsX) :-
	norm(AbsX,_,NormAbsX),
	( simple_free_type(NormAbsX,T_id,_) ->
	  T_id \== free
	; true
	).

%

arg_is_inst(ArgN,Inst0,Inst1) :-
	Inst1 is Inst0 \/ (2'11 << ((2*(ArgN-1)))).

%

subterms_are_free([],_E).
subterms_are_free(['$VAR'(N)|Xs],E) :-
	lookup_env(N,E,free(_,_,_)),
	subterms_are_free(Xs,E).

%

nonvar_term('$VAR'(_)) :- !, fail.
nonvar_term(_).


%%%
%%  A term is a 'simple nonvar' if it is a structure with only free variables
% as arguments. These variables must not have occured previously in the term.
%
% (The latter requirement is a bit of a pain, really.)
%

simple_nonvar(Term,Env) :-
	\+(atomic(Term)),
	\+(Term = '$VAR'(_)),
	Term =.. [_|Xs],
	free_terms(Xs,[],Env).

free_terms([],_,_).

free_terms(['$VAR'(N)|Xs],Vs,Env) :-
	\+(member(N,Vs)),
	lookup_env(N,Env,Type),
	simple_free_type(Type,free,_),
	free_terms(Xs,[N|Vs],Env).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  This is the counterpart of initial_i_state: final_i_state
%
%   When exiting the call, we compute the success pattern simply by looking at
% the values of the 'argument entries' of the environment.
%

final_i_state(Env,H,Sp) :-
	get_arg_inst_state(Env,ArgInst),
	apply_i_state(Env,H,Sp0),
	( Sp0 = bot ->
	  Sp = bot
	; Sp0 = shr(S,I,T) ->
	  NewI is I \/ ArgInst,
	  Sp = shr(S,NewI,T)
	; aliasing_of_tuple(Sp0,Shr,Sp1),
	  Sp = shr(Shr,ArgInst,Sp1)
	).


apply_i_state(Env,Goal,Pat) :-
	get_comp_state(Env,D0),
	( D0 = failed ->
	  Pat = bot
	; Goal =.. [_|Xs],
	  abstract_and_norm(Xs,Env,1,0,InstArgs,AbsXs),
	  CP0 =.. [D0|AbsXs],
	  Pat = shr(Al,InstArgs,NewCP),
	  aliasing_of_tuple(CP0,Al,NewCP)
	).

%

abstract_and_norm([],_A,_N,Inst,Inst,[]).

abstract_and_norm([X|Xs],A,N,Inst,NewInst,[NormAbsX|AbsXs]) :-
	abstract(X,A,TheInst,AbsX),
	norm(AbsX,NormInst,NormAbsX), 
	ThisInst is TheInst \/ NormInst,
	M is N+1,
	CurrInst is Inst \/ (ThisInst << (2*(N-1))),
	abstract_and_norm(Xs,A,M,CurrInst,NewInst,AbsXs).


update_i_state(H,Pat,A0,A1) :-
	( Pat = bot ->
	  A0 = A1,
	  set_comp_state(A1,failed)
	; Pat = shr(Shr,Inst,Tup) ->
	  install_aliases(Shr,Tup),
	  H =.. [_|Xs],
	  Tup =.. [D|AbsXs],
	  set_curr_det(A0,D),
	  update_i_state_args(Xs,AbsXs,1,Inst,A0,A1)
%	; H =.. [_|Xs],
%	  Pat =.. [D0|AbsXs],
%	  set_comp_state(A0,D0),
%	  update_i_state_args(Xs,AbsXs,A0,A1)
	).


%%%
%%  Each argument has 2 bits telling whether it was instantiated as a result of
% the call. In the case of procedure entry, these bits are zero (is that OK?)
%

update_i_state_args([],[],_N,Inst,A,A) :-
	( Inst =:= 0 ->
	  true
	; notify('% update_i_state_args: inst non-zero after update~n')
        ).

update_i_state_args([X|Xs],[AbsX|AbsXs],N,Inst,A0,A2) :-
	inst_of_arg(N,Inst,CurrInst),
	remove_inst_of_arg(N,Inst,RestInst),
	update_i_state_arg(X,AbsX,CurrInst,A0,A1),
	M is N+1,
	update_i_state_args(Xs,AbsXs,M,RestInst,A1,A2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Worst call & success pattern:
%
%   This one is invoked when we call an undefined predicate. It is supposed to
% return the worst possible approximation of a success pattern. If there are
% shared variables in it, we are justified in noting an unsafe call.
%
% Note: *** UNFINISHED ***
%	Shared vars do not cause unsafeness currently -- or?
%	Does the trick with undefineds work?
%

worst_callpat(Cp,Wp,Anc,U0,U1) :- worst_possible_pattern(Cp,Wp,Anc,U0,U1).

worst_succpat(Cp,Sp,Anc,U0,U1) :- worst_possible_pattern(Cp,Sp,Anc,U0,U1).


worst_possible_pattern(bot,Wp,_Anc,U0,U1) :- !,
	U0 = U1,
	Wp = bot.

worst_possible_pattern(shr(Alias,_Inst,Tuple),Wp,_Anc,U0,U1) :- !,
	U0 = U1,
	Wp = shr(WorstAlias,WorstInst,WorstTuple),
	install_aliases(Alias,Tuple),
	copy_term(Tuple,T0),
	T0 =.. [_Detsm|Xs],
	make_nonlinear(S),
	WorstTerm = any(_P,S,_L),
	worst_possible_aliasing(Xs,WorstTerm),
	T1 =.. [nondet|Xs],
	norm_pat(T1,T2),
	aliasing_of_tuple(T2,WorstAlias,WorstTuple),
	worst_instantiation(WorstTuple,WorstInst).

%

worst_instantiation(SpTuple,Inst) :-
	functor(SpTuple,_,N),
	Inst is (1 << (2*N))-1.

% The worst possible case is that every nonground term _may_ have
% been unified. E.g., that X,Y and Z may have done {X=Y,X=Z,Y=Z} or
% any subset thereof.

worst_possible_aliasing([],_).
worst_possible_aliasing([X|Xs],Worst) :-
	norm(X,_,NormX),
	norm(Worst,_,NormWorst),
	absu(NormX,NormWorst,_),
	worst_possible_aliasing(Xs,NormWorst).

%	absu_t(X,any(P,S,L),_),
%	norm_alias_var(P,S,L,P1),
%	worst_possible_aliasing(Xs,P1,S,L,Unsafe).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  explicit_meta_call : Meta-Predicate -> Functor, Arity
%
%   Check for an explicit call in a meta-predicate and return the corresponding
% functor and arity (if the call is explicit).
%   This is done in order to include meta-predicate information when building
% the call graph from the clause database prior to analysis.
%
explicit_meta_call(call(Goal),P,N) :- nonvariable(Goal,P,N).
explicit_meta_call(freeze(_,Goal),P,N) :- nonvariable(Goal,P,N).
explicit_meta_call(bagof(_,Goal,_),P,N) :- nonvariable(Goal,P,N).
explicit_meta_call(setof(_,Goal,_),P,N) :- nonvariable(Goal,P,N).
explicit_meta_call(findall(_,Goal,_),P,N) :- nonvariable(Goal,P,N).
explicit_meta_call(on_exception(_,Goal,_Handler),P,N) :- nonvariable(Goal,P,N).

nonvariable(Goal,P,N) :-
	is_var(Goal) -> fail ; functor(Goal,P,N).

is_var('$VAR'(N)) :- integer(N).
	   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  analyse_primitive(+Primitive,+AncestorStack,+/-State,+Mode)
%
%
% Note: The treatment of cut is a bit primitive right now. First, binding
%       variables to determinacy info bypasses the standard environment updates.
%       Second, nested cuts will croak and die due to passing 'det' etc. as
%       parameters will confuse LUB, normalization, etc.
%
%
analyse_primitive(G,Anc,A0,A1,_Mode) :-
	analyse_primitive(G,Anc,A0,A1).


analyse_primitive(X,_,_,_) :- var(X), !,
	sys_error('analyse_primitive/4: var arg').

analyse_primitive(true,_Anc,A0,A1) :- !,
	A0 = A1.
analyse_primitive(fail,_Anc,A0,A1) :- !,
	A0 = A1,
	set_comp_state(A0,failed).
analyse_primitive(false,_Anc,A0,A1) :- !,
	A0 = A1,
	set_comp_state(A0,failed).

/*******************************************************************************
analyse_primitive(X=Y,Anc,A0,A1) :- !,
	get_comp_state(A0,D0),
	( D0 == nondet ->
	    nonground_vars(X,A0,Vs0,Ws),
	    nonground_vars(Y,A0,Ws,[]),
	    sort(Vs0,Vs),
	    warning('Unsafe unification ~p with nonground vars ~p',[X=Y,Vs]),
	    show_ancestors(Anc),
	    set_comp_state(A0,unsafe)
	; ( not_instantiated_by(X,Y,A0) ->
	    X_inst = uninst
	; true,
	    X_inst = inst
	),
	( not_instantiated_by(Y,X,A0) ->
	    Y_inst = uninst
	; true,
	    Y_inst = inst
	),
	absunify(X,Y,A0,A1).
*******************************************************************************/

analyse_primitive(X=Y,_Anc,A0,A1) :- !,
	absunify(X,Y,A0,A1).
analyse_primitive('$choice'(X),_Anc,A0,A1) :- !,
	X = '$VAR'(N),
	A0 = A1,
	get_external_comp_state(A0,D0),
	setarg(N,A0,D0).
analyse_primitive('$early_cut',_Anc,A0,A1) :- !,
	A0 = A1,
	get_external_comp_state(A0,D0),
	set_comp_state(A0,D0).
analyse_primitive('$early_cut'(X),_Anc,A0,A1) :- !,
	X = '$VAR'(N), A0 = A1,
	arg(N,A0,D0),
	set_comp_state(A0,D0).
analyse_primitive('$cut'(X),_Anc,A0,A1) :- !,
	X = '$VAR'(N), A0 = A1,
	arg(N,A0,D0),
	set_comp_state(A0,D0).
analyse_primitive(atom(X),_Anc,A0,A1) :- !,
	( atom(X) ->
	    A0 = A1,
	    notify('% useless test: atom(~q)~n',[X])
	; X = '$VAR'(N) ->
	    A0 = A1,
	    lookup_env(N,A0,T),
	    type_test(T,N,ground,A0,A1)
	; true,
	    A0 = A1,
	    set_comp_state(A0,failed)
	).
analyse_primitive(atomic(X),_Anc,A0,A1) :- !,
	( atomic(X) ->
	    A0 = A1,
	    notify('% useless test: atomic(~q)~n',[X])
	; X = '$VAR'(N) ->
	    A0 = A1,
	    lookup_env(N,A0,T),
	    type_test(T,N,ground,A0,A1)
	; true,
	    A0 = A1,
	    set_comp_state(A0,failed)
	).
analyse_primitive(integer(X),_Anc,A0,A1) :- !,
	( integer(X) ->
	    A0 = A1,
	    notify('% useless test: integer(~q)~n',[X])
	; X = '$VAR'(N) ->
	    A0 = A1,
	    lookup_env(N,A0,T),
	    type_test(T,N,ground,A0,A1)
	; true,
	    A0 = A1,
	    set_comp_state(A0,failed),
	    notify('% useless test: integer(~q)~n',[X])
	).
analyse_primitive(float(X),_Anc,A0,A1) :- !,
	( float(X) ->
	    A0 = A1,
	    notify('% useless test: float(~q)~n',[X])
	; X = '$VAR'(N) ->
	    A0 = A1,
	    lookup_env(N,A0,T),
	    type_test(T,N,ground,A0,A1)
	; true,
	    A0 = A1,
	    set_comp_state(A0,failed), 
	    notify('% useless test: float(~q)~n',[X])
	).
analyse_primitive(nonvar(X),_Anc,A0,A1) :- !,
	( X = '$VAR'(N) ->
	    A0 = A1,
	    lookup_env(N,A0,T),
	    type_test(T,N,nonvar,A0,A1)
	; true,
	    A0 = A1,
	    notify('% useless test: nonvar(~q)~n',[X])
	).
analyse_primitive(number(X),_Anc,A0,A1) :- !,
	( number(X) ->
	    A0 = A1,
	    notify('% useless test: number(~q)~n',[X])
	; X = '$VAR'(N) ->
	    lookup_env(N,A0,T),
	    type_test(T,N,ground,A0,A1)
	; true,
	    A0 = A1,
	    set_comp_state(A0,failed),
	    notify('% useless test: number(~q)~n',[X])
	).
analyse_primitive(var(X),_Anc,A0,A1) :- !,
	( X = '$VAR'(N) ->
	    lookup_env(N,A0,T),
	    type_test(T,N,free,A0,A1)
	; true,
	    A0 = A1,
	    set_comp_state(A0,failed),
	    notify('% useless test: var(~q)~n',[X])
	).
analyse_primitive(compare(Dir,_X,_Y),_Anc,A0,A1) :- !,
	A0 = A1,
	abstract(Dir,A0,_Inst,AbsDir),
	absu(AbsDir,gnd,_).
analyse_primitive((Str =.. Lst),_Anc,A0,A1) :- !,
	analyse_univ(Str,Lst,A0,A1).
analyse_primitive('C'(X,Y,Z),Anc,A0,A1) :- !,
	analyse_primitive(X=[Y|Z],Anc,A0,A1).

analyse_primitive(functor(T,F,N),Anc,A0,A1) :- !,
	analyse_functor(T,F,N,Anc,A0,A1).
analyse_primitive(arg(N,T,X),Anc,A0,A1) :- !,
	analyse_arg(N,T,X,Anc,A0,A1).

analyse_primitive((_@<_),_Anc,A0,A1) :- !, A0 = A1.
analyse_primitive((_@>=_),_Anc,A0,A1) :- !, A0 = A1. 
analyse_primitive((_@>_),_Anc,A0,A1) :- !, A0 = A1.
analyse_primitive((_@=<_),_Anc,A0,A1) :- !, A0 = A1.

analyse_primitive((_==_),_Anc,A0,A1) :- !, A0 = A1.
analyse_primitive((_\==_),_Anc,A0,A1) :- !, A0 = A1.

analyse_primitive((X=:=Y),Anc,A0,A1) :- !,
	arith_comparison(X,Y,Anc,A0,A1).
analyse_primitive((X=\=Y),Anc,A0,A1) :- !,
	arith_comparison(X,Y,Anc,A0,A1).
analyse_primitive((X<Y),Anc,A0,A1) :- !,
	arith_comparison(X,Y,Anc,A0,A1).
analyse_primitive((X>Y),Anc,A0,A1) :- !,
	arith_comparison(X,Y,Anc,A0,A1).
analyse_primitive((X=<Y),Anc,A0,A1) :- !,
	arith_comparison(X,Y,Anc,A0,A1).
analyse_primitive((X>=Y),Anc,A0,A1) :- !,
	arith_comparison(X,Y,Anc,A0,A1).

analyse_primitive('$plus'(X,Y,Z),Anc,A0,A1) :- !,
	binary_arith_primop(X,Y,Z,Anc,A0,A1).
analyse_primitive('$plus_1'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$minus'(X,Y,Z),Anc,A0,A1) :- !,
	binary_arith_primop(X,Y,Z,Anc,A0,A1).
analyse_primitive('$minus_1'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$times'(X,Y,Z),Anc,A0,A1) :- !,
	binary_arith_primop(X,Y,Z,Anc,A0,A1).
analyse_primitive('$div'(X,Y,Z),Anc,A0,A1) :- !,
	binary_arith_primop(X,Y,Z,Anc,A0,A1).
analyse_primitive('$intdiv'(X,Y,Z),Anc,A0,A1) :- !,
	binary_arith_primop(X,Y,Z,Anc,A0,A1).
analyse_primitive('$mod'(X,Y,Z),Anc,A0,A1) :- !,
	binary_arith_primop(X,Y,Z,Anc,A0,A1).
analyse_primitive('$abs'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$neg'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$log'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$exp'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$pow'(X,Y,Z),Anc,A0,A1) :- !,
	binary_arith_primop(X,Y,Z,Anc,A0,A1).
analyse_primitive('$min'(X,Y,Z),Anc,A0,A1) :- !,
	binary_arith_primop(X,Y,Z,Anc,A0,A1).
analyse_primitive('$max'(X,Y,Z),Anc,A0,A1) :- !,
	binary_arith_primop(X,Y,Z,Anc,A0,A1).
analyse_primitive('$sqrt'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$cbrt'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).

analyse_primitive('$sin'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$cos'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$tan'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$asin'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$acos'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$atan'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).

analyse_primitive('$tointeger'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$tofloat'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$floor'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$ceiling'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).

analyse_primitive('$b_or'(X,Y,Z),Anc,A0,A1) :- !,
	binary_arith_primop(X,Y,Z,Anc,A0,A1).
analyse_primitive('$b_and'(X,Y,Z),Anc,A0,A1) :- !,
	binary_arith_primop(X,Y,Z,Anc,A0,A1).
analyse_primitive('$b_xor'(X,Y,Z),Anc,A0,A1) :- !,
	binary_arith_primop(X,Y,Z,Anc,A0,A1).
analyse_primitive('$b_not'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).
analyse_primitive('$lshift'(X,Y,Z),Anc,A0,A1) :- !,
	binary_arith_primop(X,Y,Z,Anc,A0,A1).
analyse_primitive('$rshift'(X,Y,Z),Anc,A0,A1) :- !,
	binary_arith_primop(X,Y,Z,Anc,A0,A1).
analyse_primitive('$msb'(X,Y),Anc,A0,A1) :- !,
	unary_arith_primop(X,Y,Anc,A0,A1).


%%%
%%  Dynamic predicates:
%
% We do not track any info on them, and the behavior is as follows:
%
%  (a) assert-style primitives are treated as null-calls: they do not modify
%      their args or anything.
%  (b) retract/1 may retract any info when defined, and so is treated as an
%      undefined call: aliasing etc. is worst possible, nondet, etc.
%  (c) dynamic predicates must be declared. Calling a dynamic predicate is
%      also a "worst case occurence".
%
% Note: Debray has devised a method to trace certain simple instances of
%       dynamic predicates.
%
analyse_primitive(assert(_),_Anc,A0,A1) :- !,
	A0 = A1.
analyse_primitive(asserta(_),_Anc,A0,A1) :- !,
	A0 = A1.
analyse_primitive(assertz(_),_Anc,A0,A1) :- !,
	A0 = A1.

analyse_primitive(retract(G),Anc,A0,A1) :- !,
	apply_i_state(A0,G,Cp),
	empty_dict(Empty),
	worst_succpat(Cp,Sp,Anc,Empty,_),
	update_i_state(G,Sp,A0,A1).

analyse_primitive(recorded(Key,Term,Ref),Anc,A0,A1) :- !,
	G = recorded(Key,Term,Ref),
	apply_i_state(A0,G,Cp),
	empty_dict(Empty),
	worst_succpat(Cp,Sp,Anc,Empty,_),
	update_i_state(G,Sp,A0,A1).

analyse_primitive(recorda(_Key,_Term,_Ref),_Anc,A0,A1) :- !,
	A0 = A1.
analyse_primitive(recordz(_Key,_Term,_Ref),_Anc,A0,A1) :- !,
	A0 = A1.
analyse_primitive(erase(_Ref),_Anc,A0,A1) :- !,
	A0 = A1.

analyse_primitive(read(X),_Anc,A0,A1) :- !,
	abstract(X,A0,_Inst,AbsX),
	make_nonlinear(S), % Nothing done about locality -- OK??
	( simple_bound_type(AbsX,X_id,X_attr) ->
	    instantiate(X_attr,P,S,L)
	; simple_free_type(AbsX,X_id,X_attr) ->
	    bind(X_attr,any(P,S,L))
	; list_type(AbsX,_Hd_id,Hd_attr,_Tl_id,Tl_attr) ->
	    instantiate(Hd_attr,P,S,L),
	    instantiate(Tl_attr,P,S,L)
	),
	norm(AbsX,Inst,NormX),
	update_i_state_term_1(X,NormX,Inst,A0,A1).

analyse_primitive(read(_,X),_Anc,A0,A1) :- !,
	abstract(X,A0,_Inst,AbsX),
	make_nonlinear(S), % Nothing done about locality -- OK??
	( simple_bound_type(AbsX,X_id,X_attr) ->
	    instantiate(X_attr,P,S,L)
	; simple_free_type(AbsX,X_id,X_attr) ->
	    bind(X_attr,any(P,S,L))
	; list_type(AbsX,_Hd_id,Hd_attr,_Tl_id,Tl_attr) ->
	    instantiate(Hd_attr,P,S,L),
	    instantiate(Tl_attr,P,S,L)
	),
	norm(AbsX,Inst,NormX),
	update_i_state_term_1(X,NormX,Inst,A0,A1).

% This primitive should actually derive failure if both args are free
% (or generate an error)

analyse_primitive(length(List,N),_Anc,A0,A1) :- !,
	A0 = A1,
	abstract(List,A0,_,AbsLst),
	abstract(N,A0,_,AbsN),
	absu(AbsLst,list_f_n(_P,_S,_L),_),
	absu(AbsN,gnd,_).

% Note that List and SortedList will be aliased since we
% unify the SAME list with both args!

analyse_primitive(sort(List,SortedList),Anc,A0,A1) :-
	abstract(List,A0,_,AbsLst),
	( simple_free_type(AbsLst,free,_) ->
	    warning('call to sort/2 will generate a runtime error'),
	    show_ancestors(Anc),
	    failed_state(A0,A1)
	; true,
	    A0 = A1,
	    abstract(SortedList,A0,_,AbsSorted),
	    ArgType = list_f_n(_P,_S,_L),
	    absu(AbsLst,ArgType,T),
	    absu(AbsSorted,T,_)
	).

analyse_primitive(keysort(List,SortedList),Anc,A0,A1) :- !,
	abstract(List,A0,_,AbsLst),
	( simple_free_type(AbsLst,free,_) ->
	    warning('call to keysort/2 will generate a runtime error'),
	    show_ancestors(Anc),
	    failed_state(A0,A1)
	; true,
	    A0 = A1,
	    abstract(SortedList,A0,_,AbsSorted),
	    ArgType = list_nv_n(_P,_S,_L),
	    absu(AbsLst,ArgType,T),
	    absu(AbsSorted,T,_)
	).

analyse_primitive(name(X,Y),_Anc,A0,A1) :- !,
	A0 = A1,
	abstract(X,A0,_,AbsX),
	abstract(Y,A0,_,AbsY),
	absu(AbsX,gnd,_),
	absu(AbsY,list_g_n,_).
analyse_primitive(atom_chars(X,Y),_Anc,A0,A1) :- !,
	A0 = A1,
	abstract(X,A0,_,AbsX),
	abstract(Y,A0,_,AbsY),
	absu(AbsX,gnd,_),
	absu(AbsY,list_g_n,_).
analyse_primitive(number_chars(X,Y),_Anc,A0,A1) :- !,
	A0 = A1,
	abstract(X,A0,_,AbsX),
	abstract(Y,A0,_,AbsY),
	absu(AbsX,gnd,_),
	absu(AbsY,list_g_n,_).

analyse_primitive(format(_,_),_,A0,A1) :- !,
	A0 = A1.
analyse_primitive(format(_,_,_),_,A0,A1) :- !,
	A0 = A1.

analyse_primitive(display(_),_,A0,A1) :- !,
	A0 = A1.

analyse_primitive(write(_),_,A0,A1) :- !,
	A0 = A1.
analyse_primitive(write(_,_),_,A0,A1) :- !,
	A0 = A1.
analyse_primitive(writeq(_),_,A0,A1) :- !,
	A0 = A1.
analyse_primitive(writeq(_,_),_,A0,A1) :- !,
	A0 = A1.
analyse_primitive(write_canonical(_),_,A0,A1) :- !,
	A0 = A1.
analyse_primitive(write_canonical(_,_),_,A0,A1) :- !,
	A0 = A1.

analyse_primitive(print(_),_,A0,A1) :- !,
	A0 = A1.
analyse_primitive(print(_,_),_,A0,A1) :- !,
	A0 = A1.

analyse_primitive(nl,_,A0,A1) :- !,
	A0 = A1.
analyse_primitive(nl(_),_,A0,A1) :- !,
	A0 = A1.

analyse_primitive(open(_,_,Stream),_Anc,A0,A1) :- !,
	A0 = A1,
	abstract(Stream,A0,_,AbsStream),
	absu(AbsStream,gnd,_).

analyse_primitive(close(_),_Anc,A0,A1) :- !,
	A0 = A1.

analyse_primitive(statistics,_Anc,A0,A1) :- !,
	A0 = A1.
analyse_primitive(statistics(Query,Ans),_Anc,A0,A1) :- !,
	determinacy_of_env(A0,Det),
	Tuple =.. [Det,gnd,list_g_n],
	max_inst(2,Inst),
	SuccPat = shr([],Inst,Tuple),
	update_i_state(statistics(Query,Ans),SuccPat,A0,A1).

analyse_primitive(absolute_file_name(X,Y),Anc,A0,A1) :- !,
	abstract(X,A0,_,AbsX),
	( simple_bound_type(AbsX,gnd,_) ->
	    A0 = A1,
	    abstract(Y,A0,_,AbsY),
	    absu(AbsY,gnd,_)
	; true,
	    warning('call to absolute_file_name/2 will generate a runtime error'),
	    show_ancestors(Anc),
	    failed_state(A0,A1)
	).

analyse_primitive(raise_exception(_),_Anc,A0,A1) :- !,
	A0 = A1.

analyse_primitive(copy_term(_,_),_Anc,A0,A1) :- !,
	A0 = A1.

analyse_primitive(expand_term(X,Y),Anc,A0,A1) :- !,
	G = expand_term(X,Y),
	apply_i_state(A0,G,Cp),
	empty_dict(Empty),
	worst_succpat(Cp,Sp,Anc,Empty,_),
	update_i_state(G,Sp,A0,A1).


% Reform Prolog new primitives

analyse_primitive(random(X,Y),_Anc,A0,A1) :- !,
	A0 = A1,
	abstract(X,A0,_,AbsX),
	absu(AbsX,gnd,_),
	abstract(Y,A0,_,AbsY),
	absu(AbsY,gnd,_).

% Pseudo-operations introduced by the compiler.

analyse_primitive('$lifted_u'(_Path,X,Y),Anc,A0,A1) :- !,
	analyse_primitive(X=Y,Anc,A0,A1).
analyse_primitive('$det',_Anc,A0,A1) :- !,
	A0 = A1.
analyse_primitive('$nondet',_Anc,A0,A1) :- !,
	A0 = A1.
analyse_primitive('$initially_det',_Anc,A0,A1) :- !,
	A0 = A1.
analyse_primitive('$initially_nondet',_Anc,A0,A1) :- !,
	A0 = A1.
analyse_primitive('$int_rec_begin',_Anc,A0,A1) :- !,
	A0 = A1.
analyse_primitive('$int_rec_end',_Anc,A0,A1) :- !,
	A0 = A1.
analyse_primitive('$get_level'(X),_Anc,A0,A1) :- !,
	A0 = A1,
	abstract(X,A0,_,AbsX),
	absu(AbsX,gnd,_).
analyse_primitive('$set_size'(X),Anc,A0,A1) :- !,
	A0 = A1,
	abstract(X,A0,_,AbsX),
	check_arith_src(AbsX,A0,Anc),
	refine_arith_src(AbsX,A0).
analyse_primitive('$get_size'(X),_Anc,A0,A1) :- !,
	A0 = A1,
	abstract(X,A0,_,AbsX),
	absu(AbsX,gnd,_).

analyse_primitive(X,_Anc,A0,A1) :- !,
	A0 = A1,
	functor(X,P,N),
	warning('Analysing ~q assuming static behaviour',[P/N]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Analysing Meta-Primitives:
%
% o freeze/2
% o call/1
% o bagof/3
% o setof/3
% o findall/3
% o on_exceception/3
%
analyse_meta_primitive(freeze(X,Goal),A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode) :- !,
	abstract(X,A0,_Inst,AbsX),
	( simple_free_type(AbsX,_X_id,b(_C,_P,L)) ->
	    ( local(L) ->
	        true %%% This variable is admissable in any mode (sequential or parallel).
	    ; true,
	        compile_mode(Mode, sequential),
		( analyse_mode(Mode, parallel) ->
		    warning('call to freeze/2 in parallel context suspends on nonlocal variable'),
		    show_ancestors(Anc)
		; true )
	    ),
	    make_freeze(L),
	    absu(AbsX,any(_P,_S,L),_)
	; true
	    %%%warning('call to freeze/2 with nonvariable first argument'),
	    %%%show_ancestors(Anc)
	),
	( is_var(Goal) ->
	    abstract(Goal,A0,_InstGoal,AbsGoal),
	    ( simple_bound_type(AbsGoal,_Goal_id,_Goal_attr) ->
	        make_freeze(L),
		absu(AbsGoal,any(_P,_S,L),_),
		A0 = A1,
		M0 = M1,
		P0 = P1,
		U0 = U1, WL0 = WL1
	    ; true,
	        warning('call to freeze/2 with nonvalid goal will generate a runtime error'),
	        show_ancestors(Anc),
 	        failed_state(A0,A1),
		M0 = M1,
		P0 = P1,
		U0 = U1, WL0 = WL1
	    )
	; true,
	    U0 = U1,
	    empty_dict(Null),
	    apply_i_state(A0,Goal,Cp),
	    worst_callpat(Cp,WCp,Anc,Null,_),
	    callpat_computed(Mode,Goal,WCp,Sp,M0,Mx,P0,Px,Computed,CDB,CG,U0,U1,WL0,WLx),
	    ( Computed == no ->
	        worst_succpat(WCp,WSp,Anc,Null,_)
	    ; Computed == yes ->
	        WSp = Sp
	    ),
	    update_i_state(Goal,WSp,A0,A1),
	    update_succpat(Mode,Goal,WSp,Mx,M1,Px,P1,CG,WLx,WL1)
	).


analyse_meta_primitive(call(Goal),A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode) :- !,
	analyse_meta_call(Goal,A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode,call/1).

analyse_meta_primitive(bagof(_X,Goal,Y),A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode) :- !,
	abstract(Y,A0,_instY,AbsY),
	absu(AbsY,list_a_n(_C,_P,_L),_),
	analyse_meta_call(Goal,A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode,bagof/3).

analyse_meta_primitive(setof(_X,Goal,Y),A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode) :- !,
	abstract(Y,A0,_instY,AbsY),
	absu(AbsY,list_a_n(_C,_P,_L),_),
	analyse_meta_call(Goal,A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode,setof/3).

analyse_meta_primitive(findall(_X,Goal,Y),A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode) :- !,
	abstract(Y,A0,_instY,AbsY),
	absu(AbsY,list_a_n(_C,_P,_L),_),
	analyse_meta_call(Goal,A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode,findall/3).

analyse_meta_primitive(on_exception(Pattern,Goal,Handler),A0,A2,M0,M2,P0,P2,CDB,CG,U0,U2,WL0,WL2,Anc,Mode) :- !,
	PN = on_exception/3,
	abstract(Pattern,A0,_instPattern,AbsPattern),
	absu(AbsPattern,any(_P,_S,_L),_),
	analyse_meta_call(Handler,A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode,PN),
	analyse_meta_call(Goal,   A1,A2,M1,M2,P1,P2,CDB,CG,U1,U2,WL1,WL2,Anc,Mode,PN).


analyse_meta_call(Goal,A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode,PN) :-
	( is_var(Goal) ->
	    abstract(Goal,A0,_InstGoal,AbsGoal),
	    ( simple_bound_type(AbsGoal,_Goal_id,_Goal_attr) ->
	        absu(AbsGoal,any(_P,_S,_L),_),
		A0 = A1,
		M0 = M1,
		P0 = P1,
		U0 = U1, WL0 = WL1
	    ; true,
	        warning('call to ~q with nonvalid goal will generate a runtime error',[PN]),
	        show_ancestors(Anc),
 	        failed_state(A0,A1),
		M0 = M1,
		P0 = P1,
		U0 = U1, WL0 = WL1
	    )
	; true,
	    analyse_goal(Goal,_Sp,A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode)
	).


% Arithmetic comparisons require ground items

arith_comparison(X,Y,Anc,A0,A1) :-
	A0 = A1,
	abstract(X,A0,_InstX,AbsX),
	abstract(Y,A0,_InstY,AbsY),
	check_arith_cmp(AbsX,A0,Anc),
	refine_arith_src(X,A0),
	check_arith_cmp(AbsY,A0,Anc),
	refine_arith_src(Y,A0).

%%%
%%  The compiler has translated operations to a form where the last argument
% is 'zapped' and so must be free. The one or two value-providing arguments
% must be ground (numbers or structures where the leaves are numbers) at the
% time of call.
%

unary_arith_primop(X,Y,Anc,A0,A1) :-
	A0 = A1,
	abstract(X,A0,_InstX,AbsX),
	abstract(Y,A0,_InstY,AbsY),
	check_arith_dest(AbsX,A0,Anc),
	check_arith_src(AbsY,A0,Anc),
	refine_arith_src(Y,A0),
	absu_t(AbsX,gnd,_).

binary_arith_primop(X,Y,Z,Anc,A0,A1) :-
	A0 = A1,
	abstract(X,A0,_InstX,AbsX),
	abstract(Y,A0,_InstY,AbsY),
	abstract(Z,A0,_InstZ,AbsZ),
	check_arith_dest(AbsX,A0,Anc),
	check_arith_src(AbsY,A0,Anc),
	refine_arith_src(Y,A0),
	check_arith_src(AbsZ,A0,Anc),
	refine_arith_src(Z,A0),
	absu_t(AbsX,gnd,_).

%%%
%%  These predicates check that arguments to primitives have the right types.
% Note that arithmetic can take structures as arguments, which means nv(...)
% isn't wrong (the analyser may have been imprecise).
%

check_arith_cmp(AbsX,A0,Anc) :-
	check_arith_src(AbsX,A0,Anc).

check_arith_src(gnd,_,_) :- !.
check_arith_src(nv(_,_,_),_,_) :- !.
check_arith_src(any(_,_,_),_,_) :- !.
check_arith_src(T,A,Anc) :-
	warning('arithmetic arg has invalid type ~p',[T]),
	show_ancestors(Anc),
	set_comp_state(A,failed).

check_arith_dest(free(_,_,_),_,_) :- !.
check_arith_dest(T,_A,Anc) :-
	warning('arithmetic dest may have invalid type ~p',[T]),
	show_ancestors(Anc).

%

refine_arith_src(X,_A0) :- atomic(X),!.
refine_arith_src('$VAR'(N),A0) :- !,
	setarg(N,A0,gnd).                      % wheee!
refine_arith_src(PX,A0) :- !,
	PX =.. [_|Xs],
	refine_arith_src_list(Xs,A0).

refine_arith_src_list([],_A0).
refine_arith_src_list([X|Xs],A0) :-
	refine_arith_src(X,A0),
	refine_arith_src_list(Xs,A0).

%%%
%%  The type_test operation has the following responsibilities:
%
% o   if argument is shared, make it shared-td (otherwise, no change)
%
% o   the abstract type may be
%	incompatible		- computation fails
%	greater than type	- restrict type
%	lesser than type	- no change
%
% There are three kinds of test: ground (i.e. atomic etc), nonvar and free.
%
% Note: *** UNFINISHED ***
%	The 'type restriction' is semi-implemented.
%

type_test(T,N,TypeTest,A0,A1) :-
	timedep_if_shared(T),
	( TypeTest = ground ->
	    A0 = A1,
	    refine_or_fail(T,gnd,N,A0)
	; TypeTest = nonvar ->
	    A0 = A1,
	    refine_or_fail(T,nv(_,_,_),N,A0)   % hmmm ... locality etc?
	; TypeTest = free ->
	    A0 = A1,
	    refine_or_fail(T,free(_,_,_),N,A0) % hmmm ... locality etc?
	).

%

timedep_if_shared(T) :-
	( simple_type(T,_,T_attr) ->
	    locality_of_attr(T_attr,L)
	; list_type(T,_,_,_,Tl_attr) -> % type tests only on 'toplevel'
	    locality_of_attr(Tl_attr,L)
	),
	( shared(L) -> make_wbf(L) ; true ).

%%%
%%  The type test works as:
%
% o  If the current type includes the new type, restrict it.
%
% o  Otherwise, the test will fail (e.g. var(X), X is nonvar) and we thus set
%    the state to failed.
%

refine_or_fail(Type,NewType,N,A0) :-
	( less_than_type(NewType,Type) ->
	    refine_by_type(Type,NewType,N,A0)
	; true,
	    set_comp_state(A0,failed)
	).

%%%
%%  There are mainly three generic term building and destructuring operations:
% univ ("=.."), functor and arg. These are a bit tedious to do right since they
% can be used in several modes (constructing or testing terms).
%
% Note: *** UNFINISHED ***
%	Gives up pretty early.
%

analyse_univ(Struct,List,A0,A1) :- !,
	A1 = A0,
	abstract(Struct,A0,_InstStr,AbsStr),
	abstract(List,A0,_InstLst,AbsLst),!,
	( (ground_type(AbsStr) ; ground_type(AbsLst)) ->
	    absu(AbsStr,gnd,_),
	    absu(AbsLst,list_g_n,_)
	; true,
	    ANY = any(P,S,L),
	    ANY_LST = list_a_n(P,S,L),
	    absu(AbsStr,ANY,_),
	    absu(AbsLst,ANY_LST,_)
	).

analyse_functor(Term,Fn,Ar,_Anc,A0,A1) :-
	A0 = A1,
	abstract(Term,A0,_InstT,AbsTerm),
	abstract(Fn,A0,_InstFn,AbsFn),
	abstract(Ar,A0,_InstAr,AbsAr),
	NV = nv(_,_,_),
	absu(AbsTerm,NV,_),
	absu(AbsFn,gnd,_),
	absu(AbsAr,gnd,_).

analyse_arg(ArgNo,Term,Arg,Anc,A0,A1) :-
	A0 = A1,
	abstract(ArgNo,A0,_,AbsArgNo),
	check_arith_src(AbsArgNo,A0,Anc),
	refine_arith_src(ArgNo,A0),
	abstract(Term,A0,_InstTerm,AbsTerm),
	abstract(Arg,A0,_InstArg,AbsArg),
	( ground_type(AbsTerm) ->
	    absu(AbsArg,gnd,_)
	; true,
	    NV = nv(P,S,L),
	    ANY = any(P,S,L),
	    absu(AbsTerm,NV,_),
	    absu(AbsArg,ANY,_)
	).

%%%
%%  This is used for safeness checking. If there are nonground vars, then
% unification (in a nondet state) is unsafe. Likewise, this may be checked
% for other operations that may bind variables.
%

nonground_vars(T1,_A) --> { atomic(T1), ! }, [].

nonground_vars('$VAR'(N),A) --> !, { lookup_env(N,A,T), \+(ground_type(T)) },
	['$VAR'(N)].

nonground_vars(X,A) --> { X =.. [_|Xs] },
	nonground_vars_of_terms(Xs,A).


nonground_vars_of_terms([],_) --> [].
nonground_vars_of_terms([X|Xs],A) -->
	nonground_vars(X,A),
	nonground_vars_of_terms(Xs,A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Environment updating.
%
%   Commonly, unifications will not bind variables in a term. For instance,
% unifying X = f(...) with X a free variable nonaliased to any variables in
% f(...) will not instantiate f(...). On updating the environment, concrete
% variables in f(...) will turn into any(...), however, (due to sub-instantiation)
% even if this is unnecessary (though correct).
%   By tracking instantiation, this is avoided. We check that the type is free;
% if it also is not aliased to a variable in the term, everything is OK.
%

not_instantiated_by(Term1,Term2,A) :-
	free_term(Term2,A,P),
	unaliased_with(Term1,A,P).

%

free_term('$VAR'(N),A,P) :- !,
	lookup_env(N,A,T),
	free_type(T,P).

free_type(free(_,P,_),P1) :- norm_alias_var(P,_,_,P1).

%

unaliased_with(X,_A,_P) :- atomic(X),!.

unaliased_with('$VAR'(N),A,P) :- !,
	aliasing_vars_of(N,A,P1,P2),
	P1 \== P,
	P2 \== P.

unaliased_with(PX,A,P) :-
	PX =.. [_|Xs],
	unaliased_with_all(Xs,A,P).

%

unaliased_with_all([],_,_).

unaliased_with_all([X|Xs],A,P) :-
	unaliased_with(X,A,P),
	unaliased_with_all(Xs,A,P).

%

aliasing_vars_of(N,A,P1,P2) :-
	lookup_env(N,A,T),
	( simple_bound_type(T,_,T_attr) ->
	    al_var(T_attr,P1)
	; simple_free_type(T,_,T_attr) ->
	    al_var(T_attr,P1)
	; list_type(T,_,Hd_attr,_,Tl_attr) ->
	    al_var(Hd_attr,P1),
	    al_var(Tl_attr,P2)
	).

al_var(none,_).
al_var(a(P,_,_),P).
al_var(b(_,P,_),P).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Term abstraction
%
%   Given a term and an environment, return the abstract element (class) the
% term belongs to.
%

abstract(X,A,AbsX) :- abstract(X,A,_Inst,AbsX).

%%%
%%  Also yield the instantiation of the term.
%
abstract(X,_,_,_) :- var(X),!,
	sys_error('abstract/3: var arg').

abstract([],_A,Inst,AbsX) :- !,
	AbsX = nil,
	Inst = 2'00.

abstract(C,_A,Inst,AbsX) :- atomic(C), !,
	AbsX = gnd,
	Inst = 2'00.

abstract('$VAR'(N),A,Inst,AbsX) :- !,
	lookup_env(N,A,AbsX),
	lookup_inst(N,A,Inst).

abstract([X|Xs],A,Inst,AbsL) :-
	abstract_list([X|Xs],A,0,Inst,AbsL).

abstract(PX,A,Inst,AbsX) :-
	PX =.. [_|Xs],
	abstract_struct(Xs,A,Inst,AbsX).

%%%
%%  Abstract_list must keep track of the following:
% - if all elements are linear, list is linear
% - if elements are nonlinear but independent, list is ind.list.
% - otherwise, list is nonlinear.
%
% For head of list:
% - if list has only ground elements, it is ground (g).
% - if list has only free vars, it is free (f)
% - if list has only nonvars but some nonground elt, it is nonvar (nv)
% - Otherwise, list is any (a).
%
% For tail of list:
% - if list ends in nil, OK (n)
% - if list ends in var, use the value of var in env
% - otherwise, list collapses into any etc., since last elt is invalid.
%
% Also, tell if list elements have been instantiated or not, incl. tail.
%
% Finally, normalize stuff.
%
% Locality for head elts is the worst of the localities; locality of tail
% elt. is inherited appropriately.
%

abstract_list([],_A,Inst,OutInst,AbsL) :- !, 
	AbsL = nil,
	uninst_tail(Inst,OutInst).

abstract_list([X|Xs],A,Inst,OutInst,AbsL) :-
	abstract(X,A,HeadInst,AbsXType),
	attr_of(AbsXType,AbsX,a(P,S,L)),
	cond_inst_head(HeadInst,Inst,NewInst),
	abstract_list(Xs,AbsX,P,S,L,A,NewInst,OutInst,AbsL).

% These predicates test for whether the term _has_been_instantiated_
% in current env!

uninst_tail(Inst,Inst).

cond_inst_head(0,Inst,NewInst) :- !, Inst = NewInst.
cond_inst_head(_,Inst,NewInst) :-
	NewInst is Inst \/ 2'10.

cond_inst_tail(0,Inst,NewInst) :- !, Inst = NewInst.
cond_inst_tail(_,Inst,NewInst) :-
	NewInst is Inst \/ 2'01.

cond_inst(0,Inst,NewInst) :- !, Inst = NewInst.
cond_inst(_,Inst,NewInst) :-
	NewInst is Inst \/ 2'11.

%

abstract_list([],AbsX,P,S,L,_A,Inst,OutInst,AbsL) :- !,
	uninst_tail(Inst,OutInst),
	list_has_head_type(AbsX,P,S,L,Hd_id,Hd_attr),
	list_type(AbsL,Hd_id,Hd_attr,n,none).

abstract_list('$VAR'(N),AbsX,P,S,L,A,Inst,OutInst,AbsL) :- !,
	abstract('$VAR'(N),A,NewInst,AbsY),
	cond_inst_tail(NewInst,Inst,OutInst),
	( AbsY = nil ->
	  list_has_head_type(AbsX,P,S,L,Hd_id,Hd_attr),
	  list_type(AbsL,Hd_id,Hd_attr,n,none)
	; AbsY = free(C1,P1,L1) ->
	  list_has_head_type(AbsX,P,S,L,Hd_id,Hd_attr),
	  list_type(AbsL,Hd_id,Hd_attr,f,b(C1,P1,L1))
	; AbsY = free_nil(C1,P1,L1) ->
	  list_has_head_type(AbsX,P,S,L,Hd_id,Hd_attr),
	  list_type(AbsL,Hd_id,Hd_attr,fn,b(C1,P1,L1))
	; list_type(AbsY,Hd_id,Hd_attr,Tl_id,Tl_attr) ->
	  list_has_head_type(AbsX,P,S,L,X_id,X_attr),
	  list_type(Abs1,X_id,X_attr,Tl_id,Tl_attr),
	  lub_types(Abs1,AbsY,AbsL)
	; abstract_list(f('$VAR'(N)),AbsX,P,S,L,A,Inst,OutInst,AbsL) 
	  % Force collapse into nonvar? (Izzat OK?)
	).

abstract_list([X|Xs],AbsY,P,S,L,A,Inst,OutInst,AbsL) :- !,
	abstract(X,A,NewInst,AbsXType),
	cond_inst_head(NewInst,Inst,CurrInst),
	attr_of(AbsXType,AbsX,a(P1,S1,L1)),
	( P == P1 ->
	  make_nonlinear(S)
	; ( nonlinear(S1) ; indlist(S1) ) ->
	  make_indlist(S)
	; true
	),
	P = P1,
	L = L1,
	lub_type_ids(AbsX,AbsY,AbsZ),
	abstract_list(Xs,AbsZ,P,S,L,A,CurrInst,OutInst,AbsL).

abstract_list(PX,AbsX,P,S,L,A,Inst,OutInst,AbsL) :- % collapse to nonvar
	abstract(PX,A,NewInst,AbsPX),
	cond_inst_tail(NewInst,Inst,OutInst),
	attr_of(AbsPX,AbsY,a(P1,S1,L1)),
	lub_type_ids(AbsX,AbsY,AbsZ),
	( ( AbsZ = nil ; AbsZ = gnd ; AbsZ = list_g_n ) ->
	  AbsL = gnd
	; P == P1 ->
	  make_nonlinear(S),
	  P = P1, L = L1,
	  AbsL = nv(P,S,L)
	; P = P1, L = L1, S = S1, AbsL = nv(P,S,L)
	).

%

list_has_head_type(AbsX,P,S,L,Hd_id,Hd_attr) :-
	( ( AbsX = nil ; AbsX = gnd ; AbsX = list_g_n ) ->
	  Hd_id = g, Hd_attr = none
	; ( AbsX = nv
	  ; AbsX = list_a_n ; AbsX = list_nv_n ; AbsX = list_f_n ) ->
	  Hd_id = nv, Hd_attr = a(P,S,L)
	; ( AbsX = any ; AbsX = free_nil
	  ; AbsX = list_a_fn ; AbsX = list_nv_fn ; 
	      AbsX = list_g_fn ; AbsX = list_f_fn
	  ; AbsX = list_a_fn ; AbsX = list_nv_fn ; 
	      AbsX = list_g_fn ; AbsX = list_f_fn ) ->
	  Hd_id = a, Hd_attr = a(P,S,L)
	; AbsX = free ->
	  Hd_id = f, Hd_attr = a(P,S,L)
	).
%
%
%abstract_all([],_,[]).
%abstract_all([X|Xs],A,[AbsX|AbsXs]) :-
%	abstract(X,A,Inst,AbsX),
%	abstract_all(Xs,A,AbsXs).
%
% abstract_struct performs abstraction on a structure. This is a lesser
% problem than abstracting a list.
%
% - All subterms are abstracted separately. (Conceptually.)
% - If the subterms share, the term is nonlinear and otherwise linear.
% - The locality of the term is the worst of the locality of the subterms.
% - The subterms are then used to `lift' into the abstraction of the
%   entire term: if subterms are ground (or nil), the term is ground and
%   otherwise nonvar.

abstract_struct([],_A,Inst,AbsT) :- !,
	Inst = 0,
	AbsT = gnd.

abstract_struct([X|Xs],A,OutInst,AbsT) :-
	abstract(X,A,Inst,AbsXType),
	attr_of(AbsXType,AbsX,a(P,S,L)),
	abstract_struct(Xs,AbsX,P,S,L,A,Inst,OutInst,AbsT).

%

abstract_struct([],AbsX,P,S,L,_A,Inst,OutInst,AbsT) :-
	Inst = OutInst,
	( ( AbsX = gnd ; AbsX = nil ; AbsX = list_g_n ) ->
	  AbsT = gnd
	; simple_type(AbsT,nv,a(P,S,L))
	).

abstract_struct([X|Xs],AbsY,P,S,L,A,Inst,OutInst,AbsT) :-
	abstract(X,A,NewInst,AbsXType),
	cond_inst(NewInst,Inst,CurrInst),
	attr_of(AbsXType,AbsX,a(P1,S1,L1)),
	( P == P1 ->
	  make_nonlinear(S)
	; S = S1
	),
	L = L1,
	P = P1, % join aliases after testing linearity above
	lub_type_ids(AbsX,AbsY,AbsZ),
	abstract_struct(Xs,AbsZ,P,S,L,A,CurrInst,OutInst,AbsT).

% attr_of collects the attributes of a type into a single
% type-id and a single attribute tuple (only a(P,S,L), since
% that's all we're interested in).

attr_of(Type,Type_id,Attr) :-
	( simple_free_type(Type,Type_id,b(_C,P,L)) ->
	  Attr = a(P,_,L)
	; simple_bound_type(Type,Type_id,At) ->
	  ( At = none -> Attr = a(_,_,_)
	  ; Attr = At
	  )
	; list_type(Type,Hd_id,Hd,Tl_id,Tl) ->
	  list_types(Type_id,Hd_id,Tl_id),
	  ( Hd = none ->
	    ( Tl = none ->
	      Attr = a(_,_,_)
	    ; Tl = b(_C,P,L) ->
	      Attr = a(P,_,L)
	    )
	  ; Hd = a(P1,S1,L1) ->
	    ( Tl = none ->
	      Attr = Hd
	    ; Tl = b(_C,P2,L2) ->
	      P1 = P2,
	      L1 = L2,
	      Attr = a(P1,S1,L1)
	    )
	  )
	).

%

lub_type_ids(X,Y,Z) :-
	t_to_num(X,N1),
	t_to_num(Y,N2),
	num_lub(N1,N2,N),
	num_to_t(N,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Updating the environment.
%
% When descending into a term, collect the 'subinstances' of the type.

update_i_state_arg(X,AbsX,Inst,A0,A1) :-
	update_i_state_term_1(X,AbsX,Inst,A0,A1).

update_i_state_term(Term,AbsX,A0,A1) :-
	norm(AbsX,Inst,NormAbsX),
	update_i_state_term_1(Term,NormAbsX,Inst,A0,A1).


% NOTE: we default to always performing a unification in
%   update_env. Is this good or bad?

update_i_state_term_1(Term,_AbsX,_Inst,A0,A1) :- atomic(Term), !,
	A0 = A1.

update_i_state_term_1('$VAR'(N),AbsX,Inst,A0,A1) :- !,
	update_env(N,AbsX,Inst,A0,A1).

update_i_state_term_1([X|Xs],AbsX,Inst,A0,A2) :- !,
	( list_type(AbsX,Hd_id,Hd_attr,_,_) ->
	  cnv_lub(Hd_id,Full_Hd_id),
	  simple_type(AbsT,Full_Hd_id,Hd_attr),
	  !,
	  update_i_state_term_1(X,AbsT,Inst,A0,A1),
	  update_i_state_term_1(Xs,AbsX,Inst,A1,A2)
	; subinst(AbsX,SubAbsX),
	  update_i_state_term_1(X,SubAbsX,Inst,A0,A1),
	  subinst(AbsX,SubAbsXs),
	  update_i_state_term_1(Xs,SubAbsXs,Inst,A1,A2)
	).

update_i_state_term_1(PX,AbsX,Inst,A0,A1) :-
	PX =.. [_|Xs],
	update_i_state_subterms_1(Xs,AbsX,Inst,A0,A1).

%

update_i_state_subterms_1([],_AbsX,_Inst,A,A).

update_i_state_subterms_1([X|Xs],AbsX,Inst,A0,A2) :-
	subinst(AbsX,SubAbsX),
	update_i_state_term_1(X,SubAbsX,Inst,A0,A1),
	update_i_state_subterms_1(Xs,AbsX,Inst,A1,A2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% subinst/2 is used to abstract terms and update envs

subinst(X,_) :- var(X),!,sys_error('subinst/2: var arg').
subinst(bot,X) :- !, X = bot.
subinst(nil,X) :- !, X = nil.
subinst(gnd,X) :- !, X = gnd.
subinst(nv(P,S,L),X) :- !, X = any(P,S,L).
subinst(any(P,S,L),X) :- !, X = any(P,S,L).
subinst(free(_,P,L),X) :- !, X = free(_,P,L).
subinst(free_nil(_,P,L),X) :- !, X = free_nil(_,P,L).
subinst(X,any(P,S,L)) :-
	attr_of(X,_,a(P,S,L)).

% subinst/3 is used to initialize environment.
%
% Here, we can use independence and linearity information to get
% better precision. We reason as follows:
% - if the term is linear (lists do not appear here) and
% - if the term is independent of all other arguments ("truly linear occ.")
% Then the subterms are actually unaliased: they do not share with
% other subterms, and they do not share with other arguments.

subinst(X,_,_) :- var(X),!,sys_error('subinst/3: var arg').
subinst(bot,_,X) :- !, X = bot.
subinst(nil,_,X) :- !, X = nil.
subinst(gnd,_,X) :- !, X = gnd.
subinst(nv(P,S,L),IndT,X) :- !,
	( ( IndT = indep, linear(S) ) ->
	  make_linear(S0),
	  X = any(_,S0,L)
	; X = any(P,S,L)
	).
subinst(any(P,S,L),IndX,X) :- !,
	( ( IndX = indep, linear(S) ) ->
	  make_linear(S0),
	  X = any(_,S0,L)
	; X = any(P,S,L)
	).
subinst(free(_,P,L),IndX,X) :- !,
	( IndX = indep ->
	  X = free(_,_,L)
	; X = free(_,P,L)
	).
subinst(free_nil(_,P,L),IndX,X) :- !,
	( IndX = indep ->
	  X = free_nil(_,_,L)
	; X = free_nil(_,P,L)
	).
subinst(X,IndX,SubX) :-
	attr_of(X,_,a(P,S,L)),
	( ( IndX = indep, linear(S) ) ->
	  make_linear(S0),
	  SubX = any(_,S0,L)
	; SubX = any(P,S,L)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Environment handling:
%
% An environment is a tuple e(OrigD,CurrD,Var1,...,VarN)
%
% So variable K in the clause is written as '$VAR'(K+2). (I.e. the
% clause grounding must be 'enumeratevars(Clause,3,N)'.
%
% OrigD is the determinacy before entering the clause
% CurrD is the current determinacy of the clause

lookup_env(N,A,NormAbsX) :-
	arg(N,A,AbsX),
	norm(AbsX,Inst,NormAbsX),
	update_inst(N,Inst,A),
	setarg(N,A,NormAbsX).

lookup_inst(N,A,InstA) :-
	get_inst_state(A,Inst),
	first_var_entry_of_env(M),
	InstA is (Inst >> (2*(N-M))) /\ 2'11.

update_env(N,A,Inst,A0,A1) :-
	A0 = A1,
	norm(A,_OtherInst,NormA),
	arg(N,A0,PrevA),
	norm(PrevA,_OtherPrevInst,NormPrevA),
	update_by_type(NormPrevA,NormA,Inst,N,A0).

% replace update_by_type-line w. this to get instantiation-usage
%	( Inst =:= 0 ->
%	  refine_by_type(NormPrevA,NormA,N,A0)
%	; update_by_type(NormPrevA,NormA,Inst,N,A0)
%	).

update_env(N,A,A0,A1) :-
	A0 = A1,
	norm(A,NewInst,NormA),
	arg(N,A0,PrevA),
	norm(PrevA,OldInst,NormPrevA),
	Inst is NewInst \/ OldInst,
	update_by_type(NormPrevA,NormA,Inst,N,A0).

% replace update_by_type - line with this for instantiation usage:
%	( Inst =:= 0 ->
%	  refine_by_type(NormPrevA,NormA,N,A0)
%	; update_by_type(NormPrevA,NormA,Inst,N,A0)
%	).

update_inst(N,InstA,A) :-
	get_inst_state(A,Inst),
	first_var_entry_of_env(M),
	NewInst is Inst \/ (InstA << (2*(N-M))),
	set_inst_state(A,NewInst).

%%%%%%%%%%
% After a call has completed, we construct a bit vector of
% entries telling what arguments were instantiated and what were not.
%  Each entry gets 2 bits, one for head & one for tail (if a list).
%
% The use of this instantiation bit vector is localized to the
% procedures below.

empty_arg_inst(0).

add_inst_arg(N,Inst,ArgInst,NewArgInst) :-
	NewArgInst is ArgInst \/ (Inst << (2*(N-1))).

inst_of_arg(N,ArgInst,Inst) :-
	Inst is (ArgInst >> (2*(N-1))) /\ 2'11.

remove_inst_of_arg(N,OldInst,NewInst) :-
	inst_of_arg(N,OldInst,TheInst),
	NewInst is OldInst - (TheInst << (2*(N-1))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Refinement is done by:
% (a) Update the entry destructively
% (b) Join aliases

refine_by_type(Prev,Curr,N,A) :-
	set_to_least(N,A,Prev,Curr),
	( list_type(Curr,_Hd1_id,Hd1_attr,_Tl1_id,Tl1_attr) ->
	  ( list_type(Prev,Hd2_id,Hd2_attr,Tl2_id,Tl2_attr) ->
	    join_attr(Hd1_attr,Hd2_attr),
	    join_attr(Tl1_attr,Tl2_attr)
	  ; simple_type(Prev,Prev_id,Prev_attr) ->
	    join_attr(Hd1_attr,Prev_attr),
	    join_attr(Tl1_attr,Prev_attr)
	  )
	; simple_type(Curr,_Curr_id,Curr_attr) ->
	  ( list_type(Prev,Hd2_id,Hd2_attr,Tl2_id,Tl2_attr) ->
	    join_attr(Curr_attr,Hd2_attr),
	    join_attr(Curr_attr,Tl2_attr)
	  ; simple_type(Prev,Prev_id,Prev_attr) ->
	    join_attr(Curr_attr,Prev_attr)
	  )
	).

% Set_to_least sets the position to the lesser of the two types.

set_to_least(N,A,T1,T2) :-
	( less_than_type(T1,T2) ->
	  setarg(N,A,T1)
	; less_than_type(T2,T1) ->
	  setarg(N,A,T2)
	; sys_error('set_to_least: uncomparable types ~q and ~q',[T1,T2])
	).

% join_attr joins the certain/possible/linearity/locality info
% of two attribute tuples. This occurs to propagate aliases & stuff
% nicely.
%
% NB. Assumes normalized types!

join_attr(none,_).
join_attr(a(P,S,L),Y) :-
	( Y = none -> true
	; Y = a(P1,S1,L1) ->
	  P = P1, S = S1, L = L1
	; Y = b(_C1,P1,L1) ->
	  P = P1, L = L1
	).
join_attr(b(C,P,L),Y) :-
	( Y = none -> true
	; Y = a(P1,_S1,L1) ->
	  P = P1, L = L1
	; Y = b(C1,P1,L1) ->
	  C = C1, P = P1, L = L1
	).

% Updating is done by:
% (a) Unification with the appropriate old entry
% (b) Updating the 'instantiation component' appropriately.
%
% NOW: Just unify them ... Instantiation is not used anyway.

%update_by_type(PrevA,CurrA,_Inst,N,A) :- !,
%	absu_t(PrevA,CurrA,NewA),
%	norm(NewA,_,NormNewA),
%	setarg(N,A,NormNewA).

% Never reached:
update_by_type(PrevA,CurrA,Inst,N,A) :- !,
	( list_type(CurrA,Hd_id,Hd_attr,Tl_id,Tl_attr) ->
	  ( Inst = 2'00 -> % no instantiation so do nothing
	    NewA = PrevA
	  ; Inst = 2'11 ->
	    absu_t(PrevA,CurrA,NewA)
          ; Inst = 2'01 ->
	    ( list_type(PrevA,_,_,Tl2_id,Tl2_attr) ->
	      cnv_lub(Tl_id,T1_id),
	      simple_type(T1,T1_id,Tl_attr),
	      cnv_lub(Tl2_id,T2_id),
	      simple_type(T2,T2_id,Tl2_attr),
	      absu_t(T1,T2,NewT),
	      simple_type(NewT,T_id,T_attr),
	      lub_cnv(T_id,NewTl_id),
	      list_type(NewA,Hd_id,Hd_attr,NewTl_id,T_attr)
	    ; absu_t(PrevA,CurrA,NewA)
	    )
          ; Inst = 2'10 ->
	    ( list_type(PrevA,Hd2_id,Hd2_attr,_,_) ->
	      cnv_lub(Hd_id,T1_id),
	      simple_type(T1,T1_id,Hd_attr),
	      cnv_lub(Hd2_id,T2_id),
	      simple_type(T2,T2_id,Hd2_attr),
	      absu_t(T1,T2,NewT),
	      simple_type(NewT,T_id,T_attr),
	      lub_cnv(T_id,NewHd_id),
	      list_type(NewA,NewHd_id,Hd_attr,Tl_id,Tl_attr)
	    ; absu_t(PrevA,CurrA,NewA)
	    )
          )
        ; absu_t(PrevA,CurrA,NewA)
        ),
	arg_has_been_instantiated(N,A),
	norm(NewA,_Inst,NormNewA),
	setarg(N,A,NormNewA).

%

arg_has_been_instantiated(N,A) :-
	get_inst_state(A,Inst),
	first_var_entry_of_env(M),
	NewInst is Inst \/ (2'11 << (2*(N-M))),
	set_inst_state(A,NewInst).

% The possible states of a computation are:
%   det         Computation is determinate (since entering par.code)
%   nondet      Computation is nondeterminate (since entering par.code)
%   failed      Computation has failed
%   unsafe      Computation has made an unsafe operation

set_comp_state(A0,X) :-
	valid_comp_state(X),
	setarg(3,A0,X).

get_comp_state(A0,X) :-
	arg(3,A0,X),
	valid_comp_state(X).

get_external_comp_state(A0,X) :-
	arg(2,A0,X),
	valid_comp_state(X).

set_inst_state(A0,X) :-
	integer(X),
	setarg(4,A0,X).

get_inst_state(A0,X) :-
	arg(4,A0,X),
	integer(X).

set_arg_inst_state(A0,X) :-
	integer(X),
	setarg(5,A0,X).

get_arg_inst_state(A0,X) :-
	arg(5,A0,X),
	integer(X).

%

all_args_instantiated(N,InstVec) :-
	InstVec is 1 << (2*N)-1.

%

set_curr_det(A,D) :- set_comp_state(A,D).

set_orig_det(A,D) :- valid_comp_state(D), setarg(2,A,D).

set_det(A,D1,D2) :-
	set_orig_det(A,D1),
	set_curr_det(A,D2).

%

determinacy_of_env(A0,Det) :-
	get_comp_state(A0,Det),
	( Det == det ->
	    true
	; Det == nondet ->
	    true
	).

%

valid_comp_state(X) :- var(X),!,fail.
valid_comp_state(det) :- !.
valid_comp_state(nondet) :- !.
valid_comp_state(failed) :- !.
valid_comp_state(unsafe) :- !.
valid_comp_state(X) :-
	sys_error('Nonvalid computational state (set or get): ~q',[X]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Least upper bound
% (All results are enumerated by test_lub.)
%

lub_tuples(X,_,_) :- var(X),!,sys_error('lub_tuples/3: var arg').

lub_tuples(bot,X,X) :- !.

lub_tuples(shr(S1,In1,T1),X,LUB) :- !,
	( X = bot ->
	  LUB = shr(S1,In1,T1)
	; X = shr(S2,In2,T2) ->
	  lub_shr(S1,S2,In1,In2,T1,T2,S,I,T),
	  LUB = shr(S,I,T)
%	; aliasing_of_tuple(X,S2,T2),
%	  lub_shr(S1,S2,T1,T2,S,T),
%	  LUB = shr(S,T)
	; ( X = det ; X = nondet ) ->
	  lub_det(T1,X,NewDet),
	  LUB = shr(S1,In1,NewDet)
	).

%lub_tuples(X,Y,LUB) :-
%	aliasing_of_tuple(X,S1,T1),
%	lub_tuples(shr(S1,T1),Y,LUB).

%%%%%%%%%%%
% MaxAliasing represent 'certainly aliased to every other arg'
% and is used to get 'nil' and closed lists to LUB properly (otherwise,
% certain aliases are lost when going from free to free+nil).

aliasing_of_tuple(PX,Aliases,T) :-
	PX =.. [P|Xs],
	get_cert_aliases(Xs,1,[],Al0,Ys),
	map_snd(Al0,Al1),
	nil_and_closed_list_aliases(Ys,1,Al1,Al2),
	sort(Al2,Aliases),
	T =.. [P|Ys].

%

map_snd([],[]).

map_snd([(_,N)|Xs],[N|Ys]) :- map_snd(Xs,Ys).

%

get_cert_aliases([],_N,Al,Al,[]).

get_cert_aliases([X|Xs],N,Al0,Al2,[Y|Ys]) :-
	( has_cert_alias(X,C,Y) ->
	  insert_alias(Al0,C,N,Al1)
	; Al0 = Al1, Y = X
	),
	M is N+1,
	get_cert_aliases(Xs,M,Al1,Al2,Ys).

%

has_cert_alias(Type,Cert,NewType) :-
	( simple_free_type(Type,T,b(Cert,P,L)) ->
	  ( var(Cert) ->
	    simple_free_type(NewType,T,b(_,P,L))
	  ; sys_error('has_cert_alias/3: cert alias non-normalized')
          )
	; list_type(Type,H,H_al,T,b(Cert,P,L)) ->
	  ( var(Cert) ->
	    list_type(NewType,H,H_al,T,b(_,P,L))
	  ; sys_error('has_cert_alias/3: cert alias non-normalized')
          )
	).

%

insert_alias([],C,N,[(C,Pos)]) :- Pos is 1 << N.

insert_alias([(C1,N1)|Xs],C0,N0,Ys) :-
	( C1 == C0 ->
	  N2 is N1 + 1 << N0,
	  Ys = [(C1,N2)|Xs]
	; Ys = [(C1,N1)|Zs],
	  insert_alias(Xs,C0,N0,Zs)
	).

%

nil_and_closed_list_aliases([],_N,Al,Al).

nil_and_closed_list_aliases([X|Xs],N,Al0,Al2) :-
	( X = nil ->
	  Pos is 1 << N,
	  all_aliased_to(Al0,Pos,Al1)
	; closed_list(X) ->
	  Pos is 1 << N,
	  all_aliased_to(Al0,Pos,Al1)
	; Al0 = Al1
	),
	M is N+1,
	nil_and_closed_list_aliases(Xs,M,Al1,Al2).

%

closed_list(T) :- list_type(T,_,_,n,_).

% Alias every entry to Pos (i.e. add it to the entries) and
% finally add Pos by itself to the list.
%
% The latter ensures that p(free,free) $lub p(nil,nil) will be OK.

all_aliased_to([],Pos,[Pos]).
all_aliased_to([X|Xs],Pos,[Y|Ys]) :-
	Y is X+Pos,
	all_aliased_to(Xs,Pos,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

install_aliases([],_T).

install_aliases([X|Xs],T) :-
	install_positions(X,_,T),
	install_aliases(Xs,T).

%

install_positions(0,_,_) :- !.

install_positions(N,V,T) :-
	Pos is msb(N),
	NewN is N - (1 << Pos),
	arg(Pos,T,Type),
	install_cert_alias(Type,V),
	install_positions(NewN,V,T).

% Modified has_cert_alias/3:
% Install a certain alias if the alias is unbound & exists.
% This one may be used on non-cert-aliased types and so must
% cope with the absence of cert vars.
%
% *** UNFINISHED ***
% I think free(free_nil(...),...) aliasing will be lost. This
% is correct but nonoptimal.

install_cert_alias(Type,Cert) :-
	( simple_free_type(Type,_,b(Cert1,P,L)) ->
	  ( var(Cert1) ->
	    Cert = Cert1
	  ; true
          )
	; list_type(Type,_,_,_,b(Cert1,P,L)) ->
	  ( var(Cert1) ->
	    Cert = Cert1
	  ; true
          )
	; true
	).

%

install_aliasing_tuple(shr(S,_I,T),T) :- !,
	install_aliases(S,T).

install_aliasing_tuple(T,T).

%%%%%%%%%%%

lub_shr(S1,S2,In1,In2,T1,T2,S,I,T) :-
	lub_cert_aliases(S1,S2,[],S),
	lub_instantiation(In1,In2,I),
	lub_type_tuples(T1,T2,T).

% Instantiation is simply 'worst of'.

lub_instantiation(In1,In2,In) :-
	In is In1 \/ In2.

%

lub_cert_aliases([],_S2,S,S).

lub_cert_aliases([X|Xs],S2,CurrS,NewS) :-
	lub_alias_set(S2,X,CurrS,TmpS),
	lub_cert_aliases(Xs,S2,TmpS,NewS).

%

lub_alias_set([],_,S,S).

lub_alias_set([X|Xs],Y,S0,S2) :-
	N is X /\ Y,
	( N =:= 0 ->
	  S0 = S1
	; S1 = [N|S0]
	),
	lub_alias_set(Xs,Y,S1,S2).

%

lub_type_tuples(T1,T2,T) :-
	T1 =.. [D0|Xs],
	T2 =.. [D1|Ys],
	lub_det(D0,D1,D),
	lub_type_list(Xs,Ys,Zs),
	T =.. [D|Zs].

%

lub_type_list([],[],[]).

lub_type_list([X|Xs],[Y|Ys],[Z|Zs]) :-
	lub_types(X,Y,Z),
	lub_type_list(Xs,Ys,Zs).

%%%%%%%%%%

lub_det(det,det,det) :- !.
lub_det(_,_,nondet).

%%%%%%%%%%%

lub_types(T1,T2,T) :-
	( simple_free_type(T1,T1_id,T1_attr) ->
	  t_to_num(T1_id,T1_num),
	  ( simple_free_type(T2,T2_id,T2_attr) ->
	    t_to_num(T2_id,T2_num),
	    num_lub(T1_num,T2_num,T_num),
	    num_to_t(T_num,T_id),
	    lub_attr(T1_attr,T2_attr,T_attr),
	    simple_type(T,T_id,T_attr)
	  ; simple_bound_type(T2,T2_id,T2_attr) ->
	    t_to_num(T2_id,T2_num),
	    num_lub(T1_num,T2_num,T_num),
	    num_to_t(T_num,T_id),
	    lub_attr(T1_attr,T2_attr,T_attr),
	    simple_type(T,T_id,T_attr)
	  ; list_type(T2,Hd2_id,Hd2_attr,Tl2_id,Tl2_attr) ->
	    list_types(T2_id,Hd2_id,Tl2_id),
	    t_to_num(T2_id,T2_num),
	    num_lub(T1_num,T2_num,T_num),
	    num_to_t(T_num,T_id),
	    lub_attr(T1_attr,Tl2_attr,T_attr), % since unbound only tail
	    ( list_types(T_id,_,_) ->
	      the_type(T,T_id,[Hd2_attr,T_attr])
	    ; simple_type(T,T_id,T_attr)
	    )
	  )
	; simple_bound_type(T1,T1_id,T1_attr) ->
	  t_to_num(T1_id,T1_num),
	  ( simple_free_type(T2,T2_id,T2_attr) ->
	    t_to_num(T2_id,T2_num),
	    num_lub(T1_num,T2_num,T_num),
	    num_to_t(T_num,T_id),
	    lub_attr(T1_attr,T2_attr,T_attr),
	    simple_type(T,T_id,T_attr)
	  ; simple_bound_type(T2,T2_id,T2_attr) ->
	    t_to_num(T2_id,T2_num),
	    num_lub(T1_num,T2_num,T_num),
	    num_to_t(T_num,T_id),
	    lub_attr(T1_attr,T2_attr,T_attr),
	    simple_type(T,T_id,T_attr)
	  ; list_type(T2,Hd2_id,Hd2_attr,Tl2_id,Tl2_attr) ->
	    list_types(T2_id,Hd2_id,Tl2_id),
	    t_to_num(T2_id,T2_num),
	    num_lub(T1_num,T2_num,T_num),
	    num_to_t(T_num,T_id),
	    lub_attr(T1_attr,Tl2_attr,T_tl_attr),
	    lub_attr(T1_attr,Hd2_attr,T_hd_attr),
	    ( list_types(T_id,_,_) ->
	      the_type(T,T_id,[T_hd_attr,T_tl_attr])
	    ; simple_type(T,T_id,T_tl_attr)
	    )
	  )
	; list_type(T1,Hd1_id,Hd1_attr,Tl1_id,Tl1_attr) ->
	  ( simple_free_type(T2,T2_id,T2_attr) ->
	    list_types(T1_id,Hd1_id,Tl1_id),
	    t_to_num(T1_id,T1_num),
	    t_to_num(T2_id,T2_num),
	    num_lub(T1_num,T2_num,T_num),
	    num_to_t(T_num,T_id),
	    lub_attr(T2_attr,Tl1_attr,T_attr), % since unbound only tail
	    ( list_types(T_id,_,_) ->
	      the_type(T,T_id,[Hd1_attr,T_attr])
	    ; simple_type(T,T_id,T_attr)
	    )
	  ; simple_bound_type(T2,T2_id,T2_attr) ->
	    list_types(T1_id,Hd1_id,Tl1_id),
	    t_to_num(T1_id,T1_num),
	    t_to_num(T2_id,T2_num),
	    num_lub(T1_num,T2_num,T_num),
	    num_to_t(T_num,T_id),
	    lub_attr(T2_attr,Tl1_attr,T_tl_attr),
	    lub_attr(T2_attr,Hd1_attr,T_hd_attr),
	    ( list_types(T_id,_,_) ->
	      the_type(T,T_id,[T_hd_attr,T_tl_attr])
	    ; simple_type(T,T_id,T_tl_attr)
	    )
	  ; list_type(T2,Hd2_id,Hd2_attr,Tl2_id,Tl2_attr) ->
	    list_types(T1_l_id,Hd1_id,Tl1_id),
	    list_types(T2_l_id,Hd2_id,Tl2_id),
	    t_to_num(T1_l_id,T1_num),
	    t_to_num(T2_l_id,T2_num),
	    num_lub(T1_num,T2_num,T_num),
	    num_to_t(T_num,T_id),
	    list_types(T_id,Hd_id,Tl_id),
	    lub_attr(Hd1_attr,Hd2_attr,Hd_attr),
	    lub_attr(Tl1_attr,Tl2_attr,Tl_attr),
	    list_type(T,Hd_id,Hd_attr,Tl_id,Tl_attr)
	  )
	; system_type(T1,T1_id,T1_attr) ->
	  ( system_type(T2,T2_id,T2_attr) ->
	    lub_attr(T1_attr,T2_attr,T_attr),
	    ( T1_id == T2_id ->
	      system_type(T,T1_id,T_attr)
	    ; T = none
	    )
	  ; T = none
	  )
	).

%

lub_attr(none,X,X).
lub_attr(a(P,S,L),X,Y) :- !,
	( X = none -> Y = a(P,S,L)
	; X = a(P1,S1,L1) ->
	  join_poss(P,P1,P2),
	  join_share(S,S1,S2),
	  join_loc(L,L1,L2),
	  Y = a(P2,S2,L2)
	; X = b(_C,P1,L1) ->
	  join_poss(P,P1,P2),
	  join_loc(L,L1,L2),
	  Y = a(P2,S,L2)
	).
lub_attr(b(C,P,L),X,Y) :- !,
	( X = none -> Y = b(C,P,L)
	; X = a(P1,S1,L1) ->
	  join_poss(P,P1,P2),
	  join_loc(L,L1,L2),
	  Y = a(P2,S1,L2)
	; X = b(_C1,P1,L1) ->
%	  join_cert(C,C1,C2),
	  join_poss(P,P1,P2),
	  join_loc(L,L1,L2),
	  Y = b(_C2,P2,L2)
	).

%

elt_to_lub_num(Type,Num) :-
	( simple_free_type(Type,T,_) ->
	  true
	; simple_bound_type(Type,T,_) ->
	  true
	; list_type(Type,Hd,_,Tl,_) ->
	  list_types(T,Hd,Tl)
	),
	t_to_num(T,Num).

%

t_to_num(X,_) :- var(X),!,sys_error('t_to_num/2: var arg').
t_to_num(any,1).
t_to_num(nv,2).
t_to_num(gnd,3).
t_to_num(list_a_fn,4).
t_to_num(list_nv_fn,5).
t_to_num(list_g_fn,6).
t_to_num(list_f_fn,7).
t_to_num(free_nil,8).
t_to_num(list_a_n,9).
t_to_num(list_nv_n,10).
t_to_num(list_g_n,11).
t_to_num(list_f_n,12).
t_to_num(nil,13).
t_to_num(list_a_f,14).
t_to_num(list_nv_f,15).
t_to_num(list_g_f,16).
t_to_num(list_f_f,17).
t_to_num(free,18).
t_to_num(bot,19).

num_to_t(X,_) :- var(X),!,sys_error('num_to_t/2: var arg').
num_to_t(1,any).
num_to_t(2,nv).
num_to_t(3,gnd).
num_to_t(4,list_a_fn).
num_to_t(5,list_nv_fn).
num_to_t(6,list_g_fn).
num_to_t(7,list_f_fn).
num_to_t(8,free_nil).
num_to_t(9,list_a_n).
num_to_t(10,list_nv_n).
num_to_t(11,list_g_n).
num_to_t(12,list_f_n).
num_to_t(13,nil).
num_to_t(14,list_a_f).
num_to_t(15,list_nv_f).
num_to_t(16,list_g_f).
num_to_t(17,list_f_f).
num_to_t(18,free).
num_to_t(19,bot).

%

num_lub(X,_,_) :- var(X),!,fail.
num_lub(1,_,L) :- L = 1.
num_lub(2,Y,L) :- num_lub_2(Y,L).
num_lub(3,Y,L) :- num_lub_3(Y,L).
num_lub(4,Y,L) :- num_lub_4(Y,L).
num_lub(5,Y,L) :- num_lub_5(Y,L).
num_lub(6,Y,L) :- num_lub_6(Y,L).
num_lub(7,Y,L) :- num_lub_7(Y,L).
num_lub(8,Y,L) :- num_lub_8(Y,L).
num_lub(9,Y,L) :- num_lub_9(Y,L).
num_lub(10,Y,L) :- num_lub_10(Y,L).
num_lub(11,Y,L) :- num_lub_11(Y,L).
num_lub(12,Y,L) :- num_lub_12(Y,L).
num_lub(13,Y,L) :- num_lub_13(Y,L).
num_lub(14,Y,L) :- num_lub_14(Y,L).
num_lub(15,Y,L) :- num_lub_15(Y,L).
num_lub(16,Y,L) :- num_lub_16(Y,L).
num_lub(17,Y,L) :- num_lub_17(Y,L).
num_lub(18,Y,L) :- num_lub_18(Y,L).
num_lub(19,Y,Y).

%

num_lub_2(X,_) :- var(X),!,fail.
num_lub_2(2,2) :- !.
num_lub_2(3,2) :- !.
num_lub_2(9,2) :- !.
num_lub_2(10,2) :- !.
num_lub_2(11,2) :- !.
num_lub_2(12,2) :- !.
num_lub_2(13,2) :- !.
num_lub_2(19,2) :- !.
num_lub_2(_,1).

%

num_lub_3(X,_) :- var(X),!,fail.
num_lub_3(2,2) :- !.
num_lub_3(3,3) :- !.
num_lub_3(9,2) :- !.
num_lub_3(10,2) :- !.
num_lub_3(11,3) :- !.
num_lub_3(12,2) :- !.
num_lub_3(13,3) :- !.
num_lub_3(19,3) :- !.
num_lub_3(_,1).

%

num_lub_4(X,_) :- var(X),!,fail.
num_lub_4(1,1) :- !.
num_lub_4(2,1) :- !.
num_lub_4(3,1) :- !.
num_lub_4(_,4).

%

num_lub_5(X,_) :- var(X),!,fail.
num_lub_5(1,1) :- !.
num_lub_5(2,1) :- !.
num_lub_5(3,1) :- !.
num_lub_5(4,4) :- !.
num_lub_5(7,4) :- !.
num_lub_5(9,4) :- !.
num_lub_5(12,4) :- !.
num_lub_5(13,4) :- !.
num_lub_5(14,4) :- !.
num_lub_5(17,4) :- !.
num_lub_5(_,5).

%

num_lub_6(X,_) :- var(X),!,fail.
num_lub_6(1,1) :- !.
num_lub_6(2,1) :- !.
num_lub_6(3,1) :- !.
num_lub_6(4,4) :- !.
num_lub_6(5,5) :- !.
num_lub_6(7,4) :- !.
num_lub_6(9,4) :- !.
num_lub_6(10,5) :- !.
num_lub_6(12,4) :- !.
num_lub_6(14,4) :- !.
num_lub_6(15,5) :- !.
num_lub_6(17,4) :- !.
num_lub_6(_,6).

%

num_lub_7(X,_) :- var(X),!,fail.
num_lub_7(1,1) :- !.
num_lub_7(2,1) :- !.
num_lub_7(3,1) :- !.
num_lub_7(7,7) :- !.
num_lub_7(8,7) :- !.
num_lub_7(12,7) :- !.
num_lub_7(13,7) :- !.
num_lub_7(17,7) :- !.
num_lub_7(19,7) :- !.
num_lub_7(_,4).

%

num_lub_8(X,_) :- var(X),!,fail.
num_lub_8(1,1) :- !.
num_lub_8(2,1) :- !.
num_lub_8(3,1) :- !.
num_lub_8(4,4) :- !.
num_lub_8(5,5) :- !.
num_lub_8(6,6) :- !.
num_lub_8(7,7) :- !.
num_lub_8(9,4) :- !.
num_lub_8(10,5) :- !.
num_lub_8(11,5) :- !.
num_lub_8(12,7) :- !.
num_lub_8(14,4) :- !.
num_lub_8(15,5) :- !.
num_lub_8(16,6) :- !.
num_lub_8(17,7) :- !.
num_lub_8(_,8).

%

num_lub_9(X,_) :- var(X),!,fail.
num_lub_9(1,1) :- !.
num_lub_9(2,2) :- !.
num_lub_9(3,2) :- !.
num_lub_9(9,9) :- !.
num_lub_9(10,9) :- !.
num_lub_9(11,9) :- !.
num_lub_9(12,9) :- !.
num_lub_9(13,9) :- !.
num_lub_9(19,9) :- !.
num_lub_9(_,4).

%

num_lub_10(X,_) :- var(X),!,fail.
num_lub_10(1,1) :- !.
num_lub_10(2,2) :- !.
num_lub_10(3,2) :- !.
num_lub_10(4,4) :- !.
num_lub_10(7,4) :- !.
num_lub_10(9,9) :- !.
num_lub_10(10,10) :- !.
num_lub_10(11,10) :- !.
num_lub_10(12,9) :- !.
num_lub_10(13,10) :- !.
num_lub_10(14,4) :- !.
num_lub_10(17,4) :- !.
num_lub_10(19,10) :- !.
num_lub_10(_,5).

%

num_lub_11(X,_) :- var(X),!,fail.
num_lub_11(1,1) :- !.
num_lub_11(2,2) :- !.
num_lub_11(3,3) :- !.
num_lub_11(4,4) :- !.
num_lub_11(5,5) :- !.
num_lub_11(6,6) :- !.
num_lub_11(7,4) :- !.
num_lub_11(8,5) :- !.
num_lub_11(9,9) :- !.
num_lub_11(10,10) :- !.
num_lub_11(11,11) :- !.
num_lub_11(12,9) :- !.
num_lub_11(13,11) :- !.
num_lub_11(14,4) :- !.
num_lub_11(15,5) :- !.
num_lub_11(16,6) :- !.
num_lub_11(17,4) :- !.
num_lub_11(18,6) :- !.
num_lub_11(19,11).

%

num_lub_12(X,_) :- var(X),!,fail.
num_lub_12(1,1).
num_lub_12(2,2).
num_lub_12(3,2).
num_lub_12(4,4).
num_lub_12(5,4).
num_lub_12(6,4).
num_lub_12(7,7).
num_lub_12(8,7).
num_lub_12(9,9).
num_lub_12(10,9).
num_lub_12(11,9).
num_lub_12(12,12).
num_lub_12(13,12).
num_lub_12(14,4).
num_lub_12(15,4).
num_lub_12(16,4).
num_lub_12(17,7).
num_lub_12(18,7).
num_lub_12(19,12).

%
num_lub_13(X,_) :- var(X),!,fail.
num_lub_13(1,1).
num_lub_13(2,2).
num_lub_13(3,3).
num_lub_13(4,4).
num_lub_13(5,4).
num_lub_13(6,6).
num_lub_13(7,7).
num_lub_13(8,8).
num_lub_13(9,9).
num_lub_13(10,10).
num_lub_13(11,11).
num_lub_13(12,12).
num_lub_13(13,13).
num_lub_13(14,4).
num_lub_13(15,5).
num_lub_13(16,6).
num_lub_13(17,7).
num_lub_13(18,8).
num_lub_13(19,13).

%
num_lub_14(X,_) :- var(X),!,fail.
num_lub_14(1,1) :- !.
num_lub_14(2,1) :- !.
num_lub_14(3,1) :- !.
num_lub_14(14,14) :- !.
num_lub_14(15,14) :- !.
num_lub_14(16,14) :- !.
num_lub_14(17,14) :- !.
num_lub_14(18,14) :- !.
num_lub_14(19,14) :- !.
num_lub_14(_,4).

%

num_lub_15(X,_) :- var(X),!,fail.
num_lub_15(1,1).
num_lub_15(2,1).
num_lub_15(3,1).
num_lub_15(4,4).
num_lub_15(5,5).
num_lub_15(6,5).
num_lub_15(7,4).
num_lub_15(8,5).
num_lub_15(9,4).
num_lub_15(10,5).
num_lub_15(11,5).
num_lub_15(12,4).
num_lub_15(13,5).
num_lub_15(14,14).
num_lub_15(15,15).
num_lub_15(16,15).
num_lub_15(17,14).
num_lub_15(18,15).
num_lub_15(19,15).

%
num_lub_16(X,_) :- var(X),!,fail.
num_lub_16(1,1).
num_lub_16(2,1).
num_lub_16(3,1).
num_lub_16(4,4).
num_lub_16(5,5).
num_lub_16(6,5).
num_lub_16(7,4).
num_lub_16(8,6).
num_lub_16(9,4).
num_lub_16(10,5).
num_lub_16(11,6).
num_lub_16(12,4).
num_lub_16(13,6).
num_lub_16(14,14).
num_lub_16(15,15).
num_lub_16(16,16).
num_lub_16(17,14).
num_lub_16(18,16).
num_lub_16(19,16).

%

num_lub_17(X,_) :- var(X),!,fail.
num_lub_17(1,1).
num_lub_17(2,1).
num_lub_17(3,1).
num_lub_17(4,4).
num_lub_17(5,4).
num_lub_17(6,4).
num_lub_17(7,7).
num_lub_17(8,7).
num_lub_17(9,4).
num_lub_17(10,4).
num_lub_17(11,4).
num_lub_17(12,7).
num_lub_17(13,7).
num_lub_17(14,14).
num_lub_17(15,14).
num_lub_17(16,14).
num_lub_17(17,17).
num_lub_17(18,17).
num_lub_17(19,17).

%

num_lub_18(X,_) :- var(X),!,fail.
num_lub_18(2,1) :- !.
num_lub_18(3,1) :- !.
num_lub_18(7,4) :- !.
num_lub_18(9,4) :- !.
num_lub_18(10,5) :- !.
num_lub_18(11,6) :- !.
num_lub_18(12,7) :- !.
num_lub_18(13,8) :- !.
num_lub_18(19,18) :- !.
num_lub_18(X,X).

%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstract unification:
%
% The result is the following:
% (a) the type of the resulting terms (after unification)
% (b) instantiations
% (c) changes to locality and sharing.
%
% Elements of the domain are:
%  bot
%  nil
%  gnd
%  nv(P,S,L)
%  any(P,S,L)
%  free(C,P,L)
%  free_nil(C,P,L)
%  list_g_n
%  list_nv_n(P,S,L)
%  list_a_n(P,S,L)
%  list_f_n(P,S,L)
%  list_g_fn(C,P,L)
%  list_nv_fn(P_hd,S_hd,L_hd,C,P,L)
%  list_a_fn(P_hd,S_hd,L_hd,C,P,L)
%  list_f_fn(P_hd,S_hd,L_hd,C,P,L)
%  list_g_f(C,P,L)
%  list_nv_f(P_hd,S_hd,L_hd,C,P,L)
%  list_a_f(P_hd,S_hd,L_hd,C,P,L)
%  list_f_f(P_hd,S_hd,L_hd,C,P,L)
%
% Abstract unification means finding the resulting type, propagating
% aliases, propagating sharing and propagating locality.

% list_type(List,HeadType,HeadAttrs,TailType,TailAttrs)
%
% This is a truly general purpose predicate. It has the following tasks:
% 1. It tests for a type being a list
% 2. It finds the types of head and tail
% 3. It finds the associated info of head and tail
%    In the case of a head item, this is
%    'none' for a ground one and 'a(P,S,L)' for an item
%    with possible aliases, sharing and locality. (Type is given as arg2)
%    For tail items, it is 'none' or 'b(C,P,L)' for open-ended lists.

list_type(list_g_n,g,none,n,none).
list_type(list_nv_n(P,S,L),nv,a(P,S,L),n,none).
list_type(list_a_n(P,S,L),a,a(P,S,L),n,none).
list_type(list_f_n(P,S,L),f,a(P,S,L),n,none).
list_type(list_g_fn(C,P,L),g,none,fn,b(C,P,L)).
list_type(list_nv_fn(P_hd,S_hd,L_hd,C,P,L),nv,a(P_hd,S_hd,L_hd),fn,b(C,P,L)).
list_type(list_a_fn(P_hd,S_hd,L_hd,C,P,L),a,a(P_hd,S_hd,L_hd),fn,b(C,P,L)).
list_type(list_f_fn(P_hd,S_hd,L_hd,C,P,L),f,a(P_hd,S_hd,L_hd),fn,b(C,P,L)).
list_type(list_g_f(C,P,L),g,none,f,b(C,P,L)).
list_type(list_nv_f(P_hd,S_hd,L_hd,C,P,L),nv,a(P_hd,S_hd,L_hd),f,b(C,P,L)).
list_type(list_a_f(P_hd,S_hd,L_hd,C,P,L),a,a(P_hd,S_hd,L_hd),f,b(C,P,L)).
list_type(list_f_f(P_hd,S_hd,L_hd,C,P,L),f,a(P_hd,S_hd,L_hd),f,b(C,P,L)).

% simple_type is like list_type: 1st arg is type,
% 2nd arg is type-id, 3rd arg is
% associated info (none, a/3 or b/3).
%
% We also use some subgroups.

simple_type(bot,bot,Attr) :-
	( Attr = none -> true ; true ).
simple_type(nil,nil,Attr) :-
	( Attr = none -> true ; true ).
simple_type(gnd,gnd,Attr) :-
	( Attr = none -> true ; true ).
simple_type(nv(P,S,L),nv,Attr) :-
	( Attr = a(P,S,L) ->
	  true
	; Attr = b(_C,P,L) ->
	  true
	; Attr = none ->
	  true
	).
simple_type(any(P,S,L),any,Attr) :-
	( Attr = a(P,S,L) ->
	  true
	; Attr = b(_C,P,L) ->
	  true
	; Attr = none ->
	  true
	).
simple_type(free(C,P,L),free,Attr) :-
	( Attr = b(C,P,L) ->
	  true
	; Attr = a(P,_S,L) ->
	  true
	; Attr = none ->
	  true
	).
simple_type(free_nil(C,P,L),free_nil,Attr) :-
	( Attr = b(C,P,L) ->
	  true
	; Attr = a(P,_S,L) ->
	  true
	; Attr = none ->
	  true
	).

/*******************************************************************************
simple_type(bot,bot,_).
simple_type(nil,nil,_).
simple_type(gnd,gnd,_).
simple_type(nv(P,S,L),nv,a(P,S,L)).
simple_type(any(P,S,L),any,a(P,S,L)).
simple_type(free(C,P,L),free,b(C,P,L)).
simple_type(free_nil(C,P,L),free_nil,b(C,P,L)).
*******************************************************************************/

ground_type(bot).
ground_type(nil).
ground_type(gnd).
ground_type(list_g_n).

simple_bound_type(bot,bot,none).
simple_bound_type(nil,nil,none).
simple_bound_type(gnd,gnd,none).
simple_bound_type(nv(P,S,L),nv,a(P,S,L)).
simple_bound_type(any(P,S,L),any,a(P,S,L)).

simple_free_type(free(C,P,L),free,b(C,P,L)).
simple_free_type(free_nil(C,P,L),free_nil,b(C,P,L)).

system_type(det,det,none).
system_type(nondet,nondet,none).

system_type_id(det).
system_type_id(nondet).

% Construct a type given attributes. Note that some types
% ignore attributes; these are used when a previous computation
% returns (useless) attributes to discard these.

the_type(bot,bot,_).
the_type(nil,nil,_).
the_type(gnd,gnd,_).
the_type(nv(P,S,L),nv,[a(P,S,L)]).
the_type(any(P,S,L),any,[a(P,S,L)]).
the_type(free(C,P,L),free,[b(C,P,L)]).
the_type(free_nil(C,P,L),free_nil,[b(C,P,L)]).
the_type(list_g_n,list_g_n,[_|_]).
the_type(list_nv_n(P,S,L),list_nv_n,[a(P,S,L)|_]).
the_type(list_a_n(P,S,L),list_a_n,[a(P,S,L)|_]).
the_type(list_f_n(P,S,L),list_f_n,[a(P,S,L)|_]).
the_type(list_g_fn(C,P,L),list_g_fn,[_,b(C,P,L)]).
the_type(list_nv_fn(P,S,L,C1,P1,L1),list_nv_fn,[a(P,S,L),b(C1,P1,L1)]).
the_type(list_a_fn(P,S,L,C1,P1,L1),list_a_fn,[a(P,S,L),b(C1,P1,L1)]).
the_type(list_f_fn(P,S,L,C1,P1,L1),list_f_fn,[a(P,S,L),b(C1,P1,L1)]).
the_type(list_g_f(C,P,L),list_g_f,[_,b(C,P,L)]).
the_type(list_nv_f(P,S,L,C1,P1,L1),list_nv_f,[a(P,S,L),b(C1,P1,L1)]).
the_type(list_a_f(P,S,L,C1,P1,L1),list_a_f,[a(P,S,L),b(C1,P1,L1)]).
the_type(list_f_f(P,S,L,C1,P1,L1),list_f_f,[a(P,S,L),b(C1,P1,L1)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Normalization of a type.
%
% If a type has been instantiated or an alias of it has been
% instantiated, it must be brought back to a normal state.
% 
% Ground types never change;
% bound types have their poss. alias variable found;
% ("...and in the darkness bind them")
% free types turn into new types if non-normal;
% lists either change somewhat or turn into 'any'.

norm_pat(PX,NormPX) :-
	( PX = bot -> NormPX = bot
	; PX = shr(S,I,T) ->
	  install_aliases(S,T),
	  T =.. [D|Xs],
	  norm_list(Xs,1,0,NewIs,NormXs),
	  NewI is I \/ NewIs,
	  NormT =.. [D|NormXs],
	  NormPX = shr(Shr,NewI,NewT),
	  aliasing_of_tuple(NormT,Shr,NewT)
	; PX =.. [D|Xs],
	  norm_list(Xs,1,0,_Is,NormXs),
	  NormPX =.. [D|NormXs]
	).

%

norm_list([],_N,I,I,[]).

norm_list([X|Xs],N,I0,I2,[NormX|NormXs]) :- 
	norm(X,Inst,NormX),
	I1 is I0 \/ (Inst << (2*(N-1))),
	M is N+1,
	norm_list(Xs,M,I1,I2,NormXs).

%

norm(uninit,X,Norm) :- !,
	X = 0,
	Norm = free(_C,_P,_L).

norm(det,X,Norm) :- !,
	X = 0,
	Norm = det.

norm(nondet,X,Norm) :- !,
	X = 0,
	Norm = det.

norm(T,Inst,NormT) :- 
	( simple_bound_type(T,Type,Attr) ->
	  norm_bound_attr(Attr,_,Inst,NewAttr),
	  simple_bound_type(NormT,Type,NewAttr)
	; simple_free_type(T,Type,Attr) ->
	  norm_free_attr(Attr,AttrOrType,Inst,NewAttr),
	  ( AttrOrType == type -> NormT = NewAttr
	  ; AttrOrType == attr -> simple_free_type(NormT,Type,NewAttr)
	  )
	; list_type(T,HeadType,HeadAttr,TailType,TailAttr) ->
	  norm_free_attr(TailAttr,AttrOrType,Inst0,NewTailAttr),
	  norm_bound_attr(HeadAttr,ChangedType,Inst1,NormHeadAttr),
	  Inst is (Inst0 << 1 ) + Inst1,
	  free_collapsed(HeadType,ChangedType,ModHeadType),
	  ( AttrOrType == type ->
	    ( list_type(NewTailAttr,T_hd,T_attr,T_t,T_t_attr) ->
	      absu_hd_fn(ModHeadType,T_hd,NewHeadType),
	      lub_attr(NormHeadAttr,T_attr,NewHeadAttr),
	      list_type(NormT,NewHeadType,NewHeadAttr,T_t,T_t_attr)
	    ; simple_type(NewTailAttr,Type,T_attr),
	      ( Type = nil ->
		list_type(NormT,ModHeadType,NormHeadAttr,n,none)
	      ; Type = free(C,P,L) ->
		list_type(NormT,ModHeadType,NormHeadAttr,f,b(C,P,L))
	      ; Type = free_nil(C,P,L) ->
		list_type(NormT,ModHeadType,NormHeadAttr,fn,b(C,P,L))
	      ; lub_attr(NormHeadAttr,T_attr,NormAttr),
		simple_type(NormT,Type,NormAttr)
	      )
	    )
	  ; AttrOrType == attr ->
	    list_type(NormT,ModHeadType,NormHeadAttr,TailType,NewTailAttr)
	  )
	).

%

free_collapsed(f,X,Y) :- !,( X = a -> Y = a ; Y = f ).
free_collapsed(X,_,X).

% norm_bound_attr/3:
% normalize arg 1, returning arg 3.
% if arg 1 changed, arg 2 yields new type as 'a','f' or 'g'. (This is
% used for list_f_* types, which do not fall under the simple_free_type
% category, but still must change type.)

norm_bound_attr(X,_,_,_) :- var(X),!,sys_error('norm_bound_attr/2: var arg').

norm_bound_attr(none,g,Inst,X) :- !, X = none, Inst = 0.

norm_bound_attr(a(P,S,L),Change,Inst,X) :-
	( nonvar(P) ->
	  X = a(NewP,S,L),
	  Change = a,
	  norm_alias_var(P,S,L,NewP),
	  Inst = 1
	; Change = f, X = a(P,S,L), Inst = 0
	).

%

norm_alias_var(X,_S,_L,NewX) :- var(X),!, NewX = X.

norm_alias_var(any(P,S,L),S1,L1,NewX) :-
	join_share(S,S1,S2),
	join_loc(L,L1,L2),
	norm_alias_var(P,S2,L2,NewX).

%

norm_free_attr(X,_,_,_) :- var(X),!,sys_error('norm_free_attr/3: var arg').
norm_free_attr(none,attr,0,none).
norm_free_attr(b(C,P,L),TypeOrAttr,Inst,NewItem) :-
	( nonvar(C) ->
	  Inst = 1,
	  TypeOrAttr = type,
	  norm(C,_Inst,NewItem)
	; nonvar(P) ->
	  Inst = 1,
	  TypeOrAttr = type,
	  norm_alias_var(P,S,L,NewP),
	  simple_bound_type(NewItem,any,a(NewP,S,L))
	; TypeOrAttr = attr,
	  Inst = 0,
	  NewItem = b(C,P,L)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstract unification:
%
% Take two types, and perform abstract unification.
% This entails:
% - binding aliases
% - increasing sharing and locality
% - returning a new type (izzat necessary? well, it's convenient!)
%
% Also, if X was instantiated by the unification, InstX is bound
% and the converse for Y.

% Unifications may be unnecessarily complex when appearing in
% programs. We dynamically simplify them into "X=Y","X=f(...)", "true"
% or "fail" operations.

absunify(X,Y,A0,A1) :-
	( atomic(X) ->
	  ( atomic(Y) ->
	    ( X == Y ->
	      A0 = A1
	    ; failed_state(A0,A1)
	    )
	  ; Y = '$VAR'(M) ->
	    A0 = A1,
	    lookup_env(M,A0,T2),
	    ( X = [] ->
	      instantiated_by(T2,nil,Inst),
	      absu(nil,T2,T),
	      update_env(M,T,Inst,A0,A1)
	    ; instantiated_by(T2,gnd,Inst),
	      absu(gnd,T2,T),
	      update_env(M,T,Inst,A0,A1)
	    )
	  ; failed_state(A0,A1)
	  )
	; X = '$VAR'(N) ->
	  lookup_env(N,A0,T1),
	  ( Y = '$VAR'(M) ->
	    lookup_env(M,A0,T2),
	    instantiated_by(T1,T2,T1_inst),
	    instantiated_by(T2,T1,T2_inst),
	    absu(T1,T2,T),
	    update_env(N,T,T1_inst,A0,Ax),
	    update_env(M,T,T2_inst,Ax,A1)
	  ; abstract(Y,A0,T2),
	    instantiated_by(T1,T2,T1_inst),
	    instantiated_by(T2,T1,T2_inst),
	    absu(T1,T2,T),
	    update_env(N,T,T1_inst,A0,Ax),
	    update_i_state_term_1(Y,T,T2_inst,Ax,A1)
	  )
	; ( Y = '$VAR'(M) ->
	    lookup_env(M,A0,T2),
	    abstract(X,A0,T1),
	    instantiated_by(T1,T2,T1_inst),
	    instantiated_by(T2,T1,T2_inst),
	    absu(T1,T2,T),
	    update_env(M,T,T2_inst,A0,Ax),
	    update_i_state_term_1(X,T,T1_inst,Ax,A1)
	  ; atomic(Y) ->
	    failed_state(A0,A1)
	  ; functor(X,P,N),
	    functor(Y,Q,M),
	    ( ( P = Q, N =:= M ) ->
	      X =.. [_|Xs],
	      Y =.. [_|Ys],
	      absunify_list(Xs,Ys,A0,A1)
	    ; failed_state(A0,A1)
	    )
	  )
	).

%

absunify_list([],[],A,A).

absunify_list([X|Xs],[Y|Ys],A0,A2) :-
	absunify(X,Y,A0,A1),
	absunify_list(Xs,Ys,A1,A2).

% In a unification, test if type X is instantiated by type Y.
%
% If not, the previous entry is not unified with the new type.

instantiated_by(X,Y,Inst) :-
	( inst_by(X,Y) ->
	  Inst = 2'11
	; Inst = 2'0
	).

inst_by(X,Y) :-
	( ground_type(X) -> fail
	; simple_type(Y,Y_id,_) ->
	  ( Y_id = free_nil ->
	    ( simple_free_type(X,free,_) -> true
	    ; simple_bound_type(X,any,_) -> true
	    ; list_type(X,_,_,f,_) -> true
	    ; fail
	    )
	  ; Y_id = free ->
	    fail
	  )
	; true
	).

%

failed_state(A0,A1) :-
	A0 = A1,
	set_comp_state(A0,failed).
	    
% Here is where the real work is done.

absu(T1,T2,T) :-
	absu_t(T1,T2,T).

% Abstract unification of types. The main thing is to find the correct
% new type. Almost as an afterthought, sharing, aliasing and so on is
% updated. (If you are trying to understand this code, look at it from
% that perspective.)

absu_t(X,_,_) :- var(X),!,sys_error('absu_t/3: var arg').

absu_t(bot,_,X) :- !, X = bot.

absu_t(Type1,Type2,Type) :-
	( simple_free_type(Type1,T1_id,T1_attr) ->
	  ( system_type(Type2,T2_id,T2_attr) ->
	    ( T1_id = free ->
	      Type = Type2,
	      bind(T1_attr,Type2)
	    ; sys_error('system type ~q unified with ~q',[T2_id,T1_id])
	    )
	  ; simple_free_type(Type2,T2_id,T2_attr) ->
	    ( T1_id = free ->
	      ( T2_id = free_nil ->
		bind(T1_attr,Type2),
		Type = Type2
	      ; T2_id = free ->
		join_free_types(T1_id,T2_id,T1_attr,T2_attr),
		Type = Type2
	      )
	    ; T1_id = free_nil ->
	      ( T2_id = free ->
		bind(T2_attr,Type1),
		Type = Type1
	      ; T2_id = free_nil ->
		join_free_types(T1_id,T2_id,T1_attr,T2_attr),
		Type = Type2		
	      )
	    )
	  ; simple_bound_type(Type2,T2_id,T2_attr) ->
	    Type = Type2,
	    bind(T1_attr,Type2)
	  ; list_type(Type2,Hd2_id,Hd2_attr,Tl2_id,Tl2_attr) ->
	    Type = Type2,
	    bind(T1_attr,Type2)
	  )

	; simple_bound_type(Type1,T1_id,T1_attr) ->
	  ( simple_free_type(Type2,T2_id,T2_attr) ->
	    Type = Type1,
	    bind(T2_attr,Type1)
	  ; simple_bound_type(Type2,T2_id,T2_attr) ->
	    instantiate(T1_attr,P,S,L),
	    instantiate(T2_attr,P,S,L),
	    T_attr = a(P,S,L),
	    absu_other_other(T1_id,T2_id,T_id),
	    ( ground_type(T_id) ->
	      Type = T_id
	    ; simple_bound_type(Type,T_id,T_attr)
	    )
	  ; list_type(Type2,Hd2_id,Hd2_attr,Tl2_id,Tl2_attr) ->
	    ( Tl2_id = n ->
	      absu_list_n_other(Hd2_id,T1_id,T_id),
	      instantiate(T1_attr,P,S,L),
	      instantiate(Hd2_attr,P,S,L),	    
	      ( ground_type(T_id) ->
		Type = T_id
	      ; the_type(Type,T_id,[a(P,S,L)])
	      )
	    ; absu_list_fn_other(Hd2_id,Tl2_id,T1_id,T_id),
	      instantiate(Hd2_attr,P,S,L),
	      instantiate(T1_attr,P,S,L),
	      norm(Type1,_Inst,NormType1),
	      bind(Tl2_attr,NormType1),
	      ( ground_type(T_id) ->
		Type = T_id
	      ; the_type(Type,T_id,[a(P,S,L)])
	      )
	    )
	  )

	; list_type(Type1,Hd1_id,Hd1_attr,Tl1_id,Tl1_attr) ->
	  ( simple_free_type(Type2,T2_id,T2_attr) ->
	    Type = Type1,
	    bind(T2_attr,Type1)
	  ; simple_bound_type(Type2,T2_id,T2_attr) ->
	    ( Tl1_id = n ->
	      absu_list_n_other(Hd1_id,T2_id,T_id),
	      instantiate(T2_attr,P,S,L),
	      instantiate(Hd1_attr,P,S,L),
	      ( ground_type(T_id) ->
		Type = T_id
	      ; the_type(Type,T_id,[a(P,S,L)])
	      )
	    ; absu_list_fn_other(Hd1_id,Tl1_id,T2_id,T_id),
	      instantiate(Hd1_attr,P,S,L),
	      instantiate(T2_attr,P,S,L),
	      norm(Type2,_,NormType2),
	      bind(Tl1_attr,NormType2),
	      ( ground_type(T_id) ->
		Type = T_id
	      ; the_type(Type,T_id,[a(P,S,L)])
	      )
	    )
	  ; list_type(Type2,Hd2_id,Hd2_attr,Tl2_id,Tl2_attr) ->
	    absu_lists_t(Hd1_id,Hd2_id,Tl1_id,Tl2_id,Hd_id,Tl_id),
	    maybe_instantiate(Hd2_id,Hd1_attr,P,S,L), % depends on other's id
	    maybe_instantiate(Hd1_id,Hd2_attr,P,S,L), % here too
	    maybe_bind(Tl1_id,Hd2_id,Hd2_attr,Tl_id,Tl1_attr,C1,P1,L1),
	    maybe_bind(Tl2_id,Hd1_id,Hd1_attr,Tl_id,Tl2_attr,C1,P1,L1),
	    Hd_attr = a(P,S,L),
	    Tl_attr = b(C1,P1,L1),
	    list_types(List,Hd_id,Tl_id),
	    the_type(Type,List,[Hd_attr,Tl_attr])
	  )
        ; system_type(Type1,T1_id,T1_attr) ->
	  ( simple_free_type(Type2,T2_id,T2_attr) ->
	    ( T2_id = free ->
	      Type = Type1,
	      bind(T2_attr,Type1)
	    ; sys_error('system type ~q unified with ~q',[T1_id,T2_id])
	    )
	  ; system_type(Type2,T2_id,T2_attr) ->
	    ( T1_id == T2_id ->
	      Type = Type1
	    ; sys_error('system type ~q unified with system type ~q',[T1_id,T2_id])
	    )
	  ; sys_error('system type ~q unified with ~q',[T1_id,Type2])
	  ) 
	).

% join_free_types means two f or fn types are unified.

join_free_types(T1_id,T2_id,T1_attr,T2_attr) :-
	( ( T1_id = f, T2_id = f ) ->
	  T_id = f
	; T_id = fn
	),
	T1_attr = b(C1,P1,L1),
	T2_attr = b(C2,P2,L2),
	C1 = C2, P1 = P2, L1 = L2. % assumes normalized!!

% bind/2 binds the attribute to the type. It holds when unification
% means binding will occur. (Bindings are made to free vars; others
% get measly instantiations.)

bind(b(C,P,L),Type) :-
	( simple_free_type(Type,T_id,T_attr) ->
	  T_id = free_nil, T_attr = b(_C1,P1,L1),
	  C = Type,
	  instantiate_var(P,P1,_,L1),
	  L = L1
	; simple_bound_type(Type,_,Attr) ->
	  norm_alias_var(P,S1,L1,NewP1),
	  ( Attr = none ->
	    C = Type, NewP1 = any(_,S1,L1)
	  ; Attr = a(P2,S2,L2) ->
	    C = Type,
	    instantiate_var(NewP1,P2,S2,L2),
	    L1 = L2
	  )
	; system_type(Type,_,_) ->
	  C = Type,
	  P = any(_,_,L)
	; list_type(Type,_,Hd_attr,_,Tl_attr) ->
	  ( Tl_attr = none ->
	    C = Type
	  ; Tl_attr = b(C1,P1,L1) ->
	    ( C == C1 -> warning('circular unify of list -- only poss inst')
	    ; C = Type
	    ),
	    norm_alias_var(P1,S1,L1,NewP1),
	    ( P == P1 ->
	      P = any(_,S1,L1), L = L1
	    ; P = any(NewP1,S1,L1), L = L1
	    )
          ; sys_error('bind/2: a(...) attribute disallowed for tail')
	  ),
	  ( Hd_attr = none ->
	    true
	  ; Hd_attr = a(P2,S2,L2) ->
	    norm_alias_var(P2,S2,L2,NewP2),
	    norm_alias_var(P,S,L,NewP),
	    NewP2 = NewP,
	    S = S2,
	    L = L2
	  ; sys_error('bind/2: b(...) attribute disallowed for head')
	  )
	).

%

instantiate_var(P,P1,S1,L1) :-
	( var(P1) ->
	  ( var(P) ->
	    ( P == P1 -> 
	      true
	    ; P = any(P1,S1,L1)
	    )
	  ; norm_alias_var(P,S,L,NewP),
	    S = S1, L = L1,
	    P1 = any(NewP,S,L)
	  )
	; norm_alias_var(P1,S1,L1,NewP1),
	  P = any(NewP1,S1,L1)
	).

% instantiate/4 is invoked when the type in question may have had
% variables instantiated; this means the alias variable has to be
% bound. Also return the sharing & locality vars.

instantiate(none,_,_,_).

instantiate(a(P,S,L),P1,S1,L1) :-
	P = any(P1,S,L),
	S1 = S,
	L1 = L.

% maybe_bind/6
% Bind Attr if New_id is more instantiated than Old_id (i.e.
% f is bound unless f, fn bound unless f or fn).
%
% This corresponds to a situation where we unify
%  list_*_f  with {list_*_f, list_*_fn, list_*_n}
%  list_*_fn with {list_*_f, list_*_fn, list_*_n}
%  list_*_n  with {list_*_f, list_*_fn, list_*_n}

maybe_bind(f,Hd_id,Hd_attr,Tl_id,b(C,P,L),C1,P1,L1) :- !,
	( Tl_id = n ->
	  Tl_attr = none
	; Tl_attr = b(C1,P1,L1)
	),
	list_type(Type,Hd_id,Hd_attr,Tl_id,Tl_attr),
	C = Type,
	P = P1,     % should we check Hd_attr as well?
	L = L1.     % here too!

maybe_bind(fn,Hd_id,Hd_attr,Tl_id,b(C,P,L),C1,P1,L1) :- !,
	( Tl_id = f ->
	  New_Tl_id = fn,
	  Tl_attr = b(C1,P1,L1)
	; Tl_id = fn ->
	  New_Tl_id = fn,
	  Tl_attr = b(C1,P1,L1)
	; Tl_id = n ->
	  New_Tl_id = n,
	  Tl_attr = none
	),
	list_type(Type,Hd_id,Hd_attr,New_Tl_id,Tl_attr),
	C = Type,
	P = P1,     % should we check Hd_attr as well?
	L = L1.     % here too!

maybe_bind(n,_Hd_id,_Hd_attr,_Tl_id,_Tl_attr,_C,_P,_L).

% maybe_instantiate/6
% Instantiate the poss. alias var if the other's ID isn't 'f'
% (free).

maybe_instantiate(f,Attr,P,S,L) :- !,
	( Attr = none -> true
	; Attr = a(P1,S1,L1),
	  norm_alias_var(P1,S1,L1,NewP1),
	  NewP1 = P, S = S1, L = L1
	).

maybe_instantiate(_,Attr,P,S,L) :-
	( Attr = none -> true
	; Attr = a(P1,S1,L1) ->
	  norm_alias_var(P1,S1,L1,NewP1),
	  ( NewP1 == P -> true % would yield circ. bind
	  ; NewP1 = any(P,S,L),
	    S1 = S,
	    L1 = L
	  )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%join_cert(C1,C2) :- C1 = C2.

join_poss(P1,P2) :- P1 = P2.

join_poss(P1,P2,P1) :- P1 = P2.

join_share(S1,S2) :- worst_of_shr(S1,S2,S), worsen(S1,S), worsen(S2,S).

join_share(S1,S2,S) :- worst_of_shr(S1,S2,S), worsen(S1,S), worsen(S2,S).

join_loc(L1,L2) :- worst_of_loc(L1,L2,L), worsen(L1,L), worsen(L2,L).

join_loc(L1,L2,L) :- worst_of_loc(L1,L2,L), worsen(L1,L), worsen(L2,L).


worsen(X,Y) :- copy_term(Y,Y0), X = Y0.


%%%%%%%%%%

symbolic_type_of(Type,Type_id) :-
	( simple_type(Type,Type_id,_) ->
	  true
	; list_type(Type,Hd_id,_,Tl_id,_) ->
	  list_types(Type_id,Hd_id,Tl_id)
	; system_type(Type,Type_id,_) ->
	  true
	; sys_error('symbolic_type_of/2: type not recognized ~q',[Type])
	).

symbolic_locality_of_type(Type, Name) :-
	locality_of_type(Type, Locality),
	name_loc(Locality, Name).

locality_of_type(T,X) :-
	( simple_type(T,_,T_attr) ->
	    locality_of_attr(T_attr,X)
	; list_type(T,_,Hd_attr,_,Tl_attr) ->
	    locality_of_attr(Hd_attr,Hd_loc),
	    locality_of_attr(Tl_attr,Tl_loc),
	    worst_of_loc(Hd_loc,Tl_loc,X)
	; system_type(T,_,_) ->
	    make_local(X)
	).


locality_of_attr(none,X) :- !, make_local(X).
locality_of_attr(a(_,_,L),X) :- !, X = L.
locality_of_attr(b(_,_,L),X) :-  X = L.

linearity_of_attr(a(_,S,_),X) :- !, S = X.
linearity_of_attr(_,X) :- make_linear(X).

poss_alias_of_attr(none,_) :- !.
poss_alias_of_attr(a(P,_,_),X) :- !, X = P.
poss_alias_of_attr(b(_,P,_),X) :- X = P.

cert_alias_of_attr(b(C,_,_),X) :- !, X = C.
cert_alias_of_attr(_,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Simple domains such as LOC (locality) and IND (independance or linearity) are
% represented using an underlying (linear) structure allowing successive
% instantiation to model decreased precision.
%  The domain DET (determinancy) is represented using symbols {det, nondet}
% with ordering: det < nondet.
%
%  Domains LOC and IND are linear and ordered by subsumption (supported by the
% use of partial instantiated structures).
%
% Representation of LOC uses a tuple '$LOC'(X,Y) where X and Y are variables:
%
%    local  <  robust  <    wbf   <   fragile
%      X        s(X)      s(s(X))     s(s(0))
%
%  non-freeze  freeze
%      Y       freeze
%
% NOTE: The freeze property is not ordered.
%
% Representation of IND, where X is a variable:
%
%    linear <  indlist <  nonlinear
%      X         s(X)       s(0)
%

%shared(X) :- upper_middle_4point(X).
%shared_td(X) :- lower_middle_4point(X).
%time_dep(X) :- worst_4point(X).
%make_shared(X) :- make_upper_middle_4point(X).
%make_shared_td(X) :- make_lower_middle_4point(X).
%make_time_dep(X) :- make_worst_4point(X).

local('$LOC'(X,_)) :- var(X).
robust('$LOC'(X,_)) :- nonvar(X), X = s(X0), var(X0).
wbf('$LOC'(X,_)) :- nonvar(X), X = s(X0), nonvar(X0), X0 = s(X1), var(X1).
fragile('$LOC'(X,_)) :- X == s(s(0)).

freeze('$LOC'(_,Y)) :- Y == freeze.


name_loc(LOC, Name) :-
	( freeze(LOC) -> Name = freeze(X) ; Name = X ),
	( local(LOC) ->
	    X = local
	; robust(LOC) ->
	    X = robust
	; wbf(LOC) ->
	    X = wbf
	; fragile(LOC) ->
	    X = fragile
	).

	
make_local('$LOC'(_,_)).
make_robust('$LOC'(s(_),_)).
make_wbf('$LOC'(s(s(_)),_)).
make_fragile('$LOC'(s(s(0)),_)).

make_freeze('$LOC'(_,freeze)).

%%% to ensure compatibility with elder code.
%%%
shared(X) :- robust(X).
make_shared(X) :- make_robust(X).


less_than_loc('$LOC'(X,_), '$LOC'(_,_)) :- var(X),!.
less_than_loc('$LOC'(X,_), '$LOC'(Y,_)) :- X = s(X0),
	var(X0), !, nonvar(Y), Y = s(_).
less_than_loc('$LOC'(X,_), '$LOC'(Y,_)) :- X = s(s(X0)),
	var(X0), !, nonvar(Y), Y = s(Y0), nonvar(Y0), Y0 = s(_).
less_than_loc('$LOC'(X,_), '$LOC'(Y,_)) :- X = s(s(0)),
	Y == s(s(0)).


worst_of_loc(L1, L2, Worst) :-
	less_than_loc(L1, L2) -> Worst = L2 ; Worst = L1.


%%%%%

linear(X) :- var(X).
indlist(X) :- nonvar(X), X = s(X0), var(X0).
nonlinear(X) :- X == s(0).


name_shr(IND, Name) :-
	( linear(IND) -> Name = linear
	; indlist(IND) -> Name = indlist
	; nonlinear(IND) -> Name = nonlinear ).


make_linear(_).
make_indlist(s(_)).
make_nonlinear(s(0)).


less_than_shr(X,_) :- var(X),!.
less_than_shr(s(X0),Y0) :-
	var(X0),!, nonvar(Y0), Y0 = s(_).
less_than_shr(s(0),Y0) :- Y0 == s(0).


worst_of_shr(S1, S2, Worst) :-
	less_than_shr(S1, S2) -> Worst = S2 ; Worst = S1.


%%%%%

less_than_det(det,_).
less_than_det(nondet,nondet).



% This absu only finds out the resulting type of unifying two types.
% Aliasing, sharing and so on is not handled here.

absu_other_other(X,Y,Z) :-
	elt_to_absu_num(X,N1),
	elt_to_absu_num(Y,N2),
	least(N1,N2,N),
	elt_to_absu_num(Z,N).

% If list is closed, unification will only instantiate elements
% in list (list-ness cannot be violated)

absu_list_n_other(H1,Oth,T) :-
	( Oth = bot -> T = bot
	; Oth = nil -> T = nil
	; Oth = gnd ->
	  T = list_g_n
	; Oth = nv ->
	  ( H1 = g -> T = list_g_n
	  ; T = list_nv_n
	  )
	; Oth = any ->
	  ( H1 = g -> T = list_g_n
	  ; H1 = nv -> T = list_nv_n
	  ; T = list_a_n
	  )
	; Oth = free ->
	  list_types(T,H1,n)
	; Oth = free_nil ->
	  list_types(T,H1,n)
	).

% If list is open-ended, we always run the
% chance of binding the tail to a 'disallowed type'.

absu_list_fn_other(H1,T1,Oth,T) :-
	( Oth = free ->
	  list_types(T,H1,T1)
	; Oth = free_nil ->
	  list_types(T,H1,free_nil)
	; T = Oth
	).

% Two lists will yield a list as result.

absu_lists(L1,L2,L) :-
	list_types(L1,H1,T1),
	list_types(L2,H2,T2),
	absu_lists_t(H1,H2,T1,T2,H,T),
	list_types(L,H,T).

absu_lists_t(H1,H2,T1,T2,H,T) :-
	( ( T1 = n ; T2 = n ) ->
	  T = n
	; ( T1 = fn ; T2 = fn ) ->
	  T = fn
	; T = f
	),
	( T = n ->
	  absu_hd_n(H1,H2,H)
	; absu_hd_fn(H1,H2,H)
	).

%

cnv_lub(X,_) :- var(X),!,sys_error('cnv_lub/2: var arg').
cnv_lub(f,free).
cnv_lub(fn,free_nil).
cnv_lub(n,nil).
cnv_lub(a,any).
cnv_lub(nv,nv).
cnv_lub(g,gnd).

lub_cnv(X,_) :- var(X),!,sys_error('lub_cnv/2: var arg').
lub_cnv(free,f).
lub_cnv(free_nil,fn).
lub_cnv(nil,n).
lub_cnv(any,a).
lub_cnv(nv,nv).
lub_cnv(gnd,g).

%

elt_to_absu_num(bot,0).
elt_to_absu_num(nil,1).
elt_to_absu_num(gnd,2).
elt_to_absu_num(nv,3).
elt_to_absu_num(any,4).
elt_to_absu_num(free_nil,5).
elt_to_absu_num(free,6).

%

least(X,Y,Z) :-
	( X < Y -> Z = X
	; Z = Y
	).

% If list ends in nil, then unification will only succeed for equally
% long lists. E.g. [1,2] = [A,B] => [1,2] which is list_g_n.

absu_hd_n(X,_,_) :- var(X),!,sys_error('absu_hd_n/3: var arg').
absu_hd_n(g,_,g).
absu_hd_n(nv,X,H) :-
	( X = g -> H = g
	; H = nv
	).
absu_hd_n(a,X,H) :-
	( X = f -> H = a
	; H = X
	).
absu_hd_n(f,X,X).

% If lists are open-ended, then unification must use worst case
% (i.e. turn things into 'any')
% E.g. [1,2|X] = [A,B,C|Y] => [1,2,C|Y] which is list_a_f.

absu_hd_fn(X,_,_) :- var(X),!,sys_error('absu_hd_fn/3: var arg').
absu_hd_fn(g,X,H) :-
	( X = f -> H = a
	; H = X
	).
absu_hd_fn(nv,X,H) :-
	( X = g -> H = nv
	; X = nv -> H = nv
	; H = a
	).
absu_hd_fn(a,_,a).
absu_hd_fn(f,X,H) :-
	( X = f -> H = f
	; H = a
	).

%

list_types(list_g_n,g,n).
list_types(list_nv_n,nv,n).
list_types(list_a_n,a,n).
list_types(list_f_n,f,n).

list_types(list_g_fn,g,fn).
list_types(list_nv_fn,nv,fn).
list_types(list_a_fn,a,fn).
list_types(list_f_fn,f,fn).

list_types(list_g_f,g,f).
list_types(list_nv_f,nv,f).
list_types(list_a_f,a,f).
list_types(list_f_f,f,f).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Comparison:
% (1) Compare types
% (2) Compare call and success patterns
%
% Comparing types means comparing type, sharing and locality.
% Comparing call patterns means also comparing certain aliases;
% the less there are, the better it is. (There is 0 or 1 certain
% alias per argument position, so it's straightforward

less_than(bot,_Tu2) :- !.

less_than(shr(S1,_In1,T1),Tu2) :- !,
	( Tu2 = bot -> fail
	; Tu2 = shr(S2,_In2,T2) ->
	  install_aliases(S1,T1),
	  install_aliases(S2,T2),
%	  less_than_cert_aliases(S1,S2),
%	  less_than_instantiation(In1,In2),
	  T1 =.. [D0|Xs],
	  T2 =.. [D1|Ys],
	  less_than_det(D0,D1),
	  all_less_than(Xs,Ys)
	; aliasing_of_tuple(Tu2,S2,T2), % What is instantiation here?
	  less_than_cert_aliases(S1,S2),
	  T1 =.. [D0|Xs],
	  T2 =.. [D1|Ys],
	  less_than_det(D0,D1),
	  all_less_than(Xs,Ys)
	).

% NB. The safe approximation is actually 0 for call patterns and
% 2'11...11 for success patterns ...

less_than(Tuple1,Tuple2) :-
	aliasing_of_tuple(Tuple1,Shr1,Type1),
%	functor(Type1,_,N),
%	all_args_instantiated(N,AllInst),           % safe approximation
	AllInst = 0,
	less_than(shr(Shr1,AllInst,Type1),Tuple2).


less_than_instantiation(In1,In2) :-
	In2 is In1 \/ In2.

%

less_than_cert_aliases(S1,S2) :-
	sort(S1,SortS1), sort(S2,SortS2),
	list_of_bitvector_subset(SortS2,SortS1).

%

list_of_bitvector_subset([],_S).

list_of_bitvector_subset([X|Xs],S) :-
	( ( member(Y,S), X is Y /\ X ) ->
	  list_of_bitvector_subset(Xs,S)
	; fail
	).

%

all_less_than([],[]).

all_less_than([X|Xs],[Y|Ys]) :-
	less_than_type(X,Y),
	all_less_than(Xs,Ys).

% A type is included in another if its concretization is a subset of
% the concretization of the other. We have already determined that the
% certain aliases of the second arg are subsets of the first (OK, we're
% speaking of tuples here and types later on). So, we do not have to
% test for certain aliases.
% Furthermore, possible aliases can be ignored -- by taking the 'possibility'
% as 'false', which is allowed.
%  So, we must
% (a) compare types
% (b) compare sharing (if applicable)
% (c) compare locality
%
% If the first arg is less than the second in all respects, there ye go!

less_than_type(X,Y) :-
	( simple_free_type(X,X_id,X_attr) ->
	  ( simple_free_type(Y,Y_id,Y_attr) ->
	    less_than_t(X_id,Y_id),
	    less_than_attr(X_attr,Y_attr)
	  ; simple_bound_type(Y,Y_id,Y_attr) ->
	    Y_id = any
	  ; list_type(Y,Hd2_id,Hd2_attr,Tl2_id,Tl2_attr) ->
	    cnv_lub(Tl2_id,Tl2_type),
	    less_than_t(X_id,Tl2_type),
	    less_than_attr(X_attr,Tl2_attr) % also compare locality of Hd2?
	  )
	; simple_bound_type(X,X_id,X_attr) ->
	  ( simple_free_type(Y,Y_id,Y_attr) ->
	    ( X_id = nil, Y_id = free_nil )
	  ; simple_bound_type(Y,Y_id,Y_attr) ->
	    less_than_t(X_id,Y_id),
	    less_than_attr(X_attr,Y_attr)
	  ; list_type(Y,Hd2_id,Hd2_attr,Tl2_id,Tl2_attr) ->
	    cnv_lub(Tl2_id,Tl2_type),
	    less_than_t(X_id,Tl2_type),
	    less_than_attr(X_attr,Tl2_attr)
	  )
	; list_type(X,Hd1_id,Hd1_attr,Tl1_id,Tl1_attr) ->
	  ( simple_free_type(Y,Y_id,Y_attr) ->
	    fail
	  ; simple_bound_type(Y,Y_id,Y_attr) ->
	    cnv_lub(Hd1_id,Hd1_type),
	    less_than_t(Hd1_type,Y_id),
	    cnv_lub(Tl1_id,Tl1_type),
	    less_than_t(Tl1_type,Y_id),
	    less_than_attr(Hd1_attr,Y_attr),
	    less_than_attr(Tl1_attr,Y_attr)
	  ; list_type(Y,Hd2_id,Hd2_attr,Tl2_id,Tl2_attr) ->
	    cnv_lub(Hd1_id,Hd1_type),
	    cnv_lub(Hd2_id,Hd2_type),
	    less_than_t(Hd1_type,Hd2_type),
	    cnv_lub(Tl1_id,Tl1_type),
	    cnv_lub(Tl2_id,Tl2_type),
	    less_than_t(Tl1_type,Tl2_type),
	    less_than_attr(Hd1_attr,Hd2_attr),
	    less_than_attr(Tl1_attr,Tl2_attr)
	  )
	; system_type(X,X_id,X_attr) ->
	  system_type(Y,Y_id,Y_attr),
	  less_than_sys_type(X_id,Y_id),
	  less_than_attr(X_attr,Y_attr)
	).

%

less_than_t(T1,T2) :-
	t_to_num(T1,X),
	t_to_num(T2,Y),
	num_lub(X,Y,Z),
	num_to_t(Z,T2).

% Note: det/nondet are invariant as arguments: if a procedure is passed
%       p(...,det,...) it will ALWAYS return with p(...,det,...). (Actual determinacy
%       is decided otherwise!) Hence, these defs for system types.

less_than_sys_type(det,det).
less_than_sys_type(nondet,nondet).

%

less_than_attr(none,_).
less_than_attr(a(_,S,L),a(_,S1,L1)) :-
	less_than_shr(S,S1), less_than_loc(L,L1).
less_than_attr(b(_,_,L),X) :-
	( X = a(_,_,L1) ->
	    less_than_loc(L,L1)
	; X = b(_,_,L1) ->
	    less_than_loc(L,L1)
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Test predicates.
%

test_lub :-
	symbolic_elts(Xs),
	member(X,Xs), member(Y,Xs),
	( lub(X,Y,Z) -> true
	; error('~q \/ ~q *** LUB failed ***',[X,Y])
	),
	format(user_error,'~q \/ ~q = ~q~n',[X,Y,Z]),
	fail.


test_absu :-
	generate_type(T1),
	generate_type(T2),
	( absu(T1,T2,T) ->
	  format(user_error,'~p = ~p => ~p~n',[T1,T2,T])
	; error('~p = ~p *** ABSUNIFY FAILED ***',[T1,T2])
	),
	fail.

%

symbolic_elts([any,nv,gnd,
	       list_a_fn,list_nv_fn,list_g_fn,list_f_fn,free_nil,
	       list_a_n,list_nv_n,list_g_n,list_f_n,nil,
	       list_a_f,list_nv_f,list_g_f,list_f_f,free,
	       bot]).


norm_dict(D,L) :-
	list_dict(D,L0),
	norm_dict_list(L0,L).

norm_dict_list([],[]).
norm_dict_list([(P,C,S)|Xs],[(P,C1,S1)|Ys]) :-
	norm_pat(C,C0), 
	install_aliasing_tuple(C0,C1),
	norm_pat(S,S0),
	install_aliasing_tuple(S0,S1),
	norm_dict_list(Xs,Ys).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generate all possible types!
% This is useful for testing purposes. Note that we also generate all
% varieties of locality &c.

% generate_tuple(+N,-Tuple)
%
% Create all tuples of arity N.
% Generators:
% - det/nondet
% - generate types for each arg
% - all possible aliasing

generate_tuple(N,Tuple) :-
	( Det = det ; Det = nondet ),
	generate_n_args(N,Xs),
	Tuple =.. [Det|Xs].

generate_n_args(0,[]) :- !.
generate_n_args(N,[X|Xs]) :-
	N > 0,
	M is N-1,
	generate_arg(X),
	generate_args(M,Xs).

% generate_type(-Type)

generate_type(bot).
generate_type(nil).
generate_type(gnd).
generate_type(nv(_P,S,L)) :- some_sharing(S), some_locality(L).
generate_type(any(_P,S,L)) :- some_sharing(S), some_locality(L).
generate_type(free(_C,_P,L)) :- some_locality(L).
generate_type(free_nil(_C,_P,L)) :- some_locality(L).
generate_type(list_g_n).
generate_type(list_nv_n(_P,S,L)) :- some_sharing(S), some_locality(L).
generate_type(list_a_n(_P,S,L)) :- some_sharing(S), some_locality(L).
generate_type(list_f_n(_P,S,L)) :- some_sharing(S), some_locality(L).
generate_type(list_g_fn(_C,_P,L)) :- some_locality(L).
generate_type(list_nv_fn(_P_hd,S_hd,L_hd,_C,_P,L)) :- 
	some_sharing(S_hd), some_locality(L_hd), some_locality(L).
generate_type(list_a_fn(_P_hd,S_hd,L_hd,_C,_P,L)) :- 
	some_sharing(S_hd), some_locality(L_hd), some_locality(L).
generate_type(list_f_fn(_P_hd,S_hd,L_hd,_C,_P,L)) :-
	some_sharing(S_hd), some_locality(L_hd), some_locality(L).
generate_type(list_g_f(_C,_P,L)) :- some_locality(L).
generate_type(list_nv_f(_P_hd,S_hd,L_hd,_C,_P,L)) :- 
	some_sharing(S_hd), some_locality(L_hd), some_locality(L).
generate_type(list_a_f(_P_hd,S_hd,L_hd,_C,_P,L)) :- 
	some_sharing(S_hd), some_locality(L_hd), some_locality(L).
generate_type(list_f_f(_P_hd,S_hd,L_hd,_C,_P,L)) :-
	some_locality(L_hd), some_sharing(S_hd), some_locality(L).

%

some_sharing(S) :-
	make_linear(S); make_indlist(S); make_nonlinear(S).

some_locality(L) :-
	( make_freeze(L) ; true ),
	  make_local(L); make_robust(L); make_wbf(L); make_fragile(L).


