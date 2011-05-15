%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%    DOMAIN-SPECIFIC PROCEDURES FOR ANALYSING RECURSION-PARALLEL PREDICATES
%
%   This file implements the interface to the analyse of recursion-parallel
% predicates.
%
%
% annotate_locality(+CallPattern,+Head,+Rec,+Prologue,+SeqEnv,-ParEnv)
%
%     This procedure simulates the large head unification before parallel
%   execution starts. The purpose is to assign the variables 'shared' or
%   'local' types. It is useful to note that there are four valid cases
%   of head-rec argument pairs:
%
%	Head	Rec	Classification
%	----	----	--------------
%      [X|Xs]	  Xs	POS-LIST	X local if indep args, Xs shared?
%	  Xs   [X|Xs]	NEG-LIST	X local if indep args, Xs shared?
%	X	X	INV		X shared
%	X	Y	NONE-NEG	X, Y shared
%
%   All other combinations mean there is an error in a previous rewrite phase
%   (at least until more kinds of r.p. are implemented).
%
%     The procedure is also supposed to assign types to the variables.
%   Assume <entry> is the type of the argument considered:
%
%   o  If the type is POS-LIST, X gets the head type and Xs the tail type.
%   o  If the type is NEG-LIST, X is free and Xs is "free l u b <entry>"
%   o  If the type is INV, X is <entry>
%   o  If the type is NONE-NEG, then X is "free l u b <entry>", Y is "free".
%
%
% propagate_and_recompute(...)
%
%     Perform binding propagation: after analysis has completed, call pattern
%   may have changed due to bindings from left and right. Compute a new call
%   pattern by taking l u b of 'left call pattern', 'right call pattern' and
%   'original callpat'.
%

:- ensure_loaded('../compilation/rp-varclasses').


annotate_locality(Callpat,Head,Rec,Prologue,OrigEnv,SeqEnv,ParEnv,Par) :-
	Head =.. [_|Xs],
	Rec  =.. [_|Ys],
	Callpat = shr(_Shr,_Inst,Tuple),
	Tuple =.. [_D|Types],
	Types = [T|_],
	( nonparallel_type(T) ->
	    Par = sequential
	; true,
	    Par = parallel,
	    independence_of_args(Types,Inds),
	    annotate_by_prologue(Prologue,OrigEnv,SemiSeqEnv),	  
	    copy_term(SemiSeqEnv,SeqEnv),
	    annotate_locality_args(Xs,Ys,Inds,Types,SemiSeqEnv,ParEnv)
	).

nonparallel_type(T) :-
	norm(T,_,NormT),
	nonparallel_table(NormT).

nonparallel_table(nil).
nonparallel_table(free(_,_,_)).
nonparallel_table(free_nil(_,_,_)).

%%%
%%  Annotating the prologue is done by executing the primitives within it,
% this includes some delimiters $int_rec_{begin,end} and stuff.
%

annotate_by_prologue((G,Gs),Env0,Env2) :- !,
	annotate_prologue_goal(G,Env0,Env1),
	annotate_by_prologue(Gs,Env1,Env2).

annotate_by_prologue(G,Env0,Env1) :-
	annotate_prologue_goal(G,Env0,Env1).

%

annotate_prologue_goal(G,Env0,Env1) :-
	analyse_primitive(G,[],Env0,Env1).



annotate_locality_args([],[],[],[],E,E).
annotate_locality_args([X|Xs],[Y|Ys],[Ind|Inds],[T|Ts],E0,E2) :-
	annotate_rec_locality_arg(X,Y,Ind,T,E0,E1),
	annotate_rest_locality_args(Xs,Ys,Inds,Ts,E1,E2).

%

annotate_rec_locality_arg(X,Y,Ind,Type,E0,E1) :-
	( poslist(X,Y,_) ->
	    Y = '$VAR'(N),
	    rec_poslist_arg(X,N,Type,Ind,E0,E1)
	; none_neg(X,Y,X_id,Y_id) ->
	    rec_none_neg_arg(X_id,Y_id,Type,E0,E1)
	; true,
	    sys_error('annotate_rec_locality_arg/6: recursion type not handled')
        ).

%

annotate_rest_locality_args([],[],[],[],E,E).
annotate_rest_locality_args([X|Xs],[Y|Ys],[Ind|Inds],[T|Ts],E0,E2) :-
	annotate_locality_arg(X,Y,Ind,T,E0,E1),
	annotate_rest_locality_args(Xs,Ys,Inds,Ts,E1,E2).

%

annotate_locality_arg(X,Y,Ind,Type,E0,E1) :-
	( inv(X,Y,X_id) ->
	    inv_arg(X_id,Type,E0,E1)
	; none_neg(X,Y,X_id,Y_id) ->
	    none_neg_arg(X_id,Y_id,Type,E0,E1)
	; neglist(X,Y,_) ->
	    X = '$VAR'(M),
	    neglist_arg(M,Y,Type,Ind,E0,E1)
	; poslist(X,Y,_) ->
	    Y = '$VAR'(N),
	    poslist_arg(X,N,Type,Ind,E0,E1)
	; true,
	    sys_error('annotate_locality_arg/6: noncanonical args ~p - ~p', [X,Y])
	).


%%%
%%  First, here is how list recursion and integer recursion are handled.
% Note that the list recursion MUST be with a list- or bound type, in particular,
% free/free_nil are not allowed.
%
% rec_poslist_arg/6 differs from poslist_arg in that we KNOW the entire list
% has been traversed, and so can set the types more precisely for the tail.
%

rec_poslist_arg(['$VAR'(X)|'$VAR'(Xs)],Xs,Type,Indep,E0,E1) :-
	( list_type(Type,Hd_id,Hd_attr,Tl_id,Tl_attr) ->
	    cnv_lub(Hd_id,T_id),
	    copy_term(Hd_attr,T_attr),
	    linearity_of_attr(T_attr,L),
	    ( linear(L) ->
		true
	    ; indlist(L) ->
	        make_nonlinear(L)
	    ; nonlinear(L) ->
	        shared_attr(Hd_attr), shared_attr(T_attr)
	    ),
	    adjust_attr(Indep,L,Hd_attr,T_attr),
	    simple_type(T,T_id,T_attr),
	    par_init_env(X,T,E0,Ex),
	    cnv_lub(Tl_id,Tl_type_id),
	    simple_type(Tail_type,Tl_type_id,Tl_attr),     % changed here
	    par_init_env(Xs,Tail_type,Ex,E1)
	; simple_bound_type(Type,T_id,T_attr) ->
	    ( T_id = nil ->
		par_init_env(X,gnd,E0,Ex),
		par_init_env(Xs,nil,E0,Ex)                 % changed here
	    ; T_id = gnd ->
	        par_init_env(X,gnd,E0,Ex),
		par_init_env(Xs,nil,E0,Ex)                 % changed here
	    ; ( T_id = nv ; T_id = any ) ->
	        T1_id = any,
		copy_term(T_attr,T1_attr),
		linearity_of_attr(T1_attr,L),
		( linear(L) ->
		    true
		; indlist(L) ->
		    make_nonlinear(L)
		; nonlinear(L) ->
		    shared_attr(T_attr), shared_attr(T1_attr)
		),
		adjust_attr(Indep,L,T_attr,T1_attr),
		simple_type(T1,T1_id,T1_attr),
		simple_type(T2,T1_id,T_attr),
		par_init_env(X,T1,E0,Ex),
		par_init_env(Xs,T2,Ex,E1)
	    )
	; true,
	    sys_error('rec_poslist_arg/6: type ~q disallowed',[Type])
        ).

rec_none_neg_arg(M,N,Type,E0,E1) :-
	( Type = gnd ->
	    true
	; true,
	    warning('% integer recursion called with ~q; unified w. gnd', [Type]),
	    absu(Type,gnd,_)
        ),
	GND = gnd,
	par_init_env(M,GND,E0,Ex),
	par_init_env(N,GND,Ex,E1).


%%%
%%  Annotating non-recursion arguments:
%
%   o  invariants become shared
%
%   o  none-neg		M becomes "free l u b Type" while N is "free"
%
%   o  neglist		a list of free vars ending in Type (which need not be a list)
%
%   o  poslist		a case analysis:
%
%	1. a list, if linear or indlist then local else shared
%	2. a bound type, if linear, then local else shared
%	3. a free type, which becomes a list of free vars, ending in free/free+nil
%

inv_arg(M,Type,E0,E1) :-
	make_shared_type(Type),
	par_init_env(M,Type,E0,E1).

none_neg_arg(M,N,Type,E0,E1) :-
	LUB_FREE = free(_,_,L0),
	make_shared(L0),
	FREE = free(_,_,L1),
	make_shared(L1),
	lub_types(Type,LUB_FREE,LUB),
	par_init_env(M,LUB,E0,Ex),
	par_init_env(N,FREE,Ex,E1).

neglist_arg(Ys,['$VAR'(Y)|'$VAR'(Ys)],Type,_Indep,E0,E1) :-
	FREE = free(_C1,P1,L1), make_local(L1),
	F_attr = a(P1,S1,L1), make_linear(S1),
	par_init_env(Y,FREE,E0,Ex),
	( Type = nil ->
	    list_type(Ys_T,f,F_attr,n,none)
	; Type = free(C,P,L) ->
	    make_shared(L),
	    list_type(Ys_T,f,F_attr,f,b(C,P,L))
	; Type = free_nil(C,P,L) ->
	    make_shared(L),
	    list_type(Ys_T,f,F_attr,f,b(C,P,L))
	; list_type(Type,Hd_id,Hd_attr,Tl_id,Tl_attr) ->
	    locality_of_attr(Hd_attr,Hd_L), make_shared(Hd_L),
	    locality_of_attr(Tl_attr,Tl_L), make_shared(Tl_L),
	    cnv_lub(Hd_id,T_id),
	    simple_type(T,T_id,Hd_attr),
	    lub_types(T,FREE,LUB),
	    simple_type(LUB,LUB_id,LUB_attr),
	    ( LUB_attr = b(_C_lub,P_lub,L_lub) ->
		NewHd_attr = a(P_lub,S_lub,L_lub),
		make_linear(S_lub)
	    ; true,
	        NewHd_attr = LUB_attr
	    ),
	    lub_cnv(LUB_id,NewHd_id),
	    list_type(Ys_T,NewHd_id,NewHd_attr,Tl_id,Tl_attr)
	; true,
	    sys_error('not list-compatible type in neglist arg -- unimplemented')
        ),
	par_init_env(Ys,Ys_T,Ex,E1).

poslist_arg(['$VAR'(X)|'$VAR'(Xs)],Xs,Type,Indep,E0,E1) :-
	( list_type(Type,Hd_id,Hd_attr,_Tl_id,_Tl_attr) ->
	  cnv_lub(Hd_id,T_id),
	  copy_term(Hd_attr,T_attr),
	  linearity_of_attr(T_attr,L),
	  ( linear(L) ->
	      true
	  ; indlist(L) ->
	      make_nonlinear(L)
	  ; nonlinear(L) ->
	      shared_attr(Hd_attr), shared_attr(T_attr)
	  ),
	  adjust_attr(Indep,L,Hd_attr,T_attr),
	  simple_type(T,T_id,T_attr),
	  par_init_env(X,T,E0,Ex),
	  par_init_env(Xs,Type,Ex,E1)
	; simple_free_type(Type,T_id,T_attr) ->
	    ( (T_id = free ; T_id = free_nil) ->
		( Indep = indep ->
		    T = free(_,_,_),
		    Ts = list_f_f(_,_,_,_,_,_),
		    par_init_env(X,T,E0,Ex),
		    par_init_env(Xs,Ts,Ex,E1)
		; Indep = dep ->
		    T_attr = b(_,P,L),
		    T = free(_,P,L),
		    Ts = list_f_f(P,_,L,_,P,L),
		    par_init_env(X,T,E0,Ex),
		    par_init_env(Xs,Ts,Ex,E1)
		)
	    ; true,
	        sys_error('poslist_arg/6: free type ~q not handled',[T_id])
	    )
	; simple_bound_type(Type,T_id,T_attr) ->
	    ( T_id = gnd ->
		par_init_env(X,gnd,E0,Ex),
		par_init_env(Xs,list_g_n,E0,Ex)
	    ; (T_id = nv ; T_id = any) ->
	        T1_id = any,
		copy_term(T_attr,T1_attr),
		linearity_of_attr(T1_attr,L),
		( linear(L) ->
		    true
		; indlist(L) ->
		    make_nonlinear(L)
		; nonlinear(L) ->
		    shared_attr(T_attr), shared_attr(T1_attr)
		),
		adjust_attr(Indep,L,T_attr,T1_attr),
		simple_type(T1,T1_id,T1_attr),
		simple_type(T2,T1_id,T_attr),
		par_init_env(X,T1,E0,Ex),
		par_init_env(Xs,T2,Ex,E1)
	    )
	; true,
	    sys_error('poslist argument matched w. ~q -- unimplemented',[Type])
        ).

%

make_shared_type(T) :-
	( simple_free_type(T,_,T_attr) ->
	    shared_attr(T_attr)
	; simple_bound_type(T,_,T_attr) ->
	    shared_attr(T_attr)
	; list_type(T,_,Hd_attr,_,Tl_attr) ->
	    shared_attr(Hd_attr),
	    shared_attr(Tl_attr)
	).

%

shared_attr(none).
shared_attr(a(_P,_S,L)) :- make_shared(L).
shared_attr(b(_C,_P,L)) :- make_shared(L).

%

par_init_env(M,T,E0,E1) :-
	arg(M,E0,OrigT),
	( OrigT = uninit ->
	    E0 = E1,
	    setarg(M,E0,T)
	; lub_types(OrigT,T,NewT),
	    E0 = E1,
	    setarg(M,E0,NewT)
	).

%%%
%%  Attributes are rejoined if they are dependent. This preserves aliasing et c.
% We rejoin aliasing + locality, linearity is NOT to be rejoined. (Locality is
% perhaps unnecessary, since normalization takes care of that.
%

adjust_attr(dep,_,A1,A2) :- !,
	poss_alias_of_attr(A1,P),
	poss_alias_of_attr(A2,P),
	cert_alias_of_attr(A1,C),
	cert_alias_of_attr(A2,C),
	locality_of_attr(A1,L),
	locality_of_attr(A2,L).

adjust_attr(indep,L,A1,A2) :-
	( nonlinear(L) ->
	    adjust_attr(dep,L,A1,A2) ; true ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%   propagate_and_recompute(+Side,+CallPattern,+SuccessPattern,+Environment,
%                           +Head,+RecursiveCall,+BodyCall,+/-Memo,+/-Memo,
%                           +/-WorkList,?Mode)
%
%   Propagation is the process of surrounding recursion levels instantiating
% the current one.
%
%   Perform binding propagation: after analysis has completed, call pattern may
% have changed due to bindings from left and right. Compute a new call pattern
% by taking l u b of the left, right and original call patterns.
%   If the new call pattern is not included in that found in the memo tables,
% the parent is marked for recomputation.
%
% The new version of propagate_and_recompute/14:
%
% (a) construct left and right _environments_ by propagation.
% (b) approximate a new environment from old, left and right.
% (c) construct a new call pattern using the new environment.
% (d) check if recomputation is to be done.
%
% Note: (a)-(c) are done in propagate_bindings/8.
%

propagate_and_recompute(LR,_Cp,Sp,Env,Head,Rec,BodyCall,M0,M1,P0,P1,WL0,WL1,Mode) :-
	analyse_mode(Mode,parallel),
	propagate_bindings(LR,Sp,Env,Head,Rec,BodyCall,NewCp),
	functor(BodyCall,P,N),
	PN = P/N,
	lookup_cp_sp(Mode,PN,M0,P0,OldCp,_OldSp),
	( less_than(NewCp,OldCp) ->
	    M0 = M1,
	    P0 = P1, WL0 = WL1
	; true,
	    analyse_mode(Temp,sequential),
	    recompute(PN,Temp,WL0,WLx), %%% P/N is entire rp predicate!
	    recompute(PN,Mode,WLx,WL1),
	    lub_tuples(NewCp,OldCp,Cp),
	    update_cp_sp(Mode,PN,Cp,Sp,M0,M1,P0,P1),
	    functor(Head,H,A),
	    propagate_freeze_violation(Cp,H/A,Mode),
	    propagate_freeze_violation(Sp,H/A,Mode)
	).



%%%
%%  Propagate bindings to the right and left, yielding two environments.
% Apply these environments to the recursion-parallel body call, yielding two
% new call patterns. At this point, the patterns are merged into one. This call
% pattern summarizes the _possible_ bindings when a process starts. If it is
% "greater than" the previous call pattern, the body must be recomputed.
%
% The new version:
%
% (a) construct new environments, reflecting bindings arriving from left and
%     right as other processes execute.
%
% (b) construct a new environment by considering the left, the old and the new
%     environment.
%
% (c) apply the new i-state to the body call.
%
%   We construct two new environments, from propagating "from 1 to N" and
% "from N to 1" in the parallel loop. Depending on whether this is the left or
% right body, the LeftEnv and RightEnv are set appropriately.
%
%   Moreover, a combined environment is constructed (as specified in propagate_env/5).
% The combined PropEnv is used to construct a new call pattern, which is the
% result of the computation.
%

propagate_bindings(LR,Sp,OrigEnv,Head,Rec,BodyCall,PropCp) :-
	copy_term(OrigEnv,CopyEnv),
	update_i_state(BodyCall,Sp,CopyEnv,PostEnv),
	empty_env_copy(CopyEnv,E_fwd),
	copy_term(E_fwd,E_bwd),
	note(3,'% constructing (1 -> N) cp~n'),
	apply_i_state(PostEnv,Rec,ForwardRecCp),
	copy_term(ForwardRecCp,FwdCp),
	initial_i_state(((Head :- true),E_fwd),FwdCp,_,_,FwdE),
	note(3,'% constructing (N -> 1) cp~n'),
        final_i_state(PostEnv,Head,BwdSuccpat),
	copy_term(BwdSuccpat,BwdCp),
	update_i_state(Rec,BwdCp,E_bwd,BwdE),
	( LR == left ->
	    LeftEnv = FwdE,
	    RightEnv = BwdE
	; LR == right ->
	    LeftEnv = BwdE,
	    RightEnv = FwdE
	),
	propagate_env(LeftEnv,OrigEnv,PostEnv,RightEnv,PropEnv),
	apply_i_state(PropEnv,BodyCall,PropCp).


%%%
%%
%
propagate_freeze_violation(Pattern,PN,Mode) :-
	Pattern = shr(_Shr,_Inst,Tuple),
	Tuple =.. [_D|Types],
	propagate_freeze_violation_types(Types,PN,Mode).


propagate_freeze_violation_types(    [],_PN,_Mode).

propagate_freeze_violation_types([T|Ts], PN, Mode) :-
	norm(T,_,NormT),
	locality_of_type(NormT,LOT),
	( freeze(LOT) ->
	    ( local(LOT) ->
		propagate_freeze_violation_types(Ts,PN,Mode)
	    ; true,
	        compile_mode(Mode,sequential),
		warning('detected violation to freeze conditions on shared types in ~q',[PN])
	    )
	; true,
	    propagate_freeze_violation_types(Ts,PN,Mode)
	).


empty_env_copy(OrigEnv,EmptyEnv) :-
	components_of_env(OrigEnv,_,OrigVs),
	length(OrigVs,N),
	fill_list(N,Vs),
	construct_env(Vs,EmptyEnv).


%%%
%%  propagate_env/5 combines the three environments into one that takes all valid
% bindings into account. An utterly simplistic method would be to take the l u b
% of the three for every variable entry (this method is correct but somewhat
% uninformative). Our method of choice uses the locality information in order
% to disregard some entries.
%

propagate_env(LeftE,PreEnv,PostEnv,RightE,PropEnv) :-
	components_of_env(PreEnv,EnvState,PreEnvVars),
	components_of_env(PostEnv,_,PostEnvVars),
	components_of_env(LeftE,_,LeftVars),
	components_of_env(RightE,_,RightVars),
	propagate_vars(LeftVars,PreEnvVars,PostEnvVars,RightVars,PropVars),
	construct_environment(EnvState,PropVars,PropEnv). % hopefully OK state

propagate_vars([],[],[],[],[]).
propagate_vars([L0|Ls],[PreX0|PreXs],[PostX0|PostXs],[R0|Rs],[P|Ps]) :-
	uninit_becomes_free(L0,PreX0,PostX0,R0,L1,PreX1,PostX1,R1),
	propagate_var(L1,PreX1,PostX1,R1,P),
	propagate_vars(Ls,PreXs,PostXs,Rs,Ps).


%%%
%%  Propagation per variable:
%
%   o  The arguments are from left environment, before body, after body
%      and the right environment.
%
%   o  If the variable is local, unchanged by propagation.
%
%   o  If the variable is fragile before or after the call, ignore the right
%      environment.
%
%   o  Otherwise, use the left, before and right environment.
%
% Note: PostX is only used to check for fragility.
%

propagate_var(L,PreX,PostX,R,P) :-
	wbf_becomes_fragile(L),
	wbf_becomes_fragile(PreX),
	wbf_becomes_fragile(PostX),
	wbf_becomes_fragile(R),
	( local_type(PreX) ->
	    P = PreX
	; fragile_type(PostX) ->
	    lub_types(L,PreX,P)
	; fragile_type(PostX) ->
	    lub_types(L,PreX,P)
	; fragile_type(L) ->
	    lub_types(L,PreX,P)
	; lub_types(L,PreX,Tmp),
	    lub_types(Tmp,R,P)
	).


% NOTE: To avoid some problems, here is an implementation dependent thing:
%       - "uninit" variables become "free". (They are shorthand for free
%         variables appearing for the first time.)
%
uninit_becomes_free(L0,X0,PostX0,R0,L1,X1,PostX1,R1) :-
	( L0 = uninit ->
	  L1 = free(_,_,_)
	; L0 = L1
	),
	( X0 = uninit ->
	  X1 = free(_,_,_)
	; X0 = X1
	),
	( PostX0 = uninit ->
	  PostX1 = free(_,_,_)
	; PostX0 = PostX1
	),
	( R0 = uninit ->
	  R1 = free(_,_,_)
	; R0 = R1
	).


%

future_bindings_prohibited(Orig,Right) :-
	( fragile_type(Orig) ; fragile_type(Right) ), !.

%

fragile_type(T) :-
	( simple_type(T,_,T_attr) ->
	    fragile_attr(T_attr)
	; list_type(T,_,Hd_attr,_,Tl_attr) ->
	    ( fragile_attr(Hd_attr) ; fragile_attr(Tl_attr) ), !
	).

fragile_attr(a(_,_,L)) :- fragile(L).
fragile_attr(b(_,_,L)) :- fragile(L).

%

wbf_becomes_fragile(T) :-
	( simple_type(T,_,T_attr) ->
	    wbf_to_fragile_attr(T_attr)
	; list_type(T,_,Hd_attr,_,Tl_attr) ->
	    wbf_to_fragile_attr(Hd_attr),
	    wbf_to_fragile_attr(Tl_attr)
	).

wbf_to_fragile_attr(none).

wbf_to_fragile_attr(a(_,_,L)) :- 
	( wbf(L) ->
	    make_fragile(L) ; true ).

wbf_to_fragile_attr(b(_,_,L)) :- 
	( wbf(L) ->
	    make_fragile(L) ; true ).

%

local_type(T) :-
	( simple_type(T,_,T_attr) ->
	    local_attr(T_attr)
	; list_type(T,_,Hd_attr,_,Tl_attr) ->
	    local_attr(Hd_attr), local_attr(Tl_attr)
	).

local_attr(none).
local_attr(a(_,_,L)) :- local(L).
local_attr(b(_,_,L)) :- local(L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%   To make the sequential computation proceed, perform a simple form of
% 'propagation' to simulate the loop-character of the computation.
%
% The following cases are used: (assume H :- LB, R, RB)
%
% 1. When the left body is finished, a recursive call would follow in standard
%    sequential execution. We construct the appropriate call pattern of this
%    recursive call and see if the predicate must be recomputed.
%
% 2. When the left body is finished, the call returns under the standard
%    sequential regime. In our case, we construct a new success pattern and
%    see if the predicate must be recomputed. Thus, recomputation of the right
%    body are driven by the recursive call yielding better and better types.
%

sequential_propagate_and_recompute(LR,SeqSp,Env,H,R,BodyGoal,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Mode) :-
	analyse_mode(Mode,sequential),
        copy_term(Env,CopyEnv),
	update_i_state(BodyGoal,SeqSp,CopyEnv,NewEnv),
	( LR == left ->
	    %%% assume no failed computation -- dangerous?
	    apply_i_state(NewEnv,R,NewCp),
	    %%% the below marks par pred for recompute if needed.
	    callpat_computed(Mode,R,NewCp,_Sp,M0,M1,P0,P1,_Computed,CDB,CG,U0,U1,WL0,WL1)
	; LR == right ->
	    final_i_state(NewEnv,H,TmpSp),
	    ( TmpSp = shr(S,_I,T) -> 
	        functor(T,_,N), max_inst(N,Inst), Sp = shr(S,Inst,T)
	    ; true,
	        sys_error('sequential_propagate_and_recompute/16: final sp not on shr form')
	    ),
	    U0 = U1,
	    update_succpat(Mode,H,Sp,M0,M1,P0,P1,CG,WL0,WL1)
	; true,
	    sys_error('sequential_propagate_and_recompute/16: ~q must be left or right',[LR])
        ).

