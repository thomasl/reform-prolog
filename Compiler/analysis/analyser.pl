%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	  ABSTRACT INTERPRETATION WITH A SINGLE CALL PATTERN
%
% This file implements a simplified version of Debray's algorithm for
% dataflow analysis of logic programs. In particular, while Debray
% maintains n callpatterns, with m_i success patterns per call pattern i,
% this analyser maintains one call pattern and one success pattern per
% predicate.
%
% We have a memo table M of entries P/N -> (Cp,Sp) with Sp being
% 'bot' for predicates that fail etc.
%
% The clause database holds entries as follows:
%   P/N -> (ClausesEnvs,Directives,File,Callers)
%    where
%    ClausesEnvs are (Clause,VarEnv) pairs.
%
% (As shown here, the call graph is a separate argument.)
%
% This analyser also supports
% - Tracking calls to undefined predicates.
% - Using a stack of ancestors for runtime messages (e.g. type violations
%   of primops).
% - Recursion-parallel predicates with 'always' and 'eventual' properties
% - Abstract indexing
% - Behaving in different ways depending on execution mode
%   (sequential, arbitrary in parallel -- i.e. rec.pred.)
%
% The analyser can be in the following modes:
% - sequential (standard left-to-right Prolog execution)
% - parallel   (parallel execution, other processes may precede)

:- ensure_loaded('../preprocess/callgraph').
:- ensure_loaded('../preprocess/load-file').
:- ensure_loaded('../preprocess/load-rp-file').

:- ensure_loaded(annotate).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pre-analysis stuff:
% - load program
% - construct call graph
% - insert given call patterns
% Then do the analysis.

analyse_files(Files,Exports,Memo,ParMemo) :-
	fload_desugar_program(Files,CDB),
	construct_cg_of_ixstructs(CDB,CG),
	empty_memotable(StartMemo),
	empty_memotable(StartParMemo),
	empty_memotable(U0),
	analyse_program(Exports,StartMemo,Memo,StartParMemo,ParMemo,CDB,CG,U0,_U1).

analyse_files_and_annotate(Files,Exports,CDB_out,Undefs) :-
	fload_desugar_program(Files,CDB),
	construct_cg_of_ixstructs(CDB,CG),
	empty_memotable(StartMemo),
	empty_memotable(StartParMemo),
	empty_memotable(U0),
	analyse_program(Exports,StartMemo,Memo,StartParMemo,ParMemo,CDB,CG,U0,Undefs),
	annotate_program(CDB,Memo,ParMemo,CDB_out).

analyse_and_time(Files,Exports,Memo,ParMemo) :-
	statistics(runtime,_),
	fload_desugar_program(Files,CDB),
	statistics(runtime,[_,LoadTime]),
	construct_cg_of_ixstructs(CDB,CG),
	statistics(runtime,[_,CGTime]),
	empty_memotable(StartMemo),
	empty_memotable(StartParMemo),
	empty_memotable(U0),
	analyse_program(Exports,StartMemo,Memo,StartParMemo,ParMemo,CDB,CG,U0,_U1),
	statistics(runtime,[_,AnalysisTime]),
	Total is LoadTime + CGTime + AnalysisTime,
	format('%     loaded file(s) in ~q ms~n',[LoadTime]),
	format('% created call graph in ~q ms~n',[CGTime]),
	format('%   analysed program in ~q ms~n',[AnalysisTime]),
        format('%            total time ~q ms~n',[Total]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Analysing a program.
%
%   This procedure drives the analysis forward. Each iteration starts with
% a list of P/N items, that represent work to be done. If this list is empty,
% the analysis is finished. If the list is non-empty, the items are analysed
% (by a ..._worklist), that returns a new worklist which is submitted to
% this procedure again.
%
%   The first iteration is different, since we use export-declarations
% (presupplied call patterns) rather than memo-table key-functors.
%
analyse_program([],M,M,P,P,_CDB,_CG,U,U) :-
	warning('no analysis entry points found').

analyse_program(Work,M0,M2,P0,P2,CDB,CG,U0,U2) :-
	note(2,'% analysing worklist ~q~n',[Work]),
        iteration_info(Work,1,M0,P0),
	analyse_first_worklist(Work,M0,M1,P0,P1,CDB,CG,U0,U1,WL,[]),
	sort(WL,SortedWL),
	note(2,'% analysing worklist ~q~n',[SortedWL]),
        Iter = 2,
	analyse_program_iterate(SortedWL,Iter,M1,M2,P1,P2,CDB,CG,U1,U2).


analyse_program_iterate([],Iter,M0,M1,P0,P1,_CDB,_CG,U0,U1) :- !,
	iteration_info([],Iter,M0,P0),
	M0 = M1,
	P0 = P1,
	U0 = U1.

analyse_program_iterate(Work,Iter,M0,M2,P0,P2,CDB,CG,U0,U2) :-
	iteration_info(Work,Iter,M0,P0),
	analyse_worklist(Work,M0,M1,P0,P1,CDB,CG,U0,U1,WL,[]),
	sort(WL,SortedWL),
	note(2,'% analysing worklist ~q~n',[SortedWL]),
        NextIter is Iter+1,
	analyse_program_iterate(SortedWL,NextIter,M1,M2,P1,P2,CDB,CG,U1,U2).


% Analysing the work list is done only for the purpose of obtaining
% a new memo table M2; hence, the resulting Sp of analyse_predicate is
% ignored here.
%
% Each item of work is a sequential(P/N) or parallel(P/N) structure.
% The functor tells us what mode to use, the argument what predicate to
% analyse.

analyse_worklist([],M,M,P,P,_CDB,_CG,U,U,WL,WL).

analyse_worklist([WorkItem|Ps],M0,M2,P0,P2,CDB,CG,U0,U2,WL0,WL2) :-
	analyse_work_item(WorkItem,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1),
	analyse_worklist(Ps,M1,M2,P1,P2,CDB,CG,U1,U2,WL1,WL2).


% Predicates to be recomputed. The predicates can be either
% standard predicates called in a parallel or sequential context,
% which is handled straightforwardly; or they can be the left or
% right bodies of parallel calls.
%
% If a predicate has call pattern 'bot', it is never called. It
% has been marked for recomputation due to a called predicate changing
% success pattern, but there is nothing to be done.
%
% Example:
%  :- export(p/0).
%  p :- r.
%  q :- r.
%  r :- ...
% 
% When r/0 changes succpat, q/0 will appear in the worklist, but
% actually hasn't been called. So, q/0 has Cp = bot and is ignored.
%
% (If q/0 is called later, it will THEN be computed normally, when
% there is a proper callpat.)
%
% This measure also prevents an r.p. call from being recomputed in
% parallel due to a body recomputing all callers when par. cp. changes.

analyse_work_item(parallel(CompileMode,P/N),M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1) :-
	Mode = mode(parallel,CompileMode),
	functor(H,P,N),
	lookup_cp_sp(Mode,P/N,M0,P0,Cp,_AnySp),
	( Cp == bot -> %%% not called, so ignore
	    M0 = M1,
	    P0 = P1,
	    U0 = U1, WL0 = WL1
	; true,
	    Anc = [P/N],
	    analyse_clauses_of_predicate(H,Cp,_Sp,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode)
        ).

analyse_work_item(sequential(CompileMode,P/N),M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1) :-
	Mode = mode(sequential,CompileMode),
	functor(H,P,N),
	lookup_cp_sp(Mode,P/N,M0,P0,Cp,_AnySp),
	( Cp == bot -> %%% not called, so ignore
	    M0 = M1,
	    P0 = P1,
	    U0 = U1, WL0 = WL1
	; true,
	    Anc = [P/N],
	    analyse_clauses_of_predicate(H,Cp,_Sp,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode)
        ).

% The first iteration is done using exports rather than P/N:s.
% Note that the first arg to a_pred is "Cp", which is then used
% to find all clauses. This can be done since Cp has same functor+arity
% as concrete predicate.

analyse_first_worklist([],M,M,P,P,_CDB,_CG,U,U,WL,WL).

analyse_first_worklist([P/N=Cp|Cps],M0,M2,P0,P2,CDB,CG,U0,U2,WL0,WL2) :-
	Anc = [], 
	functor(G,P,N),
	analyse_mode(Mode,sequential),
	update_cp_sp(Mode,P/N,Cp,bot,M0,Mx,P0,Px),
	analyse_clauses_of_predicate(G,Cp,_Sp,Mx,M1,Px,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode),
	analyse_first_worklist(Cps,M1,M2,P1,P2,CDB,CG,U1,U2,WL1,WL2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Main (loop) predicates during analysis.
%
% - analyse_clauses_of_predicate (analyse_predicate without call pattern checking)
% - analyse_predicate
% - analyse_clauses
% - analyse_clause
% - analyse_body
% - analyse_goal
% - analyse_rp_predicate
%
%   All predicates above are mutually recursive. The general parameter layout
% and naming convention is as described bellow (with indexing, i >= 0):
%
%   <Mi>	the memo table containing call and success patterns produced
%		during sequential analysis
%
%   <Pi>	the memo table containing call and success patterns produced
%		during parallel analysis
%
%   <CDB>	the Clause DataBase
%
%   <CG>	the Call Graph describing the program (P/N -> callers)
%
%   <Ui>	the memo table containing call and success patterns produced by
%		attempts to analyse undefined predicates.
%
%   <WLi>	the Work List, recording predicates to be analysed (represented
%		as a d-list). It is returned to the driver routine above, which
%		terminates if the WL is empty and does another iteration otherwise.
%
%   Analysis of recursion parallel predicates is essentially invoking the analyser
% recursively, with some changes. See below. It is _not_ really a part of the rest
% of the mutual recursion.
%
analyse_clauses_of_predicate(G,Cp,Sp,M1,M2,P1,P2,CDB,CG,U1,U2,WL1,WL2,Anc,Mode) :-
	functor(G,P,N),
	note(2,'% *** analysing predicate ~q (in ~q mode)~n',[P/N, Mode]),
        ( clause_ix_propagate(Mode,G,CDB,IxStr) ->
	    ( parallel_predicate(Mode,G,IxStr) ->
	        analyse_rp_predicate(IxStr,Cp,Sp,M1,M2,P1,P2,CDB,CG,U1,U2,WL1,WL2,Anc,Mode)
	    ; absindex(Cp,IxStr,CsCpsEs) ->
	        analyse_clauses(CsCpsEs,bot,Sp,M1,M2,P1,P2,CDB,CG,U1,U2,WL1,WL2,[P/N|Anc],Mode)
	    )
	; true,
            notify_undefined_call(G,Anc),
	    worst_succpat(Cp,Sp,Anc,U1,U2),
	    warning('assuming succpat ~p',['$pat'(Sp)]),
	    update_succpat_no_recompute(Mode,G,Sp,M1,M2,P1,P2,CG,WL1,WL2)
	),
	note(2,'% *** analysis of ~q done~n',[P/N]).


% - Check if call pattern already is computed; if so, return it.
% - Otherwise, collect the clauses etc. of the predicate and
%   compute the success pattern the hard way.
%
% NB. 'bot' is the 'unit' of least upper bound of success patterns, s.t.
%     X $lub bot = bot $lub X = X for all X.
%
analyse_predicate(G,Cp,Sp,M0,M2,P0,P2,CDB,CG,U0,U2,WL0,WL2,Anc,Mode) :-
	callpat_computed(Mode,G,Cp,Sp0,M0,M1,P0,P1,Computed,CDB,CG,U0,U1,WL0,WL1),
	( Computed == no ->
	    analyse_clauses_of_predicate(G,Cp,Sp,M1,M2,P1,P2,CDB,CG,U1,U2,WL1,WL2,Anc,Mode)
        ; Computed == yes ->
	    Sp = Sp0,
	    M1 = M2,
	    P1 = P2,
	    U0 = U2, WL1 = WL2
	).

clause_ix_propagate(Mode,Goal,CDB,IxStruct) :-
	compile_mode(Mode,CompileMode),
	clauses_and_envs(Goal,CDB,info(CompileMode,IxStruct,_Directives,_File)).

%   Check if this predicate is a parallel one, invoked in a sequential context.
% If this is the case, return the indexing structure, otherwise fail.
%
parallel_predicate(Mode,Goal,rp(_Base,_ParRec,_SeqRec,_Type,_LB,_RB)) :-
	( analyse_mode(Mode,parallel) ->
	    functor(Goal,P,N),
	    error('parallel predicate ~q invoked in parallel context',[P/N])
	; true ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Analysis of recursion-parallel predicates.
%
% This is accompilished in several steps:
% 
% 0. Select base clause and Rec = H :- LP, LB, R, RB, RP.
% 1. Annotate locality using H, R and LP.
% 2. Analyse LB (fixpt iteration for par & seq types)
% 3. All data become local & recursive clause is analysed
%    (also, here we really use the tail of arg 1! list_g_n => nil, etc)
% 4. Now reannotate for locality, using H, R and RP.
% 5. Analyse RB (same as 2)
% 6. Remove locality marks and construct success pattern; exit.
%
% *** UNFINISHED ***
% The current analyser doesn't handle reductions, integer recursion or
% calls where the first arg is not a list. Baad. This should be done
% with abstract indexing at least.
%
analyse_rp_predicate(IxStr,OrigCp,Sp,M0,M4,P0,P4,CDB,CG,U0,U3,WL0,WL4,Anc,Mode) :-
        IxStr = rp(Base,((H :- rec(LP,LB,R,RB,RP)),R_env),_SeqRec,_Type,_LB,_RB),
        analyse_mode(Mode,sequential),  %%% Ensure rp-predicate invoked in sequential context.
	copy_term(OrigCp,Cp),
	copy_term(OrigCp,BaseCp),

	functor(H,P,N),
	note(3,'% *** analysing parallel call ~q~n',[P/N]),
	%%% Simplified abstract indexing: first base case, then recursive case.
        %%%
	analyse_clause((BaseCp,Base),SpBase,M0,Mx,P0,Px,CDB,CG,U0,Ux,WL0,WLx,Anc,Mode),

	%%% ... the recursive case
        %%%
	initial_rp_i_state(H,R_env,Cp,E0),

	( LB == none -> %%% "predicate has no left body"
	    E0 = E1, Mx = M1, Px = P1, Ux = U1, WLx = WL1
	; true,
	    CpLeft = Cp,
	    analyse_rp_body(left,CpLeft,LB,E0,E1,H,R,LP,Mx,M1,Px,P1,CDB,CG,Ux,U1,WLx,WL1,Anc,Mode)
	),
	analyse_rp_base_clause(R,Base,E1,E2,M1,M2,P1,P2,CDB,CG,U1,U2,WL1,WL2,Anc,Mode),

        ( RB == none -> %%% "predicate has no right body"
	    E2 = E3, M2 = M3, P2 = P3, U2 = U3, WL2 = WL3
	; true,
	    apply_i_state(E2,H,CpRight),
	    analyse_rp_body(right,CpRight,RB,E2,E3,H,R,RP,M2,M3,P2,P3,CDB,CG,U2,U3,WL2,WL3,Anc,Mode)
	),
	final_i_state(E3,H,SpRec),

	%%% Finally we join the results together.
        %%%
	lub_tuples(SpBase,SpRec,Sp),
	update_succpat(Mode,H,Sp,M3,M4,P3,P4,CG,WL3,WL4),
	note(3,'% *** analysis of ~q done~n',[P/N]).


% Construct initial i-state for recursion-parallel clause.

initial_rp_i_state(H,Env,Cp,NewEnv) :-
	initial_i_state(((H :- true),Env),Cp,_,_,NewEnv).


% Analysing RP body (this is always a single call)
%
% First analyse the body in parallel:
% - Annotate types w. locality information + simulate head unify
%   (annotate_locality)
% - Compute call pattern to body goal
% - Analyse body in parallel
% - Compute effects by surrounding processes (propagate_and_recompute)
%
% Then analyse the body sequentially:
% - Compute sequential call pattern
% - Analyse call in sequential mode
% - Compute final effects on env.
%
% Notes: 
% (1) parallel analysis has no effect on env: it merely
%     updates the memo tables.
% (2) sequential analysis is precisely standard analysis
%     and reflects what must hold AFTER all processes have
%     terminated.
% (3) we assume that the body goal has a definition in the
%     clause database, which did not hold previously.

analyse_rp_body(LR,Cp,BodyGoal,E0,E1,H,R,Prol,M0,M4,P0,P4,CDB,CG,U0,U4,WL0,WL4,Anc,Mode) :-
	note(4,'%%% analysing ~q body~n',[LR]),
        copy_term((Cp,E0),(AnnotCp,AnnotEnv)),
        annotate_locality(AnnotCp,H,R,Prol,AnnotEnv,SeqEnv,ParEnv,Par),
	( Par == parallel ->
	    apply_i_state(ParEnv,BodyGoal,ParCp),
	    analyse_mode(ParMode, parallel),
	    analyse_predicate(BodyGoal,ParCp,ParSp,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,ParMode),
	    propagate_compile_mode(Mode,ParMode),
	    propagate_and_recompute(LR,ParCp,ParSp,ParEnv,H,R,BodyGoal,M1,M2,P1,P2,WL1,WL2,ParMode),

	    U1 = U2, %%% "just to keep in step w. others"
	    apply_i_state(SeqEnv,BodyGoal,SeqCp),
	    analyse_mode(SeqMode, sequential),
	    analyse_predicate(BodyGoal,SeqCp,SeqSp,M2,M3,P2,P3,CDB,CG,U2,U3,WL2,WL3,Anc,SeqMode),
	    propagate_compile_mode(Mode,SeqMode),
	    sequential_propagate_and_recompute(LR,SeqSp,SeqEnv,H,R,BodyGoal,M3,M4,P3,P4,CDB,CG,U3,U4,WL3,WL4,SeqMode),
	    update_i_state(BodyGoal,SeqSp,SeqEnv,E1)

	; Par == sequential -> %%% "not executed in parallel"
	    E0 = E1,
	    M0 = M4,
	    P0 = P4,
	    U0 = U4, WL0 = WL4

	; var(Par) ->
	    error('bug in annotate_locality: must set par~n')
	).

% Analysis of base case. This is done using sequential types.

analyse_rp_base_clause(Call,BaseCE,Env,NewEnv,M0,M2,P0,P2,CDB,CG,U0,U2,WL0,WL2,Anc,Mode) :-
        analyse_mode(Mode,sequential),
        apply_i_state(Env,Call,Cp),
	callpat_computed(Mode,Call,Cp,Sp0,M0,M1,P0,P1,Computed,CDB,CG,U0,U1,WL0,WL1),
	( Computed == no ->
	    analyse_clause((Cp,BaseCE),Sp,M1,M2,P1,P2,CDB,CG,U1,U2,WL1,WL2,Anc,Mode)
	; Computed == yes ->
	    Sp = Sp0,
	    M1 = M2,
	    P1 = P2,
	    U1 = U2, WL1 = WL2
	),
	update_i_state(Call,Sp,Env,NewEnv).


% analyse_clauses(ClauseEnvPair,CallPat,DefaultSuccPat,FinalSuccPat,
%   MemoIn,MemoOut,ParMemoIn,ParMemoOut,SeqMemoIn,SeqMemoOut,
%   ClauseDB,CallGraph)
%
% Analyse each clause and summarize the result by least upper bound.
%
% NB. Call pattern is given by abstract indexing and is 'hidden' in CE.

analyse_clauses([],Sp,Sp,M,M,P,P,_CDB,_CG,U,U,WL,WL,_Anc,_Mode).

analyse_clauses([CE|CEs],CurrSp,Sp,M0,M2,P0,P2,CDB,CG,U0,U2,WL0,WL2,Anc,Mode) :-
	analyse_clause(CE,ClauseSp,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode),
	lub_tuples(CurrSp,ClauseSp,NextSp),
	analyse_clauses(CEs,NextSp,Sp,M1,M2,P1,P2,CDB,CG,U1,U2,WL1,WL2,Anc,Mode).


% Analysing a single clause:
% - Call pattern is Cp, given by abstract indexing.
% - create an initial environment, pick out the head and body (all from
%   the CE (clause+env) given)
% - analyse the body
% - create a success pattern
% - update the memo table to account for success pattern + recompute all
%   callers if the new result differs.

analyse_clause((Cp,CE),Sp,M0,M2,P0,P2,CDB,CG,U0,U1,WL0,WL2,Anc,Mode) :-
	initial_i_state(CE,Cp,H,B,A0),
        notify_cls('%  analysing clause ~q~n% with ~p~n',(H :- B),'$pat'(Cp)),
	analyse_body(B,Sp0,A0,An,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode),
	( Sp0 == bot ->
	    Sp = bot,
	    M1 = M2,
	    P1 = P2, WL1 = WL2
	; true,
	    final_i_state(An,H,Sp),
	    update_succpat(Mode,H,Sp,M1,M2,P1,P2,CG,WL1,WL2)
	).

%

notify_cls(Msg,Cls0,Cp) :-
	copy_term(Cls0,Cls),
	enumeratevars(Cls,0,_),
	note(6,Msg,[Cls,Cp]).

% Analyse each goal in turn; if goal fails (only for user-defineds
% right now) then entire body fails. This is preferrable, since
% the memo table won't get cluttered with useless info on later calls.

analyse_body((G,Gs),BodySp,A0,A2,M0,M2,P0,P2,CDB,CG,U0,U2,WL0,WL2,Anc,Mode) :- !,
	analyse_goal(G,Sp,A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode),
	( Sp == bot -> 
	    BodySp = bot,
	    M1 = M2,
	    P1 = P2,
	    U1 = U2, WL1 = WL2
	; true,
	    analyse_body(Gs,BodySp,A1,A2,M1,M2,P1,P2,CDB,CG,U1,U2,WL1,WL2,Anc,Mode)
	).

analyse_body(G,Sp,A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode) :-
	analyse_goal(G,Sp,A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode).

% Analysis of a single goal.
% - if the goal is a primitive, simply call the appropriate operation
%   as given by the domain.
% - check for whether the goal is in specialization cache; if so,
%   redirect call to that version instead of original goal.
% - if not in cache, but should be specialized anyway, introduce a
%   new predicate (a duplicate) and redirect call.
% - otherwise, it is a user-defined goal. Create a call pattern,
%   call the predicate; on returning, update and return the new state.

analyse_goal(Goal,Sp,A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode) :-
	( primitive(Goal) ->
	    M0 = M1,
	    P0 = P1,
	    U0 = U1, WL0 = WL1,
	    Sp = prim_succ, %%% used as "not bottom"
	    analyse_primitive(Goal,Anc,A0,A1,Mode)
	; meta_primitive(Goal) ->
	    Sp = prim_succ, %%% used as "not bottom"
	    analyse_meta_primitive(Goal,A0,A1,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode)
	; true,
	    apply_i_state(A0,Goal,Cp),
	    analyse_predicate(Goal,Cp,Sp,M0,M1,P0,P1,CDB,CG,U0,U1,WL0,WL1,Anc,Mode),
	    update_i_state(Goal,Sp,A0,A1)
	).


% callpat_computed(+Goal,+Cp,-Sp,+M0,-M1,+CDB,+CG,WL0-WL1 (d-list))
%  M1 is always bound by callpat_computed, while
%  Sp is instantiated with the success pattern if any,
%   or uninstantiated otherwise.
%
% If call pattern changes, recompute P/N. This is done by e.g. Van Roy.
%

callpat_computed(Mode,G,Cp,Sp,M0,M1,P0,P1,Computed,_CDB,_CG,U0,U1,WL0,WL1) :-
	U0 = U1,
	functor(G,P,N),
	lookup_cp_sp(Mode,P/N,M0,P0,OrigCp,OrigSp),
	( less_than(Cp,OrigCp) ->
	    note(6,'%  ~p found in memo table~n',[P/N]),
            Computed = yes,
	    Sp = OrigSp,
	    M0 = M1,
	    P0 = P1, WL0 = WL1
	; true,
	    Computed = no,
	    Sp = bot,
	    lub_tuples(Cp,OrigCp,NewCp),
	    note(4,'%   updated ~q callpat of ~q to ~p~n% (new ~p, old ~p)~n',
	           [Mode,P/N,'$pat'(NewCp),'$pat'(Cp),'$pat'(OrigCp)]),
	    update_cp_sp(Mode,P/N,NewCp,Sp,M0,M1,P0,P1),
	    recompute(P/N,Mode,WL0,WL1)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Schedule callers for recomputation.
%
%  Two possible modes of recomputation exist:
%
% sequential(CompileMode,P/N)  P/N is to be recomputed in sequential mode,
%                              CompileMode is equivalent with "sequential".
%
% parallel(CompileMode,P/N)    P/N is to be recomputed in parallel mode,
%                              CompileMode is either "parallel" or "sequential.
%
%
% o recompute_callers(+PN,+CG,+Mode,WL0,WL1)
%
%   Extend the WorkList with additional entries, i.e. a list of callers, in order
% to schedule recomputation (analysis). Propagate CompileMode tracking through
% separate computational (itteration) levels.
%
% o recompute(+P/N,+Mode,WL0,WL1) Add P/N for recomputation
%
%   Extend the WorkList with a single entry (caller) and propagte CompileMode
% tracking to recomputation (next level in analysis).
%
%
recompute_callers(P/N,CG,Mode,WL0,WL1) :-
	( lookup_cg(P/N,CG,Callers) -> true ; Callers = [] ),
	add_callers_to_worklist(Callers,Mode,WL0,WL1).


add_callers_to_worklist([],_Mode) --> [].

add_callers_to_worklist([X|Xs],Mode) -->
	recompute(X,Mode),
	add_callers_to_worklist(Xs,Mode).


recompute(PN,mode(AnalyseMode,CompileMode)) --> [Recomp],
	{ Recomp =.. [AnalyseMode,CompileMode,PN] },
	{ note(7,'%  add ~q to worklist~n',[PN]) }.


% update_succpat(+H,+Sp,+M1,-M2,+CG,WL1-WL2 (d-list)).
%  Given the head H, and the success pattern, enter the success pattern
% into M1, producing M2. If M2 differs from M1, recompute all callers.
%  Depending on mode, we may also use P0/P1 or S0/S1.

update_succpat(Mode,H,Sp,M0,M1,P0,P1,CG,WL0,WL1) :-
	functor(H,P,N),
	lookup_cp_sp(Mode,P/N,M0,P0,Cp,Sp0),
	( less_than(Sp,Sp0) ->
	    M0 = M1,
	    P0 = P1, WL0 = WL1
	; true,
	    lub_tuples(Sp,Sp0,NewSp),
	    note(4,'%  updated ~q succpat for ~q to ~p~n% (compared ~p and old ~p)~n',
                    [Mode,P/N,'$pat'(NewSp),'$pat'(Sp),'$pat'(Sp0)]),
	    update_cp_sp(Mode,P/N,Cp,NewSp,M0,M1,P0,P1),
	    recompute_callers(P/N,CG,Mode,WL0,WL1)
	).

update_succpat_no_recompute(Mode,H,Sp,M0,M1,P0,P1,_CG,WL0,WL1) :-
	WL0 = WL1,
	functor(H,P,N),
	lookup_cp_sp(Mode,P/N,M0,P0,Cp,Sp0),
	( less_than(Sp,Sp0) ->
	    M0 = M1,
	    P0 = P1
	; true,
	    lub_tuples(Sp,Sp0,NewSp),
	    update_cp_sp(Mode,P/N,Cp,NewSp,M0,M1,P0,P1)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The following procedures must be specified by the user of the framework.
% The purpose is for the user to specify how abstract substitutions etc.
% work. The procedures work together, so representations can be fairly
% flexible.
%
% One representation is:
% - a clause+env pair is simply the clause (as read into the DB)
% - an environment A0 (etc) is a dictionary of var -> value entries
% - a call pat is a p(AbsVal1,...,AbsValN) tuple (as is a succpat),
%   as are the tuples of lub_tuples.
%
% clauses_and_envs(+Goal,+CDB,-ClauseEnvs)
%   ClauseEnvs is found in CDB to be a list of clauses+environments.
%   A clause+env structure can be simply the clause, given the proper
%   initial_i_state below; or it may be explicitly a pair of (clause,dict)
%   or a (clause,#-of-vars) pair. All three have been used by the author.
% initial_i_state(+ClauseEnv,+CallPat,-H,-B,-A0)
%    Takes a clause+env pair and the call pattern. On returning, H is
%    the clause head, B the clause body and A0 the initial environment,
%    state or abstract substitution.
% apply_i_state(+A0,+Goal,-Callpat)
%    Applies the abstract substitution A0 to the concrete term Goal,
%    producing the abstract term Callpat.
% update_i_state(+Goal,+Succpat,+A0,-A1)
%    Given a previous state A0, a concrete goal Goal, and a success pattern,
%    produce a new state A1. For instance, the goal may be "append(X,X,Y)",
%    the success pattern "append(nv,any,any)" and then A1 must have that
%    X -> nv, Y -> any (in a mode-inference situation).
% lub_tuples(+Result1,+Result2,-LUB) [nb. must handle meta-elt. 'bot']
%    Given two success patterns, construct a summary of them, the least
%    upper bound, as LUB. Note that the framework assumes a special element
%    'bot' that is used for an undefined success pattern (e.g. on failure,
%    bot is returned). It is used as a unit: bot $lub X = X $lub bot = X.
% primitive(+Goal)
%    holds when Goal is a primitive operation (that is handled by
%    analyse_primitive/3).
%
% analyse_primitive(+Goal,+Anc,+A0,-A1)
%    Given the concrete primitive operation goal Goal, and a state A0,
%    produce a new state A1. Anc is the stack of ancestors, which is
%    used to give intelligible messages to the user.
% worst_succpat(+CallPat,-SuccPat,+Anc,+Undef,-UndefOut)
%    The worst success pattern given CallPat. In an instantiation closed
%    domain, this is merely Cp = Sp. Also, the CallPat must be inserted
%    into the Undef dictionary, producing UndefOut. If no tracking of
%    undefined calls is desired, simply use "UndefOut = Undef".
%    Anc is the ancestor stack, used to report any possible bad situation.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Below are shown the concrete implementations of various accessors to
% data structures used. Change these (and probably your user primitives)
% to change representations of things.
%

:- ensure_loaded('../util/dict').
:- ensure_loaded('../preprocess/load-file').


% Abstract interface to memo tables:

empty_memotable(Empty) :- empty_dict(Empty).

% Abstract interface to export declarations:

insert_export(PN,Cp,M0,M1) :-
	insert(PN,(Cp,bot),M0,M1).

% Abstract interface to callpat-succpat table:

lookup_cp_sp(Mode,PN,M,P,Cp,Sp) :-
	analyse_mode(Mode,AnalyseMode),
	( AnalyseMode == sequential ->
	    lookup_cp_sp(PN,M,Cp,Sp)
	; AnalyseMode == parallel ->
	    lookup_cp_sp(PN,P,Cp,Sp)
	; true,
	    sys_error('lookup_cp_sp/6: ~q mode unknown',[Mode])
	).

lookup_cp_sp(PN,M0,Cp,Sp) :-
	( lookup(PN,M0,Cp0_Sp0) ->
	    copy_term(Cp0_Sp0,(Cp,Sp))
	; true,
	    Cp = bot,
	    Sp = bot
	).

% *** Specialized for listlattice :-( ***

update_cp_sp(Mode,PN,Cp0,Sp0,M0,M1,P0,P1) :-
	analyse_mode(Mode,AnalyseMode),
	( AnalyseMode == sequential ->
	    P0 = P1,
	    update_cp_sp(PN,Cp0,Sp0,M0,M1)
	; AnalyseMode == parallel ->
	    M0 = M1,
	    update_cp_sp(PN,Cp0,Sp0,P0,P1)
	; true,
	    sys_error('update_cp_sp/8: ~q mode unknown',[Mode])
	).

update_cp_sp(PN,Cp0,Sp0,M0,M1) :-
	norm_pat(Cp0,Cp1), norm_pat(Sp0,Sp1),
	copy_term((Cp1,Sp1),(Cp,Sp)),
	update(PN,(Cp,Sp),M0,M1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Ancestor Stack support.
%
notify_undefined_call(G,Anc) :-
	functor(G,P,N),
	warning('call to undefined predicate ~q',[P/N]),
	show_ancestors(Anc).


show_ancestors(Anc) :-
	show_ancestors(Anc,user_error).

show_ancestors([_],_Wr).

show_ancestors([X|Xs],Wr) :-
	format(Wr,'     ~q',[X]),
	show_each_ancestor(Xs,Wr).


show_each_ancestor([],Wr) :-
	format(Wr,'~n',[]).

show_each_ancestor([X|Xs],Wr) :-
	format(Wr,':~q',[X]),
	show_each_ancestor(Xs,Wr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Analyse & Compile Mode tracking.
%   
analyse_mode(mode( AnalyseMode,_CompileMode),AnalyseMode).

compile_mode(mode(_AnalyseMode, CompileMode),CompileMode).

propagate_compile_mode(mode(_,CompileMode),mode(_,CompileMode)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Analysis information.
%
%   The information presentated during analysis is either the current analysis
% tables (sequential and parallel), the number of predicates in the current
% iteration, or the number of the iteration. This may be altered using the
% "iteration_option" flag (see set_iteration_option/1).
%
:- dynamic iteration_option/1.

iteration_option(entries).

iteration_options([none,tables,entries,final_tables]).


iteration_info(Work,Iter,Seq,Par) :-
	iteration_option(X),!,
	( X == none ->
	    true
	; X == tables ->
	    format_analysis_tables(Seq,Par),
	    length(Work,N),
	    format('iteration ~d (~d entries to analyze)~n',[Iter,N])
	; X == entries ->
	    length(Work,N),
	    format(user_error,'[~d]',[N])
	; X == final_tables ->
	    ( Work == [] -> format_analysis_tables(Seq, Par) ; true )
	; true,
	    format(user_error,'(~d)',[Iter])
	),
	( Work == [] -> nl ; true ).


set_iteration_option(X) :-
	retractall(iteration_option(_)),
	iteration_options(Xs),
	( member(X,Xs) ->
	    assert(iteration_option(X))
	; assert(iteration_option(entries)), %%% Reset default option.
	  error('set_iteration_option/1: ~p must be one of ~p',[X,Xs]) ).


format_analysis_tables(Seq, Par) :-
	format('~n-----------sequential----------------~n',[]),
	format_analysis_table(Seq),
	format('~n------------parallel-----------------~n',[]),
	format_analysis_table(Par),
	nl.

format_analysis_table(Dict) :-
	list_dict(Dict,Lst),
	format_analysis_entries(Lst).

format_analysis_entries([]).
format_analysis_entries([(PN,Cp,Sp)|Xs]) :-
	format_analysis_entry(PN,Cp,Sp),
	format_analysis_entries(Xs).

format_analysis_entry(PN,Cp,Sp) :-
	format('~q : ~p -> ~p~n',[PN,'$pat'(Cp),'$pat'(Sp)]).

