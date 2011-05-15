%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%			  PROGRAM ANNOTATION
%
% To communicate the results of the analysis to the compiler, the
% annotator rewrites the program database to take types etc. into
% account. The input format of the clauses is a pair (GroundClause,Env)
% with variables replaced with $VAR(N) structures (N being a number
% between 1 and the number of distinct variables in the clause).
%
% After this phase, all variables instead look like
%   $VAR(N,Par,Seq,Loc)
% where
%  Par is the parallel type
%  Seq is the sequential type
%  Loc is the locality
%
% If the predicate appears only in a sequential context, Par = Seq
% and Loc is local.
%
% Annotation is done by pseudo-execution: take the call pattern, perform
% abstract indexing in the normal fashion, and "analyse". There are some
% differences:
% - parallel and sequential types are managed simultaneously
% - preceding each call, the call is annotated.
% - all calls are either primitives or user calls. User calls MUST be
%   found in the memo tables!
% - determinacy changes are recorded as extra goals, $det or $nondet.

:- ensure_loaded('../compilation/safeness').
:- ensure_loaded('../preprocess/lift').

%

annotate_entry((no_clause/0,Info),_,_,AnnotIx) :- !,
	Info = info(Ix,_,F),
	NewInfo = info(Ix,invisible,F,bot,bot),
	AnnotIx = (no_clause/0,NewInfo).

annotate_entry((P/N,Info),Seq,Par,(P/N,AnnotInfo)) :-
	Info = info(CompileMode,IxStruct,Ds,F),
	NewDs = Ds,
	AnnotInfo = info(AnnotIxStruct,NewDs,F,SeqCp,ParCp),
	( CompileMode = parallel ->
	    annotate_ix_struct(IxStruct,SeqCp,ParCp,Seq,Par,P,N,AnnotIxStruct)
	; CompileMode = sequential ->
	    SeqCp = unanalysed,
	    ParCp = unanalysed,
	    annotate_as_unanalysed(IxStruct,Seq,Par,P,N,AnnotIxStruct)
	; true,
	    sys_error('annotate_entry/4: unknown compile mode (~q)',[CompileMode])
	).


annotate_ix_struct(IxStruct,SeqCp,ParCp,Seq,Par,P,N,AnnotIxStruct) :-
	( annotate_as_parallel(IxStruct,SeqCp,ParCp,Seq,Par,P,N,AnnotIxStruct) ->
	    true
	; annotate_as_sequential(IxStruct,SeqCp,Seq,Par,P,N,AnnotIxStruct) ->
	    ParCp = unanalysed
	; true,
	    warning('predicate ~q has no call pattern (assumes defaults)', [P/N]),
	    SeqCp = unanalysed,
	    ParCp = unanalysed,
	    annotate_as_unanalysed(IxStruct,Seq,Par,P,N,AnnotIxStruct)
	).


annotate_as_unanalysed(IxStruct,Seq,Par,P,N,AnnotIxStruct) :-
	( IxStruct = ix(_V,_C,_L,_N,_S,_Cls) ->
	    annotate_sequential(IxStruct,unanalysed,Seq,Par,AnnotIxStruct)
	; IxStruct = rp(_Base,_ParRec,_Rec,_Type,_LB,_RB) ->
	    annotate_parallel_as_sequential(IxStruct,Seq,Par,P,N,AnnotIxStruct)
	; true,
	    sys_error('annotate_as_unanalysed/6: unknown indexing format (~q)',[IxStruct])	    
	).


annotate_as_parallel(IxStruct,SeqCp,ParCp,Seq,Par,P,N,AnnotIxStruct) :-
	non_bot_lookup_cp(P/N,Par,ParCp),
	( non_bot_lookup_cp(P/N,Seq,SeqCp) ->
	    true
	; true,
	    sys_error('predicate ~q has par c.p. but no seq c.p.',[P/N])
	),
	( annotate_parallel(IxStruct,SeqCp,ParCp,Seq,Par,AnnotIxStruct) ->
	    true
	; true,
	    sys_error('parallel annotation of ~q failed',[P/N])
	).


%%% Includes annotation of r.p. predicates:
%%%
annotate_as_sequential(IxStruct,SeqCp,Seq,Par,P,N,AnnotIxStruct) :-
	non_bot_lookup_cp(P/N,Seq,SeqCp),
	( annotate_sequential(IxStruct,SeqCp,Seq,Par,AnnotIxStruct) ->
	    true
	; true,
	    sys_error('sequential annotation of ~q failed',[P/N])
	).


annotate_parallel_as_sequential(IxStruct,Seq,Par,P,N,AnnotIxStruct) :-
	sequentialize_predicate(IxStruct,SeqIxStruct),
	warning('predicate ~q compiled sequentially',[P/N]),
	annotate_sequential(SeqIxStruct,unanalysed,Seq,Par,AnnotIxStruct).


%

dummy_cp(N,shr([],0,ParCp)) :-
	dummy_entries(N,ParArgs),
	ParCp =.. [det|ParArgs].

dummy_entries(0,X) :- !, X = [].
dummy_entries(N,X) :- N > 0,
	X = [any(_,_,_)|Xs], M is N-1,
	dummy_entries(M,Xs).

%

non_bot_lookup_cp(PN,Memo,Cp) :-
	lookup_cp_sp(PN,Memo,Cp,_),
	Cp \== bot.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% We do not use information for sequential compilation, but the
% annotations are needed for the sake of the compiler. (It must see
% that stuff really is local.)

annotate_sequential(Ix,SeqCp,_Seq,_Par,AnnotIx) :-
	Ix = ix(_,_,_,_,_,_),!,
	nullify_dead_clauses(Ix,SeqCp,V,C,L,N,S,NewCls),
	new_indexing_struct(V,C,L,N,S,NewCls,NewIx),
	minimal_assumptions(NewIx,AnnotIx).

 % The recursion parallel case needs only annotate the left and
 % right bodies with useful information.
 %
 % Thus: head, left+right prologues, rec calls are annotated with 'default
 %       info'.

annotate_sequential(rp(Base,Rec,SeqRec,Class,LeftBody,RightBody),_RpSeqCp,Seq,Par,AnnotIx) :-
         annotate_rp_body_clause(LeftBody,Seq,Par,AnnotLeftBody),
         annotate_rp_body_clause(RightBody,Seq,Par,AnnotRightBody),
	 Rec = ((H :- rec(LP,LB,R,RB,RP)),Env),
	 functor(Env,_,N),
	 Var is N+1,
	 unfold_body_call(LB,AnnotLeftBody,NewLB,Var,NxtVar),
	 unfold_body_call(RB,AnnotRightBody,NewRB,NxtVar,_),
	 % H and R are sequential & do not need full machinery.
	 % The same holds for base clause.
	 minimal_annot_head(H,NewH),
	 minimal_annot_body(LP,NewLP),
	 minimal_annot_body(RP,NewRP),
	 minimal_annot_rec_call(R,NewR),
	 Base = (BaseClause,Base_Env), functor(Base_Env,_,Base_N),
	 BaseVar is Base_N+1,
	 minimal_annot_clause(BaseClause,BaseVar,NewBase),
	 SeqRec = (SeqRecClause,Seq_env), functor(Seq_env,_,Seq_N),
	 SeqVar is Seq_N+1,
	 minimal_annot_clause(SeqRecClause,SeqVar,NewSeqRec),
	 NewRec = (NewH :- rec(NewLP,NewLB,NewR,NewRB,NewRP)),
	 AnnotIx = rp(NewBase,NewRec,NewSeqRec,Class).

%

minimal_annot_rec_call(R,NewR) :-
	minimal_annot_head(R,NewR).

% The following is probably what you need to care about: how
% a variable is annotated. The rest merely drives down into the
% structure.

minimal_annot_var(N,'$VAR'(N,any,any,local)).

%

minimal_assumptions(ix(V,C,L,N,S,Cls),ix(V,C,L,N,S,AnnotCls)) :-
	Cls =.. [cls|Cs],
	minimal_annot_cls(Cs,AnnotCs),
	AnnotCls =.. [cls|AnnotCs].


minimal_annot_cls([],[]).

minimal_annot_cls([CE|Cs],[AnnotC|AnnotCs]) :-
	( CE = (C,E) ->
	  functor(E,_,MaxVarNo),
	  CurrMax is MaxVarNo+1,
	  minimal_annot_clause(C,CurrMax,AnnotC),
	  minimal_annot_cls(Cs,AnnotCs)
	; CE = dummy ->
	  AnnotC = CE,
	  AnnotCs = []
	; sys_error('minimal_annot_cls/2: bad clause form ~q',[CE])
	).

%

minimal_annot_clause((H :- B),CurrMax,(AnnotH :- AnnotB)) :-
	unlift_clause((H :- B),(TmpH :- TmpB)),
	preannotate_rewrite_clause(TmpH,TmpB,CurrMax,NewH,NewB),
	minimal_annot_head(NewH,AnnotH),
	minimal_annot_body(NewB,AnnotB).

%

preannotate_rewrite_clause(H,B,M,NewH,NewB) :-
	H =.. [P|Xs],
	empty_dict(Seen),
	preannotate_rewrite_terms(Xs,M,Seen,NewXs,NewB,B),
	NewH =.. [P|NewXs].

preannotate_rewrite_terms([],_N,_Seen,[],B,B).
preannotate_rewrite_terms([X|Xs],M,Seen,[NewX|NewXs],NewB,B) :-
	preannotate_rewrite_term(X,M,N,Seen,NowSeen,NewX,NewB,RestB),
	preannotate_rewrite_terms(Xs,N,NowSeen,NewXs,RestB,B).

% Rewriting a term:
% - if it is a variable, an error
% - if it is a constant, lift it out
% - if it is a variable:
%    occurs for 1st time? don't lift
%    otherwise, lift
% - if a structure or list, lift it out.

preannotate_rewrite_term(X,_M,_N,_Seen,_NowSeen,_NewX,_NewB,_RestB) :- var(X),!,
	sys_error('preannotate_rewrite_term: first arg variable!~n').
preannotate_rewrite_term(X,M,N,Seen,NowSeen,NewX,NewB,RestB) :-	atomic(X),!,
	NewX = '$VAR'(M),
	N is M+1, NowSeen = Seen, NewB = (NewX = X, RestB).
preannotate_rewrite_term(X,M,N,Seen,NowSeen,NewX,NewB,RestB) :-
	X = '$VAR'(K), integer(K), !,
	( defined(K,Seen) ->
	  NewX = '$VAR'(M), N is M+1,
	  Seen = NowSeen, NewB = (NewX = X, RestB)
	; NewX = X, insert(K,def,Seen,NowSeen), M = N, NewB = RestB
	).
preannotate_rewrite_term(X,M,N,Seen,NowSeen,NewX,NewB,RestB) :-
	NewX = '$VAR'(M), N is M+1,
	Seen = NowSeen,
	NewB = (NewX = X, RestB).

%

minimal_annot_head(H,AnnotH) :-
	minimal_annot_term(H,AnnotH).

%

minimal_annot_body((G,Gs),AnnotBody) :- !,
	AnnotBody = (AnnotG,AnnotGs),
	minimal_annot_term(G,AnnotG),
	minimal_annot_body(Gs,AnnotGs).

minimal_annot_body(G,AnnotG) :-
	minimal_annot_term(G,AnnotG).

%

minimal_annot_term(X,Y) :- atomic(X),!,Y=X.

minimal_annot_term('$VAR'(N),X) :- integer(N),!,
	minimal_annot_var(N,X).

minimal_annot_term(PX,AnnotPX) :-
	PX =.. [P|Xs],
	minimal_annot_terms(Xs,AnnotXs),
	AnnotPX =.. [P|AnnotXs].

%

minimal_annot_terms([],[]).

minimal_annot_terms([X|Xs],[AnnotX|AnnotXs]) :-
	minimal_annot_term(X,AnnotX),
	minimal_annot_terms(Xs,AnnotXs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Annotation for parallel code is the interesting part.
% Annotation is coupled with abstract execution and
% unsafeness checking (?).
%
% First, split up the indexing structure so that no clause
% is called both determinately and nondeterminately. This is
% useful to predicates that are called as 'det'; 'nondet' calls
% need not split clauses since code will use 'strict' instructions
% anyway.
%
% After splitting, we do abstract indexing in a slightly
% nonstandard way (by first checking the par type for whether it
% will suspend or not, and using seq type if so). The result is
% a list of (Cp,Clause) pairs. The clauses that do not appear
% in the list will never be called and are replaced with
% dummy entries.
%
% Each clause is annotated starting from the call pattern. For
% every variable, we maintain parallel and sequential type and
% locality. Before executing a call, we annotate its variables
% with this information. After the call, the relevant types are
% updated. Note that we use the normal types for this re-execution,
% so they must be maintained as well.
%
% *** UNFINISHED ***
% - Can we ensure that all the info is correct? (esp. if we start
% mixing and matching parallel and sequential types -- then
% the table lookup may be wrong). I think the separation of 'physical'
% and 'useful' types is very useful.

annotate_parallel(X,_,_,_,_,_) :- var(X),!,
	sys_error('annotate_parallel: var arg').

annotate_parallel(ix(V,C,L,N,S,Cls),SeqCp,ParCp,Seq,Par,AnnotIx) :-
	split_clause_entries(V,C,L,N,S,Cls,NewV,NewC,NewL,NewN,NewS,NewCls),
	new_indexing_struct(NewV,NewC,NewL,NewN,NewS,NewCls,SplitIx),
	annotate_ix_struct(SplitIx,SeqCp,ParCp,Seq,Par,AnnotIx).

annotate_parallel(rp(Base,_Rec,_SeqRec,_Type,_LeftBody,_RightBody),
	          _SeqCp,_ParCp,_Seq,_Par,_AnnotIx) :-
        Base = (H :- _),
	functor(H,P,N), PN = P/N,
        sys_error('annotate_parallel: rec.par pred ~q has parallel c.p.',[PN]).

%

new_indexing_struct(V,C,L,N,S,Cls,AnnotIx) :-
	AnnotIx = ix(V,C,L,N,S,Cls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Annotating a body clause and unfolding it.
% Annotation is standard -- just annotate the clause; everything is
% in the tables.
%
% Unfolding is done to remove the temporary illusion of a separate
% predicate. The main issue is renaming grounded variables right.

annotate_rp_body_clause((no_clause,E),_,_,Annot) :- !, Annot = (no_clause,E).

annotate_rp_body_clause(((H :- B),E),Seq,Par,AnnotCls) :-
	functor(H,P,N),
	lookup_cp_sp(P/N,Seq,SeqCp,_SeqSp),
	lookup_cp_sp(P/N,Par,ParCp,_ParSp),
	annotate_clause(((H :- B),E),SeqCp,ParCp,Seq,Par,AnnotCls).

unfold_body_call(none,_,NewLB,Var,NxtVar) :- !,
	NewLB = true, Var = NxtVar.

unfold_body_call(Call,(H :- B),NewLB,Var,NxtVar) :-
	unfold_init(Call,H,VarMap,Var,CurrVar),
	unfold_body_call(B,NewLB,VarMap,_FinalVarMap,CurrVar,NxtVar).

%

unfold_init(Call,H,VarMap,Var,NxtVar) :-
	Call =.. [_|Xs],
	H =.. [_|Ys],
	empty_dict(Empty),
	unfold_call_args(Xs,Ys,Empty,VarMap,Var,NxtVar).

unfold_call_args([],[],Map,Map,Var,Var).

unfold_call_args([X|Xs],[Y|Ys],M0,M2,V0,V2) :-
	unfold_call_arg(X,Y,M0,M1,V0,V1),
	unfold_call_args(Xs,Ys,M1,M2,V1,V2).

% Note that this definition relies on a fairly rigorous definition
% of the head of body predicates: p(X1,...,Xn) being a linear head &c.
%
% The types of the head are worthless and thrown away. This is again
% because no nontrivial unification occurs, only 'parameter passing'.

unfold_call_arg('$VAR'(N),'$VAR'(M,_P1,_S1,_L1),M0,M1,V0,V1) :- 
	integer(N),!,
	( lookup(M,M0,Var) ->
	  ( N =:= Var ->
	    M0 = M1, V0 = V1
	  ; sys_error('unfold_call_arg/6: inconsistent arg numbering')
          )
	; V1 is V0+1,        % var was not in dict.
	  insert(M,N,M0,M1)
	).

unfold_call_arg(Call,Head,_M0,_M1,_V0,_V1) :-
	sys_error('unfold_call_arg/6: ~p unfolded with head arg ~p -- illegal',
	      [Call,Head]).

% Replace variable names by new names consistent with enclosing
% r.p. clause:

unfold_body_call(Body,NewBody,Map0,Map1,Var0,Var1) :-
	unfold_term(Body,NewBody,Map0,Map1,Var0,Var1).

%

unfold_term(X,Y,M0,M1,V0,V1) :- atomic(X),!,
	X=Y, M0=M1, V0=V1.

unfold_term('$VAR'(M,P,S,L),Y,M0,M1,V0,V1) :-
	( lookup(M,M0,N) ->
	  Y = '$VAR'(N,P,S,L), M0 = M1, V0 = V1
	; V1 is V0+1,
	  Y = '$VAR'(V0,P,S,L), insert(M,V0,M0,M1)
	).

unfold_term(PX,PY,M0,M1,V0,V1) :-
	PX =.. [P|Xs],
	unfold_term_list(Xs,Ys,M0,M1,V0,V1),
	PY =.. [P|Ys].

%

unfold_term_list([],[],M,M,V,V).

unfold_term_list([X|Xs],[Y|Ys],M0,M2,V0,V2) :-
	unfold_term(X,Y,M0,M1,V0,V1),
	unfold_term_list(Xs,Ys,M1,M2,V1,V2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Annotating an indexing structure:
% - find indexing callpats
% - remove dead clauses
% - annotate the survivors
%
% Annotating a clause:
% - set up initial environments of 'physical' and 'useful' types
% do
% - annotate goal
% - check goal for safeness
% - execute goal (analyse_primitive or table lookup)
% - take next goal
% until no goals remain.

annotate_ix_struct(Ix,SeqCp,ParCp,Seq,Par,AnnotIx) :-
	nullify_dead_clauses(Ix,SeqCp,V,C,L,N,S,TmpCls),
	new_indexing_struct(V,C,L,N,S,TmpCls,TmpIx),
	absindex(SeqCp,TmpIx,CpsCsEs),
	annotate_clauses(CpsCsEs,ParCp,Seq,Par,NewCs),
	NewCls =.. [cls|NewCs],
	new_indexing_struct(V,C,L,N,S,NewCls,AnnotIx).

% Advanced implementation: clauses that cannot occur are
% replaced with 'mode error' and non-occuring entries with
% fail. This can reduce compilation effort.

nullify_dead_clauses(ix(V0,C0,L0,N0,S0,Cls),SeqCp,V,C,L,N,S,NewCls) :-
	( SeqCp = shr(_,_,SeqTuple) -> true
	; SeqCp = bot -> sys_error('nullify_dead_clauses/8: bot seq.cp.')
	; SeqTuple = SeqCp
	),
	functor(SeqTuple,_,Ar),
	( Ar =:= 0 ->
	  V = V0, C = C0, L = L0, N = N0, S = S0, Cls = NewCls
	; Ar > 0 ->
	  arg(1,SeqTuple,SeqType), functor(SeqType,SeqT,_),
	  Mode = mode(SeqT),
	  ( SeqT == nil -> 
	    V = Mode, L = Mode, N = Mode, S = Mode, OldMap = [C0], NewMap = [C]
	  ; SeqT == gnd ->
	    V = Mode, OldMap = [C0,L0,N0,S0], NewMap = [C,L,N,S]
	  ; SeqT == nv ->
	    V = Mode, OldMap = [C0,L0,N0,S0], NewMap = [C,L,N,S]
	  ; SeqT == any ->
	    OldMap = [V0,C0,L0,N0,S0], NewMap = [V,C,L,N,S]
	  ; SeqT == free ->
	    C = Mode, L = Mode, N = Mode, S = Mode, OldMap = [V0], NewMap = [V]
	  ; SeqT == free_nil ->
	    L = Mode, N = Mode, S = Mode, OldMap = [V0,C0], NewMap = [V,C]
	  ; list_types(SeqT,_,Tail),
	    ( Tail == n ->
	      V = Mode, N = Mode, S = Mode, OldMap = [C0,L0], NewMap = [C,L]
	    ; Tail == f ->
	      C = Mode, N = Mode, S = Mode, OldMap = [V0,L0], NewMap = [V,L]
	    ; Tail == fn ->
	      N = Mode, S = Mode, OldMap = [V0,C0,L0], NewMap = [V,C,L]
	    )
	  ),
	  kill_clauses(OldMap, NewMap, Cls, NewCls)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% kill_clauses/3 extracts what clauses are still alive from List and
% replaces dead entries of Cls with dummies; the result is NewCls.

kill_clauses(OldMap,NewMap,Cls,NewCls) :-
	functor(Cls, _, N),
	create_label_table(N, LabelTable),
	surviving_clauses(OldMap, NewMap, LabelTable, Survivors),
	create_label_mapping(1, N, LabelTable, LabelMap),
	remove_dead_clauses(Survivors, LabelMap, N, Cls, NewCls),
	propagate_label_mapping(LabelMap, 0).

%

surviving_clauses(OldMap, NewMap, LabelTable, Survivors) :-
	surviving_clauses(OldMap, NewMap, LabelTable, S0, []),
	sort(S0, Survivors).

surviving_clauses(    [],     [],_LabelTable) --> [].

surviving_clauses([X|Xs], [Y|Ys], LabelTable) -->
	survivors(X, Y, LabelTable),
	surviving_clauses(Xs, Ys, LabelTable).


survivors(    [],     [],_LabelTable) --> [].

survivors([X|Xs], [Y|Ys], LabelTable) --> [X], { entry_label_table(X, LabelTable, Y) },
	survivors(Xs, Ys, LabelTable).

survivors(table(JmpMap,AuxMap,LblMap), table(NewJmpMap,NewAuxMap,NewLblMap), LabelTable) -->
	{
	    surviving_branches(JmpMap, LabelTable, NewJmpMap, []),
	    surviving_label_aux(AuxMap, LabelTable, NewAuxMap, [])
	},
	surviving_clauses(LblMap, NewLblMap, LabelTable).


surviving_branches(              [],_LabelTable) --> [].

surviving_branches([(PN,AuxMap)|Xs], LabelTable) --> [(PN,NewAuxMap)],
	{
	    surviving_label_aux(AuxMap, LabelTable, NewAuxMap, [])
	},
	surviving_branches(Xs, LabelTable).


surviving_label_aux( [],_LabelTable) --> [].

surviving_label_aux([X|Xs], LabelTable) --> [Y], { entry_label_table(X, LabelTable, Y) },
	surviving_label_aux(Xs, LabelTable).


%

create_label_table(N, Table) :-
	length(Entries, N),
	Table =.. [label_table|Entries].

entry_label_table(N, Table, Entry) :-
	arg(N, Table, Entry).


create_label_mapping(M, N,_LabelTable, []) :- M =:= N + 1.

create_label_mapping(M, N, LabelTable, ['Label'(M,Ref)|Rest]) :- M =< N,
	entry_label_table(M, LabelTable, Ref),
	K is M + 1,
	create_label_mapping(K, N, LabelTable, Rest).


propagate_label_mapping(    [],_Offset).

propagate_label_mapping([L|Ls], Offset) :-
	propagate_label(L, Offset, NewOffset),
	propagate_label_mapping(Ls, NewOffset).
	

propagate_label('Label'(N,Ref), Offset, NewOffset) :-
	( Ref == dead ->
	    NewOffset is Offset + 1
	; var(Ref) ->
	    Ref is N - Offset,
	    NewOffset is Offset
	; true,
	    sys_error('propagate_label/3: illegal label reference (~q)', [Ref])
	).


kill_label('Label'(N,Ref)) :- integer(N), Ref = dead.


remove_dead_clauses(Survivors, LabelMap, N, OldCls, NewCls) :-
	live_dead_list(Survivors, 0, N, LiveDead, []),
	OldCls =.. [F|OldClsList],
	eliminate_dead(LiveDead, LabelMap, OldClsList, NewClsList),
	NewCls =.. [F|NewClsList].


live_dead_list(    [], M, N) --> repeat_dead(M, N).

live_dead_list([X|Xs], M, N) --> { X1 is X - 1 },
	repeat_dead(M, X1),
	[live],
	live_dead_list(Xs, X, N).


repeat_dead(M, N) --> { M >= N }.

repeat_dead(M, N) --> { M < N, M1 is M + 1 },
	[dead],
	repeat_dead(M1, N).


eliminate_dead(    [],     [],     [],        []).

eliminate_dead([X|Xs], [L|Ls], [OldCls|OldCs], Remainder) :-
	( X == live ->
	    Remainder = [OldCls|NewCs]
	; X == dead ->
	    Remainder = NewCs,
	    kill_label(L),
	    OldCls = ((Head :- _Body),_Env), %%% Yack !! - where is that interface...
	    functor(Head,P,N),
	    warning('found unreachable clause in ~q',[P/N])
	; true,
	    sys_error('eliminate_dead/4: ~q must be live/dead',[X])
	),
	eliminate_dead(Xs, Ls, OldCs, NewCs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

annotate_clauses([],_ParCp,_Seq,_Par,[]).

annotate_clauses([(SeqCp,CE)|Cs],ParCp,Seq,Par,[NewC|NewCs]) :-
	annotate_clause(CE,SeqCp,ParCp,Seq,Par,NewC),
	annotate_clauses(Cs,ParCp,Seq,Par,NewCs).

%

annotate_clause(dummy,_,_,_,_,X) :- !, X = dummy.

%annotate_clause(CE,SeqCp,ParCp,Seq,Par,NewC) :- !,
%	CE = ((H :- B),Env),
%	functor(H,P,N),
%	Proc = P/N,
%	NewC = (AnnotH :- AnnotB),
%	empty_dict(LeftIn),
%	initial_determinacy(SeqCp,InitDet,NewB,B),
%	functor(Env,_,MaxVarIndex), NextVarIndex is MaxVarIndex+1,
%	NewCE = ((H :- NewB),Env),
%	annotate_head(NewCE,NextVarIndex,SeqCp,ParCp,InitDet,LeftIn,Left,
%	              SeqEnv,ParEnv,AnnotH,AnnotB,VarB,TmpB),
%	annotate_body(TmpB,SeqEnv,ParEnv,Left,Seq,Par,Proc,
%	              InitDet,VarB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEW! IMPROVED!

annotate_clause(CE,SeqCp,ParCp,Seq,Par,NewC) :-
	prior_annotation(SeqCp,CE,NewCE,NewC,NxtVar,Left,
	                 Proc,InitDet,AnnotH,AnnotB),
	annotate_head_pre(NewCE,SeqCp,ParCp,InitDet,
	                  SeqEnv,ParEnv,AnnotH_1,Body),
	annotate_body(Body,SeqEnv,ParEnv,Left,Seq,Par,Proc,InitDet,AnnotB_1),
	annotate_head_post(AnnotH_1,AnnotB_1,SeqCp,ParCp,
	                   NxtVar,AnnotH,PartOfAnnotB),
	initial_determinacy(SeqCp,InitDet,AnnotB,PartOfAnnotB).

prior_annotation(SeqCp,CE,NewCE,NewC,NxtVar,L1,Proc,
	         InitDet,AnnotH,AnnotB) :-
	CE = ((H :- B),Env),
	functor(H,P,N),
	Proc = P/N,
	NewC = (AnnotH :- AnnotB),
	H =.. [_|Xs],
	empty_dict(L0),
	indexing_args_are_leftmost(Xs,L0,L1),
	initial_determinacy(SeqCp,InitDet,_NewB,true), % moved out mod.of body
	NewB = B,
	functor(Env,_,MaxVarIndex), NxtVar is MaxVarIndex+1,
	NewCE = ((H :- NewB),Env).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Three stages:
% 1. Simple head annotation

annotate_head_pre(NewCE,SeqCp,ParCp,Det,SeqE,ParE,AnnotH,Body) :-
	initial_i_state(NewCE,SeqCp,Head,Body,SeqE),
	initial_i_state(NewCE,ParCp,_,_,ParE),!, % should be det, actually ...
	annotate_default(Head,AnnotH),
	check_head_match_safeness(Det,ParE,SeqE).

% Prior to head unification, all 
annotate_default(Head,AnnotHead) :-
	Head =.. [P|Xs],
	annotate_default_terms(Xs,NewXs),
	AnnotHead =.. [P|NewXs].

annotate_default_terms([],[]).
annotate_default_terms([X|Xs],[NewX|NewXs]) :-
	annotate_default_term(X,NewX),
	annotate_default_terms(Xs,NewXs).

annotate_default_term(X,_NewX) :- 
	var(X),!,
	sys_error('annotate_default_term: var arg~n').
annotate_default_term(X,NewX) :- atomic(X),!,NewX=X.
annotate_default_term('$VAR'(N),NewX) :- integer(N),
	!, NewX = '$VAR'(N,free,free,local).
annotate_default_term(X,NewX) :-
	X =.. [P|Xs],
	annotate_default_terms(Xs,NewXs),
	NewX =.. [P|NewXs].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2. Annotation of body, done below.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3. Adjusting annotation of head to (a) move value unifications in head
%    back into position and (b) lifting out all non-trivial unifications
%    from head

annotate_head_post(Head,Body,SeqCp,ParCp,Nxt,AnnotH,AnnotB) :-
	unlift_clause_annot_vars((Head :- Body),(NewHead :- NewBody)),
	simplify_patterns(SeqCp,ParCp,Seq2,Par2),
	final_lift_unifications(NewHead,AnnotH,AnnotB,NewBody,Nxt,Seq2,Par2).

% Lift all unifications for the last time. Main work done in
%  final_lift_unify.

final_lift_unifications(Head,NewHead,NewBody,RestBody,NxtVar,SeqCp,ParCp) :-
	Head =.. [P|Xs],
	SeqCp =.. [_|SeqXs],
	ParCp =.. [_|ParXs],
	empty_dict(Seen),
	susp_arg_list(Xs,SeqXs,ParXs,Susp),
	final_lift_unify_list(Xs,Susp,NewXs,NewBody,RestBody,Seen,NxtVar,
	                      SeqXs,ParXs),
	NewHead =.. [P|NewXs].

% Check what args cause suspension -- this includes the 1st arg usually.
% Yields a list [susp,nsusp,...]

susp_arg_list([],[],[],[]).
susp_arg_list([X|Xs],[SeqX|SeqXs],[ParX|ParXs],[S|Susp]) :-
	susp_arg(X,SeqX,ParX,S),
	susp_arg_list(Xs,SeqXs,ParXs,Susp).

% If parallel type is fragile and X is non-variable, arg is 'susp'.
% (Is this too liberal? What if type is nonvariable as well?)
% Otherwise, 'nsusp'.

susp_arg(X,_SeqX,ParX,S) :-
	(( symbolic_locality_of_type(ParX,fragile), \+(is_var(X)) ) -> 
	  S = susp
	; true,
	  S = nsusp
	).


% lift each unification, as preannotate_rewrite_term but with some extras

final_lift_unify_list([],[],[],Body,Body,_Seen,_Nxt,[],[]).
final_lift_unify_list([X|Xs],[S|Ss],[NewX|NewXs],NewBody,RestBody,Seen,Nxt,
	              [SX|SXs],[PX|PXs]) :-
	final_lift_unify(X,S,NewX,NewBody,TmpBody,Seen,NowSeen,Nxt,Curr,SX,PX),
	final_lift_unify_list(Xs,Ss,NewXs,TmpBody,RestBody,
	                      NowSeen,Curr,SXs,PXs).

% lift out unifications for the last time: 
%   X is the current head arg (annotated term of some kind)
%   NewX is the new arg, if X is nonvar etc, NewX is a fresh (annotated) var
%   NewBody: if X is lifted, do NewBody =  (NewX=X,TmpBody); otherwise,
%     NewBody = TmpBody
%   TmpBody rest of body
%   Seen is the set of variables that have been encountered before this point
%   NowSeen is the varset encountered after this point
%   M is the variable number to be used to construct NewX, if any
%   N is returned as next var number to use (incremented if NewX constr.)
%   SX is sequential type, used w. SX to annotate NewX (if X is not lifted,
%      X is already annotated with the right thing, btw)
%   PX is parallel type, used with SX
%
% The criteria for lifting a unification are adapted directly from
% preannotate_rewrite_term!

final_lift_unify(X,_Susp,_NewX,_NewB,_RestB,_Seen,_NowSeen,_M,_N,_SX,_PX) :-
	var(X),!,sys_error('lift_unify: first arg variable!~n').
final_lift_unify(X,Susp,NewX,NewB,RestB,Seen,NowSeen,M,N,SX,PX) :-
	atomic(X),!,
	N is M+1, NowSeen = Seen, 
	NewB = (NewX = X, RestB),
	annotate_arg(SX,PX,M,Susp,NewX).
final_lift_unify(X,Susp,NewX,NewB,RestB,Seen,NowSeen,M,N,SX,PX) :-
	X = '$VAR'(K,_,_,_),!,
	( defined(K,Seen) ->
	  N is M+1,
	  Seen = NowSeen, NewB = (NewX = X, RestB),
	  annotate_arg(SX,PX,M,Susp,NewX)
	; NewX = X, 
	  M = N, NewB = RestB,
	  insert(K,def,Seen,NowSeen)
	).
final_lift_unify(X,Susp,NewX,NewB,RestB,Seen,NowSeen,M,N,SX,PX) :-
	N is M+1,
	Seen = NowSeen,
	NewB = (NewX = X, RestB),
	annotate_arg(SX,PX,M,Susp,NewX).

%

annotate_arg(SeqT,ParT,N,Susp,'$VAR'(N,ParType,SeqType,Loc)) :-
	functor(SeqT,SeqType,_),
	functor(ParT,ParT0,_),
	symbolic_locality_of_type(ParT,Loc),
	( Susp = susp ->
	  ParType = SeqType
	; system_type_id(ParT) ->
	  ParType = ParT0
	; t_to_num(SeqType,S),
	  t_to_num(ParT0,P),
	  num_lub(S,P,Par_num),
	  num_to_t(Par_num,ParType)
	).


	
%

simplify_patterns(Seq,Par,ModSeq,ModPar) :-
	( Seq = shr(Shr,_,ModSeq) ->
	  install_aliases(Shr,ModSeq)
	; ModSeq = Seq
	),
	( Par = shr(ParShr,_,ModPar) ->
	  install_aliases(ParShr,ModPar)
	; ModPar = Par
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Types and locality. So far, nothing has become leftmost or anything.
% Hence, we can use this simple annotation.

types_and_locality(SeqX,ParX,Par,Seq,Loc) :-
	functor(SeqX,Seq,_),
	functor(ParX,Par,_),
	symbolic_locality_of_type(ParX,Loc).

% check_head_match_safeness: clauses are rewritten to have
% linear heads. This means head matching is ALWAYS safe.

check_head_match_safeness(_,_,_).

%%%%%%%%%%
% The arguments we are indexing on (currently only the first) are
% inserted into the 'leftmost vars' dictionary.

indexing_args_are_leftmost([],L,L). % arity-0 preds

indexing_args_are_leftmost([X|_],L0,L1) :-
	var_indices_of(X,Vs,[]),
	insert_indices(Vs,L0,L1).

% Note that if the indexing arg is not a variable, there is
% no suspension. E.g., indexing on the term f(X1,X2) does
% not mean the type of X1 or X2 is 'sequential'.

var_indices_of(X) --> { var(X),! }.

var_indices_of(X) --> { atomic(X),! }.

var_indices_of('$VAR'(N)) --> { integer(N) }, !,[N].

var_indices_of(_PX) --> !,[].

% Old code:
%	{ PX =.. [_|Xs] },
%	var_indices_of_list(Xs).
%
%var_indices_of_list([]) --> [].
%var_indices_of_list([X|Xs]) -->
%	var_indices_of(X), var_indices_of_list(Xs).
%
%

insert_indices([],L,L).

insert_indices([X|Xs],L0,L2) :-
	update(X,dummy,L0,L1),
	insert_indices(Xs,L1,L2).

%%%%%%%%%%%
%
%init_annotate_terms([],[],[],_Det,_SeqE,_ParE,_Left,[]).
%
%init_annotate_terms([X|Xs],[SeqX|SeqXs],[ParX|ParXs],Det,
%	SeqE,ParE,Left,[NewX|NewXs]) :-
%        init_annotate_term(X,SeqX,ParX,Det,SeqE,ParE,Left,NewX),
%	init_annotate_terms(Xs,SeqXs,ParXs,Det,SeqE,ParE,Left,NewXs).
%
%%
%
%init_annotate_term(X,SeqX,ParX,Det,SeqE,ParE,Left,NewX) :- atomic(X),!,
%	X = NewX,
%	check_match_safeness(Det,X,SeqX).
%
%init_annotate_term('$VAR'(N),SeqX,ParX,Det,SeqE,ParE,Left,NewX) :- !,
%	NewX = '$VAR'(N,ParType,SeqType,Locality),
%	nyi.
%
%init_annotate_term(X,SeqX,ParX,Det,SeqE,ParE,Left,NewX) :-
%	X =.. [P|Xs], functor(X,_,N),
%	subinst(SeqX,N,SeqXs),
%	subinst(
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initial_determinacy(SeqCp,InitDet,NewB,RestNewB) :-
	( SeqCp = shr(_,_,SeqTuple) ->
	  functor(SeqTuple,ExternalDet,_)
	; SeqCp = bot ->
	  sys_error('initial_determinacy/4: seq call pattern is bottom')
	; functor(SeqCp,ExternalDet,_)
	),
	( ExternalDet = det ->
	  NewB = ('$initially_det',RestNewB), InitDet = det
	; ExternalDet = nondet ->
	  NewB = ('$initially_nondet',RestNewB), InitDet = nondet
	; ExternalDet = shallow ->
	  NewB = ('$initially_nondet',RestNewB), InitDet = nondet
	; sys_error('initial_determinacy/4: invalid determinacy ~q',[InitDet])
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Annotating a body means each goal in the body is:
% - supplied with types
% - checked for safeness
% - pseudo-executed to get the changes for a proper environment
%   to the next goal.
%
% *** UNFINISHED ***
% Suspending goals should modify Left for the rec. call to annotate_body.

annotate_body((G,Gs),SeqE,ParE,Left,Seq,Par,Proc,Det,NewB) :- !,
	annotate_and_pseudo_exec_goal(G,SeqE,NewSeqE,ParE,NewParE,Left,NewLeft,
	        Seq,Par,Proc,Det,NewDet,NewB,RestGsTail),
	annotate_body(Gs,NewSeqE,NewParE,NewLeft,Seq,Par,
	              Proc,NewDet,RestGsTail).

annotate_body(G,SeqE,ParE,Left,Seq,Par,Proc,Det,NewB) :- !,
	annotate_and_pseudo_exec_goal(G,SeqE,_NewSeqE,ParE,_NewParE,Left,_NewLeft,
	        Seq,Par,Proc,Det,_NewDet,NewB,true).

%%%	annotate_goal(G,SeqE,ParE,Left,NewB),
%%%	check_goal_safeness(Det,NewB,Proc).

%%%%%%%%%%

annotate_and_pseudo_exec_goal(G,SeqE,NewSeqE,ParE,NewParE,Left,NewLeft,
	                      Seq,Par,Proc,Det,NewDet,NewB,RestGsTail) :-
	annotate_goal(G,SeqE,ParE,Left,NewG),
	NewB = (NewG,RestGs),
	check_goal_safeness_and_leftmost(Det,NewG,Proc,Left,NewLeft),
	pseudo_analyse_goal(G,SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,
	                    RestGs,RestGsTail).

%%%%%%%%%%%

annotate_goal(G,SeqE,ParE,Left,NewG) :-
	G =.. [P|Xs],
	annotate_terms(Xs,SeqE,ParE,Left,NewXs),
	NewG =.. [P|NewXs].

%

annotate_terms([],_S,_P,_L,[]).

annotate_terms([X|Xs],SeqE,ParE,Left,[NewX|NewXs]) :-
	annotate_term(X,SeqE,ParE,Left,NewX),
	annotate_terms(Xs,SeqE,ParE,Left,NewXs).

%

annotate_term(C,_,_,_,NewC) :- atomic(C),!, C = NewC.

annotate_term('$VAR'(N),SeqE,ParE,Left,NewX) :- integer(N),!,
	annotate_var(N,SeqE,ParE,Left,NewX).

annotate_term(PX,SeqE,ParE,Left,NewPX) :-
	PX =.. [P|Xs],
	annotate_terms(Xs,SeqE,ParE,Left,NewXs),
	NewPX =.. [P|NewXs].

% Here is where the actual annotation is done.
%
% If the variable is 'leftmost' (i.e. computation may have
% suspended on the variable), then the type is the sequential
% type. Otherwise, there may have been bindings from the right
% and we use the lub of the seq type and the par type.
%
% *** UNFINISHED ***
% Actually, lub can be too imprecise since it also includes bindings
% from the left in the par computation etc.

annotate_var(N,SeqE,ParE,Leftmost,'$VAR'(N,ParType,SeqType,Locality)) :-
	lookup_env(N,SeqE,SeqFullType),
	functor(SeqFullType,SeqType,_),
	lookup_env(N,ParE,ParFullType),
	functor(ParFullType,PrelParType,_),
	symbolic_locality_of_type(ParFullType,Locality),
	( defined(N,Leftmost) ->
	  ParType = SeqType
	; system_type_id(PrelParType) ->
	  ParType = PrelParType
	; t_to_num(SeqType,S),
	  t_to_num(PrelParType,P),
	  num_lub(S,P,Par_num),
	  num_to_t(Par_num,ParType)
	).

%%%%%%%%%%%

check_goal_safeness(X,_,_) :- var(X),!,
	sys_error('check_goal_safeness/3: var arg').

check_goal_safeness(det,_AnnotatedGoal,_) :- !. % In det state, we're OK

check_goal_safeness(nondet,AnnotatedGoal,Proc) :-
	functor(AnnotatedGoal,P,N),
	AnnotatedGoal =.. [_|Xs],
	safeness_requirements(P/N,Safeness),         % defined in safeness.pl
	check_argument_safeness(Xs,Safeness,1,Proc).

% *** UNFINISHED ***
% The variables that are suspended upon must be noted.
% This suspension should be formalized somewhere!

check_goal_safeness_and_leftmost(X,_,_,_,_) :- var(X),!,
	sys_error('check_goal_safeness_and_leftmost/5: var arg').

check_goal_safeness_and_leftmost(det,AnnotatedGoal,_,L0,L1) :- !,
	functor(AnnotatedGoal,P,N),
	suspension_args(P/N,Susp),
	susp_arguments(Susp,AnnotatedGoal,L0,L1).

check_goal_safeness_and_leftmost(nondet,AnnotatedGoal,Proc,L0,L1) :-
	functor(AnnotatedGoal,P,N),
	AnnotatedGoal =.. [_|Xs],
	safeness_and_susp(P/N,Safeness,Susp),
	check_argument_safeness(Xs,Safeness,1,Proc),
	susp_arguments(Susp,AnnotatedGoal,L0,L1).

%%%%%%%%%%%%%%%%%%%%%

check_argument_safeness([],[],_,_).

check_argument_safeness([X|Xs],[Safe|Safes],N,Proc) :-
	check_arg_safeness(Safe,X,N,Proc),
	M is N+1,
	check_argument_safeness(Xs,Safes,M,Proc).

%

check_arg_safeness(safe,_,_,_) :- !.

check_arg_safeness(unsafe,X,N,Proc) :-
	( shared_term(X) ->
	  warning('shared argument ~q to ~q may be conditionally bound',
                  [N,Proc])
	; true
	).

%

shared_term(X) :- atomic(X),!,fail.

shared_term('$VAR'(_,_,_,L)) :- !,
	( L = shared ; L = fragile ).
shared_term(PX) :-
	PX =.. [_|Xs],
	shared_term_list(Xs).

%

shared_term_list([X|Xs]) :-
	( shared_term(X) -> true
	; shared_term_list(Xs) -> true
	).

%%%%%%%%%%%%%%%%%%%%
%
% The argument positions in the list are suspended upon by
% the compiler. If they are variables, the index of the var
% is inserted into the Leftmost dictionary. Otherwise, there
% is no suspension on that arg.

susp_arguments([],_G,L,L).

susp_arguments([N|Ns],G,L0,L2) :-
	arg(N,G,X),
	( X = '$VAR'(Index,_,_,_) ->
	  update(Index,dummy,L0,L1)
	; L0 = L1
	),
	susp_arguments(Ns,G,L1,L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pseudo-analyse a goal to get changes to types &c.

pseudo_analyse_goal(G,SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,Gs0,Gs1) :-
	( primitive(G) ->
	    analyse_primitive(G,[],SeqE,NewSeqE),
	    analyse_primitive(G,[],ParE,NewParE),
	    change_det(Det,NewSeqE,NewDet,Gs0,Gs1)
	; meta_primitive(G) ->
	    pseudo_analyse_meta_primitive(G,SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,Gs0,Gs1)
	; true,
	    pseudo_analyse_user_call(G,SeqE,NewSeqE,Seq,Det,NewDet,Gs0,Gs1),
	    pseudo_analyse_user_call(G,ParE,NewParE,Par,  _,     _,  _,  _)
	).


pseudo_analyse_meta_primitive(call(Goal),SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,Gs0,Gs1) :-
	pseudo_analyse_meta_goal(Goal,SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,Gs0,Gs1,call/1).

pseudo_analyse_meta_primitive(freeze(_,Goal),SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,Gs0,Gs1) :-
	pseudo_analyse_meta_goal(Goal,SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,Gs0,Gs1,freeze/2).

pseudo_analyse_meta_primitive(bagof(_,Goal,_),SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,Gs0,Gs1) :-
	pseudo_analyse_meta_goal(Goal,SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,Gs0,Gs1,bagof/3).

pseudo_analyse_meta_primitive(setof(_,Goal,_),SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,Gs0,Gs1) :-
	pseudo_analyse_meta_goal(Goal,SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,Gs0,Gs1,setof/3).

pseudo_analyse_meta_primitive(findall(_,Goal,_),SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,Gs0,Gs1) :-
	pseudo_analyse_meta_goal(Goal,SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,Gs0,Gs1,findall/3).

pseudo_analyse_meta_primitive(on_exception(_,Goal,Handler),SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,Gs0,Gs2) :-
	PN = on_exception/3,
	pseudo_analyse_meta_goal(Goal,SeqE,ParE,TmpSeqE,TmpParE,Seq,Par,Det,TmpDet,Gs0,Gs1,PN),
	pseudo_analyse_meta_goal(Handler,TmpSeqE,TmpParE,NewSeqE,NewParE,Seq,Par,TmpDet,NewDet,Gs1,Gs2,PN).


pseudo_analyse_meta_goal(Goal,SeqE,ParE,NewSeqE,NewParE,Seq,Par,Det,NewDet,Gs0,Gs1,PN) :-
	( is_var(Goal) ->
	    NewSeqE = SeqE,
	    NewParE = ParE,
	    NewDet  = Det,
	    Gs1 = Gs0,
	    warning('analysis have lost precision due to variable meta call in ~q',[PN])
	; true,
	    pseudo_analyse_user_call(Goal,SeqE,NewSeqE,Seq,Det,NewDet,Gs0,Gs1),
	    pseudo_analyse_user_call(Goal,ParE,NewParE,Par,  _,     _,  _,  _)
	).


% If determinacy changes, insert a ghost primitive that tells us so.

change_det(OldDet,Env,NewDet,Gs,RestGs) :-
	get_comp_state(Env,NewDet),
	( NewDet == OldDet ->
	    Gs = RestGs
	; NewDet == det ->
	    Gs = ('$det',RestGs)
	; NewDet == nondet ->
	    Gs = ('$nondet',RestGs)
	; NewDet == failed ->
	    Gs = RestGs
	; true,
	    sys_error('change_det/5: unknown determinacy information ~q',[NewDet])
	).

% This is a 'procedure call' where the result must be found in the
% supplied memo table.

pseudo_analyse_user_call(G,Env,NewEnv,Table,Det,NewDet,Goals,RestGoals) :-
	functor(G,P,N),
	apply_i_state(Env,G,Cp),!,      % should be det, actually ...
	lookup_cp_sp(P/N,Table,Cp0,Sp),
	( less_than(Cp,Cp0) ->
	  true
	; sys_error('procedure ~q called with ~p, callpat ~p in table',[P/N,'$pat'(Cp),'$pat'(Cp0)])
	),
	update_i_state(G,Sp,Env,NewEnv),!, % should be det ...
	change_det(Det,NewEnv,NewDet,Goals,RestGoals).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This should be done only for predicates called by a parallel predicate.
% It duplicates clauses so that a clause is either entered determinately
% or nondeterminately, but never both.
%
% NB. Disabled for now!

split_clause_entries(V,C,L,N,S,Cls,NewV,NewC,NewL,NewN,NewS,NewCls) :- !,
	NewV = V, NewC = C, NewL = L, NewN = N, NewS = S, NewCls = Cls.

split_clause_entries(V,C,L,N,S,Cls,NewV,NewC,NewL,NewN,NewS,NewCls) :-
	empty_entry_pt_table(Empty),
	entry_pts_of_seq(V,Empty,TableV,NewV),
	entry_pts_of_table(C,TableV,TableC,NewC),
	entry_pts_of_seq(L,TableC,TableL,NewL),
	entry_pts_of_table(N,TableL,TableN,NewN),
	entry_pts_of_table(S,TableN,TableS,NewS),
	duplicate_clauses(TableS,Cls,NewCls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NB. See below for the structure of a table entry.

duplicate_clauses(EntryTable,ClsStruct,NewClsStruct) :-
	ClsStruct =.. [_|Clauses],
	functor(ClsStruct,_,M), N is M+1,
	list_dict(EntryTable,EntryLst),
	duplicate_clause_list(EntryLst,ClsStruct,N,DupClauses),
	append(Clauses,DupClauses,ModClauses),
	NewClsStruct =.. [cls|ModClauses].

%

duplicate_clause_list([],_Cls,_N,[]).

duplicate_clause_list([E|Es],Cls,M,NewCs) :-
	duplicate_entry(E,Cls,M,N,NewCs,RestNewCs),
	duplicate_clause_list(Es,Cls,N,RestNewCs).

% An entry is duplicated if both the Det and NonDet entries are bound.
% In that case, the nondet entry is redirected to a duplicate of the
% clause. As a side-effect of sorts, the entry points of the ix-struct
% are filled in.

duplicate_entry((K,entry(D,ND)),Cls,NxtNum,RetNxtNum,NewCs,RestNewCs) :-
	( ( nonvar(D), nonvar(ND) ) ->
	  arg(K,Cls,NewCls), % find clause to duplicate
	  NewCs = [NewCls|RestNewCs], % insert it
	  D = +(K), % det entry is to old clause
	  ND = +(NxtNum), % nondet entry is to new clause
	  RetNxtNum is NxtNum+1 % next clause is numbered ...
	; nonvar(D) -> % det entry is numbered as key
	  D = +(K),
	  RetNxtNum = NxtNum, RestNewCs = NewCs
	; nonvar(ND) -> % nondet entry is numbered as key
	  ND = +(K),
	  RetNxtNum = NxtNum, RestNewCs = NewCs
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The new sequence is 

entry_pts_of_seq([],T,T,[]).

entry_pts_of_seq([X|Xs],T0,T2,NewSeq) :-
	entry_of_seq(Xs,X,T0,T2,NewSeq).

%

entry_pts_of_table([],T0,T1,NewTable) :- T0 = T1, NewTable = [].

entry_pts_of_table([X|Xs],T0,T2,NewTable) :-
	entry_pts_of_seq([X|Xs],T0,T2,NewTable).

entry_pts_of_table(table(Cases,Dflt,Summary),T0,T2,
	           table(NewCases,NewDflt,Summary)) :-
        entry_pts_of_cases(Cases,T0,T1,NewCases),
	entry_pts_of_seq(Dflt,T1,T2,NewDflt).

%

entry_pts_of_cases([],T,T,[]).

entry_pts_of_cases([(C,Xs)|Cases],T0,T2,[(C,NewXs)|NewCases]) :-
	entry_pts_of_seq(Xs,T0,T1,NewXs),
	entry_pts_of_cases(Cases,T1,T2,NewCases).

%

entry_of_seq([],X,T0,T1,NewSeq) :-
	det_entry(X,T0,T1,D),
	NewSeq = [D].

entry_of_seq([X|Xs],Y,T0,T2,NewSeq) :-
	nondet_entry(Y,T0,T1,ND),
	NewSeq = [ND|NewSeqRest],
	entry_of_seq(Xs,X,T1,T2,NewSeqRest).

% An entry looks like:
%   entry(+DetLbl,+NondetLbl) [i.e. structure +(X) ]
% if the arg is a variable, the clause is not entered
% either determinately or nondeterminately. If the clause
% has no entry, it is not entered.

empty_entry_pt_table(X) :- empty_dict(X).

det_entry(X,T0,T1,D) :-
	( lookup(X,T0,entry(+D,_)) ->
	  T0 = T1
	; insert(X,entry(+D,_),T0,T1)
	).

nondet_entry(X,T0,T1,ND) :-
	( lookup(X,T0,entry(_,+ND)) ->
	  T0 = T1
	; insert(X,entry(_,+ND),T0,T1)
	).

%

portray('$VAR'(N,P,S,L)) :-
	format('~p_~p_~p_~p',['$VAR'(N),P,S,L]).
