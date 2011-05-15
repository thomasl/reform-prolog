%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		   ANNOTATING PREDICATES FOR USERS
%
% Annotates clauses with information selected by user.
%

:- ensure_loaded('../util/equivclass').

% By domain-dep code:

:- ensure_loaded(absindex).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user_annot(Aspects,File) :-
	Opts = [],
	restore_analysis_results(Opts,CDB,_Exp,Seq,Par),
	expand_aspects(Aspects,All_aspects),
	user_annot_database(Opts,All_aspects,CDB,Seq,Par,AnnotLst),
	absolute_file_name(File,FileName),
	open(FileName,write,Wr),
	display_annot_list(AnnotLst,Wr),
	close(Wr).

%

expand_aspects(Aspects,All_aspects) :-
	exp_asp(Aspects,All_asp,[]),
	sort(All_asp,All_aspects).

exp_asp([]) --> [].
exp_asp([X|Xs]) --> exp_a(X), exp_asp(Xs).

exp_a(all) --> !,[determinacy,types,
	          certain_aliases,possible_aliases,independence].
exp_a(aliases) --> !,[certain_aliases,possible_aliases].
exp_a(X) --> [X].

%

user_annot_database(_Opts,Aspects,CDB,Seq,Par,AnnotLst) :-
	list_dict(CDB,Lst),
	user_annot_list(Lst,Aspects,Seq,Par,AnnotLst).

user_annot_list([],_Asp,_S,_P,[]).
user_annot_list([(PN,Ix)|Xs],Aspects,Seq,Par,[AnnotPred|AnnotPreds]) :-
	format(user_error,'(~q',[PN]),
	user_annot_pred(PN,Ix,Aspects,Seq,Par,AnnotPred),
	format(user_error,')',[]),
	user_annot_list(Xs,Aspects,Seq,Par,AnnotPreds).

display_annot_list([],_).
display_annot_list([(PN,Clss,Cp,Sps,Sp)|Xs],Wr) :-
	display_annot_pred(Wr,PN,Clss,Cp,Sps,Sp),
	display_annot_list(Xs,Wr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Pseudo-execute a predicate and annotate it.
%
% There are several operations used to instantiate this 'framework':
%  select_clauses/3
%  initial_info/3
%  call_info/3
%  annotate_with_info/3
%  annotate_changes/3

user_annot_pred(PN,Ix,Aspects,Seq,Par,
	        (PN,Clss,(SeqCp,ParCp),Sps,(SeqSp,ParSp))) :-
	( lookup(PN,Seq,(SeqCp,SeqSp)) ->
	  ( lookup(PN,Par,(ParCp,ParSp)) ->
	    par_pseudo_exec_predicate(Ix,SeqCp,ParCp,Seq,Par,
	                              Aspects,Clss,Sps)
	  ; pseudo_exec_predicate(Ix,SeqCp,Seq,Aspects,Clss,Sps)
	  )
	; warning('% predicate ~q not found in sequential memo~n',[PN])
	).

%

pseudo_exec_predicate(PredStruct,Cp,Memo,Templ,AnnotClss,Sps) :-
	callpatterns_of_clauses(PredStruct,Cp,ClsCpPairs),
	pseudo_exec_clauses(ClsCpPairs,Memo,Templ,AnnotClss,Sps).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SEQUENTIAL VERSION (one memo table)

pseudo_exec_clauses([],_M,_T,[],[]).

pseudo_exec_clauses([(Cp,CE)|Xs],Memo,Templ,[AnnotCls|AnnotClss],[Sp|Sps]) :-
	( Cp = bot ->
	  CE = (C,_E),
	  AnnotCls = not_called(C)
	; AnnotCls = (Cp,AnnotC),
	  copy_term(Cp,CpCopy),
	  pseudo_exec_clause(CE,CpCopy,Memo,Templ,AnnotC,Sp)
	),
	pseudo_exec_clauses(Xs,Memo,Templ,AnnotClss,Sps).

%

pseudo_exec_clause(CE,Cp,Memo,Templ,AnnotCls,Sp) :-
	initial_i_state(CE,Cp,H,B,A0),
	initial_info(A0,Templ,AfterHead),
	annotate_with_info(Templ,AfterHead,AnnotBody,AnnotGoals),
	pseudo_exec_body(B,A0,An,Memo,Templ,AfterHead,AnnotGoals),
	AnnotCls = (H :- AnnotBody),
	final_i_state(An,H,Sp).

%

pseudo_exec_body((G,Gs),A0,An,Memo,Templ,Info,AnnotBody) :- !,
	pseudo_exec_goal(G,NewG,Failed,A0,A1,Memo),
	( Failed = fail ->
	  AnnotBody = (NewG,{'FAILURE'},Gs)
	; call_info(A1,Templ,GoalInfo),
	  AnnotBody = (NewG,RestAnnot),
	  annotate_changes(Templ,GoalInfo,Info,RestAnnot,AnnotGoals),
	  pseudo_exec_body(Gs,A1,An,Memo,Templ,GoalInfo,AnnotGoals)
	).

pseudo_exec_body(G,A0,An,Memo,Templ,Info,AnnotBody) :-
	pseudo_exec_goal(G,NewG,Failed,A0,An,Memo),
	( Failed = fail ->
	  AnnotBody = (NewG,{'FAILURE'})
	; call_info(An,Templ,CallInfo),
	  AnnotBody = (NewG,RestAnnot),
	  annotate_changes(Templ,CallInfo,Info,RestAnnot,true)
	).

%

pseudo_exec_goal(G,NewG,Failed,A0,A1,Memo) :-
	( primitive(G) ->
	  analyse_primitive(G,[],A0,A1,sequential),
	  NewG = G,
	  get_comp_state(A1,State),
	  ( State = failed ->
	    Failed = fail
	  ; Failed = success
	  )
	; functor(G,P,N),
	  lookup(P/N,Memo,(Cp,Sp)),
	  apply_i_state(A0,G,CallCp),
	  ( less_than(CallCp,Cp) ->
	    NewG = G
	  ; NewG = 'Call not in memotable':G
	  ),
	  ( Sp = bot ->
	    Failed = fail
	  ; Failed = success,
	    copy_term(Sp,SpCopy),
	    update_i_state(G,SpCopy,A0,A1)
	  )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Pseudo-execute a PARALLEL predicate and annotate it.
%

par_pseudo_exec_predicate(PredStruct,SeqCp,ParCp,Seq,Par,
	                  Templ,AnnotClss,Sps) :-
	callpatterns_of_clauses(PredStruct,SeqCp,SeqClsCpPairs),
	callpatterns_of_clauses(PredStruct,ParCp,ParClsCpPairs),
	combine_called_clauses(SeqClsCpPairs,ParClsCpPairs,ClsCpPairs),
	par_pseudo_exec_clauses(ClsCpPairs,Seq,Par,Templ,AnnotClss,Sps).

% The callpattern_of_clauses ensures that clauses occur in the same order.
% I think.

combine_called_clauses([],[],[]).
combine_called_clauses([(SeqCp,CE1)|Seqs],[(ParCp,CE2)|Pars],
	               [(SeqCp,ParCp,CE1)|Combineds]) :-
        ( CE1 == CE2 ->
	  combine_called_clauses(Seqs,Pars,Combineds)
	; sys_error('combine_called_clauses/3: not same clause order ~q and ~q~n',
	        [CE1,CE2])
	).

%

par_pseudo_exec_clauses([],_S,_P,_T,[],[]).

par_pseudo_exec_clauses([(SeqCp,ParCp,CE)|Xs],Seq,Par,Templ,
	                [AnnotCls|AnnotClss],[(SeqSp,ParSp)|Sps]) :-
	( SeqCp = bot ->
	  ( ParCp = bot ->
	    CE = (C,_E),
	    AnnotCls = not_called(C)
	  ; warning('clause (~q) has par cp but no seq cp~n',[C])
	  )
	; ParCp = bot ->
	  warning('clause (~q) has seq cp but no par cp -- impossible~n',[C])
	; AnnotCls = (SeqCp,ParCp,AnnotC),
	  copy_term(SeqCp,SeqCpCopy),
	  copy_term(ParCp,ParCpCopy),
	  par_pseudo_exec_clause(CE,SeqCpCopy,ParCpCopy,
	                         Seq,Par,Templ,AnnotC,SeqSp,ParSp)
	),
	par_pseudo_exec_clauses(Xs,Seq,Par,Templ,AnnotClss,Sps).

%

par_pseudo_exec_clause(CE,SeqCp,ParCp,Seq,Par,Templ,AnnotCls,SeqSp,ParSp) :-
	initial_i_state(CE,SeqCp,H,B,A0),
	copy_term(CE,CE1),
	initial_i_state(CE1,ParCp,_H,_B,B0),
	initial_info(A0,Templ,SeqI),
	initial_info(B0,Templ,ParI),
	annotate_with_info(Templ,SeqI,ParI,AnnotBody,AnnotGoals),
	par_pseudo_exec_body(B,A0,An,B0,Bn,Seq,Par,
	                     Templ,SeqI,ParI,AnnotGoals),
	AnnotCls = (H :- AnnotBody),
	final_i_state(An,H,SeqSp),
	final_i_state(Bn,H,ParSp).

%

par_pseudo_exec_body((G,Gs),A0,An,B0,Bn,Seq,Par,Templ,SeqI,ParI,AnnotB) :- !,
	par_pseudo_exec_goal(G,NewG,SeqF,ParF,A0,A1,B0,B1,Seq,Par),
	( SeqF = fail ->
	  ( ParF = fail ->
	    AnnotB = (NewG,{'SEQ AND PAR FAILURE'},Gs)
	  ; AnnotB = (NewG,{'SEQ FAILURE'},Gs)
	  )
	; ParF = fail ->
	  AnnotB = (NewG,{'PAR FAILURE'},Gs)
	; call_info(A1,Templ,SeqInfo),
	  call_info(B1,Templ,ParInfo),
	  AnnotB = (NewG,RestAnn),
	  annotate_changes(Templ,SeqInfo,SeqI,ParInfo,ParI,RestAnn,AnnotGs),
	  par_pseudo_exec_body(Gs,A1,An,B1,Bn,Seq,Par,
	                       Templ,SeqInfo,ParInfo,AnnotGs)
	).

par_pseudo_exec_body(G,A0,An,B0,Bn,Seq,Par,Templ,SeqI,ParI,AnnotBody) :-
	par_pseudo_exec_goal(G,NewG,SeqF,ParF,A0,An,B0,Bn,Seq,Par),
	( SeqF = fail ->
	  ( ParF = fail ->
	    AnnotBody = (NewG,{'SEQ AND PAR FAILURE'})
	  ; AnnotBody = (NewG,{'SEQ FAILURE'})
	  )
	; ParF = fail ->
	  AnnotBody = (NewG,{'PAR FAILURE'})
	; call_info(An,Templ,SeqInfo),
	  call_info(Bn,Templ,ParInfo),
	  AnnotBody = (NewG,RestAnnot),
	  annotate_changes(Templ,SeqInfo,SeqI,ParInfo,ParI,RestAnnot,true)
	).

%

par_pseudo_exec_goal(G,NewG,SeqF,ParF,A0,A1,B0,B1,Seq,Par) :-
	( primitive(G) ->
	  primitive_i_state(G,SeqNewG,A0,A1,SeqF,sequential),
	  primitive_i_state(G,ParNewG,B0,B1,ParF,parallel),
	  combine_goals(SeqNewG,ParNewG,NewG)
	; new_i_state(G,SeqNewG,A0,A1,SeqF,Seq),
	  new_i_state(G,ParNewG,B0,B1,ParF,Par),
	  combine_goals(SeqNewG,ParNewG,NewG)
	).

% Retain annotations from both par and seq if necessary.

combine_goals(Seq:G1,Par:_G2,NewG) :- !, NewG = seq(Seq):par(Par):G1.
combine_goals(Seq:G1,_G2,NewG) :- !, NewG = seq(Seq):G1.
combine_goals(_G1,Par:G2,NewG) :- !, NewG = par(Par):G2.
combine_goals(G1,_G2,G1).

% pseudo-exec primitive

primitive_i_state(G,NewG,A0,A1,Failed,Mode) :- 
	  analyse_primitive(G,[],A0,A1,Mode),
	  NewG = G,
	  get_comp_state(A1,State),
	  ( State = failed ->
	    Failed = fail
	  ; Failed = success
	  ).

% Create new i-state from given memo table and old i-state.

new_i_state(G,NewG,A0,A1,Failed,Memo) :-
	functor(G,P,N),
	lookup(P/N,Memo,(Cp,Sp)),
	apply_i_state(A0,G,CallCp),
	( less_than(CallCp,Cp) ->
	  NewG = G
	; NewG = 'Call not in memotable':G
	),
	( Sp = bot ->
	  Failed = fail
	; Failed = success,
	  copy_term(Sp,SpCopy),
	  update_i_state(G,SpCopy,A0,A1)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Below are implementations and documentation for the generic
% procedures to be supplied.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% callpatterns_of_clauses/3 takes a predicate definition and a call pattern,
% and returns a list of (Clause,CallPattern) pairs. If a clause is
% not called (e.g. due to abstract indexing) its call pattern should
% be 'bot'. (If the clause is not included, it will not appear in the
% output!)
%
% This can be simple or more involved.

callpatterns_of_clauses(Info,Cp,CpCEs) :-
	Info = info(IxStr,_Ds,_F),
	absindex_ret_all_clauses(Cp,IxStr,CpCEs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Extracting information from abstract substitutions.
%
% We assume the template is a list of aspects to be collected.
% The allowed aspects

initial_info(Env,Aspects,Info_after_head) :- 
	call_info(Env,Aspects,Info_after_head).


call_info(Env,Asp,CallInfo) :-
	copy_term(Env,CopyEnv),
	components_of_env(CopyEnv,State,Vars),
	aspects_of_call_info(Asp,State,Vars,CallInfo).

% Collect info in a list, one entry per template entry

aspects_of_call_info([],_S,_V,[]).
aspects_of_call_info([A|As],S,V,[Info|Infos]) :-
	aspect_of_call_info(A,S,V,Info),
	aspects_of_call_info(As,S,V,Infos).

% Here are the aspects we have defined for our domain:

aspect_of_call_info(determinacy,[_env,_OrigDet,Det,_Inst,_ArgInst],_Vs,Det).

aspect_of_call_info(types,_S,Vs,Types) :-
	first_var_entry_of_env(N),
	extract_types(Vs,N,Types,[]).

aspect_of_call_info(independence,_S,Vs,Indep) :-
	first_var_entry_of_env(N),
	extract_independence(Vs,N,Indep,[]).

aspect_of_call_info(locality,_S,Vs,Loc) :-
	first_var_entry_of_env(N),
	extract_locality(Vs,N,Loc,[]).

aspect_of_call_info(certain_aliases,_S,Vs,Cert) :-
	first_var_entry_of_env(N),
	extract_certain_aliases(Vs,N,Cert).

aspect_of_call_info(possible_aliases,_S,Vs,Poss) :-
	first_var_entry_of_env(N),
	extract_possible_aliases(Vs,N,Poss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_types([],_) --> [].

extract_types([X|Xs],N) -->
	extract_type(X,N),
	{ M is N+1 },
	extract_types(Xs,M).

%

extract_type(X,N) -->
	{ norm(X,_,NormX),
	  functor(NormX,T,_) },
	[(N,T)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_independence([],_) --> [].

extract_independence([X|Xs],N) -->
	extract_ind(X,N),
	{ M is N+1 },
	extract_independence(Xs,M).

%

extract_ind(det,N) --> !,[(N,trivial)].
extract_ind(nondet,N) --> !,[(N,trivial)].
extract_ind(uninit,N) --> !,[(N,trivial)].
extract_ind(X,N) -->
	{ norm(X,_,NormX) },
	( { list_type(NormX,_,Hd_attr,_,Tl_attr) } ->
	  ind_from_attr(Hd_attr,hd(N)),
	  ind_from_attr(Tl_attr,tl(N))
	; { simple_type(NormX,_,Attr) } ->
	  ind_from_attr(Attr,N)
	).

ind_from_attr(none,N) --> [(N,trivial)].
ind_from_attr(a(_P,S,_L),N) -->
	{ ind_to_atom(S,Ind) },
	[(N,Ind)].
ind_from_attr(b(_C,_P,_L),N) --> [(N,trivial)].

ind_to_atom(Ind,Atm) :-
	( linear(Ind) -> Atm = linear
	; indlist(Ind) -> Atm = indlist
	; nonlinear(Ind) -> Atm = nonlinear
	; sys_error('ind_to_atom/2: linearity unknown for ~q~n',[Ind])
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_locality([],_) --> [].

extract_locality([X|Xs],N) -->
	extract_loc(X,N),
	{ M is N+1 },
	extract_locality(Xs,M).

%

extract_loc(det,N) --> !,[(N,trivial)].
extract_loc(nondet,N) --> !,[(N,trivial)].
extract_loc(uninit,N) --> !,[(N,trivial)].
extract_loc(X,N) -->
	{ norm(X,_,NormX) },
	( { list_type(NormX,_,Hd_attr,_,Tl_attr) } ->
	  loc_from_attr(Hd_attr,hd(N)),
	  loc_from_attr(Tl_attr,tl(N))
	; { simple_type(NormX,_,Attr) } ->
	  loc_from_attr(Attr,N)
	).

loc_from_attr(none,N) --> [(N,trivial)].
loc_from_attr(a(_P,_S,L),N) -->
	{ loc_to_atom(L,Loc) },
	[(N,Loc)].
loc_from_attr(b(_C,_P,L),N) -->
	{ loc_to_atom(L,Loc) },
	[(N,Loc)].

loc_to_atom(Loc,Atm) :-
	( local(Loc) -> Atm = local
	; shared(Loc) -> Atm = shared
	; wbf(Loc) -> Atm = wbf
	; fragile(Loc) -> Atm = fragile
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copy, normalize elts, extract what alias class a descriptor belongs to,
% form sets of descriptors.

extract_certain_aliases(Vs,N,Cert) :-
	copy_term(Vs,Us),
	norm_var_list(Us,Ws),
	cert_alias_classes(Ws,N,1,_,CAC,[]),
	form_alias_sets(CAC,Cert).

% First normalize all elts. Afterwards, variables are instantiated
% and bound to numbers, which are not recognized by the normalizer.
% (The copy_term/2 above is a bit of a blemish, since we would
% like to reuse the normalized values if possible aliases are employed.
% However, that would break modularity and has not shown itself
% critical yet.)

norm_var_list([],[]).
norm_var_list([X|Xs],[Y|Ys]) :-
	norm(X,_,Y),
	norm_var_list(Xs,Ys).

% AC is a counter for naming new alias classes (by instantiation)

cert_alias_classes([],_N,AC,AC) --> [].

cert_alias_classes([V|Vs],N,AC0,AC2) -->
	cert_alias_cls(V,N,AC0,AC1),
	{ M is N+1 },
	cert_alias_classes(Vs,M,AC1,AC2).

%

cert_alias_cls(uninit,N,AC0,AC1) --> !,
	{ AC1 is AC0+1 },
	[(AC0,N)].

cert_alias_cls(det,_N,AC0,AC1) --> !,
	{ AC0 = AC1 }.

cert_alias_cls(nondet,_N,AC0,AC1) --> !,
	{ AC0 = AC1 }.

cert_alias_cls(X,N,AC0,AC2) -->
	( { list_type(X,_,Hd_attr,_,Tl_attr) } ->
	  cert_alias_cls(Hd_attr,hd(N),AC0,AC1),
	  cert_alias_cls(Tl_attr,tl(N),AC1,AC2)
	; { simple_type(X,_,Attr) } ->
	  cert_alias_cls(Attr,N,AC0,AC2)
	).

% Each aliasing variable C forms an equivalence class, which is
% made explicit here as (C,Descriptor) pairs. All Descriptors
% with C are certainly aliased.

cert_alias_cls(none,_N,AC,AC) --> [].
cert_alias_cls(a(_P,_S,_L),_N,AC,AC) --> [].
cert_alias_cls(b(C,_P,_L),N,AC0,AC1) -->
	( { integer(C) } ->
	  [(C,N)], { AC0 = AC1 }
	; { var(C) } ->
	  { C = AC0, AC1 is AC0+1 },
	  [(C,N)]
	; { sys_error('cert_alias_cls/6: ~q not var or int~n',[C]) }
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% We now form alias sets by equivalence classes.

form_alias_sets(Pairs,AliasSets) :-
	equivalence_classes(Pairs,AliasSets).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copy, normalize elts, extract what alias class a descriptor belongs to,
% form sets of descriptors.

extract_possible_aliases(Vs,N,Cert) :-
	copy_term(Vs,Us),
	norm_var_list(Us,Ws), % see extract_certain_aliases
	poss_alias_classes(Ws,N,1,_,CAC,[]),
	form_alias_sets(CAC,Cert).

% AC is a counter for naming new alias classes (by instantiation)

poss_alias_classes([],_N,AC,AC) --> [].

poss_alias_classes([V|Vs],N,AC0,AC2) -->
	poss_alias_cls(V,N,AC0,AC1),
	{ M is N+1 },
	poss_alias_classes(Vs,M,AC1,AC2).

%

poss_alias_cls(uninit,N,AC0,AC1) --> !,
	{ AC1 is AC0+1 },
	[(AC0,N)].

poss_alias_cls(det,_N,AC0,AC1) --> !,
	{ AC0 = AC1 }.

poss_alias_cls(nondet,_N,AC0,AC1) --> !,
	{ AC0 = AC1 }.

poss_alias_cls(X,N,AC0,AC2) -->
	( { list_type(X,_,Hd_attr,_,Tl_attr) } ->
	  poss_alias_cls(Hd_attr,hd(N),AC0,AC1),
	  poss_alias_cls(Tl_attr,tl(N),AC1,AC2)
	; { simple_type(X,_,Attr) } ->
	  poss_alias_cls(Attr,N,AC0,AC2)
	).

% Each aliasing variable P forms an equivalence class, which is
% made explicit here as (P,Descriptor) pairs. All Descriptors
% with P are possibly aliased.

poss_alias_cls(none,_N,AC,AC) --> [].
poss_alias_cls(a(P,_S,_L),N,AC0,AC1) -->
	( { integer(P) } ->
	  [(P,N)], { AC0 = AC1 }
	; { var(P) } ->
	  { P = AC0, AC1 is AC0+1 },
	  [(P,N)]
	; { sys_error('poss_alias_cls/6: ~q not var or int~n',[P]) }
        ).
poss_alias_cls(b(_C,P,_L),N,AC0,AC1) -->
	( { integer(P) } ->
	  [(P,N)], { AC0 = AC1 }
	; { var(P) } ->
	  { P = AC0, AC1 is AC0+1 },
	  [(P,N)]
	; { sys_error('poss_alias_cls/6: ~q not var or int~n',[P]) }
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Annotation by finding the changes between calls.
% "Change" is defined as either pointwise (i.e. one var has changed,
% new info is shown) or inequality (i.e. the set of aliases has changed,
% so show it).

% sequential version (one info set):

annotate_with_info(Aspects,AfterHead,AnnotatedBody,RestOfBody) :-
	annotate_aspects(Aspects,AfterHead,Annot,[]),
	annot_to_group(Annot,Grp),
	( Grp = {} ->
	  AnnotatedBody = RestOfBody
	; AnnotatedBody = (Grp,RestOfBody)
	).

% parallel version (two info sets):

annotate_with_info(Aspects,SeqI,ParI,AnnotatedBody,RestOfBody) :-
	annotate_aspects(Aspects,SeqI,SeqAnnot,[]),
	annot_to_group(SeqAnnot,SeqGrp),
	annotate_aspects(Aspects,ParI,ParAnnot,[]),
	annot_to_group(ParAnnot,ParGrp),
	( SeqGrp = {} ->
	  ( ParGrp = {} ->
	    AnnotatedBody = RestOfBody
	  ; AnnotatedBody = (par:ParGrp,RestOfBody)
	  )
	; ( ParGrp = {} ->
	    AnnotatedBody = (seq:SeqGrp,RestOfBody)
	  ; AnnotatedBody = (seq:SeqGrp,par:ParGrp,RestOfBody)
	  )
	).

annotate_aspects([],[]) --> [].
annotate_aspects([A|As],[X|Xs]) -->
	annotate_aspect(A,X),
	annotate_aspects(As,Xs).

% sequential version (one info set):

annotate_changes(Aspects,Curr,Prev,AnnotatedBody,RestOfBody) :-
	annotate_aspect_changes(Aspects,Curr,Prev,Annot,[]),
	annot_to_group(Annot,Grp),
	( Grp = {} ->
	  AnnotatedBody = RestOfBody
	; AnnotatedBody = (Grp,RestOfBody)
	).

% parallel version (two info sets):

annotate_changes(Aspects,SeqCurr,SeqPrev,ParCurr,ParPrev,
	         AnnotatedBody,RestOfBody) :-
	annotate_aspect_changes(Aspects,SeqCurr,SeqPrev,SeqAnnot,[]),
	annot_to_group(SeqAnnot,SeqGrp),
	annotate_aspect_changes(Aspects,ParCurr,ParPrev,ParAnnot,[]),
	annot_to_group(ParAnnot,ParGrp),
	( SeqGrp = {} ->
	  ( ParGrp = {} ->
	    AnnotatedBody = RestOfBody
	  ; AnnotatedBody = (par:ParGrp,RestOfBody)
	  )
	; ( ParGrp = {} ->
	    AnnotatedBody = (seq:SeqGrp,RestOfBody)
	  ; AnnotatedBody = (seq:SeqGrp,par:ParGrp,RestOfBody)
	  )
	).

%

annotate_aspect_changes([],[],[]) --> [].
annotate_aspect_changes([A|As],[C|Cs],[P|Ps]) -->
	annotate_aspect_change(A,C,P),
	annotate_aspect_changes(As,Cs,Ps).

%

annotate_aspect(A,X) --> [(A,X)].

% Changes are noted either by:
% - inequality
% - pointwise inequality

annotate_aspect_change(Asp,Curr,Prev) -->
	{ Asp = determinacy 
	; Asp = certain_aliases 
	; Asp = possible_aliases },!,
	( { Curr == Prev } ->
	  []
	; [(Asp,Curr)]
	).
annotate_aspect_change(Asp,Curr,Prev) -->
	{ Asp = types ; Asp = independence ; Asp = locality },!,
	{ eltwise_diff(Curr,Prev,Diff,[]) },
	( { Diff = [] } ->
	  []
	; [(Asp,Diff)]
	).

% Note that some elements may have appeared, e.g. if a type
% has become a list or somesuch. If an element in Curr does not
% appear in Prev, it is put up as a difference. If an element in
% Curr appears in Prev but is not equal, that's a difference as
% well.
%
% At present, elements in Prev that have disappeared in Curr
% are not shown. This could be corrected, but note that e.g.
% an element (1,...) should not be shown if (hd(1),...) appears
% in Curr.

eltwise_diff([],_Prev) --> [].

eltwise_diff([(K,V)|Xs],Prev) -->
	( { key_member_then_elt(Prev,K,V1) } ->
	  ( { V == V1 } ->
	    []
	  ; [(K,V)]
	  )
	; [(K,V)]
	),
	eltwise_diff(Xs,Prev).

%

key_member_then_elt([(K1,V1)|Xs],K,V) :-
	( K == K1 ->
	  V = V1
	; key_member_then_elt(Xs,K,V)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Finally, things are converted from a list
%   [(Aspect,Stuff),...]
% into a 'writeable' group { Aspect: Stuff, ... }

annot_to_group([],X) :- !, X = {}.

annot_to_group([(Aspect,Info)],X) :- !,
	X = {Ys},
	info_to_group(Aspect,Info,Ys).

annot_to_group(Asps,X) :-
	annot_list_of_info(Asps,List,[]),
	list_to_conj(List,Conj),
	X = {Conj}.

annot_list_of_info([]) --> [].
annot_list_of_info([(Asp,Info)|Asps]) -->
	{ info_to_group(Asp,Info,Ys) },
	[Asp:Ys],
	annot_list_of_info(Asps).

%

info_to_group(determinacy,X,X).
info_to_group(types,Ts,Xs) :- vars_have_props(Ts,Xs).
info_to_group(independence,Inds,Xs) :- vars_have_props(Inds,Xs).
info_to_group(locality,Locs,Xs) :- vars_have_props(Locs,Xs).
info_to_group(certain_aliases,Certs,Xs) :- alias_sets(Certs,Xs).
info_to_group(possible_aliases,Poss,Xs) :- alias_sets(Poss,Xs).

%

vars_have_props([],[]).
vars_have_props([(N,Prop)|Xs],['$VAR'(N)=Prop|Ys]) :-
	vars_have_props(Xs,Ys).

alias_sets([],[]).
alias_sets([X|Xs],[Y|Ys]) :- alias_set(X,Y), alias_sets(Xs,Ys).

alias_set([],X) :- !, X = {}.
alias_set(Ns,{Set}) :-
	indx_to_vars(Ns,Lst),
	list_to_conj(Lst,Set).

indx_to_vars([],[]).
indx_to_vars([N|Ns],['$VAR'(N)|Xs]) :- indx_to_vars(Ns,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Here are the printing routines.
% Each predicate is here represented as 
% - a functor P/N
% - a list of clauses, either not_called(C) or (Cp,C)
% - a list of success patterns

 % parallel case:

display_annot_pred(Wr,PN,Clss,(SeqCp,ParCp),Sps,(SeqSp,ParSp)) :- !,
	crop_pattern(SeqCp,SeqCp1),
	crop_pattern(ParCp,ParCp1),
	crop_pattern(SeqSp,SeqSp1),
	crop_pattern(ParSp,ParSp1),
	format(Wr,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n',[]),
	format(Wr,'% predicate ~q~n',[PN]),
        format(Wr,'% call pattern:~n%     seq: ~p~n%     par: ~p~n',
               [SeqCp1,ParCp1]),
        format(Wr,'% success pattern:~n%     seq: ~p~n%     par: ~p~n',
               [SeqSp1,ParSp1]),
        format(Wr,'% clause success patterns:~n',[]),
        display_succpats(Sps,1,Wr),
	format(Wr,'~n',[]),
	display_annot_clss(Clss,1,Wr),
	format(Wr,'~n',[]).

 % sequential case:

display_annot_pred(Wr,PN,Clss,Cp,Sps,Sp) :-
	crop_pattern(Cp,Cp1),
	crop_pattern(Sp,Sp1),
	format(Wr,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n',[]),
	format(Wr,'% predicate ~q~n',[PN]),
        format(Wr,'% call pattern: ~p~n',[Cp1]),
        format(Wr,'% success pattern: ~p~n',[Sp1]),
        format(Wr,'% clause success patterns:~n',[]),
        display_succpats(Sps,1,Wr),
	format(Wr,'~n',[]),
	display_annot_clss(Clss,1,Wr),
	format(Wr,'~n',[]).

display_succpats([],_,_).
display_succpats([Sp|Sps],N,Wr) :-
	( Sp = (SeqSp,ParSp) ->
	  crop_pattern(SeqSp,SeqSp1),
	  crop_pattern(ParSp,ParSp1),
	  format(Wr,'% ~d: seq: ~p~n%    par: ~p~n',[N,SeqSp1,ParSp1])
        ; crop_pattern(Sp,Sp1),
	  format(Wr,'% ~d: ~p~n',[N,Sp1])
        ),
	M is N+1,
	display_succpats(Sps,M,Wr).


display_annot_clss([],_,_).
display_annot_clss([Cls|Clss],N,Wr) :-
	display_annot_cls(Cls,N,Wr),
	M is N+1,
	display_annot_clss(Clss,M,Wr).

%

display_annot_cls(not_called(C),M,Wr) :- !,
	format(Wr,'% clause ~d: NOT CALLED~n',[M]),
        display_annot_clause(C,Wr).

display_annot_cls((SeqCp,ParCp,C),M,Wr) :- !,
	crop_pattern(SeqCp,SeqCp1),
	crop_pattern(ParCp,ParCp1),
	format(Wr,'% clause ~d: seq: ~p~n',[M,SeqCp1]),
        format(Wr,'%           par: ~p~n',[ParCp1]),
        display_annot_clause(C,Wr).

display_annot_cls((Cp,C),M,Wr) :-
	crop_pattern(Cp,Cp1),
	format(Wr,'% clause ~d: ~p~n',[M,Cp1]),
        display_annot_clause(C,Wr).

%

display_annot_clause((H :- B),Wr) :-
	format(Wr,'~p :-~n',[H]),
	eliminate_empty_goals(B,TrimmedB),
	display_annot_body(TrimmedB,Wr).

%

display_annot_body((G,Gs),Wr) :- !,
	format(Wr,'   ~p,~n',[G]),
	display_annot_body(Gs,Wr).

display_annot_body(G,Wr) :-
	format(Wr,'   ~p.~n~n',[G]).

%

crop_pattern(shr(_,_,Tuple),Tuple) :- !, enumeratevars(Tuple,0,_).
crop_pattern(shr(_,Tuple),Tuple) :- !, enumeratevars(Tuple,0,_).
crop_pattern(Tuple,Tuple) :- enumeratevars(Tuple,0,_).

%

eliminate_empty_goals(Gs,NewGoals) :-
	elim_empty_goals(Gs,NewGs),
	( var(NewGs) ->
	  NewGoals = true
	; NewGoals = NewGs
	).

% Kill all 'true'-goals

elim_empty_goals((G,Gs),NewGoals) :- !,
	( G = true ->
	  elim_empty_goals(Gs,NewGoals)
	; elim_empty_goals(Gs,NewGsTail),
	  ( var(NewGsTail) ->
	    NewGoals = G
	  ; NewGoals = (G,NewGsTail)
	  )
	).

elim_empty_goals(Goal,NewGoal) :- !,
	( Goal = true ->
	  true % "return a variable"
	; NewGoal = Goal
	).

%

portray('$VAR'(hd(N))) :- integer(N), format('~p_hd',['$VAR'(N)]).
portray('$VAR'(tl(N))) :- integer(N), format('~p_tl',['$VAR'(N)]).
