%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  load-rp-file.pl
%
%
%   This file contains the info to load programs into the database properly.
% It also constructs the indexing structure of the predicate.
%
%

:- ensure_loaded(index).
:- ensure_loaded('load-file').
:- ensure_loaded('../analysis/empty-env').
:- ensure_loaded('../compilation/reductions').


fload_desugar_program(Fs,DB) :-
	fload_desugar(Fs,DB0),
	ground_clauses_and_ix(DB0,DB).

fload_desugar_program(Fs,DB,Callables,Exports,Directives) :-
	fload_desugar(Fs,DB,Directives),
	find_callable_and_export_predicates(DB,Callables,Exports).

fload_desugar_program_one(Fs,DB,Callables,Exports,Directives) :-
	fload_desugar_one(Fs,DB,Directives),
	find_callable_and_export_predicates(DB,Callables,Exports).



find_callable_and_export_predicates(DB,Callables,Exports) :-
	list_dict(DB,List),
	find_callable_and_export_list(List,Callables,Exports,[]).


find_callable_and_export_list([],[]) --> [].

find_callable_and_export_list([(P/N,info(_CompileMode,_Clauses,Directives,_File))|Xs],Callables) -->
	( { N =:= 0 } ->
	  [P/N=det]
	; { has_directive(mode(Mode),Directives) } ->
	  [P/N=Mode]
	; []
	),
	{ ( has_directive(parallel,Directives) ->
	    ( has_directive(callable,Directives) ->
	        Callables = [predicate(P/N,Directives)|Ps]
	    ; true,
	        Callables = Ps
	    )
	  ; true,
	      Callables = [predicate(P/N,Directives)|Ps]
	  )
	},
	find_callable_and_export_list(Xs,Ps).


%%%
%%
%
ground_clauses_and_ix(DB0,DB) :-
	list_dict(DB0,List),
	empty_dict(Dict),
	ground_each_entry_and_ix(List,Dict,DB).

%

ground_each_entry_and_ix([],DB,DB).

ground_each_entry_and_ix([(P/N,info(CompileMode,Cs,Ds,F))|Xs],DB0,DB5) :-
	( has_directive(parallel,Ds) ->
	    ( Cs = [Base,Rec] ->
		true
	    ; true,
	        error('parallel predicate does not have precisely two clauses')
	    ),
	    preprocess_rp_pred(Base,Rec,Ix,Spawns),
	    ground_ix_struct(Ix,Spawns,NewIx,NewSpawns,L,LB_pred,R,RB_pred),
	    update_definition(P/N,info(CompileMode,NewIx,Ds,F),DB0,DB1),
	    enter_spawned_and_ixed_predicates(NewSpawns,F,DB1,DB2),
	    insert_body_call(L,LB_pred,DB2,DB3),
	    insert_body_call(R,RB_pred,DB3,DB4)
	; true,
	    index_struct_of_pred(Cs,IxStruct),
	    update_definition(P/N,info(CompileMode,IxStruct,Ds,F),DB0,DB4)
	),
	ground_each_entry_and_ix(Xs,DB4,DB5).


%%%
%%  Note: We construct the "information item" using the directive 'nonverbose'
%         in order to suppress verbose mode on spawned predicates.
%
enter_spawned_and_ixed_predicates([],_File,DB,DB).

enter_spawned_and_ixed_predicates([Sp|Sps],File,DB0,DB2) :-
	clause_functor(Sp,P,N),
	take_spawned_clauses(Sps,P,N,RestSps,SpCls,[]),
	index_struct_of_pred([Sp|SpCls],IxStr),
	information_item(IxStr,[nonverbose],File,Info),
	update_definition(P/N,Info,DB0,DB1),
	enter_spawned_and_ixed_predicates(RestSps,File,DB1,DB2).


%%%
%%
%
enter_ix_item([C|Cs],File,Mode,DB0,DB1) :-
	reverse([C|Cs],ClauseList), % really unnecessary ... (???)
	clause_functor(C,P,N),
	( Mode == unexpanded ->
	    index_struct_of_pred(ClauseList,IxStr),
	    information_item(IxStr,[],File,Info),
	    update_definition(P/N,Info,DB0,DB1)
	; Mode == expanded ->
%	    ( ( lookup(P/N,DB0,info(_CM,_Cls,Ds,_F)), has_directive(parallel,Ds) ) ->
%		full_clauses(ClauseList,ExpClauses), Spawns = []
%	    ; true,
%	        expand_clause_list(ClauseList,ExpClauses,Spawns,[])
%	    ),
	    ExpClauses = ClauseList, Spawns = [],
	    index_struct_of_pred(ExpClauses,IxStr),
	    information_item(IxStr,[],File,Info),
	    update_definition(P/N,Info,DB0,DBtmp),
	    enter_spawned_and_ixed_predicates(Spawns,File,DBtmp,DB1)
	; true,
	    sys_error('enter_ix_item/5: unknown mode ~q',[Mode])
	).

%

insert_body_call(no_clause/0,_,DB0,DB1) :- !, DB0 = DB1.
insert_body_call(none,_,DB0,DB1) :- !, DB0 = DB1.
insert_body_call(B,B_pred,DB0,DB1) :- insert(B,B_pred,DB0,DB1).


%% Note: Also enters left & right bodies as spawns. They end up in database.
%

ground_ix_struct(rp(Base,Rec,OrigRec,Type,LB,RB),
	            Spawns,NewIx,NewSpawns,L,LB_pred,R,RB_pred) :-
        expand_clause(OrigRec,ExpOrigRec,NewSpawns,Spawns),  %%% processed later
	construct_clause_envs([Base,Rec,ExpOrigRec,LB,RB],[NewB,NewR,NewOR,NewLB,NewRB]),
	NewIx = rp(NewB,NewR,NewOR,Type,NewLB,NewRB),
	body_predicate_ixstr(NewLB,L,LB_pred),
	body_predicate_ixstr(NewRB,R,RB_pred).

body_predicate_ixstr(NewB,Fn,B_pred) :-
	( NewB = ((H :- _),_) ->
	    functor(H,P,N), Fn = P/N,
	    B_pred = info(_CompileMode,ix([1],[1],[1],[1],[1],cls(NewB)),invisible,none)
	; NewB = (no_clause,_) ->
	    Fn = no_clause/0,
	    B_pred = info(_CompileMode,ix([1],[1],[1],[1],[1],cls(dummy)),invisible,none)
	).

%

insert_body_pred((PN,Info),DB0,DB1) :-
	insert(PN,Info,DB0,DB1).


% Used by index.pl

construct_clause_envs_no_ix(IxCs,NewCs) :-
	strip_ix(IxCs,Cs),
	construct_clause_envs(Cs,NewCs).

strip_ix([],[]).
strip_ix([(C,_Ix)|IxCs],[C|Cs]) :- strip_ix(IxCs,Cs).

construct_clause_envs(Cs,NewCs) :-
	ground_clauses(Cs,NewCs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%   First var-entry is the first position in the environment where variables
% get allocated.
%
%   We allocate N entries to hold the N call arguments (this is convenient
% e.g. to exit the clause quickly) and a further FV arguments to hold each
% free variable of the clause.
%
%   A clause "p(T1,...,Tn) :- B" could be viewed as 'implicitly considered'
% as "p(X1,...,Xn) :- X1=T1,...,Xn=Tn,B" by the system. This never shows
% up in the code or anything, however.
%
% The environment looks like:
%
% env(OrigDet,CurrDet,Inst,Var1,...,VarK)
%

ground_clauses([],[]).

%ground_clauses([C|Cs],[(C,E)|CEs]) :-
%	C = (H :- _B),
%	functor(H,_,K),
%	first_var_entry_of_env(M),
%	FirstEntry is M+K,
%	enumeratevars(C,FirstEntry,N),
%	Size is N-M,
%	fill_list(Size,Vs),
%	construct_env(Vs,E),
%	ground_clauses(Cs,CEs).

ground_clauses([C|Cs],[CE|CEs]) :-
	construct_clause_env(C,CE),
	ground_clauses(Cs,CEs).


construct_clause_env(C,(C,E)) :-
	first_var_entry_of_env(M),
	enumeratevars(C,M,N),
	Size is N-M,
	fill_list(Size,Vs),
	construct_env(Vs,E).


%% Used by annotate.pl when the clause is rewritten to use explicit unifications.
%
reconstruct_clause_env(MaxVarNum,Env) :-
	first_var_entry_of_env(M),
	Size is MaxVarNum+1 - M,
	fill_list(Size,Vs),
	construct_env(Vs,Env).

%%%
%%  These are the actual environment constructors. The two undefs are the
% determinacy states, which are updated on entering the clause. The zeros
% are instantiation bit vectors. The first one records the current
% instantiation of variables while the second holds what arguments were
% instantiated on entering the call. (Both are destructively updated.)
%

first_var_entry_of_env(6).

construct_env(Vars,Env) :-
	Env =.. [env,_SuperStupied,undef,undef,0,0|Vars].

construct_environment([env,D0,D1,I0,I1],Xs,Env) :-
	Env =.. [env,_SuperStupied,D0,D1,I0,I1|Xs].

components_of_env(Env,State,Xs) :-
	Env =.. [env,_SuperStupied,D0,D1,I0,I1|Xs],
	State = [env,D0,D1,I0,I1].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%
%
%
display_cdb(CDB) :-
	list_dict(CDB,List),
	display_cdb_entries(List,user_error).


display_cdb_entries(    [],_Wr).
display_cdb_entries([X|Xs], Wr) :- 
	display_cdb_entry(X, Wr), 
	display_cdb_entries(Xs, Wr).


display_cdb_entry((Key,info(_,Clauses,Ds,F)),Wr) :-
	display_file(Key,F,Wr),
	display_directives(Ds,Key,Wr),
	display_clauses(Clauses,Wr).


display_file(Pred,F,Wr) :- 
	format(Wr,'% ~q defined in file ~q.pl~n',[Pred,F]).


display_directives(    [],_K,_Wr).
display_directives([D|Ds], K, Wr) :-
	format(Wr,':- ~q(~q).~n',[D,K]),
	display_directives(Ds, K, Wr).


display_clauses(ix(_V,_C,_L,_N,_S,Cs), Wr) :-
	Cs =.. [cls|CsEs],
	display_cs_es_list(CsEs, Wr).

display_clauses(rp((Base,_),(Rec,_),(_,_),Type,(LB_pred,_),(RB_pred,_)),Wr) :-
	format(Wr,'% ~q predicate~n',[Type]),
	display_clause(Base,Wr),
	Rec = (H :- rec(LP,LB,R,RB,RP)),
	DisplayRec = (H :- red(LP), LB, R, RB, red(RP)),
	display_clause(DisplayRec,Wr),
	display_clause(LB_pred,Wr),
	display_clause(RB_pred,Wr).


display_cs_es_list([],_Wr).
display_cs_es_list([(C,_E)|CsEs],Wr) :-
	display_clause(C,Wr),
	display_cs_es_list(CsEs,Wr).


display_clause((H :- B),Wr) :-
	format(Wr,'~p :-',[H]),
	display_body(B,Wr),
	format(Wr,'~n',[]).

%% Note: The current goal is supposed to do newline and indentation.
%        That way, cuts look better.
%
display_body((G,Gs),Wr) :- !,
	( G = '$early_cut' -> format(' ~p,',[G])
	; G = '$early_cut'(_) -> format(' ~p,',[G])
	; G = '$cut'(_) -> format(' ~p,',[G])
	; format(Wr,'~n        ~p,',[G])
	),
	display_body(Gs,Wr).

display_body(G,Wr) :-
	( G = '$early_cut' -> format(' ~p.~n',[G])
	; G = '$early_cut'(_) -> format(' ~p.~n',[G])
	; G = '$cut'(_) -> format(' ~p.~n',[G])
	; format(Wr,'~n        ~p.~n',[G])
	).
	
