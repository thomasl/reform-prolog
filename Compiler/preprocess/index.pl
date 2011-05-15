%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%		     INDEXING IN A SHARED SETTING
%
%   This is the new version of the indexer. Indexing is computed before
% analysis as well as after analysis, the information generated is used
% during code generation. The indexing info computed is returned as a
% structure (and this structure is later used for code generation).
%
%   The returned item is the following:
%
%	ix(V,C,L,N,S,Cls)	where:
%
%	V,L	are lists (possibly empty) of clause numbers
%	C,N,S	are tables as follows:
%
%	table([(K1,C1),...,(Kn,Cn)],Default,Summary)	with n >= 0
%
%		Ki	being keys (atoms, functors, numbers respectively)
%		Ci	being lists of clause numbers corresponding to Ki
%			(note that this list is not necessarily non-empty)
%		Default	is a clause number list, possibly empty
%		Summary	is a clause list that includes ALL clauses mentioned
%			in Ci and Default.
%
% 	Cls	is a structure that (for N clauses) has N entries, each being
%		a clause and an environment pair, (C,E). The clause is grounded
%		by enumeratevars/3, the environment E has arity equal to the
%		number of variables in Cls.
%
%
% The indexing structure construction proceeds as follows:
%
%   1. Find the indexing info for every clause and number them
%   2. Construct the V,C,L,N,S entries
%   3. Construct the clause+env thingie.
%

:- ensure_loaded('../util/dict').

:- ensure_loaded(lift).



index_struct_of_pred(Cs,ix(V,C,L,N,S,IxCsEs)) :-
	index_info_of_clauses(Cs,IxCs),
	indexing_tables(IxCs,V,C,L,N,S),
	construct_clause_envs_no_ix(IxCs,CsEs),
	IxCsEs =.. [cls|CsEs].


%%
%  indexing_tables(ClauseIxList,CurrCls,V0,V1,C0,C1,L0,L1,N0,N1,S0,S1)
%
%   For each, of N clause indexing information structures, collect indexing
% information, in the form of lists of clause numbers.
%
%   Each indexing information item has the form:
%
%	ix(Cut,Var,Const,List,Num,Struct)	where
%
%	Cut	is yes if cut is done directly after indexing
%	Var,
%	List	are yes or no depending on whether a variable or list in
%		the first-argument implies that the clause is a candidate
%       Num,
%	Const,
%	Struct	are no, yes or a specific P/N (or number)
%
%   A preliminary pass collects the pertinent information for each category.
% Tables are then created for constants, structures and numbers.
%
indexing_tables(IxCs,V,C,L,N,S) :-
	initial_ix_table(IxCs,1,V,[],C0,[],L,[],N0,[],S0,[]),
	create_jump_table(C0,C),
	create_jump_table(N0,N),
	create_jump_table(S0,S).

%

initial_ix_table([],_ClsNo,V,V,C,C,L,L,N,N,S,S).

initial_ix_table([(_Clause,ix(Cut,V,C,L,N,S))|IxCs],Cls,
	        V0,V2,C0,C2,L0,L2,N0,N2,S0,S2) :-
	introduce_ix(V,Cut,Cls,V0,V1),
	introduce_ix(L,Cut,Cls,L0,L1),
	introduce_ix_key(C,Cut,Cls,C0,C1),
	introduce_ix_key(N, no,Cls,N0,N1),
	introduce_ix_key(S,Cut,Cls,S0,S1),
	NxtCls is Cls+1,
	initial_ix_table(IxCs,NxtCls,V1,V2,C1,C2,L1,L2,N1,N2,S1,S2).

%

introduce_ix_key(Key, Cut, N, Seq, Hdl) :-
	( introduce_ix(Key, Cut, N, Seq, Hdl) -> true ; Seq = [ix_key(Cut,Key,N)|Hdl] ).


introduce_ix( no,_Cut,_N, Seq, Hdl) :- Seq = Hdl.

introduce_ix(yes, Cut, N, Seq, Hdl) :-
	( Cut == yes -> Seq = [N] ; Seq = [N|Hdl] ).


%%
%   A jump tables consist of a switch table, a default switch and a collection
% of all clause sequences. The latter is used for analysis purposes, and plays
% no part in the code generation.
%
create_jump_table(KeySeq, Table) :-
	jump_table_split(KeySeq, [], [], EntryTable, DefaultSeq, []),
	jump_table_close(EntryTable, SwitchTable, AllSeq, [DefaultSeq]),
	jump_table_build(SwitchTable, DefaultSeq, AllSeq, Table).

%

jump_table_split(    [],_Ui, Ei,Ei, Di,Di).

jump_table_split([K|Ks], Ui, Ei,Ej, Di,Dj) :-
	jump_table_entry(K, Ui,Ux, Ei,Ex, Di,Dx),
	jump_table_split(Ks, Ux, Ex,Ej, Dx,Dj).


jump_table_entry(ix_key(Cut,Key,N), Ui,Ux, Ei,Ex, Di,Dx) :-
	Ui = Ux,
	( float(Key) -> %%% floats are treated as default entries
	    Ei = Ex,
	    Di = [N|Dx]
	; true,
	    Di = Dx,
	    insert_key(Ei, Ui, Cut,Key,N, Ex)
	).

jump_table_entry(N, Ui,Ux, Ei,Ex, Di,Dx) :- integer(N),
	Di = [N|Dx],
	append(Ui, [N], Ux),
	insert_all_keys(Ei, N, Ex).  % should NOT have Universal param? yes.


%%
%   jump_table_close simply closes the d-lists of each entry and removes unnecessary
% information. Also, the list of all clause sequences is constructed to aid the
% analysis phase later on.
%
jump_table_close([],[]) --> [].

jump_table_close([entry(Key,_Cut,Seq,[])|Xs],[(Key,Seq)|Ys]) --> [Seq],
	jump_table_close(Xs,Ys).


%%
%   jump_table_build(Entries, Default, All, Table) collapses trivial tables,
% as described bellow:
%
%   o	if the list of entries is empty (i.e. there are no entries), the table
%	collapses to default (= all)
%
%   o	if the list of entries have one single item and no default, the table
%	collapses to that clause list
%
%   o	otherwise, build a table
%
jump_table_build(    [], Default,_All, Table) :-
	Table = Default.

jump_table_build([X|Xs], Default, All, Table) :-
	( (Xs = [], Default = []) ->
	    X = (_,Table)
	; true,
	    Table = table([X|Xs],Default,All)
	).


%%
%  insert_all_keys(+EntryList, +ClauseNumber, -NewEntryList)
%
%   The clause is inserted in all entries of the jump tables that have not made
% a cut yet. This occurs e.g. for variable keys. If the clause has not occured
% yet, a new entry is created.
%
insert_all_keys(    [],_N,     []).

insert_all_keys([X|Xs], N, [Y|Ys]) :-
	X = entry(Key,Cut,Seq,Hdl),
	Y = entry(Key,Cut,Seq,New),
	( Cut == yes ->
	    Hdl = New
	; true,
	    Hdl = [N|New]
	),
	insert_all_keys(Xs, N, Ys).

%%
%  insert_key(+EntryList, +Universal, +Cut,+Key,+ClauseNumber, -NewEntryList)
%
%   Include a clause number into the current sequence of clause numbers matching
% the specified key (with associated cut information).
%   Currently the table is represented as a simple list. For large numbers of
% keys, we should use a dictionary.
%
% NOTE: Universal is the list of clauses appearing before the current key.
%	It has been prepended to all entries already in the table. Thus, we
%	prepend it only if the Key does not appear previously.
%
insert_key(    [], Univ, Cut,Key,N, [entry(Key,Cut,Seq,Hdl)]) :-
	append(Univ, [N|Hdl], Seq).

insert_key([X|Xs], Univ, Cut,Key,N, NewList) :-
	X = entry(K,C,Seq,Hdl),
	( K \== Key ->
	    NewList = [X|Ys],
	    insert_key(Xs, Univ, Cut,Key,N, Ys)
	; %%%%% K == Key,
	    cut_seq(Cut, C, Cx, N, Hdl, Nxt),
	    NewList = [entry(K,Cx,Seq,Nxt)|Xs]
	).


%%
%  cut_seq(+ThisClauseHasCut, +SequenceHasCut, -SequenceHasCut,
%          +ClauseNumber, -SequenceTail, -Handle)
%
%   Add the ClauseNumber to the Tail of a Sequence if the sequence has not been
% closed by a cut, i.e. SequenceHasCut is confirmative.
%   When adding a ClauseNumber to the Tail of a Sequence, if ThisClauseHasCut is
% confirmative; close the Tail of the Sequence, otherwise produce a new handle.
%
cut_seq(yes, Cut, CutSeq, N, Seq, Hdl) :-
	( Cut == no ->
	    CutSeq = yes,
	    Seq = [N|Hdl], Hdl = []
	; true,
	    CutSeq = no,
	    Seq = Hdl
	).

cut_seq( no, Cut, CutSeq, N, Seq, Hdl) :-
	CutSeq = Cut,
	( Cut == no -> Seq = [N|Hdl] ; Seq = Hdl, Hdl = [] ).


%%
%   Indexing information is extracted as follows:
%
%   o	take all explicit and implicit unifications and possibly type tests up
%	to the first cut or user call. Ignore harmless primitives such as
%	'$choice'(_) and '$info'(_) (and other type info).
%
%   o	if there are testing unifications before a type test, ignore the type test.
%
%   o	mark whether there is a cut or not.
%
index_info_of_clauses([],[]).

index_info_of_clauses([C0|Cs],[(C1,Ix)|CIxs]) :-
	lift_clause(C0,C1),
	( C0 = (H :- B) ->
	  indexing_info(H,B,Ix)
	; indexing_info(C0,true,Ix)
	),
	index_info_of_clauses(Cs,CIxs).

%%
%   indexing_info(Head, Body, IndexingInfo) computes available indexing
% information for a given clause.
%
indexing_info(H,B,IxInfo) :-
	H =.. [_|Xs],
	empty_dict(Occ0),
	no_indexing_info(EmptyIx),
	indexing_info_head(Xs,1,Occ0,Occ1,EmptyIx,HeadIxInfo,Stop),
	( Stop == stop ->
	    IxInfo = HeadIxInfo
	; true,
	    indexing_info_body(B,Occ1,HeadIxInfo,IxInfo,Stop)
	).

%%
%   Keep an occurence dictionary to tell whether a var-to-var unification is
% significant or not.
%
% NOTE: Stop is the atom 'stop' if further indexing is unnecessary,
%	unbound otherwise.
%
indexing_info_head([],_N,Occ,Occ,HdIx,HdIx,_Stop).

indexing_info_head([X|Xs],M,Occ0,Occ2,HdIx0,HdIx2,Stop) :-
	( var(X) ->
	  ( first_occurence_of_var(X,Occ0) ->
	    insert(X,M,Occ0,Occ1),
	    HdIx1 = HdIx0
	  ; Occ1 = Occ0,
	    HdIx1 = HdIx0,
	    Stop = stop
	  )
	; term_unification(HdIx0,X,M,HdIx1,Stop),
	  vars_sort(X,Vs),
	  insert_new_vars(Vs,Occ0,Occ1,Stop) % stop if repeated var occurs
	),
	N is M+1,
	indexing_info_head(Xs,N,Occ1,Occ2,HdIx1,HdIx2,Stop).

%

insert_new_vars(    [],Occ,  Occ, _Stop).
insert_new_vars([X|Xs],Occ0, Occ2, Stop) :-
	insert_var(X,Occ0,Occ1,Stop),
	insert_new_vars(Xs,Occ1,Occ2,Stop).

insert_var(X,Occ0,Occ1,Stop) :-
	( defined(X,Occ0) ->
	  Occ1 = Occ0, Stop = stop
	; insert(X,0,Occ0,Occ1)
	).

%

indexing_info_body((G,Gs),Occ0,BodIx0,BodIx2,Stop) :- !,
	( inlineable(G) ->
	  ( interesting_indexing_primop(G) ->
	    indexing_info_primop(G,Occ0,Occ1,BodIx0,BodIx1,Stop)
	  ; Stop = stop,
	    Occ1 = Occ0,
	    BodIx1 = BodIx0
	  ),
	  ( Stop == stop -> % Indexing has stopped
	    BodIx2 = BodIx1
	  ; indexing_info_body(Gs,Occ1,BodIx1,BodIx2,Stop)
	  )
	; BodIx2 = BodIx0
	).

indexing_info_body(G,Occ,BodIx0,BodIx1,Stop) :-
	( inlineable(G) ->
	  ( interesting_indexing_primop(G) ->
	    indexing_info_primop(G,Occ,_Occ1,BodIx0,BodIx1,Stop)
	  ; BodIx1 = BodIx0 % no need to 'stop' since last goal
	  )
	; BodIx1 = BodIx0   % see above
	).

%%
%   This is the abstract datatype representing indexing information:
%
%	ix(Cut,Var,Const,List,Num,Struct)	where
%
%   Var, List and Num	are 'yes' or 'no'
%
%   Const and Struct	are 'Functor/Arity', 'yes' or 'no' (the Arity is zero
%			for constants)
%
%   We use 'yes' as a result of type test etc. Cut is 'yes' if there is a cut
% and 'no' otherwise.
%
% NOTE: No indexing information is given as ix(no,yes,yes,yes,yes,yes),
%	i.e. no cut, indexing must go and all types are possible.
%
no_indexing_info(ix(no,yes,yes,yes,yes,yes)).

% invert_indexing is done for negated goals.

invert_indexing(ix(Cut,V,C,L,N,S),ix(Cut,V1,C1,L1,N1,S1)) :-
	invert_elts([V,C,L,N,S],[V1,C1,L1,N1,S1]).

invert_elts([],[]).

invert_elts([X|Xs],[InvX|InvXs]) :-
	( X == no ->
	    InvX = yes
	; X == yes ->
	    InvX = no
	; true,
	    sys_error('invert_elts/2: illegal indexing element ~q',[X])
	),
	invert_elts(Xs,InvXs).

%%
%   A more sophisticated approach would (a) have multiple possible atoms etc.
% per clause (to account for disjunctions) and not set other types to 'no'
% directly (again, for the same reasons).
%
term_unification(ix(Cut,V0,C0,L0,N0,S0),Term,ArgNo,
	ix(Cut,V,C,L,N,S),Stop) :-
        ( ArgNo =:= 1 ->
	  ( atom(Term) ->
	    V = V0,
	    C = Term/0,
	    L = no,
	    N = no,
	    S = no
	  ; number(Term) ->
	    V = V0,
	    C = no,
	    L = no,
	    N = Term,
	    S = no
	  ; var(Term) ->
	    sys_error('term_unification/4: variable where term expected')
	  ; Term = [_|_] ->
	    V = V0,
	    C = no,
	    L = yes,
	    N = no,
	    S = no,
	    some_nonvar_then_stop(Term,Stop)
	  ; V = V0,
	    C = no,
	    L = no,
	    N = no,
	    functor(Term,Functor,Arity),
	    S = Functor/Arity,
	    some_nonvar_then_stop(Term,Stop)
	  )
	; Stop = stop,
	  V0 = V, C0 = C, L0 = L, N0 = N, S0 = S
	).

%

some_nonvar_then_stop(Term,Stop) :-
	Term =.. [_|Xs],
	s_nv_stop(Xs,Stop).

s_nv_stop([],_).
s_nv_stop([X|Xs],Stop) :-
	( nonvar(X) ->
	  Stop = stop
	; s_nv_stop(Xs,Stop)
	).

%%
%   Unifications with nonvars or previously occuring variables are interesting;
% one provides more indexing information, the other stops the indexing process.
%   The current treatment can be improved somewhat, e.g. it does not simplify
% unifications.
%
indexing_info_primop(X=Y,Occ0,Occ1,Ix0,Ix1,Stop) :- !,
	( ( var(X), lookup(X,Occ0,1), T = Y 
	  ; var(Y), lookup(Y,Occ0,1), T = X ) ->
	  ( var(T) ->
	      Ix0 = Ix1,
	      ( lookup(T,Occ0,N) ->
		  ( N =\= 1 ->
	              Occ0 = Occ1,
		      Stop = stop
		  ; N =:= 1 ->
	              Occ0 = Occ1
		  )
	      ; true,
	          insert(T,0,Occ0,Occ1)
	      )
	  ; true, %%%%% i.e. nonvar(T)
	      Occ0 = Occ1,
	      term_unification(Ix0,T,1,Ix1,Stop)
	  )
	; true,
	    Ix0 = Ix1,
	    Occ0 = Occ1,
	    Stop = stop
	).

indexing_info_primop('$choice'(_),Occ0,Occ1,Ix0,Ix1,_Stop) :-
	Ix0 = Ix1, Occ0 = Occ1.

indexing_info_primop('$cut'(_),Occ0,Occ1,Ix0,Ix1,Stop) :-
	Ix0 = ix(_,V,C,L,N,S),
	Ix1 = ix(yes,V,C,L,N,S),
	Stop = stop,
	Occ1 = Occ0.

indexing_info_primop('$early_cut',Occ0,Occ1,Ix0,Ix1,Stop) :-
	Ix0 = ix(_,V,C,L,N,S),
	Ix1 = ix(yes,V,C,L,N,S),
	Stop = stop,
	Occ1 = Occ0.

indexing_info_primop('$early_cut'(_),Occ0,Occ1,Ix0,Ix1,Stop) :-
	Ix0 = ix(_,V,C,L,N,S),
	Ix1 = ix(yes,V,C,L,N,S),
	Stop = stop,
	Occ1 = Occ0.

indexing_info_primop(var(X),Occ0,Occ1,Ix0,Ix1,Stop) :-
	Occ0 = Occ1,
	( lookup(X,Occ0,1) ->
	  strengthen_exclusively(var,Ix0,Ix1)
	; Ix0 = ix(_,V,C,L,N,S),
	  Ix1 = ix(no,V,C,L,N,S),
	  Stop = stop
	).

indexing_info_primop(atom(X),Occ0,Occ1,Ix0,Ix1,Stop) :-
	Occ0 = Occ1,
	( lookup(X,Occ0,1) ->
	  strengthen_exclusively(const,Ix0,Ix1)
	; Ix0 = ix(_,V,C,L,N,S),
	  Ix1 = ix(no,V,C,L,N,S),
	  Stop = stop
	).

indexing_info_primop(atomic(X),Occ0,Occ1,Ix0,Ix1,Stop) :-
	Occ0 = Occ1,
	( lookup(X,Occ0,1) ->
	  strengthen_only([const,number],[var,list,struct],Ix0,Ix1)
	; Ix0 = ix(_,V,C,L,N,S),
	  Ix1 = ix(no,V,C,L,N,S),
	  Stop = stop
	).

indexing_info_primop(nonvar(X),Occ0,Occ1,Ix0,Ix1,Stop) :-
	Occ0 = Occ1,
	( lookup(X,Occ0,1) ->
	  strengthen_only([const,list,number,struct],[var],Ix0,Ix1)
	; Ix0 = ix(_,V,C,L,N,S),
	  Ix1 = ix(no,V,C,L,N,S),
	  Stop = stop
	).

indexing_info_primop(number(X),Occ0,Occ1,Ix0,Ix1,Stop) :-
	Occ0 = Occ1,
	( lookup(X,Occ0,1) ->
	  strengthen_exclusively(number,Ix0,Ix1)
	; Ix0 = ix(_,V,C,L,N,S),
	  Ix1 = ix(no,V,C,L,N,S),
	  Stop = stop
	).

indexing_info_primop(float(X),Occ0,Occ1,Ix0,Ix1,Stop) :-
	Occ0 = Occ1,
	( lookup(X,Occ0,1) ->
	  strengthen_exclusively(number,Ix0,Ix1)
	; Ix0 = ix(_,V,C,L,N,S),
	  Ix1 = ix(no,V,C,L,N,S),
	  Stop = stop
	).

indexing_info_primop(integer(X),Occ0,Occ1,Ix0,Ix1,Stop) :-
	Occ0 = Occ1,
	( lookup(X,Occ0,1) ->
	  strengthen_exclusively(number,Ix0,Ix1)
	; Ix0 = ix(_,V,C,L,N,S),
	  Ix1 = ix(no,V,C,L,N,S),
	  Stop = stop
	).

indexing_info_primop((_=:=_),Occ0,Occ1,Ix0,Ix1,_Stop) :-
	Occ0 = Occ1, Ix0 = Ix1.

indexing_info_primop((_==_),Occ0,Occ1,Ix0,Ix1,_Stop) :-
	Occ0 = Occ1, Ix0 = Ix1.

%%
%   Only one type is useful. This is handled by throwing away all the others.
%
strengthen_exclusively(X,ix(Cut,V,C,L,N,S),Ix1) :- atomic(X), !,
	( X == var ->
	  Ix1 = ix(Cut,V,no,no,no,no)
	; X == const ->
	  strengthen(C,C1),
	  Ix1 = ix(Cut,no,C1,no,no,no)
	; X == list ->
	  strengthen(L,L1),
	  Ix1 = ix(Cut,no,no,L1,no,no)
	; X == number ->
	  strengthen(N,N1),
	  Ix1 = ix(Cut,no,no,no,N1,no)
	; X == struct ->
	  strengthen(S,S1),
	  Ix1 = ix(Cut,no,no,no,no,S1)
	).

%

strengthen_only(Xs,Ys,Ix0,Ix2) :-
	strengthen_all(Xs,Ix0,Ix1),
	kill_all_ix(Ys,Ix1,Ix2).

%

kill_all_ix([],Ix,Ix).

kill_all_ix([X|Xs],Ix0,Ix2) :-
	kill_ix(X,Ix0,Ix1),
	kill_all_ix(Xs,Ix1,Ix2).

%

strengthen_all([],Ix0,Ix1) :- !, Ix0 = Ix1.

strengthen_all([X|Xs],Ix0,Ix3) :-
	strengthen_ix(X,Ix0,Ix1),
	strengthen_all(Xs,Ix0,Ix2),
	join_ix(Ix1,Ix2,Ix3).

%

join_ix(Ix1,Ix2,Ix3) :-
	Ix1 =.. [ix,Cut|Args1],
	Ix2 =.. [ix,Cut|Args2],
	join_args(Args1,Args2,Args3),
	Ix3 =.. [ix,Cut|Args3].

%

join_args([],[],[]).

join_args([X|Xs],[Y|Ys],[Z|Zs]) :-
	( ( X == yes ; Y == yes ) -> Z = yes ; Z = no ),
	join_args(Xs,Ys,Zs).

%

strengthen_ix(X,ix(Cut,V,C,L,N,S),Ix1) :- atomic(X), !,
	( X == var ->
	  Ix1 = ix(Cut,yes,C,L,N,S)
	; X == const ->
	  strengthen(C,C1),
	  Ix1 = ix(Cut,V,C1,L,N,S)
	; X == list ->
	  strengthen(L,L1),
	  Ix1 = ix(Cut,V,C,L1,N,S)
	; X == number ->
	  strengthen(N,N1),
	  Ix1 = ix(Cut,V,C,L,N1,S)
	; X == struct ->
	  strengthen(S,S1),
	  Ix1 = ix(Cut,V,C,L,N,S1)
	).

%

kill_ix(X,ix(Cut,V,C,L,N,S),Ix1) :- atomic(X), !,
	( X == var ->
	  Ix1 = ix(Cut,no,C,L,N,S)
	; X == const ->
	  Ix1 = ix(Cut,V,no,L,N,S)
	; X == list ->
	  Ix1 = ix(Cut,V,C,no,N,S)
	; X == number ->
	  Ix1 = ix(Cut,V,C,L,no,S)
	; X == struct ->
	  Ix1 = ix(Cut,V,C,L,N,no)
	).

%

strengthen(no,  X) :- !, X = yes.
strengthen(yes, X) :- !, X = yes.
strengthen(  X, X).   %%% i.e. known index


%%
%
%
% NOTE: On future extension.
%
%   Operations that are tests but also provide type information for indexing
% could be defined to:
%
%  (a)	be interesting and
%  (b)	stop indexing
%
%   For instance, the case p(X) :- X =:= 0,... imposes that X must be a number,
% while indexing should stop since the comparsion is a test.
%
interesting_indexing_primop(_=_).
interesting_indexing_primop('$choice'(_)). % dummy
interesting_indexing_primop('$cut'(_)).
interesting_indexing_primop('$early_cut').
interesting_indexing_primop('$early_cut'(_)).

interesting_indexing_primop(var(_)).
interesting_indexing_primop(atom(_)).
interesting_indexing_primop(atomic(_)).
interesting_indexing_primop(nonvar(_)).
interesting_indexing_primop(number(_)).
interesting_indexing_primop(float(_)).
interesting_indexing_primop(integer(_)).

%%%interesting_indexing_primop((_=:=_)).
%%%interesting_indexing_primop((_==_)).

%

first_occurence_of_var(X,Env) :- \+(defined(X,Env)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%   Next comes the generation of indexing code. The input is an indexing
% structure and a call pattern. The output is a code stream including the
% entire predicate.
%
% The indexer requires other procedures to do the following (properly):
%
%   We assume that clauses have been annotated properly and surplus clauses
% eliminated appropriately by the annotator. Hence, what remains is merely to
% emit code. No call pattern is required, since that has already been factored
% in by the annotator. However, await-instructions may be required, as given by
% the types Par and Seq and locality Loc.
%
compile_ix_struct(ix(V,C,L,N,S,Cls),P,Ar,Par,Seq,Loc) --> !,
	( { V = C, V = L, V = N, V = S } -> %%% no indexing code
	  construct_clause_seq(V,LV,Identical,ModeErr,Labels,[])
	; suspend_indexing(Loc,Par,Seq),
	  ['Switch_On_Term'(LV,LC,LL,LN,LS)],
	  construct_clause_seq(V,LV,Identical,ModeErr,Labels,CLabels),
	  construct_table(C,LC,Identical,ModeErr,'Switch_On_Constant', CLabels,LLabels),
	  construct_clause_seq(L,LL,Identical,ModeErr,LLabels,NLabels),
	  construct_table(N,LN,Identical,ModeErr,'Switch_On_Constant', NLabels,SLabels),
	  construct_table(S,LS,Identical,ModeErr,'Switch_On_Structure',SLabels,[])
	),
	construct_mode_code(ModeErr,P,Ar),
	compile_clauses(Cls,ClsLabels),
	{ LabelStruct =.. [labels|ClsLabels],
	  resolve_labels(Labels,LabelStruct) 
	}.

compile_ix_struct(rp(Base,Rec,SeqRec,Type),_P,_N,Par,Seq,Loc) --> !,
	( { Type = listrec } ->
	  [comment('list recursive')],
	  suspend_indexing(Loc,Par,Seq),
	  ['Switch_On_Term'(LV,LC,LL,fail,fail)],
	  construct_clause_seq([1,2],LV,Identical,_,Labels,CLabels),
	  construct_table([1],LC,Identical,_,'Switch_On_Constant',CLabels,LLabels),
	  { RecCase = 3 }, %%%%% if unsafe, it should be 2!!
	  construct_clause_seq([RecCase],LL,Identical,_,LLabels,[]),
	  compile_clauses(cls(Base,SeqRec,Rec),ClsLabels),
	  { LabelStruct =.. [labels|ClsLabels],
	    resolve_labels(Labels,LabelStruct) }
	; { Type = intrec } ->
	  [comment('integer recursive')],
	  suspend_indexing(Loc,Par,Seq),
	  ['Switch_On_Term'(fail,fail,fail,LN,fail)],
	  construct_table([1,2],LN,Identical,_,'Switch_On_Constant',Labels,[]),
	  compile_clauses(cls(Base,Rec),ClsLabels),
	  { LabelStruct =.. [labels|ClsLabels],
	    resolve_labels(Labels,LabelStruct)
	  }
	; { sys_error('compile_ix_struct/8: recursion type ~q unknown',[Type]) }
	).

%%
%   If there are switching entries that are impossible due to the inferred type,
% the ModeErr variable is set to yes(SeqT,L) where SeqT is an atom signifying
% type and L is the 'error code' label to jump to. If there are no such entries,
% the variable is unbound and no code is generated.
%
construct_mode_code(X,_,_) --> { var(X), ! }.

construct_mode_code(yes(SeqT,L),P,N) -->
	['Label'(L),
	 comment('register 0 holds offending argument'),
	 'Put_Constant'(SeqT,x(1)),
	 'Put_Constant'(P,x(2)),
	 'Put_Constant'(N,x(3)),
	 'Execute'(mode_error,4)
	].

%

suspend_indexing(freeze(Loc),Par,Seq) --> !, suspend_indexing(Loc,Par,Seq).

suspend_indexing(local,_,_) --> !, [].

suspend_indexing(robust,Par,Seq) --> !,
	await_instantiation(Par,Seq).

suspend_indexing(fragile,Par,Seq) --> !,
	await_instantiation(Par,Seq).

suspend_indexing(L,_,_) --> !,
	{ sys_error('suspend_indexing/5: no such locality ~q',[L]) }.

%

await_instantiation(nil,_) --> !,
	[].
await_instantiation(gnd,_) --> !,
	[].
await_instantiation(nv,_) --> !,
	[].
await_instantiation(any,_) --> !,
	['Await_Nonvar'(x(0))].
await_instantiation(free,_) --> !,
	['Await_Nonvar'(x(0))].
await_instantiation(free_nil,_) --> !,
	['Await_Nonvar'(x(0))].
await_instantiation(Lst,Seq) -->
	{ list_types(Lst,_,T0), !, cnv_lub(T0,T) },
	await_instantiation(T,Seq).

%

unb_type(free).
unb_type(free_nil).
unb_type(any).
unb_type(X) :- list_type(X,_,_,T,_), ( T = f -> true ; T = fn -> true ).


%%
%   The following 'clause compiler' is not very pretty. Since several passes are
% made over the code, we cannot use a d-list (in a straightforward way).
%
%   The 'dummy' clause is introduced when clauses are eliminated due to inferred
% types, and generate no code.
%
compile_clauses(ClsStruct,Labels) -->
	{
          ClsStruct =.. [_|CsEs]
        },
	compile_clause_list(CsEs,Labels).


compile_clause_list(    [],    []) --> [].
compile_clause_list([C|Cs],[L|Ls]) --> [comment(clause(C)),'Label'(L)],
	compile_clause(C),
	compile_clause_list(Cs,Ls).


compile_clause(dummy ) --> !, [].
compile_clause(Clause) -->
	{
          Clause = (_Head :- rec(_LP,_LB,_R,_RB,_RP)),
	  !,
	  rp_compile_preproc(Clause, Code)
        },
	dlist(Code).

compile_clause(Clause) -->
	{
          sh_compile_clause(Clause, Code)
        },
	dlist(Code).

%

construct_clause_seq(mode(T), Label,_Identical, ModeErr, Labels,Labels) --> !,
	{ ModeErr = yes(T,Label) }.

construct_clause_seq(    [], fail, _Identical,_ModeErr, Labels,Labels) --> [].

construct_clause_seq(   [N], Label, Identical,_ModeErr, Labels,Handle) --> !,
	{
          Labels = [(N,Label)|Handle],
	  short_circuit(Identical, [N], Label,_Result)
        }.

construct_clause_seq(Branch, Label, Identical,_ModeErr, Labels,Handle) -->
	{
          Branch = [L,N|Ns],
	  short_circuit(Identical, Branch, Label, Result)
        },
	( { Result == identical } ->
	    { Labels = Handle }
	; { Labels = [(L,LL)|Next] },
	    ['Label'(Label),'Try'(LL)],
	    construct_rest_clause_seq(Ns, N, Next,Handle)
	).

%

construct_rest_clause_seq(    [], L, [(L,Label)|Rest],Rest) --> ['Trust'(Label)].

construct_rest_clause_seq([N|Ns], L, [(L,Label)|Next],Rest) --> ['Retry'(Label)],
	construct_rest_clause_seq(Ns, N, Next,Rest).


short_circuit(Identical, Branch, Label, Result) :- var(Identical), !,
	Identical = [(Label, Branch)|_],
	Result = nonidentical.

short_circuit([Pair|Ps], Branch, Label, Result) :- Pair = (L,B),
	( B == Branch ->
	    L = Label,
	    Result = identical
	; true,
	    short_circuit(Ps, Branch, Label, Result)
	).


%%
%   Constructing the actual jump table:
%
%   o	if a clause sequence, simply code it as usual.
%
%   o	if a true table, emit the instruction and construct the jump table
%	and default sequence.
%
construct_table(mode(T), Label,_Identical, Mode,_Type, Labels,Labels) --> !,
	{ Mode = yes(T,Label) }.

construct_table(table(Es,Ds,_All), Label, Identical,_Mode, Type, Labels,Handle) --> !,
	['Label'(Label),Insn],
	{
            Insn =.. [Type,Entries,Default]
	},
	table_entries(Es, Entries, Identical, Labels,Defaults),
	construct_clause_seq(Ds, Default, Identical, _, Defaults,Handle).

construct_table(Branch, Label, Identical,_Mode,_Type, Labels,Handle) -->
	construct_clause_seq(Branch, Label, Identical, _, Labels,Handle).


%% This is where we construct the try-sequence for each key in the table.
%
table_entries([], [],_Identical, Labels,Labels) --> [].

table_entries([(Key,Branch)|Xs], [(Key,Label)|Ys], Identical, Labels,Handle) -->
	construct_clause_seq(Branch, Label, Identical, _, Labels,Rest),
	table_entries(Xs, Ys, Identical, Rest,Handle).

%%
%   Now unify the labels used during label construction with the actual ones
% appearing by the clauses.
%
resolve_labels(        [],_Labels).
resolve_labels([(N,L)|Xs], Labels) :-
	arg(N, Labels, L),
	resolve_labels(Xs, Labels).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%
%
display_clause_table(Dict) :-
	list_dict(Dict,Lst),
	display_info_entries(Lst).

display_info_entries([]).
display_info_entries([(PN,info(_,IX,_,_))|Xs]) :-
	format('-----~n~q:~n',[PN]),
	display_ix_struct(IX),
	display_info_entries(Xs).

display_ix_struct(ix(V,C,L,N,S,Clss)) :- !,
	format('var:    ~q~n',[V]),
	format('const:  ',[]),
	display_table(C),
	format('list:   ~q~n',[L]),
	format('number: ',[]),
	display_table(N),
	format('struct: ',[]),
	display_table(S),
	Clss =.. [_|Clauses],
	display_numbered_clauses(Clauses,1).
display_ix_struct(_) :- format('<ix not handled>~n',[]).

display_table(table(Entries,Default,_)) :- !,
	format('(~n',[]),
	display_entries(Entries),
	format('default: ~q~n)~n',[Default]).
display_table(X) :-
	format('~q~n',[X]).

display_entries([]).
display_entries([(K,Cs)|Xs]) :-
	format('    ~q: ~q~n',[K,Cs]),
	display_entries(Xs).

display_numbered_clauses([],_).
display_numbered_clauses([C|Cs],N) :-
	format('~d: ~p~n',[N,C]),
	M is N+1,
	display_numbered_clauses(Cs,M).
