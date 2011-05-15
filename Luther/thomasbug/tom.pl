%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%			2-3-4 TREES IN PROLOG
%
% When you need balanced trees in Prolog, you can use red-black trees or
% their somewhat simpler predecessors, 2-3-4 trees. This is an implementation
% based on Sedgewick's description.
%
% The tree holds nodes:
%   leaf
%   n2(L,R,K1,V1)
%   n3(L,C,R,K1,V1,K2,V2)
%   n4(L,LC,RC,R,K1,V1,K2,V2,K3,V3)
%
% L, R, C, LC, RC are subtrees (left, right, center)
% Ki, Vi are key-value pairs
%
% When things are inserted into the tree, the algorithm splits 4-nodes
% into 2-nodes, depending on the parent. This is slower than standard
% binary-tree insertion, but keeps the tree balanced. For big dictionaries
% with a poor key distribution, this may be a win.
%
% The following operations are provided:
%
% empty_234(EmptyTree)
% lookup_234(Tree,Key,Value)
% insert_234(Tree,Key,Value,NewTree)
% update_234(Tree,Key,Value,NewTree)
% list_dict_234(Tree,List)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_234(leaf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_234(_Key,Dict,_Val) :- 
	var(Dict),!,
	error('lookup: 234-tree is variable').
lookup_234(Key,Dict,Val) :-
	look_234(Dict,Key,Val).

look_234(X,_,_) :- var(X),!,error('look_234: var arg').

look_234(n2(L,R,K1,V1),K,V) :-
	( K @< K1 ->
	  look_234(L,K,V)
	; K == K1 ->
	  V = V1
	; look_234(R,K,V)
	).

look_234(n3(L,C,R,K1,V1,K2,V2),K,V) :-
	( K @< K1 ->
	  look_234(L,K,V)
	; K == K1 ->
	  V = V1
	; K @< K2 ->
	  look_234(C,K,V)
	; K == K2 ->
	  V = V2
	; look_234(R,K,V)
	).

look_234(n4(L,LC,RC,R,K1,V1,K2,V2,K3,V3),K,V) :-
	( K @< K1 ->
	  look_234(L,K,V)
	; K == K1 ->
	  V = V1
	; K @< K2 ->
	  look_234(LC,K,V)
	; K == K2 ->
	  V = V2
	; K @< K3 ->
	  look_234(RC,K,V)
	; K == K3 ->
	  V = V3
	; look_234(R,K,V)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_234(_Key,_Value,Dict,_NewDict) :- 
	var(Dict),!,error('insert: 234-tree is variable').

insert_234(K,V,n4(L,LC,RC,R,K1,V1,K2,V2,K3,V3),NewTree) :- !,
	TmpTree = n2(n2(L,LC,K1,V1),n2(RC,R,K3,V3),K2,V2),
	ins_234(TmpTree,K,V,NewTree).

insert_234(K,V,T,NewTree) :-
	ins_234(T,K,V,NewTree).

%%%%%%%%%%

ins_234(X,_,_,_) :- var(X),!,error('insert: 234-tree is variable').

ins_234(leaf,K,V,n2(leaf,leaf,K,V)).

ins_234(n2(L,R,K1,V1),K,V,NewTree) :- 
	( K @< K1 ->
	  split_and_insert_2(left,L,R,K1,V1,K,V,NewTree)
	; K == K1 ->
	  error
	; split_and_insert_2(right,R,L,K1,V1,K,V,NewTree)
	).

ins_234(n3(L,C,R,K1,V1,K2,V2),K,V,NewTree) :-
	( K @< K1 ->
	  split_and_insert_3(left,L,C,R,K1,V1,K2,V2,K,V,NewTree)
	; K == K1 ->
	  error
	; K @< K2 ->
	  split_and_insert_3(center,C,L,R,K1,V1,K2,V2,K,V,NewTree)
	; K == K2 ->
	  error
	; split_and_insert_3(right,R,L,C,K1,V1,K2,V2,K,V,NewTree)
	).

ins_234(n4(L,LC,RC,R,K1,V1,K2,V2,K3,V3),K,V,NewTree) :-
	( K @< K1 ->
	  NewTree = n4(NewL,LC,RC,R,K1,V1,K2,V2,K3,V3),
	  ins_234(L,K,V,NewL)
	; K == K1 ->
	  error
	; K @< K2 ->
	  NewTree = n4(L,NewLC,RC,R,K1,V1,K2,V2,K3,V3),
	  ins_234(LC,K,V,NewLC)
	; K == K2 ->
	  error
	; K @< K3 ->
	  NewTree = n4(L,LC,NewRC,R,K1,V1,K2,V2,K3,V3),
	  ins_234(RC,K,V,NewRC)
	; K == K3 ->
	  error
	; NewTree = n4(L,LC,RC,NewR,K1,V1,K2,V2,K3,V3),
	  ins_234(R,K,V,NewR)
	).

% split_and_insert:
% - if the node is a 4-node, it must be split.
% - otherwise, if node is a leaf, the 2-node or 3-node grows by 1
% - otherwise, just insert the node.
%
% param passing order:
%  s_a_i_2(left,L,R)
%  s_a_i_2(right,R,L)

split_and_insert_2(Dir,T1,T2,K1,V1,K,V,NewTree) :-
	( split(T1,SubL,K2,V2,SubR) ->
	  ( Dir = left ->
	    NewTree = n3(NewT1,NewT2,T2,K2,V2,K1,V1)
	  ; Dir = right ->
	    NewTree = n3(T2,NewT1,NewT2,K1,V1,K2,V2)
	  ; error
	  ),
	  ( K @< K2 ->
	    NewT2 = SubR, ins_234(SubL,K,V,NewT1)
	  ; K == K2 ->
	    error
	  ; NewT1 = SubL, ins_234(SubR,K,V,NewT2)
	  )
	; T1 = leaf ->      % then (T1,T2,K1,V1) + (K,V) turn into 3-node
	  ( Dir = left ->
	    NewTree = n3(leaf,leaf,T2,K,V,K1,V1)
	  ; Dir = right ->
	    NewTree = n3(T2,leaf,leaf,K1,V1,K,V)
	  )
	; Dir = left ->
	  NewTree = n2(NewL,T2,K1,V1), ins_234(T1,K,V,NewL)
	; Dir = right ->
	  NewTree = n2(T2,NewR,K1,V1), ins_234(T1,K,V,NewR)
	; error
	).

% note the subtree param passing order:
%  s_a_i_3(left,L,C,R)
%  s_a_i_3(center,C,L,R)
%  s_a_i_3(right,R,L,C)

split_and_insert_3(Dir,T1,T2,T3,K1,V1,K2,V2,K,V,NewTree) :-
	( split(T1,SubL,NewK,NewV,SubR) ->
	  ( Dir = left ->
	    NewTree = n4(NewT1,NewT2,T2,T3,NewK,NewV,K1,V1,K2,V2)
	  ; Dir = center ->
	    NewTree = n4(T2,NewT1,NewT2,T3,K1,V1,NewK,NewV,K2,V2)
	  ; Dir = right ->
	    NewTree = n4(T2,T3,NewT1,NewT2,K1,V1,K2,V2,NewK,NewV)
	  ; error
	  ),
	  ( K @< NewK ->
	    NewT2 = SubR, ins_234(SubL,K,V,NewT1)
	  ; K == NewK ->
	    error
	  ; NewT1 = SubL, ins_234(SubR,K,V,NewT2)
	  )
	; T1 = leaf ->      % (T1,T2,T3,K1,V1,K2,V2)+(K,V) turn into 4-node
	  ( Dir = left ->
	    NewTree = n4(leaf,leaf,T2,T3,K,V,K1,V1,K2,V2)
	  ; Dir = center ->
	    NewTree = n4(T2,leaf,leaf,T3,K1,V1,K,V,K2,V2)
	  ; Dir = right ->
	    NewTree = n4(T2,T3,leaf,leaf,K1,V1,K2,V2,K,V)
	  )
	; Dir = left ->
	  NewTree = n3(NewL,T2,T3,K1,V1,K2,V2), ins_234(T1,K,V,NewL)
	; Dir = center ->
	  NewTree = n3(T2,NewC,T3,K1,V1,K2,V2), ins_234(T1,K,V,NewC)
	; Dir = right ->
	  NewTree = n3(T2,T3,NewR,K1,V1,K2,V2), ins_234(T1,K,V,NewR)
	; error
	).

% split
%  - only done when parent is a 2-node or 3-node and this node
%    is a 4-node. Serves as deep guard

split(n4(L,LC,RC,R,K1,V1,K2,V2,K3,V3),SubT1,K,V,SubT2) :-
	SubT1 = n2(L,LC,K1,V1),
	K = K2,
	V = V2,
	SubT2 = n2(RC,R,K3,V3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_234(_Key,_Value,Dict,_NewTree) :- 
	var(Dict),!,error('update: 234-tree is variable').

update_234(K,V,n4(L,LC,RC,R,K1,V1,K2,V2,K3,V3),NewTree) :- !,
	TmpTree = n2(n2(L,LC,K1,V1),n2(RC,R,K3,V3),K2,V2),
	upd_234(TmpTree,K,V,NewTree).

update_234(K,V,T,NewTree) :-
	upd_234(T,K,V,NewTree).

%%%%%%%%%%

upd_234(X,_,_,_) :- var(X),!,error('upd: 234-tree is variable').

upd_234(leaf,K,V,n2(leaf,leaf,K,V)).

upd_234(n2(L,R,K1,V1),K,V,NewTree) :- 
	( K @< K1 ->
	  split_and_update_2(left,L,R,K1,V1,K,V,NewTree)
	; K == K1 ->
	  NewTree = n2(L,R,K1,V)
	; split_and_update_2(right,R,L,K1,V1,K,V,NewTree)
	).

upd_234(n3(L,C,R,K1,V1,K2,V2),K,V,NewTree) :-
	( K @< K1 ->
	  split_and_update_3(left,L,C,R,K1,V1,K2,V2,K,V,NewTree)
	; K == K1 ->
	  NewTree = n3(L,C,R,K1,V,K2,V2)
	; K @< K2 ->
	  split_and_update_3(center,C,L,R,K1,V1,K2,V2,K,V,NewTree)
	; K == K2 ->
	  NewTree = n3(L,C,R,K1,V1,K2,V)
	; split_and_update_3(right,R,L,C,K1,V1,K2,V2,K,V,NewTree)
	).

upd_234(n4(L,LC,RC,R,K1,V1,K2,V2,K3,V3),K,V,NewTree) :-
	( K @< K1 ->
	  NewTree = n4(NewL,LC,RC,R,K1,V1,K2,V2,K3,V3),
	  upd_234(L,K,V,NewL)
	; K == K1 ->
	  NewTree = n4(L,LC,RC,R,K1,V ,K2,V2,K3,V3)
	; K @< K2 ->
	  NewTree = n4(L,NewLC,RC,R,K1,V1,K2,V2,K3,V3),
	  upd_234(LC,K,V,NewLC)
	; K == K2 ->
	  NewTree = n4(L,LC,RC,R,K1,V1,K2,V ,K3,V3)
	; K @< K3 ->
	  NewTree = n4(L,LC,NewRC,R,K1,V1,K2,V2,K3,V3),
	  upd_234(RC,K,V,NewRC)
	; K == K3 ->
	  NewTree = n4(L,LC,RC,R,K1,V1,K2,V2,K3,V )
	; NewTree = n4(L,LC,RC,NewR,K1,V1,K2,V2,K3,V3),
	  upd_234(R,K,V,NewR)
	).

% split_and_update:
% - if the node is a 4-node, it must be split.
% - otherwise, if node is a leaf, the 2-node or 3-node grows by 1
% - otherwise, just update the node.
%
% param passing order:
%  s_a_i_2(left,L,R)
%  s_a_i_2(right,R,L)
%
% This could be split into 2 procedures.

split_and_update_2(Dir,T1,T2,K1,V1,K,V,NewTree) :-
	( split(T1,SubL,K2,V2,SubR) ->
	  ( Dir = left ->
	    ( K @< K2 ->
	      NewTree = n3(NewT1,NewT2,T2,K2,V2,K1,V1),
	      NewT2 = SubR, upd_234(SubL,K,V,NewT1)
	    ; K == K2 ->
	      NewTree = n3(SubL,SubR,T2,K2,V ,K1,V1)
	    ; NewT1 = SubL, 
	      NewTree = n3(NewT1,NewT2,T2,K2,V2,K1,V1),
	      upd_234(SubR,K,V,NewT2)
	    )
	  ; Dir = right ->
	    ( K @< K2 ->
	      NewTree = n3(T2,NewT1,NewT2,K1,V1,K2,V2),
	      NewT2 = SubR, upd_234(SubL,K,V,NewT1)
	    ; K == K2 ->
	      NewTree = n3(T2,NewT1,NewT2,K1,V1,K2,V )
	    ; NewT1 = SubL, 
	      NewTree = n3(T2,NewT1,NewT2,K1,V1,K2,V2),
	      upd_234(SubR,K,V,NewT2)
	    )
	  ; error
	  )
	; T1 = leaf ->      % then (T1,T2,K1,V1) + (K,V) turn into 3-node
	  ( Dir = left ->
	    NewTree = n3(leaf,leaf,T2,K,V,K1,V1)
	  ; Dir = right ->
	    NewTree = n3(T2,leaf,leaf,K1,V1,K,V)
	  ; error
	  )
	; Dir = left ->
	  NewTree = n2(NewL,T2,K1,V1), upd_234(T1,K,V,NewL)
	; Dir = right ->
	  NewTree = n2(T2,NewR,K1,V1), upd_234(T1,K,V,NewR)
	; error
	).

% note the subtree param passing order:
%  s_a_i_3(left,L,C,R)
%  s_a_i_3(center,C,L,R)
%  s_a_i_3(right,R,L,C)
%
% This could be split into 3 procedures

split_and_update_3(Dir,T1,T2,T3,K1,V1,K2,V2,K,V,NewTree) :-
	( split(T1,SubL,NewK,NewV,SubR) ->
	  ( K @< NewK ->
	    NewT2 = SubR, 
	    ( Dir = left ->
	      NewTree = n4(NewT1,NewT2,T2,T3,NewK,NewV,K1,V1,K2,V2)
	    ; Dir = center ->
	      NewTree = n4(T2,NewT1,NewT2,T3,K1,V1,NewK,NewV,K2,V2)
	    ; Dir = right ->
	      NewTree = n4(T2,T3,NewT1,NewT2,K1,V1,K2,V2,NewK,NewV)
	    ; error
	    ),
	    upd_234(SubL,K,V,NewT1)
	  ; K == NewK ->
	    SubL = NewT1, SubR = NewT2,
	    ( Dir = left ->
	      NewTree = n4(NewT1,NewT2,T2,T3,NewK,NewV,K1,V1,K2,V2)
	    ; Dir = center ->
	      NewTree = n4(T2,NewT1,NewT2,T3,K1,V1,NewK,NewV,K2,V2)
	    ; Dir = right ->
	      NewTree = n4(T2,T3,NewT1,NewT2,K1,V1,K2,V2,NewK,NewV)
	    ; error
	    )
	  ; NewT1 = SubL, 
	    ( Dir = left ->
	      NewTree = n4(NewT1,NewT2,T2,T3,NewK,NewV,K1,V1,K2,V2)
	    ; Dir = center ->
	      NewTree = n4(T2,NewT1,NewT2,T3,K1,V1,NewK,NewV,K2,V2)
	    ; Dir = right ->
	      NewTree = n4(T2,T3,NewT1,NewT2,K1,V1,K2,V2,NewK,NewV)
	    ; error
	    ),
	    upd_234(SubR,K,V,NewT2)
	  )
	; T1 = leaf ->      % (T1,T2,T3,K1,V1,K2,V2)+(K,V) turn into 4-node
	  ( Dir = left ->
	    NewTree = n4(leaf,leaf,T2,T3,K,V,K1,V1,K2,V2)
	  ; Dir = center ->
	    NewTree = n4(T2,leaf,leaf,T3,K1,V1,K,V,K2,V2)
	  ; Dir = right ->
	    NewTree = n4(T2,T3,leaf,leaf,K1,V1,K2,V2,K,V)
	  )
	; Dir = left ->
	  NewTree = n3(NewL,T2,T3,K1,V1,K2,V2), upd_234(T1,K,V,NewL)
	; Dir = center ->
	  NewTree = n3(T2,NewC,T3,K1,V1,K2,V2), upd_234(T1,K,V,NewC)
	; Dir = right ->
	  NewTree = n3(T2,T3,NewR,K1,V1,K2,V2), upd_234(T1,K,V,NewR)
	; error
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Convert dictionary to list of (Key,Value) pairs

list_234(Tree,List) :- list_234(Tree,List,[]).

list_234(X) --> { var(X),!,error('list_234: var arg') }.

list_234(leaf) --> [].

list_234(n2(L,R,K,V)) -->
	list_234(L),
	[(K,V)],
	list_234(R).

list_234(n3(L,C,R,K1,V1,K2,V2)) -->
	list_234(L),
	[(K1,V1)],
	list_234(C),
	[(K2,V2)],
	list_234(R).

list_234(n4(L,LC,RC,R,K1,V1,K2,V2,K3,V3)) -->
	list_234(L),
	[(K1,V1)],
	list_234(LC),
	[(K2,V2)],
	list_234(RC),
	[(K3,V3)],
	list_234(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Insert a list of (Key,Value) pairs into a tree

insert_list_234([],T,T).
insert_list_234([(K,V)|Xs],T0,T2) :-
	insert_234(K,V,T0,T1),
	insert_list_234(Xs,T1,T2).

update_list_234([],T,T).
update_list_234([(K,V)|Xs],T0,T2) :-
	update_234(K,V,T0,T1),
	update_list_234(Xs,T1,T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Misc. testing routines.

test_234(Keys,Tree,List) :-
	empty_234(Empty),
	insert_list_234(Keys,Empty,Tree),
	list_234(Tree,List,[]).

testu_234(Keys,Tree,List) :-
	empty_234(Empty),
	update_list_234(Keys,Empty,Tree),
	list_234(Tree,List,[]).


% The result is a tuple (Dict,Table)
% where
% - Dict maps nodes to table-indices
% - Table holds collections of labels (lists or bitvectors)

% :- ensure_loaded('../util/dict').
% :- ensure_loaded('../util/tree234').
% :- ensure_loaded('../util/basics').
% :- use_module(library(ugraphs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is how to use the table:

cfa_info(Node,(Dict,Table),Labels) :-
	lookup(Node,Dict,Index),
	aref(Index,Table,Labels).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cfa_arc_list(Local_CFA,CFA_table,Labels_to_nodes) :-
	vertices_edges_to_ugraph([],Local_CFA,CFA_graph),
	reduce(CFA_graph,SCC_graph),
	collect_labels_and_nodes(SCC_graph,SCCs,NamedSCC_graph),
	create_table(NamedSCC_graph,SCCs,CFA_table,Labels_to_nodes).

cfa_arc_list_and_stat(Local_CFA,CFA_table,Labels_to_nodes) :-
	time_check(T0),
	vertices_edges_to_ugraph([],Local_CFA,CFA_graph),
	time_check(T1),
	reduce(CFA_graph,SCC_graph),
	time_check(T2),
	length(SCC_graph,SCC_num),
	format(user_error,'~q',[SCC_graph]),
	stat_report(1,'number of sccs: ~q',[SCC_num]),
	collect_labels_and_nodes(SCC_graph,SCCs,NamedSCC_graph),
	time_check(T3),
	list_234(NamedSCC_graph,Named_Lst), length(Named_Lst,NamedSCCs),
	stat_report(1,'named scc graph: ~q',[NamedSCCs]),
	create_table(NamedSCC_graph,SCCs,CFA_table,Labels_to_nodes),
	time_check(T4),
	timing_info(1,[T0,T1,T2,T3,T4],[make_graph,scc,collect,make_table]).

label(label(_)).

relevant(fail(_,_)).
relevant(succ(_,_)).
relevant(_/_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Relies on SCC:s appearing in reverse topological order
%
% Uses 2-3-4 trees.

collect_labels_and_nodes(SCC_graph,SCCs,NamedSCCs) :-
	empty_234(E),
	collect_all(SCC_graph,1,SCCs,E,NamedSCCs).

collect_all([],N,N,D,D).
collect_all([SCC-Subparts|SCCs],M0,M2,D0,D2) :-
	collect_items(SCC,Ls,[],Ns,[]),
	insert_scc(SCC,Subparts,Ls,Ns,M0,M1,D0,D1),
	collect_all(SCCs,M1,M2,D1,D2).

collect_items([],L,L,N,N).
collect_items([X|Xs],L0,L2,N0,N2) :-
	( label(X) ->
	  L0 = [X|L1], N0 = N1
	; L0 = L1, N0 = [X|N1]
	),
	collect_items(Xs,L1,L2,N1,N2).

insert_scc(SCC,Subparts,Labels,Nodes,N0,N2,D0,D2) :-
	lookup_scc_numbers(Subparts,D0,D1,N0,N1,Subpart_numbers),
	( lookup_234(SCC,D0,(ID,_)) ->
	  N1 = N2,
	  update_234(SCC,(ID,Subpart_numbers,Nodes,Labels),D1,D2)
	; N2 is N1+1,
	  insert_234(SCC,(N1,Subpart_numbers,Nodes,Labels),D1,D2)
	).

lookup_scc_numbers([],D,D,ID,ID,[]).
lookup_scc_numbers([X|Xs],D0,D2,ID0,ID2,[N|Ns]) :-
	( lookup_234(X,D0,(N,_)) ->
	  D0 = D1, ID0 = ID1
	; ID0 = N, ID1 is ID0+1,
	  insert_234(X,(N,_),D0,D1)
	),
	lookup_scc_numbers(Xs,D1,D2,ID1,ID2,Ns).

%

convert_dict(Dict,CFA) :-
	dict_to_edges(Dict,Es),
	vertices_edges_to_ugraph([],Es,Graph),
	reduce(Graph,CFA).

dict_to_edges(Dict,Edges) :-
	list_dict(Dict,Lst),
	list_to_edges(Lst,Edges,[]).

list_to_edges([]) --> [].
list_to_edges([(X,Sub)|Xs]) -->
	item_edges(Sub,X),
	list_to_edges(Xs).

item_edges([],_) --> [].
item_edges([X|Xs],Y) -->
	[Y-X],
	item_edges(Xs,Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The table is a dictionary of (SCC,(SCC_ID,OtherSCC_IDs,Nodes,Labels))
% where
% SCC is a list of original nodes
% SCC_ID is a unique integer
% OtherSCC_IDs is a list of the SCC_IDs that are immediately reachable
% Nodes is the list of non-label nodes of the SCC
% Labels is the list of labels of the SCC
%
% The table is computed as:
% - each relevant node is given a table-index
%   (succ(P,N) and fail(P,N) nodes are relevant, while redo(P,N) are not)
% - each table index holds the labels that are reached from that point
%   (this may be a list, if labels are non-integers, or a bitvector)

% *** UNFINISHED ***
% At present uses lists of labels only

% :- ensure_loaded('../util/arrays').

create_table(SCC_dict,N,(FinalNodeMap,Table),Ls1) :-
	list_234(SCC_dict,SCCs),
	empty_array(Table),
	empty_array(TmpTable),
	empty_234(NodeMap),
	process_sccs(SCCs,TmpTable,NodeMap,FinalNodeMap),
	empty_dict(Ls0),
	process_table(N,TmpTable,Table,Ls0,Ls1).

% First create dictionary + table:
% - dictionary maps nodes to SCC-ids
% - table holds (PointsToOtherSCCs,Labels)

process_sccs([],_T,Map,Map).
process_sccs([(_,(ID,Others,Nodes,Labels))|Xs],Table,Map0,Map2) :-
	process_scc(ID,Others,Nodes,Labels,Table,Map0,Map1),
	process_sccs(Xs,Table,Map1,Map2).

process_scc(ID,Others,Nodes,Labels,Table,Map0,Map1) :-
	insert_nodes(Nodes,ID,Map0,Map1),
	aref(ID,Table,(Others,Labels)).

insert_nodes([],_ID,Map,Map).
insert_nodes([X|Xs],ID,Map0,Map2) :-
	( relevant(X) ->
	  insert_234(X,ID,Map0,Map1)
	; Map0 = Map1
	),
	insert_nodes(Xs,ID,Map1,Map2).

% Take the TmpTable and resolve the (OtherSCCs,LabelsList) into
% a list of labels.
%
% The temporary results are cached in Table to avoid useless recomputations.

process_table(0,_TmpTable,_Table,Ls0,Ls1) :- !, Ls0 = Ls1.
process_table(N,Tmp,Table,Ls0,Ls2) :- N > 0,
	compute_labels(N,Tmp,Table,Ls0,Ls1),
	M is N-1,
	process_table(M,Tmp,Table,Ls1,Ls2).

compute_labels(N,Tmp,Table,Ls0,Ls2) :-
	aref(N,Table,Elt),
	( nonvar(Elt) ->
	  Ls0 = Ls2
	; compute_labels_elts(N,Tmp,Table,Ls0,Ls1,Elts,[]),
	  sort(Elts,RawElts),
	  label_list(RawElts,Elt,Ls1,Ls2)
	).

compute_labels_elts(N,Tmp,Table,Ls0,Ls1) -->
	{ aref(N,Tmp,(Others,Labels)) },
	dlist(Labels),
	compute_labels_list(Others,Tmp,Table,Ls0,Ls1).

compute_labels_list([],_Tmp,_Tab,Ls,Ls) --> [].
compute_labels_list([X|Xs],Tmp,Tab,Ls0,Ls2) -->
	{ compute_labels(X,Tmp,Tab,Ls0,Ls1),
	  aref(X,Tab,Elt) },
	dlist(Elt),
	compute_labels_list(Xs,Tmp,Tab,Ls1,Ls2).

%

label_list([],[],L,L).
label_list([X|Xs],[(X,N)|Es],L0,L2) :-
	insert(X,N,L0,L1),
	label_list(Xs,Es,L1,L2).

l(1,[[U]-[],[W]-[],[Y]-[],[A1]-[],[C1]-[],[E1]-[],[G1]-[],[I1]-[],[K1]-[],[M1]-[],[O1]-[],[Q1]-[],[S1]-[],[U1]-[],[W1]-[],[Y1]-[],[A2]-[],[C2]-[],[E2]-[],[G2]-[],[I2]-[],[K2]-[],[M2]-[],[O2]-[],[Q2]-[],[S2]-[],[U2]-[]]).

l(2,[[W2]-[],[Y2]-[],[A3]-[],[C3]-[],[E3]-[],[G3]-[],[I3]-[],[K3]-[],[M3]-[],[O3]-[],[Q3]-[],[S3]-[],[U3]-[],[W3]-[],[Y3]-[],[A4]-[],[C4]-[],[E4]-[],[G4]-[],[I4]-[],[K4]-[],[M4]-[],[O4]-[],[Q4]-[],[S4]-[],[U4]-[],[W4]-[],[Y4]-[],[A5]-[],[C5]-[],[E5]-[],[H5]-[],[M12]-[],[O12]-[],[Q12]-[],[S12]-[],[U12]-[],[W12]-[],[I13]-[],[O13]-[],[Q13]-[],[S13]-[],[U13]-[],[Z13]-[],[B14]-[],[T15]-[],[V15]-[],[X15]-[],[Z15]-[]]).
l(3 ,[[B16]-[],[D16]-[],[F16]-[],[H16]-[],[J16]-[],[L16]-[],[N16]-[],[P16]-[],[R16]-[],[Z16]-[],[B17]-[],[D17]-[],[F17]-[],[H17]-[],[M17]-[],[O17]-[],[T17]-[],[V17]-[],[Y17]-[],[E18]-[],[G18]-[],[I18]-[],[K18]-[],[P18]-[],[R18]-[],[T18]-[],[V18]-[],[A19]-[],[C19]-[],[E19]-[],[J19]-[],[L19]-[],[N19]-[],[S19]-[],[U19]-[],[W19]-[],[Y19]-[],[D20]-[],[F20]-[],[H20]-[],[M20]-[],[O20]-[],[Q20]-[],[S20]-[],[U20]-[],[N32]-[],[K33]-[],[W33]-[],[C34]-[],[U34]-[],[W34]-[],[Y34]-[],[A35]-[],[C35]-[],[E35]-[]]).
l(4,[[Q35]-[],[W35]-[],[Y35]-[],[D36]-[],[I36]-[],[N36]-[],[S36]-[],[P38]-[],[U38]-[],[A39]-[],[C39]-[],[F40]-[],[I40]-[],[H41]-[],[J41]-[],[G42]-[],[I42]-[],[L43]-[],[N43]-[],[Z43]-[],[F44]-[],[K44]-[],[H45]-[],[J45]-[],[L45]-[],[O45]-[],[A46]-[],[H47]-[],[K47]-[],[K48]-[],[M48]-[],[O48]-[],[T48]-[],[Y48]-[],[D49]-[],[G49]-[],[M49]-[],[O49]-[],[Q49]-[],[S49]-[],[O50]-[],[E51]-[],[C52]-[],[D53]-[],[H53]-[],[R53]-[],[W53]-[],[V54]-[],[X54]-[],[Z54]-[],[E55]-[],[G55]-[],[L55]-[],[N55]-[],[S55]-[],[U55]-[],[X55]-[],[D56]-[],[F56]-[],[H56]-[],[J56]-[],[L56]-[]]).
l(5,[[[[fail(difference,3)]]]-[],[[[fail(eq,3)]]]-[],[[[fail(equal,2)]]]-[],[[[fail(falsep,2)]]]-[],[[[fail(gcd,3)]]]-[],[[[fail(lessp,3)]]]-[],[[[fail(mylength,2)]]]-[],[[[fail(nth,3)]]]-[],[[[fail(plus,3)]]]-[],[[[fail(power_eval,3)]]]-[],[[[fail(quotient,3)]]]-[],[[[fail(remainder,3)]]]-[],[[[fail(rewrite_args,3)]]]-[],[[[fail(tautology1,3)]]]-[],[[[fail(times,3)]]]-[],[[[fail(truep,2)]]]-[],[[fail(difference,3)]]-[],[[fail(eq,3)]]-[],[[fail(equal,2)]]-[],[[fail(falsep,2)]]-[],[[fail(gcd,3)]]-[],[[fail(lessp,3)]]-[],[[fail(mylength,2)]]-[],[[fail(nth,3)]]-[],[[fail(plus,3)]]-[],[[fail(power_eval,3)]]-[],[[fail(quotient,3)]]-[],[[fail(remainder,3)]]-[],[[fail(rewrite_args,3)]]-[],[[fail(tautology1,3)]]-[],[[fail(times,3)]]-[],[[fail(truep,2)]]-[]]).
l(6,[[fail(difference,3)]-[[fail(equal11,2)]],[fail(difference1,3)]-[[M12],[O13],[Z13],[[fail(difference,3)]]],[fail(difference2,3)]-[[O12],[Q13]],[fail(difference3,3)]-[[Q12],[S13]],[fail(difference4,3)]-[[S12],[U13]],[fail(difference5,3)]-[[U12],[[[fail(difference,3)]]]],[fail(difference6,3)]-[[W12],[B14]],[fail(difference7,3)]-[[[[fail(difference,3)]]],[[fail(difference,3)]]],[fail(eq,3)]-[[fail(equal15,2)]],[fail(eq01,3)]-[[T15],[E18]],[fail(eq10,3)]-[[L16],[D17],[H17],[O17],[V17],[K18],[T18],[E19],[N19],[Y19],[F20],[O20],[U20]]]).
l(7,[[fail(eq11,3)]-[[N16],[V18],[H20],[Q20],[[[fail(eq,3)]]],[[fail(eq,3)]]],[fail(eq12,3)]-[[P16],[[[fail(eq,3)]]]],[fail(eq13,3)]-[[R16],[[[fail(eq,3)]]]],[fail(eq14,3)]-[[[[fail(eq,3)]]],[[fail(eq,3)]]],[fail(eq2,3)]-[[V15],[G18]],[fail(eq3,3)]-[[X15],[Z16]],[fail(eq4,3)]-[[Z15],[B17],[F17],[M17],[T17],[I18],[P18],[A19],[J19],[S19],[D20],[M20],[S20]]]).
l(8,[[fail(eq5,3)]-[[B16],[R18]],[fail(eq6,3)]-[[D16],[C19]],[fail(eq7,3)]-[[F16],[L19]],[fail(eq8,3)]-[[H16],[U19]],[fail(eq9,3)]-[[J16],[W19]],[fail(equal,2)]-[],[fail(equal01,2)]-[[U],[[[fail(equal,2)]]]],[fail(equal10,2)]-[[M1],[[[fail(equal,2)]]]],[fail(equal11,2)]-[[O1],[[[fail(equal,2)]]]],[fail(equal12,2)]-[[Q1],[[[fail(equal,2)]]]],[fail(equal13,2)]-[[S1],[[[fail(equal,2)]]]],[fail(equal14,2)]-[[U1],[[[fail(equal,2)]]]],[fail(equal15,2)]-[[W1],[[[fail(equal,2)]]]],[fail(equal16,2)]-[[Y1],[[[fail(equal,2)]]]]]).
l(9,[[fail(equal17,2)]-[[A2],[[[fail(equal,2)]]]],[fail(equal18,2)]-[[C2],[[[fail(equal,2)]]]],[fail(equal19,2)]-[[E2],[[[fail(equal,2)]]]],[fail(equal2,2)]-[[W],[[[fail(equal,2)]]]],[fail(equal20,2)]-[[G2],[[[fail(equal,2)]]]],[fail(equal21,2)]-[[I2],[[[fail(equal,2)]]]],[fail(equal22,2)]-[[K2],[[[fail(equal,2)]]]],[fail(equal23,2)]-[[M2],[[[fail(equal,2)]]]]]).
l(10,[[fail(equal24,2)]-[[O2],[[[fail(equal,2)]]]],[fail(equal25,2)]-[[Q2],[[[fail(equal,2)]]]],[fail(equal26,2)]-[[S2],[[[fail(equal,2)]]]],[fail(equal27,2)]-[[U2],[[[fail(equal,2)]]]],[fail(equal28,2)]-[[W2],[[[fail(equal,2)]]]],[fail(equal29,2)]-[[Y2],[[[fail(equal,2)]]]],[fail(equal3,2)]-[[Y],[[[fail(equal,2)]]]],[fail(equal30,2)]-[[A3],[[[fail(equal,2)]]]],[fail(equal31,2)]-[[C3],[[[fail(equal,2)]]]],[fail(equal32,2)]-[[E3],[[[fail(equal,2)]]]]]).
l(11,[[fail(equal33,2)]-[[G3],[[[fail(equal,2)]]]],[fail(equal34,2)]-[[I3],[[[fail(equal,2)]]]],[fail(equal35,2)]-[[K3],[[[fail(equal,2)]]]],[fail(equal36,2)]-[[M3],[[[fail(equal,2)]]]],[fail(equal37,2)]-[[O3],[[[fail(equal,2)]]]],[fail(equal38,2)]-[[Q3],[[[fail(equal,2)]]]],[fail(equal39,2)]-[[S3],[[[fail(equal,2)]]]]]).
l(12,[[fail(equal4,2)]-[[A1],[[[fail(equal,2)]]]],[fail(equal40,2)]-[[U3],[[[fail(equal,2)]]]],[fail(equal41,2)]-[[W3],[[[fail(equal,2)]]]],[fail(equal42,2)]-[[Y3],[[[fail(equal,2)]]]],[fail(equal43,2)]-[[A4],[[[fail(equal,2)]]]],[fail(equal44,2)]-[[C4],[[[fail(equal,2)]]]],[fail(equal45,2)]-[[E4],[[[fail(equal,2)]]]],[fail(equal46,2)]-[[G4],[[[fail(equal,2)]]]],[fail(equal47,2)]-[[I4],[[[fail(equal,2)]]]],[fail(equal48,2)]-[[K4],[[[fail(equal,2)]]]]]).
l(13,[[fail(equal49,2)]-[[M4],[[[fail(equal,2)]]]],[fail(equal5,2)]-[[C1],[[[fail(equal,2)]]]],[fail(equal50,2)]-[[O4],[[[fail(equal,2)]]]],[fail(equal51,2)]-[[Q4],[[[fail(equal,2)]]]],[fail(equal52,2)]-[[S4],[[[fail(equal,2)]]]],[fail(equal53,2)]-[[U4],[[[fail(equal,2)]]]],[fail(equal54,2)]-[[W4],[[[fail(equal,2)]]]],[fail(equal55,2)]-[[Y4],[[[fail(equal,2)]]]],[fail(equal56,2)]-[[A5],[[[fail(equal,2)]]]],[fail(equal57,2)]-[[C5],[[[fail(equal,2)]]]]]).
l(14,[[fail(equal58,2)]-[[E5],[[[fail(equal,2)]]]],[fail(equal59,2)]-[[[[fail(equal,2)]]],[[fail(equal,2)]]],[fail(equal6,2)]-[[E1],[[[fail(equal,2)]]]],[fail(equal7,2)]-[[G1],[[[fail(equal,2)]]]],[fail(equal8,2)]-[[I1],[[[fail(equal,2)]]]],[fail(equal9,2)]-[[K1],[[[fail(equal,2)]]]],[fail(exp,3)]-[[fail(equal18,2)]],[fail(exp1,3)]-[[N32]],[fail(exp2,3)]-[[fail(exp,3)]],[fail(falsep,2)]-[],[fail(falsep1,2)]-[[[[fail(falsep,2)]]],[[fail(falsep,2)]]],[fail(falsep2,2)]-[[[fail(falsep,2)]],[fail(falsep,2)]]]).
l(15,[[fail(gcd,3)]-[[fail(equal23,2)]],[fail(gcd1,3)]-[[K33],[C34],[[fail(gcd,3)]]],[fail(gcd2,3)]-[[[[fail(gcd,3)]]],[[fail(gcd,3)]]],[fail(lessp,3)]-[[fail(equal34,2)]],[fail(lessp1,3)]-[[U34],[W35]],[fail(lessp2,3)]-[[W34],[D36]],[fail(lessp3,3)]-[[Y34],[Y35]],[fail(lessp4,3)]-[[A35],[I36]],[fail(lessp5,3)]-[[C35],[N36]],[fail(lessp6,3)]-[[E35],[S36],[[[fail(lessp,3)]]],[[fail(lessp,3)]]],[fail(lessp7,3)]-[[[[fail(lessp,3)]]],[[fail(lessp,3)]]],[fail(main,0)]-[],[fail(main1,0)]-[[fail(main,0)]],[fail(meaning,3)]-[[fail(equal37,2)]],[fail(meaning1,3)]-[[A39]],[fail(meaning2,3)]-[[C39]],[fail(meaning3,3)]-[[fail(meaning,3)]]]).
l(16,[[fail(member,2),fail(member2,2)]-[[fail(falsep2,2)],[fail(truep2,2)]],[fail(member1,2)]-[[U38]],[fail(mylength,2)]-[[fail(equal32,2)]],[fail(mylength1,2)]-[[F40],[[[fail(mylength,2)]]]],[fail(mylength2,2)]-[[[[fail(mylength,2)]]],[[fail(mylength,2)]]],[fail(mymember,3)]-[[fail(equal38,2)]],[fail(mymember1,3)]-[[H41]],[fail(mymember2,3)]-[[J41]],[fail(mymember3,3)]-[[fail(mymember,3)]],[fail(nth,3)]-[[fail(equal40,2)]]]).
l(17,[[fail(nth1,3)]-[[G42],[[[fail(nth,3)]]]],[fail(nth2,3)]-[[I42],[[[fail(nth,3)]]]],[fail(nth3,3)]-[[[fail(nth,3)]],[fail(nth,3)]],[fail(plus,3)]-[[fail(equal43,2)]],[fail(plus1,3)]-[[L43],[F44]],[fail(plus2,3)]-[[N43],[K44]],[fail(plus3,3)]-[[[[fail(plus,3)]]],[[fail(plus,3)]]],[fail(power_eval,3)]-[[fail(equal44,2)]]]).
l(18,[[fail(power_eval1,3)]-[[H45],[[[fail(power_eval,3)]]]],[fail(power_eval2,3)]-[[J45],[[[fail(power_eval,3)]]]],[fail(power_eval3,3)]-[[L45],[A46]],[fail(power_eval4,3)]-[[[[fail(power_eval,3)]]],[[fail(power_eval,3)]]],[fail(quotient,3)]-[[fail(equal47,2)]]]).
l(19,[[fail(quotient1,3)]-[[H47],[[[fail(quotient,3)]]]],[fail(quotient2,3)]-[[[[fail(quotient,3)]]],[[fail(quotient,3)]]],[fail(remainder,3)]-[[fail(equal48,2)]],[fail(remainder1,3)]-[[K48],[T48],[Y48],[D49],[M49],[S49]],[fail(remainder2,3)]-[[M48],[O49],[[fail(remainder,3)]]],[fail(remainder3,3)]-[[O48],[Q49]],[fail(remainder4,3)]-[[[[fail(remainder,3)]]],[[fail(remainder,3)]]],[fail(reverse_loop,3)]-[[fail(equal51,2)]]]).
l(20,[[fail(reverse_loop1,3)]-[[O50]],[fail(reverse_loop2,3)]-[[fail(reverse_loop,3)]],[fail(rewrite,2)]-[],[fail(rewrite1,2)]-[[P38]],[fail(rewrite2,2)]-[[fail(rewrite,2)]],[fail(rewrite_args,3)]-[[redo(succ_cont,2)]],[fail(rewrite_args1,3)]-[[C52],[[fail(rewrite_args,3)]]],[fail(rewrite_args2,3)]-[[[[fail(rewrite_args,3)]]],[[fail(rewrite_args,3)]],[fail(rewrite_args,3)]],[fail(succ_cont,2)]-[[H5],[I13],[Y17],[W33],[Q35],[I40],[Z43],[O45],[K47],[G49],[E51],[D53],[H53],[R53],[W53],[X55],[[[fail(tautology1,3)]]],[[fail(tautology1,3)]],[fail(main1,0)],[fail(rewrite2,2)],[fail(rewrite_args2,3)],[fail(tautology1,1)],[redo(succ_cont,2)]],[fail(tautology,1)]-[]]).
l(21,[[fail(tautology,3)]-[[redo(succ_cont,2)]],[fail(tautology1,1)]-[[fail(tautology,1)]],[fail(tautology1,3)]-[[fail(tautology,3)]],[fail(times,3)]-[[fail(equal56,2)]],[fail(times1,3)]-[[V54],[E55],[L55],[S55],[D56],[J56]],[fail(times2,3)]-[[X54],[F56]],[fail(times3,3)]-[[Z54],[G55],[N55],[U55],[H56],[L56]],[fail(times4,3)]-[[[[fail(times,3)]]],[[fail(times,3)]]],[fail(truep,2)]-[],[fail(truep1,2)]-[[[[fail(truep,2)]]],[[fail(truep,2)]]],[fail(truep2,2)]-[[[fail(truep,2)]],[fail(truep,2)]],[fail(wff,1)]-[],[fail(wff1,1)]-[[fail(wff,1)]],[no_label_after_goal(succ_cont,2)]-[]]).
l(22,[[redo(difference,3)]-[[fail(difference,3)],[redo(difference1,3)],[redo(difference2,3)],[redo(difference3,3)],[redo(difference4,3)],[redo(difference5,3)],[redo(difference6,3)],[redo(difference7,3)]],[redo(difference1,3)]-[[fail(difference1,3)]],[redo(difference2,3)]-[[fail(difference2,3)]],[redo(difference3,3)]-[[fail(difference3,3)]],[redo(difference4,3)]-[[fail(difference4,3)]],[redo(difference5,3)]-[[fail(difference5,3)]],[redo(difference6,3)]-[[fail(difference6,3)]],[redo(difference7,3)]-[[fail(difference7,3)]]]).
l(23,[[redo(eq,3)]-[[fail(eq,3)],[redo(eq01,3)],[redo(eq10,3)],[redo(eq11,3)],[redo(eq12,3)],[redo(eq13,3)],[redo(eq14,3)],[redo(eq2,3)],[redo(eq3,3)],[redo(eq4,3)],[redo(eq5,3)],[redo(eq6,3)],[redo(eq7,3)],[redo(eq8,3)],[redo(eq9,3)]],[redo(eq01,3)]-[[fail(eq01,3)]],[redo(eq10,3)]-[[fail(eq10,3)]],[redo(eq11,3)]-[[fail(eq11,3)]],[redo(eq12,3)]-[[fail(eq12,3)]],[redo(eq13,3)]-[[fail(eq13,3)]],[redo(eq14,3)]-[[fail(eq14,3)]]]).
l(24,[[redo(eq2,3)]-[[fail(eq2,3)]],[redo(eq3,3)]-[[fail(eq3,3)]],[redo(eq4,3)]-[[fail(eq4,3)]],[redo(eq5,3)]-[[fail(eq5,3)]],[redo(eq6,3)]-[[fail(eq6,3)]],[redo(eq7,3)]-[[fail(eq7,3)]],[redo(eq8,3)]-[[fail(eq8,3)]],[redo(eq9,3)]-[[fail(eq9,3)]],[redo(equal,2)]-[[[fail(equal,2)]],[fail(equal,2)],[redo(equal01,2)],[redo(equal10,2)],[redo(equal11,2)],[redo(equal12,2)],[redo(equal13,2)],[redo(equal14,2)],[redo(equal15,2)],[redo(equal16,2)],[redo(equal17,2)],[redo(equal18,2)],[redo(equal19,2)],[redo(equal2,2)],[redo(equal20,2)],[redo(equal21,2)],[redo(equal22,2)],[redo(equal23,2)],[redo(equal24,2)],[redo(equal25,2)],[redo(equal26,2)],[redo(equal27,2)],[redo(equal28,2)],[redo(equal29,2)],[redo(equal3,2)],[redo(equal30,2)],[redo(equal31,2)],[redo(equal32,2)],[redo(equal33,2)],[redo(equal34,2)],[redo(equal35,2)],[redo(equal36,2)],[redo(equal37,2)],[redo(equal38,2)],[redo(equal39,2)],[redo(equal4,2)],[redo(equal40,2)],[redo(equal41,2)],[redo(equal42,2)],[redo(equal43,2)],[redo(equal44,2)],[redo(equal45,2)],[redo(equal46,2)],[redo(equal47,2)],[redo(equal48,2)],[redo(equal49,2)],[redo(equal5,2)],[redo(equal50,2)],[redo(equal51,2)],[redo(equal52,2)],[redo(equal53,2)],[redo(equal54,2)],[redo(equal55,2)],[redo(equal56,2)],[redo(equal57,2)],[redo(equal58,2)],[redo(equal59,2)],[redo(equal6,2)],[redo(equal7,2)],[redo(equal8,2)],[redo(equal9,2)]]]).
l(25,[[redo(equal01,2)]-[[fail(equal01,2)]],[redo(equal10,2)]-[[fail(equal10,2)]],[redo(equal11,2)]-[[redo(difference,3)]],[redo(equal12,2)]-[[fail(equal12,2)]],[redo(equal13,2)]-[[fail(equal13,2)]],[redo(equal14,2)]-[[fail(equal14,2)]],[redo(equal15,2)]-[[redo(eq,3)]],[redo(equal16,2)]-[[fail(equal16,2)]],[redo(equal17,2)]-[[fail(equal17,2)]],[redo(equal18,2)]-[[redo(exp,3)]],[redo(equal19,2)]-[[fail(equal19,2)]]]).
l(26,[[redo(equal2,2)]-[[fail(equal2,2)]],[redo(equal20,2)]-[[fail(equal20,2)]],[redo(equal21,2)]-[[fail(equal21,2)]],[redo(equal22,2)]-[[fail(equal22,2)]],[redo(equal23,2)]-[[redo(gcd,3)]],[redo(equal24,2)]-[[fail(equal24,2)]],[redo(equal25,2)]-[[fail(equal25,2)]],[redo(equal26,2)]-[[fail(equal26,2)]],[redo(equal27,2)]-[[fail(equal27,2)]],[redo(equal28,2)]-[[fail(equal28,2)]],[redo(equal29,2)]-[[fail(equal29,2)]],[redo(equal3,2)]-[[fail(equal3,2)]]]).
l(27,[[redo(equal30,2)]-[[fail(equal30,2)]],[redo(equal31,2)]-[[fail(equal31,2)]],[redo(equal32,2)]-[[redo(mylength,2)]],[redo(equal33,2)]-[[fail(equal33,2)]],[redo(equal34,2)]-[[redo(lessp,3)]],[redo(equal35,2)]-[[fail(equal35,2)]],[redo(equal36,2)]-[[fail(equal36,2)]],[redo(equal37,2)]-[[redo(meaning,3)]],[redo(equal38,2)]-[[redo(mymember,3)]],[redo(equal39,2)]-[[fail(equal39,2)]],[redo(equal4,2)]-[[fail(equal4,2)]]]).
l(28,[[redo(equal40,2)]-[[redo(nth,3)]],[redo(equal41,2)]-[[fail(equal41,2)]],[redo(equal42,2)]-[[fail(equal42,2)]],[redo(equal43,2)]-[[redo(plus,3)]],[redo(equal44,2)]-[[redo(power_eval,3)]],[redo(equal45,2)]-[[fail(equal45,2)]],[redo(equal46,2)]-[[fail(equal46,2)]],[redo(equal47,2)]-[[redo(quotient,3)]],[redo(equal48,2)]-[[redo(remainder,3)]],[redo(equal49,2)]-[[fail(equal49,2)]],[redo(equal5,2)]-[[fail(equal5,2)]],[redo(equal50,2)]-[[fail(equal50,2)]],[redo(equal51,2)]-[[redo(reverse_loop,3)]]]).
l(29,[[redo(equal52,2)]-[[fail(equal52,2)]],[redo(equal53,2)]-[[fail(equal53,2)]],[redo(equal54,2)]-[[fail(equal54,2)]],[redo(equal55,2)]-[[fail(equal55,2)]],[redo(equal56,2)]-[[redo(times,3)]],[redo(equal57,2)]-[[fail(equal57,2)]],[redo(equal58,2)]-[[fail(equal58,2)]],[redo(equal59,2)]-[[fail(equal59,2)]],[redo(equal6,2)]-[[fail(equal6,2)]],[redo(equal7,2)]-[[fail(equal7,2)]],[redo(equal8,2)]-[[fail(equal8,2)]]]).
l(30,[[redo(equal9,2)]-[[fail(equal9,2)]],[redo(exp,3)]-[[redo(exp1,3)],[redo(exp2,3)]],[redo(exp1,3)]-[[fail(exp1,3)]],[redo(exp2,3)]-[[fail(exp2,3)]],[redo(falsep,2)]-[[redo(falsep1,2)],[redo(falsep2,2)]],[redo(falsep1,2)]-[[fail(falsep1,2)]],[redo(falsep2,2)]-[[redo(member,2),redo(member2,2)]],[redo(gcd,3)]-[[fail(gcd,3)],[redo(gcd1,3)],[redo(gcd2,3)]],[redo(gcd1,3)]-[[fail(gcd1,3)]],[redo(gcd2,3)]-[[fail(gcd2,3)]],[redo(lessp,3)]-[[fail(lessp,3)],[redo(lessp1,3)],[redo(lessp2,3)],[redo(lessp3,3)],[redo(lessp4,3)],[redo(lessp5,3)],[redo(lessp6,3)],[redo(lessp7,3)]]]).
l(31,[[redo(lessp1,3)]-[[fail(lessp1,3)]],[redo(lessp2,3)]-[[fail(lessp2,3)]],[redo(lessp3,3)]-[[fail(lessp3,3)]],[redo(lessp4,3)]-[[fail(lessp4,3)]],[redo(lessp5,3)]-[[fail(lessp5,3)]],[redo(lessp6,3)]-[[fail(lessp6,3)]],[redo(lessp7,3)]-[[fail(lessp7,3)]],[redo(main,0)]-[[redo(main1,0)]],[redo(main1,0)]-[[redo(tautology,3)]],[redo(meaning,3)]-[[redo(meaning1,3)],[redo(meaning2,3)],[redo(meaning3,3)]]]).
l(32,[[redo(meaning1,3)]-[[fail(meaning1,3)]],[redo(meaning2,3)]-[[fail(meaning2,3)]],[redo(meaning3,3)]-[[fail(meaning3,3)]],[redo(member,2),redo(member2,2)]-[[redo(member1,2)]],[redo(member1,2)]-[[fail(member1,2)]],[redo(mylength,2)]-[[[fail(mylength,2)]],[fail(mylength,2)],[redo(mylength1,2)],[redo(mylength2,2)]],[redo(mylength1,2)]-[[fail(mylength1,2)]],[redo(mylength2,2)]-[[fail(mylength2,2)]],[redo(mymember,3)]-[[redo(mymember1,3)],[redo(mymember2,3)],[redo(mymember3,3)]],[redo(mymember1,3)]-[[fail(mymember1,3)]],[redo(mymember2,3)]-[[fail(mymember2,3)]]]).
l(33,[[redo(mymember3,3)]-[[fail(mymember3,3)]],[redo(nth,3)]-[[[fail(nth,3)]],[redo(nth1,3)],[redo(nth2,3)],[redo(nth3,3)]],[redo(nth1,3)]-[[fail(nth1,3)]],[redo(nth2,3)]-[[fail(nth2,3)]],[redo(nth3,3)]-[[fail(nth3,3)]],[redo(plus,3)]-[[fail(plus,3)],[redo(plus1,3)],[redo(plus2,3)],[redo(plus3,3)]],[redo(plus1,3)]-[[fail(plus1,3)]],[redo(plus2,3)]-[[fail(plus2,3)]],[redo(plus3,3)]-[[fail(plus3,3)]],[redo(power_eval,3)]-[[[fail(power_eval,3)]],[fail(power_eval,3)],[redo(power_eval1,3)],[redo(power_eval2,3)],[redo(power_eval3,3)],[redo(power_eval4,3)]],[redo(power_eval1,3)]-[[fail(power_eval1,3)]]]).
l(34,[[redo(power_eval2,3)]-[[fail(power_eval2,3)]],[redo(power_eval3,3)]-[[fail(power_eval3,3)]],[redo(power_eval4,3)]-[[fail(power_eval4,3)]],[redo(quotient,3)]-[[[fail(quotient,3)]],[fail(quotient,3)],[redo(quotient1,3)],[redo(quotient2,3)]],[redo(quotient1,3)]-[[fail(quotient1,3)]],[redo(quotient2,3)]-[[fail(quotient2,3)]],[redo(remainder,3)]-[[fail(remainder,3)],[redo(remainder1,3)],[redo(remainder2,3)],[redo(remainder3,3)],[redo(remainder4,3)]],[redo(remainder1,3)]-[[fail(remainder1,3)]]]).
l(35,[[redo(remainder2,3)]-[[fail(remainder2,3)]],[redo(remainder3,3)]-[[fail(remainder3,3)]],[redo(remainder4,3)]-[[fail(remainder4,3)]],[redo(reverse_loop,3)]-[[redo(reverse_loop1,3)],[redo(reverse_loop2,3)]],[redo(reverse_loop1,3)]-[[fail(reverse_loop1,3)]],[redo(reverse_loop2,3)]-[[fail(reverse_loop2,3)]],[redo(rewrite,2)]-[[redo(rewrite1,2)],[redo(rewrite2,2)]],[redo(rewrite1,2)]-[[fail(rewrite1,2)]],[redo(rewrite2,2)]-[[fail(rewrite2,2)]],[redo(rewrite_args,3),redo(rewrite_args2,3)]-[[redo(rewrite_args1,3)]],[redo(rewrite_args1,3)]-[[fail(rewrite_args1,3)]]]).
l(36,[[redo(succ_cont,2)]-[],[redo(tautology,1)]-[[redo(tautology1,1)]],[redo(tautology,3)]-[[redo(tautology1,3)]],[redo(tautology1,1)]-[[redo(tautology,3)]],[redo(tautology1,3)]-[[fail(tautology1,3)]],[redo(times,3)]-[[fail(times,3)],[redo(times1,3)],[redo(times2,3)],[redo(times3,3)],[redo(times4,3)]],[redo(times1,3)]-[[fail(times1,3)]],[redo(times2,3)]-[[fail(times2,3)]],[redo(times3,3)]-[[fail(times3,3)]],[redo(times4,3)]-[[fail(times4,3)]],[redo(truep,2)]-[[redo(truep1,2)],[redo(truep2,2)]],[redo(truep1,2)]-[[fail(truep1,2)]],[redo(truep2,2)]-[[redo(member,2),redo(member2,2)]],[redo(wff,1)]-[[redo(wff1,1)]],[redo(wff1,1)]-[[fail(wff1,1)]],[succ(difference,3)]-[[succ(equal11,2)]],[succ(difference1,3)]-[[succ(difference,3)]],[succ(difference2,3)]-[[succ(difference,3)]],[succ(difference3,3)]-[[succ(difference,3)]]]).
l(37,[[succ(difference4,3)]-[[succ(difference,3)]],[succ(difference5,3)]-[[succ(difference,3)]],[succ(difference6,3)]-[[succ(difference,3)]],[succ(difference7,3)]-[[succ(difference,3)]],[succ(eq,3)]-[[succ(equal15,2)]],[succ(eq01,3)]-[[succ(eq,3)]],[succ(eq10,3)]-[[succ(eq,3)]],[succ(eq11,3)]-[[succ(eq,3)]],[succ(eq12,3)]-[[succ(eq,3)]],[succ(eq13,3)]-[[succ(eq,3)]],[succ(eq14,3)]-[[succ(eq,3)]],[succ(eq2,3)]-[[succ(eq,3)]],[succ(eq3,3)]-[[succ(eq,3)]]]).
l(38,[[succ(eq4,3)]-[[succ(eq,3)]],[succ(eq5,3)]-[[succ(eq,3)]],[succ(eq6,3)]-[[succ(eq,3)]],[succ(eq7,3)]-[[succ(eq,3)]],[succ(eq8,3)]-[[succ(eq,3)]],[succ(eq9,3)]-[[succ(eq,3)]],[succ(equal,2)]-[],[succ(equal01,2)]-[[succ(equal,2)]],[succ(equal10,2)]-[[succ(equal,2)]],[succ(equal11,2)]-[[succ(equal,2)]],[succ(equal12,2)]-[[succ(equal,2)]],[succ(equal13,2)]-[[succ(equal,2)]],[succ(equal14,2)]-[[succ(equal,2)]],[succ(equal15,2)]-[[succ(equal,2)]],[succ(equal16,2)]-[[succ(equal,2)]],[succ(equal17,2)]-[[succ(equal,2)]],[succ(equal18,2)]-[[succ(equal,2)]],[succ(equal19,2)]-[[succ(equal,2)]],[succ(equal2,2)]-[[succ(equal,2)]],[succ(equal20,2)]-[[succ(equal,2)]],[succ(equal21,2)]-[[succ(equal,2)]],[succ(equal22,2)]-[[succ(equal,2)]],[succ(equal23,2)]-[[succ(equal,2)]]]).
l(39,[[succ(equal24,2)]-[[succ(equal,2)]],[succ(equal25,2)]-[[succ(equal,2)]],[succ(equal26,2)]-[[succ(equal,2)]],[succ(equal27,2)]-[[succ(equal,2)]],[succ(equal28,2)]-[[succ(equal,2)]],[succ(equal29,2)]-[[succ(equal,2)]],[succ(equal3,2)]-[[succ(equal,2)]],[succ(equal30,2)]-[[succ(equal,2)]],[succ(equal31,2)]-[[succ(equal,2)]],[succ(equal32,2)]-[[succ(equal,2)]],[succ(equal33,2)]-[[succ(equal,2)]],[succ(equal34,2)]-[[succ(equal,2)]],[succ(equal35,2)]-[[succ(equal,2)]],[succ(equal36,2)]-[[succ(equal,2)]]]).
l(40,[[succ(equal37,2)]-[[succ(equal,2)]],[succ(equal38,2)]-[[succ(equal,2)]],[succ(equal39,2)]-[[succ(equal,2)]],[succ(equal4,2)]-[[succ(equal,2)]],[succ(equal40,2)]-[[succ(equal,2)]],[succ(equal41,2)]-[[succ(equal,2)]],[succ(equal42,2)]-[[succ(equal,2)]],[succ(equal43,2)]-[[succ(equal,2)]],[succ(equal44,2)]-[[succ(equal,2)]],[succ(equal45,2)]-[[succ(equal,2)]],[succ(equal46,2)]-[[succ(equal,2)]],[succ(equal47,2)]-[[succ(equal,2)]],[succ(equal48,2)]-[[succ(equal,2)]],[succ(equal49,2)]-[[succ(equal,2)]]]).
l(41,[[succ(equal5,2)]-[[succ(equal,2)]],[succ(equal50,2)]-[[succ(equal,2)]],[succ(equal51,2)]-[[succ(equal,2)]],[succ(equal52,2)]-[[succ(equal,2)]],[succ(equal53,2)]-[[succ(equal,2)]],[succ(equal54,2)]-[[succ(equal,2)]],[succ(equal55,2)]-[[succ(equal,2)]],[succ(equal56,2)]-[[succ(equal,2)]],[succ(equal57,2)]-[[succ(equal,2)]],[succ(equal58,2)]-[[succ(equal,2)]],[succ(equal59,2)]-[[succ(equal,2)]],[succ(equal6,2)]-[[succ(equal,2)]],[succ(equal7,2)]-[[succ(equal,2)]],[succ(equal8,2)]-[[succ(equal,2)]],[succ(equal9,2)]-[[succ(equal,2)]],[succ(exp,3)]-[[succ(equal18,2)]],[succ(exp1,3)]-[[succ(exp,3)]],[succ(exp2,3)]-[[succ(exp,3)]],[succ(falsep,2)]-[],[succ(falsep1,2)]-[[succ(falsep,2)]],[succ(falsep2,2)]-[[succ(falsep,2)]]]).
l(42,[[succ(gcd,3)]-[[succ(equal23,2)]],[succ(gcd1,3)]-[[succ(gcd,3)]],[succ(gcd2,3)]-[[succ(gcd,3)]],[succ(lessp,3)]-[[succ(equal34,2)]],[succ(lessp1,3)]-[[succ(lessp,3)]],[succ(lessp2,3)]-[[succ(lessp,3)]],[succ(lessp3,3)]-[[succ(lessp,3)]],[succ(lessp4,3)]-[[succ(lessp,3)]],[succ(lessp5,3)]-[[succ(lessp,3)]],[succ(lessp6,3)]-[[succ(lessp,3)]],[succ(lessp7,3)]-[[succ(lessp,3)]],[succ(main,0)]-[],[succ(main1,0)]-[[succ(main,0)]],[succ(meaning,3)]-[[succ(equal37,2)]],[succ(meaning1,3)]-[[succ(meaning,3)]],[succ(meaning2,3)]-[[succ(meaning,3)]],[succ(meaning3,3)]-[[succ(meaning,3)]],[succ(member,2),succ(member2,2)]-[[succ(falsep2,2)],[succ(truep2,2)]],[succ(member1,2)]-[[succ(member,2),succ(member2,2)]],[succ(mylength,2)]-[[succ(equal32,2)]],[succ(mylength1,2)]-[[succ(mylength,2)]],[succ(mylength2,2)]-[[succ(mylength,2)]]]).
l(43,[[succ(mymember,3)]-[[succ(equal38,2)]],[succ(mymember1,3)]-[[succ(mymember,3)]],[succ(mymember2,3)]-[[succ(mymember,3)]],[succ(mymember3,3)]-[[succ(mymember,3)]],[succ(nth,3)]-[[succ(equal40,2)]],[succ(nth1,3)]-[[succ(nth,3)]],[succ(nth2,3)]-[[succ(nth,3)]],[succ(nth3,3)]-[[succ(nth,3)]],[succ(plus,3)]-[[succ(equal43,2)]],[succ(plus1,3)]-[[succ(plus,3)]],[succ(plus2,3)]-[[succ(plus,3)]],[succ(plus3,3)]-[[succ(plus,3)]],[succ(power_eval,3)]-[[succ(equal44,2)]],[succ(power_eval1,3)]-[[succ(power_eval,3)]],[succ(power_eval2,3)]-[[succ(power_eval,3)]],[succ(power_eval3,3)]-[[succ(power_eval,3)]],[succ(power_eval4,3)]-[[succ(power_eval,3)]]]).
l(44,[[succ(quotient,3)]-[[succ(equal47,2)]],[succ(quotient1,3)]-[[succ(quotient,3)]],[succ(quotient2,3)]-[[succ(quotient,3)]],[succ(remainder,3)]-[[succ(equal48,2)]],[succ(remainder1,3)]-[[succ(remainder,3)]],[succ(remainder2,3)]-[[succ(remainder,3)]],[succ(remainder3,3)]-[[succ(remainder,3)]],[succ(remainder4,3)]-[[succ(remainder,3)]],[succ(reverse_loop,3)]-[[succ(equal51,2)]],[succ(reverse_loop1,3)]-[[succ(reverse_loop,3)]],[succ(reverse_loop2,3)]-[[succ(reverse_loop,3)]],[succ(rewrite,2)]-[],[succ(rewrite1,2)]-[[succ(rewrite,2)]],[succ(rewrite2,2)]-[[succ(rewrite,2)]],[succ(rewrite_args,3),succ(rewrite_args2,3)]-[]]).
l(45,[[succ(rewrite_args1,3)]-[[succ(rewrite_args,3),succ(rewrite_args2,3)]],[succ(succ_cont,2)]-[[no_label_after_goal(succ_cont,2)],[succ(difference,3)],[succ(eq,3)],[succ(equal,2)],[succ(gcd,3)],[succ(lessp,3)],[succ(main1,0)],[succ(mylength,2)],[succ(plus,3)],[succ(power_eval,3)],[succ(quotient,3)],[succ(remainder,3)],[succ(rewrite2,2)],[succ(rewrite_args,3),succ(rewrite_args2,3)],[succ(tautology1,1)],[succ(tautology1,3)],[succ(times,3)]],[succ(tautology,1)]-[]]).
l(46,[[succ(tautology,3)]-[[succ(main1,0)],[succ(tautology1,1)]],[succ(tautology1,1)]-[[succ(tautology,1)]],[succ(tautology1,3)]-[[succ(tautology,3)]],[succ(times,3)]-[[succ(equal56,2)]],[succ(times1,3)]-[[succ(times,3)]],[succ(times2,3)]-[[succ(times,3)]],[succ(times3,3)]-[[succ(times,3)]],[succ(times4,3)]-[[succ(times,3)]],[succ(truep,2)]-[],[succ(truep1,2)]-[[succ(truep,2)]],[succ(truep2,2)]-[[succ(truep,2)]],[succ(wff,1)]-[],[succ(wff1,1)]-[[succ(wff,1)]]]).

test(N) :-
l(N,Lst),
numbervars(Lst,0,_),
collect_labels_and_nodes(Lst,SCC,N_SCC).

error(X) :- format("Error: ~w~n",[X]).
