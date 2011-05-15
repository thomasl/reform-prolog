%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  tree.pl
%
%
/*
:- module tree.
*/



tree_lookup(Tree,_Key) :- var(Tree), !,
	fail.

tree_lookup(tree(Left,Entry,Right), Key) :- 
	( Entry = Key ->
	    true
	; Entry @< Key ->
	    tree_lookup(Left, Key)
	; Entry @> Key ->
	    tree_lookup(Right, Key)
	).


tree_insert(Tree, Key) :- var(Tree), !,
	Tree = tree(_,Key,_).

tree_insert(tree(Left,Entry,Right), Key) :-
	( Entry @< Key ->
	    tree_insert(Left, Key)
	; Entry @> Key ->
	    tree_insert(Right, Key)
	; true ).

