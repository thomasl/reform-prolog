%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%			DICTIONARIES OF VALUES
%
% Used by parmatch and build, this file provides the handling of
% environments (dictionaries). We say that a key is _defined_ in
% a dictionary if it has a value therein. Keys and Values may be
% arbitrary terms and are guaranteed not to be instantiated by these
% procedures.
%
% The following operations are provided:
%
%  empty_dict(X):  X is an empty dictionary
%
%  lookup(Key,Dict,Val): in dictionary Dict, Key has Value. If Key is
%    not present in dictionary, fail.
%
%  insert(Key,Val,OldDict,NewDict): NewDict is OldDict with Key defined as
%    Val. If Key is defined in OldDict, an error results.
%
%  update(Key,Val,OldDict,NewDict): As insert/4, but replaces the definition
%    of Key if present in OldDict with Val.
%
%  delete(Key,OldDict,NewDict): Key is deleted from OldDict if present. If
%    key is not present, an error occurs. NewDict is OldDict with Key deleted.
%

empty_dict(leaf).

%

defined(X,Dict) :-
	lookup(X,Dict,_).

% Provides an in-order listing that prolly yields better results
% on re-inserting keys.

list_dict(Dict,List) :- list_dict(Dict,List,[]).

list_dict(leaf) --> [].

list_dict(node(L,K,V,R)) --> [(K,V)],
	list_dict(L),
	list_dict(R).

%

list_dict_keys(Dict,Vals) :-
	list_dict_keys(Dict,Vals,[]).

list_dict_keys(leaf) --> [].

list_dict_keys(node(L,K,_V,R)) --> [K],
	list_dict_keys(L),
	list_dict_keys(R).

%

list_dict_values(Dict,Vals) :-
	list_dict_values(Dict,Vals,[]).

list_dict_values(leaf) --> [].

list_dict_values(node(L,_K,V,R)) --> [V],
	list_dict_values(L),
	list_dict_values(R).

%

lookup_list([],_Dict,[]).

lookup_list([X|Xs],D0,[V|Vs]) :-
	lookup(X,D0,V),
	lookup_list(Xs,D0,Vs).
%

insert_list([],Dict,Dict).

insert_list([(K,V)|Xs],D0,D2) :-
	insert(K,V,D0,D1),
	insert_list(Xs,D1,D2).

%

init_dict(List,Dict) :-
	empty_dict(Empty),
	insert_list(List,Empty,Dict).

%

insert_dummy_list([],Dict,Dict).

insert_dummy_list([K|Xs],D0,D2) :-
	insert(K,dummy,D0,D1),
	insert_dummy_list(Xs,D1,D2).

%

init_dummy_dict(List,Dict) :-
	empty_dict(Empty),
	insert_dummy_list(List,Empty,Dict).

%

update_list([],Dict,Dict).

update_list([(K,V)|Xs],D0,D2) :-
	update(K,V,D0,D1),
	update_list(Xs,D1,D2).

%

lookup(X,E0,Val) :-
	nonvar(E0),!,
	look_dict(E0,X,Val).

lookup(_,_,_) :-
	sys_error('lookup/3: dictionary is variable'),
	fail.

look_dict(node(L,K,V,R),K0,V0) :-
	( K0 == K ->
	  V0 = V
	; K0 @< K ->
	  look_dict(L,K0,V0)
	; K0 @> K ->
	  look_dict(R,K0,V0)
	).

%

insert(X,Val,E0,E1) :-
	nonvar(E0),
	( ins_dict(E0,X,Val,E1) ->
	  true
	; sys_error('insert/4: failed for key ~q',[X]),
	  fail
	).

ins_dict(leaf,K0,V0,node(leaf,K0,V0,leaf)).

ins_dict(node(L,K,V,R),K0,V0,Dict) :-
	( K0 == K ->
	  sys_error('ins_dict/4: key already present ~q',[K]),
	  fail
	; K0 @< K ->
	  Dict = node(L1,K,V,R),
	  ins_dict(L,K0,V0,L1)
	; K0 @> K ->
	  Dict = node(L,K,V,R1),
	  ins_dict(R,K0,V0,R1)
	).

%

update(X,Val,E0,E1) :-
	nonvar(E0),!,
	( upd_dict(E0,X,Val,E1) ->
	  true
	; sys_error('update/4: failed for key ~q',[X]),
	  fail
	).

update(_,_,_,_) :-
	sys_error('update/4: dictionary is variable').

%

upd_dict(leaf,K0,V0,node(leaf,K0,V0,leaf)).

upd_dict(node(L,K,V,R),K0,V0,Dict) :-
	( K0 == K ->
	  Dict = node(L,K0,V0,R)
	; K0 @< K ->
	  Dict = node(L1,K,V,R),
	  upd_dict(L,K0,V0,L1)
	; K0 @> K ->
	  Dict = node(L,K,V,R1),
	  upd_dict(R,K0,V0,R1)
	).

%

delete(X,E0,E1) :-
	( del_dict(E0,X,E1) ->
	  true
	; sys_error('delete/3: failed for key ~q',[X]),
	  fail
	).

del_dict(node(L,K,V,R),K0,Dict) :-
	( K0 == K ->
	  reorg_dict(R,L,Dict)
	; K0 @< K ->
	  Dict = node(L1,K,V,R),
	  del_dict(L,K0,L1)
	; K0 @> K ->
	  Dict = node(L,K,V,R1),
	  del_dict(R,K0,R1)
	).

reorg_dict(leaf,Branch,Branch).

reorg_dict(node(L,K,V,R),Branch,node(L1,K,V,R)) :-
	reorg_dict(L,Branch,L1).

%

replace(X,Old,New,D0,D1) :-
	rep(D0,X,Old,New,D1).

rep(node(L,K,V,R),K0,OldV0,NewV0,Dict) :-
	( K0 == K ->
	  OldV0 = V,
	  Dict = node(L,K,NewV0,R)
	; K0 @< K ->
	  Dict = node(L0,K,V,R),
	  rep(L,K0,OldV0,NewV0,L0)
	; K0 @> K ->
	  Dict = node(L,K,V,R0),
	  rep(R,K0,OldV0,NewV0,R0)
	).

% In some cases, balancing a dictionary so that the distance to
% all nodes is guaranteed to be approximately log N is useful.
% In balance_dict/2, this is ensured afterwards.

balance_dict(D0,D0).

%

dictionary(leaf).

dictionary(node(_L,_K,_V,_R)).


% Displaying a dictionary:

portray(Dict) :- dictionary(Dict),!,portray_dict(Dict).

portray_dict(leaf) :-
	format('dict{}',[]).

portray_dict(node(_,_,_,_)) :-
	format('dict{...}',[]).

%% saving and restoring dictionaries from streams

save_dict_to_file(Dict,File) :-
	open(File,write,Wr),
	save_dict(Dict,Wr),
	close(Wr).

save_dicts_to_file(File,Dicts) :-
	open(File,write,Wr),
	save_dicts(Dicts,Wr),
	close(Wr).

save_dicts([],_).
save_dicts([D|Ds],Wr) :-
	save_dict(D,Wr),
	save_dicts(Ds,Wr).

%

save_dict(Dict,Wr) :-
	list_dict(Dict,Lst),
	format(Wr,'start_dict.~n',[]),
	save_dict_entries(Lst,Wr),
	format(Wr,'end_dict.~n',[]).

save_dict_entries([],_).
save_dict_entries([X|Xs],Wr) :-
	write_canonical(Wr,X), write(Wr,'.'),nl(Wr),
	save_dict_entries(Xs,Wr).

%

restore_dict_from_file(File,Dict) :-
	open(File,read,Rd),
	restore_dict(Rd,Dict),
	close(Rd).

restore_dicts_from_file(File,Dicts) :-
	open(File,read,Rd),
	restore_dicts(Dicts,Rd),
	close(Rd).

restore_dicts([],_).
restore_dicts([D|Ds],Rd) :-
	restore_dict(Rd,D),
	restore_dicts(Ds,Rd).

%

restore_dict(Rd,Dict) :-
	empty_dict(D0),
	( read(Rd,start_dict) ->
	  restore_dict_entries(Rd,D0,Dict)
	; error('% restore_dict: stream does not start with start_dict.~n')
	).

restore_dict_entries(Rd,D0,D2) :-
	read(Rd,X),
	( X = end_dict ->
	  D0 = D2
	; X = (K,V) ->
	  insert(K,V,D0,D1),
	  restore_dict_entries(Rd,D1,D2)
	; error('% restore_dict: item ~q does not have (Key,Value) form~n',
		[X])
	).

% Formatted output of entries

format_dict(Dict) :-
	list_dict(Dict,KVlist,[]),
	format_kv_pairs(KVlist).

%

format_kv_pairs([]).

format_kv_pairs([(K,V)|KVs]) :-
	format('~q ->~n        ~q~n',[K,V]),
	format_kv_pairs(KVs).

%% Write dict to a file.

format_dict(Dict,File) :-
	list_dict(Dict,BagKVlist,[]),
	sort(BagKVlist,KVlist),
	open(File,write,Stream),
	format_kv_pairs(KVlist,Stream),
	close(Stream).

format_kv_pairs([],_Stream).

format_kv_pairs([(K,V)|KVs],Stream) :-
	format(Stream,'~q ->~n        ~q.~n',[K,V]),
	format_kv_pairs(KVs,Stream).

depth_limit(3).

%%%
% Sometimes, it is useful to store nonground terms in a non-tamperable
% format and then return them to their nonground state.

ground_term(Term,GroundedTerm) :-
	( 'contains_$VAR_terms'(Term) ->
	  error('% ground_entry/2: term contains $VAR -- ~q~n',[Term])
	; copy_term(Term,GroundedTerm),
	  enumeratevars(GroundedTerm,0,_)
	).

%

'contains_$VAR_terms'(X) :- var(X),!,fail.

'contains_$VAR_terms'(X) :- atomic(X),!,fail.

'contains_$VAR_terms'('$VAR'(_)) :- !.

'contains_$VAR_terms'(PX) :-
	PX =.. [_|Xs],
	member(X,Xs),
	'contains_$VAR_terms'(X),!.

%%%

unground_term(GroundedTerm,Term) :-
	empty_dict(E),
	ungnd(GroundedTerm,E,_,Term).

% Actually 'unground' a term (i.e. replace '$VAR'(N) with "X").

ungnd(X,_,_,_) :- var(X),!,
	error('% ungnd: variable in supposed ground term~n').

ungnd(X,E0,E1,Y) :- atomic(X),!, E0 = E1, Y = X.

ungnd('$VAR'(N),E0,E1,Y) :- !,
	( lookup(N,E0,Y) ->
	  E1 = E0
	; insert(N,Y,E0,E1)
	).

ungnd(PX,E0,E1,PY) :-
	PX =.. [P|Xs],
	ungnd_list(Xs,E0,E1,Ys),
	PY =.. [P|Ys].

ungnd_list([],E,E,[]).
ungnd_list([X|Xs],E0,E2,[Y|Ys]) :-
	ungnd(X,E0,E1,Y),
	ungnd_list(Xs,E1,E2,Ys).
