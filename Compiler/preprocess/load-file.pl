%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%		    READING A FILE INTO A DATABASE
%
% The program database can be represented explicitly. Here, it is done
% by a binary tree where nodes are predicates:
%
%  Functor -> Information
%
% The information part is both attendant flags as well as clauses. When
% clauses are retrieved from the database, they should be copied.
% This is done by the standard predicates.
%
% * fload/2 takes a filename or list of filenames and returns a database
%   of clauses as read from the files and expanded by standard term
%   expansion.
%
% * fload_desugar/2 takes the arguments of fload/2, but also expands away
%   the use of syntactic sugar and cuts into internal predicates. See the
%   returned database for relevant examples.
% 
% * The predicate to retrieve all clauses of a predicate (given as a functor)
%   is clauses/3. The returned term is a copy of the list of clauses in
%   top-to-bottom order.
%
% * To add information to a predicate, use add_directive(PN,Dir,DB0,DB1)
%   that takes a functor PN, a new directive (your info!) and a database DB0
%   and returns DB1.
%
% * To update information on a predicate, use update_directive(PN,Dir,DB0,DB1)
%   that works like add_directive/4 but removes previous directives of the
%   same functor.
%

:- ensure_loaded('../util/dict').
:- ensure_loaded('../util/error').
:- ensure_loaded('../util/basics').
:- ensure_loaded('../util/filename').
:- ensure_loaded('../util/enumeratevars').

:- ensure_loaded(expander).


:- dynamic '$undo_op'/3.


fload(Files,Database) :-
	fload(Files,Database,_Directives).

fload(Files,Database,Directives) :-
	empty_database(DB),
	empty_database(Ds),
	fload_files(Files,unexpanded/multiple,DB,Database,Ds,Directives,[],_),
	kill_defined_operators.


fload_desugar(Files,Database) :-
	fload_desugar(Files,Database,_Directives).

fload_desugar(Files,Database,Directives) :-
	empty_database(DB),
	empty_database(Ds),
	fload_files(Files,expanded/multiple,DB,Database,Ds,Directives,[],_),
	kill_defined_operators.


%% These variants only load the named files (i.e. consult et al are ignored).
%
fload_one(Files,Database,Directives) :-
	empty_database(DB),
	empty_database(Ds),
	fload_files(Files,unexpanded/single,DB,Database,Ds,Directives,[],_),
	kill_defined_operators.

fload_desugar_one(Files,Database,Directives) :-
	empty_database(DB),
	empty_database(Ds),
	fload_files(Files,expanded/single,DB,Database,Ds,Directives,[],_),
	kill_defined_operators.

%%%
%%  fload_files(+Files,+Mode,+/-Database,+/-Directives,+/-Visited)
%
fload_files([],_Mode,DB0,DB1,D0,D1,V0,V1) :- !,
	DB0 = DB1, D0 = D1, V0 = V1.

fload_files(X,Mode,DB0,DB1,D0,D1,V0,V1) :- atom(X), !,
	fload_file(X,Mode,DB0,DB1,D0,D1,V0,V1).

fload_files([X|Xs],Mode,DB0,DB2,D0,D2,V0,V2) :-
	fload_file(X,Mode,DB0,DB1,D0,D1,V0,V1),
	fload_files(Xs,Mode,DB1,DB2,D1,D2,V1,V2).

%%%
%%  fload_file(+File,+Mode,+/-Database,+/-Directives,+/-Visited)
%
fload_file(File,Mode,DB0,DB1,D0,D1,V0,V1) :-
	absolute_file_name(File,FileName),
	( visited(FileName,V0) ->
	    DB0 = DB1, D0 = D1, V0 = V1
	; true,
	  ( open(FileName,read,Stream) ->
	      report('preloading file ~q...',[FileName]),
	      read(Stream,X),
	      Vx = [FileName|V0],
	      fload_stream(X,Stream,FileName,Mode,[],DB0,DB1,D0,D1,Vx,V1),
	      close(Stream)
	  ; true,
	      warning('unable to open file ~q',[FileName]),
	      DB0 = DB1, D0 = D1, V0 = V1
	  )
	).


visited(X,Files) :- member(X,Files).

%

fload_stream(end_of_file,_Strm,File,Mode,Item,DB0,DB1,D0,D1,V0,V1) :- !,
	V0 = V1,
	enter_item(Item,File,Mode,DB0,DB1,D0,D1).

fload_stream((:- D),Stream,File,Mode,Item,DB0,DB3,D0,D3,V0,V3) :- !,
	enter_item(Item,File,Mode,DB0,DB1,D0,D1),
	insert_directive(D,File,D1,Dx),
	enter_directive(D,File,Mode,DB1,DB2,Dx,D2,V0,V2),
	read(Stream,X),
	fload_stream(X,Stream,File,Mode,[],DB2,DB3,D2,D3,V2,V3).

fload_stream(Term,Stream,File,Mode,Item,DB0,DB2,D0,D2,V0,V2) :-
	expand_term(Term,ExpandedTerm),
	read(Stream,X),
	( belongs_to_item(ExpandedTerm,Item) ->
	    fload_stream(X,Stream,File,Mode,[ExpandedTerm|Item],
	                   DB0,DB2,D0,D2,V0,V2)
	; true,
	    enter_item(Item,File,Mode,DB0,DB1,D0,D1),
	    fload_stream(X,Stream,File,Mode,[ExpandedTerm],DB1,DB2,D1,D2,V0,V2)
	).


/*******************************************************************************
clauses(P/N,DB,CopyClauses) :- !,
	( lookup(P/N,DB,Info) ->
	  Info = info(_CompileMode,Clauses,_Directives,_File),
	  copy_term(Clauses,CopyClauses)
	; CopyClauses = []
	).

clauses(Call,DB,CopyClauses) :-
	functor(Call,P,N), PN = P/N,
	( lookup(PN,DB,Info) ->
	  Info = info(_CompileMode,Clauses,_Directives,_File),
	  copy_term(Clauses,CopyClauses)    % or the DB will decay
	; CopyClauses = []
	).
*******************************************************************************/


%%%
%%  enter_item(+Clauses,+File,+Mode,+/-Database,+/-Directives)
%
enter_item(    [],_File,_Mode, DBx,DBx, Dx,Dx).

enter_item([C|Cs], File, Mode, DBi,DBs, Di,Ds) :-
	reverse([C|Cs], ClauseList), % really unnecessary ...
	clause_functor(C, P, N),
	Mode = Read/_Load,
	( Read == unexpanded ->
	    information_item(ClauseList, [], File, Info),
	    update_definition(P/N, Info, DBi,DBs, Di,Ds)
	; Read == expanded ->
 	    ( lookup(P/N, DBi, info(_CompileMode,_Clauses,Directives,_File)) ->
		( has_directive(dynamic, Directives) -> %%% suppress expansion
		    ClauseList = ExpClauses,
		    Spawns = []
		; has_directive(parallel, Directives) ->
		    uniform_clauses(ClauseList, ExpClauses),
		    Spawns = []
		; true,
		    expand_clause_list(ClauseList, ExpClauses, Spawns,[])
		),
		information_item(ExpClauses, [], File, Info),
		update_definition(P/N, Info, DBi,DBj, Di,Ds),
		enter_spawned_predicates(Spawns, File, DBj,DBs)
	    ; true,
		expand_clause_list(ClauseList, ExpClauses, Spawns,[]),
		information_item(ExpClauses, [], File, Info),
		update_definition(P/N, Info, DBi,DBj, Di,Ds),
		enter_spawned_predicates(Spawns, File, DBj,DBs)
	    )
	; true,
	    sys_error('enter_item/5: unknown mode (~q) during file load',[Read])
	).


%%%
%%  uniform_clauses(+Clauses,-UniformClauses)
%
uniform_clauses(    [],     []).

uniform_clauses([X|Xs], [Y|Ys]) :-
	( X = (_ :- _) ->
	    Y = X
	; true, %%% X is a fact.
	    Y = (X :- true)
	),
	uniform_clauses(Xs, Ys).


%%%
%%  expand_clause_list(+Clauses,-ExpandedClauses,+/-Spawns)
%
expand_clause_list(    [],     [], Spawns,   Spawns).

expand_clause_list([X|Xs], [Y|Ys], SpawnsIn, SpawnsOut) :-
	( X = (_ :- _) ->
	    C = X
	; true, %%% X is a fact.
	    C = (X :- true)
	),
	expand_clause(C, Y, SpawnsIn, SpawnsTmp),
	expand_clause_list(Xs, Ys, SpawnsTmp, SpawnsOut).


%%%
%%  enter_spawned_predicates(+Spawns,+File,+/-Database)
%
% Note: All spawns are already fully expanded.
%
enter_spawned_predicates(    [],_File, DBx,DBx).

enter_spawned_predicates([S|Ss], File, DBi,DBs) :- [S|Ss] = Spawns,
	clause_functor(S,P,N),
	take_spawned_clauses(Spawns, P,N, SpawnsLeft, Clauses,[]),
	information_item(Clauses, [], File, Info),
	update_definition(P/N,Info, DBi,DBj, _,_),
	enter_spawned_predicates(SpawnsLeft, File, DBj,DBs).

%%%
%%  take_spawned_clauses(+Spawns,+Name,+Arity,-SpawnsLeft) --> Clauses,Handle
%
take_spawned_clauses(    [],_P,_N, []) --> [].

take_spawned_clauses([X|Xs], P, N, Ys) -->
	( { clause_functor(X,P,N) } ->
	  [X],
	  take_spawned_clauses(Xs, P,N, Ys)
	; { Ys = [X|Xs] } ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%   Directives are sometimes general goals, including goals that might read
% other files as well. Other directives describe properties of the associated
% predicate or predicates.
%
% Note: (interpretation of directives)
%
% - a directive might be a nullary operation, e.g. "dynamic foo/1", that
%   tells us something (meta logical) about the predicate. In the example,
%   foo/1 gets a 'dynamic' in its directive list.
%
% - descriptors are written as ":- mode([append/3=append(gnd,gnd,any),...])."
%   (according to the directive concerned). In the example, append/3 gets a
%   "mode(append(gnd,any,any))" in its directive list.
%
%
% Note: Directives do not check if a directive and its associated predicate
%       occurs in the same file. If they do not, there is a possible error.
%
%
enter_directive([X|Xs],File,Mode,DB0,DB1,D0,D1,V0,V1) :- !,
	Mode = _Read/Load,
	( Load == single ->
	    DB0 = DB1, D0 = D1, V0 = V1
	; Load == multiple ->
	    relative_file_names([X|Xs],File,[Y|Ys]),
	    fload_files([Y|Ys],Mode,DB0,DB1,D0,D1,V0,V1)
	; true,
	    sys_error('unknown loading mode: ~q',[Load])
	).

enter_directive(ensure_loaded(Xs),File,Mode,DB0,DB1,D0,D1,V0,V1) :- !,
	Mode = _Read/Load,
	( Load == single ->
	    DB0 = DB1, D0 = D1, V0 = V1
	; Load == multiple ->
	    relative_file_names(Xs,File,Ys),
	    fload_files(Ys,Mode,DB0,DB1,D0,D1,V0,V1)
	; true,
	    sys_error('unknown loading mode: ~q',[Load])
	).

enter_directive((public Ps),File,_Mode,DB0,DB1,D0,D1,V0,V1) :- !,
	D0 = D1, V0 = V1,
	enter_directives(Ps,public,File,DB0,DB1).

enter_directive((dynamic Ps),File,_Mode,DB0,DB1,D0,D1,V0,V1) :- !,
	D0 = D1, V0 = V1,
	enter_directives(Ps,dynamic,File,DB0,DB1).

enter_directive((multifile Ps),File,_Mode,DB0,DB1,D0,D1,V0,V1) :- !,
	D0 = D1, V0 = V1,
	enter_directives(Ps,multifile,File,DB0,DB1).

enter_directive(mode(Ps),File,_Mode,DB0,DB1,D0,D1,V0,V1) :- !,
	D0 = D1, V0 = V1,
	enter_descriptors(Ps,mode,File,DB0,DB1).

enter_directive(parallel(Xs),File,_Mode,DB0,DB1,D0,D1,V0,V1) :- !,
	D0 = D1, V0 = V1,
	( Xs = [] ->
	    DB0 = DB1
	; true,
	    list_to_conj(Xs,Ps),
	    enter_directives(Ps,parallel,File,DB0,DB1)
	).

enter_directive(callable(Xs),File,_Mode,DB0,DB1,D0,D1,V0,V1) :- !,
	D0 = D1, V0 = V1,
	( Xs = [] ->
	    DB0 = DB1
	; true,
	    list_to_conj(Xs,Ps),
	    enter_directives(Ps,callable,File,DB0,DB1)
	).

enter_directive(op(Prec,Assoc,Ops),_File,_Mode,DB0,DB1,D0,D1,V0,V1) :- !,
	V0 = V1,
	D0 = D1,
	DB0 = DB1,
	assert('$undo_op'(op(Prec,Assoc,Ops))),
	op(Prec,Assoc,Ops).

enter_directive(D,_File,_Mode,DB0,DB1,D0,D1,V0,V1) :-
	D0 = D1,
	V0 = V1,
	DB0 = DB1,
	warning('directive not supported: ~q',[D]).


enter_directives((PN,Ps),Directive,File,DB0,DB2) :- !,
	( lookup(PN,DB0,Item) ->
	    Item = info(CM,Cs,Ds,F),
	    Info = info(CM,Cs,[Directive|Ds],F),
	    update(PN,Info,DB0,DB1)
	; true,
	    information_item([],[Directive],File,Info),
	    insert(PN,Info,DB0,DB1)
	),
	enter_directives(Ps,Directive,File,DB1,DB2).

enter_directives(PN,Directive,File,DB0,DB1) :-
	( lookup(PN,DB0,Item) ->
	    Item = info(CM,Cs,Ds,F),
	    Info = info(CM,Cs,[Directive|Ds],F),
	    update(PN,Info,DB0,DB1)
	; true,
	    information_item([],[Directive],File,Info),
	    insert(PN,Info,DB0,DB1)
	).


enter_descriptors(    [],_Directive,_File, DB,  DB).

enter_descriptors([X|Xs], Directive, File, DB0, DB2) :-
	X = (PN = Descriptor),
	D =.. [Directive,Descriptor],
	( lookup(PN,DB0,Item) ->
	    Item = info(CM,Cs,Ds,F),
	    Info = info(CM,Cs,[D|Ds],F),
	    update(PN,Info,DB0,DB1)
	; true,
	    information_item([],[D],File,Info),
	    insert(PN,Info,DB0,DB1)
	),
	enter_descriptors(Xs,Directive,File,DB1,DB2).


%%%
%%  The directives of a file are given in a list, in the order they occurred
% (but independent of the rest of the program).
%
insert_directive(Directive, File, D0, D1) :-
	( lookup(File, D0, Item) ->
	    append(Item, [Directive], NewItem)
	; NewItem = [Directive]
	),
	update(File, NewItem, D0, D1).

%

belongs_to_item(NextClause, [LastClause|_Cs]) :-
	clause_functor(NextClause, P, N),
	clause_functor(LastClause, P, N).


%%%
%%  update_definition(+Predicate,+Info,+/-Database)
%
%   We need to handle a number of cases when updating the clause database,
% depending on whether the definition (predicate) has any dynamic properties
% (i.e. directives such as dynamic or multifile) or not.
%
% If the definition is known to the clause database:
% 
%  o	If it is a multifile definition we just append any new clauses
%	to the existing ones.
%
%  o	If it is a dynamic definition we include the new clauses in the
%	collective directive list (in order for the clauses to be compiled
%	as load directivs which will assert the clauses).
%
%  o	If it is neither multifile nor dynamic, but reside in the same file
%	as a previous definition, we handle two cases;	1) if the current
%	definition contains no clauses we include the new set of clauses
%	and possible directives, otherwise 2) a warning is reported and
%	the new definition is inserted into the clause database.
%
% If the definition is new to the clause database:
%
%  o	If it is a dynamic definition we include the clauses in the
%	directive list, otherwise we include the definition in the
%	clause database.
%
%
% Note: The current version does not handle definitions which are both
%	dynamic and multifile.
%
%
update_definition(PN, Info, DB0,DB1) :-
	empty_dict(D),
	update_definition(PN, Info, DB0,DB1, D,_).

update_definition(PN, info(CM,Cs,Ds,F), DB0,DB1, D0,D1) :-
	( lookup(PN,DB0,Item) ->
	    Item = info(CM,Cs1,Ds1,F1),
	    ( has_directive(multifile,Ds1) ->
	        D0 = D1,
		append(Cs1,Cs,NewCs),
		append(Ds1,Ds,NewDs),
		update(PN,info(CM,NewCs,NewDs,F),DB0,DB1)
	    ; has_directive(dynamic,Ds1) ->
		DB0 = DB1,
		enter_asserted_clauses_list(Cs,F,D0,D1)
	    ; true,
	        D0 = D1,
		( F == F1 ->
		    ( Cs1 == [] -> %%% e.g. previous dynamic or multifile declarations.
			append(Ds,Ds1,NewDs),
			update(PN,info(CM,Cs,NewDs,F),DB0,DB1)
		    ; true,
	                warning('predicate ~q with multiple definitions in ~q',[PN,F]),
			append(Cs1,Cs,NewCs),
			append(Ds1,Ds,NewDs),
			update(PN,info(CM,NewCs,NewDs,F),DB0,DB1)
		    )
		; true, %%% F \== F1,
	            warning('predicate ~q redefined in ~q, previously defined in ~q',[PN,F1,F]),
		    update(PN,info(CM,Cs,Ds,F),DB0,DB1)
		)
	    )
	; has_directive(dynamic,Ds) ->
	    DB0 = DB1,
	    enter_asserted_clauses(Cs,F,D0,D1)
	; true,
	    D0 = D1,
	    update(PN,info(CM,Cs,Ds,F),DB0,DB1)
	).

%

enter_asserted_clauses(ix(_,_,_,_,_,Cs),F,D0,D1) :-
	Cs =.. [cls|Cls],
	enter_asserted_clauses_list(Cls,F,D0,D1).

enter_asserted_clauses_list([],_,D,D).
enter_asserted_clauses_list([C|Cs],F,D0,D2) :-
	insert_directive(assert(C),F,D0,D1),
	enter_asserted_clauses_list(Cs,F,D1,D2).


%%%
%%  Each functor P/N points at an information structure, consisting of:
%
%   o	The clauses of the predicate in textual order.
%
%   o	Directives and information on the predicate.
%
%   o	The file the predicate was defined in, or the first file it was
%	defined in if "multifile". 
%
%
information_item(Clauses,Directives,File,Info) :-
	Info = info(_CompileMode,Clauses,Directives,File).


% Size of env is E, # vars in clause is N-<start val> = N-1.

clauses_w_envs([],[]).

clauses_w_envs([C|Cs],[(C,E)|CEs]) :-
	enumeratevars(C,1,N),
	E is N-1,
	clauses_w_envs(Cs,CEs).

%

clause_functor(X,_P,_N) :- var(X), !, 
	sys_error('clause_functor/3: var arg').

clause_functor((H :- _B),P,N) :- !,
	functor(H,P,N).

clause_functor(((H :- _B),_Env),P,N) :- !,
	functor(H,P,N).

clause_functor(H,P,N) :-
	functor(H,P,N).

%

has_directive(D,Ds) :- member(D,Ds),!.

%

empty_database(X) :- empty_dict(X).


%%%
%%  Loading files also defines a number of new operators. These are removed
% after loading. (We assume here that ALL loaded files are in the scope of
% the op/3-declarations, which may or may not be a good thing.)
%
kill_defined_operators :-
	( retract('$undo_op'(op(_Prec,Assoc,Ops))) ->
	    op(0,Assoc,Ops),
	    kill_defined_operators
	; true ).
