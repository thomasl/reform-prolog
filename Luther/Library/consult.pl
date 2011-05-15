%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  consult.pl
%
%  Patric Hedlin.....Mon Jul 4 1994
%
%
% Description:   
%
%
/*
:- module prolog.
*/

:- public
	'.'/2,
	consult/1,
	reconsult/1.


[F|Fs] :- consult([F|Fs]).

%

reconsult(X) :- consult(X).

%

consult([]) :- !.

consult([X|Xs]) :- !,
	consult(X),
	consult(Xs).

consult(user) :- !,
	consult_stream(user).

consult(File) :- atom(File),
	'$get_module'(Module),
	absolute_file_name(File, AbsPath, AbsFile),
	'$unix_cd'(AbsPath, OldPath),
	( need_loading(AbsFile) ->
	    ( consult_aux(AbsFile) ->
		consult_restore(OldPath, Module)
	    ; true,
	        fail_loading(AbsFile),
		consult_restore(OldPath, Module), fail
	    )
	; void_loading(AbsFile) ->
	    ( consult_nop(AbsFile) ->
		consult_restore(OldPath, Module)
	    ; true,
		consult_restore(OldPath, Module), fail
	    )
	).


consult_restore(Path, Module) :-
	'$unix_cd'(Path),
	'$set_module'(Module).


%%%
%%   Consulting a file which is has not been consulted previously or,
%  has been modified since it was last consulted.
%
consult_aux(File) :-
	open(File, read, Stream),
	report('consulting ~w...', [File]),
	statistics(code, [C|_]),
	statistics(atom, [A|_]),
	statistics(runtime, _),
	consult_stream(Stream), close(Stream),
	statistics(runtime, [_,R]),
	statistics(atom, [B|_]),
	statistics(code, [D|_]),
	S is D+B-C-A,
	report('~w consulted, ~d msec ~d bytes', [File, R, S]).

%

consult_stream(Stream) :-
	read(Stream, Term),
	expand_term(Term, ExTerm),
	consult_stream_aux(ExTerm, Stream,_Seen).


consult_stream_aux(end_of_file,   _Stream,_Seen) :- !.

consult_stream_aux([],             Stream, Seen) :- !,
	read(Stream, Term),
	expand_term(Term, ExTerm),
	consult_stream_aux(ExTerm, Stream, Seen).

consult_stream_aux([Clause|Rest],  Stream, Seen) :- !,
	consult_stream_aux(Clause, Stream, Seen),
	consult_stream_aux(Rest, Stream, Seen).

consult_stream_aux((:- Directive), Stream, Seen) :- !,
	( valid_load_directive(Directive, File) ->
	    consult(File)
	; unsupported_directive(Directive) ->
	    warning('directive "~q" is not supported during consulting', [Directive])
	; call(Directive) ->
	    true
	; true,
	    warning('~q - directive failed', [Directive])
	),
	read(Stream, Term),
	expand_term(Term, ExTerm),
	consult_stream_aux(ExTerm, Stream, Seen).

consult_stream_aux(Clause, Stream, Seen) :-
	clause_signature(Clause, Signature),
	tree_lookup(Seen, Signature),
	!,
	assert(Clause),
	read(Stream, Term),
	expand_term(Term, ExTerm),
	consult_stream_aux(ExTerm, Stream, Seen).

consult_stream_aux(Clause, Stream, Seen) :-
	assert_delete_other(Clause),
	read(Stream, Term),
	expand_term(Term, ExTerm),
	clause_signature(Clause, Signature),
	tree_insert(Seen, Signature),
	consult_stream_aux(ExTerm, Stream, Seen).


%%%
%%   Consulting a file which has not been modified since it was last consulted,
%  reading any descendant files which may have been modified.
%
% Note: We should use a table of file dependencies to avoid reading entire
%       files just to find load directives.
%
% ***** NOT IMPLEMENTED *****
%
consult_nop(_).


%%%
%%
%
valid_load_directive(Directive, File) :-
	( Directive = ensure_loaded(File) ->
	    true
	; ( Directive = load(File) ;
	    Directive = consult(File) ;
	    Directive = reconsult(File) ;
	    Directive = compile(File) ;
	    Directive = fcompile(File) ) ->
	    warning('Found (shell) directive ~w, treated as (recomended) ensure_loaded(~w)',
	             [Directive, File])
	).


unsupported_directive(parallel(_)).


%%%
%%
%
clause_signature((Head :- _), F/A) :- !, functor(Head,F,A).

clause_signature(Head, F/A) :- functor(Head,F,A).
