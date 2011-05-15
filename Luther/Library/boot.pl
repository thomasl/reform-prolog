%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  boot.pl
%
%  Patric Hedlin.....Mon Jul 4 1994
%
%
% Description:
%
%    Boot-loading takes place with a minimal environment. All predicates
% loaded during boot-loading are exclusive members of the 'prolog' module
% if not otherwise stated, i.e. via a 'public' declaration.
%
%


start :-
	public(module/1),
	public(public/1),
	public(dynamic/1),
	public(multifile/1),

	public('$$LOAD_DIRECTIVE$$'/0),

	load_runtime_library,
	initialize_exceptions,
	initialize_operators,
	version,
	workers,
	default_resource_file(File),
	consult_resource_file(File),
	'USERCALL'(shell),
	halt.


load_runtime_library :-
	boot_load('$LUTHERHOME/Library/inout.wam'),
	boot_load('$LUTHERHOME/Library/modules.wam'),
	boot_load('$LUTHERHOME/Library/inline.wam'),
	boot_load('$LUTHERHOME/Library/builtin.wam'),
	boot_load('$LUTHERHOME/Library/tokenizer.wam'),
	boot_load('$LUTHERHOME/Library/expand_term.wam'),
	boot_load('$LUTHERHOME/Library/read.wam'),
	boot_load('$LUTHERHOME/Library/read_term.wam'),
	boot_load('$LUTHERHOME/Library/write.wam'),
	boot_load('$LUTHERHOME/Library/format.wam'),
	boot_load('$LUTHERHOME/Library/listing.wam'),
	boot_load('$LUTHERHOME/Library/current_op.wam'),
	boot_load('$LUTHERHOME/Library/error.wam'),
	boot_load('$LUTHERHOME/Library/load.wam'),
	boot_load('$LUTHERHOME/Library/absolute_file_name.wam'),
	boot_load('$LUTHERHOME/Library/if.wam'),
	boot_load('$LUTHERHOME/Library/not.wam'),
	boot_load('$LUTHERHOME/Library/setof.wam'),
	boot_load('$LUTHERHOME/Library/sorts.wam'),
	boot_load('$LUTHERHOME/Library/exception.wam'),
	boot_load('$LUTHERHOME/Library/freeze.wam'),
	boot_load('$LUTHERHOME/Library/unix.wam'),
	boot_load('$LUTHERHOME/Library/consult.wam'),
	boot_load('$LUTHERHOME/Library/interpret4.wam'),
	boot_load('$LUTHERHOME/Library/tree.wam'),
	boot_load('$LUTHERHOME/Library/shell.wam').


default_resource_file('~/.lutrc.pl').

consult_resource_file(File) :-
	( unix(access(File, r_ok)) -> consult(File) ; true ).


workers :-
	active_workers(Count),
	( Count > 0 ->
	    report('Started with ~d active workers', [Count])
	; true ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  boot_load(+File)
%
%   Load WAM code in ascii format using absolute filenames, with a ".wam"
% extension.
%

boot_load(File):- atom(File),
	'$get_module'(Module),
	'$loading_mode'(OldMode, boot),
	expand_file_name(File, AbsFile, AbsPath),
	'$unix_cd'(AbsPath, OldPath),
	( boot_load_aux(AbsFile) ->
	    '$unix_cd'(OldPath),
	    '$set_module'(Module),
	    '$loading_mode'(_, OldMode)
	; true,
	    '$unix_cd'(OldPath),
	    '$set_module'(Module),
	    '$loading_mode'(_, OldMode), fail
	).


boot_load_aux(File) :-
	open(File, 'read', Stream),
	load_stream(Stream),
	close(Stream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Handle load directives during (boot) loading.
%

load_stream(Stream) :-
	'$load'(State, Stream, Buffer),
	load_stream_aux(State, Stream, Buffer).


load_stream_aux(end_of_file,_Stream,_Buffer).

load_stream_aux(directive,   Stream, Buffer) :-
	( '$$LOAD_DIRECTIVE$$' ->
	    true
	; true,
	    '$display'('{Warning: load directive failed}'), nl
	),
	'$load'(State, Stream, Buffer),
	load_stream_aux(State, Stream, Buffer).


%%%
%%  This is a "minimal" set of directives supported during boot-loading.
%

module(Module) :- '$set_module'(Module).


public(Name/Arity) :-  '$public'(Name, Arity).

public((Predicate, Predicates)) :-
	public(Predicate),
	public(Predicates).


dynamic(Name/Arity) :- '$dynamic'(Name, Arity).

dynamic((Predicate, Predicates)) :- 
	dynamic(Predicate),
	dynamic(Predicates).


multifile(Predicate) :- dynamic(Predicate).
