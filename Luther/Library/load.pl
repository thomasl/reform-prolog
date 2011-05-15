%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  load.pl
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
	load/1,
	qload/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Load WAM code in ascii format (using default file name extension ".wam").
%
%
load([]) :- !.

load([X|Xs]):- !,
	load(X),
	load(Xs).

load(File):- atom(File),
	'$get_module'(Module),
	'$loading_mode'(OldMode, load),
	absolute_file_name(File, '.wam', AbsPath, AbsFile),
	'$unix_cd'(AbsPath, OldPath),
	( need_loading(AbsFile) ->
	    ( load_aux(AbsFile) ->
		load_restore(OldPath, OldMode, Module)
	    ; true,
	        fail_loading(AbsFile),
		load_restore(OldPath, OldMode, Module), fail
	    )
	; void_loading(AbsFile) ->
	    ( load_nop(AbsFile) ->
		load_restore(OldPath, OldMode, Module)
	    ; true,
		load_restore(OldPath, OldMode, Module), fail
	    )
	).


load_restore(Path, Mode, Module) :-
	'$unix_cd'(Path),
	'$set_module'(Module),
	'$loading_mode'(_, Mode).


%%%
%%   Loading a file which is has not been loaded previously or, has been
%  modified since it was last loaded.
%
load_aux(File) :-
	open(File, read, Stream),
	report('loading ~w...', [File]),
	statistics(code, [C|_]),
	statistics(atom, [A|_]),
	statistics(runtime, _),
	load_stream(Stream), close(Stream),
	statistics(runtime, [_,R]),
	statistics(atom, [B|_]),
	statistics(code, [D|_]),
	S is D+B-C-A,
	report('~w loaded, ~d msec ~d bytes', [File, R, S]).


%%%
%%   Loading a file which has not been modified since it was last loaded,
%  reading any descendant files which may have been modified.
%
% Note: We should use a table of file dependencies to avoid reading entire
%       files just to find load directives.
%
% ***** NOT IMPLEMENTED *****
%
load_nop(_).

qload_nop(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Load WAM code in byte format (using default file name extension ".ql").
%
%
qload([]) :- !.

qload([X|Xs]):- !,
	qload(X),
	qload(Xs).

qload(File):- atom(File),
	'$get_module'(Module),
	'$loading_mode'(OldMode, qload),
	absolute_file_name(File, '.ql', AbsPath, AbsFile),
	'$unix_cd'(AbsPath, OldPath),
	( need_loading(AbsFile) ->
	    ( qload_aux(AbsFile) ->
		load_restore(OldPath, OldMode, Module)
	    ; true,
	        fail_loading(AbsFile),
		load_restore(OldPath, OldMode, Module), fail
	    )
	; void_loding(AbsFile) ->
	    ( qload_nop(AbsFile) ->
		load_restore(OldPath, OldMode, Module)
	    ; true,
		load_restore(OldPath, OldMode, Module), fail
	    )
	).


qload_aux(File) :-
	open(File, read, Stream),
	report('loading ~w...', [File]),
	statistics(code, [C|_]),
	statistics(atom, [A|_]),
	statistics(runtime, _),
	'$qload'(Stream), close(Stream),
	statistics(runtime,[_,R]),
	statistics(atom,[B|_]),
	statistics(code,[D|_]),
	S is D+B-C-A,
	report('~w loaded, ~d msec ~d bytes', [File, R, S]).



%%%
%%   Check if File needs to be read.
%
% Note: These predicates are also used during consulting (see consult.pl).
%
need_loading(File) :-
	( '$unix_stat_time'(File,_Atime, Modified,_Ctime) ->
	    ( recorded('$$FILE$$'(File), time(Loaded), Ref) ->
		( Modified > Loaded ->
		    erase(Ref),
		    recordz('$$FILE$$'(File), time(Modified), _),
		    recorded('$$FILE$$'(File), time(Modified), _)
		)
	    ; true, %%% This file has not been read before.
	        recordz('$$FILE$$'(File), time(Modified), _),
		recorded('$$FILE$$'(File), time(Modified), _)
	    )
	; true
	    %%% Asume no 'stat' available.
	).


fail_loading(File) :-
	( recorded('$$FILE$$'(File), time(_), Ref) -> erase(Ref) ; true ).


%%%
%%   This File does not need to be read it self, however, it might have
%  dependencies to files which do need consulting. This predicate only
% ensures that File is indeed loaded.
%
void_loading(File) :-
	recorded('$$FILE$$'(File), time(_Loaded),_Ref).
