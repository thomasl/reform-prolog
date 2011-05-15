%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%                      MAIN COMPILER DEFINITION
%
%
% This file defines the compiler shell.
%
% The compiler does the following:
%
%  o	Reads a number of files, as specified, into a database.
%  o	Globally analyses these files (unless prohibited by an option).
%  o	Annotates the database of predicates.
%  o	Performs Reform compilation and parallelization on the predicates.
%  o	Emits the predicates into one or more .wam-files.
%
%
% The compiler (shell) interface:
%
%  o	rcompile/0	Dummy (used for analysis purposes).
%  o	rcompile/1	Default compiler entry (using default options).
%  o	rcompile/2	General compiler entry (accepting compile time options).
%
%  o	analyse/2	General analyser entry (accepting compile time options).
%  o	compile_analysed/1	Compiler version reading previous analysis
%			results from the default analysis output file.
%  o	show_analysis_results/1	Show analysis results (as save via analyse/2 or
%			the 'ouput_analysis' option during compilation.
%  o	annotate_and_compile/4	Compiler version accepting external clause tables.
%
%
%   The compiler accepts a number of options specified by an option list,
% if no options are given, a set of default options will be used.
%
% The following compile time options exist:
%
%  o   analyse		Perform global analysis (default).
%  o   verbose		Present successive compiler information.
%  o   output(File)	Write the compiled result to File.
%  o   output_indexing	Write the indexing information of predicates.
%  o   output_analysis	Write the analysis result to a default output file.
%  o   output_separate	Write the compiled result of each file, "file.pl", to
%			a separate file, using the base name of the file with
%			the extension ".wam" (producing output to "file.wam").
%  o   named_files_only	Suppress any load directives (such as ensure_loaded/1,
%			load/1, consult/1 etc.) during compilation.
%  o   analysis_statistics	Present statistics from the analysis phase
%			when the analyse is ready.
%  o   numa		Assume nonuniform memory architecture (numa) when
%                       generating code (default).
%  o   byte_format	Generate byte format output.
%  o   ascii_format	Generate ascii format output (default).
%
% Note:	If no output option is present, the compiler uses the default output
%	file "a.wam" in the current directory.
%
%   An option is enabled by including it in the option list, and disabled by
% excluding it (alternatively an option may be excluded through 'negation',
% thus e.g.  -analyse   means no analysis should be performed).
%
% Note: Default options must be suppressed using negation.
%
%

:- multifile portray/1.


:- ensure_loaded('util/basics').			% Utilities
:- ensure_loaded('util/filename').			% Output file names
:- ensure_loaded('preprocess/inline').			% Inlining
:- ensure_loaded('preprocess/index').			% Indexing + compilation
:- ensure_loaded('analysis/astat').			% Analysis statistics
:- ensure_loaded('analysis/analyser').			% Analyse program
:- ensure_loaded('analysis/userannot').			% User view of analysis
:- ensure_loaded('analysis/listlattice').		% Abstract domain
:- ensure_loaded('analysis/recursion-parallel').	% Analysis of rp-preds
:- ensure_loaded('compilation/shared-clause').		% Compile clauses
:- ensure_loaded('compilation/rp-compiler').		% Compile rp-preds
:- ensure_loaded('postprocess/locality').		% Generate global_*
:- ensure_loaded('postprocess/luther-emit').		% Emit compiled code


reform_compiler_version(1.8).


%% Some default options. Change to suit.
%
default_options([numa, analyse, byte_format]).
default_analysis_output_file('analysis.out').
default_compiled_output_file('a.wam').


%% Supply dummy predicate with arity zero for analysis purposes.
%
rcompile :- rcompile(reform).

%

rcompile(Files) :-
        rcompile(Files, []).

rcompile(Files, Options) :-
        statistics(runtime, [T0,_]),
	reform_options(Options, OptionList),
	reform_compiler_version(Version),
	report('reform compiler: Version ~q', [Version]),
        translate_file_names(Files, FileNames),
        load_files(OptionList, FileNames, CDB, Callables, Exports, Directives),
        analyse_clause_database(OptionList, CDB, NewCDB, Callables, Exports, SeqMemo, ParMemo),
	( has_opt(output_analysis, OptionList) ->
	    save_analysis_results(OptionList, NewCDB, Exports, SeqMemo, ParMemo, Directives)
	; true ),
	( has_opt(analysis_statistics, OptionList) ->
	    show_analysis_statistics(SeqMemo, ParMemo)
	; true ),
        annotate_clause_database(OptionList, NewCDB, SeqMemo, ParMemo, AnnotatedList),
        compile_clause_database(OptionList, AnnotatedList, Directives),
        statistics(runtime, [T1,_]),
        Time is T1-T0,
	report('reform compiler: Ready (~q msec)', [Time]).


%%%
%%  A version of the compiler that only analyses the program and then
% dumps the analysis results on a file.
%
analyse(Files, Options) :-
	reform_options(Options, OptionList),
        translate_file_names(Files, FileNames),
        load_files(OptionList, FileNames, CDB, Callables, Exports, Directives),
        analyse_clause_database(OptionList, CDB, NewCDB, Callables, Exports, SeqMemo, ParMemo),
        save_analysis_results(OptionList, NewCDB, Exports, SeqMemo, ParMemo, Directives),
	show_analysis_statistics(SeqMemo, ParMemo).


%%%
%%  Continue an 'interrupted' compilation by loading (restoring) a saved analysis
% state/result (clauses + memo tables + exports) and compiling the restored image.
%
compile_analysed(Options) :-
	reform_options(Options, OptionList),
        load_analysis_results(OptionList, CDB,_Exports, SeqMemo, ParMemo, Directives),
        annotate_clause_database(OptionList, CDB, SeqMemo, ParMemo, AnnotatedList),
        compile_clause_database(OptionList, AnnotatedList, Directives).


%%%
%%  save_analysis_results & load_analysis_results
%
%
save_analysis_results(Options, CDB, Exports, SeqMemo, ParMemo, Directives) :-
	reform_options(Options, OptionList),	
        notify_if_opt(verbose, OptionList,'saving analysis results...'),
        statistics(runtime, [T0,_]),
        default_analysis_output_file(Analysis),
        translate_file_names(Analysis, OutFile),
        open(OutFile, write, OutStream),
        save_dicts([CDB, SeqMemo, ParMemo, Directives], OutStream),
        format(OutStream,'~q.~n', [Exports]),
        close(OutStream),
        statistics(runtime, [T1,_]),
        Time is T1-T0,
        notify_if_opt(verbose, OptionList,'analysis results saved to file ~q (~q msec)', [OutFile, Time]).


load_analysis_results(Options, CDB, Exports, SeqMemo, ParMemo, Directives) :-
	reform_options(Options, OptionList),	
        notify_if_opt(verbose, OptionList,'loading analysis results...'),
        statistics(runtime, [T0,_]),
        default_analysis_output_file(Analysis),
        translate_file_names(Analysis, InFile),
        open(InFile, read, InStream),
        restore_dicts([CDB, SeqMemo, ParMemo, Directives], InStream),
        read(InStream, Exports),
        close(InStream),
        statistics(runtime, [T1,_]),
        Time is T1-T0,
        notify_if_opt(verbose, OptionList,'analysis results loaded from file ~q (~d msec)', [InFile, Time]).


%%%
%%  Support to show previously saved analysis results.
%
show_analysis_results(Options) :-
	reform_options(Options, OptionList),
        load_analysis_results(OptionList,_CDB,_Exports, SeqMemo, ParMemo,_Directives),
	( has_opt(analysis_statistics, OptionList) ->
	    show_analysis_statistics(SeqMemo, ParMemo) ; true ),
	format_analysis_tables(SeqMemo, ParMemo).


show_analysis_statistics(SeqMemo, ParMemo) :-
	astat(SeqMemo, ParMemo, SeqStat, ParStat),
	format('~nstatistics of analysis (~w):~n~p~n', [sequential,SeqStat]),
	format('~nstatistics of analysis (~w):~n~p~n', [parallel,ParStat]).


%%%
%%  A version of the compiler where clause tables are provided from the outside.
% This is useful to test better analysis results on the rest of the compiler.
%
annotate_and_compile(Files, SeqMemo, ParMemo, Options) :-
        translate_file_names(Files, FileNames),
        load_files(Options, FileNames, CDB,_Callables,_Exports, Directives),
        annotate_clause_database(Options, CDB, SeqMemo, ParMemo, AnnotatedList),
        compile_clause_database(Options, AnnotatedList, Directives).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  First phase: Preloading
%
%   Load the files and extract the entry points of the program (which are either
% given as ':- export(PredList)' or are the predicates of arity 0), and remove
% syntactic sugar. Then analyse the program and emit two tables of information,
% one for sequential execution, one for parallel execution.
%
% *** UNFINISHED ***
%   The current implementation does not use the information from undefined
% predicates. This would include: storing away the U-dictionary and using
% the U-dictionary to provide 'Exports' when compiling other files.
%
load_files(Options, Files, CDBx, Callables, Exports, Directives) :-
        statistics(runtime, [T0,_]),
	( has_opt(named_files_only, Options) ->
	  ( has_opt(analyse, Options) ->
	      error('analyse option does not work with option named_files_only')
	  ; true,
	    ( fload_desugar_program_one(Files, CDB, Callables, Exports, Directives) ->
	        true
	    ; notify_if_opt(verbose, Options,'preloading file(s): Failed'),
	        fail
	    )
	  )
	; true,
	  ( fload_desugar_program(Files, CDB, Callables, Exports, Directives) ->
	      true
	  ; notify_if_opt(verbose, Options,'preloading failed'),
	      fail
	  )
	),
	( has_opt(inline, Options) ->
	    inline_clause_database(CDB, CDBx)
	; true,
	    CDBx = CDB
	),
        statistics(runtime, [T1,_]),
        LoadTime is T1 - T0,
        notify_if_opt(verbose, Options,'preloading file(s): Ready (~d msec)', [LoadTime]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Second phase: Analysing the program. 
%
%   If the user specified no analysis, then no analysis is done and a dummy
% table of call patterns is constructed. Parallel predicates are converted to
% a somewhat dumb sequential code. The advantage is when you are compiling
% sequential code that doesn't call parallel predicates: the compiler doesn't
% use analysis to compile sequential predicates.
%
analyse_clause_database(Options, CDB, NewCDB, Callables, Exports, SeqMemo, ParMemo) :-
        notify_if_opt(verbose, Options,'computing indexing...'),
        statistics(runtime, [T0,_]),
        ground_clauses_and_ix(CDB, CurrCDB),
        statistics(runtime, [T1,_]),
        IxTime is T1-T0,
        notify_if_opt(verbose, Options,'computing indexing: Ready (~d msec)', [IxTime]),
	( has_opt(output_indexing, Options) ->
	    display_clause_table(CurrCDB)
	; true
	),
        notify_if_opt(verbose, Options,'analysing program...'),
        statistics(runtime, [T2,_]),
        empty_memotable(StartSeqMemo),
	empty_memotable(StartParMemo),
	empty_memotable(U0),
        ( hasnt_opt(analyse, Options) -> %%% assumes no parallel predicates
	    notify_if_opt(verbose, Options,'no analysis requested'),
	    unanalysed_memo(CurrCDB, NewCDB, StartSeqMemo, SeqMemo),
	    ParMemo = StartParMemo
	; true,
	    construct_cg_of_ixstructs(CurrCDB, Callables, CG),
	    ( analyse_program(Exports, StartSeqMemo, SeqMemo, StartParMemo, ParMemo,
	                    CurrCDB, CG, U0,_U1) ->
	      NewCDB = CurrCDB
	  ; notify_if_opt(verbose, Options,'analysing program: Failed'),
	      fail
	  )
        ),
        statistics(runtime, [T3,_]),
        AnalysisTime is T3-T2,
        notify_if_opt(verbose, Options,'analysing program: Ready (~d msec)', [AnalysisTime]).

%

unanalysed_memo(CDB, NewCDB, M0, M1) :-
        list_dict(CDB, List),
        unanalysed_entries(List, NewList, M0, M1),
        empty_dict(NewCDB0),
        insert_list(NewList, NewCDB0, NewCDB).

unanalysed_entries([], [], M, M).
unanalysed_entries([(PN, info(Mode, Ix, Directives, FileName))|Xs], [Y|Ys], M0, M2) :-
        ( has_directive(parallel, Directives) ->
	    sequentialize_predicate(Ix, NewIx),
	    delete_from_list(Directives, parallel, NewDs), % defined in basics.pl
	    Y = (PN, info(Mode, NewIx, NewDs, FileName))
        ; true,
	    Y = (PN, info(Mode, Ix, Directives, FileName))
        ),
        insert(PN,(unanalysed, unanalysed), M0, M1),
        unanalysed_entries(Xs, Ys, M1, M2).

%%%
%%
%
sequentialize_predicate(rp(Base,_ParRec, Rec, Type,_LB,_RB), IxStruct) :-
	( Type == intrec ->
	    IxStruct = ix([], [], [], [1, 2], [], cls(Base, Rec))
	; Type == listrec ->
	    IxStruct = ix([1, 2], [1], [2], [], [], cls(Base, Rec))
	; true,
	    sys_error('sequentialize_predicate/2: unknown recursion type ~q', [Type])
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Third phase: Annotating the program.
%
%   Predicates are annotated with types and locality. The result is a list of
% < predicate, info, file > where predicates are indexing structures (we use
% IxStructs for short, see definition in index.pl) with ground clauses where
% variables are '$VAR'(Index,....) annotated with type info.
%
annotate_clause_database(Options, CDB, SeqMemo, ParMemo, AnnotatedList) :-
        notify_if_opt(verbose, Options,'annotating predicates...'),
        statistics(runtime, [T0,_]),
        list_dict(CDB, ListCDB),
        ( annotate_predicates(ListCDB, Options, SeqMemo, ParMemo, AnnotatedList) ->
            true
        ; notify_if_opt(verbose, Options,'annotating predicates: Failed'),
	    fail
        ),
        statistics(runtime, [T1,_]),
        AnnotTime is T1 - T0,
        notify_if_opt(verbose, Options,'annotating predicates: Ready (~d msec)', [AnnotTime]).


annotate_predicates(    [],_Options,_S,_P,       []).
annotate_predicates([X|Xs], Options, S, P, [AX|AXs]) :-	X = (Functor, _),
	notify_if_opt(verbose, Options, 'annotating predicate : ~q', [Functor]),
        annotate_entry(X, S, P, AX),
        annotate_predicates(Xs, Options, S, P, AXs).


% NOTE: annotate_entry/4 is defined in annotate.pl


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Fourth phase: Compiling the program
%
%   Compile each predicate using the types and locality. The compiled code is
% emitted either on a single, ".wam" file (using "a.wam" as a standard name) or
% to one ".wam" file for each ".pl" file compiled.
%
compile_clause_database(Options, AnnotatedList, Directives) :-
        notify_if_opt(verbose, Options,'compiling predicates...'),
        statistics(runtime, [T0,_]),
        ( compile_predicates(AnnotatedList, Options, Directives) ->
            true
        ; notify_if_opt(verbose, Options,'compiling predicates: Failed'),
            fail
        ),
        statistics(runtime, [T1,_]),
        CompileTime is T1 - T0,
        notify_if_opt(verbose, Options,'compiling predicates: Ready (~d msec)', [CompileTime]).

%%%
%%  First, check whether all predicates are to be written to the same file
% or not. If they are, we use the provided name, otherwise we use one output
% stream per file.
%
compile_predicates(Predicates, Options, Directives) :-
        ( has_opt(output_separate, Options) ->
	  empty_dict(Streams),
	  ( comp_predicates(Predicates, Options, Directives, Streams, OutStreams) ->
	      close_all_streams(OutStreams)
	  ; true,
	      close_all_streams(OutStreams),
	      fail
          )
        ; true,
	    ( has_opt(output(FileName), Options) ->
		true
	    ; true,
	        default_compiled_output_file(FileName)
	    ),
	    open(FileName, write, OutStream),
	    compile_all_directives(Directives, Options, OutStream),
	    ( comp_predicates(Predicates, Options, OutStream) ->
	        close(OutStream)
	    ; true,
	        close(OutStream),
		fail
          )
        ).


%% Compile all predicates and write them on the same stream.
%
comp_predicates(    [],_Options,_OutStream).
comp_predicates([X|Xs], Options, OutStream) :-
        ( comp_predicate(X, Options, OutStream) ->
	    true
	; true,
	    X = (Functor, _),
	    warning('Failed to compile predicate ~q', [Functor])
	),
	comp_predicates(Xs, Options, OutStream).


%% Compile all predicates and write them to separate streams.
%
comp_predicates(    [],_Options,_Directives,  S,  S).
comp_predicates([X|Xs], Options, Directives, S0, S2) :-
        ( comp_predicate(X, Options, Directives, S0, S1) ->
	    true
	; true,
	    X = (Functor, _),
	    S0 = S1,
	    warning('Failed to compile predicate ~q', [Functor])
	),
        comp_predicates(Xs, Options, Directives, S1, S2).


%%%
%%  Compile one predicate, checking what stream it is to be written to.
% If the file of the predicate has no stream, allocate one.
%
comp_predicate((P/N, info(Ix, Directives, FileName, Seq, Par)), Options, DsTbl, S0, S1) :-
        ( lookup(FileName, S0, OutStream) ->
            S0 = S1,
	    comp_predicate((P/N, info(Ix, Directives, FileName, Seq, Par)), Options, OutStream)
        ; open_output_file(FileName, OutStream),
	  ( lookup(FileName, DsTbl, FileDirectives) ->
	      compile_directives(FileDirectives, Options, OutStream) ; true ),
          insert(FileName, OutStream, S0, S1),
          comp_predicate((P/N, info(Ix, Directives, FileName, Seq, Par)), Options, OutStream)
        ).


%%%
%%  Actual compilation code.
%
%   Compile the ix-struct (i.e. annotated predicate code) and write the result
% on the given stream. Invisible predicates (i.e. left and right bodies in
% parallel ditto) are not compiled since thay have already been unfolded into
% the parallel body.
%
% Note: The 'nonverbose' directive may be used to suppress verbose printing,
%       this could be the case for source code transformations (such as clauses
%       spawned during term expansion).
%
comp_predicate((P/N, info(Ix, Directives, FileName, SeqCp, ParCp)), Options, OutStream) :-
	( Directives == invisible ->
	    true
	; true,
	    ( has_directive(nonverbose, Directives) ->
		true
	    ; true,
	        notify_if_opt(verbose, Options, 'compiling predicate : ~q', [P/N])
	    ),
	    find_indexing_info(SeqCp, ParCp, Par, Seq, Loc),
	    compile_ix_struct(Ix, P, N, Par, Seq, Loc, Code, []),
	    peephole_opt2(Code, OptCode),
	    PredCode = [predicate(P, N)|OptCode],
	    enumerate_labels(OptCode, 1),
	    emit_compiled_predicate(Options, P, N, PredCode, Directives, FileName, OutStream)
	).


%%%
%%  open_output_file(+File, -Stream)
%
%   Given a file name "file.pl" (possibly written as e.g. file-name), open a
% file "file.wam" for writing (creating an associated stream).
%
open_output_file(File, OutStream) :-
        file_name(File, FileName),
        base_name(FileName, FileBase),
        compiled_file_extension(Extension),
        append(FileBase, Extension, OutFileName),
        name(OutFile, OutFileName),
        open(OutFile, write, OutStream).

% Extension of compiler output files.

compiled_file_extension(".wam").


%%%
%%
%
find_indexing_info(shr(_,_, Seq), ParCp, P, S, L) :- !,
	( ( Seq == det ; Seq == nondet ) ->
	    P = any, S = any, L = local
	; arg(1, Seq, S0),
	  symbolic_type_of(S0, S),
	  ( ParCp == unanalysed ->
	      P = any,
	      L = local
	  ; ParCp = shr(_,_, Par) ->
	      arg(1, Par, P0),
	      symbolic_type_of(P0, P),
	      symbolic_locality_of_type(P0, L)
	  )
	).

find_indexing_info(unanalysed, ParCp, P, S, L) :- !,
	( ParCp == unanalysed ->
	    P = any, S = any, L = local
	; true,
	    sys_error('find_indexing_info/5: unanalysed seq cp but par cp ~q', [ParCp])
	).

find_indexing_info(det,_, P, S, L) :- !,
	P = any, S = any, L = local.

find_indexing_info(nondet,_, P, S, L) :- !,
	P = any, S = any, L = local.

find_indexing_info(SeqTuple, ParTuple, P, S, L) :-
	functor(SeqTuple,_, SeqAr), SeqAr > 0,
	functor(ParTuple,_, ParAr), ParAr > 0,
	arg(1, SeqTuple, SeqT),
	arg(1, ParTuple, ParT),
	symbolic_type_of(SeqT, S),
	symbolic_type_of(ParT, P),
	symbolic_locality_of_type(ParT, L).

%

enumerate_labels(    [],_M).
enumerate_labels([X|Xs], M) :-
	enumerate_label(X, M, N),
        enumerate_labels(Xs, N).

enumerate_label('Label'(Lbl), M, N) :- !,
	( var(Lbl) -> Lbl = M, N is M+1 ; N = M ).
enumerate_label(_Instruction, N, N).

%

close_all_streams(Streams) :-
        list_dict(Streams, List),
        close_all_streams_in_list(List).


close_all_streams_in_list(        []).
close_all_streams_in_list([(_,X)|Xs]) :-
        close(X),
        close_all_streams_in_list(Xs).

%

emit_compiled_predicate(Options, P, N, Code, Directives, FileName, OutStream) :-
	( has_opt(ascii_format, Options) ->
	    format(OutStream, '% Source:     ~q~n', [FileName]),
            format(OutStream, '% Compiled:   ~q/~d~n', [P,N]),
	    format(OutStream, '% Directives: ~p~n', [Directives])
	; true ),
        ( has_opt(numa, Options) ->
	    locality_pho(Code, NewCode)
	; true,
	    NewCode = Code
	),
        emit_compiled_code(Options, NewCode, OutStream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Compiling load directives.
%
%   A directive is a single goal, and must be annotated ("the default way"),
% compiled and emitted. This is done by generating a 'dummy predicate'
% $$LOAD_DIRECTIVE$$/0, which is recognized by the machine.
%
compile_all_directives(Directives, Options, Stream) :-
	list_dict(Directives, KeyDirectiveList),
	directives_of(KeyDirectiveList, DirectiveList, []),
	compile_directives(DirectiveList, Options, Stream).


directives_of([]) --> [].
directives_of([(_K, V)|Xs]) -->
	dlist(V),
	directives_of(Xs).


compile_directives(    [],_Options,_Stream).
compile_directives([X|Xs], Options, Stream) :-
	( compile_directive(X, Options, Stream) ->
	    compile_directives(Xs, Options, Stream)
	; true,
	    error('directive ~q failed to compile', [X])
	).


compile_directive(X, Options, Stream) :-
	( is_void_directive(X) ->
	    true
	; is_load_directive(X), hasnt_opt(named_files_only, Options) ->
	    true
	; true,
	    Ix = ix([1],[1],[1],[1],[1], cls(AnnotCls)),
	    Cls = ('$$LOAD_DIRECTIVE$$' :- X),
	    enumeratevars(X, 0, N),
	    M is N+1,
	    minimal_annot_clause(Cls, M, AnnotCls),
	    compile_ix_struct(Ix,'$$LOAD_DIRECTIVE$$', 0, det, det, local, Code, []),
	    peephole_opt2(Code, OptCode),
	    DirCode = [directive('$$LOAD_DIRECTIVE$$', 0)|OptCode],
	    enumerate_labels(OptCode, 1),
	    emit_compiled_code(Options, DirCode, Stream)
	).

is_void_directive(parallel(_)).

is_load_directive(load(_)).
is_load_directive(consult(_)).
is_load_directive(reconsult(_)).
is_load_directive(ensure_loaded(_)).
is_load_directive(compile(_)).
is_load_directive(fcompile(_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  reform_options(+Options, -OptionList)
%
%
reform_options(Options, OptionList) :-
        default_options(Default),
	translate_options(Options, Default, OptionList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  translate_options(+Options, +Default, -OptionList)
%
%   Translate possible short hand options into full name ditos.
%
% Note: We rely on the Deafult list to contain full name options.
%
translate_options(Options, Default, OptionList) :-
	translate_options(Options, Expands),
	translate_default(Default, Expands, OptionList).


translate_options(    [],     []) :- !.

translate_options([X|Xs], [Y|Ys]) :- translate_option(X, Y), !,
	translate_options(Xs, Ys).

translate_options([X|Xs], [X|Ys]) :- translate_option(_, X), !,
	translate_options(Xs, Ys).

translate_options([X|Xs],     Ys) :- !,
	warning('unknown option (~w) ignored', [X]),
	translate_options(Xs, Ys).

translate_options(X, Y) :- nonvar(X),
	translate_options([X], Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  translate_default(+Default, +Options, -OptionList)
%
%   Translate default options with respect to user supplied options in order
% to detect/remove conflicting defaults.
%
translate_default(    [], Options, Options).

translate_default([D|Ds], Options, OptionList) :-
	( member(-D, Options) ->
	    translate_default(Ds, Options, OptionList)
	; conflicting_option(D, Options) ->
	    translate_default(Ds, Options, OptionList)
	; true,
	    OptionList = [D|OptionTail],
	    translate_default(Ds, Options, OptionTail)
	).


conflicting_option(Default, Options) :-
	conflicting_options(Default, Conflict),
	( member(Conflict, Options) -> true ; fail ).

%%%
%%  translate_option(?ShortHandOption, ?FullNameOption)
%
translate_option(i, inline).

translate_option(a, analyse).
translate_option(v, verbose).

translate_option(oi, output_indexing).
translate_option(oa, output_analysis).
translate_option(os, output_separate).

translate_option(o(File), output(File)).

translate_option(nfo, named_files_only).

translate_option(as, analysis_statistics).

translate_option(numa, numa).

translate_option(bf, byte_format).
translate_option(af, ascii_format).

translate_option(-(X), -(Y)) :-
	translate_option(X, Y).


conflicting_options(ascii_format, byte_format).
conflicting_options(byte_format, ascii_format).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Compile time option support.
%
% Options are represented as:
%
%  Option   if included
% -Option   if excluded
%
notify_if_opt(Opt, Options, Msg) :-
        ( has_opt(Opt, Options) -> report(Msg) ; true ).

notify_if_opt(Opt, Options, Msg, ArgList) :-
        ( has_opt(Opt, Options) -> report(Msg, ArgList) ; true ).


has_opt(Opt, [X|Xs]) :-
	( X =  Opt ->
	    true
	; X = -Opt ->
	    fail
	; true,
	    has_opt(Opt, Xs)
	).


hasnt_opt(_Opt,     []).

hasnt_opt( Opt, [X|Xs]) :-
	( X =  Opt ->
	    fail
	; X = -Opt ->
	    true
	; true,
	    hasnt_opt(Opt, Xs)
	).
