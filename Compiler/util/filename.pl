%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  filename.pl
%
%
%   Convert a list of terms or a single term into a file name.
%

:- ensure_loaded(basics).


%%%
%%  translate_file_names(+FileNames, -FileNameList)
%
translate_file_names(    [],     []) :- !.

translate_file_names([X|Xs], [Y|Ys]) :- !,
        file_name(X, Name),
        absolute_file_name(Name, Y),
        translate_file_names(Xs, Ys).

translate_file_names(X, Y) :-
	file_name(X, Name),
	absolute_file_name(Name, Y).


%%%
%%  file_name(+FileName, -AbsoluteFileName)
%
file_name(FileName, AbsoluteFileName) :-
	file_name(FileName, FileNameList, []),
	atom_chars(AbsoluteFileName, FileNameList).


file_name(F/Fs) --> !,
	file_name(F), "/", file_name(Fs).

file_name(F-Fs) --> !,
	file_name(F), "-", file_name(Fs).

file_name(File) --> { atomic(File), !, atom_chars(File, Name) },
	dlist(Name).


%%%
%%  base_name(+FileName, ?BaseName)
%
%   If the file name, FileName, ends with a ".pl", the preceeding text is the
% base name, otherwise the entire name (string) is a base name.
%
base_name(FileName, BaseName) :- atomic(FileName), !,
	atom_chars(FileName, Name),
	base_name(Name, BaseName).

base_name(FileName, BaseName) :-
	( append(Name, ".pl", FileName) ->
	    BaseName = Name
	; true,
	    BaseName = FileName
	).


%%%
%%  relative_file_names(+RelativeFileName, +CurrentFileName, -AbsoluteFileName)
%
%   Given a list of (or single) relative file name and the current file name
% (as an absolute file name), construct a normalized file name in order to make
% 'consult' et al work properly without requiring full paths.
%
relative_file_names(Relative, Current, Absolute) :-
	atom_chars(Current, File),
	examine_file_name(File, Directory,_Base,_Ext),
	( atom(Relative) -> %%% Single file name.
	    relative_file_name(Relative, Directory, Absolute)
	; true,
	    relative_file_names_aux(Relative, Directory, Absolute)
	).

relative_file_names_aux(    [],_Directory,     []).
relative_file_names_aux([X|Xs], Directory, [Y|Ys]) :-
	relative_file_name(X, Directory, Y),
	relative_file_names_aux(Xs, Directory, Ys).


relative_file_name(X, Directory, Y) :-
	atom_chars(X, Xchars),
	append(Directory, Xchars, Ychars),
	atom_chars(Y, Ychars).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   
%  Note: These predicates have been copied from the emulator library function
%        absolute_file_name/N (file $LUTHERHOME/Library/absolute_file_name.pl).
%

%%%
%%  examine_file_name(+FileName,?Path,?BaseName,?Extension)
%
%   Examine a file name (represented as an ascii-list) in order to extract
% information of a possible prefix, the Path, the Base name (i.e. the file
% name without its extension) and a possible Extension.
%
examine_file_name(File, Path, Base, Ext) :-
	( match_file_name(File, 0'/, Head, Hdl, Tail) ->
	    concat_path(Path, 0'/, Head, Hdl, Next),
	    examine_file_name(Tail, Next, Base, Ext)
	; match_file_name(File, 0'., Head, Hdl, Tail) ->
	    Path = [],
	    Base = Head,
	    examine_base_name(Tail, Hdl, Ext)
	; true,
	    Path = [],
	    Base = File, Hdl = [], Ext = []
	).

examine_base_name(Base, Name, Ext) :-
	( match_file_name(Base, 0'., Head, Hdl, Tail) ->
	    concat_base(Name, 0'., Head),
	    examine_base_name(Tail, Hdl, Ext)
	; true,
	    Name = [],
	    Ext = [0'.|Base]
	).


match_file_name([C|Cs], Delim, Head, Hdl, Tail) :- C == Delim, !,
	Head = Hdl,
	Tail = Cs.

match_file_name([C|Cs], Delim, Head, Hdl, Tail) :- %%% C \== Delim
	Head = [C|Ds],
	match_file_name(Cs, Delim, Ds, Hdl, Tail).


concat_base([Delim|Part], Delim, Part).

concat_path(Path, Delim, Path, [Delim|Next], Next).

