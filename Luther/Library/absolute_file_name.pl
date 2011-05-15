%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  absolute_file_name.pl
%
%  Patric Hedlin.....Thu Jun 30 1994
%
%
% Description:
%
%  absolute_file_name(+RealtiveFileName,?AbsoluteFileName)
%  absolute_file_name(+RealtiveFileName,?AbsolutePath,?AbsoluteFileName)
%
%	Search for a relative name with or without additional suffix '.pl' and
%	unify result with (AbsolutePath and) AbsoluteFileName if the file exists.
%
%  absolute_file_name(+RelativeFileName,+Extension,?AbsolutePath,?AbsoluteFileName)
%
%	Search for a relative name with or without additional suffix Extension and
%	unify result with (AbsolutePath and) AbsoluteFileName if the file exists.
%	If no suffix (Extension = '') is supplied, the relative filename will be
%	searched as is. If supplied, the suffix (Extension) replaces any suffix
%	present in the relative filename, or if none is found, extends the base
%	name in the relative file.
%
%	Example:
%
%	  absolute_file_name('$HOME/foo.pl', '',    Path, File)
%	  absolute_file_name('$HOME/foo',    '.pl', Path, File)
%	  absolute_file_name('$HOME/foo.xx', '.pl', Path, File)
%
%		will all search for file foo.pl in the home directory.
%
%
%  absolute_file_name(+Path,+BaseName,+Extension,?AbsolutePath,?AbsoluteFileName)
%
%	Search for file Path/BaseNameExtension and unify result with AbsolutePath
%	and AbsoluteFileName if the file exists. If the file can not be found,
%	AbsoluteFileName is expanded without the supplied suffix (Extension).
%
%
/*
:- module prolog.
*/

:- public
	absolute_file_name/2,
	absolute_file_name/3,
	absolute_file_name/4,
	absolute_file_name/5.



absolute_file_name(File, AbsFile) :-
	absolute_file_name(File,_AbsPath, AbsFile).

absolute_file_name(File, AbsPath, AbsFile) :-
	absolute_file_name(File, '.pl', AbsPath, AbsFile).

absolute_file_name(File, Ext, AbsPath, AbsFile) :-
	atom(File),
	extract_file_name(File, Path, Base, HasExt),
	( nil(Ext) ->
	    absolute_file_name(Path, Base, HasExt, AbsPath, AbsFile)
	; true,
	    absolute_file_name(Path, Base, Ext, AbsPath, AbsFile)
	).

absolute_file_name(Path, Base, Ext, AbsPath, AbsFile) :-
	atom(Path),
	atom(Base),
	atom( Ext),
	( concat_file_name(Path, Base, Ext, File),
	  expand_file_name(File, AbsFile, AbsPath),
	  unix(stat_type(AbsFile, s_ifreg)) ->
	    true
	; true,
	    concat_file_name(Path, Base,  '', File),
	    expand_file_name(File, AbsFile, AbsPath)
	).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Private support predicates.
%
%

concat_file_name(Path, Base, Ext, File) :-
	atom_chars(Ext,  Es),
	atom_chars(Base, Bs),
	append(Bs, Es, Xs),
	( nil(Path) ->
	    Ys = Xs
	; true,
	    atom_chars(Path, Ps),
	    append(Ps, Xs, Ys)
	),
	atom_chars(File, Ys).

%%%
%%  extract_file_name(+FileName,?Path,?BaseName,?Extension)
%
extract_file_name(File, Path, Base, Ext) :- atom(File),
	atom_chars(File, F),
	examine_file_name(F, P, B, E),
	atom_chars(Path, P),
	atom_chars(Base, B),
	atom_chars(Ext,  E).

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


nil('').
