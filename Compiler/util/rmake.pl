%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%		SAVING THE COMPILER TO A 'BINARY IMAGE'.
%
%
%   To generate an executable (under SISCtus) in the Reform/Compiler directory,
% follow the guidelines below:
%
%   > prolog
%   <prolog text>
%   ?- ['util/save'].
%   <consult message>
%   yes.
%   ?- rmake.
%
%   Alternatively, rmake/1 or rmake/2 may be used to produce the compiler to a
% specified file (with a possible compile mode option).
%
%   ?- rmake('TargetFile').
%   ?- rmake('TargetFile', {fastcode|compactcode|profiledcode|debugcode}).
%
%

default_compiler_file('reform-compiler').


rmake :- default_compiler_file(File),
	rmake(File).

rmake(File) :-
	rmake(File, fastcode).


rmake(File, Option) :- ( prolog_flag(compiling, _, Option) -> true ; true ),
	compile(reform),
	date(Date),
	reform_compiler_version(Version),
	reform_compiler(File, Version, Date).


reform_compiler(File, Version, Date) :-
	save(File),
	format(user_output, 'Reform compiler, version ~q of ~a~n', [Version,Date]),
	format(user_output, 'Report bugs to reform-bugs@csd.uu.se~n', []).

date(Date) :-
	unix(popen(date,read,Stream)),
	read_string(Stream, String),
	name(Date, String).

read_string(Stream, Str) :-
	get0(Stream, Char),
	( ( Char =:= -1 ; Char =:= 10 ) -> % stop at end-of-stream or newline
	    Str = []
	; true,
	    Str = [Char|Next],
	    read_string(Stream, Next)
	).
