%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  format.pl
%
%  Patric Hedlin.....Mon Jun 27 1994
%
%
% Description:
%
%  format(+ControlDirective,+ArgumentList)
%  format(+Stream,+ControlDirective,+ArgumentList)
%
%	Format write to user output or Stream according to ControlDirective
%	and ArgumentList.
%
%  The following control directives are supported:
%
%	~~	'~'
%	~*	numeric substitution ('~*a',[3,foo]) == ('~3a',[foo])
%	~Na	atom (repeated N times)
%	~Nc	character (repeated N times)
%	~Nd	decimal integer (N is neglected)
%	~Ne	floating point number (as with C-function printf())
%	~NE
%	~Nf	
%	~Ng	
%	~NG	
%	~Ni	ignore argument(s) 
%	~Nk	canonical form (repeated N times)
%	~Nn	new line (repeated N times)
%	~NN
%	~Np	format as with print/1
%	~Nq	format as with writeq/1
%	~NQ	format as with writeq/1 but always qoute atoms
%	~Nr	radix number with base N
%	~NR	radix number with base N using capitals
%	~Ns	string
%	~Nt	tabulator (repeated N times)
%	~Nw	format as with write/1
%
/*
:- module prolog.
*/

:- public
	format/2,
	format/3.



format(Control, ArgList) :- current_output(Stream), 
	format(Stream, Control, ArgList).

format(Stream, Control, ArgList) :-
	( format_check(Control, ControlList) ->
	    format_parse(ControlList, ArgList, OutputList, [])
	; true,
	    format_error('illegal control directive: ~s', [Control])
	),
	!,
	format_output(OutputList, Stream).

format(_Stream, Control,_ArgList) :- 
	format_error('unable to follow control directive: ~s', [Control]).


format_check(Control, ControlList) :- atom(Control), !,
	name(Control, ControlList).
format_check(Control, ControlList) :- format_check(Control),
	ControlList = Control.

format_check(Control) :- var(Control), !, fail.
format_check([]).
format_check([X|Xs]) :- integer(X),
	format_check(Xs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	
%   The format parser:
%

format_parse([],_ArgList) --> [].

format_parse([X|Xs], ArgList) --> format_parse(X, Xs, ArgList).


format_parse(0'~, CtrList, ArgList) --> !,
	{
	    CtrList = [Ctr|Next],
	    format_count(Ctr, Next, nil, Count, Directive, Rest)
	},
	format_tilde(Directive, Rest, Count, ArgList).

format_parse(INT, CtrList, ArgList) --> [INT], format_parse(CtrList, ArgList).


format_count(X, Xs, N, Count, Directive, Rest) :- is_digit(X, Digit), !,
	default_count(N, 0, Ni),
	Xs = [Y|Ys],
	Nj is 10*Ni + Digit,
	format_count(Y, Ys, Nj, Count, Directive, Rest).

format_count(X, Xs, N, Count, Directive, Rest) :-
	Count = N,
	Directive = X, Rest = Xs.


default_count(nil, Def, Def) :- !.
default_count(Val,_Def, Val) :- integer(Val).

is_digit(0'0, 0).
is_digit(0'1, 1).
is_digit(0'2, 2).
is_digit(0'3, 3).
is_digit(0'4, 4).
is_digit(0'5, 5).
is_digit(0'6, 6).
is_digit(0'7, 7).
is_digit(0'8, 8).
is_digit(0'9, 9).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%   This is the format table used to translate all supported directives
% into meta level calls that will perform the actuall printing.
%

format_tilde(0'~, Rest,_Count, ArgList) --> [0'~],
	format_parse(Rest, ArgList).

format_tilde(0'*, Rest,_Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	{
	    integer(Arg), Rest = [Next|Tail]
	},
	format_tilde(Next, Tail, Arg, List).

format_tilde(0'a, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(Count,write_atom(Arg))],
	format_parse(Rest, List).

format_tilde(0'c, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(Count,write_char(Arg))],
	format_parse(Rest, List).

format_tilde(0'd, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(1, write_integer(0'd,Arg,Count))],
	format_parse(Rest, List).

format_tilde(0'e, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(1, write_float(0'e,Arg,Count))],
	format_parse(Rest, List).

format_tilde(0'E, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(1, write_float(0'E,Arg,Count))],
	format_parse(Rest, List).

format_tilde(0'f, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(1, write_float(0'f,Arg,Count))],
	format_parse(Rest, List).

format_tilde(0'g, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(1, write_float(0'g,Arg,Count))],
	format_parse(Rest, List).

format_tilde(0'G, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(1, write_float(0'G,Arg,Count))],
	format_parse(Rest, List).

format_tilde(0'i, Rest, Count, ArgList) -->
	{
	    default_count(Count, 1, N),
	    ignore_arg(N, ArgList, List)
	},
	format_parse(Rest, List).

format_tilde(0'k, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(Count,write_canonical(Arg))],
	format_parse(Rest, List).

format_tilde(0'n, Rest, Count, ArgList) -->
	[format_aux(Count,nl)],
	format_parse(Rest, ArgList).

format_tilde(0'N, Rest, Count, ArgList) -->
	[format_aux(Count,nl)],
	format_parse(Rest, ArgList).

format_tilde(0'p, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(Count,print(Arg))],
	format_parse(Rest, List).

format_tilde(0'q, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(Count,writeq(Arg))],
	format_parse(Rest, List).

format_tilde(0'Q, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(Count,writeqq(Arg))],
	format_parse(Rest, List).

format_tilde(0'r, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(1, write_radix(0'r,Arg,Count))],
	format_parse(Rest, List).

format_tilde(0'R, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(1, write_radix(0'R,Arg,Count))],
	format_parse(Rest, List).

format_tilde(0's, Rest,_Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	string_check(Arg),
	format_parse(Rest, List).

format_tilde(0't, Rest, Count, ArgList) -->
	[format_aux(Count,write_tab)],
	format_parse(Rest, ArgList).

format_tilde(0'w, Rest, Count, ArgList) --> {arg_check(ArgList, Arg, List)},
	[format_aux(Count,write(Arg))],
	format_parse(Rest, List).


arg_check([],_Arg,_List) :-
	format_error('argument list exhausted', []).

arg_check([Arg|List], Arg, List).


string_check(X) --> {var(X)}, !,
	{format_error('argument is not ~w ~w: ~q', [a,'proper string',X])}.

string_check([]) --> !, [].

string_check([X|Xs]) -->
	string_check(X),
	string_check(Xs).

string_check(X) --> {atom(X)}, !,
	{name(X, Xs)},
	string_check(Xs).

string_check(X) --> {integer(X)},
	[X].


ignore_arg(0, ArgList, ArgList).

ignore_arg(N, ArgList, OutList) :- N > 0,
	arg_check(ArgList,_Arg, TmpList),
	M is N-1,
	ignore_arg(M, TmpList, OutList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%   Print formated output to a stream.
%

format_output(OutputList, Stream) :-
	with_default_output(Stream, 'SYSCALL'(format_out(OutputList))).


format_out([]).

format_out([X|Xs]) :-
	( integer(X) -> put(X) ; call(X) ), format_out(Xs).


format_aux(nil, Predicate) :- call(Predicate).

format_aux(1, Predicate) :- call(Predicate).

format_aux(N, Predicate) :- N > 1,
	call(Predicate),
	M is N - 1,
	format_aux(M, Predicate).


write_tab :- put(9).

write_atom(Arg) :-
	( atom(Arg) ->
	    write(Arg)
	; true,
	    format_error('argument is not ~w ~w: ~q', [an,atom,Arg])
	).


write_char(Arg) :-
	( integer(Arg) ->
	    put(Arg)
	; true,
	    format_error('argument is not ~w ~w: ~q', [a,character,Arg])
	).


write_float(Dir, Arg, Decimal) :-
	default_count(Decimal, 6, Dec),
	( float(Arg) ->
	    '$write_float'(Dir, Arg, Dec)
	; true,
	    format_error('argument is not ~w ~w: ~q', [a,float,Arg])
	).

write_radix(Dir, Arg, Radix) :-
	default_count(Radix, 16, Rad),
	( integer(Arg) ->
	    '$write_radix'(Dir, Arg, Rad)
	; true,
	    format_error('argument is not ~w ~w: ~q', [an,integer,Arg])
	).

write_integer(Dir, Arg, Decimal) :-
	default_count(Decimal, 0, Dec),
	( integer(Arg) ->
	    '$write_integer'(Dir, Arg, Dec)
	; true,
	    format_error('argument is not ~w ~w: ~q', [an,integer,Arg])
	).



%%%%% Temporary in order to interact with SICStus. %%%%%
/*
'$write_float'(Dir, Arg, Decimal) :-
	prolog:'$format_print_float'(Dir, Arg, Decimal).


'$write_radix'(0'r, Arg, Radix) :-
	prolog:'$format_print_integer'(0'r, Arg, Radix).

'$write_radix'(0'R, Arg, Radix) :-
	prolog:'$format_print_integer'(0'r, Arg, Radix).


'$write_integer'(Dir, Arg, Decimal) :-
	prolog:'$format_print_integer'(Dir, Arg, Decimal).


writeqq(Arg) :-
	( atom(Arg) ->
	  ( '$atom_mode'(Arg, 1) ->
	      writeq(Arg)
	  ; true,
	      put(0''),
	      writeq(Arg),
	      put(0'')
	  )
	; true,
	    writeq(Arg)
	).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%   Report internal failure.
%
format_error(Control, ArgList) :- 
	write('{Error: format failed - '),
	format(Control, ArgList),
	write('}'),
	nl.
