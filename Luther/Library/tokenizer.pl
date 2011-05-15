%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  tokenizer.pl
%
%  Patric Hedlin.....Wed Jul 13 1994
%
%
% Description:
%
%   The tokenizer produces the following tokens:
%
%	  atom		atom(Name)
%	  number	number(Number)
%	  string	string(AsciiList)
%	  functor	functor(Name)
%	  variable	var(Ref, Name)
%
%	  punctuation:	,()[]{|}
%
%	  end of file:	end_of_file
%
%
% Dictionary:
%
%   Dictionary ::= [Slot|Dictionary] | Variable
%   Slot       ::= [Name|Reference]
%   Name       ::= AsciiList
%   Reference  ::= [Variable] | [Variable|Single]
%   Single     ::= Variable
%
%	where Single indicates that the variable is a singleton
%
%
% Character classification:
%
%	00 = layout:	\nl\cr\del\tab\eof etc.
%	10 = lowercase:	a-z
%	20 = uppercase:	A-Z
%	30 = digit:	0-9
%	40 = symbol:	+-*/\^<>=`~:.?@#$&
%	50 = solo:	;!
%	60 = punctuation: %,()[]{|}
%	70 = quote:	'"
%	80 = underline:	_
%
%
/*
:- module prolog.
*/

:- public read_tokens/2.


%%%
%%  read_tokens(-Dictionary, -TokenList)
%
%   Return a list of tokens and a Dictionary with read variables.
%
read_tokens(Dict, Tokens) :-
	'$getch'(NextTyp, NextCh),
	read_tokens(NextTyp, NextCh, Dict, Tokens).


%%%
%%  read_tokens(+Type, +Char, +Dict, -Tokens)
%
%   Handle read character according to classification.
%
read_tokens(X, _Ch,_Dict,_Tokens) :- var(X), !, fail.	%%% space saver

read_tokens(00, Ch, Dict, Tokens) :-		%%% ignore layout
	( Ch > -1 ->
	    read_tokens(Dict, Tokens)
	; true,
	    Tokens = [end_of_file]		%%% END OF FILE
        ).

read_tokens(10, Ch, Dict, Tokens) :-
	'$getch0'(NextTyp, NextCh),
	read_name(NextTyp, NextCh, Chars, PostCh, PostTyp),
	atom_name([Ch|Chars], Name),
	read_after_atom(PostTyp, PostCh, Name, Dict, Tokens).

read_tokens(20, Ch, Dict, [var(Var,Name)|Tokens]) :-
	'$getch0'(NextTyp, NextCh),
	read_name(NextTyp, NextCh, Chars, PostCh, PostTyp),
	Name = [Ch|Chars],
	var_lookup(Dict, Name, Var),		%%% lookup/enter variable in dictionary
	read_tokens(PostTyp, PostCh, Dict, Tokens).

read_tokens(30, Ch, Dict, [number(Value)|Tokens]) :-
	read_number(Ch, Value, Dict, Tokens).

read_tokens(40, 0'/, Dict, Tokens) :- !,	%%% `/*'-comment
	'$getch0'(NextTyp, NextCh),
	read_solidus(NextTyp, NextCh, Dict, Tokens).
read_tokens(40, 0'., Dict, Tokens) :- !,	%%% a full stop or a symbol (i.e .=. etc.)
	'$getch0'(NextTyp, NextCh),
	read_fullstop(NextTyp, NextCh, Dict, Tokens).
read_tokens(40, Ch, Dict, Tokens) :-
	'$getch0'(NextTyp, NextCh),
	read_symbol(NextTyp, NextCh, Chars, PostCh, PostTyp),
	atom_name([Ch|Chars], Name),
	read_after_atom(PostTyp, PostCh, Name, Dict, Tokens).

read_tokens(50, Ch, Dict, [atom(Name)|Tokens]) :-	%%% a solo character
	solo_name(Ch, Name),
	read_tokens(Dict, Tokens).

read_tokens(60, 0'%, Dict, Tokens) :- !,	%%% `%'-comment
        skip(10),
	read_tokens(Dict, Tokens).
read_tokens(60, Ch, Dict, [Name|Tokens]) :-	%%% a punctuation
	punct_name(Ch, Name),
	read_tokens(Dict, Tokens).

read_tokens(70, 0'", Dict, [string(Chars)|Tokens]) :- !,	%%% a "string"
	'$getch0'(NextTyp, NextCh),
	read_string(NextTyp, NextCh, Chars, 0'", PostTyp, PostCh),
	read_tokens(PostTyp, PostCh, Dict, Tokens).
read_tokens(70, 0'', Dict, Tokens) :-			%%% an 'atom'
	'$getch0'(NextTyp, NextCh),
	read_string(NextTyp, NextCh, Chars, 0'', PostTyp, PostCh),
	atom_name(Chars, Name),
	read_after_atom(PostTyp, PostCh, Name, Dict, Tokens).

%%%
%%  According to ISO-Prolog, a variable, "_name", is not an anonymous variable,
% only plain "_" is (I do not fancy this convention but we do want our reader
% to be compatible with other systems ditto, do we not).
%
read_tokens(80, 0'_, Dict, [var(Var,Name)|Tokens]) :-	%%% an anonymous variable
	'$getch0'(NextTyp, NextCh),
	read_name(NextTyp, NextCh, Chars, PostCh, PostTyp),
	Name = [0'_|Chars],
	( Chars = "" ->
	    true
	; true,
	    var_lookup(Dict, Name, Var)			%%% lookup/enter variable in dictionary
	),
	read_tokens(PostTyp, PostCh, Dict, Tokens).

%%%
%%  Decide wether this was a plain atom or the name of a functor.
%
read_after_atom(60, 0'(, Name, Dict, [functor(Name),'('|Tokens]) :- !,
	'$getch'(NextTyp, NextCh),
	read_tokens(NextTyp, NextCh, Dict, Tokens).
read_after_atom(Typ, Ch, Name, Dict, [atom(Name)|Tokens]) :-
	read_tokens(Typ, Ch, Dict, Tokens).


%%%
%%  read_string(+Type, +Char, -Chars, -Quote, -PostCh, -PostTyp)
%
%   Read the body of a string delimited by Quote characters. The result is
% a list of ASCII codes. We have to handle two complications: the first
% complication is encountered if we reach the end of the file inside the
% string this predicate succeeds, returning the end of file (-1) character
% as PostCh; the second complication is encountered if we find a Quote,
% we then have to look ahead one character in case it is a double Quote.
%
read_string(00, -1, [], _, 00, -1) :- !. %%% end of file

read_string(70, Quote, Chars, Quote, PostTyp, PostCh) :- !,	%%% closing or double quote
	'$getch0'(NextTyp, NextCh),			
	more_string(NextTyp, NextCh, Quote, Chars, PostTyp, PostCh).

read_string(_, Ch, [Ch|Chars], Quote, PostTyp, PostCh) :- %%% ordinary character
	'$getch0'(NextTyp, NextCh),
	read_string(NextTyp, NextCh, Chars, Quote, PostTyp, PostCh).


more_string(70, Quote, Quote, [Quote|Chars], PostTyp, PostCh) :- !,	%%% double quote
	'$getch0'(Typ, Ch),
	read_string(Typ, Ch, Chars, Quote, PostTyp, PostCh).

more_string(PostTyp, PostCh, _, [], PostTyp, PostCh).	%%% end of string


%%%
%%  read_solidus(+Type, +Char, +Dict, -Tokens)
%
%   Checks to see whether /Ch is a /* comment or a symbol. If the former,
% it skips the comment. If the latter it just calls read_symbol.
%
read_solidus(40, 0'*, Dict, Tokens) :- !,
	'$getch0'(NextTyp, NextCh),
	read_comment(NextTyp, NextCh, 1, Dict, Tokens).

read_solidus(Typ, Ch, Dict, Tokens) :-
	read_symbol(Typ, Ch, Chars, PostCh, PostTyp),
	atom_name([0'/|Chars], Name),
	read_after_atom(PostTyp, PostCh, Name, Dict, Tokens).


%%%
%%  read_comment(+Type, +Char, +Level, +Dict, -Tokens)
%
%   Read past comment (possibly nested). Level is used to track the depth of
% a nested comment, where level one (1) is treated as the top level (as invoked
% by read_solidus/4).
%
% Note: This simple treatment of nested comments does not handle pathological
%       cases like '/**/**/**/', nor does it allow comments to unbalanced.
%
read_comment(40, 0'/, Level, Dict, Tokens) :- !,
	'$getch0'(NextTyp, NextCh),
	read_nested_next(NextTyp, NextCh, Level, Dict, Tokens).
read_comment(40, 0'*, Level, Dict, Tokens) :- !,
	'$getch0'(NextTyp, NextCh),
	read_nested_prev(NextTyp, NextCh, Level, Dict, Tokens).

read_comment(_Typ,_Ch, Level, Dict, Tokens) :-
	'$getch0'(NextTyp, NextCh),
	read_comment(NextTyp, NextCh, Level, Dict, Tokens).


read_nested_next(40, 0'*, Level, Dict, Tokens) :- !,
	Next is Level + 1,
	'$getch0'(NextTyp, NextCh),
	read_comment(NetxTyp, NextCh, Next, Dict, Tokens).

read_nested_next(Typ, Ch, Level, Dict, Tokens) :-
	read_comment(Typ, Ch, Level, Dict, Tokens).


read_nested_prev(40, 0'/, Level, Dict, Tokens) :- !,
	'$getch0'(NextTyp, NextCh),
	( Level == 1 ->
	    read_tokens(NextTyp, NextCh, Dict, Tokens)
	; true, %%% Level > 1
	    Prev is Level - 1,
	    read_comment(NextTyp, NextCh, Prev, Dict, Tokens)
	).

read_nested_prev(Typ, Ch, Level, Dict, Tokens) :-
	read_comment(Typ, Ch, Level, Dict, Tokens).




%%%
%%  read_name(+Type, +Char, -String, -PostCh, -PostTyp)
%
%   Read a sequence of letters, digits, and underscores, and return the
% result as String. The first character which cannot join this sequence
% is returned as PostCh.
%
read_name(10, Ch, [Ch|Chars], PostCh, PostTyp) :- !,
	'$getch0'(NextTyp, NextCh),
	read_name(NextTyp, NextCh, Chars, PostCh, PostTyp).

read_name(20, Ch,[Ch|Chars], PostCh, PostTyp) :- !,
	'$getch0'(NextTyp, NextCh),
	read_name(NextTyp, NextCh, Chars, PostCh, PostTyp).

read_name(30, Ch, [Ch|Chars], PostCh, PostTyp) :- !,
	'$getch0'(NextTyp, NextCh),
	read_name(NextTyp, NextCh, Chars, PostCh, PostTyp).

read_name(80, Ch, [Ch|Chars], PostCh, PostTyp) :- !,
	'$getch0'(NextTyp, NextCh),
	read_name(NextTyp, NextCh, Chars, PostCh, PostTyp).

read_name(PostTyp, PostCh, [], PostCh, PostTyp).


%%%
%%  read_symbol(+Type, +Char, -String, -PostCh, -PostTyp)
%
%   Read "symbolic" atom (which needs no quoting), i.e. an atom which is a
% string of "symbol" characters.
%
read_symbol(40, Ch, [Ch|Chars], PostCh, PostTyp) :- !,
	'$getch0'(NextTyp, NextCh),
	read_symbol(NextTyp, NextCh, Chars, PostCh, PostTyp).

read_symbol(PostTyp, PostCh, [], PostCh, PostTyp).


%%%
%%  read_fullstop(+Type, +Char, +Dict, -Tokens)
%
% Look at the next character after a full stop. There are two cases:
%
% o the next character is a layout character. This is a clause terminator.
%
% o the next character is anything else. This is just an ordinary symbol and we
%   call read_symbol to process it.
%
read_fullstop(00,_Ch,_Dict, Tokens) :- !, Tokens = [].	%%% END OF CLAUSE

read_fullstop(Typ, Ch, Dict, Tokens) :-
	read_symbol(Typ, Ch, S, PostCh, PostTyp),
	atom_name([0'.|S], Name),
	read_after_atom(PostTyp, PostCh, Name, Dict, Tokens).


%%%
%%  read_number(+Char, -Value, +Dict, -Tokens)
%
% read_number is complicated by having to understand radix notation.
%
% We recognize the following number formats:
%
%   <number> ::= <digits>		- integer value, read in base 10
%                <nobase> ' <character>	- integer value of ASCII code for that character
%                <digits> ' <digits>	- integer value, read in prefixed base (in [2..36])
%                <digits> . <fraction>	- floating point value
%
%   <fraction> ::= <digits> | <digits> <exponent>
%
%   <exponent> ::= 'E' <signed> | 'e' <signed>
%
%   <signed> ::= '+' <number> | '-' <number> | <number>
%
%   <digits> ::= <digit> | <digit> <digits>
%
%   <nobase> ::= '0'
%
read_number(Ch, Value, Dict, Tokens) :-
	Chars = Hdl,
	read_int(30, Ch, Chars, Hdl, Pdl, PostCh, PostTyp),
	read_after_int(PostTyp, PostCh, Chars, Pdl, Value, Dict, Tokens).


read_int(30, Digit, Chars, [Digit|Hdl], Pdl, PostCh, PostTyp) :- !,
	'$getch0'(NextTyp, NextCh),
	read_int(NextTyp, NextCh, Chars, Hdl, Pdl, PostCh, PostTyp).

read_int(Typ, Ch,_Chars, NIL, NIL, Ch, Typ).


%%% This may stil be a radix or floating point number (or a plain integer).
%%
read_after_int(40, 0'., Chars, Hdl, Value, Dict, Tokens) :- !,
	'$getch0'(NextTyp, NextCh),
	read_after_period(NextTyp, NextCh, Chars, Hdl, Value, Dict, Tokens).

read_after_int(70, 0'', Chars,  [], Value, Dict, Tokens) :- !,
	number_chars(Base, Chars),
	read_radix(Base, Value, PostCh, PostTyp),
	read_tokens(PostTyp, PostCh, Dict, Tokens).

read_after_int(Typ, Ch, Chars,  [], Value, Dict, Tokens) :-
	number_chars(Value, Chars),
	read_tokens(Typ, Ch, Dict, Tokens).


%%% This is either a floating point number or simply an integer followed by a period.
%%
read_after_period(30, Ch, Chars, [0'.|Hdl], Value, Dict, Tokens) :- !,
	read_int(30, Ch, Chars, Hdl, Pdl, PostCh, PostTyp),
	read_after_fraction(PostTyp, PostCh, Chars, Pdl, Value, NextCh, NextTyp),
	read_tokens(NextTyp, NextCh, Dict, Tokens).

read_after_period(Typ, Ch, Chars, [], Value, Dict, Tokens) :-
	number_chars(Value, Chars),
	read_fullstop(Typ, Ch, Dict, Tokens).


%%% This may be an exponential floating point number (e.g. 0.314156592E+1) or not.
%%
read_after_fraction(10, 0'e, Chars, [0'e|Hdl], Value, PostCh, PostTyp) :- !,
	'$getch0'(NextTyp, NextCh),
	read_exponent(NextTyp, NextCh, Chars, Hdl, Value, PostCh, PostTyp).

read_after_fraction(20, 0'E, Chars, [0'E|Hdl], Value, PostCh, PostTyp) :- !,
	'$getch0'(NextTyp, NextCh),
	read_exponent(NextTyp, NextCh, Chars, Hdl, Value, PostCh, PostTyp).

read_after_fraction(Typ, Ch, Chars, [], Value, Ch, Typ) :-
	number_chars(Value, Chars).


%%% The exponent may be explicitly signed or not.
%%
read_exponent(40, 0'-, Chars, [0'-|Hdl], Value, PostCh, PostTyp) :- !,
	'$getch0'(NextTyp, NextCh), 
	read_exponent_aux(NextTyp, NextCh, Chars, Hdl, Value, PostCh, PostTyp).

read_exponent(40, 0'+, Chars, [0'+|Hdl], Value, PostCh, PostTyp) :- !,
	'$getch0'(NextTyp, NextCh),
	read_exponent_aux(NextTyp, NextCh, Chars, Hdl, Value, PostCh, PostTyp).

read_exponent(Typ, Ch, Chars, Hdl, Value, PostCh, PostTyp) :-
	read_exponent_aux(Typ, Ch, Chars, Hdl, Value, PostCh, PostTyp).


read_exponent_aux(Typ, Ch, Chars, Hdl, Value, PostCh, PostTyp) :-
	read_int(Typ, Ch, Chars, Hdl, [], PostCh, PostTyp),
	number_chars(Value, Chars).


%%% This is either an explicit character or a radix number.
%%
read_radix(0, Value, PostCh, PostTyp) :- !,
	'$getch0'(_Typ_, Value),
	'$getch'(PostTyp, PostCh).

read_radix(Base, Value, PostCh, PostTyp) :- 2 =< Base, Base =< 36, !,
	'$getch0'(NextTyp, NextCh),
	read_name(NextTyp, NextCh, Chars, PostCh, PostTyp),
	( number_chars(Value, Chars, Base) ->
	    true
	; true,
	    Value = Chars
	).

read_radix(Value, Value, 0'', 70).


%%%
%%  lexeme to name conversion.
%
atom_name(Chars, Name) :- atom_chars(Name, Chars), !.
atom_name(Chars,_Name) :-
	sys_error('unable to create atom of lexeme (%s)', [Chars]).


solo_name(0';, ';').
solo_name(0'!, '!').


punct_name(0',, ',').
punct_name(0'(, '(').
punct_name(0'), ')').
punct_name(0'[, '[').
punct_name(0'], ']').
punct_name(0'{, '{').
punct_name(0'|, '|').
punct_name(0'}, '}').


%%%
%%  var_lookup(Dict, Key, Var)
%
var_lookup(Dict, Key, Var) :-
	dic_lookup(Dict, Key, Obj),
	( var(Obj) ->
	    Obj = [Var|_]	%%% single reference
	; true,
	    Obj = [Var]		%%% multiple references
	).


dic_lookup([ Node|_Rest], Key, Obj) :- Node = [Key|Obj], !.
dic_lookup([_Node| Rest], Key, Obj) :-
	dic_lookup(Rest, Key, Obj).


%%%
%%  print_tokens(Tokens) & print_token(Token)
%
print_tokens([X|Xs]) :-
	print_token(X),
	print_tokens(Xs).

print_tokens([]).


print_token(var(_,V)) :- format(user_output, '~s', [V]).
print_token(atom(A)) :- format(user_output, ' ~q ', [A]).
print_token(string(S)) :- format(user_output, '"~s"', [S]).
print_token(number(N)) :- format(user_output, '~w', [N]).
print_token(functor(F)) :- format(user_output, '~q', [F]).
print_token(',') :- write(user_output, ',').
print_token('(') :- write(user_output, '(').
print_token(')') :- write(user_output, ')').
print_token('[') :- write(user_output, '[').
print_token(']') :- write(user_output, ']').
print_token('{') :- write(user_output, '{').
print_token('|') :- write(user_output, '|').
print_token('}') :- write(user_output, '}').
