%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  read.pl
%
%  Patric Hedlin.....Wed Oct 19 1994
%
%
% Description:
%
%   Read a term from the current input stream (or Stream) using a modified
% operator precedence parser. Read may support an optional dictionary if
% desired (supported by the tokenizer).
%   The dictionary is a list containing variable names and references (you may
% find a BNF description in tokenizer.pl).
%
%
% Interface:
%
%   read(?Term)
%   read(+Stream, ?Term)
%
%
/*
:- module prolog.
*/

:- public
	read/1,
	read/2.


%%%
%%  read(?Term) & read(+Stream, ?Term)
%
read(Term) :-
	Mode = verbose,
	read_internal(Mode, Term,_Dict).

read(Stream, Term) :-
	Mode = verbose,
	read_internal(Stream, Mode, Term,_Dict).


%%%
%%  read_internal(+ErrorMode, ?Term, -Dictionary) &
%		read_internal(+Stream, +ErrorMode, ?Term, -Dictionary)
%
read_internal(Mode, Term, Dict) :-
	current_input(Stream),
	with_default_input(Stream, prolog:read_internal_aux(Mode, Term, Dict)).

read_internal(Stream, Mode, Term, Dict) :-
	with_default_input(Stream, prolog:read_internal_aux(Mode, Term, Dict)).


read_internal_aux(Mode, Term, Dict) :-
	repeat,
	read_tokens(Dict, Tokens),
	( parse_expr(Tokens, 1200, Term, Rest), end_of_expr(Rest) ->
	  ( recorded('$$SYNTAX ERROR$$', error(_Message,_ArgList,_Index), Ref) ->
	      erase(Ref) ; true )
	; true,
	    recorded('$$SYNTAX ERROR$$', error( Message, ArgList, Index), Ref),
	    erase(Ref),
	    ( Mode == silent ->
		fail
	    ; Mode == verbose ->
	        syntax_error(Index, Tokens, Message, ArgList)
	    ; Mode == exception ->
	        raise_exception(syntax_error(Index, Tokens, Message, ArgList))
	    ; true,
	        error('reader received unknown error mode (~w)', [Mode])
	    )
	), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  parse_expr(+Tokens, +Precedence, -Term, -RestTokens)
%
%   Given a list of Tokens, parse an expression in the current context/precedence
% (Precedence) yielding a Term and a list of remaining tokens (RestTokens).
%
parse_expr(            [],_Prec,_Term,_RestTokens) :-
	record_syntax_error('unexpected end of expression', [], []).

parse_expr([Token|Tokens], Prec, Term, RestTokens) :-
	parse_token(Token, Tokens,  Prec, Term, RestTokens).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  parse_token(+Token, +Tokens, +Precedence, -Term, ?RestTokens).
%
%   When parsing an expression, this is where we encounter the first token of
% that expression. This token is used in order to build the primary part of
% the resulting expression. We thus handle any token admissable as the first
% token in an expression (including prefix operators, but excluding infix and
% postfix operators).
%
parse_token(var(Variable,_), Tokens, Prec, Term, RestTokens) :- !,
	parse_operator(Tokens, Prec, 0, Variable, Term, RestTokens).

parse_token(atom(Op), Tokens, Prec, Term, RestTokens) :- %can_start_expr(Tokens),
	current_prefixop(Op,_Type, OpPrec, RightPrec),
	( OpPrec =< Prec ->
	    ( Op == '-', next(number(Number), Tokens, Ts) ->		
		Negative is -Number,
		parse_operator(Ts, Prec, 0, Negative, Term, RestTokens)
	    ; Op == '+', next(number(Number), Tokens, Ts) ->
	        Positive is +Number,
		parse_operator(Ts, Prec, 0, Positive, Term, RestTokens)
	    ; true,
	        parse_expr(Tokens, RightPrec, NextTerm, Ts),
		Expr =.. [Op,NextTerm],
		parse_operator(Ts, Prec, OpPrec, Expr, Term, RestTokens)
	    )
	; true,
	    record_syntax_error('~w operator with precedence ~d in context ~d',
	                            [prefix,OpPrec,Prec], Tokens)
	).

parse_token(atom(Atom), Tokens, Prec, Term, RestTokens) :- !,
	parse_operator(Tokens, Prec, 0, Atom, Term, RestTokens).

parse_token(number(Number), Tokens, Prec, Term, RestTokens) :- !,
	parse_operator(Tokens, Prec, 0, Number, Term, RestTokens).

parse_token(string(String), Tokens, Prec, Term, RestTokens) :- !,
	parse_operator(Tokens, Prec, 0, String, Term, RestTokens).

parse_token(functor(Functor), Tokens, Prec, Term, RestTokens) :- !,
	next('(', Tokens, ArgTokens),
	parse_arguments(ArgTokens, ArgList, Ts),
	Struct =.. [Functor|ArgList],
	parse_operator(Ts, Prec, 0, Struct, Term, RestTokens).

parse_token('[', Tokens, Prec, Term, RestTokens) :- !,
	( next(']', Tokens, Ts) ->
	    parse_token(atom([]), Ts, Prec, Term, RestTokens)
	; true,
	    parse_list(Tokens, List, Ts1),
	    parse_operator(Ts1, Prec, 0, List, Term, RestTokens)
	).

parse_token('(', Tokens, Prec, Term, RestTokens) :- !,
	parse_expr(Tokens, 1200, Expr, Ts),
	expect(')', Ts, Ts1),
	parse_operator(Ts1, Prec, 0, Expr, Term, RestTokens).

parse_token('{', Tokens, Prec, Term, RestTokens) :- !,
	( next('}', Tokens, Ts) ->
	    parse_token(atom({}), Ts, Prec, Term, RestTokens)
	; true,
	    parse_expr(Tokens, 1200, Expr, Ts),
	    expect('}', Ts, Ts1),
	    parse_operator(Ts1, Prec, 0, { Expr }, Term, RestTokens)
	).

parse_token(end_of_file, Tokens,_Prec, Term, RestTokens) :- !,
	Term = end_of_file,
	RestTokens = Tokens.

parse_token(Token, Tokens,_Prec,_Term,_RestTokens) :-
	record_syntax_error('~w cannot start an expression', [Token], Tokens).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  parse_operator(+Tokens, +Precedence, +PrevPrec, +PrevTerm, -Term, ?RestTokens).
%
%   Having parsed a primary expression (PrevTerm) we now have to look for an
% operator (infix or postfix) which is admissable in the current context
% (Precedence) in order for the resulting expression to be built.
%   The ','-operator is handled separately since the comma is reported, not as
% an atom, but as a plain comma (',').
%
% NOTE: The tokenizer may have classified an infix operator as a functor if it
%       is immediately followed by a left parentheses ('('), we thus have to
%       handle the case when an infix operator is reported as a functor.
%
parse_operator(            [],_Prec,_PrevPrec,PrevTerm, Term, RestTokens) :-
	Term = PrevTerm,
	RestTokens = [].

parse_operator([Token|Tokens], Prec, PrevPrec,PrevTerm, Term, RestTokens) :-
	parse_operator(Token, Prec, PrevPrec,PrevTerm, Term, RestTokens, Tokens).


parse_operator(functor(Op), Prec, PrevPrec,PrevTerm, Term, RestTokens, Tokens) :- %can_start_expr(Tokens),
	current_infixop(Op,_Type, LeftPrec, OpPrec, RightPrec),
	( PrevPrec =< LeftPrec, OpPrec =< Prec ->
	    parse_expr(Tokens, RightPrec, NextTerm, Ts),
	    Expr =.. [Op,PrevTerm,NextTerm],
	    parse_operator(Ts, Prec, OpPrec, Expr, Term, RestTokens)
	; true,
	    record_syntax_error('~w operator with precedence ~d in context ~d',
	                            [infix,OpPrec,Prec], Tokens)
	).

parse_operator(atom(Op), Prec, PrevPrec,PrevTerm, Term, RestTokens, Tokens) :- %can_start_expr(Tokens),
	current_infixop(Op,_Type, LeftPrec, OpPrec, RightPrec),
	( PrevPrec =< LeftPrec, OpPrec =< Prec ->
	    parse_expr(Tokens, RightPrec, NextTerm, Ts),
	    Expr =.. [Op,PrevTerm,NextTerm],
	    parse_operator(Ts, Prec, OpPrec, Expr, Term, RestTokens)
	; true,
	    record_syntax_error('~w operator with precedence ~d in context ~d',
	                            [infix,OpPrec,Prec], Tokens)
	).

parse_operator(atom(Op), Prec, PrevPrec,PrevTerm, Term, RestTokens, Tokens) :- %can_follow_expr(Tokens),
	current_postfixop(Op,_Type, LeftPrec, OpPrec),
	( PrevPrec =< LeftPrec, OpPrec =< Prec ->
	    Expr =.. [Op,PrevTerm],
	    parse_operator(Tokens, Prec, OpPrec, Expr, Term, RestTokens)
	; true,
	    record_syntax_error('~w operator with precedence ~d in context ~d',
	                            [postfix,OpPrec,Prec], Tokens)
	).

parse_operator(',', Prec, PrevPrec,PrevTerm, Term, RestTokens, Tokens) :- PrevPrec < 1000, 1000 =< Prec, !,
	parse_expr(Tokens, 1000, NextTerm, Ts),
	parse_operator(Ts, Prec, 1000, (PrevTerm,NextTerm), Term, RestTokens).

parse_operator(Token,_Prec,_PrevPrec,PrevTerm, Term, RestTokens, Tokens) :-
	Term = PrevTerm,
	RestTokens = [Token|Tokens].
	
%%%
%%  parse_arguments(+Tokens, -ArgumentList, -RestTokens)
%
parse_arguments(Tokens, ArgList, RestTokens) :-
	parse_expr(Tokens, 999, Arg, [T|Ts]),
	ArgList = [Arg|List],
	parse_arguments(T, List, RestTokens, Ts).


parse_arguments(',', ArgList, RestTokens, Tokens) :- !,
	parse_expr(Tokens, 999, Arg, [T|Ts]),
	ArgList = [Arg|List],
	parse_arguments(T, List, RestTokens, Ts).

parse_arguments(')', ArgList, RestTokens, Tokens) :- !,
	ArgList = [],
	RestTokens = Tokens.

parse_arguments(_Token,_ArgList,_RestTokens, Tokens) :-
	record_syntax_error(''','' or '')'' expected in argument list', [], Tokens).


%%%
%%  parse_list(+Tokens, -List, -RestTokens)
%
parse_list(Tokens, List, RestTokens) :-
	parse_expr(Tokens, 999, Head, [T|Ts]),
	List = [Head|Tail],
	parse_list(T, Tail, RestTokens, Ts).


parse_list(',', List, RestTokens, Tokens) :- !,
	parse_expr(Tokens, 999, Head, [T|Ts]),
	List = [Head|Tail],
	parse_list(T, Tail, RestTokens, Ts).
	
parse_list('|', List, RestTokens, Tokens) :-
	parse_expr(Tokens, 999, List, Ts), expect(']', Ts, RestTokens).

parse_list(']', List, RestTokens, Tokens) :-
	List = [],
	RestTokens = Tokens.

parse_list(_Token,_List,_RestTokens, Tokens) :-
	record_syntax_error(''','' ''|'' or '']'' expected in list', [], Tokens).


%%%
%%  next(+Token, +Tokens, -RestTokens) & expect(+Token, +Tokens, -RestTokens)
%
next(Token, [Token|Tokens], Tokens).


expect(Token, Tokens, RestTokens) :-
	( next(Token, Tokens, RestTokens) ->
	    true
	; true,
	    record_syntax_error('~q expected', [Token], Tokens)
	).

%%%
%%  can_start_expr(+Token(s)) & can_follow_expr(+Token(s))
%
% NOTE: These are not used at present but the code still remains in case we want to
%       alter the current nonusage (when we have a primitive/inline expanding compiler).
%
can_start_expr([]) :- !, fail.
can_start_expr([Token|_Tokens]) :- !,
	can_start_expr(Token).

can_start_expr(')') :- !, fail.
can_start_expr(']') :- !, fail.
can_start_expr('}') :- !, fail.
can_start_expr('|') :- !, fail.
can_start_expr(',') :- !, fail.
can_start_expr(_OK).


can_follow_expr([]).
can_follow_expr([Token|_Tokens]) :-
	can_follow_expr(Token).

can_follow_expr(')').
can_follow_expr(']').
can_follow_expr('}').
can_follow_expr('|').
can_follow_expr(',').
can_follow_expr(atom(Op)) :-
	(
	    current_infixop(Op,_Type,_LeftPrec,_OpPrec,_RightPrec)
	;
	    current_postfixop(Op,_Type,_LeftPrec,_OpPrec)
	).


%%
%
end_of_expr([]).
end_of_expr([H|T]) :- record_syntax_error('operator expected after expression', [], [H|T]).


%%%
%%  record_syntax_error(+Message, +ArgList, +Tokens)
%
%   Record a syntax error message along with its arguments and the length of the
% list of remaining tokens (used by syntax_error/3 to pin point the source of the
% error reported). Only the last error encountered is recorded (and reported if
% the reader/parser fails).
%
record_syntax_error( Message, ArgList, Tokens) :-
	length(Tokens, N),
	( recorded('$$SYNTAX ERROR$$', error(_Msg,_Arg, M), Ref) ->
	    ( M > N -> erase(Ref) )
	; true ),
	recorda('$$SYNTAX ERROR$$', error(Message, ArgList, N),_Ref), fail.


%%%
%%  syntax_error(+Message, +Tokens, +Index)
%
syntax_error(Index, Tokens, Message, ArgList) :-
	length(Tokens, N),
	M is N - Index,
	split_list(M, Tokens, Prefix, Suffix),
	syntax_error_aux(Message, ArgList, Prefix, Suffix).

syntax_error_aux(Message, ArgList, Prefix, Suffix) :-
	usr_message('Syntax Error: ', Message, ArgList),
	print('*** in ***'), nl,
	print_tokens(Prefix), nl,
	print('*** at ***'), nl,
	print_tokens(Suffix), nl, nl, fail.
