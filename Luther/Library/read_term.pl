%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  read_term.pl
%
%  Patric Hedlin.....Wed Oct 19 1994
%
%
% Description:
%
%   Read a term from the current input stream (or Stream) and evaluate any
% suplied options according to the description below.
%
%
% Interface:
%
%   read_term(?Term, +Options)
%   read_term(+Stream, ?Term, +Options)
%
%   where Options may be a list of, or a single, control directive:
%
%	variables(-VariableNameList)
%
%	singletons(-VariableNameList)
%
%	syntax_error(+Mode)		Mode in {quite,verbose,exception}
%
%	cycles(+Boolean)		Boolean in {false,true}
%
%
/*
:- module prolog.
*/

:- public
	read_term/2,
	read_term/3.


%%%
%%  read_term(?Term, +Options) & read_term(+Stream, ?Term, +Options)
%
read_term(Term, Options) :-
	read_term_preoptions(Options, Mode),
	read_internal(Mode, Temp, Dict),
	read_term_postoptions(Options, Temp, Term, Dict).

read_term(Stream, Term, Options) :-
	raed_term_preoptions(Options, Mode),
	read_internal(Stream, Mode, Temp, Dict),
	read_term_postoptions(Options, Temp, Term, Dict).


%%%
%%
%
read_term_preoptions(Options,_Mode) :- var(Options), !, fail.

read_term_preoptions(    [], Mode) :-
	( var(Mode) -> Mode = verbose ; true ).

read_term_preoptions([X|Xs], Mode) :-
	read_term_preoption(X, Mode),
	read_term_preoptions(Xs, Mode).


read_term_preoption( Option,_Mode) :- var(Option), !, fail.

read_term_preoption(syntax_error(Mode), Mode) :- !.

read_term_preoption(_Option,_Mode).


%%%
%%
%
read_term_postoptions(    [], Term, Term,_Dict).

read_term_postoptions([X|Xs], TermIn, TermOut, Dict) :-
	read_term_postoption(X, TermIn, TermTmp, Dict),
	read_term_postoptions(Xs, TermTmp, TermOut, Dict).


read_term_postoption(variables(VarList), Term, Term, Dict) :- !,
	read_term_variables(Dict, VarList, []).

read_term_postoption(singletons(VarList), Term, Term, Dict) :- !,
	read_term_singletons(Dict, VarList, []).

read_term_postoption(syntax_error(Mode), Term, Term,_Dict) :- !.

read_term_postoption(cycles(Boolean), Temp, Term,_Dict) :- !,
	( Boolean == true ->
	    read_term_cycles(Temp, Term)
	; Boolean == false ->
	    Term = Temp
	).

read_term_postoption(UnknownOption, Term, Term,_Dict) :-
	warning('unknown option (~q) supplied to read_term/2/3', [UnknownOption]).


read_term_variables(Dict) --> { var(Dict) }, !.

read_term_variables([[String,Reference|_Singleton]|More]) --> [Name = Reference],
	{ atom_chars(Name, String) },
	read_term_variables(More).


read_term_singletons(Dict) --> { var(Dict) }, !.

read_term_singletons([[String,Reference|Singleton]|More]) -->
        ( { Singleton == [] } ->
	    []
        ; { explicit_singleton(String) } ->
	    []
	; { true },
	    [Name = Reference],
	    { atom_chars(Name, String) }
	),
	read_term_singletons(More).


explicit_singleton([0'_|_]).


%%%
%%
%
read_term_cycles(X, Y) :- var(X), !,
	Y = X.

read_term_cycles(X, Y) :- atomic(X), !,
	Y = X.

read_term_cycles(@(X,List), X) :-
	read_term_bind(List).

read_term_cycles(X, Y) :-
	functor(X, F, N),
	functor(Y, F, N),
	read_term_cycles(N, X, Y).


read_term_cycles(0,_X,_Y) :- !.

read_term_cycles(N, X, Y) :- M is N - 1,
	arg(N, X, ArgX),
	arg(N, Y, ArgY),
	read_term_cycles(ArgX, ArgY),
	read_term_cycles(M, X, Y).


read_term_bind([]).

read_term_bind([X = X|Rest]) :-
	read_term_bind(Rest).

