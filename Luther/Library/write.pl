%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  write.pl
%
%  Patric Hedlin.....Thu Jun 30 1994
%
%
%  Adapted from shared code written by Richard A. O'Keefe (Oct 1984)
%
%
% NOTE:
%
%     This file was written to assist portability and to help people
%   get a decent set of output routines off the ground fast.  It is
%   not particularly efficient. Information about atom names and
%   properties should be precomputed and fetched as directly as
%   possible, and strings should not be created as lists!
%
%   The output routines differ in the following respects:
%
%    [a] write_canonical doesn't use operator information or handle
%        {X} or [H|T] specially. The others do.
%    [b] print calls portray/1 to give the user a chance to do
%        something different. The others don't.
%    [c] writeq puts quotes around atoms that can't be read back.
%        The others don't.
%    [d] writeqq puts quotes around all atoms.
%
%     Since they have such a lot in common, we just pass around
%   arguments (SynStyle, LexStyle) saying what to do.
%
%   In a Prolog which supports strings;
%	write(<string>) should just write the text of the string, this so
%	that write("Beware bandersnatch") can be used. The other output
%	commands should quote the string.
%
/*
:- module(prolog).
*/

:- public
	write_canonical/1,
	write_canonical/2,
	display/1,
	print/1,
	print/2,
	write/1,
	write/2,
	writeq/1,
	writeq/2,
	writeqq/1,
	writeqq/2.



write_canonical(Term) :- writeq_quick(Term), !.
write_canonical(Term) :- write_out(Term, noop, quote, 1200, 0, 0, '(', 2'100, _).

write_canonical(Stream, Term) :-
	with_default_output(Stream, write_canonical(Term)).


display(Term) :-
	with_default_output(user_output, 'SYSCALL'(display_aux(Term))).

display_aux(Term) :- display_quick(Term), !.
display_aux(Term) :- write_out(Term, noop, noquote, 1200, 0, 0, '(', 2'100, _).


print(Term) :- write_out(Term, print(999), noquote, 1200, 0, 0, '(', 2'100, _).

print(Stream, Term) :-
	with_default_output(Stream, print(Term)).


write(Term) :- write_quick(Term), !.
write(Term) :- write_out(Term, op, noquote, 1200, 0, 0, '(', 2'100, _).

write(Stream, Term) :-
	with_default_output(Stream, write(Term)).


writeq(Term) :- writeq_quick(Term), !.
writeq(Term) :- write_out(Term, op, quote, 1200, 0, 0, '(', 2'100, _).

writeq(Stream, Term) :- 
	with_default_output(Stream, writeq(Term)).


writeqq(Term) :-
	( atom(Term) ->
	  ( '$atom_mode'(Term, 1) ->
	      writeq(Term)
	  ; true,
	      put(0''),
	      writeq(Term),
	      put(0'')
	  )
	; true,
	    writeq(Term)
	).

writeqq(Stream, Term) :- 
	with_default_output(Stream, writeqq(Term)).


writeq_quick(Term) :- var(Term),
	'$write'(Term).
writeq_quick(Term) :- atomic(Term),
	'$write'(Term).
writeq_quick(Term) :- generic(Term),
	'$write'(Term).

write_quick(Term) :- var(Term),
	'$display'(Term).
write_quick(Term) :- atomic(Term),
	'$display'(Term).
write_quick(Term) :- generic(Term),
	'$display'(Term).

display_quick(Term) :- var(Term),
	'$display'(user_output, Term).
display_quick(Term) :- atomic(Term),
	'$display'(user_output, Term).
display_quick(Term) :- generic(Term),
	'$display'(user_output, Term).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  write_out(Term, SynStyle, LexStyle, Prio, PrePrio, Depth, Lpar, Ci, Co)
%
%   Write out a Term in given SynStyle, LexStyle at nesting depth Depth
% in a context of priority Priority (that is, expressions with greater
% priority must be parenthesized), and prefix operators =< PrePrio must
% be parenthesized, where the last token to be written was of type Ci,
% and reports that the last token it wrote was of type Co.
%
% Note: We may not use cut in indexing, since this would prevent our last
%       (default) clause to trigger on generics. This is due to the fact
%       that generics are treated as variables (the most general case)
%       during indexing.
%
write_out(Term, _, _, _, _, _, _, Ci, 2'000) :-	var(Term),
	maybe_space(Ci, 2'000),
	!,
	'$write'(Term).

write_out('$VAR'(N), SynStyle, LexStyle, _, _, Depth, _, Ci, Co) :-
	Depth1 is Depth+1,
	!,
	write_VAR(N, SynStyle, LexStyle, Depth1, Ci, Co).

write_out(_, print(Limit), _, _, _, Depth, _, Ci, 2'010) :- Depth > Limit,
	maybe_space(Ci, 2'010),
	!,
	'$display'(...).

write_out(Term, print(_), _, _, _, _, _, _, 2'000) :-
	'USERCALL'(current_predicate(_,portray(_))),
	%%%%% NOTE: portray/1 might bind variables.
	( \+ 'USERCALL'(portray(Term)) -> fail ; true ), !.

write_out(Atom, _, LexStyle, _, PrePrio, _, Lpar, _, 2'100) :- atom(Atom),
	current_prefixop(Atom, _, P, _),
	P =< PrePrio,
	!,
	'$display'(Lpar),
	write_atom(LexStyle, Atom, 2'100, _),
	put(0')).
write_out(Atom, _, LexStyle, _, _, _, _, Ci, Co) :- atom(Atom), !,
	write_atom(LexStyle, Atom, Ci, Co).

write_out(N, _, _, _, _, _, _, Ci, 2'000) :- number(N), !,
	( N < 0 -> maybe_space(Ci, 2'010) ; maybe_space(Ci, 2'000) ),
	'$write'(N).

write_out(Term, noop, LexStyle, _, _, Depth, _, Ci, 2'100) :- functor(Term, Atom, Arity), !,
	write_atom(LexStyle, Atom, Ci, _),
	Depth1 is Depth+1,
	write_args(0, Arity, Term, noop, LexStyle, Depth1).

write_out({Term}, SynStyle, LexStyle, _, _, Depth, _, _, 2'100) :-
	Depth1 is Depth+1,
	!,
	put(0'{),
	write_out(Term, SynStyle, LexStyle, 1200, 0, Depth1, '(', 2'100, _),
	put(0'}).

write_out([Head|Tail], SynStyle, LexStyle, _, _, Depth, _, _, 2'100) :-
	Depth1 is Depth+1,
	!,
	put(0'[),
	write_out(Head, SynStyle, LexStyle, 999, 0, Depth1, '(', 2'100, _),
	write_tail(Tail, SynStyle, LexStyle, Depth1).

%%% This clause stops writeq quoting commas.

write_out((A,B), SynStyle, LexStyle, Prio, _, Depth, Lpar, Ci, Co) :-
	Depth1 is Depth+1,
	!,
	maybe_paren(1000, Prio, Lpar, Lpar1, Ci, C1),
	write_out(A, SynStyle, LexStyle, 999, 0, Depth1, Lpar1, C1, _),
	put(0',),
	write_out(B, SynStyle, LexStyle, 1000, 1000, Depth1, '(', 2'100, C2),
	maybe_paren(1000, Prio, C2, Co).

write_out(Term, SynStyle, LexStyle, Prio, PrePrio, Depth, Lpar, Ci, Co) :- functor(Term, F, N), !,
	Depth1 is Depth+1,
	write_out(N, F, Term, SynStyle, LexStyle, Prio, PrePrio, Depth1, Lpar, Ci, Co).

write_out(Term, _, _, _, _, _, _, _, _) :-
	'$write'(Term).



write_out(1, F, Term, SynStyle, LexStyle, Prio, _, Depth, Lpar, Ci, Co) :-
        current_postfixop(F, _, P, O),
	!,
	( current_infixop(F, _, _, _, _) -> O1 = 1200 ; O1 = O ),
	maybe_paren(O1, Prio, Lpar, Lpar1, Ci, C1),
	arg(1, Term, A),
	write_out(A, SynStyle, LexStyle, P, 1200, Depth, Lpar1, C1, C2),
	write_atom(LexStyle, F, C2, C3),
	maybe_paren(O1, Prio, C3, Co).
write_out(1, F, Term, SynStyle, LexStyle, Prio, PrePrio, Depth, Lpar, Ci, Co) :-
        F \== -,
        current_prefixop(F, _, O, P),
	!,
	( PrePrio == 1200 -> O1 is P+1 ; O1 = O ), %%% for "fy X yf" etc. cases
	maybe_paren(O1, Prio, Lpar, _, Ci, C1),
	write_atom(LexStyle, F, C1, C2),
	arg(1, Term, A),
	write_out(A, SynStyle, LexStyle, P, P, Depth, ' (', C2, C3),
	maybe_paren(O1, Prio, C3, Co).
write_out(2, F, Term, SynStyle, LexStyle, Prio, PrePrio, Depth, Lpar, Ci, Co) :-
        current_infixop(F, _, P, O, Q),
	!,
	( PrePrio == 1200 -> O1 is Q+1 ; O1 = O ), %%% for "U xfy X yf" etc. cases
	maybe_paren(O1, Prio, Lpar, Lpar1, Ci, C1),
	arg(1, Term, A),
	write_out(A, SynStyle, LexStyle, P, 1200, Depth, Lpar1, C1, C2),
	write_atom(LexStyle, F, C2, C3),
	arg(2, Term, B),
	write_out(B, SynStyle, LexStyle, Q, Q, Depth, '(', C3, C4),
	maybe_paren(O1, Prio, C4, Co).
write_out(N, F, Term, SynStyle, LexStyle, _, _, Depth, _, Ci, 2'100) :-
	write_atom(LexStyle, F, Ci, _),
	write_args(0, N, Term, SynStyle, LexStyle, Depth).

write_VAR(N, SynStyle, _, _, Ci, 2'000) :-
	integer(N), N >= 0,
	SynStyle \== noop,
	!,
	maybe_space(Ci, 2'000),
	Letter is N mod 26 + 0'A,
	put(Letter),
	( N >= 26 -> Rest is N//26, '$write'(Rest) ; true ).
write_VAR(String, SynStyle, _, _, Ci, Co) :-
	nonvar(String),
	( atom_chars(Atom, String) -> true ; Atom = String ),
	atom(Atom),
	SynStyle \== noop,
	!,
	'$atom_mode'(Atom, Co),
	maybe_space(Ci, Co),
	'$display'(Atom).
write_VAR(X, SynStyle, LexStyle, Depth, Ci, 2'100) :-
	write_atom(LexStyle, '$VAR', Ci, _),
	write_args(0, 1, '$VAR'(X), SynStyle, LexStyle, Depth).


write_atom(noquote, Atom, Ci, Co) :-
	'$atom_mode'(Atom, Co),
	maybe_space(Ci, Co),
        '$display'(Atom).

write_atom(quote, Atom, Ci, Co) :-
	'$atom_mode'(Atom, Co),
	maybe_space(Ci, Co),
        '$write'(Atom).


%%
%  write_args(DoneSoFar, Arity, Term, SynStyle, LexStyle, Depth)
%
%   Write the remaining arguments of a Term with Arity arguments all told in
% SynStyle, LexStyle, given that DoneSoFar have already been written.
%
write_args(N, N, _, _, _, _) :- !,
	put(0')).
write_args(I, _, _, print(Limit), _, Depth) :- Depth > Limit, !,
	write_args(I, Depth),
	'$display'(...),
	put(0')).
write_args(I, N, Term, SynStyle, LexStyle, Depth) :-
	write_args(I, Depth),
	J is I+1,
	arg(J, Term, A),
	write_out(A, SynStyle, LexStyle, 999, 0, Depth, '(', 2'100, _),
	Depth1 is Depth+1,
	write_args(J, N, Term, SynStyle, LexStyle, Depth1).

write_args(0, _) :- !, put(0'().
write_args(I, I) :- !, '$display'(', ').
write_args(_, _) :- put(0',).


%%
%  write_tail(Tail, SynStyle, LexStyle, Depth)
%
%   Writes the tail of a list of a given SynStyle, LexStyle, Depth.
%
write_tail(Var, _, _, _) :- var(Var), !,	%  |var]
	put(0'|),
	'$write'(Var),
	put(0']).
write_tail([], _, _, _) :- !,			%  ]
	put(0']).
write_tail(_, print(Limit), _, Depth) :- Depth > Limit, !,
	put(0',),
	'$display'(...),
	put(0']).
write_tail([Head|Tail], SynStyle, LexStyle, Depth) :- !, %  ,Head tail
	put(0',),
	write_out(Head, SynStyle, LexStyle, 999, 0, Depth, '(', 2'100, _),
	Depth1 is Depth+1,
	write_tail(Tail, SynStyle, LexStyle, Depth1).
write_tail(Other, SynStyle, LexStyle, Depth) :-	%  |junk]
	put(0'|),
	write_out(Other, SynStyle, LexStyle, 999, 0, Depth, '(', 2'100, _),
	put(0']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  maybe_paren(P, Prio, Char, Ci, Co)
%
%   Write a parenthesis if the context demands it.
%
%   Context = 2'000 for alpha
%   Context = 2'001 for quote
%   Context = 2'010 for other
%   Context = 2'100 for punct
%
maybe_paren(P, Prio, Lpar, '(', _, 2'100) :- P > Prio, !,
	'$display'(Lpar).
maybe_paren(_, _, Lpar, Lpar, C, C).


maybe_paren(P, Prio, _, 2'100) :- P > Prio, !,
	'$display'(')').
maybe_paren(_, _, C, C).


%%
%  maybe_space(LeftContext, TypeOfToken)
%
%   Generates spaces as needed to ensure that two successive tokens won't
% run into each other.
%
maybe_space(Ci, Co) :-
	( Ci\/Co < 2'100, Ci#Co < 2'010 -> put(0' ) ; true ).

/*
sticky_contexts(alpha, alpha).
sticky_contexts(quote, quote).
sticky_contexts(other, other).
sticky_contexts(alpha, quote).
sticky_contexts(quote, alpha).
*/
