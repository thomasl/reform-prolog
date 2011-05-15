/*
 * enumeratevars.pl - Variable enumeration.
 *
 *
 * NOTE: SICStus 2.1 numbervars/3 does not handle clashes with the internal
 *       representation.
 *
 * | ?- numbervars('$VAR'(X),1,Z).
 *
 * X = 1,
 * Z = 2 ? 
 *
 * yes
 *
 *
 * NOTE: cyclic structures are not supported.
 *
 */
enumeratevars(Term,M,N) :-
	( integer(M) ->
	    '$enumeratevars'(Term,M,N)
	; true,
	    error('illegal second argument in ~g', enumeratevars(Term,M,N))
	).

'$enumeratevars'(Term,M,N) :-
	( var(Term) ->
	    Term = '$VAR'(M),
	    N is M + 1
	; atomic(Term) ->
	    N = M
	; Term = [Head|Tail] ->
	    '$enumeratevars'(Head,M,Mx),
	    '$enumeratevars'(Tail,Mx,N)
	; Term = '$VAR'(Count), integer(Count) ->
	    ( M =:= Count ->
	        N is M + 1
	    ; true,
	        N = M
	    )
	; true,
	    Term =.. [_Functor|Arguments],
	    '$enumeratevars'(Arguments,M,N)
	).
