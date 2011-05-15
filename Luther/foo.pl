propagate_and_recompute(P,N) :-
        foo(P,N),
	PN = P/N,
	lookup_cp_sp(PN),
	less_than(PN).

