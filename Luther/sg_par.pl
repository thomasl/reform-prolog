
solution :-
	solution(Answer),
	format('Answer is ~q.~n',[Answer]).

solution(Answer) :-
	g1(X1),
	g2(X2),
	g3(X3),
	g4(X4),
	sort([X1,X2,X3,X4], Set),
	mm(Table),
	solution(Set, Set, Table, Answer).

solution([], State, _, State) :-
 	length(State, Cnt),
 	format(user, '~d elts total~n', [Cnt]),
	true.
solution([N|New0], State0, Table, Sol) :-
 	length(State0, Cnt),
 	format(user, '~d elts so far~n', [Cnt]),
	gen(N, N1, N2, N3, N4, Table),
	put_in_tree(Tree, N1),
	put_in_tree(Tree, N2),
	put_in_tree(Tree, N3),
	put_in_tree(Tree, N4),
	all_products(New0, Table, Tree),
	tree_to_list(Tree, Ps, []),
	merge_new(State0, Ps, State, New),
	solution(New, State, Table, Sol).

:- parallel([all_products/3]).

all_products([], _Table, _Tree) :- !.
all_products([N|Ns], Table, Tree) :-
	gen(N, N1, N2, N3, N4, Table),
	put_in_tree(Tree, N1),
	put_in_tree(Tree, N2),
	put_in_tree(Tree, N3),
	put_in_tree(Tree, N4),
	all_products(Ns, Table, Tree).

/* merge_new(A, B, union(A,B), set_diff(B,A)). */

merge_new([], Set, Set, Set) :- !.
merge_new(Set, [], Set, []) :- !.
merge_new([O|Os], [N|Ns], Set, New) :-
	compare(C, O, N), 
	merge_new(C, O, Os, N, Ns, Set, New).
	
merge_new(<, O, [], N, Ns, [O, N|Ns], [N|Ns]) :- !.
merge_new(<, O1, [O|Os], N, Ns, [O1|Set], New) :-
	compare(C, O, N), 
	merge_new(C, O, Os, N, Ns, Set, New).
merge_new(=, _, Os, N, Ns, [N|Set], New) :-
	merge_new(Os, Ns, Set, New).
merge_new(>, O, Os, N, [], [N, O|Os], [N]) :- !.
merge_new(>, O, Os, N1, [N|Ns], [N1|Set], [N1|New]) :-
	compare(C, O, N), 
	merge_new(C, O, Os, N, Ns, Set, New).

put_in_tree(t(_, E, _), E) :- !.
put_in_tree(t(L, F, _R), E) :-
	E @< F, !,
	put_in_tree(L, E).
put_in_tree(t(_L, _F, R), E) :-
	% E > F,
	put_in_tree(R, E).

tree_to_list(T, H0, T0) :- var(T), !, H0=T0.
tree_to_list(t(L, E, R), H, T) :-
	tree_to_list(L, H, [E|T0]),
	tree_to_list(R, T0, T).

/* compiled generators */

%% The algebra (associative).
mm(m(m(1,1,1,1,1,1),
     m(1,2,1,4,1,2),
     m(1,1,3,1,5,3),
     m(1,1,4,1,2,4),
     m(1,5,1,3,1,5),
     m(1,2,3,4,5,6))).

%% The generators.
/*
    position 1 is 1
    position 8 is 2
    position 15 is 3
    position 36 is 6
    position 37 is 1
    position 44 is 1
    position 51 is 3
    position 72 is 6
    position 73 is 1
    position 80 is 2
    position 87 is 1
    position 108 is 6
    position 109 is 1
    position 116 is 2
    position 123 is 3
    position 144 is 6
    position 145 is 1
    position 152 is 2
    position 159 is 3
    position 180 is 6
    position 181 is 1
    position 188 is 2
    position 195 is 3
    position 216 is 6
*/
g1(*(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,1,1,1,1,1,1,2,1,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,1,1,1,1,1,1,2,2,2,2,2,2,3,3,1,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6)).
g2(*(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,1,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,1,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6)).
g3(*(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,5,5,5,5,5,5,4,4,4,4,4,4,6,6,6,6,6,6,1,1,1,1,1,1,2,1,2,2,2,2,3,3,3,3,3,3,5,5,5,5,5,5,4,4,4,4,4,4,6,6,6,6,6,6,1,1,1,1,1,1,2,2,2,2,2,2,3,3,1,3,3,3,5,5,5,5,5,5,4,4,4,4,4,4,6,6,6,6,6,6,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,5,5,5,5,5,5,4,4,4,4,4,4,6,6,6,6,6,6,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,5,5,5,5,5,5,4,4,4,4,4,4,6,6,6,6,6,6,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,5,5,5,5,5,5,4,4,4,4,4,4,6,6,6,6,6,6)).
g4(*(1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,1,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,1,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6,1,2,3,5,4,6)).

gen(N, N1, N2, N3, N4, m(_,M2,M3,M4,M5,_)) :-
	functor(N1, *, 216),
	functor(N2, *, 216),
	functor(N3, *, 216),
	functor(N4, *, 216),
	gen1(N, N1, N2, N3, N4, M2, M3, M4, M5).

gen1(A, B, C, D, E, F, G, H, I) :-
        arg(1, B, 1),
        arg(1, C, 1),
        arg(1, D, 1),
        arg(1, E, 1),
        gen2(A, B, C, D, E, F, G, H, I).
gen2(A, B, E, G, H, D, J, K, L) :-
        arg(2, A, C),
        arg(2, B, 1),
        arg(C, D, F),
        arg(2, E, F),
        arg(2, G, 1),
        arg(C, D, I),
        arg(2, H, I),
        gen3(A, B, E, G, H, D, J, K, L).
gen3(A, B, E, G, H, J, D, K, L) :-
        arg(3, A, C),
        arg(3, B, 1),
        arg(C, D, F),
        arg(3, E, F),
        arg(3, G, 1),
        arg(C, D, I),
        arg(3, H, I),
        gen4(A, B, E, G, H, J, D, K, L).
gen4(A, B, E, G, I, K, L, D, H) :-
        arg(4, A, C),
        arg(4, B, 1),
        arg(C, D, F),
        arg(4, E, F),
        arg(4, G, 1),
        arg(C, H, J),
        arg(4, I, J),
        gen5(A, B, E, G, I, K, L, D, H).
gen5(A, B, E, G, I, K, L, H, D) :-
        arg(5, A, C),
        arg(5, B, 1),
        arg(C, D, F),
        arg(5, E, F),
        arg(5, G, 1),
        arg(C, H, J),
        arg(5, I, J),
        gen6(A, B, E, G, I, K, L, H, D).
gen6(A, B, C, E, F, G, H, I, J) :-
        arg(6, A, D),
        arg(6, B, 1),
        arg(6, C, D),
        arg(6, E, 1),
        arg(6, F, D),
        gen7(A, B, C, E, F, G, H, I, J).
gen7(A, D, F, G, I, C, J, K, L) :-
        arg(7, A, B),
        arg(B, C, E),
        arg(7, D, E),
        arg(7, F, 1),
        arg(B, C, H),
        arg(7, G, H),
        arg(7, I, 1),
        gen8(A, D, F, G, I, C, J, K, L).
gen8(A, B, C, D, E, F, G, H, I) :-		% by hand
        arg(8, B, 2),
        arg(8, C, 2),
        arg(8, D, 2),
        arg(8, E, 2),
        gen9(A, B, C, D, E, F, G, H, I).
gen9(A, D, G, I, K, C, F, M, N) :-
        arg(9, A, B),
        arg(B, C, E),
        arg(9, D, E),
        arg(B, F, H),
        arg(9, G, H),
        arg(B, C, J),
        arg(9, I, J),
        arg(B, F, L),
        arg(9, K, L),
        gen10(A, D, G, I, K, C, F, M, N).
gen10(A, D, G, I, L, C, N, F, K) :-
        arg(10, A, B),
        arg(B, C, E),
        arg(10, D, E),
        arg(B, F, H),
        arg(10, G, H),
        arg(B, C, J),
        arg(10, I, J),
        arg(B, K, M),
        arg(10, L, M),
        gen11(A, D, G, I, L, C, N, F, K).
gen11(A, D, G, I, L, C, N, K, F) :-
        arg(11, A, B),
        arg(B, C, E),
        arg(11, D, E),
        arg(B, F, H),
        arg(11, G, H),
        arg(B, C, J),
        arg(11, I, J),
        arg(B, K, M),
        arg(11, L, M),
        gen12(A, D, G, I, L, C, N, K, F).
gen12(A, D, F, G, I, C, J, K, L) :-
        arg(12, A, B),
        arg(B, C, E),
        arg(12, D, E),
        arg(12, F, B),
        arg(B, C, H),
        arg(12, G, H),
        arg(12, I, B),
        gen13(A, D, F, G, I, C, J, K, L).
gen13(A, D, F, G, I, J, C, K, L) :-
        arg(13, A, B),
        arg(B, C, E),
        arg(13, D, E),
        arg(13, F, 1),
        arg(B, C, H),
        arg(13, G, H),
        arg(13, I, 1),
        gen14(A, D, F, G, I, J, C, K, L).
gen14(A, D, G, I, K, F, C, M, N) :-
        arg(14, A, B),
        arg(B, C, E),
        arg(14, D, E),
        arg(B, F, H),
        arg(14, G, H),
        arg(B, C, J),
        arg(14, I, J),
        arg(B, F, L),
        arg(14, K, L),
        gen15(A, D, G, I, K, F, C, M, N).
gen15(A, B, C, D, E, F, G, H, I) :-		% by hand
        arg(15, B, 3),
        arg(15, C, 3),
        arg(15, D, 3),
        arg(15, E, 3),
        gen16(A, B, C, D, E, F, G, H, I).
gen16(A, D, G, I, L, N, C, F, K) :-
        arg(16, A, B),
        arg(B, C, E),
        arg(16, D, E),
        arg(B, F, H),
        arg(16, G, H),
        arg(B, C, J),
        arg(16, I, J),
        arg(B, K, M),
        arg(16, L, M),
        gen17(A, D, G, I, L, N, C, F, K).
gen17(A, D, G, I, L, N, C, K, F) :-
        arg(17, A, B),
        arg(B, C, E),
        arg(17, D, E),
        arg(B, F, H),
        arg(17, G, H),
        arg(B, C, J),
        arg(17, I, J),
        arg(B, K, M),
        arg(17, L, M),
        gen18(A, D, G, I, L, N, C, K, F).
gen18(A, D, F, G, I, J, C, K, L) :-
        arg(18, A, B),
        arg(B, C, E),
        arg(18, D, E),
        arg(18, F, B),
        arg(B, C, H),
        arg(18, G, H),
        arg(18, I, B),
        gen19(A, D, F, G, I, J, C, K, L).
gen19(A, D, F, H, J, K, L, C, G) :-
        arg(19, A, B),
        arg(B, C, E),
        arg(19, D, E),
        arg(19, F, 1),
        arg(B, G, I),
        arg(19, H, I),
        arg(19, J, 1),
        gen20(A, D, F, H, J, K, L, C, G).
gen20(A, D, G, J, L, F, N, C, I) :-
        arg(20, A, B),
        arg(B, C, E),
        arg(20, D, E),
        arg(B, F, H),
        arg(20, G, H),
        arg(B, I, K),
        arg(20, J, K),
        arg(B, F, M),
        arg(20, L, M),
        gen21(A, D, G, J, L, F, N, C, I).
gen21(A, D, G, J, L, N, F, C, I) :-
        arg(21, A, B),
        arg(B, C, E),
        arg(21, D, E),
        arg(B, F, H),
        arg(21, G, H),
        arg(B, I, K),
        arg(21, J, K),
        arg(B, F, M),
        arg(21, L, M),
        gen22(A, D, G, J, L, N, F, C, I).
gen22(A, D, F, I, K, M, N, C, H) :-
        arg(22, A, B),
        arg(B, C, E),
        arg(22, D, E),
        arg(B, C, G),
        arg(22, F, G),
        arg(B, H, J),
        arg(22, I, J),
        arg(B, H, L),
        arg(22, K, L),
        gen23(A, D, F, I, K, M, N, C, H).
gen23(A, D, G, I, K, M, N, C, F) :-
        arg(23, A, B),
        arg(B, C, E),
        arg(23, D, E),
        arg(B, F, H),
        arg(23, G, H),
        arg(B, F, J),
        arg(23, I, J),
        arg(B, C, L),
        arg(23, K, L),
        gen24(A, D, G, I, K, M, N, C, F).
gen24(A, D, F, H, J, K, L, C, G) :-
        arg(24, A, B),
        arg(B, C, E),
        arg(24, D, E),
        arg(24, F, B),
        arg(B, G, I),
        arg(24, H, I),
        arg(24, J, B),
        gen25(A, D, F, H, J, K, L, C, G).
gen25(A, D, F, H, J, K, L, G, C) :-
        arg(25, A, B),
        arg(B, C, E),
        arg(25, D, E),
        arg(25, F, 1),
        arg(B, G, I),
        arg(25, H, I),
        arg(25, J, 1),
        gen26(A, D, F, H, J, K, L, G, C).
gen26(A, D, G, J, L, F, N, I, C) :-
        arg(26, A, B),
        arg(B, C, E),
        arg(26, D, E),
        arg(B, F, H),
        arg(26, G, H),
        arg(B, I, K),
        arg(26, J, K),
        arg(B, F, M),
        arg(26, L, M),
        gen27(A, D, G, J, L, F, N, I, C).
gen27(A, D, G, J, L, N, F, I, C) :-
        arg(27, A, B),
        arg(B, C, E),
        arg(27, D, E),
        arg(B, F, H),
        arg(27, G, H),
        arg(B, I, K),
        arg(27, J, K),
        arg(B, F, M),
        arg(27, L, M),
        gen28(A, D, G, J, L, N, F, I, C).
gen28(A, D, G, I, K, M, N, F, C) :-
        arg(28, A, B),
        arg(B, C, E),
        arg(28, D, E),
        arg(B, F, H),
        arg(28, G, H),
        arg(B, F, J),
        arg(28, I, J),
        arg(B, C, L),
        arg(28, K, L),
        gen29(A, D, G, I, K, M, N, F, C).
gen29(A, D, F, I, K, M, N, H, C) :-
        arg(29, A, B),
        arg(B, C, E),
        arg(29, D, E),
        arg(B, C, G),
        arg(29, F, G),
        arg(B, H, J),
        arg(29, I, J),
        arg(B, H, L),
        arg(29, K, L),
        gen30(A, D, F, I, K, M, N, H, C).
gen30(A, D, F, H, J, K, L, G, C) :-
        arg(30, A, B),
        arg(B, C, E),
        arg(30, D, E),
        arg(30, F, B),
        arg(B, G, I),
        arg(30, H, I),
        arg(30, J, B),
        gen31(A, D, F, H, J, K, L, G, C).
gen31(A, B, D, E, F, G, H, I, J) :-
        arg(31, A, C),
        arg(31, B, C),
        arg(31, D, 1),
        arg(31, E, C),
        arg(31, F, 1),
        gen32(A, B, D, E, F, G, H, I, J).
gen32(A, B, E, G, H, D, J, K, L) :-
        arg(32, A, C),
        arg(32, B, C),
        arg(C, D, F),
        arg(32, E, F),
        arg(32, G, C),
        arg(C, D, I),
        arg(32, H, I),
        gen33(A, B, E, G, H, D, J, K, L).
gen33(A, B, E, G, H, J, D, K, L) :-
        arg(33, A, C),
        arg(33, B, C),
        arg(C, D, F),
        arg(33, E, F),
        arg(33, G, C),
        arg(C, D, I),
        arg(33, H, I),
        gen34(A, B, E, G, H, J, D, K, L).
gen34(A, B, E, G, I, K, L, D, H) :-
        arg(34, A, C),
        arg(34, B, C),
        arg(C, D, F),
        arg(34, E, F),
        arg(34, G, C),
        arg(C, H, J),
        arg(34, I, J),
        gen35(A, B, E, G, I, K, L, D, H).
gen35(A, B, E, G, I, K, L, H, D) :-
        arg(35, A, C),
        arg(35, B, C),
        arg(C, D, F),
        arg(35, E, F),
        arg(35, G, C),
        arg(C, H, J),
        arg(35, I, J),
        gen36(A, B, E, G, I, K, L, H, D).
gen36(A, B, D, E, F, G, H, I, J) :-		% by hand
        arg(36, B, 6),
        arg(36, D, 6),
        arg(36, E, 6),
        arg(36, F, 6),
        gen37(A, B, D, E, F, G, H, I, J).
gen37(A, B, C, D, E, F, G, H, I) :-
        arg(37, B, 1),
        arg(37, C, 1),
        arg(37, D, 1),
        arg(37, E, 1),
        gen38(A, B, C, D, E, F, G, H, I).
gen38(A, B, E, G, H, D, J, K, L) :-
        arg(38, A, C),
        arg(38, B, 1),
        arg(C, D, F),
        arg(38, E, F),
        arg(38, G, 1),
        arg(C, D, I),
        arg(38, H, I),
        gen39(A, B, E, G, H, D, J, K, L).
gen39(A, B, E, G, H, J, D, K, L) :-
        arg(39, A, C),
        arg(39, B, 1),
        arg(C, D, F),
        arg(39, E, F),
        arg(39, G, 1),
        arg(C, D, I),
        arg(39, H, I),
        gen40(A, B, E, G, H, J, D, K, L).
gen40(A, B, E, G, I, K, L, D, H) :-
        arg(40, A, C),
        arg(40, B, 1),
        arg(C, D, F),
        arg(40, E, F),
        arg(40, G, 1),
        arg(C, H, J),
        arg(40, I, J),
        gen41(A, B, E, G, I, K, L, D, H).
gen41(A, B, E, G, I, K, L, H, D) :-
        arg(41, A, C),
        arg(41, B, 1),
        arg(C, D, F),
        arg(41, E, F),
        arg(41, G, 1),
        arg(C, H, J),
        arg(41, I, J),
        gen42(A, B, E, G, I, K, L, H, D).
gen42(A, B, C, E, F, G, H, I, J) :-
        arg(42, A, D),
        arg(42, B, 1),
        arg(42, C, D),
        arg(42, E, 1),
        arg(42, F, D),
        gen43(A, B, C, E, F, G, H, I, J).
gen43(A, D, F, G, I, C, J, K, L) :-
        arg(43, A, B),
        arg(B, C, E),
        arg(43, D, E),
        arg(43, F, 1),
        arg(B, C, H),
        arg(43, G, H),
        arg(43, I, 1),
        gen44(A, D, F, G, I, C, J, K, L).
gen44(A, B, C, D, E, F, G, H, I) :-
        arg(44, B, 1),
        arg(44, C, 1),
        arg(44, D, 1),
        arg(44, E, 1),
        gen45(A, B, C, D, E, F, G, H, I).
gen45(A, D, G, I, K, C, F, M, N) :-
        arg(45, A, B),
        arg(B, C, E),
        arg(45, D, E),
        arg(B, F, H),
        arg(45, G, H),
        arg(B, C, J),
        arg(45, I, J),
        arg(B, F, L),
        arg(45, K, L),
        gen46(A, D, G, I, K, C, F, M, N).
gen46(A, D, G, I, L, C, N, F, K) :-
        arg(46, A, B),
        arg(B, C, E),
        arg(46, D, E),
        arg(B, F, H),
        arg(46, G, H),
        arg(B, C, J),
        arg(46, I, J),
        arg(B, K, M),
        arg(46, L, M),
        gen47(A, D, G, I, L, C, N, F, K).
gen47(A, D, G, I, L, C, N, K, F) :-
        arg(47, A, B),
        arg(B, C, E),
        arg(47, D, E),
        arg(B, F, H),
        arg(47, G, H),
        arg(B, C, J),
        arg(47, I, J),
        arg(B, K, M),
        arg(47, L, M),
        gen48(A, D, G, I, L, C, N, K, F).
gen48(A, D, F, G, I, C, J, K, L) :-
        arg(48, A, B),
        arg(B, C, E),
        arg(48, D, E),
        arg(48, F, B),
        arg(B, C, H),
        arg(48, G, H),
        arg(48, I, B),
        gen49(A, D, F, G, I, C, J, K, L).
gen49(A, D, F, G, I, J, C, K, L) :-
        arg(49, A, B),
        arg(B, C, E),
        arg(49, D, E),
        arg(49, F, 1),
        arg(B, C, H),
        arg(49, G, H),
        arg(49, I, 1),
        gen50(A, D, F, G, I, J, C, K, L).
gen50(A, D, G, I, K, F, C, M, N) :-
        arg(50, A, B),
        arg(B, C, E),
        arg(50, D, E),
        arg(B, F, H),
        arg(50, G, H),
        arg(B, C, J),
        arg(50, I, J),
        arg(B, F, L),
        arg(50, K, L),
        gen51(A, D, G, I, K, F, C, M, N).
gen51(A, B, C, D, E, F, G, H, I) :-		% by hand
        arg(51, B, 3),
        arg(51, C, 3),
        arg(51, D, 3),
        arg(51, E, 3),
        gen52(A, B, C, D, E, F, G, H, I).
gen52(A, D, G, I, L, N, C, F, K) :-
        arg(52, A, B),
        arg(B, C, E),
        arg(52, D, E),
        arg(B, F, H),
        arg(52, G, H),
        arg(B, C, J),
        arg(52, I, J),
        arg(B, K, M),
        arg(52, L, M),
        gen53(A, D, G, I, L, N, C, F, K).
gen53(A, D, G, I, L, N, C, K, F) :-
        arg(53, A, B),
        arg(B, C, E),
        arg(53, D, E),
        arg(B, F, H),
        arg(53, G, H),
        arg(B, C, J),
        arg(53, I, J),
        arg(B, K, M),
        arg(53, L, M),
        gen54(A, D, G, I, L, N, C, K, F).
gen54(A, D, F, G, I, J, C, K, L) :-
        arg(54, A, B),
        arg(B, C, E),
        arg(54, D, E),
        arg(54, F, B),
        arg(B, C, H),
        arg(54, G, H),
        arg(54, I, B),
        gen55(A, D, F, G, I, J, C, K, L).
gen55(A, D, F, H, J, K, L, C, G) :-
        arg(55, A, B),
        arg(B, C, E),
        arg(55, D, E),
        arg(55, F, 1),
        arg(B, G, I),
        arg(55, H, I),
        arg(55, J, 1),
        gen56(A, D, F, H, J, K, L, C, G).
gen56(A, D, G, J, L, F, N, C, I) :-
        arg(56, A, B),
        arg(B, C, E),
        arg(56, D, E),
        arg(B, F, H),
        arg(56, G, H),
        arg(B, I, K),
        arg(56, J, K),
        arg(B, F, M),
        arg(56, L, M),
        gen57(A, D, G, J, L, F, N, C, I).
gen57(A, D, G, J, L, N, F, C, I) :-
        arg(57, A, B),
        arg(B, C, E),
        arg(57, D, E),
        arg(B, F, H),
        arg(57, G, H),
        arg(B, I, K),
        arg(57, J, K),
        arg(B, F, M),
        arg(57, L, M),
        gen58(A, D, G, J, L, N, F, C, I).
gen58(A, D, F, I, K, M, N, C, H) :-
        arg(58, A, B),
        arg(B, C, E),
        arg(58, D, E),
        arg(B, C, G),
        arg(58, F, G),
        arg(B, H, J),
        arg(58, I, J),
        arg(B, H, L),
        arg(58, K, L),
        gen59(A, D, F, I, K, M, N, C, H).
gen59(A, D, G, I, K, M, N, C, F) :-
        arg(59, A, B),
        arg(B, C, E),
        arg(59, D, E),
        arg(B, F, H),
        arg(59, G, H),
        arg(B, F, J),
        arg(59, I, J),
        arg(B, C, L),
        arg(59, K, L),
        gen60(A, D, G, I, K, M, N, C, F).
gen60(A, D, F, H, J, K, L, C, G) :-
        arg(60, A, B),
        arg(B, C, E),
        arg(60, D, E),
        arg(60, F, B),
        arg(B, G, I),
        arg(60, H, I),
        arg(60, J, B),
        gen61(A, D, F, H, J, K, L, C, G).
gen61(A, D, F, H, J, K, L, G, C) :-
        arg(61, A, B),
        arg(B, C, E),
        arg(61, D, E),
        arg(61, F, 1),
        arg(B, G, I),
        arg(61, H, I),
        arg(61, J, 1),
        gen62(A, D, F, H, J, K, L, G, C).
gen62(A, D, G, J, L, F, N, I, C) :-
        arg(62, A, B),
        arg(B, C, E),
        arg(62, D, E),
        arg(B, F, H),
        arg(62, G, H),
        arg(B, I, K),
        arg(62, J, K),
        arg(B, F, M),
        arg(62, L, M),
        gen63(A, D, G, J, L, F, N, I, C).
gen63(A, D, G, J, L, N, F, I, C) :-
        arg(63, A, B),
        arg(B, C, E),
        arg(63, D, E),
        arg(B, F, H),
        arg(63, G, H),
        arg(B, I, K),
        arg(63, J, K),
        arg(B, F, M),
        arg(63, L, M),
        gen64(A, D, G, J, L, N, F, I, C).
gen64(A, D, G, I, K, M, N, F, C) :-
        arg(64, A, B),
        arg(B, C, E),
        arg(64, D, E),
        arg(B, F, H),
        arg(64, G, H),
        arg(B, F, J),
        arg(64, I, J),
        arg(B, C, L),
        arg(64, K, L),
        gen65(A, D, G, I, K, M, N, F, C).
gen65(A, D, F, I, K, M, N, H, C) :-
        arg(65, A, B),
        arg(B, C, E),
        arg(65, D, E),
        arg(B, C, G),
        arg(65, F, G),
        arg(B, H, J),
        arg(65, I, J),
        arg(B, H, L),
        arg(65, K, L),
        gen66(A, D, F, I, K, M, N, H, C).
gen66(A, D, F, H, J, K, L, G, C) :-
        arg(66, A, B),
        arg(B, C, E),
        arg(66, D, E),
        arg(66, F, B),
        arg(B, G, I),
        arg(66, H, I),
        arg(66, J, B),
        gen67(A, D, F, H, J, K, L, G, C).
gen67(A, B, D, E, F, G, H, I, J) :-
        arg(67, A, C),
        arg(67, B, C),
        arg(67, D, 1),
        arg(67, E, C),
        arg(67, F, 1),
        gen68(A, B, D, E, F, G, H, I, J).
gen68(A, B, E, G, H, D, J, K, L) :-
        arg(68, A, C),
        arg(68, B, C),
        arg(C, D, F),
        arg(68, E, F),
        arg(68, G, C),
        arg(C, D, I),
        arg(68, H, I),
        gen69(A, B, E, G, H, D, J, K, L).
gen69(A, B, E, G, H, J, D, K, L) :-
        arg(69, A, C),
        arg(69, B, C),
        arg(C, D, F),
        arg(69, E, F),
        arg(69, G, C),
        arg(C, D, I),
        arg(69, H, I),
        gen70(A, B, E, G, H, J, D, K, L).
gen70(A, B, E, G, I, K, L, D, H) :-
        arg(70, A, C),
        arg(70, B, C),
        arg(C, D, F),
        arg(70, E, F),
        arg(70, G, C),
        arg(C, H, J),
        arg(70, I, J),
        gen71(A, B, E, G, I, K, L, D, H).
gen71(A, B, E, G, I, K, L, H, D) :-
        arg(71, A, C),
        arg(71, B, C),
        arg(C, D, F),
        arg(71, E, F),
        arg(71, G, C),
        arg(C, H, J),
        arg(71, I, J),
        gen72(A, B, E, G, I, K, L, H, D).
gen72(A, B, D, E, F, G, H, I, J) :-		% by hand
        arg(72, B, 6),
        arg(72, D, 6),
        arg(72, E, 6),
        arg(72, F, 6),
        gen73(A, B, D, E, F, G, H, I, J).
gen73(A, B, C, D, E, F, G, H, I) :-
        arg(73, B, 1),
        arg(73, C, 1),
        arg(73, D, 1),
        arg(73, E, 1),
        gen74(A, B, C, D, E, F, G, H, I).
gen74(A, B, E, G, H, D, J, K, L) :-
        arg(74, A, C),
        arg(74, B, 1),
        arg(C, D, F),
        arg(74, E, F),
        arg(74, G, 1),
        arg(C, D, I),
        arg(74, H, I),
        gen75(A, B, E, G, H, D, J, K, L).
gen75(A, B, E, G, H, J, D, K, L) :-
        arg(75, A, C),
        arg(75, B, 1),
        arg(C, D, F),
        arg(75, E, F),
        arg(75, G, 1),
        arg(C, D, I),
        arg(75, H, I),
        gen76(A, B, E, G, H, J, D, K, L).
gen76(A, B, E, G, I, K, L, D, H) :-
        arg(76, A, C),
        arg(76, B, 1),
        arg(C, D, F),
        arg(76, E, F),
        arg(76, G, 1),
        arg(C, H, J),
        arg(76, I, J),
        gen77(A, B, E, G, I, K, L, D, H).
gen77(A, B, E, G, I, K, L, H, D) :-
        arg(77, A, C),
        arg(77, B, 1),
        arg(C, D, F),
        arg(77, E, F),
        arg(77, G, 1),
        arg(C, H, J),
        arg(77, I, J),
        gen78(A, B, E, G, I, K, L, H, D).
gen78(A, B, C, E, F, G, H, I, J) :-
        arg(78, A, D),
        arg(78, B, 1),
        arg(78, C, D),
        arg(78, E, 1),
        arg(78, F, D),
        gen79(A, B, C, E, F, G, H, I, J).
gen79(A, D, F, G, I, C, J, K, L) :-
        arg(79, A, B),
        arg(B, C, E),
        arg(79, D, E),
        arg(79, F, 1),
        arg(B, C, H),
        arg(79, G, H),
        arg(79, I, 1),
        gen80(A, D, F, G, I, C, J, K, L).
gen80(A, B, C, D, E, F, G, H, I) :-		% by hand
        arg(80, B, 2),
        arg(80, C, 2),
        arg(80, D, 2),
        arg(80, E, 2),
        gen81(A, B, C, D, E, F, G, H, I).
gen81(A, D, G, I, K, C, F, M, N) :-
        arg(81, A, B),
        arg(B, C, E),
        arg(81, D, E),
        arg(B, F, H),
        arg(81, G, H),
        arg(B, C, J),
        arg(81, I, J),
        arg(B, F, L),
        arg(81, K, L),
        gen82(A, D, G, I, K, C, F, M, N).
gen82(A, D, G, I, L, C, N, F, K) :-
        arg(82, A, B),
        arg(B, C, E),
        arg(82, D, E),
        arg(B, F, H),
        arg(82, G, H),
        arg(B, C, J),
        arg(82, I, J),
        arg(B, K, M),
        arg(82, L, M),
        gen83(A, D, G, I, L, C, N, F, K).
gen83(A, D, G, I, L, C, N, K, F) :-
        arg(83, A, B),
        arg(B, C, E),
        arg(83, D, E),
        arg(B, F, H),
        arg(83, G, H),
        arg(B, C, J),
        arg(83, I, J),
        arg(B, K, M),
        arg(83, L, M),
        gen84(A, D, G, I, L, C, N, K, F).
gen84(A, D, F, G, I, C, J, K, L) :-
        arg(84, A, B),
        arg(B, C, E),
        arg(84, D, E),
        arg(84, F, B),
        arg(B, C, H),
        arg(84, G, H),
        arg(84, I, B),
        gen85(A, D, F, G, I, C, J, K, L).
gen85(A, D, F, G, I, J, C, K, L) :-
        arg(85, A, B),
        arg(B, C, E),
        arg(85, D, E),
        arg(85, F, 1),
        arg(B, C, H),
        arg(85, G, H),
        arg(85, I, 1),
        gen86(A, D, F, G, I, J, C, K, L).
gen86(A, D, G, I, K, F, C, M, N) :-
        arg(86, A, B),
        arg(B, C, E),
        arg(86, D, E),
        arg(B, F, H),
        arg(86, G, H),
        arg(B, C, J),
        arg(86, I, J),
        arg(B, F, L),
        arg(86, K, L),
        gen87(A, D, G, I, K, F, C, M, N).
gen87(A, B, C, D, E, F, G, H, I) :-
        arg(87, B, 1),
        arg(87, C, 1),
        arg(87, D, 1),
        arg(87, E, 1),
        gen88(A, B, C, D, E, F, G, H, I).
gen88(A, D, G, I, L, N, C, F, K) :-
        arg(88, A, B),
        arg(B, C, E),
        arg(88, D, E),
        arg(B, F, H),
        arg(88, G, H),
        arg(B, C, J),
        arg(88, I, J),
        arg(B, K, M),
        arg(88, L, M),
        gen89(A, D, G, I, L, N, C, F, K).
gen89(A, D, G, I, L, N, C, K, F) :-
        arg(89, A, B),
        arg(B, C, E),
        arg(89, D, E),
        arg(B, F, H),
        arg(89, G, H),
        arg(B, C, J),
        arg(89, I, J),
        arg(B, K, M),
        arg(89, L, M),
        gen90(A, D, G, I, L, N, C, K, F).
gen90(A, D, F, G, I, J, C, K, L) :-
        arg(90, A, B),
        arg(B, C, E),
        arg(90, D, E),
        arg(90, F, B),
        arg(B, C, H),
        arg(90, G, H),
        arg(90, I, B),
        gen91(A, D, F, G, I, J, C, K, L).
gen91(A, D, F, H, J, K, L, C, G) :-
        arg(91, A, B),
        arg(B, C, E),
        arg(91, D, E),
        arg(91, F, 1),
        arg(B, G, I),
        arg(91, H, I),
        arg(91, J, 1),
        gen92(A, D, F, H, J, K, L, C, G).
gen92(A, D, G, J, L, F, N, C, I) :-
        arg(92, A, B),
        arg(B, C, E),
        arg(92, D, E),
        arg(B, F, H),
        arg(92, G, H),
        arg(B, I, K),
        arg(92, J, K),
        arg(B, F, M),
        arg(92, L, M),
        gen93(A, D, G, J, L, F, N, C, I).
gen93(A, D, G, J, L, N, F, C, I) :-
        arg(93, A, B),
        arg(B, C, E),
        arg(93, D, E),
        arg(B, F, H),
        arg(93, G, H),
        arg(B, I, K),
        arg(93, J, K),
        arg(B, F, M),
        arg(93, L, M),
        gen94(A, D, G, J, L, N, F, C, I).
gen94(A, D, F, I, K, M, N, C, H) :-
        arg(94, A, B),
        arg(B, C, E),
        arg(94, D, E),
        arg(B, C, G),
        arg(94, F, G),
        arg(B, H, J),
        arg(94, I, J),
        arg(B, H, L),
        arg(94, K, L),
        gen95(A, D, F, I, K, M, N, C, H).
gen95(A, D, G, I, K, M, N, C, F) :-
        arg(95, A, B),
        arg(B, C, E),
        arg(95, D, E),
        arg(B, F, H),
        arg(95, G, H),
        arg(B, F, J),
        arg(95, I, J),
        arg(B, C, L),
        arg(95, K, L),
        gen96(A, D, G, I, K, M, N, C, F).
gen96(A, D, F, H, J, K, L, C, G) :-
        arg(96, A, B),
        arg(B, C, E),
        arg(96, D, E),
        arg(96, F, B),
        arg(B, G, I),
        arg(96, H, I),
        arg(96, J, B),
        gen97(A, D, F, H, J, K, L, C, G).
gen97(A, D, F, H, J, K, L, G, C) :-
        arg(97, A, B),
        arg(B, C, E),
        arg(97, D, E),
        arg(97, F, 1),
        arg(B, G, I),
        arg(97, H, I),
        arg(97, J, 1),
        gen98(A, D, F, H, J, K, L, G, C).
gen98(A, D, G, J, L, F, N, I, C) :-
        arg(98, A, B),
        arg(B, C, E),
        arg(98, D, E),
        arg(B, F, H),
        arg(98, G, H),
        arg(B, I, K),
        arg(98, J, K),
        arg(B, F, M),
        arg(98, L, M),
        gen99(A, D, G, J, L, F, N, I, C).
gen99(A, D, G, J, L, N, F, I, C) :-
        arg(99, A, B),
        arg(B, C, E),
        arg(99, D, E),
        arg(B, F, H),
        arg(99, G, H),
        arg(B, I, K),
        arg(99, J, K),
        arg(B, F, M),
        arg(99, L, M),
        gen100(A, D, G, J, L, N, F, I, C).
gen100(A, D, G, I, K, M, N, F, C) :-
        arg(100, A, B),
        arg(B, C, E),
        arg(100, D, E),
        arg(B, F, H),
        arg(100, G, H),
        arg(B, F, J),
        arg(100, I, J),
        arg(B, C, L),
        arg(100, K, L),
        gen101(A, D, G, I, K, M, N, F, C).
gen101(A, D, F, I, K, M, N, H, C) :-
        arg(101, A, B),
        arg(B, C, E),
        arg(101, D, E),
        arg(B, C, G),
        arg(101, F, G),
        arg(B, H, J),
        arg(101, I, J),
        arg(B, H, L),
        arg(101, K, L),
        gen102(A, D, F, I, K, M, N, H, C).
gen102(A, D, F, H, J, K, L, G, C) :-
        arg(102, A, B),
        arg(B, C, E),
        arg(102, D, E),
        arg(102, F, B),
        arg(B, G, I),
        arg(102, H, I),
        arg(102, J, B),
        gen103(A, D, F, H, J, K, L, G, C).
gen103(A, B, D, E, F, G, H, I, J) :-
        arg(103, A, C),
        arg(103, B, C),
        arg(103, D, 1),
        arg(103, E, C),
        arg(103, F, 1),
        gen104(A, B, D, E, F, G, H, I, J).
gen104(A, B, E, G, H, D, J, K, L) :-
        arg(104, A, C),
        arg(104, B, C),
        arg(C, D, F),
        arg(104, E, F),
        arg(104, G, C),
        arg(C, D, I),
        arg(104, H, I),
        gen105(A, B, E, G, H, D, J, K, L).
gen105(A, B, E, G, H, J, D, K, L) :-
        arg(105, A, C),
        arg(105, B, C),
        arg(C, D, F),
        arg(105, E, F),
        arg(105, G, C),
        arg(C, D, I),
        arg(105, H, I),
        gen106(A, B, E, G, H, J, D, K, L).
gen106(A, B, E, G, I, K, L, D, H) :-
        arg(106, A, C),
        arg(106, B, C),
        arg(C, D, F),
        arg(106, E, F),
        arg(106, G, C),
        arg(C, H, J),
        arg(106, I, J),
        gen107(A, B, E, G, I, K, L, D, H).
gen107(A, B, E, G, I, K, L, H, D) :-
        arg(107, A, C),
        arg(107, B, C),
        arg(C, D, F),
        arg(107, E, F),
        arg(107, G, C),
        arg(C, H, J),
        arg(107, I, J),
        gen108(A, B, E, G, I, K, L, H, D).
gen108(A, B, D, E, F, G, H, I, J) :-		% by hand
        arg(108, B, 6),
        arg(108, D, 6),
        arg(108, E, 6),
        arg(108, F, 6),
        gen109(A, B, D, E, F, G, H, I, J).
gen109(A, B, C, D, E, F, G, H, I) :-
        arg(109, B, 1),
        arg(109, C, 1),
        arg(109, D, 1),
        arg(109, E, 1),
        gen110(A, B, C, D, E, F, G, H, I).
gen110(A, B, E, G, H, D, J, K, L) :-
        arg(110, A, C),
        arg(110, B, 1),
        arg(C, D, F),
        arg(110, E, F),
        arg(110, G, 1),
        arg(C, D, I),
        arg(110, H, I),
        gen111(A, B, E, G, H, D, J, K, L).
gen111(A, B, E, G, H, J, D, K, L) :-
        arg(111, A, C),
        arg(111, B, 1),
        arg(C, D, F),
        arg(111, E, F),
        arg(111, G, 1),
        arg(C, D, I),
        arg(111, H, I),
        gen112(A, B, E, G, H, J, D, K, L).
gen112(A, B, E, G, I, K, L, D, H) :-
        arg(112, A, C),
        arg(112, B, 1),
        arg(C, D, F),
        arg(112, E, F),
        arg(112, G, 1),
        arg(C, H, J),
        arg(112, I, J),
        gen113(A, B, E, G, I, K, L, D, H).
gen113(A, B, E, G, I, K, L, H, D) :-
        arg(113, A, C),
        arg(113, B, 1),
        arg(C, D, F),
        arg(113, E, F),
        arg(113, G, 1),
        arg(C, H, J),
        arg(113, I, J),
        gen114(A, B, E, G, I, K, L, H, D).
gen114(A, B, C, E, F, G, H, I, J) :-
        arg(114, A, D),
        arg(114, B, 1),
        arg(114, C, D),
        arg(114, E, 1),
        arg(114, F, D),
        gen115(A, B, C, E, F, G, H, I, J).
gen115(A, D, F, G, I, C, J, K, L) :-
        arg(115, A, B),
        arg(B, C, E),
        arg(115, D, E),
        arg(115, F, 1),
        arg(B, C, H),
        arg(115, G, H),
        arg(115, I, 1),
        gen116(A, D, F, G, I, C, J, K, L).
gen116(A, B, C, D, E, F, G, H, I) :-		% by hand
        arg(116, B, 2),
        arg(116, C, 2),
        arg(116, D, 2),
        arg(116, E, 2),
        gen117(A, B, C, D, E, F, G, H, I).
gen117(A, D, G, I, K, C, F, M, N) :-
        arg(117, A, B),
        arg(B, C, E),
        arg(117, D, E),
        arg(B, F, H),
        arg(117, G, H),
        arg(B, C, J),
        arg(117, I, J),
        arg(B, F, L),
        arg(117, K, L),
        gen118(A, D, G, I, K, C, F, M, N).
gen118(A, D, G, I, L, C, N, F, K) :-
        arg(118, A, B),
        arg(B, C, E),
        arg(118, D, E),
        arg(B, F, H),
        arg(118, G, H),
        arg(B, C, J),
        arg(118, I, J),
        arg(B, K, M),
        arg(118, L, M),
        gen119(A, D, G, I, L, C, N, F, K).
gen119(A, D, G, I, L, C, N, K, F) :-
        arg(119, A, B),
        arg(B, C, E),
        arg(119, D, E),
        arg(B, F, H),
        arg(119, G, H),
        arg(B, C, J),
        arg(119, I, J),
        arg(B, K, M),
        arg(119, L, M),
        gen120(A, D, G, I, L, C, N, K, F).
gen120(A, D, F, G, I, C, J, K, L) :-
        arg(120, A, B),
        arg(B, C, E),
        arg(120, D, E),
        arg(120, F, B),
        arg(B, C, H),
        arg(120, G, H),
        arg(120, I, B),
        gen121(A, D, F, G, I, C, J, K, L).
gen121(A, D, F, G, I, J, C, K, L) :-
        arg(121, A, B),
        arg(B, C, E),
        arg(121, D, E),
        arg(121, F, 1),
        arg(B, C, H),
        arg(121, G, H),
        arg(121, I, 1),
        gen122(A, D, F, G, I, J, C, K, L).
gen122(A, D, G, I, K, F, C, M, N) :-
        arg(122, A, B),
        arg(B, C, E),
        arg(122, D, E),
        arg(B, F, H),
        arg(122, G, H),
        arg(B, C, J),
        arg(122, I, J),
        arg(B, F, L),
        arg(122, K, L),
        gen123(A, D, G, I, K, F, C, M, N).
gen123(A, B, C, D, E, F, G, H, I) :-		% by hand
        arg(123, B, 3),
        arg(123, C, 3),
        arg(123, D, 3),
        arg(123, E, 3),
        gen124(A, B, C, D, E, F, G, H, I).
gen124(A, D, G, I, L, N, C, F, K) :-
        arg(124, A, B),
        arg(B, C, E),
        arg(124, D, E),
        arg(B, F, H),
        arg(124, G, H),
        arg(B, C, J),
        arg(124, I, J),
        arg(B, K, M),
        arg(124, L, M),
        gen125(A, D, G, I, L, N, C, F, K).
gen125(A, D, G, I, L, N, C, K, F) :-
        arg(125, A, B),
        arg(B, C, E),
        arg(125, D, E),
        arg(B, F, H),
        arg(125, G, H),
        arg(B, C, J),
        arg(125, I, J),
        arg(B, K, M),
        arg(125, L, M),
        gen126(A, D, G, I, L, N, C, K, F).
gen126(A, D, F, G, I, J, C, K, L) :-
        arg(126, A, B),
        arg(B, C, E),
        arg(126, D, E),
        arg(126, F, B),
        arg(B, C, H),
        arg(126, G, H),
        arg(126, I, B),
        gen127(A, D, F, G, I, J, C, K, L).
gen127(A, D, F, H, J, K, L, C, G) :-
        arg(127, A, B),
        arg(B, C, E),
        arg(127, D, E),
        arg(127, F, 1),
        arg(B, G, I),
        arg(127, H, I),
        arg(127, J, 1),
        gen128(A, D, F, H, J, K, L, C, G).
gen128(A, D, G, J, L, F, N, C, I) :-
        arg(128, A, B),
        arg(B, C, E),
        arg(128, D, E),
        arg(B, F, H),
        arg(128, G, H),
        arg(B, I, K),
        arg(128, J, K),
        arg(B, F, M),
        arg(128, L, M),
        gen129(A, D, G, J, L, F, N, C, I).
gen129(A, D, G, J, L, N, F, C, I) :-
        arg(129, A, B),
        arg(B, C, E),
        arg(129, D, E),
        arg(B, F, H),
        arg(129, G, H),
        arg(B, I, K),
        arg(129, J, K),
        arg(B, F, M),
        arg(129, L, M),
        gen130(A, D, G, J, L, N, F, C, I).
gen130(A, D, F, I, K, M, N, C, H) :-
        arg(130, A, B),
        arg(B, C, E),
        arg(130, D, E),
        arg(B, C, G),
        arg(130, F, G),
        arg(B, H, J),
        arg(130, I, J),
        arg(B, H, L),
        arg(130, K, L),
        gen131(A, D, F, I, K, M, N, C, H).
gen131(A, D, G, I, K, M, N, C, F) :-
        arg(131, A, B),
        arg(B, C, E),
        arg(131, D, E),
        arg(B, F, H),
        arg(131, G, H),
        arg(B, F, J),
        arg(131, I, J),
        arg(B, C, L),
        arg(131, K, L),
        gen132(A, D, G, I, K, M, N, C, F).
gen132(A, D, F, H, J, K, L, C, G) :-
        arg(132, A, B),
        arg(B, C, E),
        arg(132, D, E),
        arg(132, F, B),
        arg(B, G, I),
        arg(132, H, I),
        arg(132, J, B),
        gen133(A, D, F, H, J, K, L, C, G).
gen133(A, D, F, H, J, K, L, G, C) :-
        arg(133, A, B),
        arg(B, C, E),
        arg(133, D, E),
        arg(133, F, 1),
        arg(B, G, I),
        arg(133, H, I),
        arg(133, J, 1),
        gen134(A, D, F, H, J, K, L, G, C).
gen134(A, D, G, J, L, F, N, I, C) :-
        arg(134, A, B),
        arg(B, C, E),
        arg(134, D, E),
        arg(B, F, H),
        arg(134, G, H),
        arg(B, I, K),
        arg(134, J, K),
        arg(B, F, M),
        arg(134, L, M),
        gen135(A, D, G, J, L, F, N, I, C).
gen135(A, D, G, J, L, N, F, I, C) :-
        arg(135, A, B),
        arg(B, C, E),
        arg(135, D, E),
        arg(B, F, H),
        arg(135, G, H),
        arg(B, I, K),
        arg(135, J, K),
        arg(B, F, M),
        arg(135, L, M),
        gen136(A, D, G, J, L, N, F, I, C).
gen136(A, D, G, I, K, M, N, F, C) :-
        arg(136, A, B),
        arg(B, C, E),
        arg(136, D, E),
        arg(B, F, H),
        arg(136, G, H),
        arg(B, F, J),
        arg(136, I, J),
        arg(B, C, L),
        arg(136, K, L),
        gen137(A, D, G, I, K, M, N, F, C).
gen137(A, D, F, I, K, M, N, H, C) :-
        arg(137, A, B),
        arg(B, C, E),
        arg(137, D, E),
        arg(B, C, G),
        arg(137, F, G),
        arg(B, H, J),
        arg(137, I, J),
        arg(B, H, L),
        arg(137, K, L),
        gen138(A, D, F, I, K, M, N, H, C).
gen138(A, D, F, H, J, K, L, G, C) :-
        arg(138, A, B),
        arg(B, C, E),
        arg(138, D, E),
        arg(138, F, B),
        arg(B, G, I),
        arg(138, H, I),
        arg(138, J, B),
        gen139(A, D, F, H, J, K, L, G, C).
gen139(A, B, D, E, F, G, H, I, J) :-
        arg(139, A, C),
        arg(139, B, C),
        arg(139, D, 1),
        arg(139, E, C),
        arg(139, F, 1),
        gen140(A, B, D, E, F, G, H, I, J).
gen140(A, B, E, G, H, D, J, K, L) :-
        arg(140, A, C),
        arg(140, B, C),
        arg(C, D, F),
        arg(140, E, F),
        arg(140, G, C),
        arg(C, D, I),
        arg(140, H, I),
        gen141(A, B, E, G, H, D, J, K, L).
gen141(A, B, E, G, H, J, D, K, L) :-
        arg(141, A, C),
        arg(141, B, C),
        arg(C, D, F),
        arg(141, E, F),
        arg(141, G, C),
        arg(C, D, I),
        arg(141, H, I),
        gen142(A, B, E, G, H, J, D, K, L).
gen142(A, B, E, G, I, K, L, D, H) :-
        arg(142, A, C),
        arg(142, B, C),
        arg(C, D, F),
        arg(142, E, F),
        arg(142, G, C),
        arg(C, H, J),
        arg(142, I, J),
        gen143(A, B, E, G, I, K, L, D, H).
gen143(A, B, E, G, I, K, L, H, D) :-
        arg(143, A, C),
        arg(143, B, C),
        arg(C, D, F),
        arg(143, E, F),
        arg(143, G, C),
        arg(C, H, J),
        arg(143, I, J),
        gen144(A, B, E, G, I, K, L, H, D).
gen144(A, B, D, E, F, G, H, I, J) :-		% by hand
        arg(144, B, 6),
        arg(144, D, 6),
        arg(144, E, 6),
        arg(144, F, 6),
        gen145(A, B, D, E, F, G, H, I, J).
gen145(A, B, C, D, E, F, G, H, I) :-
        arg(145, B, 1),
        arg(145, C, 1),
        arg(145, D, 1),
        arg(145, E, 1),
        gen146(A, B, C, D, E, F, G, H, I).
gen146(A, B, E, G, H, D, J, K, L) :-
        arg(146, A, C),
        arg(146, B, 1),
        arg(C, D, F),
        arg(146, E, F),
        arg(146, G, 1),
        arg(C, D, I),
        arg(146, H, I),
        gen147(A, B, E, G, H, D, J, K, L).
gen147(A, B, E, G, H, J, D, K, L) :-
        arg(147, A, C),
        arg(147, B, 1),
        arg(C, D, F),
        arg(147, E, F),
        arg(147, G, 1),
        arg(C, D, I),
        arg(147, H, I),
        gen148(A, B, E, G, H, J, D, K, L).
gen148(A, B, E, G, I, K, L, D, H) :-
        arg(148, A, C),
        arg(148, B, 1),
        arg(C, D, F),
        arg(148, E, F),
        arg(148, G, 1),
        arg(C, H, J),
        arg(148, I, J),
        gen149(A, B, E, G, I, K, L, D, H).
gen149(A, B, E, G, I, K, L, H, D) :-
        arg(149, A, C),
        arg(149, B, 1),
        arg(C, D, F),
        arg(149, E, F),
        arg(149, G, 1),
        arg(C, H, J),
        arg(149, I, J),
        gen150(A, B, E, G, I, K, L, H, D).
gen150(A, B, C, E, F, G, H, I, J) :-
        arg(150, A, D),
        arg(150, B, 1),
        arg(150, C, D),
        arg(150, E, 1),
        arg(150, F, D),
        gen151(A, B, C, E, F, G, H, I, J).
gen151(A, D, F, G, I, C, J, K, L) :-
        arg(151, A, B),
        arg(B, C, E),
        arg(151, D, E),
        arg(151, F, 1),
        arg(B, C, H),
        arg(151, G, H),
        arg(151, I, 1),
        gen152(A, D, F, G, I, C, J, K, L).
gen152(A, B, C, D, E, F, G, H, I) :-		% by hand
        arg(152, B, 2),
        arg(152, C, 2),
        arg(152, D, 2),
        arg(152, E, 2),
        gen153(A, B, C, D, E, F, G, H, I).
gen153(A, D, G, I, K, C, F, M, N) :-
        arg(153, A, B),
        arg(B, C, E),
        arg(153, D, E),
        arg(B, F, H),
        arg(153, G, H),
        arg(B, C, J),
        arg(153, I, J),
        arg(B, F, L),
        arg(153, K, L),
        gen154(A, D, G, I, K, C, F, M, N).
gen154(A, D, G, I, L, C, N, F, K) :-
        arg(154, A, B),
        arg(B, C, E),
        arg(154, D, E),
        arg(B, F, H),
        arg(154, G, H),
        arg(B, C, J),
        arg(154, I, J),
        arg(B, K, M),
        arg(154, L, M),
        gen155(A, D, G, I, L, C, N, F, K).
gen155(A, D, G, I, L, C, N, K, F) :-
        arg(155, A, B),
        arg(B, C, E),
        arg(155, D, E),
        arg(B, F, H),
        arg(155, G, H),
        arg(B, C, J),
        arg(155, I, J),
        arg(B, K, M),
        arg(155, L, M),
        gen156(A, D, G, I, L, C, N, K, F).
gen156(A, D, F, G, I, C, J, K, L) :-
        arg(156, A, B),
        arg(B, C, E),
        arg(156, D, E),
        arg(156, F, B),
        arg(B, C, H),
        arg(156, G, H),
        arg(156, I, B),
        gen157(A, D, F, G, I, C, J, K, L).
gen157(A, D, F, G, I, J, C, K, L) :-
        arg(157, A, B),
        arg(B, C, E),
        arg(157, D, E),
        arg(157, F, 1),
        arg(B, C, H),
        arg(157, G, H),
        arg(157, I, 1),
        gen158(A, D, F, G, I, J, C, K, L).
gen158(A, D, G, I, K, F, C, M, N) :-
        arg(158, A, B),
        arg(B, C, E),
        arg(158, D, E),
        arg(B, F, H),
        arg(158, G, H),
        arg(B, C, J),
        arg(158, I, J),
        arg(B, F, L),
        arg(158, K, L),
        gen159(A, D, G, I, K, F, C, M, N).
gen159(A, B, C, D, E, F, G, H, I) :-		% by hand
        arg(159, B, 3),
        arg(159, C, 3),
        arg(159, D, 3),
        arg(159, E, 3),
        gen160(A, B, C, D, E, F, G, H, I).
gen160(A, D, G, I, L, N, C, F, K) :-
        arg(160, A, B),
        arg(B, C, E),
        arg(160, D, E),
        arg(B, F, H),
        arg(160, G, H),
        arg(B, C, J),
        arg(160, I, J),
        arg(B, K, M),
        arg(160, L, M),
        gen161(A, D, G, I, L, N, C, F, K).
gen161(A, D, G, I, L, N, C, K, F) :-
        arg(161, A, B),
        arg(B, C, E),
        arg(161, D, E),
        arg(B, F, H),
        arg(161, G, H),
        arg(B, C, J),
        arg(161, I, J),
        arg(B, K, M),
        arg(161, L, M),
        gen162(A, D, G, I, L, N, C, K, F).
gen162(A, D, F, G, I, J, C, K, L) :-
        arg(162, A, B),
        arg(B, C, E),
        arg(162, D, E),
        arg(162, F, B),
        arg(B, C, H),
        arg(162, G, H),
        arg(162, I, B),
        gen163(A, D, F, G, I, J, C, K, L).
gen163(A, D, F, H, J, K, L, C, G) :-
        arg(163, A, B),
        arg(B, C, E),
        arg(163, D, E),
        arg(163, F, 1),
        arg(B, G, I),
        arg(163, H, I),
        arg(163, J, 1),
        gen164(A, D, F, H, J, K, L, C, G).
gen164(A, D, G, J, L, F, N, C, I) :-
        arg(164, A, B),
        arg(B, C, E),
        arg(164, D, E),
        arg(B, F, H),
        arg(164, G, H),
        arg(B, I, K),
        arg(164, J, K),
        arg(B, F, M),
        arg(164, L, M),
        gen165(A, D, G, J, L, F, N, C, I).
gen165(A, D, G, J, L, N, F, C, I) :-
        arg(165, A, B),
        arg(B, C, E),
        arg(165, D, E),
        arg(B, F, H),
        arg(165, G, H),
        arg(B, I, K),
        arg(165, J, K),
        arg(B, F, M),
        arg(165, L, M),
        gen166(A, D, G, J, L, N, F, C, I).
gen166(A, D, F, I, K, M, N, C, H) :-
        arg(166, A, B),
        arg(B, C, E),
        arg(166, D, E),
        arg(B, C, G),
        arg(166, F, G),
        arg(B, H, J),
        arg(166, I, J),
        arg(B, H, L),
        arg(166, K, L),
        gen167(A, D, F, I, K, M, N, C, H).
gen167(A, D, G, I, K, M, N, C, F) :-
        arg(167, A, B),
        arg(B, C, E),
        arg(167, D, E),
        arg(B, F, H),
        arg(167, G, H),
        arg(B, F, J),
        arg(167, I, J),
        arg(B, C, L),
        arg(167, K, L),
        gen168(A, D, G, I, K, M, N, C, F).
gen168(A, D, F, H, J, K, L, C, G) :-
        arg(168, A, B),
        arg(B, C, E),
        arg(168, D, E),
        arg(168, F, B),
        arg(B, G, I),
        arg(168, H, I),
        arg(168, J, B),
        gen169(A, D, F, H, J, K, L, C, G).
gen169(A, D, F, H, J, K, L, G, C) :-
        arg(169, A, B),
        arg(B, C, E),
        arg(169, D, E),
        arg(169, F, 1),
        arg(B, G, I),
        arg(169, H, I),
        arg(169, J, 1),
        gen170(A, D, F, H, J, K, L, G, C).
gen170(A, D, G, J, L, F, N, I, C) :-
        arg(170, A, B),
        arg(B, C, E),
        arg(170, D, E),
        arg(B, F, H),
        arg(170, G, H),
        arg(B, I, K),
        arg(170, J, K),
        arg(B, F, M),
        arg(170, L, M),
        gen171(A, D, G, J, L, F, N, I, C).
gen171(A, D, G, J, L, N, F, I, C) :-
        arg(171, A, B),
        arg(B, C, E),
        arg(171, D, E),
        arg(B, F, H),
        arg(171, G, H),
        arg(B, I, K),
        arg(171, J, K),
        arg(B, F, M),
        arg(171, L, M),
        gen172(A, D, G, J, L, N, F, I, C).
gen172(A, D, G, I, K, M, N, F, C) :-
        arg(172, A, B),
        arg(B, C, E),
        arg(172, D, E),
        arg(B, F, H),
        arg(172, G, H),
        arg(B, F, J),
        arg(172, I, J),
        arg(B, C, L),
        arg(172, K, L),
        gen173(A, D, G, I, K, M, N, F, C).
gen173(A, D, F, I, K, M, N, H, C) :-
        arg(173, A, B),
        arg(B, C, E),
        arg(173, D, E),
        arg(B, C, G),
        arg(173, F, G),
        arg(B, H, J),
        arg(173, I, J),
        arg(B, H, L),
        arg(173, K, L),
        gen174(A, D, F, I, K, M, N, H, C).
gen174(A, D, F, H, J, K, L, G, C) :-
        arg(174, A, B),
        arg(B, C, E),
        arg(174, D, E),
        arg(174, F, B),
        arg(B, G, I),
        arg(174, H, I),
        arg(174, J, B),
        gen175(A, D, F, H, J, K, L, G, C).
gen175(A, B, D, E, F, G, H, I, J) :-
        arg(175, A, C),
        arg(175, B, C),
        arg(175, D, 1),
        arg(175, E, C),
        arg(175, F, 1),
        gen176(A, B, D, E, F, G, H, I, J).
gen176(A, B, E, G, H, D, J, K, L) :-
        arg(176, A, C),
        arg(176, B, C),
        arg(C, D, F),
        arg(176, E, F),
        arg(176, G, C),
        arg(C, D, I),
        arg(176, H, I),
        gen177(A, B, E, G, H, D, J, K, L).
gen177(A, B, E, G, H, J, D, K, L) :-
        arg(177, A, C),
        arg(177, B, C),
        arg(C, D, F),
        arg(177, E, F),
        arg(177, G, C),
        arg(C, D, I),
        arg(177, H, I),
        gen178(A, B, E, G, H, J, D, K, L).
gen178(A, B, E, G, I, K, L, D, H) :-
        arg(178, A, C),
        arg(178, B, C),
        arg(C, D, F),
        arg(178, E, F),
        arg(178, G, C),
        arg(C, H, J),
        arg(178, I, J),
        gen179(A, B, E, G, I, K, L, D, H).
gen179(A, B, E, G, I, K, L, H, D) :-
        arg(179, A, C),
        arg(179, B, C),
        arg(C, D, F),
        arg(179, E, F),
        arg(179, G, C),
        arg(C, H, J),
        arg(179, I, J),
        gen180(A, B, E, G, I, K, L, H, D).
gen180(A, B, D, E, F, G, H, I, J) :-		% by hand
        arg(180, B, 6),
        arg(180, D, 6),
        arg(180, E, 6),
        arg(180, F, 6),
        gen181(A, B, D, E, F, G, H, I, J).
gen181(A, B, C, D, E, F, G, H, I) :-
        arg(181, B, 1),
        arg(181, C, 1),
        arg(181, D, 1),
        arg(181, E, 1),
        gen182(A, B, C, D, E, F, G, H, I).
gen182(A, B, E, G, H, D, J, K, L) :-
        arg(182, A, C),
        arg(182, B, 1),
        arg(C, D, F),
        arg(182, E, F),
        arg(182, G, 1),
        arg(C, D, I),
        arg(182, H, I),
        gen183(A, B, E, G, H, D, J, K, L).
gen183(A, B, E, G, H, J, D, K, L) :-
        arg(183, A, C),
        arg(183, B, 1),
        arg(C, D, F),
        arg(183, E, F),
        arg(183, G, 1),
        arg(C, D, I),
        arg(183, H, I),
        gen184(A, B, E, G, H, J, D, K, L).
gen184(A, B, E, G, I, K, L, D, H) :-
        arg(184, A, C),
        arg(184, B, 1),
        arg(C, D, F),
        arg(184, E, F),
        arg(184, G, 1),
        arg(C, H, J),
        arg(184, I, J),
        gen185(A, B, E, G, I, K, L, D, H).
gen185(A, B, E, G, I, K, L, H, D) :-
        arg(185, A, C),
        arg(185, B, 1),
        arg(C, D, F),
        arg(185, E, F),
        arg(185, G, 1),
        arg(C, H, J),
        arg(185, I, J),
        gen186(A, B, E, G, I, K, L, H, D).
gen186(A, B, C, E, F, G, H, I, J) :-
        arg(186, A, D),
        arg(186, B, 1),
        arg(186, C, D),
        arg(186, E, 1),
        arg(186, F, D),
        gen187(A, B, C, E, F, G, H, I, J).
gen187(A, D, F, G, I, C, J, K, L) :-
        arg(187, A, B),
        arg(B, C, E),
        arg(187, D, E),
        arg(187, F, 1),
        arg(B, C, H),
        arg(187, G, H),
        arg(187, I, 1),
        gen188(A, D, F, G, I, C, J, K, L).
gen188(A, B, C, D, E, F, G, H, I) :-		% by hand
        arg(188, B, 2),
        arg(188, C, 2),
        arg(188, D, 2),
        arg(188, E, 2),
        gen189(A, B, C, D, E, F, G, H, I).
gen189(A, D, G, I, K, C, F, M, N) :-
        arg(189, A, B),
        arg(B, C, E),
        arg(189, D, E),
        arg(B, F, H),
        arg(189, G, H),
        arg(B, C, J),
        arg(189, I, J),
        arg(B, F, L),
        arg(189, K, L),
        gen190(A, D, G, I, K, C, F, M, N).
gen190(A, D, G, I, L, C, N, F, K) :-
        arg(190, A, B),
        arg(B, C, E),
        arg(190, D, E),
        arg(B, F, H),
        arg(190, G, H),
        arg(B, C, J),
        arg(190, I, J),
        arg(B, K, M),
        arg(190, L, M),
        gen191(A, D, G, I, L, C, N, F, K).
gen191(A, D, G, I, L, C, N, K, F) :-
        arg(191, A, B),
        arg(B, C, E),
        arg(191, D, E),
        arg(B, F, H),
        arg(191, G, H),
        arg(B, C, J),
        arg(191, I, J),
        arg(B, K, M),
        arg(191, L, M),
        gen192(A, D, G, I, L, C, N, K, F).
gen192(A, D, F, G, I, C, J, K, L) :-
        arg(192, A, B),
        arg(B, C, E),
        arg(192, D, E),
        arg(192, F, B),
        arg(B, C, H),
        arg(192, G, H),
        arg(192, I, B),
        gen193(A, D, F, G, I, C, J, K, L).
gen193(A, D, F, G, I, J, C, K, L) :-
        arg(193, A, B),
        arg(B, C, E),
        arg(193, D, E),
        arg(193, F, 1),
        arg(B, C, H),
        arg(193, G, H),
        arg(193, I, 1),
        gen194(A, D, F, G, I, J, C, K, L).
gen194(A, D, G, I, K, F, C, M, N) :-
        arg(194, A, B),
        arg(B, C, E),
        arg(194, D, E),
        arg(B, F, H),
        arg(194, G, H),
        arg(B, C, J),
        arg(194, I, J),
        arg(B, F, L),
        arg(194, K, L),
        gen195(A, D, G, I, K, F, C, M, N).
gen195(A, B, C, D, E, F, G, H, I) :-		% by hand
        arg(195, B, 3),
        arg(195, C, 3),
        arg(195, D, 3),
        arg(195, E, 3),
        gen196(A, B, C, D, E, F, G, H, I).
gen196(A, D, G, I, L, N, C, F, K) :-
        arg(196, A, B),
        arg(B, C, E),
        arg(196, D, E),
        arg(B, F, H),
        arg(196, G, H),
        arg(B, C, J),
        arg(196, I, J),
        arg(B, K, M),
        arg(196, L, M),
        gen197(A, D, G, I, L, N, C, F, K).
gen197(A, D, G, I, L, N, C, K, F) :-
        arg(197, A, B),
        arg(B, C, E),
        arg(197, D, E),
        arg(B, F, H),
        arg(197, G, H),
        arg(B, C, J),
        arg(197, I, J),
        arg(B, K, M),
        arg(197, L, M),
        gen198(A, D, G, I, L, N, C, K, F).
gen198(A, D, F, G, I, J, C, K, L) :-
        arg(198, A, B),
        arg(B, C, E),
        arg(198, D, E),
        arg(198, F, B),
        arg(B, C, H),
        arg(198, G, H),
        arg(198, I, B),
        gen199(A, D, F, G, I, J, C, K, L).
gen199(A, D, F, H, J, K, L, C, G) :-
        arg(199, A, B),
        arg(B, C, E),
        arg(199, D, E),
        arg(199, F, 1),
        arg(B, G, I),
        arg(199, H, I),
        arg(199, J, 1),
        gen200(A, D, F, H, J, K, L, C, G).
gen200(A, D, G, J, L, F, N, C, I) :-
        arg(200, A, B),
        arg(B, C, E),
        arg(200, D, E),
        arg(B, F, H),
        arg(200, G, H),
        arg(B, I, K),
        arg(200, J, K),
        arg(B, F, M),
        arg(200, L, M),
        gen201(A, D, G, J, L, F, N, C, I).
gen201(A, D, G, J, L, N, F, C, I) :-
        arg(201, A, B),
        arg(B, C, E),
        arg(201, D, E),
        arg(B, F, H),
        arg(201, G, H),
        arg(B, I, K),
        arg(201, J, K),
        arg(B, F, M),
        arg(201, L, M),
        gen202(A, D, G, J, L, N, F, C, I).
gen202(A, D, F, I, K, M, N, C, H) :-
        arg(202, A, B),
        arg(B, C, E),
        arg(202, D, E),
        arg(B, C, G),
        arg(202, F, G),
        arg(B, H, J),
        arg(202, I, J),
        arg(B, H, L),
        arg(202, K, L),
        gen203(A, D, F, I, K, M, N, C, H).
gen203(A, D, G, I, K, M, N, C, F) :-
        arg(203, A, B),
        arg(B, C, E),
        arg(203, D, E),
        arg(B, F, H),
        arg(203, G, H),
        arg(B, F, J),
        arg(203, I, J),
        arg(B, C, L),
        arg(203, K, L),
        gen204(A, D, G, I, K, M, N, C, F).
gen204(A, D, F, H, J, K, L, C, G) :-
        arg(204, A, B),
        arg(B, C, E),
        arg(204, D, E),
        arg(204, F, B),
        arg(B, G, I),
        arg(204, H, I),
        arg(204, J, B),
        gen205(A, D, F, H, J, K, L, C, G).
gen205(A, D, F, H, J, K, L, G, C) :-
        arg(205, A, B),
        arg(B, C, E),
        arg(205, D, E),
        arg(205, F, 1),
        arg(B, G, I),
        arg(205, H, I),
        arg(205, J, 1),
        gen206(A, D, F, H, J, K, L, G, C).
gen206(A, D, G, J, L, F, N, I, C) :-
        arg(206, A, B),
        arg(B, C, E),
        arg(206, D, E),
        arg(B, F, H),
        arg(206, G, H),
        arg(B, I, K),
        arg(206, J, K),
        arg(B, F, M),
        arg(206, L, M),
        gen207(A, D, G, J, L, F, N, I, C).
gen207(A, D, G, J, L, N, F, I, C) :-
        arg(207, A, B),
        arg(B, C, E),
        arg(207, D, E),
        arg(B, F, H),
        arg(207, G, H),
        arg(B, I, K),
        arg(207, J, K),
        arg(B, F, M),
        arg(207, L, M),
        gen208(A, D, G, J, L, N, F, I, C).
gen208(A, D, G, I, K, M, N, F, C) :-
        arg(208, A, B),
        arg(B, C, E),
        arg(208, D, E),
        arg(B, F, H),
        arg(208, G, H),
        arg(B, F, J),
        arg(208, I, J),
        arg(B, C, L),
        arg(208, K, L),
        gen209(A, D, G, I, K, M, N, F, C).
gen209(A, D, F, I, K, M, N, H, C) :-
        arg(209, A, B),
        arg(B, C, E),
        arg(209, D, E),
        arg(B, C, G),
        arg(209, F, G),
        arg(B, H, J),
        arg(209, I, J),
        arg(B, H, L),
        arg(209, K, L),
        gen210(A, D, F, I, K, M, N, H, C).
gen210(A, D, F, H, J, K, L, G, C) :-
        arg(210, A, B),
        arg(B, C, E),
        arg(210, D, E),
        arg(210, F, B),
        arg(B, G, I),
        arg(210, H, I),
        arg(210, J, B),
        gen211(A, D, F, H, J, K, L, G, C).
gen211(A, B, D, E, F, G, H, I, J) :-
        arg(211, A, C),
        arg(211, B, C),
        arg(211, D, 1),
        arg(211, E, C),
        arg(211, F, 1),
        gen212(A, B, D, E, F, G, H, I, J).
gen212(A, B, E, G, H, D, J, K, L) :-
        arg(212, A, C),
        arg(212, B, C),
        arg(C, D, F),
        arg(212, E, F),
        arg(212, G, C),
        arg(C, D, I),
        arg(212, H, I),
        gen213(A, B, E, G, H, D, J, K, L).
gen213(A, B, E, G, H, J, D, K, L) :-
        arg(213, A, C),
        arg(213, B, C),
        arg(C, D, F),
        arg(213, E, F),
        arg(213, G, C),
        arg(C, D, I),
        arg(213, H, I),
        gen214(A, B, E, G, H, J, D, K, L).
gen214(A, B, E, G, I, K, L, D, H) :-
        arg(214, A, C),
        arg(214, B, C),
        arg(C, D, F),
        arg(214, E, F),
        arg(214, G, C),
        arg(C, H, J),
        arg(214, I, J),
        gen215(A, B, E, G, I, K, L, D, H).
gen215(A, B, E, G, I, K, L, H, D) :-
        arg(215, A, C),
        arg(215, B, C),
        arg(C, D, F),
        arg(215, E, F),
        arg(215, G, C),
        arg(C, H, J),
        arg(215, I, J),
        gen216(A, B, E, G, I, K, L, H, D).
gen216(_, B, D, E, F, _, _, _, _) :-		% by hand
        arg(216, B, 6),
        arg(216, D, 6),
        arg(216, E, 6),
        arg(216, F, 6).


