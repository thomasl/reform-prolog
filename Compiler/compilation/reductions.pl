%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	 REDUCTIONS, INDUCTIONS, SCANS AND INTEGER RECURSION
%
% This file splits a recursion parallel predicate into its
% 'indexing structure', which is:
%
% rp(Base,Rec)
%
% and Rec being H :- rec(LP,LB,R,RB,RP)
% where LP and RP are the left and right prologues (of red'ns)
%       LB and RB the left and right bodies
%       H the head and R the recursive call.
%
% A call gets put in the left part if it is to the left of the
% recursive call, and in the right part if to the right.
%
% If it is a reduction (etc.) it is put in LP or RP.
%
% The compiler recognizes:
%  - inductions "X is Y +/- C"
%  - reductions "X is Y +/- E"
%
% Integer recursion is recognized as an induction in the first argument
% plus mutually exclusive tests in base and recursive clause, with the test
% in the recursive clause being 'of the right direction'.
%
% Scans are found as reductions where the computed result occurs in
% the body (this is either X or Y in the formulation above).
%
% An induction refers to variables X and Y and a constant C.
%
% A reduction can only refer to X and Y in the same none-neg pair
% and an arbitrary expression E where free variables must be poslist-heads
% or invariants.

:- ensure_loaded('../util/basics').
:- ensure_loaded('../util/sets').
:- ensure_loaded('../util/error').
:- ensure_loaded('../preprocess/expander').
:- ensure_loaded('../preprocess/arithmetic').

:- ensure_loaded('rp-rewrite.pl').


% All arithmetic is replaced with '$cancel'(Dead,Goal) in the bodies
% and (Dead,Goal) pair in the prologues. If the goal turns out to
% be part of a reduction, Dead is bound to 'dead' and the $cancel goal
% disappears. Otherwise, the $cancel goal is simply Goal.

test_rp_prep(Base,Rec,IxStruct,Spawns) :-
	preprocess_rp_pred(Base,Rec,IxStruct,Spawns).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% preprocess_rp_pred(Base,Rec,IxStruct,Spawns)
%
% This procedure performs the necessary preprocessing for recursion
% parallel predicates. It detects reductions etc. and annotates
% the predicate as list or integer recursive. Also, the new predicates
% spawned by syntactic sugars are returned as a list of clauses Spawns.
%
% The indexing structure returned is written
%
%    rp(BaseClause,RecursiveClause,TypeOfRecursion,Base,Rec)
%
% Note that it presumes only two clauses (or at least two classes
% of clauses). The original clauses are preserved for analysis.
% *** UNFINISHED ***
% Scan operations are not found
% Arithmetic should be expanded in left/right preludes.

preprocess_rp_pred(Base,Rec,Ix,Spawns) :-
	preprocess_rp_clause(Rec,RecCls,Spawns,Rest),
	detect_recursion_type(Base,RecCls,NewRecCls,Type),
	expand_clause(Base,BaseCls,Rest,[]),
	recode_rec_clause(NewRecCls,RecodedClause),
	extract_body_clauses(RecodedClause,FinalRecCls,LeftBodyClause,RightBodyClause),
	Ix = rp(BaseCls,FinalRecCls,Rec,Type,LeftBodyClause,RightBodyClause).

%

find_rp_body(P/N,Side,CDB,Pred) :-
	lookup(P/N,CDB,Ix),
	Ix = rp(_,_,_,Left,Right),
	( Side = left ->
	  Pred = Left
	; Side = right ->
	  Pred = Right
	).

% For testing purposes: rewrite_reductions is defined as null.
%
% rewrite_reductions(X,Y) :- !, X = Y.

rewrite_reductions((H1 :- rec(LP0,LB0,R,RB0,RP0)),
	           (H1 :- rec(LP,LB,R,RB,RP))) :-
        induction_vars(LP0,LeftIV,[]),
        rewrite_red_body(LP0,LB0,LeftIV,LP,LB),
	induction_vars(RP0,RightIV,LeftIV),
	rewrite_red_body(RP0,RB0,RightIV,RP,RB).

% To simplify analysis, we break out left and right bodies as
% separate predicates/clauses

extract_body_clauses((H :- rec(LP,LB,   R,RB,   RP)),
	             (H :- rec(LP,NewLB,R,NewRB,RP)),
		     LeftC,RightC) :-
        functor(H,P,N),
        vars_find((H,R,LP,RP),Vs1,[]),
	vars_find(LB,VsLB,Vs1), sort(VsLB,OutsideRB),   % vars occuring outside LB
	vars_find(RB,VsRB,Vs1), sort(VsRB,OutsideLB),   % vars occuring outside RB
	extract_body_clause(LB,P,N,left_body,OutsideLB,LeftC,NewLB),
	extract_body_clause(RB,P,N,right_body,OutsideRB,RightC,NewRB).

%


extract_body_clause(none,_,_,_,_,NoCls,None) :- !,
	NoCls = no_clause, None = none.

extract_body_clause(Body,P,N,BodyID,OutsideVars,Clause,NewBody) :-
	vars(Body,Vs),
	intersect(Vs,OutsideVars,InterfaceVars),
	remove_duplicates(InterfaceVars,Interface),
	build_name([P,'_',N,'_',BodyID],NewName),
	NewH =.. [NewName|Interface],
	copy_term((NewH :- Body),Clause), % so that no var. sharing problems
	NewBody = NewH.


build_name(List,Name) :-
	build_ascii_list(List,Ascii),
	name(Name,Ascii).


build_ascii_list([],[]).

build_ascii_list([X|Xs],AsciiList) :-
	build_ascii_list(Xs,Tail),
	name(X,AsciiName),
	append(AsciiName,Tail,AsciiList).

%%%
%%  Remove duplicates and preserve left to right order.
%
remove_duplicates(    [],    []).

remove_duplicates([X|Xs],[X|Ys]) :-
	strict_remove(Xs,X,Rest),
	remove_duplicates(Rest,Ys).


strict_remove(    [],_,  []).

strict_remove([Y|Ys],X,List) :-
	( Y == X ->
	    List = Rest
	; true,
	    List = [Y|Rest]
	),
	strict_remove(Ys,X,Rest).


% Induction_vars collects the induction variables.
%
% All induction variables are associated with a value, which is how
% they are regenerated from the level counter. If the induction vars
% occur in the body, they are generated by arithmetic.
%
% *** UNFINISHED ***
% Here generate the 'closed form' of the inductions etc.

induction_vars([],IV,IV).

induction_vars([R|Rs],IV0,IV2) :-
	( R = '$ind'(Intrec,X,Y,Op,C) ->
	  ( var(Intrec) -> % see (*) above
	    Expr =.. [Op,Y,C],
	    IV0 = [(X,Expr),(Y,level*C)|IV1]
	  ; IV0 = IV1
	  )
	; R = '$intrec'('$ind'(_,X,Y,Op,C),_Test) ->
	  Expr =.. [Op,Y,C],
	  IV0 = [(X,Expr),(Y,Y*C)|IV1]
	; R = '$red'(_,_X,_Y,_E) ->
	  IV0 = IV1
	),
	induction_vars(Rs,IV1,IV2).

% Rewrite_red_body rewrites both the reductions and the bodies.
% In the body, each occurence of an induction variable becomes
% an arithmetic sequence by manipulating the level counter.
% Also, reduction variables found in the body turn the corresponding
% reduction into a scan.

%

mark_scans([],_) --> [].

mark_scans([R|Rs],Vs) -->
	( { R = '$red'(_Scan,X,Y,E) } ->
	  ( { soft_member(X,Vs) ; soft_member(Y,Vs) } ->
	    ['$scan'(X,Y,E)]
	  ; ['$red'(X,Y,E)]
	  )
	; [R]
	),
	mark_scans(Rs,Vs).

%

compute_inductions((G,Gs),IV) --> !,
	compute_inductions_of_goal(G,IV),
	compute_inductions(Gs,IV).

compute_inductions(G,IV) -->
	compute_inductions_of_goal(G,IV).

%

compute_inductions_of_goal(G,IV) -->
	{ G =.. [P|Xs] },
	compute_inductions_of_terms(Xs,IV,Ys),
	{ H =.. [P|Ys] },
	[H].

%

compute_inductions_of_terms([],_,[]) --> [].

compute_inductions_of_terms([X|Xs],IV,[Y|Ys]) -->
	compute_inductions_of_term(X,IV,Y),
	compute_inductions_of_terms(Xs,IV,Ys).

%

compute_inductions_of_term(X,_IV,Y) -->
	{ atomic(X),!, Y = X }.

compute_inductions_of_term(X,IV,Y) -->
	{ var(X), ! },
	( { induction_var(IV,X,InductionExpr) } ->
	  induction_expression(InductionExpr,R), { Y = R }
	; { Y = X }
	).

compute_inductions_of_term(X,IV,Y) -->
	{ X =.. [P|Xs] },
	compute_inductions_of_terms(Xs,IV,Ys),
	{ Y =.. [P|Ys] }.

%

induction_var([(X,E)|Xs],Y,F) :-
	( X == Y ->
	  E = F
	; induction_var(Xs,Y,F)
	).

% *** UNFINISHED ***

induction_expression(level*C,R) --> !,
	['$get_level'(L), R is C*L ].

induction_expression(_X,_R) --> !,[nyi(induction_expression/2)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Replace induction variables with (a) a computed variable if available,
% or (b) an expression that computes the value.
%
% Note that integer recursions are also considered inductions when
% they occur in the body.
%
% An induction "X is Y+C" is computed as:
%   * where X occurs
%       $get_level(L), Tmp is C*L+ Y0 (Y0 original argument Y)
%   * where Y occurs
%       $get_level(L), Tmp is C*(L-1) + Y0 (again, Y0 orig arg)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Detecting the type of the recursion.
%
% List recursion is simple: the first argument pair must
% be ( [X1,...,Xk|Xs],Xs )
%
% The trouble child is of course integer recursion.
% We have six cases to consider:
% - It is an induction X is Y+C with a test X < T, X =< T, X = T
% - It is an induction X is Y-C with a test X > T, X >= T, X = T
%
% All others are rejected. X and Y are variables, C is an integer and
% T is either an integer or an INV variable.
%
% After integer recursion has been found, code is generated for the
% integer recursion, inductions and reductions. For reductions, an
% occurence of either X or Y in the body means the reduction becomes
% a scan operation which is marked by instantiating a variable in
% the reduction call. References to induction variables are replaced
% with arithmetic and references to the level counter.
%
% We introduce new primitives to access the size register and the
% level register:
%
% $set_size(N)      size register is set to N
% $get_level(L)     register L is set to level register
% $set_reg(R,N)     register R is set to variable N (for rec calls)
%
% *** UNFINISHED ***
% Actual compilation step not yet done.

detect_recursion_type((H0 :- B0),(H1 :- rec(LP,LB,R,RB,RP)),NewR,Type) :-
	H0 =.. [_,Base|BaseXs],
	H1 =.. [_,Head|HeadXs],
	R  =.. [_,Rec|RecXs],
	( list_recursion(Base,Head,Rec) ->
	  NewR = (H1 :- rec(LP,LB,R,RB,RP)),
	  Type = listrec
	; find_inv_vars(HeadXs,RecXs,2,Invs),
	  integer_recursion(Head,Rec,Invs,Base,BaseXs,B0,LP,LB,NewLB,Ind),!,
	  Type = intrec,
	  NewR = (H1 :- rec([Ind|LP],NewLB,R,RB,RP))
	; functor(H0,P,N),
	  error('Unable to recognize recursion in predicate ~q',[P/N])
	).

%

list_recursion([],Head,Rec) :-
	list_with_tail(Head,Rec).

list_with_tail(X,Y) :- var(X),!,X==Y.

list_with_tail([_|Xs],Y) :- list_with_tail(Xs,Y).

%

find_inv_vars([],[],_,[]).

find_inv_vars([X|Xs],[Y|Ys],Pos,Zs) :-
	( X == Y ->
	  Zs = [(X,Pos)|Zs0]
	; Zs = Zs0
	),
	NewPos is Pos+1,
	find_inv_vars(Xs,Ys,NewPos,Zs0).

%

integer_recursion(Head,Rec,Invs,Base,BaseXs,BaseBody,LP,LB,NewLB,Ind) :-
	var(Head), var(Rec),
	( LB = ('$choice'(X),Test,Rest) -> 
	  NewLB = ('$choice'(X),Rest)
	; LB = ('$choice'(X),Test) ->
	  NewLB = '$choice'(X)
	; LB = (Test,Rest),
	  NewLB = Rest
	),
	find_integer_recursion(LP,Head,Rec,Induction,Op),
	( Op = '+' ->
	  upward_test_expr(Test,Head,
	                   Invs,TestExpr,TestsArg,TestsConst)
	; Op = '-' ->
	  downward_test_expr(Test,Head,
	                     Invs,TestExpr,TestsArg,TestsConst)
	),
	check_base_case(TestsArg,TestsConst,TestExpr,Base,BaseXs,BaseBody),
	Ind = '$intrec'(Induction,TestExpr).

%

find_integer_recursion([IND|Xs],H,R,Ind,Op) :-
	( IND = '$ind'(IntRec,X,Y,Op,_C), R == X, H == Y ->
	  Ind = IND, IntRec = intrec
	; find_integer_recursion(Xs,H,R,Ind,Op)
	).

%

upward_test_expr(X<E,H,Invs,TestExpr,TestArgs,TestsConst) :-
	X == H,
	( var(E), inv_variable_and_position(E,Invs,Pos) ->
	  TestArgs = Pos, TestExpr = (X<E)
	; integer(E) ->
	  TestsConst = E, TestExpr = (X<E)
	).
upward_test_expr(X=<E,H,Invs,TestExpr,TestArgs,TestsConst) :-
	X == H,
	( var(E), inv_variable_and_position(E,Invs,Pos) ->
	  TestArgs = Pos, TestExpr = (X<E+1)
	; integer(E) ->
	  TestsConst = E, TestExpr = (X<E+1)
	).
upward_test_expr(X=:=E,H,Invs,TestExpr,TestArgs,TestsConst) :-
	X == H,
	( var(E), inv_variable_and_position(E,Invs,Pos) ->
	  TestArgs = Pos, TestExpr = (0 is X mod E, X<E+1)
	; integer(E) ->
	  TestsConst = E, TestExpr = (0 is X mod E, X<E+1)
	).

%

downward_test_expr(X>E,H,Invs,TestExpr,TestArgs,TestsConst) :-
	X == H,
	( var(E), inv_variable_and_position(E,Invs,Pos) ->
	  TestArgs = Pos, TestExpr = (X>E)
	; integer(E) ->
	  TestsConst = E, TestExpr = (X>E)
	).
downward_test_expr(X>=E,H,Invs,TestExpr,TestArgs,TestsConst) :-
	X == H,
	( var(E), inv_variable_and_position(E,Invs,Pos) ->
	  TestArgs = Pos, TestExpr = (X>E-1)
	; integer(E) ->
	  TestsConst = E, TestExpr = (X>E-1)
	).
downward_test_expr(X=:=E,H,Invs,TestExpr,TestArgs,TestsConst) :-
	X == H,
	( var(E), inv_variable_and_position(E,Invs,Pos) ->
	  TestArgs = Pos, TestExpr = (0 is X mod E, X>E-1)
	; integer(E) ->
	  TestsConst = E, TestExpr = (0 is X mod E, X>E-1)
	).

%

inv_variable_and_position(Y,Invs,Pos) :-
	inv_and_pos(Invs,Y,Pos).

inv_and_pos([(X,Pos)|Xs],Y,Posn) :-
	( X == Y ->
	  Posn = Pos
	; inv_and_pos(Xs,Y,Posn)
	).

% We assume base case is OK ... (i.e. mutually exclusive w. rec clause)

check_base_case(Arg,Const,_Test,Base,_BaseXs,_BaseBody) :-
	integer(Arg), var(Const),
	( var(Base) -> true
	; integer(Base)
	),!.

check_base_case(Arg,Const,_Test,Base,_BaseXs,_BaseBody) :-
	var(Arg), integer(Const),
	( var(Base) -> true
	; integer(Base)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% preprocess_rp_clause((H :- B,Env),(H :- rec(LP,LB,R,RB,RP),Env))
%
% This is done for the recursive clause only.
% Afterwards, we look at both this clause and the base case to
% discern integer recursion and scans.
%
% Env is invariant.

preprocess_rp_clause(RecCls,(NewH :- rec(LP,LB,R,RB,RP)),Spawns,RestSpawns) :-
	copy_term(RecCls,RecCpy),
	expand_clause(RecCpy,(H :- B),intact_arithmetic,Spawns,RestSpawns),
	functor(H,P,N),
	split_body(B,P,N,LP0,LB0,R,RB0,RP0),
	rp_rewrite(H,R,LB0,NewH,NewR,NewLB),
	find_recurrences(NewH,NewR,LP0,RP0,LP,RP),
	remove_cancelled_goals(NewLB,LB),
	remove_cancelled_goals(RB0,RB).

%

remove_cancelled_goals((G,Gs), H) :- !,
	remove_cancelled_goals(Gs, Hs),
	remove_cancelled_goal(G, Hs, H).

remove_cancelled_goals(G, H) :-
	remove_cancelled_goal(G, true, H).


remove_cancelled_goal('$cancel'(Dead,G),Hs,H) :- !,
	(var(Dead) ->
	    expand_arithmetic_goal(G,Expanded,[Hdl]),
	    list_to_conj(Expanded,H),
	    Hdl = Hs
	; H = Hs).

remove_cancelled_goal(none, _Hs, H) :- !, H = none.

remove_cancelled_goal(G,Hs,H) :- H = (G,Hs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% split_body partitions the body into the five parts that
% are required. LP and RP are lists of pairs (Dead,Goal) while
% LB and RB are conjunctions (unit true) and R is the rec. call.

split_body(Body,P,N,LP,LB,R,RB,RP) :-
	partition_left_body(Body,P,N,LB0,R,Rest,LP,[]),
	empty_body(LB0,LB),
	partition_right_body(Rest,RB0,RP,[]),
	empty_body(RB0,RB).

%

empty_body(true,X) :- !, X = none.
empty_body(X,X).

%

partition_left_body((G,Gs),P,N,LB,R,Rest) --> !,
	( { functor(G,P,N) } ->
	  { LB = true, Rest = Gs, R = G }
	; { G = (_ is _) } ->
	  [(Dead,G)],
	  { LB = ('$cancel'(Dead,G),LB0) },
	  partition_left_body(Gs,P,N,LB0,R,Rest)
	; { LB = (G,LB0) },
	  partition_left_body(Gs,P,N,LB0,R,Rest)
	).

partition_left_body(G,P,N,LB,R,Rest) -->
	( { functor(G,P,N) } ->
	  { LB = true, Rest = true, R = G }
	; { sys_error('partition_left_body/8: unable to find recursive call') }
	).

%

partition_right_body((G,Gs),RB) --> !,
	( { G = (_ is _) } ->
	  { RB = ('$cancel'(Dead,G),RB0) },
	  [(Dead,G)]
	; { RB = (G,RB0) }
	),
	partition_right_body(Gs,RB0).

partition_right_body(G,RB) -->
	( { G = (_ is _) } ->
	  { RB = '$cancel'(Dead,G) },
	  [(Dead,G)]
	; { RB = G }
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Locate reductions and inductions. After this
% call, scans and integer recursion has not been found.
% Arithmetic has been properly cancelled.
%
% *** UNFINISHED ***
% Should locate integer recursion in particular.

find_recurrences(H,R,LP0,RP0,LP,RP) :-
	H =.. [_,X|Xs], R =.. [_,Y|Ys],
%	classify_first_arg(X,Y,B,RecType)
	classify_vars_redn([X|Xs],[Y|Ys],Pos,[],Inv,[],NoneNeg,[]),
	find_red(LP0,Pos,Inv,NoneNeg,LP,[]),
	find_red(RP0,Pos,Inv,NoneNeg,RP,[]).

%

classify_vars_redn([],[],Pos,Pos,Inv,Inv,NoneNeg,NoneNeg).

classify_vars_redn([X|Xs],[Y|Ys],Pos,Pos0,Inv,Inv0,NoneNeg,NoneNeg0) :-
	( ( var(X), var(Y) ) ->
	  ( X == Y ->
	    Inv = [X|Inv1], Pos = Pos1, NoneNeg = NoneNeg1
	  ; Inv = Inv1, Pos = Pos1, NoneNeg = [(X,Y)|NoneNeg1]
	  )
	; ( list(X,Tail,Pos,Pos1), var(Y), Y == Tail ) ->
	  Inv = Inv1, NoneNeg = NoneNeg1
	; Inv = Inv1, Pos = Pos1, NoneNeg = NoneNeg1
	),
	classify_vars_redn(Xs,Ys,Pos1,Pos0,Inv1,Inv0,NoneNeg1,NoneNeg0).

% Note that in a list "[X,1,Y,2|Zs]" 1 and 2 would simply be thrown away.
% Later on, they should be broken out and replaced by vars _1 and _2
% with explicit unification _1 = 1 and _2 = 2 in the left body. Now, they
% are (as I said) forgotten.

list(X,Tail) --> { var(X),!, Tail = X }.

list([X|Xs],Tail) -->
	( { var(X) } ->
	  [X]
	; []
	),
	list(Xs,Tail).

%

find_red([],_Pos,_Inv,_NoneNeg) --> [].

find_red([A|As],Pos,Inv,NoneNeg) -->
	( arith_reduction(A,Pos,Inv,NoneNeg) ->
	  []
	; []
	),
	find_red(As,Pos,Inv,NoneNeg).

%

arith_reduction((Dead,X is E),Pos,Inv,NoneNeg) -->
	( { none_neg_pair(NoneNeg,X,Y) } ->
	  ( { induction(E,Y,Op,C) } ->
	    { Dead = dead },
	    ['$ind'(_,X,Y,Op,C)]
	  ; { reduction(E,Y,Pos,Inv) } ->
	    { Dead = dead },
	    ['$red'(_,X,Y,E)]
	  )
	).

%

none_neg_pair([(X,Y)|Xs],X0,Y0) :-
	( X == X0 ->
	  Y = Y0
	; Y == X0 ->
	  X = Y0
	; none_neg_pair(Xs,X0,Y0)
	).

%

induction((A+B),Y,Op,C) :-
	integer(A), var(B), B == Y,!, C = A, Op = '+'.

induction((A+B),Y,Op,C) :-
	var(A), integer(B), A == Y,!, C = B, Op = '+'.

%	( integer(A) ->
%	  ( var(B) ->
%	    ( B == Y ->
%	      C = A, Op = '+'
%	    ; fail
%	    )
%	  ; fail
%	  )
%	; integer(B) ->
%	  ( var(A) ->
%	    ( A == Y ->
%	      C = B, Op = '+'
%	    ; fail
%	    )
%	  ; fail
%	  )
%	; fail
%	).
%
%	( ( integer(A), B == Y ) ->
%	  C = A, Op = '+'
%	; ( integer(B), A == Y ) ->
%	  C = B, Op = '+'
%	).

 % do not allow X is 2-Y but allow X is Y-2.

induction((A-B),Y,Op,C) :- 
	integer(B), var(A), A == Y,
	C = B, Op = '-'.

induction((A*B),Y,Op,C) :-
	integer(A), var(B), B == Y,!, C = A, Op = '*'.
induction((A*B),Y,Op,C) :-
	var(A), integer(B), A == Y,!, C = A, Op = '*'.

%induction((A*B),Y,Op,C) :-
%	( ( integer(A), B == Y ) ->
%	  C = A, Op = '*'
%	; ( integer(B), A == Y ) ->
%	  C = B, Op = '*'
%	).
%
% Reductions consist of operations involving X,
% vars in Pos and vars in Inv only. 
%
% delete from basics.pl
% diff from sets.pl

reduction(E,X,Pos,Inv) :-
	vars_find(E,Vs,[]),
	delete_from_list(Vs,X,Ws),
	diff(Ws,Pos,Us),
	diff(Us,Inv,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Compiling reductions, integer recursion and inductions into
% primitives.
%
% How to handle reductions: [NYI]
% ------------------------
% 1. A builtin creates a temporary vector of length = #workers
%    (initialized to an appropriate argument value)
% 2. On each iteration, the worker accumulates the result using
%    a standard arithmetic operation and stores the result
%    using '$accumulate_{+,*}'(N) which does
%     workvec[I] += N
% 3. A second sequential builtin after execution has completed then
%    computes the sum or product of the work vector.
%
% How to handle scans: [NYI]
% -------------------
% One way to manage a scan operation (i.e. a reduction where the
% intermediate results are used somehow) is to code it as a reduction
% with a special primitive to access the current accumulated value.
% Doing this entails: awaiting leftmostness, and computing the current
%  sum (product) for the accumulated value.
% It is perhaps simpler to code a scan as either a sequential
% primitive (yeilding a vector list result) or simply code it as
% arithmetic.
%
% How to handle integer recursion:
% -------------------------------
% 1. Convert to appropriately set size-register and recursive call
%    value.
% 2. Otherwise, treat as an induction in bodies.
%
% How to handle inductions:
% ------------------------
% 1. Induction X is Y+C is converted into
%    X = $ind_high(Y,C,Op)  [NB. Y inside $ind refers to call argument]
%    Y = $ind_low(Y,C,Op)   [ here as well ]
% 2. When they appear in the body, they are converted into
%    arithmetic vs the level counter on each recursion level.
%    (If $ind_low, Lvl-1)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Coding integer recursion:
%
% Reductions are not handled currently. Thus, only
% integer recursion is converted. We do the following:
% (1) Rewrite integer recursion + introduce as inductions
% (2) Rewrite left + right bodies using induction info
%
% Compiler must handle manipulation of size and level counters later on.

recode_rec_clause((H :- rec(LP,LB,R,RB,RP)),
	          (H :- rec(NewLP,NewLB,R,NewRB,NewRP))) :-
        recode_prologue(LP,NewLP,[],IndLP),
	replace_iv_left(LB,NewLB,[],IndLP,unavailable),
	recode_prologue(RP,NewRP,IndLP,Ind),
	replace_iv_right(RB,NewRB,[],Ind,unavailable).

%

recode_prologue([],true,Ind,Ind).

recode_prologue([X|Xs],NewPrologue,Ind0,Ind2) :-
	recode_recurrence(X,NewPrologue,RestPrologue,Ind0,Ind1),
	recode_prologue(Xs,RestPrologue,Ind1,Ind2).

% Integer recursion is encoded as:
% (a) Arithmetic + setting size reg, enclosed in 'markers'.
%     The markers serve to point out that these are actions to
%     be taken during or prior to head matching, essentially.
% (b) Otherwise, appears as an induction.
%
% Inductions are encoded as:
% (a) Code using the size register to set the value in rec. call
% (b) In the body, replaced by arithmetic vs level register (below)
%
% Reductions are encoded as:
%   Nothing, currently
%
% Scans are encoded as:
%   Nothing, currently.

recode_recurrence('$intrec'('$ind'(intrec,New,Old,Op,Incr),GreaterThan),
	          Prol,RestProl,Ind0,Ind1) :-
        ( size_expression_of(GreaterThan,New,Old,Incr,Op,SizeExp) ->
	  Prol = ('$int_rec_begin',
	          Prologue),
	  expand_arith_to_conj(SizeExp,Size,
	                       Prologue,
			       ('$set_size'(Size),
				'$int_rec_end',
				RestProl)),
          Ind1 = [(New,'$ind_high'(Old,Incr,Op)),
	          (Old,'$ind_low'(Old,Incr,Op))|Ind0]
        ; sys_error('recode_recurrence/3: failed for intrec')
        ).

recode_recurrence('$ind'(_,New,Old,Op,Incr),Prol,RestProl,Ind0,Ind1) :-
          Ind1 = [(New,'$ind_high'(Old,Incr,Op)),
	          (Old,'$ind_low'(Old,Incr,Op))|Ind0],
	  ( Op = '+' ->
	    Prol = ('$get_size'(N), Prologue),
	    expand_arith_to_conj(N*Incr+Old,New,Prologue,RestProl)
	  ; Op = '-' ->
	    Prol = ('$get_size'(N),Prologue),
	    expand_arith_to_conj(Old-N*Incr,New,Prologue,RestProl)
	  ).

 % recurrences should actually be re-inserted into the appropriate body
 % right now. Well, they aren't. So there.
recode_recurrence('$red'(_Scan,New,_Old,Expr),Prol,RestProl,Ind0,Ind1) :-
	warning('% reduction (~p) not handled -- will be incorrect.~n',
                [(New is Expr)]), Ind0 = Ind1, Prol = RestProl.

%

expand_arith_to_conj(AExp,Val,Prol,RestProl) :-
	expand_arithmetic(AExp,RetCell,LstProl,[Val=RetCell]),
	l_to_conj(LstProl,Prol,RestProl).

%

size_expression_of((X < Y),_New,Old,Incr,'+',SizeExp) :- !,
	( Old == X ->
  	  SizeExp = ceiling((Y-Old)/Incr)
	; sys_error('size_expression/6: comparison wrong')
        ).
size_expression_of((X =< Y),_New,Old,Incr,'+',SizeExp) :- !,
	( Old == X ->
	  SizeExp = ceiling(((Y+1)-Old)/Incr)
	; sys_error('size_expression/6: comparison wrong')
        ).
size_expression_of((X > Y),_New,Old,Incr,'-',SizeExp) :- !,
	( Old == X ->
	  SizeExp = ceiling((Old-Y)/Incr)
	; sys_error('size_expression/6: comparison wrong')
        ).
size_expression_of((X >= Y),_New,Old,Incr,'-',SizeExp) :- !,
	( Old == X ->
	  SizeExp = ceiling((Old-(Y-1))/Incr)
	; sys_error('size_expression/6: comparison wrong')
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Replacing induction variables in the body:
%
% The situation differs if you are in the left or right body.
% In the right body, we are counting down.

replace_iv_left(Gs,NewB,IV,IndE,Lvl) :-
	replace_iv(Gs,NewB,IV,IndE,Lvl).

replace_iv_right(Gs,NewB,IV,IndE,Lvl) :-
%	reverse_induction_sense(IndE,NewIndE),
	NewIndE = IndE,
	replace_iv(Gs,NewB,IV,NewIndE,Lvl).

% Low inductions become high and vice versa in right body

reverse_induction_sense([],[]).
reverse_induction_sense([X|Xs],[NewX|NewXs]) :-
	( X = '$ind_high'(N,K,Op) ->
	  NewX = '$ind_low'(N,K,Op)
	; X = '$ind_low'(N,K,Op) ->
	  NewX = '$ind_high'(N,K,Op)
	; sys_error('reverse_induction_sense/2: unknown induction expr')
	),
	reverse_induction_sense(Xs,NewXs).

%
% We assume all inductions are on the form
%  $ind_high(N,K,Op)
%  $ind_low(N,K,Op)
%
% For an induction M is N+K, we have
%   M = $ind_high(N,K,+)
%   N = $ind_low(N,K,+)

replace_iv((G,Gs),NewB,IV,IndE,Lvl) :- !,
	replace_iv_goal(G,NewB,RestB,IV,NewIV,IndE,Lvl,NewLvl),
	( primitive(G) -> % if so, prev. results are kept
	  replace_iv(Gs,RestB,NewIV,IndE,NewLvl)
	; replace_iv(Gs,RestB,[],IndE,unavailable)
	).

 % Note the sneaky trick: the goal being built is the last elt of the
 % conjunction!

replace_iv(G,NewB,IV,IndE,Lvl) :-
	replace_iv_term(G,NewG,NewB,NewG,IV,_,IndE,Lvl,_).

%

replace_iv_goal(G,Body,RestB,IV,NewIV,IndE,Lvl,NewLvl) :-
	replace_iv_term(G,NewG,Body,TmpRest,IV,NewIV,IndE,Lvl,NewLvl),
	TmpRest = (NewG, RestB).

%

replace_iv_term(X,Y,Body,RestB,IV,NewIV,IndE,Lvl,NewLvl) :- var(X),!,
	( available_iv(X,IV,Y) ->
	  Body = RestB, IV = NewIV, Lvl = NewLvl
	; indvar(X,IndE,IndExpr) ->
	  introduce_new_induction_var(X,IndExpr,Body,RestB,
	                              IV,NewIV,Lvl,NewLvl,Y)
	; X = Y, Body = RestB, IV = NewIV, Lvl = NewLvl
	).

replace_iv_term(X,Y,Body,RestB,IV,NewIV,_IndE,Lvl,NewLvl) :- atomic(X),!,
	X = Y, Body = RestB, IV = NewIV, Lvl = NewLvl.

replace_iv_term(PX,PY,Body,RestB,IV,NewIV,IndE,Lvl,NewLvl) :-
	PX =.. [P|Xs],
	replace_iv_term_list(Xs,Ys,Body,RestB,IV,NewIV,IndE,Lvl,NewLvl),
	PY =.. [P|Ys].

%

replace_iv_term_list([],[],B,B,IV,IV,_IndE,Lvl,Lvl).

replace_iv_term_list([X|Xs],[Y|Ys],B,RestB,IV,NewIV,IndE,Lvl,NewLvl) :-
	replace_iv_term(X,Y,B,TmpB,IV,CurrIV,IndE,Lvl,TmpLvl),
	replace_iv_term_list(Xs,Ys,TmpB,RestB,CurrIV,NewIV,IndE,TmpLvl,NewLvl).

%

available_iv(Var,IndVars,NewVar) :-
	soft_key_lookup(IndVars,Var,NewVar).

soft_key_lookup([(X,Y)|Xs],X0,Y0) :-
	( X == X0 ->
	  Y0 = Y
	; soft_key_lookup(Xs,X0,Y0)
	).

%

indvar(Var,IndExps,IndExp) :-
	soft_key_lookup(IndExps,Var,IndExp).

%

introduce_new_induction_var(X,IndE,Body,RestB,IV,NewIV,Lvl,NewLvl,Y) :-
	( IndE = '$ind_high'(Start,Incr,Op) ->
	  Multiplier = (NewLvl+1)
	; IndE = '$ind_low'(Start,Incr,Op) ->
	  Multiplier = NewLvl
	),
	( var(Lvl) ->
	  NewLvl = Lvl, Body = TmpB
	; Body = ('$get_level'(NewLvl),TmpB)
	),
	InductionExpr =.. [Op,Start,Multiplier*Incr],
	expand_arith_to_conj(InductionExpr,Y,TmpB,RestB),
%	TmpB = (Y is InductionExpr, RestB),
	NewIV = [(X,Y)|IV].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some test cases:

test_case(1,Base,Rec) :-
	Base = (p(X1,X2,X3,X4,X1,X2,X3,X4) :- X1 =< 0),
	Rec = (p(X1,X2,X3,X4,X5,X6,X7,X8) :-
	            X1 > 0,
		    Y4 is 3*4+X4,
		    Y3 is 3*4+X3,
		    Y2 is X2+1,
		    Y1 is X1-1,
		    q(Y2,Y4),
		    p(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8),
		    X8 is 3*4+Y8,
		    X7 is 3*4+Y7,
		    X6 is Y6+1,
		    X5 is Y5-1,
		    q(X2,X4)).

test_case(2,Base,Rec) :-
	Base = (p(X1,X2,X3,X4,X1,X2,X3,X4,N) :- X1 =< N),
	Rec = (p(X1,X2,X3,X4,X5,X6,X7,X8,N) :-
	            X1 > N,
		    Y4 is 3*4+X4,
		    Y3 is 3*4+X3,
		    Y2 is X2+1,
		    Y1 is X1-1,
		    q(Y2,Y4),
		    p(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,N),
		    X8 is 3*4+Y8,
		    X7 is 3*4+Y7,
		    X6 is Y6+1,
		    X5 is Y5-1,
		    q(X2,X4)).

test_case(3,Base,Rec) :-
	Base = (p(0) :- true),
	Rec = (p(N) :- N > 0, M is N-1, p(M)).

test_case(4,Base,Rec) :-
	Base = (p(0,0) :- true),
	Rec = (p(N,X) :- N > 0, M is N-1, p(M,Y), X is Y+1).

test_case(5,Base,Rec) :-
	Base = (nrev_star(0,[],[]) :- true),
	Rec = (nrev_star(N,[X|Xs],Zs) :-
	                 N > 0, M is N-1,
			 nrev_star(M,Xs,Ys),
			 append_star(M,Ys,[X],Zs)).

test_case(6,Base,Rec) :-
	Base = (gen(N,Max,Ns) :- N > Max, !, Ns=[]),
	Rec = (gen(N,Max,Ns) :- 
	               N =< Max, 
		       Ns=[N|Ns0], 
		       N1 is N+1, 
		       gen(N1,Max,Ns0)).

test_case(7,Base,Rec) :-
	Base = (p(0) :- !),
	Rec = (p(N) :- N > 0, M is N-1, p(M)).

test_case(8,Base,Rec) :-
	Base = (p(M,N) :- M > N),
	Rec = (p(M,N) :- M =< N, M1 is M+1, p(M1,N)).

test_case(9,Base,Rec) :-
	Base = (p(0) :- !),
	Rec = (p(N) :- N > 0, M is N-1, q(N,M), p(M), q(N,M)).

test_case(10,Base,Rec) :-
	Base = (tsp(0, _, _, P0, C0, P1, C1) :- !, P0 = P1, C0 = C1),
	Rec = (tsp(V, N, Matrix, MinP, MinC, P, C) :-
	           V > 0,
		   V1 is V-1,
		   all_but_this_vertex(N, V, Vs1),
		   travel(Vs1, Matrix, [V], P1),
		   cost([V|P1], Matrix, C1),
		   update_minimum1(C1, P1, V, MinC, MinP, MinC1, MinP1),
		   tsp(V1, N, Matrix, MinP1, MinC1, P, C)).


test_case(11,Base,Rec) :-
	Base = (primes(0,_,_,R0,R) :- !, R0 = R),
	Rec = (primes(K,N,Ps,R1,R) :-
	           K > 0,
		   loop_body(N,Ps,R1,R2),
		   K1 is K-2,
		   N1 is N+2,
		   primes(K1,N1,Ps,R2,R)).

test_case(12,Base,Rec) :-
	Base = (primes(0,_,_,R0,R) :- !, R0 = R),
	Rec = (primes(K,N,Ps,R1,R) :-
	           K > 0,
		   ( is_prime(N,Ps) ->
		     R1 = [N|R2]
		   ; R1 = R2
		   ),
		   K1 is K-2,
		   N1 is N+2,
		   primes(K1,N1,Ps,R2,R)).

portray_rec_clause((H :- rec(LP,LB,R,RB,RP))) :-
	portray_clause((H :- LP,LB,R,RB,RP)).

expand_and_display(N) :-
	test_case(N,Base,Rec),
	test_rp_prep(Base,Rec,rp(B,R,_S,_,LB,RB),_Spw),
	portray_clause(B),
	portray_rec_clause(R),
	portray_clause(LB),
	portray_clause(RB).

expand_and_display(N,B,R,LB,RB) :-
	test_case(N,Base,Rec),
	test_rp_prep(Base,Rec,rp(B,R,_S,_,LB,RB),_Spw),
	portray_clause(B),
	portray_rec_clause(R),
	portray_clause(LB),
	portray_clause(RB).

