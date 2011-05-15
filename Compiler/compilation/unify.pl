%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		      INLINE CODED UNIFICATIONS
%
% The various transformation steps of the Reform compiler means there
% are quite a lot of explicit unifications appearing in the code. This
% module specializes unifications into standard WAM instructions (rather
% than generic calls) for some common cases.
%
% In particular, unifications X = T will appear much more commonly;
% this is a result of throwing away variable classes in the recursion
% parallel compiler, and of breaking out head unifications in the
% shared compiler.
%
% There are two versions: sh_inline_unify(X,Y,...) and
%  sh_inline_unify(X,Y,...) that are used by each of the compilers. Since
% there are some differences in e.g. where variables can be situated and
% in locking accesses, I have decided to separate them.
%
% The following schema is used:
% In the unification of X with Y, do as follows:
% - X or Y is a void variable, emit no code.
% - X and Y are variables:
%   * Both X and Y appear for the first time,
%     put X, then get Y from X.
%   * X or Y appear for the first time:
%     get_variable X from Y or get_variable Y from X.
%   * Both X and Y have appeared previously:
%     - move one of them into a temporary
%     - get_value <other>,<moved>
% - X is a variable and Y is a nonvar term T (or vice versa):
%   * X has appeared before, "get_constant Y,X" or "get_structure Y,X; ..."
%   * X has not appeared before, "put_constant Y,X" or "put_structure Y,X; ..."
% - both X and Y are nonvariables
%   * If the toplevels of X and Y are non-unifiable, emit "fail"
%   * Otherwise, recursively unify subarguments (or do nothing for constants)
%
% The inlining process also involves Locality, (parallel and sequential)
% Type and Determinacy
% information; L, P and T are dictionaries and D is one of det or nondet.
% This must be done to encode unification of shared data correctly in
% the process bodies.

% *** UNFINISHED ***
% No real handling of head, tail or global arguments. These must use
% locking unification, also! How can we use independence etc. information
% to avoid locking etc.?
%
% We should really have a table of type/locality/(det) information to
% use even here!
%
% L0,L1,U0,U1,E0,E1 must be threaded correctly before use!

sh_inline_unify(X,Y,E0,E1,L0,L1,U0,U1,Leftmost,D) -->
	( { X  = '$VAR'(_,_,_,_) } ->
	  { lookup(X,E0,LocX) },
	  ( { LocX = new(void) } -> /* X is a void: no code */
	    { L0 = L1, U0 = U1, E0 = E1 }
	  ; ( { var(Y) } ->
	      { lookup(Y,E0,LocY) },
	      ( { LocY = new(void) } -> /* Y is a void: no code */
	        { L0 = L1, U0 = U1, E0 = E1 }
	      ; { LocX = new(RX), LocY = new(RY) } -> /* X and Y are new */
		( { RX = x(_) } ->
		  ['Put_Void'(RX)],
		  { update_list(X,RX,E0,Ex),
		    locality_of(X,LocX),
		    par_type_of(X,ParX),
		    seq_type_of(X,SeqX),
		    L0 = L1, U0 = U1 },
		  sh_match(Y,RX,ParX,SeqX,LocX,Ex,E1,Leftmost,D)
		; { RY = x(_) } ->
		  ['Put_Void'(RY)],
		  { update_list(Y,RY,E0,Ex),
		    locality_of(Y,LocY),
		    par_type_of(Y,ParY),
		    seq_type_of(Y,SeqY),
		    L0 = L1, U0 = U1 },
		  sh_match(X,RY,ParY,SeqY,LocY,Ex,E1,Leftmost,D)
		; /* Both vars will be in permanent registers */
		  { Tmp = x(_) },
		  ['Put_Void'(Tmp)],
		  sh_match(X,Tmp,free,free,local,E0,Ex,Leftmost,D),
		  { lookup_location(X,Ex,RegX),
		    locality_of(X,LocX),
		    par_type_of(X,ParX),
		    seq_type_of(X,SeqX) },
		  sh_match(Y,RegX,ParX,SeqX,LocX,Ex,E1,Leftmost,D),
	          { L0 = L1, U0 = U1 } % *** UNFINISHED *** in sh_match instd!
		)
	      ; { LocX = new(RX) } -> /* X is new, Y appears prev. */
	        sh_move(RY,L0,L1,U0,U1,RX),
		{ update(X,RX,E0,E1) }
	      ; { LocY = new(RY) } -> /* Y is new, X appears prev. */
	        sh_move(RX,L0,L1,U0,U1,RY),
		{ update(Y,RY,E0,E1) }
	      ; { /* X and Y have appeared previously */ },
		{ par_type_of(X,ParX),
		  seq_type_of(X,SeqX),
		  locality_of(X,LocalityX),
		  L0 = L1, U0 = U1 },
		sh_match(Y,LocX,ParX,SeqX,LocalityX,E0,E1,Leftmost,D)
		)
	      )
	  ; { atomic(Y) } ->
	    ( { LocX = new(RX) } ->
              sh_build_arg(Y,RX,E0,E1,L0,L1,U0,U1)
	    ; { par_type_of(X,ParX),
		seq_type_of(X,SeqX),
		locality_of(X,LocalityX),
		L0 = L1, U0 = U1 },
	      sh_match(Y,LocX,ParX,SeqX,LocalityX,E0,E1,Leftmost,D)
	    )
	  ; { LocX = new(RX) } ->
	    sh_build_arg(Y,RX,L0,L1,U0,U1,E0,Ex),
	    { update(X,RX,Ex,E1) }
	  ; { locality_of(X,LocalityX),
	      par_type_of(X,ParX),
	      seq_type_of(X,SeqX),
	      L0 = L1, U0 = U1 },
	    sh_match(Y,RX,ParX,SeqX,LocalityX,E0,E1,Leftmost,D)
	  )
	; { atomic(X) } ->
	    ( { var(Y) } ->
	      sh_inline_unify(Y,X,L0,L1,U0,U1,E0,E1,D)
	    ; { atomic(Y) } ->
		( { X == Y } ->
		  []
		; ['Fail']
		),
		{ L0 = L1, U0 = U1, E0 = E1 }
	    ; ['Fail'],
	      { L0 = L1, U0 = U1, E0 = E1 }
	   )
	; { X = [X1|X2] } ->
	    ( { var(Y) } ->
	      sh_inline_unify(Y,X,E0,E1,L0,L1,U0,U1,Leftmost,D)
	    ; { Y = [Y1|Y2] } ->
	      sh_inline_unify(X1,Y1,E0,Ex,L0,Lx,U0,Ux,Leftmost,D),
	      sh_inline_unify(X2,Y2,Ex,E1,Lx,L1,Ux,U1,Leftmost,D)
	    ; ['Fail'],
	      { L0 = L1, U0 = U1, E0 = E1 }
	    )
	; ( { var(Y) } ->
	    sh_inline_unify(Y,X,E0,E1,L0,L1,U0,U1,Leftmost,D)
	  ; { atomic(X) ; X = [_|_] } ->
	    ['Fail'],
	    { L0 = L1, U0 = U1, E0 = E1 }
	  ; { functor(X,P,N), functor(Y,P,N) } ->
	    { X =.. [_|Xs],
	      Y =.. [_|Ys] },
	    sh_inline_unify_list(Xs,Ys,E0,E1,L0,L1,U0,U1,Leftmost,D)
	  ; ['Fail'],
	    { L0 = L1, U0 = U1, E0 = E1 }
	  )
	).

%

sh_inline_unify_list([],[],E,E,L,L,U,U,_,_) --> [].

sh_inline_unify_list([X|Xs],[Y|Ys],L0,L2,U0,U2,E0,E2,Left,D) -->
	sh_inline_unify(X,Y,L0,L1,U0,U1,E0,E1,Left,D),
	sh_inline_unify_list(Xs,Ys,L1,L2,U1,U2,E1,E2,Left,D).

