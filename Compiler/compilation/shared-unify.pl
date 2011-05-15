%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%	 ALTERNATIVE MATCHING SCHEME FOR SHARED UNIFICATIONS
%
%   Unification between shared terms can be broken down to the cases shown
% below. Code emitted is written as a list of instructions.
%
% Note: modify ../postprocess/locality.pl if any pragma:s that can be
%       used to avoid locking binding are introduced!
%

:- ensure_loaded('../util/dict').
:- ensure_loaded('../util/vars').
:- ensure_loaded('../util/error').

:- ensure_loaded(types).
:- ensure_loaded('shared-clause').


% The generic unification inliner reduces unifications to instances of
% getting or putting a structure.

sh_inline_unify(X,Y,R0,R1,Lv0,Lv1,U0,U1,D,Leftmost,NewLeftmost) -->
	{ X = '$VAR'(N,_,_,_), integer(N), ! },
	( { void_variable(X,R0) } ->
	  { R1 = R0, NewLeftmost = Leftmost }
	; { first_occurence(X,R0) } ->
	  { location_of(X,R0,new(R)) },
	  ( { R = x(_) } ->
	    { update_location(X,R,R0,Rx) },
	    put_term(Y,R,Rx,R1,Lv0,Lv1,U0,U1,D,Leftmost,NewLeftmost)
	  ; { R = y(_) } ->
	    { Tmp = x(_) },
	    put_term(Y,Tmp,R0,Rx,Lv0,Lv1,U0,U1,D,Leftmost,NewLeftmost),
	    ['Get_Variable'(R,Tmp)],
	    { update_location(X,R,Rx,R1) }
	  ; { R = void } ->
	    { R0 = R1, Lv0 = Lv1, U0 = U1, Leftmost = NewLeftmost }
	  ; { sys_error('sh_inline_unify: bad dest ~q~n',[R]) }
	  )
	; /* { subsequent occurence(X,R0) } -> */
	  { location_of(X,R0,LocX),
	    locality_of(X,LX),
	    par_type_of(X,PX),
	    seq_type_of(X,SX) },
	  get_term(Y,LocX,LX,PX,SX,R0,R1,Lv0,Lv1,U0,U1,D,Leftmost,NewLeftmost)
	).

sh_inline_unify(X,Y,R0,R1,Lv0,Lv1,U0,U1,D,Leftmost,NewLeftmost) -->
	{ atomic(X),! },
	( { atomic(Y) } ->
	  ( { X == Y } ->
	    { R1 = R0, Leftmost = NewLeftmost, Lv0 = Lv1, U0 = U1 }
	  ; ['Fail'],
	    { R1 = R0, Leftmost = NewLeftmost, Lv0 = Lv1, U0 = U1 }
	  )
	; { Y = '$VAR'(Y_id,_,_,_), integer(Y_id) } ->
	  sh_inline_unify(Y,X,R0,R1,Lv0,Lv1,U0,U1,D,Leftmost,NewLeftmost)
	; ['Fail'],
	  { Leftmost = NewLeftmost, R0 = R1, U0 = U1, Lv0 = Lv1 }
	).

sh_inline_unify([X|Xs],Y,R0,R1,Lv0,Lv2,U0,U2,D,Leftmost,NewLeftmost) --> !,
	( { Y = '$VAR'(Y_id,_,_,_), integer(Y_id) } ->
	  sh_inline_unify(Y,[X|Xs],R0,R1,Lv0,Lv2,U0,U2,D,Leftmost,NewLeftmost)
	; { Y = [Y0|Ys] } ->
	  sh_inline_unify(X,Y0,R0,Rx,Lv0,Lv1,U0,U1,D,Leftmost,TmpLeft),
	  sh_inline_unify(Xs,Ys,Rx,R1,Lv1,Lv2,U1,U2,D,TmpLeft,NewLeftmost)
	; ['Fail'], { Leftmost = NewLeftmost, R0 = R1, U0 = U1, Lv0 = Lv1 }
	).

sh_inline_unify(X,Y,R0,R1,Lv0,Lv1,U0,U1,D,Leftmost,NewLeftmost) -->
	{ functor(X,F,N) },
	( { Y = '$VAR'(Y_id,_,_,_), integer(Y_id) } ->
	  sh_inline_unify(Y,X,R0,R1,Lv0,Lv1,U0,U1,D,Leftmost,NewLeftmost)
	; { functor(Y,F,N) } ->
	  { X =.. [_|Xs], Y =.. [_|Ys] },
	  sh_inline_unify_list(Xs,Ys,R0,R1,Lv0,Lv1,U0,U1,D,Leftmost,NewLeftmost)
	; ['Fail'], { Leftmost = NewLeftmost, R0 = R1, U0 = U1, Lv0 = Lv1 }
	).

%

sh_inline_unify_list([],[],R,R,Lv,Lv,U,U,_D,L,L) --> [].

sh_inline_unify_list([X|Xs],[Y|Ys],R0,R2,Lv0,Lv2,U0,U2,D,Left,NewLeft) -->
	sh_inline_unify(X,Y,R0,R1,Lv0,Lv1,U0,U1,D,Left,CurrLeft),
	sh_inline_unify_list(Xs,Ys,R1,R2,Lv1,Lv2,U1,U2,D,CurrLeft,NewLeft).

% put_term(T,X,R0,R1,Level0,Lvl1,Use,UseTail,D,Leftmost,NewLeftmost)
%
% Build term T into the location of X, which must be:
%   new(R), y(_), x(_), heap, global(_), head(_,_), tail(_,_), void

put_term(T,LocX,R0,R1,Lv0,Lv1,U0,U1,_D,Leftmost,NewLeftmost) -->
	{ Leftmost = NewLeftmost },
	sh_build_arg(T,LocX,R0,R1,Lv0,Lv1,U0,U1).


% get_term(T,LocX,LX,PX,SX,R0,R1,Lvl0,Lvl1,Use,UseT,D,Leftmost,NewLeftmost)
%
% The term T is 'gotten' from location Loc. This entails a wholesale
% unification, which is broken down into wearisome detail below.
% X is the corresponding variable (encountered previously) and
% we must take a look at its locality and type status (and so keep
% the actual var. name around).
%
% LX,PX and SX are locality, parallel type and sequential type information
% for LocX (location of X).
%
% *** UNFINISHED ***
% Is the variable-to-variable case OK?

get_term(T,LocX,LX,PX,SX,R0,R1,Lv0,Lv1,U0,U1,D,Leftmost,NewLeft) -->
	{ T = '$VAR'(N,_,_,_), integer(N), ! },
	{ location_of(T,R0,LocY),
	  locality_of(T,LY),
	  par_type_of(T,PY),
	  seq_type_of(T,SY) },
	await_leftmost(LX,PX,SX,LY,PY,SY,Leftmost,CurrLeft),
	( { LocX = heap } ->
	  { CurrLeft = NewLeft },
	  ( { LocY = new(R) } ->
	    { Lv0 = Lv1, U0 = U1 },
	    ( { R = void } ->
	      ['Unify_Void'],
	      { R0 = R1 }
	    ; ['Unify_Variable'(R)],
	      { update(N,R,R0,R1) }
	    )
	  ; { LocY = x(_) ; LocY = y(_) } ->
	    { R0 = R1, Lv0 = Lv1, U0 = U1 },
	    [comment(pragma([info(LocY,PY,SY,LY),
	                     unified_with(PX,SX,LX)]))],
	    ['Unify_Value'(LocY)]
	  ; { LocY = head(_,_) ; LocY = tail(_,_) } ->
	    { R0 = R1 },
	    move_to_reg(LocY,Lv0,Lv1,U0,U1,LocX)  % prev. Reg/LocX
	  ; { LocY = global(_) } ->
	    { R0 = R1 },
	    move_to_reg(LocY,Lv0,Lv1,U0,U1,LocX)   % prev Reg/LocX
	  )
	; { LocX = x(_) } ->
	  { CurrLeft = NewLeft },
	  ( { LocY = new(R) } ->
	    ( { R = void } ->
              { R0 = R1, Lv0 = Lv1, U0 = U1 }
	    ; ['Get_Variable'(R,LocX)],
	      { update(N,R,R0,R1), Lv0 = Lv1, U0 = U1 }
	    )
	  ; { LocY = x(_) ; LocY = y(_) } ->
	    [comment(pragma([info(LocX,PX,SX,LX),info(LocY,PY,SY,LY)]))],
	    ['Get_Value'(LocY,LocX)],
	    { R0 = R1, Lv0 = Lv1, U0 = U1 }
	  ; { LocY = head(_,_) ; LocY = tail(_,_) ; LocY = global(_) } ->
	    { R0 = R1 },
	    move_to_reg(LocY,Lv0,Lv1,U0,U1,Reg),
	    [comment(pragma([info(LocX,PX,SX,LX),info(Reg,PY,SY,LY)]))],
	    ['Get_Value'(Reg,LocX)]
	  )
	; { LocX = y(_) } ->
	  { CurrLeft = NewLeft },
	  ( { LocY = new(R) } ->
	    ( { R = void } ->
		{ R0 = R1, Lv0 = Lv1, U0 = U1 }
	    ; { R = x(_) } ->
		['Put_Value'(LocX,R)],
		{ update(N,R,R0,R1), Lv0 = Lv1, U0 = U1 }
	    ; { R = y(_) } ->
		{ Reg = x(_) },
		[comment(pragma([info(LocX,PX,SX,LX),info(Reg,PY,SY,LY)]))],
		['Put_Value'(R,Reg),'Get_Value'(LocX,Reg)],
		{ R0 = R1, Lv0 = Lv1, U0 = U1 }
	    )
	  ; { LocY = x(_) } ->
	      [comment(pragma([info(LocX,PX,SX,LX),info(LocY,PY,SY,LY)]))],
	      ['Get_Value'(LocX,LocY)],
	      { R0 = R1, Lv0 = Lv1, U0 = U1 }
	  ; { LocY = y(_) } ->
	      { Reg = x(_) },
	      [comment(pragma([info(LocX,PX,SX,LX),info(Reg,PY,SY,LY)]))],
	      ['Put_Value'(LocY,Reg),'Get_Value'(LocX,Reg)],
	      { R0 = R1, Lv0 = Lv1, U0 = U1 }
	  ; { LocY = head(_,_) ; LocY = tail(_,_) ; LocY = global(_) } ->
	      { R0 = R1 },
	      move_to_reg(LocY,Lv0,Lv1,U0,U1,Reg),
	      [comment(pragma([info(LocX,PX,SX,LX),info(LocY,PY,SY,LY)]))],
	      ['Get_Value'(Reg,LocX)]
	  )
	; { LocX = head(_,_) ; LocX = tail(_,_) ; LocX = global(_) } ->
	  move_to_reg(LocX,Lv0,Lvx,U0,Ux,Reg),
	  get_term(T,Reg,LX,PX,SX,
	           R0,R1,Lvx,Lv1,Ux,U1,D,CurrLeft,NewLeft)
	).

get_term(T,LocX,LX,PX,SX,R0,R1,Lv0,Lv1,U0,U1,D,Leftmost,NewLeft) -->
	{ atomic(T),! },
	await_nonvar(T,LocX,Lv0,Lvx,U0,Ux,
	             LX,PX,SX,R0,D,_DidWait,Leftmost,CurrLeft),
	( { LocX = heap } ->
	  [comment(pragma([unified_with(PX,SX,LX)]))],
	  ['Unify_Constant'(T)],
	  { R0 = R1, Lvx = Lv1, Ux = U1, CurrLeft = NewLeft }
	; { LocX = x(_) } ->
	  [comment(pragma([info(LocX,PX,SX,LX)]))],
	  ['Get_Constant'(T,LocX)],
	  { R0 = R1, Lvx = Lv1, Ux = U1, CurrLeft = NewLeft }
	; { LocX = y(_) } ->
	  { Tmp = x(_) },
	  [comment(pragma([info(LocX,PX,SX,LX)]))],
	  ['Put_Value'(LocX,Tmp),
	   'Get_Constant'(T,Tmp)],
	  { R0 = R1, Lvx = Lv1, Ux = U1, CurrLeft = NewLeft }
	; { LocX = head(_,_) ; LocX = tail(_,_) ; LocX = global(_) } ->
	  move_to_reg(LocX,Lvx,Lvy,Ux,Uy,Reg),
	  get_term(T,Reg,LX,PX,SX,R0,R1,Lvy,Lv1,Uy,U1,D,CurrLeft,NewLeft)
	; { sys_error('Bad dest in get_term (const): ~q',[LocX])}
        ).

get_term([T|Ts],LocX,LX,PX,SX,R0,R1,
	 Lv0,Lv3,U0,U3,D,Leftmost,NewLeft) --> !,
	await_nonvar([T|Ts],LocX,Lv0,Lv1,U0,U1,LX,PX,SX,R0,D,
	             DidWait,Leftmost,CurrLeft),
	( { LocX = heap } ->
	  ( { LX = local ; DidWait = yes, nonvar_t(SX) } ->
	    [comment(pragma([unified_with(PX,SX,LX)]))],
	    ['Unify_List'],
	    { Nonlocking = yes }
	  ; [comment(pragma([unified_with(PX,SX,LX)]))],
	    ['Lock_And_Unify_List'(Tmp)],
	    { Tmp = x(_), Nonlocking = no }
	  ),
	  { subterms_have_type(PX,2,SubPXs),
	    subterms_have_type(SX,2,SubSXs) },
	  get_terms([T,Ts],heap,LX,SubPXs,SubSXs,
	            R0,Rx,Lv1,Lv2,U1,U2,D,CurrLeft,NxtLeft,DeepTerms),
	  ( { Nonlocking == yes } ->
	    []
	  ; { nonvar_annot_term(Tmp) }, ['Unlock'(Tmp)]
	  ),
	  get_deep_terms(DeepTerms,LX,Rx,R1,
	                 Lv2,Lv3,U2,U3,D,NxtLeft,NewLeft)
	; { LocX = x(_) ; LocX = y(_) } ->
	  ( { LocX = y(_) } ->  % move into x-reg if permanent
	    { LocXX = x(_) },
	    ['Put_Value'(LocX,LocXX)]
	  ; { LocXX = LocX }    % otherwise nothing
	  ),
	  ( { LX = local 
	    ; nonvar_t(PX) 
	    ; DidWait = yes, nonvar_t(SX) } ->
	    [comment(pragma([info(LocXX,PX,SX,LX)]))],
	    ['Get_List'(LocXX)],
	    { Nonlocking = yes }
	  ; [comment(pragma([info(LocXX,PX,SX,LX)]))],
	    ['Lock_And_Get_List'(LocXX,Tmp)],
	    { Tmp = x(_), Nonlocking = no }
	  ),
	  { subterms_have_type(PX,2,SubPXs),
	    subterms_have_type(SX,2,SubSXs) },
	  get_terms([T,Ts],heap,LX,SubPXs,SubSXs,
	            R0,Rx,Lv1,Lv2,U1,U2,D,CurrLeft,NxtLeft,DeepTerms),
	  ( { Nonlocking == yes } ->
	    []
	  ; { nonvar_annot_term(Tmp) }, ['Unlock'(LocXX,Tmp)]
	  ),
	  get_deep_terms(DeepTerms,LX,Rx,R1,
	                 Lv2,Lv3,U2,U3,D,NxtLeft,NewLeft)
	; { LocX = head(_,_) ; LocX = tail(_,_) ; LocX = global(_) } ->
	  move_to_reg(LocX,Lv1,Lv2,U1,U2,Reg),
	  get_term([T|Ts],Reg,LX,PX,SX,R0,R1,
	           Lv2,Lv3,U2,U3,D,CurrLeft,NewLeft)
	).

get_term(T,LocX,LX,PX,SX,R0,R1,Lv0,Lv3,U0,U3,D,Leftmost,NewLeft) -->
	{ functor(T,F,N), T =.. [_|Ts] },
	await_nonvar(T,LocX,Lv0,Lv1,U0,U1,
                     LX,PX,SX,R0,D,DidWait,Leftmost,CurrLeft),
	( { LocX = heap } ->
	  ( { LX = local ; DidWait = yes, nonvar_t(SX) } ->
	    [comment(pragma([unified_with(PX,SX,LX)]))],
	    ['Unify_Structure'(F/N)],
	    { Nonlocking = yes }
	  ; [comment(pragma([unified_with(PX,SX,LX)]))],
	    ['Lock_And_Unify_Structure'(F/N,Tmp)],
	    { Tmp = x(_), Nonlocking = no }
	  ),
	  { subterms_have_type(PX,N,SubPXs),
	    subterms_have_type(SX,N,SubSXs) },
	  get_terms(Ts,heap,LX,SubPXs,SubSXs,
	            R0,Rx,Lv1,Lv2,U1,U2,D,CurrLeft,NxtLeft,DeepTerms),
	  ( { LX = local } ->
	    []
	  ; { nonvar_annot_term(Tmp) }, ['Unlock'(Tmp)]
	  ),
	  get_deep_terms(DeepTerms,LX,Rx,R1,
	                 Lv2,Lv3,U2,U3,D,NxtLeft,NewLeft)
	; { LocX = x(_) ; LocX = y(_) } ->
	  ( { LocX = y(_) } ->       % if permanent, move to temp
	    { LocXX = x(_) },
	    ['Put_Value'(LocX,LocXX)]
	  ; { LocXX = LocX }         % otherwise, keep in place
	  ),
	  ( { LX = local 
	    ; nonvar_t(PX)
	    ; DidWait = yes, nonvar_t(SX) } ->
	    [comment(pragma([info(LocXX,PX,SX,LX)]))],
	    ['Get_Structure'(F/N,LocXX)],
	    { Nonlocking = yes }
	  ; [comment(pragma([info(LocXX,PX,SX,LX)]))],
	    ['Lock_And_Get_Structure'(F/N,LocXX,Tmp)],
	    { Tmp = x(_), Nonlocking = no }
	  ),
	  { subterms_have_type(PX,N,SubPXs),
	    subterms_have_type(SX,N,SubSXs) },
	  get_terms(Ts,heap,LX,SubPXs,SubSXs,
	            R0,Rx,Lv1,Lv2,U1,U2,D,CurrLeft,NxtLeft,DeepTerms),
	  ( { Nonlocking == yes } ->
	    []
	  ; { nonvar_annot_term(Tmp) }, ['Unlock'(LocXX,Tmp)]
	  ),
	  get_deep_terms(DeepTerms,LX,Rx,R1,
	                 Lv2,Lv3,U2,U3,D,NxtLeft,NewLeft)
	; { LocX = global(_) ; LocX = head(_,_) ; LocX = tail(_,_) } ->
	  move_to_reg(LocX,Lv1,Lv2,U1,U2,Reg),
	  get_term(T,Reg,LX,PX,SX,R0,R1,Lv2,Lv3,U2,U3,D,CurrLeft,NewLeft)
	).

% get_terms/14 [DCG] encodes getting the toplevel structure of a term.
% Deep terms occuring in a last position (that's what Xs == [] means)
% are coded immediately while non-last deep terms are passed for later
% action.

get_terms([],_LocX,_LX,[],[],R,R,Lv,Lv,U,U,_D,Left,Left,[]) --> [].

get_terms([X|Xs],LocX,LX,[PX|PXs],[SX|SXs],R0,R2,
	  Lv0,Lv2,U0,U2,D,Left,NewLeft,Deep) -->
	( { X = '$VAR'(X_id,_,_,_), integer(X_id) } ->
	  { location_of(X,R0,LocationX) },
	  ( { LocationX = new(_) } ->
	    { Deep = Deep0 },
	    get_term(X,LocX,LX,PX,SX,R0,R1,
	             Lv0,Lv1,U0,U1,D,Left,CurrLeft)
	  ; { locality_of(X,LocalX), LocalX = fragile } ->
	    { Deep = [(X,Reg,PX,SX)|Deep0], Reg = x(_), R0 = R1 },
	    { Lv0 = Lv1, U0 = U1, Left = CurrLeft },
	    ['Unify_Variable'(Reg)] % known to be local etc.
	  ; { Deep = Deep0 },
	    get_term(X,LocX,LX,PX,SX,R0,R1,
	             Lv0,Lv1,U0,U1,D,Left,CurrLeft)
	  )
	; { atomic(X) } ->
	  { Deep = Deep0 },
	  get_term(X,LocX,LX,PX,SX,R0,R1,
	           Lv0,Lv1,U0,U1,D,Left,CurrLeft)
	; { Xs == [], LX = local } ->
	  { Deep = Deep0 },
	  get_term(X,LocX,LX,PX,SX,R0,R1,
	           Lv0,Lv1,U0,U1,D,Left,CurrLeft)
	; { Deep = [(X,Reg,PX,SX)|Deep0], Reg = x(_), R0 = R1 },
	  { Lv0 = Lv1, U0 = U1, Left = CurrLeft },
	  ['Unify_Variable'(Reg)]
	),
	get_terms(Xs,LocX,LX,PXs,SXs,R1,R2,
	          Lv1,Lv2,U1,U2,D,CurrLeft,NewLeft,Deep0).

% get_deep_terms/10 [DCG] finalizes the getting of a structure, by matching
% the 'leftover' terms of get_terms.

get_deep_terms([],_LX,R,R,Lv,Lv,U,U,_D,Left,Left) --> [].

get_deep_terms([(Term,Reg,PX,SX)|Xs],LX,R0,R2,
	       Lv0,Lv2,U0,U2,D,Left,NewLeft) -->
	get_term(Term,Reg,LX,PX,SX,R0,R1,
	         Lv0,Lv1,U0,U1,D,Left,CurrLeft),
	get_deep_terms(Xs,LX,R1,R2,
	               Lv1,Lv2,U1,U2,D,CurrLeft,NewLeft).

% The WAIT(T,X) algorithm occurs in pairs, so this algorithm
% test both if left and right terms require suspension.
%
% The criterion is:
% if T1 is fragile or (sh+nondet) and T1's partype is 'uninst'
%    and T2's partype is 'nonvar'
%  or vice versa,
% then suspend
% else do not suspend.
%
% The rest of the compiler can make use of knowing whether a
% suspension test was generated or not; hence DidWait (yes/no).

await_nonvar(T,LocX,Lv0,Lv2,U0,U2,LX,PX,SX,R,D,DidWait,Leftmost,CurrLeft) -->
	{ Leftmost = CurrLeft,
	  ( Leftmost == yes ->
	    construct_seq_type(T,TypeT),
	    TypeX = SX
	  ; Leftmost == no ->
	    construct_par_type(T,TypeT),
	    TypeX = PX
	  ),
	  construct_locality(T,LT),
	  construct_location(T,R,LocT)
	},
	await_nv(LX,LocX,Lv0,Lv1,U0,U1,TypeX,TypeT,D,DidWait0),
	await_nv(LT,LocT,Lv1,Lv2,U1,U2,TypeT,TypeX,D,DidWait1),
	{ ( DidWait0 == yes ; DidWait1 == yes ) ->
	  DidWait = yes
	; DidWait = no
	}.

%
await_nv(freeze(Loc),LocX,Lv0,Lv1,U0,U1,ParX,ParT,D,DidWait) --> !,
	await_nv(Loc,LocX,Lv0,Lv1,U0,U1,ParX,ParT,D,DidWait).

await_nv(fragile,LocX,Lv0,Lv1,U0,U1,ParX,ParT,D,DidWait) --> !,
	( { uninst_t(ParX), binding_t(ParT) } ->
	  { DidWait = yes },
	  generate_wait(LocX,Lv0,Lv1,U0,U1,D)
	; { DidWait = no, Lv0 = Lv1, U0 = U1 }
	).

await_nv(wbf,LocX,Lv0,Lv1,U0,U1,ParX,ParT,D,DidWait) --> !,
	( { D = nondet, uninst_t(ParX), binding_t(ParT) } ->
	  { DidWait = yes },
	  generate_wait(LocX,Lv0,Lv1,U0,U1,D)
	; { DidWait = no, Lv0 = Lv1, U0 = U1 }
	).

await_nv(robust,LocX,Lv0,Lv1,U0,U1,ParX,ParT,D,DidWait) --> !,
	( { D = nondet, uninst_t(ParX), binding_t(ParT) } ->
	  { DidWait = yes },
	  generate_wait(LocX,Lv0,Lv1,U0,U1,D)
	; { DidWait = no, Lv0 = Lv1, U0 = U1 }
	).

await_nv(local,_LocX,Lv0,Lv1,U0,U1,_ParX,_ParT,_D,DidWait) --> !,
	{ DidWait = no, Lv0 = Lv1, U0 = U1 }.

await_nv(Loc,_LocX,Lv0,Lv1,U0,U1,_ParX,_ParT,_D,_DidWait) -->
	{ Lv0 = Lv1, U0 = U1 },
	{ sys_error('await_nv/8: invalid locality info ~q',[Loc]) }.

% The current type of a term in the computation:
%
% If leftmost, the type to use is the sequential type, if not,
% use the parallel type.

current_type(X,_,_,_) :- var(X), !,
	sys_error('current_type/4: variable as leftmost info!',[]).

current_type(yes,_ParT,SeqT,Type) :- !, Type = SeqT.

current_type(no, ParT,_SeqT,Type) :- !, Type = ParT.

current_type(Left,_,_,_) :- 
	sys_error('current_type/4: invalid leftmost info ~q',[Left]).

% Generate the proper await-instruction depending on determinacy and
% location. It is assumed that data residing in strange locations
% now are either on the heap (at the S-ptr, to be precise) or in a
% temporary register. If the location is 'none', we are dealing with
% a term being matched. (E.g. X =[A1|A2], then the rhs has location none)
% In that case, no suspension is generated.

generate_wait(heap,Lv0,Lv1,U0,U1,D) --> !,
	{ Lv0 = Lv1, U0 = U1 },
	( { D = det } ->
	  ['Await_Nonvar']
	; { D = nondet } ->
	  ['Await_Strictly_Nonvar'],
	  { warning('suspends if nondeterministic') }
	; { sys_error('generate_wait/4: invalid determinacy info ~q',[D]) }
        ).

generate_wait(x(R),Lv0,Lv1,U0,U1,D) --> !,
	{ Lv0 = Lv1, U0 = U1 },
	( { D = det } ->
	  ['Await_Nonvar'(x(R))]
	; { D = nondet } ->
	  ['Await_Strictly_Nonvar'(x(R))],
	  { warning('suspends if nondeterministic') }
	; { sys_error('generate_wait/4: invalid determinacy info ~q',[D]) }
        ).

generate_wait(y(R),Lv0,Lv2,U0,U2,D) --> !,
	{ Temp = x(_) },
	sh_move(y(R),Lv0,Lv1,U0,U1,Temp),
	generate_wait(Temp,Lv1,Lv2,U1,U2,D).

generate_wait(none,Lv0,Lv1,U0,U1,_D) --> !,
	{ Lv0 = Lv1, U0 = U1 },
	[].

generate_wait(Loc,Lv0,Lv2,U0,U2,D) -->
	move_to_reg(Loc,Lv0,Lv1,U0,U1,R),
	generate_wait(R,Lv1,Lv2,U1,U2,D).

% This one is taken from the global analyser. Until it's been constructed, we
% use a simplified version.
%
% *** UNFINISHED ***
% Use new variable representation instead

construct_par_type('$VAR'(N,P,_S,_L),P) :- integer(N), !.

construct_par_type(Term,Type) :- atomic(Term), !,
	Type = gnd.

construct_par_type(_FX,Type) :- % struct or (concrete) list
	Type = nv.

%

construct_seq_type('$VAR'(N,_P,S,_L),S) :- integer(N), !.

construct_seq_type(Term,Type) :- atomic(Term), !,
	Type = gnd.

construct_seq_type(_FX,Type) :-
	Type = nv.

% Constructing the locality of a term is simply:
% 1. Get the variables of the term
% 2. Take the worst of their locality.

construct_locality(Term,Locality) :-
	variable_terms(Term,Vs),
	locality_list(Vs,LocList),
	( member(fragile,LocList) ->
	  Locality = fragile
	; member(robust,LocList) ->
	  Locality = robust
	; Locality = local
	).

%

locality_list([],[]).

locality_list([X|Xs],[L|Ls]) :-
	locality_of(X,L),
	locality_list(Xs,Ls).

% The location of a term is either the location of
% the variable, or 'none'

construct_location(X,Regs,LocX) :-
	X = '$VAR'(N,_,_,_), integer(N), !, lookup(N,Regs,LocX).

construct_location(_,_,none).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

variable_annot_term('$VAR'(_,_,_,_)).

nonvar_annot_term(X) :- \+(variable_annot_term(X)).

%

variable_terms(Term,Vs) :- 
	variable_terms(Term,Vs,[]).

unique_variable_terms(Term,Vs) :-
	variable_terms(Term,Ws),
	sort(Ws,Vs).

%

variable_terms('$VAR'(N,P,S,L)) --> { integer(N) }, !,
	['$VAR'(N,P,S,L)].

variable_terms(X) --> { atomic(X) }, !.

variable_terms(PX) -->
	{ PX =.. [_|Xs] },
	variable_terms_list(Xs).

%

variable_terms_list([]) --> [].
variable_terms_list([X|Xs]) -->
	variable_terms(X),
	variable_terms_list(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The LEFTMOST(X) algorithm follows:
%
% The computation must await leftmostness when the term involved
% is fragile and unpredictably instantiated by a unification.
%
% The test occurs when a value-value unification is done.
% At the point, the compiler cannot tell how much of the
% term will get bound.

await_leftmost(LX,PX,SX,LY,PY,SY,Leftmost,CurrLeft) -->
	( { Leftmost == yes } ->
	  { CurrLeft = Leftmost }
	; { current_type(Leftmost,PX,SX,TypeX),
	    current_type(Leftmost,PY,SY,TypeY) },
	  ( { LX = fragile, nonground_t(TypeX), binding_t(TypeY)
	    ; LY = fragile, nonground_t(TypeY), binding_t(TypeX) } ->
	    ['Await_Leftmost'],
	    { CurrLeft = yes, warning('suspends until leftmost') }
	  ; { CurrLeft = Leftmost }
	  )
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some miscellaneous predicates follow.

void_variable(X,R) :-
	lookup_location(X,R,Loc),
	( Loc = void ; Loc = new(void) ),!.

first_occurence(X,R) :-
	lookup_location(X,R,Loc),
	Loc = new(_).

location_of(X,R,Loc) :-
	lookup_location(X,R,Loc).

par_type_of('$VAR'(_N,P,_S,_L),P).
seq_type_of('$VAR'(_N,_P,S,_L),S).
locality_of('$VAR'(_N,_P,_S,L),L).

%locality_of(X,R,Loc) :-
%	lookup(X,R,Loc).
%
%par_type_of(X,R,Loc) :-
%	lookup(X,R,Loc).
%
%seq_type_of(X,R,Loc) :-
%	lookup(X,R,Loc).
%

move_to_reg(x(N),Lv0,Lv1,U0,U1,R) --> !, { R = x(N), Lv0 = Lv1, U0 = U1 }.

move_to_reg(y(N),Lv0,Lv1,U0,U1,R) --> !, { R = y(N), Lv0 = Lv1, U0 = U1 }.

move_to_reg(new(Reg),_Lv0,_Lv1,_U0,_U1,_R) --> !,
	{ sys_error('move_to_reg has src ~q -- rejected for now?',[new(Reg)])}.

move_to_reg(R1,Lv0,Lv1,U0,U1,R2) -->
	{ R2 = x(_) },!,
	( { R1 = head(W,K) } ->
	  { U0 = [W|U1] },
	  sh_move_level_reg(Lv0,Lv1),
	  ['Put_Nth_Head'(W,Lv1,K,R2)]
	; { R1 = tail(W,K) } ->
	  { U0 = [W|U1] },
	  sh_move_level_reg(Lv0,Lv1),
	  ['Put_Nth_Tail'(W,Lv1,K,R2)]
	; { R1 = global(A) } ->
	  { Lv0 = Lv1, U0 = [A|U1] },
	  ['Put_Global_Arg'(A,R2)]
	).

move_to_reg(R1,Lv0,Lv1,U0,U1,R2) -->
	{ R2 = heap },
	( { R1 = head(W,K) } ->
	  { U0 = [W|U1] },
	  sh_move_level_reg(Lv0,Lv1),
	  [comment(pragma([]))],
	  ['Unify_Nth_Head'(W,Lv1,K)]
	; { R1 = tail(W,K) } ->
	  { U0 = [W|U1] },
	  sh_move_level_reg(Lv0,Lv1),
	  [comment(pragma([]))],
	  ['Unify_Nth_Tail'(W,Lv1,K)]
	; { R1 = global(A) } ->
	  { Lv0 = Lv1, U0 = [A|U1] },
	  [comment(pragma([]))],
	  ['Unify_Global'(A)]
	).

%

init_env(Xs,R,L,P,S) :-
	empty_dict(D),
	environment(Xs,D,R,D,L,D,P,D,S).

%

environment([],R,R,L,L,P,P,S,S).

environment([(X,R,L,P,S)|Xs],R0,R2,L0,L2,P0,P2,S0,S2) :-
	insert(X,R,R0,R1),
	insert(X,L,L0,L1),
	insert(X,P,P0,P1),
	insert(X,S,S0,S1),
	environment(Xs,R1,R2,L1,L2,P1,P2,S1,S2).
