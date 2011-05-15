%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%			COMPILATION OF CLAUSES
%
%   This is the clause compiler. It handles the compilation for shared
% execution. The main changes from a standard compiler are the following:
%
% - If present, a 'sysmode' declaration initializes information on determinacy,
%   locality and types of variables.
%
% - So called 'ghost primitives' update the state of determinacy, types and
%   locality.
%
%   Also, when compared to the sequential compiler, there is the change that a
% dictionary of register bindings are used rather than the 'walking 3 terms'
% approach of before.
%
% The clause is rewritten as follows:
%
%	p(T1,...,Tn) :- B.				becomes...
%	p(X1,...,Xn) :- X1 = T1, ..., Xn = Tn, B.
%
% where X1,...,Xn are new variables.
%
%   This will allow us a more uniform handling of sharing etc., since the
% arguments to the procedure can be handled by standard dictionaries
% (i.e. source variables represent values, there is no notion of 'first arg'
% in the dictionary).
%
%   Also, we have introduced level registers and register usage parameters.
% This allows us to use the same compiler for clauses and process bodies.
% This is necessary when process bodies perform unifications etc. on shared
% data too.
%

:- ensure_loaded('../util/dict').
:- ensure_loaded('../util/vars').
:- ensure_loaded('../util/basics').
:- ensure_loaded('../postprocess/peep').

:- ensure_loaded('rp-util').		% var classification
:- ensure_loaded('rp-compiler').
:- ensure_loaded('builtin-map').
:- ensure_loaded('shared-unify').
:- ensure_loaded(unify).
:- ensure_loaded(safeness).

:- dynamic '$sysmode'/4.


%%%
%%  sh_compile_clause(+Clause,-Code)
%
% All clauses submitted to sh_compile_clause/2 MUST look like:
%
% p(X1,...,Xn) :- X1=T1,...,Xn=Tn,...
%
% (This is for reasons of type annotation &c being done properly)
%
% An occurence of a variable is denoted '$VAR'(N,P,S,L)
%   with N the variable's index (integer)
%        P the parallel type
%        S sequential type
%        L locality
%
% (Properly annotated clauses appear on this form.)
%
%
sh_compile_clause((H :- B),Code) :- !,
	functor(H,_,N),
	sh_comp_clause(H,B,RawCode,[]),
	final_process(RawCode,N,Code).

sh_compile_clause(H,Code) :-
	functor(H,_,N),
	sh_comp_clause(H,true,RawCode,[]),
	final_process(RawCode,N,Code).

%

final_process(RawCode,N,Code) :-
	regalloc(RawCode,N),
	peephole_opt(RawCode,OptCode),
	heaptest(OptCode,N,TestingCode),
	safe_code(TestingCode,SafeCode),
	peephole_opt2(SafeCode,Code),!.

final_process(_RawCode,_N,_Code) :-
	sys_error('final_process/3: failed').


%%%
%%  Classify the variables of the clause; collect determinacy information etc.
% (or make worst case assumptions); group the clause into compilation blocks;
% set up register, type and locality environments; finally, generate code.
%
sh_comp_clause(Head,Body) -->
	{
          sh_classify_body(Head,Body,GrpBody,Trim,Regs,N),
	  ( Trim = [_] ->
	      HasEnv = no
	  ; true,
	      HasEnv = yes(N)
	  )
	},
	sh_comp_clause(Head,GrpBody,Regs,Trim,_Det,HasEnv),!.

sh_comp_clause(_,_) -->
	{ sys_error('sh_comp_clause/4: failed') }.

%%%

sh_classify_body(Head,Body,Grp,Trim,Regs,EnvSz) :-
	sequential_classify_body((Head :- Body),Grp,Trim,Allocation,EnvSz),
	empty_dict(Empty),
	insert_list_of_new_regs(Allocation,Empty,Regs).

%

insert_list_of_new_regs([],D,D).

insert_list_of_new_regs([(X,R)|Xs],D0,D2) :-
	insert(X,new(R),D0,D1),
	insert_list_of_new_regs(Xs,D1,D2).

%%%
%%  If no mode is given, we use "default assumptions" and use mode
% p(T,...,T) with T = t(any,any,local) which is suitable for sequential
% compilation (only).
%

default_mode(N,Mode) :-
	default_mode_list(N,Xs),
	Mode =.. [mode|Xs].

default_mode_list(0,X) :- !, X = [].
default_mode_list(N,[X|Xs]) :- N > 0,
	X = t(any,any,local),
	M is N-1,
	default_mode_list(M,Xs).


%%%
%%  sh_comp_clause(+Head,+Groups,R0,Trim,D0,HasEnv) --> CodeStream,Handle
%
%   Compile a clause decorated with ghost primitives that tell us how locality
% etc. changes with time. The definitions of how to handle ghost primitives and
% what "ghost" primitivess are available can be found in inlineable.pl .
%   The LvN and UN parameters are used only when compiling process bodies.
% For standard shared predicates, they can be ignored. They are used to:
% hold the recursion level register (used to access parameters held in vectors),
% which is moved into a temporary for every chunk it appears in; and to trace
% what global registers are used in the procedure body (used for register
% allocation of prelude).
%   The compiler at this stage produces suboptimal code, e.g. call/proceed
% pairs rather than execute. It is the task of the peephole optimizer to
% collapse this into execute (deallocate + execute) when needed.
%
%  Lv0,Lv1,...	stand for the location of the level register
%  U0, U1, ...	stand for register uses
%  L0, L1, ...	stand for the locality dictionaries
%  P0, P1, ...	for parallel type dictionaries
%  T0, T1, ...	for sequential type dictionaries
%  D0, D1, ...	for determinacy info (det or nondet)
%  R0, R1, ...	stands for the changing register allocation
%  Head		is the head of the clause
%  GrpBody	is a conjunction of groups, g(X) where X is a conjunction
%		of goals, all except the last being primitives and the last goal
%		being a primitive or user defined goal.
%

sh_comp_clause(Head,GrpBody,R0,Trim,D0,HasEnv) -->
	sh_match_head(Head,R0,R1,D0,D1,HasEnv),
	{ Leftmost = no },
%	  eliminate_empty_groups(GrpBody,NewGrpBody) },
	sh_compile_groups(GrpBody,Trim,R1,_R2,no_reg,_U0,_U1,D1,HasEnv,Leftmost),
	['Proceed'].


% This procedure relies on the head being a completely, moronically trivial
% schmuckly p(X1,...,Xn).
%
% We also CANNOT reorder arguments at this point. Annotation must do that,
% since types depend on order of unifications!
%

% All arguments in the head must be new variables, on the form
%  '$VAR'(N,P,S,L). Their register is simply updated from being "new(R)"
% to being "R".

sh_match_head(Head,R0,R1,D0,D1,HasEnv) -->
	( { HasEnv = yes(N) } ->
	  ['Allocate'(N)]
	; []
	),
	{ Head =.. [_|Xs], D0 = D1 },
	initialize_regs(Xs,0,R0,R1).


% eliminate_empty_groups/2 removes g(true) from wherever it
% occurs -- except if it is the sole group of the body, in
% which case the compiler must have something to do!
%
% (That case is unlikely to occur with a real program, since
% $choice/1 is stuffed into every clause. Still, if that changes,
% we are prepared!)

eliminate_empty_groups(Grps,NewGrps) :-
	elim_empty_groups(Grps,ElimGrps),
	( var(ElimGrps) ->
	    NewGrps = g(true)
	; true,
	    NewGrps = ElimGrps
	).

% Kill all g(true) groups.

elim_empty_groups((Grp,Grps),NewGrps) :-
	( Grp = g(true) ->
	    elim_empty_groups(Grps,NewGrps)
	; true,
	    elim_empty_groups(Grps,NewGrpsTail),
	    ( var(NewGrpsTail) ->
		NewGrps = Grp
	    ; true,
	        NewGrps = (Grp,NewGrpsTail)
	    )
	).

elim_empty_groups(g(Goals),NewGrps) :-
	( Goals = true ->
	    true	%%% "return a variable"
	; true,
	    NewGrps = g(Goals)
	).

%

initialize_regs([],_,R,R) --> [].

initialize_regs([X|Xs],Arg,R0,R2) -->
	( { X = '$VAR'(N,_P,_S,_L) } ->
	  { lookup(N,R0,Reg) },
	  ( { Reg = new(R) } ->
	    ( { R = void } -> [], { N_reg = R }
	    ; { R = x(Arg) } -> ['Get_Variable'(N_reg,x(Arg))], { N_reg = x(_) }
	    ; { R = y(_) } -> ['Get_Variable'(R,x(Arg))], { N_reg = R }
            ),
	    { NxtArg is Arg+1,
	      update(N,N_reg,R0,R1) },
	    initialize_regs(Xs,NxtArg,R1,R2)
	  ; { sys_error('initialize_regs/6: repeated var. in rewritten clause hd') }
          )
	; { sys_error('initialize_regs/6: non-var term in rewritten clause head') }
        ).
	
% This procedure is called both in order to compile a standard shared
% predicate, and to compile process bodies.

sh_compile_groups((G,Gs),Trim,R0,R2,Lv0,U0,U2,D0,HasEnv,Left) --> !,
	{ Trim = [T|Ts], G = g(Xs) },
	sh_compile_group(Xs,T,R0,R1,Lv0,U0,U1,D0,D1,Left,NewLeft),
	sh_compile_groups(Gs,Ts,R1,R2,Lv0,U1,U2,D1,HasEnv,NewLeft).

sh_compile_groups(g(Xs),Trim,R0,R1,Lv0,U0,U1,D0,HasEnv,Left) -->
	{ Trim = [T] },
	sh_compile_group(Xs,T,R0,R1,Lv0,U0,U1,D0,_D1,Left,_NewL),
	( { HasEnv = yes(_) } ->
	  ['Deallocate']
	; []
	).

%

sh_compile_group((X,Xs),T,R0,R2,Lv0,U0,U2,D0,D2,Left0,Left2) --> !,
	sh_inline_primitive(X,R0,R1,Lv0,Lv1,U0,U1,D0,D1,Left0,Left1),
	sh_compile_group(Xs,T,R1,R2,Lv1,U1,U2,D1,D2,Left1,Left2).

sh_compile_group(X,T,R0,R1,Lv0,U0,U1,D0,D1,Left0,Left1) -->
	( { inlineable(X) } ->
	    sh_inline_primitive(X,R0,R1,Lv0,_,U0,U1,D0,D1,Left0,Left1)
	; { D0 = D1,
	    Left0 = Left1,
	    functor(X,_,N),
	    M is N-1,
	    target_reg_list(0,M,Rs,As)
	  },
	  sh_compile_goal(X,T,Rs,As,R0,R1,Lv0,U0,U1)
	).

% Compiling a user defined goal means building the arguments followed
% by calling the predicate with correct stack trimming. The locality,
% type or determinacy info isn't needed, since building a struct doesn't
% synch.

sh_compile_goal(X,T,Rs,As,R0,R1,Lvl,U0,U1) -->
	{ X =.. [_|Xs], functor(X,P,N) },
	sh_build_args(Xs,Rs,R0,R1,Lvl,_,U0,U1),
	sh_move_to_hard_regs(Rs,As),
	['Call'(P,N,lco=T)].

% sh_build_args(Terms,Regs,RegDict,NewRegDict,Lvl,LvlAfter,Use,UseTail)

sh_build_args([],[],R,R,Lv,Lv,U,U) --> [].

sh_build_args([X|Xs],[R|Rs],R0,R2,Lv0,Lv2,U0,U2) -->
	sh_build_arg(X,R,R0,R1,Lv0,Lv1,U0,U1),
	sh_build_args(Xs,Rs,R1,R2,Lv1,Lv2,U1,U2).

% Build a list of soft and hard registers. Results are built in
% the soft registers, and then moved to hard ones. This move can
% often be collapsed by the register allocator, and avoids a problem
% with using hard registers directly: a hard register might be occupied
% when set, which means an error. (E.g. append/3 usually puts the tail
% of lists directly in the argument register, but with lock/unlock
% code this means the reg. allocator can't do its job w/o a lot of
% shuffling.)

target_reg_list(M,N,[],[]) :- N < M.

target_reg_list(M,N,[x(M)|Xs],[x(M)|As]) :-  % change 1st x(M) to get old way
	M =< N,
	M1 is M+1,
	target_reg_list(M1,N,Xs,As).

% This one moves the temp register into the hard one. As said
% previously, the move can often but not always be collapsed.

sh_move_to_hard_regs([],[]) --> [].
sh_move_to_hard_regs([X|Xs],[A|As]) -->
	['Put_Value'(X,A)],
	sh_move_to_hard_regs(Xs,As).


%%%
%%    Inlining of primitive operations is done right in this file for the sake
%   of compactness. The differences with standard primitive inlining are:
%
% - some primitives are 'ghosts' that modify the computation state for later
%   useage, and disappear quietly.
%
% - primitive operations also take an extra argument list of argument registers
%   that must be instantiated or the primitive suspends.
%
%     The action taken is await_nonvar(_); if the requirement is
%   await_strictly_nonvar(_) or await_leftmost, this must be done explicitly
%   (with an instruction).
%

sh_inline_primitive(X,_,_,_,_,_,_,_,_,L,L) -->
	{ var(X), !, sys_error('sh_inline_primitive - var arg')	}.

sh_inline_primitive('$choice'(X),R0,R1,Lv0,Lv1,U0,U1,D0,D1,Left0,Left1) --> !,
	{
          D0 = D1, Lv0 = Lv1,
	  U0 = U1, Left0 = Left1,
	  lookup_location(X,R0,Loc)
        },
	( { Loc = void } ->
	    { R0 = R1 }
	; { Loc = new(x(I)) } ->
	    ['Choice_X'(I)],
	    { update_location(X,x(I),R0,R1) }
	; { Loc = new(y(I)) } ->
	    ['Choice_Y'(I)],
	    { update_location(X,y(I),R0,R1) }
	; { Loc = new(void) } ->
	    { R1 = R0 }
	; { Loc = x(I) } -> % these appear due to broken-out unifications?
	    ['Choice_X'(I)],
	    { R1 = R0 }
	; { Loc = y(I) } ->
	    ['Choice_Y'(I)],
	    { R1 = R0 }
	; { format(user_error,'% Bad argument to choice/1: ~q.~n',[Loc]), R1 = R0 }
	).

sh_inline_primitive('$early_cut',R0,R1,Lv0,Lv1,U0,U1,D0,D1,Left0,Left1) --> !,
	{
	  R0 = R1,
	  D0 = D1, Lv0 = Lv1,
	  U0 = U1, Left0 = Left1
	},
	['Cut'].

sh_inline_primitive('$early_cut'(X),R0,R1,Lv0,Lv1,U0,U1,D0,D1,Left0,Left1) --> !,
	{
	  R1 = R0,
	  D0 = D1, Lv0 = Lv1,
	  U0 = U1, Left0 = Left1,
	  lookup_location(X,R0,Loc)
	},
	( { Loc = x(R) } ->
	    ['Cut_X'(R)]
	; { Loc = y(R) } ->
	    ['Cut_Y'(R)]
	; { Loc = void } ->
	    { format(user_error,'% early_cut with void argument!~n',[]) }
	; { Loc = new(_) } ->
	    { format(user_error,'% Uninitialized arg to early_cut ~q~n',[Loc]) }
	).

sh_inline_primitive('$cut'(X),R0,R1,Lv0,Lv1,U0,U1,D0,D1,Left0,Left1) --> !,
	{
	  R1 = R0,
	  D0 = D1, Lv0 = Lv1,
	  U0 = U1, Left0 = Left1,
	  lookup_location(X,R0,Loc)
	},
	( { Loc = x(R) } ->
	    ['Cut_X'(R)]
	; { Loc = y(R) } ->
	    ['Cut_Y'(R)]
	; { Loc = void } ->
	    { format(user_error,'% $cut with void argument!~n',[]) }
	; { Loc = new(_) } ->
	    { format(user_error,'% Uninitialized arg to $cut -- ~q~n',[Loc]) }
	).

sh_inline_primitive('$initially_det',R0,R1,Lv0,Lv1,U0,U1,_D0,D1,Left0,Left1) --> !,
	{ R0 = R1, Lv0 = Lv1, U0 = U1, D1 = det, Left0 = Left1 }.

sh_inline_primitive('$initially_nondet',R0,R1,Lv0,Lv1,U0,U1,_D0,D1,Left0,Left1) --> !,
	{ R0 = R1, Lv0 = Lv1, U0 = U1, D1 = nondet, Left0 = Left1 }.

sh_inline_primitive('$det',R0,R1,Lv0,Lv1,U0,U1,_D0,D1,Left0,Left1) --> !,
	{ R0 = R1, Lv0 = Lv1, U0 = U1, D1 = det, Left0 = Left1 }.

sh_inline_primitive('$nondet',R0,R1,Lv0,Lv1,U0,U1,_D0,D1,Left0,Left1) --> !,
	{ R0 = R1, Lv0 = Lv1, U0 = U1, D1 = nondet, Left0 = Left1 }.

sh_inline_primitive('$get_level'(X),R0,R1,Lv0,Lv1,U0,U1,D0,D1,Left0,Left1) --> !,
	{ Lv0 = Lv1, U0 = U1, D0 = D1, Left0 = Left1 },
	{ lookup_location(X,R0,new(RegX)) },
	['Put_Value'(Lv0,RegX)],
	{ update_location(X,RegX,R0,R1) }.

sh_inline_primitive('$set_size'(X),R0,R1,Lv0,Lv1,U0,U1,D0,D1,Left0,Left1) --> !,
	{ R0 = R1, Lv0 = Lv1, U0 = U1, D0 = D1, Left0 = Left1 },
	{ lookup(size_counter,R0,SizeReg) },
	{ lookup_location(X,R0,RegX) },
	['Put_Value'(RegX,SizeReg)].

sh_inline_primitive('$get_size'(X),R0,R1,Lv0,Lv1,U0,U1,D0,D1,Left0,Left1) --> !,
	{ Lv0 = Lv1, U0 = U1, D0 = D1, Left0 = Left1 },
	{ lookup(size_counter,R0,SizeReg) },
	{ lookup_location(X,R0,new(RegX)) },
	['Put_Value'(SizeReg,RegX)],
	{ update_location(X,RegX,R0,R1) }.

sh_inline_primitive('C'(A,B,C),R0,R1,Lv0,Lv1,U0,U1,D0,D1,Left0,Left1) --> !,
	sh_inline_primitive((A=[B|C]),R0,R1,Lv0,Lv1,U0,U1,D0,D1,Left0,Left1).

sh_inline_primitive((X=Y),R0,R1,Lv0,Lv1,U0,U1,D0,D1,Left0,Left1) --> !,
	{ D0 = D1 },
	sh_inline_unify(X,Y,R0,R1,Lv0,Lv1,U0,U1,D0,Left0,Left1).

sh_inline_primitive(true,R0,R1,Lv0,Lv1,U0,U1,D0,D1,Left0,Left1) --> !,
	{ R0 = R1, Lv0 = Lv1, U0 = U1, D0 = D1, Left0 = Left1 }.

sh_inline_primitive(fail,R0,R1,Lv0,Lv1,U0,U1,D0,D1,Left0,Left1) --> !, ['Fail'],
	{ R0 = R1, Lv0 = Lv1, U0 = U1, D0 = D1, Left0 = Left1 }.

sh_inline_primitive(false,R0,R1,Lv0,Lv1,U0,U1,D0,D1,Left0,Left1) --> !, ['Fail'],
	{ R0 = R1, Lv0 = Lv1, U0 = U1, D0 = D1, Left0 = Left1 }.

sh_inline_primitive(Goal,R0,R1,Lv0,Lv1,U0,U1,D,D,Left0,Left1) -->
	{
          functor(Goal,P,N),
	  builtin_map(P,N,Builtin),
	  suspension_args(P/N,SuspArgs),
	  Goal =.. [_|Xs],
	  Builtin = Atom/Arity,
	  inline_op_code(Atom,Arity,_,FnOrPred,DefArg),
	  build_def_arg_list(FnOrPred,DefArg,Arity,UseDefList)
	},
	sh_build_builtin_args(UseDefList,Xs,Rs,1,SuspArgs,Ws,R0,R1,
	                      Lv0,Lv1,U0,U1,D,Left0,Left1),
	['Par_Builtin'(Builtin,Ws,_LiveX,Rs)].

% build_def_arg_list builds a list of 'used' or 'defined' atoms of length
% equal to arity of primop. 'defined' positions are overwritten and
% generate no code. 'used' positions are generated normally.

build_def_arg_list(predicate,_Def,N,UseDef) :-
	NewDef is N+1, % i.e. never appears
	build_define_use_list(1,N,NewDef,UseDef,[]).

build_def_arg_list(function,Def,N,UseDef) :-
	build_define_use_list(1,N,Def,UseDef,[]).

build_define_use_list(M,N,_D) --> { M > N,! }.

build_define_use_list(M,N,D) --> 
	{ M =< N, M1 is M+1 },
	( { M =:= D } ->
	  [defined]
	; [used]
	),
	build_define_use_list(M1,N,D).
	
% Builtin args:
% - if the argument may be suspended upon, check if that suspension
%   can be removed
% - if the argument already is in a temporary, do nothing
% - if arg is overwritten, allocate a temp but generate no code
% - otherwise, build argument in a new temp.

sh_build_builtin_args([],[],[],_N,_Susp,[],R,R,Lv,Lv,U,U,_D,L,L) --> [].

sh_build_builtin_args([Def|Defs],[X|Xs],[R|Rs],N,Susp,Ws,R0,R2,Lv0,Lv2,U0,U2,
	              D,Left0,Left2) -->
        { suspend_on_pos(N,X,R,Susp,Ws,WsTail,Left0,Left1), N1 is N+1 },
	sh_build_builtin_arg(Def,X,R,R0,R1,Lv0,Lv1,U0,U1),
	sh_build_builtin_args(Defs,Xs,Rs,N1,Susp,WsTail,R1,R2,
	                      Lv1,Lv2,U1,U2,D,Left1,Left2).

% Builtin arguments can be used or defined. (I.e. some builtins
% overwrite one arg reg) In the latter case, it has turned out to
% be important not to generate code for defined args. This is handled
% as follows: 
%   check Def, 
%   if 'defined' then no code (just allocate a temp and note that variable
%    is found there) UNLESS the argument is a non-variable. In that case,
%    generate code normally. (Better safe ...)
%   if 'used' then build argument. If the argument can be found in a temp,
%    generate no code. This is covered in sh_build_builtin_arg_elim_temp
%    below.

sh_build_builtin_arg(Def,_R,_R0,_R1,_Lv0,_Lv1,_U0,_U1) -->
	{ var(Def),!, sys_error('sh_build_builtin_arg/10: var arg') }.

sh_build_builtin_arg(defined,X,R,R0,R1,Lv0,Lv1,U0,U1) --> !,
	( { X = '$VAR'(_,_,_,_) } ->
	  { Lv0 = Lv1, U0 = U1,
	    lookup_location(X,R0,Loc),
	    ( Loc = new(Reg) ->
	      ( Reg = void -> 
		R = x(_)
	      ; R = Reg
	      ),
	      update_location(X,R,R0,R1)
	    ; sys_error('sh_build_builtin_arg/10: defined reg prev. used')
            )
	  }
	; sh_build_builtin_arg_elim_temp(X,R,R0,R1,Lv0,Lv1,U0,U1)
	).

sh_build_builtin_arg(used,X,R,R0,R1,Lv0,Lv1,U0,U1) --> !,
	sh_build_builtin_arg_elim_temp(X,R,R0,R1,Lv0,Lv1,U0,U1).

% Builtin args are generated as always: if the arg is in a
% temporary, use that temporary. Otherwise, move the arg into
% a fresh temporary. This is done in the standard way.

sh_build_builtin_arg_elim_temp(X,R,R0,R1,Lv0,Lv1,U0,U1) -->
	{ X = '$VAR'(N,_,_,_),!,
	  lookup(N,R0,Loc) },
	( { Loc = x(_) } ->
	  { R0 = R1, R = Loc, Lv0 = Lv1, U0 = U1 }
	; { R = x(_) },
	  sh_build_arg(X,R,R0,R1,Lv0,Lv1,U0,U1)
	).

sh_build_builtin_arg_elim_temp(X,R,R0,R1,Lv0,Lv1,U0,U1) -->
	{ R = x(_) },
	sh_build_arg(X,R,R0,R1,Lv0,Lv1,U0,U1).

% Suspending on an argument of a builtin:
% - if arg position is one to suspend on, and
%   arg is a variable, and
%   arg has uninstantiated par-type, and
%   arg is not local, then
% - add to suspension list. (Ws)

suspend_on_pos(N,X,R,Susp,Ws,WsTail,Left0,Left1) :-
	Left0 = Left1,
	( suspension_entry(N,Susp) ->
	  ( X = '$VAR'(_,P,_,L),
	    nonlocal(L),
	    uninst_t(P) ->
	    Ws = [R|WsTail]
	  ; Ws = WsTail
	  )
	; Ws = WsTail
	).

%

suspension_entry(N,Susp) :- member(N,Susp).
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Generate the arguments to a call. This can be done
% without synchronization, since the results won't be made public
% before the structure is fully built (in the standard WAM fashion).
%
% There are two differences from normal WAM code generation:
% - Some variables are found as offsets from a vector and the level
%   register, hence Lv0,Lv1,Lv2.
% - The prelude of a parallel call must know what globally available args
%   were used when code is generated to allocate registers properly. (U0,U1,U2)

sh_build_arg(X,Dest,E0,E1,Lv0,Lv1,U0,U1) -->  % Used for struct temporaries.
	{ var(X),! },
	{ lookup(X,E0,Src) },
	sh_move(Src,Lv0,Lv1,U0,U1,Dest),
	{ (( Src = new(R) ; Src = new ) ->
	    update(X,R,E0,E1)
	  ; E0 = E1
	  )
        }.

sh_build_arg(X,Dest,E0,E1,Lv0,Lv1,U0,U1) -->
	{ X  = '$VAR'(N,_,_,_),! }, % could be done better!
	{ lookup(N,E0,Src) },
	sh_move(Src,Lv0,Lv1,U0,U1,Dest),
	{ (( Src = new(R) ; Src = new ) ->
	     update(N,R,E0,E1)
	  ; E0 = E1
	  )
        }.

sh_build_arg(C,Dest,E0,E1,Lv0,Lv1,U0,U1) -->
	{ atomic(C),!, E0 = E1, Lv0 = Lv1, U0 = U1 },
	( { Dest = heap } ->
	  ['Unify_Constant'(C)]
	; ['Put_Constant'(C,Dest)]
	).

sh_build_arg([X|Xs],Dest,E0,E2,Lv0,Lv2,U0,U2) --> !,
	{
	  Dest \== heap
	},
	sh_build_struct_args([X,Xs],E0,E1,Lv0,Lv1,U0,U1,Ys),
	['Put_List'(Dest)],
	sh_build_struct_toplevel(Ys,E1,E2,Lv1,Lv2,U1,U2).

sh_build_arg(FX,Dest,E0,E2,Lv0,Lv2,U0,U2) --> 
	{
	  Dest \== heap,
	  FX =.. [P|Xs],
	  functor(FX,P,N)
	},
	sh_build_struct_args(Xs,E0,E1,Lv0,Lv1,U0,U1,Ys),
	['Put_Structure'(P/N,Dest)],
	sh_build_struct_toplevel(Ys,E1,E2,Lv1,Lv2,U1,U2).

%

sh_build_struct_toplevel([],E,E,Lv,Lv,U,U) --> [].

sh_build_struct_toplevel([X|Xs],E0,E2,Lv0,Lv2,U0,U2) -->
	( ( { atomic(X)
	    ; X = '$VAR'(_,_,_,_) } ) ->
	  sh_build_arg(X,heap,E0,E1,Lv0,Lv1,U0,U1)
	; ['Unify_Value'(X)], { E0 = E1, Lv0 = Lv1, U0 = U1 }
	),
	sh_build_struct_toplevel(Xs,E1,E2,Lv1,Lv2,U1,U2).

%

sh_build_struct_args([],E,E,Lv,Lv,U,U,[]) --> [].

sh_build_struct_args([X|Xs],E0,E2,Lv0,Lv2,U0,U2,[Y|Ys]) -->
	sh_build_struct_arg(X,E0,E1,Lv0,Lv1,U0,U1,Y),
	sh_build_struct_args(Xs,E1,E2,Lv1,Lv2,U1,U2,Ys).

%

sh_build_struct_arg(X,E0,E1,Lv0,Lv1,U0,U1,Y) -->
	{ X = '$VAR'(_,_,_,_),!, E0 = E1, Y = X, Lv0 = Lv1, U0 = U1
	}.

sh_build_struct_arg(X,E0,E1,Lv0,Lv1,U0,U1,Y) -->
	{ atomic(X),!, E0 = E1, Y = X, Lv0 = Lv1, U0 = U1 }.

sh_build_struct_arg(X,E0,E2,Lv0,Lv1,U0,U1,Reg) -->
	{ Reg = x(_), E0 = E1 },
	sh_build_arg(X,Reg,E1,E2,Lv0,Lv1,U0,U1).

% For variables only:
% sh_move/3 [DCG]  generates instructions to move the item from a
% location into another.
% The source destinations are: x(_), y(_), head(_,_), tail(_,_), global(_),
%   new(Reg) [Reg is one of x(_), y(_), void]
% The target destinations are: x(_), y(_), heap

sh_move(x(M),Lv,Lv,U,U,x(N)) --> !, ['Put_Value'(x(M),x(N))].

sh_move(x(M),Lv,Lv,U,U,y(N)) --> !, ['Get_Value'(y(N),x(M))]. % I think

sh_move(x(M),Lv,Lv,U,U,heap) --> !, ['Unify_Value'(x(M))].

sh_move(y(M),Lv,Lv,U,U,x(N)) --> !, ['Put_Value'(y(M),x(N))].

sh_move(y(M),Lv,NewLv,U,UT,y(N)) --> !,
	sh_move(y(M),Lv,NxtLv,U,U1,x(Tmp)),
	sh_move(x(Tmp),NxtLv,NewLv,U1,UT,y(N)).

sh_move(y(M),Lv,Lv,U,U,heap) --> !, ['Unify_Value'(y(M))].

sh_move(head(W,C),Lv,NewLv,[W|U0],U0,x(N)) --> !,
	sh_move_level_reg(Lv,NewLv),
	['Put_Nth_Head'(W,NewLv,C,x(N))].

sh_move(head(W,C),Lv,NewLv,[W|U0],U0,y(N)) --> !,
	sh_move(head(W,C),Lv,NxtLv,x(Tmp)),
	sh_move(x(Tmp),NxtLv,NewLv,y(N)).

sh_move(head(W,C),Lv,NewLv,[W|U0],U0,heap) --> !,
	sh_move_level_reg(Lv,NewLv),
	['Unify_Nth_Head'(W,NewLv,C)].

sh_move(tail(W,C),Lv,NewLv,[W|U0],U0,x(N)) --> !,
	sh_move_level_reg(Lv,NewLv),
	['Put_Nth_Tail'(W,NewLv,C,x(N))].

sh_move(tail(W,C),Lv,NewLv,[W|U0],U0,y(N)) --> !,
	sh_move(tail(W,C),Lv,NxtLv,x(Tmp)),
	sh_move(x(Tmp),NxtLv,NewLv,y(N)).

sh_move(tail(W,C),Lv,NewLv,[W|U0],U0,heap) --> !,
	sh_move_level_reg(Lv,NewLv),
	['Unify_Nth_Tail'(W,NewLv,C)].

 % NB. x(M) is NOT an xreg of the body process but of the prelude!
sh_move(global(M),Lv,Lv,[M|U0],U0,x(N)) --> !,['Put_Global_Arg'(M,x(N))].

sh_move(global(M),Lv,NewLv,[M|U0],U1,y(N)) --> !,
	sh_move(global(M),Lv,NxtLv,x(Tmp)),
	sh_move(x(Tmp),NxtLv,NewLv,U0,U1,y(N)).

 % NB. x(M) is NOT an xreg of the body process, but of the prelude!
sh_move(global(M),Lv,Lv,[M|U0],U0,heap) --> !, ['Unify_Global_Arg'(x(M))].

sh_move(new,Lv,Lv,U,U,x(N)) --> !, ['Put_Void'(x(N))].

sh_move(new,Lv,NewLv,U0,U2,y(N)) --> !,
	sh_move(new,Lv,NxtLv,U0,U1,x(Tmp)), 
	sh_move(x(Tmp),NxtLv,NewLv,U1,U2,y(N)).

sh_move(new,Lv,Lv,U,U,heap) --> !, ['Unify_Void'].

sh_move(new(x(M)),Lv,Lv,U,U,heap) --> !, ['Unify_Variable'(x(M))].

sh_move(new(x(M)),Lv,Lv,U,U,x(N)) --> !, ['Put_Variable'(x(M),x(N))].

sh_move(new(x(M)),Lv,Lv,U,U,y(N)) --> !, ['Put_Variable'(y(N),x(M))].

sh_move(new(y(M)),Lv,Lv,U,U,heap) --> !, ['Unify_Variable'(y(M))].

sh_move(new(y(M)),Lv,Lv,U,U,x(N)) --> !, ['Put_Variable'(y(M),x(N))].

sh_move(new(y(M)),Lv0,Lv2,U0,U2,y(N)) --> !,
	sh_move(new(y(M)),Lv0,Lv1,U0,U1,x(Tmp)),
	sh_move(x(Tmp),Lv1,Lv2,U1,U2,y(N)).

sh_move(new(void),Lv,NewLv,U0,U1,R) --> !, sh_move(new,Lv,NewLv,U0,U1,R).

%

make_arg_list(M,N) --> { M > N }, !.

make_arg_list(M,N) -->
	[x(M)],
	{ M1 is M+1 },
	make_arg_list(M1,N).

% sh_move_level_reg moves the level counter from its environment
% slot to a temporary if necessary. Note that 2nd arg must be unbound!

sh_move_level_reg(y(M),x(N)) --> !, ['Put_Value'(y(M),x(N))].

sh_move_level_reg(x(M),x(M)) --> [].

