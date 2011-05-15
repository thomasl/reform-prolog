%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  regalloc.pl - Register allocation and usage.
%		      
%
% Register allocation:
%
%   We allocate registers on a chunk-by-chunk basis. In the first chunk, the
% allocator starts at max(<head arity>,<call arity>) + 1 while in the rest of
% the chunks, it starts at <call arity> + 1.
%
%   During register allocation we also collect all live x-registers in each
% chunk so that we may ensure correct liveness information to the parallel
% builtin (Par_Builtin). This information is needed in order to allow garbage
% collection in cases when a call to a parallel builtin suspends.
%
%
% Register usage information:
%
%   We define a database describing what registers are used/defined/copied
% by each instruction. Each description should have the following format:
%
%   ud(Instruction) --> !, [<registers used,defined,copied>].
%
%   If an instruction does not use, define, or copy any registers, it should
% be left out. It will be caught by the default case.
%
% Note: There should be a _single_ clause for every instruction that _does_
%       use/define/copy registers.
%
%


%%%
%%  regalloc(+Code, +Arity) & regalloc(+Code)
%
%
regalloc(Code, N) :-
	take_chunk(Code, Chunk, M, Rest),
	regalloc_chunk(Chunk, N, M),
	regalloc(Rest).


regalloc([]) :- !.

regalloc(Code) :-
	take_chunk(Code, Chunk, M, Rest),
	regalloc_chunk(Chunk, M),
	regalloc(Rest).


%%%
%%  regalloc_chunk(+Code, +ArgsIn, +ArgsOut) & regalloc_chunk(+Code, +ArgsOut)
%
% regalloc_chunk/3
%
%   When allocating registers in the first chunk we have to consider all
% argument registers as live (initially). This is handled by an initial
% set of live x-registers (arguments).
%
% regalloc_chunk/2
%
%   During register allocation to successive chunks, no x-registers are
% considered live on entry (into a chunk). The initial x-register set is
% thus empty.
%
regalloc_chunk(Code, N, M) :-
	def_arg_regs(N, InitRegDef),
	live_arg_regs(N, LiveRegSet),
	ud_chunk(Code, LiveRegSet, UD, InitRegDef),
	first_last(UD, [], _, FirstLast, []),
	max(N, M, MaxReg),
	assign_names(FirstLast, MaxReg).

	
regalloc_chunk(Code, M) :-
	ud_chunk(Code, [], UD, []),
	first_last(UD, [], _, FirstLast, []),
	MaxReg = M,
	assign_names(FirstLast, MaxReg).


%%%
%%  
% 
% Note: Argument N is mapped into x-register N - 1.
%
def_arg_regs(0, RegDef) :- !,
	RegDef = [].
def_arg_regs(1, RegDef) :- !,
	RegDef = [d(0)].
def_arg_regs(N, RegDef) :- N > 1,
	M is N - 1,
	RegDef = [d(M)|Rest],
	def_arg_regs(M, Rest).


live_arg_regs(0, RegSet) :- !,
	RegSet = [].
live_arg_regs(N, RegSet) :- N > 0,
	M is N - 1,
	RegSet = [x(M)|Rest],
	live_arg_regs(M, Rest).

%%%

take_chunk([],[],0,[]).
take_chunk(['Proceed'|Xs],['Proceed'],0,Xs) :- !.
take_chunk(['Execute'(P,N)|Xs],['Execute'(P,N)],N,Xs) :- !.
take_chunk(['Call'(P,N,LCO)|Xs],['Call'(P,N,LCO)],N,Xs) :- !.
take_chunk(['Jump'(L)|Xs],['Jump'(L)],0,Xs) :- !.
take_chunk([X|Xs],[X|Ys],N,Zs) :-
	take_chunk(Xs,Ys,N,Zs).


%%%
%%   ud_chunk(+Chunk, +LiveRegSet, -/+UseDefChain)
%
%
ud_chunk(    [],_LiveRegSet, NIL, NIL).
ud_chunk([X|Xs], LiveRegSet, UDC, Hdl) :-
	ud(X, UDC, Tmp),
	live_reg_set(UDC, NewRegSet, LiveRegSet),
	( X = 'Par_Builtin'(_Op,_SuspList, LiveX,_ArgList) ->
	    LiveX = LiveRegSet
	; true ),
	ud_chunk(Xs, NewRegSet, Tmp, Hdl).


live_reg_set(NIL) --> { var(NIL) }, !, [].
live_reg_set([X|Xs]) -->
	live_udc(X),
	live_reg_set(Xs).


live_udc(u(R)) --> [x(R)].
live_udc(d(R)) --> [x(R)].
live_udc(c(S,T)) --> [x(S),x(T)]. %%% "Source to Target".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%   This is the instruction register u-d-c (use-define-copy) database.
%
%
ud(X) --> { var(X) }, !, 
	{ sys_error('Unbound variable appears as instruction (regalloc)') }.

%%% GET instructions:

ud('Get_Variable'(R,A)) --> !,           % if R temp, copy A. Else use A
	{ reg_name_of(A,I) },
	copy_if_temp_else_use(R,I).
ud('Get_Value'(R,A)) --> !,
	use(A),
	use_if_temp(R).
ud('Get_Constant'(_,A)) --> !,
	use_if_temp(A).
ud('Get_Nil'(A)) --> !,
	use_if_temp(A).
ud('Get_Structure'(_,A)) --> !,
	use_if_temp(A).
ud('Get_List'(A)) --> !,
	use_if_temp(A).
ud('Get_Constant_X0') --> !,
	[u(0)].
ud('Get_Nil_X0') --> !,
	[u(0)].
ud('Get_Structure_X0'(_)) --> !,
	[u(0)].
ud('Get_List_X0') --> !,
	[u(0)].

%%% UNIFY instructions:

ud('Unify_Variable'(R)) --> !,
	def_if_temp(R).
ud('Unify_Value'(R)) --> !,
	use_if_temp(R).
ud('Unify_Local_Value'(R)) --> !,
	use_if_temp(R).
ud('Unify_Constant'(_)) --> !, [].
ud('Unify_Nil') --> !, [].
ud('Unify_List') --> !, [].
ud('Unify_Structure'(_)) --> !, [].
ud('Unify_Void') --> !, [].
ud('Unify_Void'(_)) --> !, [].

%%% PUT instructions:

ud('Put_Variable'(R,A)) --> !,
	def(A),
	def_if_temp(R).
ud('Put_Value'(R,A)) --> !,             % if R temp, copy to A. Else, def A.
	{ reg_name_of(A,I) },
	copy_if_temp_else_def(R,I).
ud('Put_Unsafe_Value'(R,A)) --> !,
	def(A),
	use_if_temp(R).
ud('Put_Constant'(_,A)) --> !,
	def(A).
ud('Put_Nil'(A)) --> !,
	def(A).
ud('Put_Structure'(_,A)) --> !,
	def_if_temp(A).
ud('Put_List'(A)) --> !,
	def_if_temp(A).
ud('Put_Void'(A)) --> !,
	def_if_temp(A).

%%%
%%  New scheme supporting compilation of dynamic predicates.
%
% Note: Not supported yet.
%
/*******************************************************************************
ud('Read_List_Top'(R,_)) --> !,
	use(R).
ud('Read_Struct_Top'(_,R,_)) --> !,
	use(R).
ud('Read_List'(R,_)) --> !,
	use(R).
ud('Read_Struct'(_,R,_)) --> !,
	use(R).

%%% Note: Read_List_Tail and Read_Struct_Tail do not use registers.

ud('Unify_Variable_Up'(R)) --> !,
	def_if_temp(R).
ud('Unify_Value_Up'(R)) --> !,
	use_if_temp(R).
ud('Unify_Local_Value_Up'(R)) --> !,
	use_if_temp(R).
ud('Write_List_Top'(R,_)) --> !,
	def(R).
ud('Write_Struct_Top'(_,R,_)) --> !,
	def(R).
*******************************************************************************/

%%% RECURSION PARALLELISM instructions:

ud('Build_Rec_Poslist'(A,S,V,T)) --> !,
	use(A), def(S), def(V), def(T).
ud('Build_Poslist'(A,S,V,T)) --> !,
	use(A), use(S), def(V), def(T).
ud('Build_Poslist_Value'(A,S,V,T)) --> !,
	use(A), use(S), use(V), def(T).
ud('Build_Neglist'(A,S,V,T)) --> !,
	use(A), use(S), def(V), def(T).
ud('Build_Neglist_Value'(A,S,From,To,H)) --> !,
	use(A), use(S), use(From), def(To), def(H).
ud('Build_Variables'(A,S,V,T)) --> !,
	use(A), use(S), def(V), def(T).

ud('Put_Nth_Head'(_V,L,_Off,Dst)) --> !,
	use(L), def(Dst).
ud('Put_Nth_Tail'(_V,L,_Off,Dst)) --> !,
	use(L), def(Dst).
ud('Unify_Nth_Head'(_V,L,_Off)) --> !,
	use(L).
ud('Unify_Nth_Tail'(_V,L,_Off)) --> !,
	use(L).

ud('Put_Global_Arg'(_G,R)) --> !,
	def(R).
ud('Unify_Global_Arg'(_G)) --> !, [].

ud('Start_Left_Body'(_L,Uses)) --> !,
	use_all(Uses).
ud('Start_Right_Body'(_Sz,_L,Uses)) --> !,
	use_all(Uses).

%%% [TL] I _think_ R in Initialize_Right is supposed to be allocated
%%%      by the prologue - hence, nothing done for that instruction.

ud('Initialize_Left'(_)) --> !, [].
ud('Initialize_Right'(_,_R)) --> !, [].   % def(R). ???

ud('Spawn_Left'(_,L,_)) --> !,
	def(L).
ud('Spawn_Right'(_,L)) --> !,
	def(L).

ud('Await_Nonvar'(R)) --> !,
	use(R).
ud('Await_Strictly_Nonvar'(R)) --> !,
	use(R).
ud('Await_Nonvar') --> !, [].
ud('Await_Strictly_Nonvar') --> !, [].
ud('Await_Leftmost') --> !, [].

ud('Lock_And_Get_Structure'(_,R,Tmp)) --> !,
	use(R), def(Tmp).
ud('Lock_And_Get_List'(R,Tmp)) --> !,
	use(R), def(Tmp).

ud('Lock_And_Unify_Structure'(_,Tmp)) --> !,
	def(Tmp).
ud('Lock_And_Unify_List'(Tmp)) --> !,
	def(Tmp).

ud('Unlock'(Tmp)) --> !,
	use(Tmp).
ud('Unlock'(R,Tmp)) --> !,
	use(R), use(Tmp).

%%% OTHER instructions:

ud('Allocate') --> !, [].
ud('Allocate'(_)) --> !, [].

ud('Deallocate') --> !, [].

ud('Choice'(R)) --> !,
	def_if_temp(R).
ud('Choice_X'(R)) --> !, [d(R)]. %%% Note: we define a register not yet assiged.
ud('Choice_Y'(_)) --> !, [].

ud('Cut') --> !, [].
ud('Cut'(R)) --> !,
	use_if_temp(R).
ud('Cut_X'(R)) --> !, [u(R)]. %%% Note: we use a register not yet assiged.
ud('Cut_Y'(_)) --> !, [].

%%% Note: Meta_Call and Meta_Execute appear after this pass.

ud('Inline'(PN, _, ArgList)) --> !,
	builtin_x_args(PN, ArgList).
ud('Builtin'(PN, ArgList)) --> !,
	builtin_x_args(PN, ArgList).
ud('Par_Builtin'(PN, SuspList,_LiveX, ArgList)) --> !,
	use_x_args(SuspList),
	builtin_x_args(PN, ArgList).

ud('Call'(_,N,_)) --> !, { M is N-1 },
	use_all(0,M).
ud('Execute'(_,N)) --> !, { M is N-1 },
	use_all(0,M).
ud('Proceed') --> !, [].

ud('Fail') --> !, [].
ud('Jump'(_)) --> !, [].
ud('Label'(_)) --> !, [].

ud('Require_Using'(_,_)) --> !, [].
ud('Require'(_)) --> !, [].
ud('Init'(_,_)) --> !, [].


%%% PSEUDO instructions:

ud(comment(_)) --> !, [].

ud(define_reg(X)) --> !,
	( { var(X) } ->
	    []
	; { X = y(_) } ->
	    []
	; { X = x(R) } ->
	    [d(R)]
	).

%%% Default case.

ud(X) --> { sys_error('ud: unknown instruction ~q in regalloc', [X]) }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reg_name_of(X,_) :- var(X), !, fail.

reg_name_of(x(I), I).

reg_name_of(a(I), I).

%

copy_if_temp_else_use(X,_) --> { var(X), !, fail }.

copy_if_temp_else_use(x(J), I) --> !,
	[c(I,J)].

copy_if_temp_else_use(a(J), I) --> !,
	[c(I,J)].

copy_if_temp_else_use(y(_), I) --> !,
	[u(I)].

% Note that we do c(J,I) here but c(I,J) above. Not so weird. Just
% different usages.

copy_if_temp_else_def(X,_) --> { var(X), !, fail }.

copy_if_temp_else_def(x(J), I) --> !,
	[c(J,I)].

copy_if_temp_else_def(a(J), I) --> !,
	[c(J,I)].

copy_if_temp_else_def(y(_), I) --> !,
	[d(I)].

%

def(X) --> { var(X), !, sys_error('Variable in def/1 (regalloc)',[]) }.

def(x(I)) --> !, [d(I)].

def(a(I)) --> !, [d(I)].

def(X) --> { sys_error('Improper register in def/1 ~q (regalloc)',[X]) }.

%

def_if_temp(X) -->
	{ var(X), !, sys_error('Variable in def_if_temp/1 (regalloc)',[]) }.

def_if_temp(x(I)) --> !, [d(I)].

def_if_temp(a(I)) --> !, [d(I)].

def_if_temp(y(_)) --> !, [].

def_if_temp(X) --> 
	{ sys_error('Improper register in def_if_temp/1 ~q (regalloc)',[X]) }.

%

use(X) --> { var(X), !, sys_error('Variable in use/1 (regalloc)',[]) }.

use(x(I)) --> !, [u(I)].

use(a(I)) --> !, [u(I)].

use(X) --> { sys_error('Improper register in use/1 ~q (regalloc)',[X]) }.

%

use_if_temp(X) --> 
	{ var(X), !, sys_error('Variable in use_if_temp/1 (regalloc)',[]) }.

use_if_temp(x(I)) --> !, [u(I)].

use_if_temp(y(_)) --> !, [].

use_if_temp(X) --> 
	{ sys_error('Improper register in use_if_temp/1 ~q (regalloc)',[X]) }.

%

builtin_x_args(P/N, Xs) -->
	{ inline_op_code(P,N,_,FnOrPred,DefArg) },
	( { FnOrPred == function } ->
	    { select_nth_1(DefArg,Xs,Uses,Def) },
	    use_x_args(Uses),
	    def_if_temp(Def)
	; { FnOrPred == predicate } ->
	    use_x_args(Xs)
	; { sys_error('builtin_x_args: regalloc primop ~q as ~q', [P/N,FnOrPred]) }
	).

% Select n'th arg, with start at 1.

select_nth_1(1,XXs,Uses,Def) :- !,
	XXs = [Def|Uses].

select_nth_1(N,XXs,Uses,Def) :- N > 1,
	XXs = [X|Xs],
	Uses = [X|Uses0],
	M is N-1,
	select_nth(M,Xs,Uses0,Def).

%

use_x_args(    []) --> [].
use_x_args([X|Xs]) -->
	( { X = x(I) } -> [u(I)] ; [] ),
	use_x_args(Xs).

%

use_all(M,N) -->
	( { M > N } ->
	    []
	; [u(M)],
	    { M1 is M+1 },
	    use_all(M1,N)
	).

%

use_all(    []) --> [].
use_all([X|Xs]) -->
	use_if_temp(X),
	use_all(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Compute the first and last uses of a variable.
%
%   If a register is redefined, then the preceding use is the last use.
% This only happens to hard registers, since other regs are never redefined.
%

first_last(    [],_Prev,   []) --> [].

first_last([X|Xs], Prev, Succ) -->
	( { X = u(R) } ->
	  [Action],
	  first_last(Xs,[R|Prev],Succ1),
	  { Succ = [R|Succ1],
	    ( does_not_occ(Succ1,R) ->
	      Action = last(R)
	    ; Action = none
	    )
	  }
	; { X = d(R) } ->
	  ( { does_occ(Prev,R) } ->
	    [last(R)]
	  ; []
	  ),
	  [first(R),Action],
	  first_last(Xs,[R|Prev],Succ1),
	  { does_not_occ(Succ1,R) ->
	    Action = last(R)
	  ; Action = none
	  },
	  { delete_if_hard(R,Succ1,Succ) }
	; { X = c(R1,R2) } ->
	  [Action],
	  first_last(Xs,[R1,R2|Prev],Succ1),
	  ( { does_not_occ(Succ1,R1) } ->
	    { Action = last_copy(R1,R2) }
	  ; { Action = copy(R1,R2) }
	  ),
	  { delete_if_hard(R2,Succ1,Succ2), 
	    Succ = [R1|Succ2] } % Succ=[R1,R2|Succ1]
	).

%

delete_if_hard(X,Succ0,Succ1) :- var(X), !,
	Succ0 = Succ1.
delete_if_hard(X,Succ0,Succ1) :- integer(X), !,
	delete_occs(Succ0, X, Succ1).

delete_occs([],_,[]).
delete_occs([X|Xs],R,Ys) :-
	( var(X) ->
	    Ys = [X|Ys0]
	; integer(X) ->
	    ( X =:= R -> Ys = Ys0 ; Ys = [X|Ys0] )
	),
	delete_occs(Xs,R,Ys0).

%

does_not_occ(    [],_R).
does_not_occ([X|Xs], R) :- X \== R,
	does_not_occ(Xs, R).

does_occ([X|Xs], R) :- X \== R, !,
	does_occ(Xs, R).
does_occ([X|_], R) :- X == R.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%   Assignment of hard register names.
%
% Case analysis:
%
% - if first(X):
%   Add X to live regs
%   Search for last occurence:
%     last(X) -> use new name
%     last_copy(X,Y) ->
%       if Y is already assigned (e.g. arg reg) and not live during X,
%       assign X = Y. Otherwise, use a new name.
%     no last occurence ->
%       this is a void parameter (appears due to build-insns, e.g.)
%
% - if last(X): check that X assigned, delete X from live regs
%
% - if copy(X,Y):
%   Add Y to live regs.
%   If X is not assigned during lifetime of Y, assign Y = X
%   Otherwise, use a new name.
%
% - if last_copy(X,Y):
%   Delete X from live regs. If X not defined during Y, assign Y = X.
%   Add Y to live regs.
%
% Note: if a register has already been assigned (e.g. first(0), and so on)
%       then nothing happens.
%
% Note: This algorithm is O(N^2) in the number of registers, I think. It might
%       be possible to make it O(N log N) by using a dictionary for live regs?
%

assign_names(Lifetimes,Supply) :-
	assign_names(Lifetimes,[],Supply).

%

assign_names([],_,_).

assign_names([Life|Xs],LiveRegs,Supply) :-
	( Life = first(R) ->
	    assign_reg_name(R,Xs,LiveRegs,NewLiveRegs,Supply,NewSupply)
	; Life = none -> % dummies introduced for simplicity in first_last.
	    NewSupply = Supply, NewLiveRegs = LiveRegs
	; Life = last_copy(R1,R2) ->
	    ( integer(R1) ->
		delete_all_occurences(LiveRegs,R1,TmpLiveRegs),
		NewLiveRegs = [R2|TmpLiveRegs],
		( var(R2) ->
		    possibly_collapse_copy(Xs,R1,R2,Supply,NewSupply)
		; true,
		    NewSupply = Supply
		)
	    ; true,
	        sys_error('register has last_copy occurence but not first')
	    )
	; Life = last(R) ->
	    ( integer(R) ->
		NewSupply = Supply,
		delete_all_occurences(LiveRegs,R,NewLiveRegs)
	    ; true,
	        sys_error('register has last occurence but not first')
	    )
	; Life = copy(R1,R2) ->
	    ( integer(R1) ->
		NewLiveRegs = [R2|LiveRegs],
		( var(R2) ->
		    possibly_collapse_copy(Xs,R1,R2,Supply,NewSupply)
		; true,
	            NewSupply = Supply
		)
	    ; true,
	        sys_error('register has copy occurence but not first')
	    )
	),
	assign_names(Xs,NewLiveRegs,NewSupply).


%%%
%%  A register X is assigned a name as follows:
%
% - if X is assigned already, do nothing.
% - if lifetime ends with last(X), then take any name (consider copy(_,_)?)
% - if lifetime ends with last_copy(X,Y) and Y is assigned, then assign X = Y
%   if Y is not live during X:s lifetime.
% - otherwise, X gets a new name.
%

assign_reg_name(R,_Life,Live,NewLive,Supply,NewSupply) :- integer(R), !,
	NewLive = [R|Live],
	NewSupply = Supply.

assign_reg_name(R,Life,Live,NewLive,Supply,NewSupply) :- var(R), !,
	NewLive = [R|Live],
	lifetime_ends_with(Life,R,End,_Copies,[]),
	( End = last_copy(_,Y) ->
	    ( does_occ(Live,Y) ->
		R = Supply, NewSupply is Supply+1
	    ; true,
	        R = Y, 
		( integer(R) ->
		    NewSupply = Supply
		; true,
		    R = Supply, NewSupply is Supply+1
		)
	    )
	; End = last(_) -> % here, Copies can be used for better choice!
	    R = Supply, NewSupply is Supply+1
	).

%%%
%%
%   Find the terminating occurence of the live range of R. Also record any copy
% operations where R is the source. These can be used to avoid some useless
% instruction.
%   Some definitions have no use, appearing due to build-insns. This is handled
% by a 'non-ended' lifetime getting an implicit last(R) when the list is empty.
%

lifetime_ends_with([],_,_) --> 
	{ sys_error('register has no last use!') }.
%	{ End = last(R) }.

lifetime_ends_with([X|Xs],R,End) -->
	( { X = last_copy(R1,_), R1 == R } ->
	    { End = X }
	; { X = last(R1), R1 == R } ->
	    { End = X }
	; { X = copy(R1,R2), R1 == R } ->
	    [R2],
	    lifetime_ends_with(Xs,R,End)
	; { true },
	    lifetime_ends_with(Xs,R,End)
	).
	  
%%%
%%
%   A copy can be collapsed (i.e. R2 = R1):
%
% - if R2 is a variable (checked above).
% - if R1 is not defined during the lifetime of R2
%   Case analysis:
%      R1 is defined by first(_), copy(_,_), last_copy(_,_)
%      R2 dies by last(_) and last_copy(_,_).
%
% - otherwise, R2 gets a new name.
%
% Note: If the list is [] (first clause), R1 has not been used or defined
%       after the copy instruction. Hence, the copy can be collapsed.
%

possibly_collapse_copy([],R1,R2,Supply,NewSupply) :-
	R1 = R2, Supply = NewSupply.

possibly_collapse_copy([Life|Xs],R1,R2,Supply,NewSupply) :-
	( Life = first(X) ->
	  ( X == R1 ->
	    R2 = Supply, NewSupply is Supply+1
	  ; possibly_collapse_copy(Xs,R1,R2,Supply,NewSupply)
	  )
	; Life = copy(_,X) ->
	  ( X == R1 ->
	    R2 = Supply, NewSupply is Supply+1
	  ; possibly_collapse_copy(Xs,R1,R2,Supply,NewSupply)
	  )
	; Life = last(Y) ->
	  ( Y == R2 ->
	    R1 = R2, Supply = NewSupply
	  ; possibly_collapse_copy(Xs,R1,R2,Supply,NewSupply)
	  )
	; Life = last_copy(Y,X) ->
	  ( Y == R2 ->
	    R1 = R2, Supply = NewSupply
	  ; X == R1 ->
	    R2 = Supply, NewSupply is Supply+1
	  ; possibly_collapse_copy(Xs,R1,R2,Supply,NewSupply)
	  )
	; Life = none ->
	  possibly_collapse_copy(Xs,R1,R2,Supply,NewSupply)
	).

%

delete_all_occurences(    [],_,[]).
delete_all_occurences([X|Xs],R,Ys) :-
	( X == R -> 
	    Ys = Ys0 
	; true,
	    Ys = [X|Ys0]
	),
	delete_all_occurences(Xs,R,Ys0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%
%
test_reg(1) :-
	Code = ['Lock_And_Get_List'(x(0),x(A)),
	        'Unify_Variable'(x(B)),
		'Unify_Variable'(x(C)),
		'Unlock'(x(0),x(A)),
		'Lock_And_Get_List'(x(2),x(D)),
		'Unify_Value'(x(B)),
		'Unify_Variable'(x(E)),
		'Unlock'(x(2),x(D)),
		'Put_Value'(x(C),x(F)),
		'Put_Value'(x(1),x(G)),
		'Put_Value'(x(E),x(H)),
		'Put_Value'(x(F),x(0)),
		'Put_Value'(x(G),x(1)),
		'Put_Value'(x(H),x(2)),
		'Call'(append,3,lco=0),
		'Proceed'],
	regalloc(Code,3),
	pp_list(Code).

test_reg(2) :-
	Code = ['Allocate'(5),
	        'Build_Rec_Poslist'(x(0),x(B),x(C),x(D)),
		'Build_Variables'(x(1),x(B),x(E),x(F)),
		'Put_Value'(x(D),x(0)),
		'Put_Value'(x(F),x(1)),
		'Get_Variable'(y(1),x(E)),
		'Get_Variable'(y(2),x(C)),
		'Get_Variable'(y(3),x(B)),
		'Call'(nrev,2,lco=0),
		'Put_Value'(y(1),x(G)),
		'Put_Value'(y(2),x(H)),
		'Put_Value'(y(3),x(I)),
		'Start_Right_Body'(x(I),J,[x(I),x(G),x(H),x(G)]),
		'Deallocate',
		'Proceed',
		'Label'(J),
		'Initialize_Right'(-1,x(I)),
		'Label'(K),
		'Spawn_Right'(-1,x(3)),
		'Require_Using'(3,0),
		'Put_Nth_Head'(x(G),x(3),1,x(0)),
		'Put_List'(x(1)),
		'Unify_Nth_Head'(x(H),x(3),0),
		'Unify_Nil',
		'Put_Nth_Head'(x(G),x(3),0,x(2)),
		'Init'(0,[]),
		'Call'(append,3,lco=1),
		'Require'(0),
		'Jump'(K)],
	regalloc(Code,2),
	pp_list(Code).
