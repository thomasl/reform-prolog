%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  unsafe.pl
%
%
% Description:
%
%   This pass is responsible for removing possible references to stack
% variables after they have disappeared.
%
% The following algorithm is used:
%
%  o   put_variable(R,_)	R is safe if temporary
%  o   put_structure(_,R)	R is safe
%  o   put_list(R)		R is safe
%
%  o   get_variable(R1,R2)	R1 is safe if R2 is safe
%  o   get_structure(_,R)	R is safe (+)
%  o   get_list(R)		R is safe (+)
%
%  o   unify_variable(R)	R is safe
%
%  o   lock_and_get_*		as get_*
%
%  o   build_*			all arguments are safe
%
% otherwise, R is unsafe.
%
% (+) Note: (26/11-94)
%   Treated as nonsafe while waiting for consensus.
%
%   Note that e.g. get_constant(_,R) yields an unsafe register, however,
% R is usually (always) dead afterwards so this will not have any (negative)
% impact on the code generated.
%
%   The strategy (of tracking safe registers instead of unsafe ditto) is used
% because we may optimize away initializations, tracking unsafe registers thus
% become more awkward than tracking safe ones.
%
% Generating safe(ty) code:
%
%  o   put_value(R1,R2)		put_unsafe_value(R1,R2) if R1 is unsafe and
%				this is the _last_ such occurrence.
%
%  o   unify_value(R1)		unify_local_value(R) if R is unsafe
%
%  o   call			flushes the cached x-registers	(++)
%
% (++) Note: Is it an error to have any x-registers remaining?
%
%
%   Also found in this pass is the introduction of the Init-instruction in
% order to initialize all unused permanent variables (y-registers). This has
% to be done prior to any point where garbage collection may occur when there
% are still slots (y-registers) which has not been referenced, i.e. Call,
% Start_Left_Body, Start_Right_Body and Par_Builtin.
%
%   Initialisation prior to a Par_Builtin is needed in order to allow garabage
% collection during a suspended parallel builtin. The set of temporary (x-)
% registers live at each parallel builtin has already been calculated (during
% register allocation and assignment). To support the garabage collector we
% also have to supply each parallel builtin with the size of the environment.
% We thus calculate the live portion of the environment (as the largest index
% among the permanent registers seen at each point) during this pass.
%   Each initialisation may then be treated as a partial operation, only
% affecting the live portion of the environment. This enables us to avoid
% many initalisations that would otherwise be introduced (redundantly).
%
%   As a consequence of the Init-instructions introduced, references to
% permanent registers that use to be "primary occurences" may no longer be
% treated as such. These mal classified references may cause the garbage
% collector to havoc since they are not trailed and thus not restored (to
% "initialized") on backtracking. Our solution is to treat all initialized
% permanent registers as "secondary occurances" after a Call (i.e. after
% the first choice-point possible). Reclassified references are thus
% transformed from Variable to Value ditto.
%
%
% Note: Meta_Call is not present at this point, but introduced during
%       peep-hole optimization (as a Call to a call).
%
%
% Note: We use -1 to represent the lack of an environment.
%

safe_code(Code, SafeCode) :-
	safe_code(Code, -1, [], 0, [], [], [],_RSX,_RSY, SafeCode, []).


%%%
%%  safe_code(+Code,+ES,+PR,+In,+SX,+SY,+PY,-RSX,-RSY) --> SafeCode
%
%  +Code			is the code being checked for safety
%  +ES (EnvironmentSize)	is the size of the current environment
%  +PR (PermanentRegisters)	are the permanent registers seen so far
%				(these are used to suppress initialization)
%  +In (Initialised)		is the size of the initialised environment
%				so far, i.e. the GC-safe (initialised) portion
%				of the permanent registers
%  +SX (SafeXs)			are the temporary registers determined to be
%				safe so far
%  +SY (SafeYs)			are the permanent variables determined to be
%				safe so far (this is a subset of PR)
%  +PY (PrevYs)			are the permanent registers that have been seen
%				in the current "block" by Put_Value previously
%  -RSX (RealSafeXs)		are the temporary registers that are safe
%  -RSY (RealSafeYs)		are the permanent registers that are safe
%
%   SafeCode			is the safe code sequence corresponding to Code
%
%
safe_code(    [],_ES,_PR,_In, SX, SY,_PY,  SX,  SY) --> [].

safe_code([X|Xs], ES, PR, In, SX, SY, PY, RSX, RSY) -->
	safe_code(X, ES, PR, In, SX, SY, PY, RSX, RSY, Xs).

%%%

safe_code('Allocate'(Size),_ES,_PR,_In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Allocate'(Size)],
	safe_code(Xs,Size,[],0,SX,SY,PY,RSX,RSY).

safe_code('Call'(P,N,LCO),_ES,PR,In,SX,SY,_PY,RSX,RSY,Xs) --> !,
	{
	  LCO = (lco = Size)
	},
	initialize_permanents(PR,In,Size,NewPR,NewIn),
	['Call'(P,N,LCO)],
	safe_code(Xs,Size,NewPR,NewIn,SX,SY,[],RSX,RSY).

safe_code('Choice_Y'(I),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Choice_Y'(I)],
        safe_code(Xs,ES,[I|PR],In,SX,SY,PY,RSX,RSY).

%%%
%% (+++) Note: Reclassifying permanent register references.
%
%   We use the PermanentRegister (PR) list to detect mal classified
% y-registers. This may introduce Value instructions where Variable
% ditto is correct, however no harm will be caused.
%
safe_code('Put_Variable'(x(I),A),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Put_Variable'(x(I),A)],
	safe_code(Xs,ES,PR,In,[I|SX],SY,PY,RSX,RSY).

safe_code('Put_Variable'(y(I),A),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	( { member(I,PR) } ->
	    safe_code('Put_Value'(y(I),A),ES,PR,In,SX,SY,PY,RSX,RSY,Xs)
	; { true },
	    { PYi = PY },
	    ['Put_Variable'(y(I),A)],
	    safe_code(Xs,ES,[I|PR],In,SX,SY,PYi,RSX,RSY)
	).

safe_code('Put_Value'(y(I),A),ES,PR,In,SX,SY,PY,RSX,NewRSY,Xs) --> !,
	[PutPossiblyUnsafe],
	safe_code(Xs,ES,[I|PR],In,SX,SY,[I|PY],RSX,RSY),
	{
          possibly_put_unsafe(I,A,PY,RSY,NewRSY,PutPossiblyUnsafe)
        }.

safe_code('Put_Void'(y(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Put_Void'(y(I))],
	safe_code(Xs,ES,[I|PR],In,SX,SY,PY,RSX,RSY).

safe_code('Put_Structure'(F,y(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Put_Structure'(F,y(I))],
	safe_code(Xs,ES,[I|PR],In,SX,[I|SY],PY,RSX,RSY).

safe_code('Put_Structure'(F,x(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Put_Structure'(F,x(I))],
	safe_code(Xs,ES,PR,In,[I|SX],SY,PY,RSX,RSY).

safe_code('Put_Constant'(F,y(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Put_Constant'(F,y(I))],
	safe_code(Xs,ES,[I|PR],In,SX,[I|SY],PY,RSX,RSY).

safe_code('Put_Constant'(F,x(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Put_Constant'(F,x(I))],
	safe_code(Xs,ES,PR,In,[I|SX],SY,PY,RSX,RSY).

safe_code('Put_List'(y(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Put_List'(y(I))],
	safe_code(Xs,ES,[I|PR],In,SX,[I|SY],PY,RSX,RSY).

safe_code('Put_List'(x(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Put_List'(x(I))],
	safe_code(Xs,ES,PR,In,[I|SX],SY,PY,RSX,RSY).


safe_code('Get_Variable'(R,A),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
        ( { R = y(I), member(I,PR) } ->
	    safe_code('Get_Value'(R,A),ES,PR,In,SX,SY,PY,RSX,RSY,Xs)
        ; { true },
	    ['Get_Variable'(R,A)],
            {
               safeness_of_reg(R,A,SX,SY,NewSX,NewSY,PR,NewPR)
            },
	    safe_code(Xs,ES,NewPR,In,NewSX,NewSY,PY,RSX,RSY)
	).

safe_code('Get_Structure'(F,y(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Get_Structure'(F,y(I))],
	safe_code(Xs,ES,[I|PR],In,SX,SY,PY,RSX,RSY).

safe_code('Get_Structure'(F,x(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Get_Structure'(F,x(I))],
	safe_code(Xs,ES,PR,In,SX,SY,PY,RSX,RSY).

safe_code('Get_Constant'(F,y(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Get_Constant'(F,y(I))],
	safe_code(Xs,ES,[I|PR],In,SX,[I|SY],PY,RSX,RSY).

safe_code('Get_Constant'(F,x(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Get_Constant'(F,x(I))],
	safe_code(Xs,ES,PR,In,[I|SX],SY,PY,RSX,RSY).

safe_code('Get_List'(y(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Get_List'(y(I))],
	safe_code(Xs,ES,[I|PR],In,SX,SY,PY,RSX,RSY).

safe_code('Get_List'(x(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Get_List'(x(I))],
	safe_code(Xs,ES,PR,In,SX,SY,PY,RSX,RSY).

%%%
%%  See note (+++).
%
safe_code('Unify_Variable'(x(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Unify_Variable'(x(I))],
	safe_code(Xs,ES,PR,In,[I|SX],SY,PY,RSX,RSY).

safe_code('Unify_Variable'(y(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	( { member(I,PR) } ->
	    safe_code('Unify_Value'(y(I)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs)
	; { true },
	    { PYi = PY },
	    ['Unify_Variable'(y(I))],
	    safe_code(Xs,ES,[I|PR],In,SX,[I|SY],PYi,RSX,RSY)
	).

safe_code('Unify_Value'(R),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	[UnifyPossiblyLocal],
	{
	  possibly_local_value(R,SX,SY,NewSX,NewSY,PR,NewPR,UnifyPossiblyLocal)
        },
	safe_code(Xs,ES,NewPR,In,NewSX,NewSY,PY,RSX,RSY).


safe_code('Par_Builtin'(Op,SuspList,LiveX,ArgList),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	pre_initialize_permanents(PR,In,LiveY,NewPR,NewIn),
	['Par_Builtin'(Op,SuspList,LiveX,LiveY,ArgList)],
	safe_code(Xs,ES,NewPR,NewIn,SX,SY,PY,RSX,RSY).

safe_code('Start_Left_Body'(Lbl,Uses),ES,PR,In,SX,SY,_PY,RSX,RSY,Xs) --> !,
	{
	  compute_live_registers(Xs,PR,ES,Uses,LiveT,LiveP)
        },
	initialize_permanents(PR,In,ES,NewPR,NewIn),
	['Start_Left_Body'(Lbl,LiveT,LiveP,Uses)],
	safe_code(Xs,ES,NewPR,NewIn,SX,SY,[],RSX,RSY).

safe_code('Start_Right_Body'(Size,Lbl,Uses),ES,PR,In,SX,SY,_PY,RSX,RSY,Xs) --> !,
	{
	  compute_live_registers(Xs,PR,ES,Uses,LiveT,LiveP)
        },
	initialize_permanents(PR,In,ES,NewPR,NewIn),
	['Start_Right_Body'(Size,Lbl,LiveT,LiveP,Uses)],
	safe_code(Xs,ES,NewPR,NewIn,SX,SY,[],RSX,RSY).

safe_code('Lock_And_Get_Structure'(F,x(I),x(J)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Lock_And_Get_Structure'(F,x(I),x(J))],
	safe_code(Xs,ES,PR,In,[I,J|SX],SY,PY,RSX,RSY).

safe_code('Lock_And_Get_List'(x(I),x(J)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Lock_And_Get_List'(x(I),x(J))],
	safe_code(Xs,ES,PR,In,[I,J|SX],SY,PY,RSX,RSY).

safe_code('Build_Rec_Poslist'(x(A),x(S),x(V),x(T)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Build_Rec_Poslist'(x(A),x(S),x(V),x(T))],
        safe_code(Xs,ES,PR,In,[A,S,V,T|SX],SY,PY,RSX,RSY).

safe_code('Build_Variables'(x(A),x(S),x(V),x(T)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Build_Variables'(x(A),x(S),x(V),x(T))],
        safe_code(Xs,ES,PR,In,[A,S,V,T|SX],SY,PY,RSX,RSY).

safe_code('Build_Poslist_Value'(x(A),x(S),x(V),x(T)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Build_Poslist_Value'(x(A),x(S),x(V),x(T))],
        safe_code(Xs,ES,PR,In,[A,S,V,T|SX],SY,PY,RSX,RSY).

safe_code('Build_Poslist'(x(A),x(S),x(V),x(T)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Build_Poslist'(x(A),x(S),x(V),x(T))],
        safe_code(Xs,ES,PR,In,[A,S,V,T|SX],SY,PY,RSX,RSY).

safe_code('Build_Neglist_Value'(x(A),x(S),x(V),x(W),x(T)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Build_Neglist_Value'(x(A),x(S),x(V),x(W),x(T))],
        safe_code(Xs,ES,PR,In,[A,S,V,W,T|SX],SY,PY,RSX,RSY).

safe_code('Build_Neglist'(x(A),x(S),x(V),x(T)),ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> !,
	['Build_Neglist'(x(A),x(S),x(V),x(T))],
        safe_code(Xs,ES,PR,In,[A,S,V,T|SX],SY,PY,RSX,RSY).


safe_code(X,ES,PR,In,SX,SY,PY,RSX,RSY,Xs) --> [X],
	safe_code(Xs,ES,PR,In,SX,SY,PY,RSX,RSY).


%%%
%%  possibly_put_unsafe(+Y,+X,+PrevYs,+SafeYs,-NewSafeYs,-Instruction)
%
%   Given a permanent variable which has not been seen previously in this "block",
% generate a safe(ty) instruction (Put_Unsafe_Value) and announce the corresponding
% variable (y-register) as safe.
%
% Note: +Y is a y-register number while +X is a register identifier.
%
possibly_put_unsafe(Y,X,PY,RSY,NewRSY,PutPossiblyUnsafe) :-
	( soft_member(Y,PY) ->	%%% Y has been seen previously.
	    NewRSY = RSY,
	    PutPossiblyUnsafe = 'Put_Value'(y(Y),X)
	; soft_member(Y,RSY) ->	%%% Y is among safe variables.
	    NewRSY = RSY,
	    PutPossiblyUnsafe = 'Put_Value'(y(Y),X)
	; true,			%%% Y has not been seen previously and is not safe.
	    NewRSY = [Y|RSY],
	    PutPossiblyUnsafe = 'Put_Unsafe_Value'(y(Y),X)
	).


%%%
%%  possibly_local_value(+R,+SafeXs,+SafeYs,-NewSafeXs,-NewSafeYs,+PR,-NewPR,-Instruction)
%
%
possibly_local_value(y(I),SX,SY,NewSX,NewSY,PR,NewPR,UnifyPossiblyLocal) :-
	NewSX = SX,
	NewPR = [I|PR],
	( soft_member(I,SY) -> %%% It's safe
	    NewSY = SY,
	    UnifyPossiblyLocal = 'Unify_Value'(y(I))
	; true,
	    NewSY = [I|SY],
	    UnifyPossiblyLocal = 'Unify_Local_Value'(y(I))
	).

possibly_local_value(x(I),SX,SY,NewSX,NewSY,PR,NewPR,UnifyPossiblyLocal) :-
	NewSY = SY,
	NewPR = PR,
	( soft_member(I,SX) -> %%% It's safe
	    NewSX = SX,
	    UnifyPossiblyLocal = 'Unify_Value'(x(I))
	; true,
	    NewSX = [I|SX],
	    UnifyPossiblyLocal = 'Unify_Local_Value'(x(I))
	).

possibly_local_value(z(N),SX,SY,NewSX,NewSY,PR,NewPR,UnifyNonLocal) :-
	NewSX = SX,
	NewSY = SY,
	NewPR = PR,
	UnifyNonLocal = 'Unify_Value'(z(N)).


%%%
%%  pre_initialize_permanents(+SeenPR,+Init,-Size,-NewSeenPR,-NewInit) --> Code
%
%
pre_initialize_permanents(PR, In, Size, NewPR, NewIn) -->
	{
          max_in_list(PR, Temp, -1), Size is Temp + 1
        },
	initialize_permanents(PR, In, Size, NewPR, NewIn).


%%%
%%  initialize_permanents(+SeenPR,+Init,+Size,-NewSeenPR,-NewInit) --> Code
%
%   Introduce the special 'Init' instruction that initializes a set of permanent
% registers not referenced prior to this point, but within the active portion of
% the environment (given by Size).
%
%
initialize_permanents(PR, In, Size, NewPR, Size) -->
	( { In < Size } -> %%% Not all registers are initialized.
	    initialize_permanents_aux(PR, Size, NewPR)
	; { true },
	    [],
	    { NewPR = PR }
	).
	

initialize_permanents_aux(PR, Size, NewPR) --> ['Init'(N,Uninits)],
	{
          uninitialized_permanents(0, Size, PR, NewPR, Uninits),
	  length(Uninits, N)
        }.


%%%
%%  Find all permanents that haven't been initialized.
%
uninitialized_permanents( I, Size, PR, NewPR, Us) :- I < Size, !,
	( member(I, PR) -> %%% This y-register is valid.
	    Us = Vs
	; true,
	    Us = [I|Vs]
	),
	J is I + 1,
	NewPR = [I|MorePR],
	uninitialized_permanents(J, Size, PR, MorePR, Vs).

uninitialized_permanents(_I,_Size,_OldPR, PR, Us) :- %%% I =:= Size
	PR = [],
	Us = [].
	

%%%
%%  safeness_of_reg(+R,+A,+SafeXs,+SafeYs,-NewSafeXs,-NewSafeYs,+SeenPR,-NewSeenPR)
%
%   If R is fetched from register A, then R is safe if A is safe (A is always a
% X-register) else R is unsafe, i.e. maybe stack allocated.
%
safeness_of_reg(y(I),x(A),SX,SY,NewSX,NewSY,PR,NewPR) :-
	NewSX = SX,
	NewPR = [I|PR],
	( member(A,SX) -> NewSY = [I|SY] ; NewSY = SY ).

safeness_of_reg(x(I),x(A),SX,SY,NewSX,NewSY,PR,NewPR) :-
	NewSY = SY,
	NewPR = PR,
	( member(A,SX) -> NewSX = [I|SX] ; NewSX = SX ).


%%%
%%  The compute_live_registers/6 predicate is used to find what registers
% of the sequential engine are in use at the point of a parallel loop
% (i.e. Start_Left_Body and Start_Right_Body).
%
% LiveT is a (sorted) list of the temporaries alive
% LiveP is the environment size
%
compute_live_registers(Xs,_PR, Size, Uses, LiveT, LiveP) :-
	LiveP = Size,	%%% Size = number of permanent registers.
	take_chunk(Xs, Chunk,_M,_Rest),  %%% see regalloc.pl
	ud_chunk(Chunk, [], UD, []),     %%% see regalloc.pl
	live_regs(UD, Uses, Regs0),
	strip_regs(Regs0, Regs1),
	sort(Regs1, LiveT).

%%%
%%  The algorithm proceeds in reverse order of code, by adding used registers
% and removing defined registers from those live.
%
% When a register appears:
%
%  o   u(X)	add it to the register list
%  o   d(X)	delete it from the register list
%  o   c(X1,X2)	do as for "u(X1),d(X2)"
%
live_regs(    [], Regs0, Regs0).
live_regs([X|Xs], Regs0, Regs1) :-
	live_regs(Xs, Regs0, Regs2),
	( X = u(R) ->
	    add_reg(R, Regs2, Regs1)
	; X = d(R) ->
	    remove_reg(R, Regs2, Regs1)
	; X = c(R1,R2) ->
	    remove_reg(R2, Regs2, Regs3),
	    add_reg(R1, Regs3, Regs1)
	).

add_reg(R, Regs0, Regs1) :-
	Regs1 = [R|Regs0].

remove_reg(R, Regs0, Regs1) :-
	delete_all_from_list(Regs0, R, Regs1).


%%%
%%  Some registerss appear as x(N) while others are only N (where N is an integer).
% Remove the x(_)-wrapper.
%
strip_regs([],[]).
strip_regs([X|Xs],[Y|Ys]) :-
	( X = x(N) -> N = Y ; X = Y ),
	strip_regs(Xs,Ys).
