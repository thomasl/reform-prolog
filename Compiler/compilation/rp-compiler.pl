%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%
%           CODE GENERATOR FOR RECURSION PARALLEL PREDICATES
%
%
% We generate code for a recursion parallel clause H :- L, H' , R.
%
%
% *** UNFINISHED ***
% Does not handle integer recursion or reductions!
% For LIST-k classifications, head(W,I,K) should be introduced!
%

:- ensure_loaded('../util/dict').
:- ensure_loaded('../util/basics').
:- ensure_loaded('../definitions/inlineable').
:- ensure_loaded('../postprocess/peep').
:- ensure_loaded('../postprocess/unsafe').
:- ensure_loaded('../postprocess/heaptest').
:- ensure_loaded('../postprocess/regalloc').

:- ensure_loaded('rp-util').           % body splitting, var classify
:- ensure_loaded('rp-rewrite').
:- ensure_loaded('rp-intrec').
:- ensure_loaded('rp-alloc').
:- ensure_loaded('rp-varclasses').
:- ensure_loaded('shared-clause').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rp_comp(C) :-
        rp_compile(C,Code),
        enumeratevars(Code,0,_),
        pp_list(Code).

%

pp_list(    []) :-
	format('[].~n',[]).

pp_list([X|Xs]) :-
        format('[~q',[X]),
        ( Xs == [] ->
	    format('~n].~n',[])
        ; true,
	    format(',~n',[]),
	    pp_tail(Xs)
        ).

%

pp_tail(    []) :-
	format('].~n',[]).

pp_tail([X|Xs]) :-
        format(' ~q,~n',[X]),
        pp_tail(Xs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%rp_compile((H :- B),Code) :-
%       functor(H,P,N),
%       split_body(B,P,N,Left,Rec,Right),
%       Head = H,
%       rp_rewrite(Head,Rec,Left,NewHead,NewRec,NewLeft),
%       rp_varclass(NewHead,NewLeft,NewRec,Right,
%                   VarsRight,BodyVecs,_LocalL,_LocalR),
%       ( Right = true -> % then no RB => env. is not required
%         HasEnv = no
%       ; HasEnv = yes    % env. ALWAYS required otherwise! (size reg)
%       ),!,
%       rp_compile_clause(NewHead,NewLeft,NewRec,Right,
%                         VarsRight,BodyVecs,HasEnv,Code,[]).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This predicate compiles a preprocessed clause. The prologues
% should be compiled to reductions and integer recursion. We assume
% that inductions have been expanded into $get_level/1.
%

rp_compile_preproc((Head :- rec(LeftPrologue,
                                Left,
                                Rec,
                                Right,
                                RightPrologue)),
                    Code) :-
%       rp_rewrite(Head,Rec,Left,NewHead,NewRec,NewLeft),
        Head = NewHead, Rec = NewRec, Left = NewLeft,
        rp_varclass(NewHead,NewLeft,NewRec,Right,
                    VarsRight,BodyVecs,_LocalL,_LocalR),
        ( trivial_body(Right) ->
	    HasEnv = no		%%% No RB => environment is not required.
        ; true,
	    HasEnv = yes	%%% Otherwise, environment is always required (size reg).
        ),
	!,
        rp_compile_clause(LeftPrologue,RightPrologue,
                          NewHead,NewLeft,NewRec,Right,
                          VarsRight,BodyVecs,HasEnv,Code,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% rp_compile_clause(H,L,H1,R) [DCG]

rp_compile_clause(LP,RP,H,L,H1,R,VarsR,BodyVecs,HasEnv,Code,CodeTail) :-
        rp_codegen_clause(LP,RP,H,L,H1,R,
                          VarsR,BodyVecs,HasEnv,RawCode,[]),
        functor(H,_,N),
        regalloc(RawCode,N),
        peephole_opt(RawCode,OptCode),
        heaptest(OptCode,N,HeapTestingCode),
        safe_code(HeapTestingCode,AlmostFinalCode),
        peephole_opt2(AlmostFinalCode,FinalCode),
        dlist(FinalCode,Code,CodeTail).

%

rp_codegen_clause(LeftP,RightP,H,L,H1,R,VarsR,BodyVecs,HasEnv) -->
        ( { HasEnv == no } -> [] ; ['Allocate'(EnvSize)] ),
        {
	  LeftSize = x(_),
          RightSize = x(_),
	  rp_allocate_body_vectors(BodyVecs,SaveL)
        },
        rp_compile_head(H,H1,LeftP,RightP,
                        LeftSize,RightSize,Step,VarsR,HasEnv,SaveL,
                        LRD,RRD,
                        LE0,RE0,RecCode,RecTail,
			LeftRecs,RightRecs),
        rp_compile_body_vectors(BodyVecs,LeftSize,SaveL,LE0,RE0,LE1,RE1),
        rp_compile_body(L,R,LeftP,RightP,LeftRecs,RightRecs,LRD,RRD,
                        LeftSize,RightSize,Step,HasEnv,EnvSize,
                        LE1,RE1,_LE,_RE,RecCode,RecTail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% First arg MUST be recursion arg
%
% rp_compile_head(Head,RecCall,
%    LeftPrologue, RightPrologue,
%    LeftSizeReg, RightSizeReg,
%    RecursionStep,
%    VarsInRightBody, ClauseRequiresEnv,
%    SaveL, (?)
%    RegsOfVarsInLeftPrologue, DitoForRightPrologue,
%    LeftEnvironment, RightEnvironment,
%    CodeForRecCall, TailOfCode [DCG pair]
%    LeftRecs (?) RightRecs (?)
%   ) [DCG]

rp_compile_head(H,R,LeftPrologue,RightPrologue,
                LeftSize,RightSize,Step,VarsR,HasEnv,SaveL,
                LRD2,RRD2,
                LeftEnv,RightEnv,RecCode,RecTail,LeftRecs,RightRecs) -->
        { H =.. [_,X|Xs],
          R =.. [_,Y|Ys],
          functor(H,P,N),
          empty_dict(LE0),
          empty_dict(RE0),
          empty_dict(LRD0),
          empty_dict(RRD0)
        },
        { FirstReg = 0, NxtArg = 1 },
	rp_get_invariant_args(Xs,Ys,NxtArg,VarsR,SaveL,SaveL1,LE0,LE1,RE0,RE1,
	                      LRD0,LRDx,RRD0,RRDx,RecCode,SizeCode),
        rp_compile_rec_arg(X,Y,LeftPrologue,RightPrologue,FirstReg,
                           LeftSize,RightSize,Step,VarsR,HasEnv,
                           LRDx,LRD1,RRDx,RRD1,
                           SaveL1,SaveRegs,[],
                           Hd1,[],Oth1,LE1,RE1,LE2,RE2,SizeCode,SizeTmp,
			   LeftRecs,LeftR1,RightRecs,RightR1),
        rp_compile_args(Xs,Ys,NxtArg,LeftSize,P/N,VarsR,SaveRegs,
                        LeftPrologue,RightPrologue,LRD1,LRD2,RRD1,RRD2,
                        Hd1,_,Oth1,_,LE2,RE2,LeftEnv,RightEnv,SizeTmp,RecTail,
			LeftR1,[],RightR1,[]).

%

occurs_previously(X,Xs) :-
        soft_member(X,Xs).

%

lookup_location('$VAR'(X,_,_,_),Dict,Loc) :- lookup(X,Dict,Loc).

vector_reg_of(head(W,_),W).

vector_reg_of(tail(W,_),W).

update_location('$VAR'(X,_,_,_),NewLoc,Dict,NewDict) :-
        update(X,NewLoc,Dict,NewDict).

% rp_compile_args(HdArgs,RecArgs,ArgNo,SizeReg,Pred/Arity,VarsOfRightBody,
%                 RegsToBeSaved,HeadOccIn,HeadOccOut,OtherOccIn,OtherOccOut,
%                 LeftEnvIn,RightEnvIn,LeftEnvOut,RightEnvOut,
%                 RecCode,RecCodeTail,
%                 LeftRecurrences/Tail,RightRecurrences/Tail)
%
% The regList is the arguments to be saved and restored. Every entry in the
% list looks like:
%  (VarsAccessing,LeftBodyTemp,Perm,RightBodyTemp)
%
% Register save/restore is done in two phases: first, the code is
% generated. Next, the right environment is updated to tell
% of 'what's new'.

rp_compile_args([],[],_ArgNo,_Size,P/N,_VarsR,RegList,_LP,_RP,LRD,LRD,RRD,RRD,
        Hd,Hd,Oth,Oth,LE0,RE0,LE1,RE1,R1,RT1,
	LeftR,LeftR,RightR,RightR) -->
	{ R = ['Call'(P,N,lco=LCO)|RT], % LCO = # surviving env slots
          LE0 = LE1, RE0 = RE1,
          FirstEnvSlot = 0,
          rp_save_restore_regs(RegList,FirstEnvSlot,LCO,R1,R,RT,RT1) }.

rp_compile_args([X|Xs],[Y|Ys],M,Size,PN,VarsR,RegL,LP,RP,LRD0,LRD2,RRD0,RRD2,
                Hd0,Hd2,Oth0,Oth2,LE0,RE0,LE2,RE2,R,RT2,
		LeftR0,LeftR2,RightR0,RightR2) -->
        rp_compile_arg(X,Y,M,Size,VarsR,RegL,RegL1,LP,RP,LRD0,LRD1,RRD0,RRD1,
                       Hd0,Hd1,Oth0,Oth1,LE0,RE0,LE1,RE1,R,RT1,
		       LeftR0,LeftR1,RightR0,RightR1),
        { N is M+1 },
        rp_compile_args(Xs,Ys,N,Size,PN,VarsR,
                        RegL1,LP,RP,LRD1,LRD2,RRD1,RRD2,
                        Hd1,Hd2,Oth1,Oth2,LE1,RE1,LE2,RE2,RT1,RT2,
			LeftR1,LeftR2,RightR1,RightR2).

% The head of the clause is handled in a simple fashion right now.
% It must be a (simple!) recursion list, until we add support for
% reductions and manipulating the number of levels w.r.t. the length
% of the list. That'll come.
%
% rp_compile_rec_arg(HeadArg,RecArg,Prologue,ArgNo,LeftSizeReg,RightSizeReg,
%                    StepNo,VarsOfRightBody,
%                    RequiresEnv,SaveRegsIn,SaveRegsOut,HeadOccIn,HeadOccOut,
%                    OtherOccIn,OtherOccOut,LeftEnvIn,RightEnvIn,
%                    LeftEnvOut,RightEnvOut,RecCode,RecCodeTail)

rp_compile_rec_arg(X,Y,LeftProl,RightProl,
                   N,LeftSize,RightSize,Step,VarsR,HasEnv,
                   LRD0,LRD1,RRD0,RRD1,RegL0,RegL1,
                   Hd0,Hd1,Oth0,Oth1,LE0,RE0,LE1,RE1,R,RT,
		   LeftR,LeftR0,RightR,RightR0) -->
        ( { poslist(X,Y,X1) } ->
           ['Build_Rec_Poslist'(x(N),LeftSize,LW,T)],
           { ident_of(X1,X_hd),
             Hd1 = [X_hd|Hd0],
             ident_of(Y,X_tl),
             Oth1 = [X_tl|Oth0],
             T = x(_),
             LW = x(_),
             RW = x(_),
             insert(size_counter,LeftSize,LRD0,LRD1),
             insert(size_counter,RightSize,RRD0,RRD1),
             ( HasEnv = yes ->
               TmpRegL = [(size_counter,LeftSize,y(_),RightSize)|RegL0]
             ; TmpRegL = RegL0
             ),
             ( any_soft_member([X_hd,X_tl],VarsR) ->
               TmpY = y(_),
               RegL1 = [([X_hd,X_tl],LW,TmpY,RW)|TmpRegL]
             ; RegL1 = TmpRegL
             ),
             Step = 1,   % change to suit for other recursion steps!
             % Move arg into position for recursive call.
             R = ['Put_Value'(T,x(N))|RT],
             insert(X_hd,head(LW,0),LE0,LE1),
             insert(X_hd,head(RW,0),RE0,RE1),
	     LeftR = LeftR0, RightR = RightR0
           }
        ; { none_neg(X,Y,X_id,Y_id) } ->
	  { LeftR = [(X_id,Y_id,N)|LeftR0],
	    RightR = [(Y_id,X_id,N)|RightR0],
	    L_X_reg = x(_),
	    R_X_reg = x(_) },
	  ( { any_soft_member([X_id,Y_id],VarsR) } ->
	    { TmpY = y(_),
	      RegLx = [([X_id,Y_id],L_X_reg,TmpY,R_X_reg)|RegL0] },
	    ['Put_Value'(x(N),L_X_reg)]
	  ; { RegLx = RegL0 }
	  ),
	  ['Get_Variable'(L_X_reg,x(N))],
	  {
	    % Is Y_id OK? Well, if references to Y are rewritten to X+C
            % by previous transformations.
	    insert(X_id,global(L_X_reg),LE0,LEx),
	    insert(Y_id,global(L_X_reg),LEx,LExx),
	    insert(X_id,global(R_X_reg),RE0,REx),
	    insert(Y_id,global(R_X_reg),REx,RExx)
	  },
          rp_compile_int_rec(X_id,Y_id,LeftProl,RightProl,
                 N,LeftSize,RightSize,Step,VarsR,HasEnv,
                 LRD0,LRD1,RRD0,RRD1,RegLx,RegL1,
                 Hd0,Hd1,Oth0,Oth1,LExx,RExx,LE1,RE1,R,RT)
        ).

%

ident_of('$VAR'(X,_,_,_),X).

% First find the invariant arguments. One of them is used if
% the recursion is an integer recursion testing a register (e.g. a
% loop from 1 to N, where N is an INV-variable).
%
% The net effect: we get invariant args before computing size,
% rather than afterwards. This means the INV-part of the rp_compile_arg
% becomes empty.

rp_get_invariant_args([],[],_N,_VarsR,RegL,RegL,LE,LE,RE,RE,
	              LRD,LRD,RRD,RRD,Rec,Rec) --> [].
rp_get_invariant_args([X|Xs],[Y|Ys],N,VarsR,RegL0,RegL2,LE0,LE2,RE0,RE2,
	              LRD0,LRD2,RRD0,RRD2,R,RT2) -->
	( { inv(X,Y,X_id) } ->
          ['Get_Variable'(Tmp,x(N))],
          { Tmp = x(_),
            RightTmp = x(_),
            ( soft_member(X,VarsR) ->
              RegL1 = [ ([X_id],Tmp,y(_),RightTmp) | RegL0 ]
            ; RegL1 = RegL0
            ),
            R = ['Put_Value'(Tmp,x(N))|RT],
            insert(X_id,global(x(N)),LE0,LE1),
            insert(X_id,global(x(N)),RE0,RE1),
            insert(X,Tmp,LRD0,LRD1),
            insert(X,RightTmp,RRD0,RRD1)
          }
	; { RegL0 = RegL1, LE0 = LE1, RE0 = RE1, 
	    LRD0 = LRD1, RRD0 = RRD1, R = RT }
	),
	{ M is N+1 },
	rp_get_invariant_args(Xs,Ys,M,VarsR,RegL1,RegL2,LE1,LE2,RE1,RE2,
	                      LRD1,LRD2,RRD1,RRD2,RT,RT2).

% Arguments have a simplified form after the rewriting process
% of rp-classify:
%
% [X|Xs] -> Xs
% Xs -> [X|Xs]
% [X1,...,Xn|Xs] -> Xs
% Xs -> [X1,...,Xn|Xs]
% X -> X
% X -> Y
%
% The returned environment holds the locations allocated to
% variables occuring in the head. They are:
%  global(ArgReg)
%  head(Vector,Offset)
%  tail(Vector,Offset)

% rp_compile_arg(HeadArg,RecArg,ArgNo,SizeReg,VarsOfRightBody,
%             SaveRegsIn,SaveRegsOut,
%             HdIn,HdOut,OthIn,OthOut,
%             LeftEnvIn,RightEnvIn,LeftEnvOut,RightEnvOut,
%             RecCode,RecCodeTail) [DCG]
%

rp_compile_arg(X,Y,N,Size,VarsR,RegL0,RegL1,LP,RP,LRD0,LRD1,RRD0,RRD1,
               HdPrev0,HdPrev1,OthPrev0,OthPrev1,
               LE0,RE0,LE1,RE1,R,RT,LeftR,LeftR0,RightR,RightR0) --> !,
        ( { poslist(X,Y,X_hd_var) } ->
          { LRD0 = LRD1, RRD0 = RRD1,
	    ident_of(X_hd_var,X_hd) },
          ( { occurs_previously(X_hd,HdPrev0) } ->
            { lookup(X_hd,LE0,Loc), vector_reg_of(Loc,W) },
            ['Build_Poslist_Value'(x(N),Size,W,T)],
            { HdPrev1 = HdPrev0,
              ident_of(Y,X_tl),
              OthPrev1 = [X_tl | OthPrev0],
              T = x(_),
              RW = x(_),
              ( any_soft_member([X_hd,X_tl],VarsR) ->
                RegL1 = [ ([X_hd,X_tl],W,y(_),RW) | RegL0 ]
              ; RegL1 = RegL0
              ),
              R = ['Put_Value'(T,x(N))|RT],
              LE0 = LE1, RE0 = RE1,
	      LeftR = LeftR0, RightR = RightR0
            }
          ; ['Build_Poslist'(x(N),Size,LW,T)],
	    { ident_of(Y,X_tl),
              HdPrev1 = [X_hd|HdPrev0],
              OthPrev1 = [X_tl|OthPrev0],
              LW = x(_),
              RW = x(_),
              T = x(_),
              ( any_soft_member([X_hd,X_tl],VarsR) ->
                RegL1 = [([X_hd,X_tl],LW,y(_),RW)|RegL0]
              ; RegL1 = RegL0
              ),
              insert_list([(X_hd,head(LW,0)),(X_tl,tail(LW,0))],LE0,LE1),
              insert_list([(X_hd,head(RW,0)),(X_tl,tail(RW,0))],RE0,RE1),
              R = ['Put_Value'(T,x(N))|RT],
	      LeftR = LeftR0, RightR = RightR0
            }
          )
        ; { neglist(X,Y,Y_hd_var) } ->
	  { LRD0 = LRD1, RRD0 = RRD1,
	    ident_of(Y_hd_var,Y_hd) },
          ( { occurs_previously(Y_hd,HdPrev0) } ->
            { lookup(Y_hd,LE0,Loc), vector_reg_of(Loc,W1) },
            ['Build_Neglist_Value'(x(N),Size,W1,LW2,H)],
            { HdPrev1 = HdPrev0,
              ident_of(X,Y_tl),
              OthPrev1 = [ Y_tl | OthPrev0 ],
              LW2 = x(_),
              RW2 = x(_),
              H = x(_),
              insert(X_tl,tail(LW2,0),LE0,LE1),
              insert(X_tl,tail(RW2,0),RE0,RE1),
              ( any_soft_member([Y_tl],VarsR) ->
                RegL1 = [ ( [Y_tl], LW2, y(_), RW2 ) | RegL0 ]
              ; RegL1 = RegL0
              ),
              R = ['Put_Value'(H,x(N))|RT],
	      LeftR = LeftR0, RightR = RightR0
            }
          ; ['Build_Neglist'(x(N),Size,LW,H)],
            { HdPrev1 = [Y_hd|HdPrev0],
              ident_of(X,Y_tl),
              OthPrev1 = [Y_tl | OthPrev0 ],
              insert_list([(Y_hd,head(LW,0)),(Y_tl,tail(LW,0))],LE0,LE1),
              insert_list([(Y_hd,head(RW,0)),(Y_tl,tail(RW,0))],RE0,RE1),
              LW = x(_),
              RW = x(_),
              H = x(_),
              ( any_soft_member([Y_hd,Y_tl],VarsR) ->
                RegL1 = [ ( [Y_hd,Y_tl], LW, y(_), RW ) | RegL0 ]
              ; RegL1 = RegL0
              ),
              R = ['Put_Value'(H,x(N))|RT],
	      LeftR = LeftR0, RightR = RightR0
            }
          )
        ; { inv(X,Y,X_id) } -> % has already been compiled!
	  { HdPrev1 = HdPrev0,
            OthPrev1 = OthPrev0,
            RegL1 = RegL0,
            R = RT,
	    LE0 = LE1,
	    RE0 = RE1,
	    LRD0 = LRD1,
	    RRD0 = RRD1,
	    LeftR = LeftR0, RightR = RightR0
          }
        ; { none_neg(X,Y,X_id,Y_id) } ->
          ( { recurrence_arg(X_id,Y_id,LP,RP,_LeftRight) } ->
            { RegL1 = RegL0,
              R = RT,
              LRD0 = LRD1,
              RRD0 = RRD1,
              HdPrev1 = HdPrev0,
              OthPrev1 = OthPrev0,
              LE0 = LE1,
              RE0 = RE1,
	      LeftR = [(X_id,Y_id,N)|LeftR0],
	      RightR = [(Y_id,X_id,N)|RightR0]
            }
          ; ['Build_Variables'(x(N),Size,LW,T)],
            { HdPrev1 = HdPrev0,
              OthPrev1 = [X_id,Y_id|OthPrev0],
              T = x(_),
              LW = x(_),
              RW = x(_),
              ( any_soft_member([X_id,Y_id],VarsR) ->
                RegL1 = [ ([X_id,Y_id],LW,y(_),RW) | RegL0 ]
              ; RegL1 = RegL0
              ),
              R = ['Put_Value'(T,x(N))|RT],
              insert_list([ (X_id,head(LW,0)), (Y_id,head(LW,1))], LE0, LE1),
              insert_list([ (X_id,head(RW,0)), (Y_id,head(RW,1))], RE0, RE1),
              LRD0 = LRD1, RRD0 = RRD1, LeftR = LeftR0, RightR = RightR0
            }
          )
        ; { poslist_k(X,Y,K,Vs) } ->
          { sys_error('poslist-k argument ~d not handled',[N]) }
        ; { neglist_k(X,Y,K,Vs) } ->
          { sys_error('poslist-k argument ~d not handled',[N]) }
        ; { sys_error('argument ~d has no valid classification',[N]) }
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Is the current argument a recurrence (rather than a temporary
% variable)? Test by looking into LP and RP. The variables are
% assumed to either occur in left or right body! Otherwise, give an
% error.
%
% If the argument is a recurrence, LeftRight denotes whether it
% occurs in the left or right body.

recurrence_arg(X_id,Y_id,LP,RP,LeftRight) :-
        ( has_either_var(LP,X_id,Y_id) ->
          ( has_either_var(RP,X_id,Y_id) ->
            sys_error('recurrence occurs in left and right body -- not impl~n')
          ; LeftRight = left
          )
        ; ( has_either_var(RP,X_id,Y_id) ->
            LeftRight = right
          ; fail
          )
        ).

%

has_either_var('$VAR'(N,_,_,_),X_id,Y_id) :-
        ( N =:= X_id -> true
        ; N =:= Y_id
        ).

has_either_var(C,_,_) :- atomic(C),!,fail.

has_either_var(PX,X_id,Y_id) :-
        PX =.. [_|Xs],
        has_either_var_list(Xs,X_id,Y_id).

%

has_either_var_list([X|Xs],X_id,Y_id) :-
        ( has_either_var(X,X_id,Y_id) ->
          true
        ; has_either_var_list(Xs,X_id,Y_id)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% To be able to build recursive call before allocating the vectors, we
% must know what registers the 'body vectors' will appear in. Hence, the
% following phase before the main code generation of head args and 
% body vectors.

rp_allocate_body_vectors([],[]).

rp_allocate_body_vectors([X|Xs],[([X],x(_),y(_),x(_))|SaveL]) :-
        rp_allocate_body_vectors(Xs,SaveL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rp_compile_body_vectors([],_Size,_SaveL,LE,RE,LE,RE) --> [].

rp_compile_body_vectors([X|Xs],Size,SaveL,LE0,RE0,LE2,RE2) -->
        { insert(X,head(LW,0),LE0,LE1),
          insert(X,head(RW,0),RE0,RE1),
          body_vec_entry(SaveL,X,LW,RW),
          Tmp = x(_), T = x(_) },
        ['Put_Void'(Tmp),
         'Build_Variables'(Tmp,Size,LW,T)], % wot about T??
        rp_compile_body_vectors(Xs,Size,SaveL,LE1,RE1,LE2,RE2).

%

body_vec_entry([([X],LW,_,RW)|Xs],Y,TrueLW,TrueRW) :-
        ( X == Y ->
          TrueLW = LW,
          TrueRW = RW
        ; body_vec_entry(Xs,Y,TrueLW,TrueRW)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The prologue is compiled using a conjunction of goals and
% a register dictionary. The dictionary maps variable names to
% registers. Some variables are destined to end up in hard registers,
% such as the recursion arguments. Others are unassigned.
%
% The compilation invokes shared-clause's implementation of
% compiling a group.
%
% *** UNFINISHED ***
% The prologue must (of course) be compiled properly.
% This predicate must take a lot more arguments as well.
% Alter rp_compile_body when this occurs.

rp_compile_prologue(Prologue,Recurrences,R,Pre,PreEnd,Post,PostEnd) :-
        partition_recurrences(Prologue,_,PreLoop,PostLoop),
	prologue_classification(PreLoop,PostLoop,Recurrences,R,R0),
        sh_compile_group(PreLoop,_,R0,R1,no_reg,_U0,_U1,
                         det,_D,left,_,Pre,PreEnd),
        sh_compile_group(PostLoop,_,R1,_R2,no_reg,_U1x,_U2,
                         det,_Dx,left,_,Post,PostEnd).
%       Pre = [comment(['preloop',PreLoop,'not compiled'])|PreEnd],
%       Post = [comment(['postloop',PostLoop,'not compiled'])|PostEnd].

%rp_compile_prologue(Prologue,Pre,PreEnd,Post,PostEnd) :-
%       partition_recurrences(Prologue,_IntRec,Pre,Post),
%       rp_compile_loopcode(PreLoop,Pre,PreEnd),
%       rp_compile_loopcode(PostLoop,Post,PostEnd).
%
%
%
%rp_compile_loopcode((G,Gs),Pre0,Pre2) :- !,
%       rp_compile_loopgoal(G,Pre0,Pre1),
%       rp_compile_loopgoal(Gs,Pre1,Pre2).
%
%rp_compile_loopcode(G,Pre0,Pre1) :-
%       rp_compile_loopgoal(G,Pre0,Pre1).
%
% Here is where the work shudda been done.
%
%rp_compile_loopgoal(_G,Pre,Pre).
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Compilation of integer recursion: pick out the
% integer recursion-bit of the prologue and generate
% code to set the size register.

rp_compile_int_rec(X_id,Y_id,LeftProl,_RightProl,
                   _N,LeftSize,RightSize,Step,_VarsR,HasEnv,
                   LRD0,LRD1,RRD0,RRD1,RegL0,RegL1,
                   Hd0,Hd1,Oth0,Oth1,LE0,RE0,LE1,RE1,R,RT) -->
        { partition_recurrences(LeftProl,IntRec,_,_),
          LE1 = LE0,
          RE1 = RE0,
          Hd1 = Hd0,
          Oth1 = [X_id,Y_id|Oth0], R = RT,
          Step = 1,
          insert(size_counter,LeftSize,LRD0,LRD1),
          insert(size_counter,RightSize,RRD0,RRD1),
          ( HasEnv = yes ->
            TmpRegL = [(size_counter,LeftSize,y(_),RightSize)|RegL0]
          ; TmpRegL = RegL0
          ),
%	  Left = x(_), Right = x(_),
%          ( any_soft_member([X_id,Y_id],VarsR) ->
%            TmpY = y(_),
%            RegL1 = [([X_id,Y_id],Left,TmpY,Right)|TmpRegL]
%          ; RegL1 = TmpRegL
%          ),
	  RegL1 = TmpRegL,    % Never require saving to right body?
          intrec_classification(IntRec,X_id,Y_id,LeftSize,LRD1,R0)
        },
        rp_compile_intrec(IntRec,R0,_R1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

intrec_classification(IR,X_id,Y_id,LeftSize,LRD,R5) :-
	IR_arg = 0,
        empty_dict(R0),
        var_id_of_prologue(IR,RawVarIDs,[]),
        sort(RawVarIDs,VarIDs),
        insert_all_temps(VarIDs,R0,R1),
        update(X_id,x(IR_arg),R1,R2),
        update(Y_id,new(x(IR_arg)),R2,R3),
	list_dict(LRD,DefinedVars),
	update_defined_vars(DefinedVars,R3,R4),
	insert(size_counter,LeftSize,R4,R5).

% INV-vars are defined in LRD -- they will be 'unintialized' unless
% below:

update_defined_vars([],R,R).
update_defined_vars([(X,R)|Xs],R0,R2) :-
	( X = '$VAR'(ID,_,_,_) ->
	  update(ID,R,R0,R1)
	; X = size_counter ->   % handled later
	  R0 = R1
	; sys_error('update_defined_vars/3: ~q not handled',[X])
	),
	update_defined_vars(Xs,R1,R2).

% we handle prologues here as well. There is a subset of
% argument positions that are recurrences. These are allocated
% hard regs. Furthermore, size counter is fixed.

prologue_classification(Pre,Post,Recurrences,R,R3) :-
	empty_dict(R0),
	lookup(size_counter,R,Reg),              % unnecessary to use a dict..
	var_id_of_prologue(Pre,RawVarIDs,RestRaw),
	var_id_of_prologue(Post,RestRaw,[]),
	sort(RawVarIDs,VarIDs),
	insert_all_temps(VarIDs,R0,R1),
	update_all_recurrences(Recurrences,R1,R2),
	insert(size_counter,Reg,R2,R3).

% Recurrence args have fixed sources & destinations.

update_all_recurrences([],R,R).
update_all_recurrences([(X_id,Y_id,N)|Xs],R0,R3) :-
	update(X_id,x(N),R0,R1),
	update(Y_id,new(x(N)),R1,R2),
	update_all_recurrences(Xs,R2,R3).

%

var_id_of_prologue(X) --> {atomic(X),!}.
var_id_of_prologue('$VAR'(X,_,_,_)) --> !,[X].
var_id_of_prologue(PX) -->
        { PX =.. [_|Xs] },
        var_id_of_prologue_list(Xs).

var_id_of_prologue_list([]) --> [].
var_id_of_prologue_list([X|Xs]) -->
        var_id_of_prologue(X),
        var_id_of_prologue_list(Xs).

%

insert_all_temps([],R,R).
insert_all_temps([X|Xs],R0,R2) :-
        insert(X,new(x(_)),R0,R1),
        insert_all_temps(Xs,R1,R2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rp_compile_intrec(IR,R0,R1) -->
        sh_compile_group(IR,_,R0,R1,no_reg,_U0,_U1,
                         det,_D,left,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Compile the body of the procedure. We do not handle
% reductions yet.
%
% Note that the head compilation procedure already has produced code for
% the recursive call. We only handle the left and right bodies, if any.
%
% This is done as follows:
% - Generate code to call the left and right bodies if necessary
%   and splice in the call to the base case. (Has been generated
%   previously.)
% - For non-empty left/right bodies, generate code starting with the
%   positions as given by the environment from head compilation. As
%   this takes place, values are moved into temporary registers (which
%   can be exploited by later usage of the value).

rp_compile_body(LB,RB,LeftP,RightP,LeftR,RightR,LRD,RRD,
                _LeftSize,_RightSize,_Step,_HasEnv,no_perms,
                LE0,RE0,LE1,RE1,RecCode,RecTail,Code,CodeTail) :-
        trivial_body(LB), trivial_body(RB),!,
        LE0 = LE1,
        RE0 = RE1,
        rp_compile_prologue(LeftP,LeftR,LRD,
                            Code,TmpL,
                            TmpL,RecCode),
        rp_compile_prologue(RightP,RightR,RRD,
                            RecTail,TmpR,
                            TmpR,['Proceed'|CodeTail]).

rp_compile_body(L,RB,LeftP,RightP,LeftR,RightR,LRD,RRD,
                LeftSize,_RightSize,Step,_HasEnv,no_perms,
                LE0,RE0,LE1,RE1,RecCode,RecTail,Code,CodeTail) :-
        trivial_body(RB),!,
        rp_compile_prologue(LeftP,LeftR,LRD,
                Code,['Start_Left_Body'(Lbl,LeftUse)|TmpL],
                TmpL,RecCode),
        rp_compile_prologue(RightP,RightR,RRD,
                RecTail,TmpR,
                TmpR,['Proceed'|LeftBodyCode]),
        RE0 = RE1,
        rp_compile_left_body(L,LeftSize,Step,Lbl,LeftUse,LE0,LE1,
                             LeftBodyCode,CodeTail).

rp_compile_body(LB,R,LeftP,RightP,LeftR,RightR,LRD,RRD,
                _LeftSize,RightSize,Step,E,EnvSize,
                LE0,RE0,LE1,RE1,RecCode,RecTail,Code,CodeTail) :-
        trivial_body(LB),!,
        rp_compile_prologue(LeftP,LeftR,LRD,
                Code,TmpL,
                TmpL,RecCode),
        LE0 = LE1,
        ( E = no ->
          rp_compile_prologue(RightP,RightR,RRD,
                  RecTail,['Start_Right_Body'(RightSize,Lbl,UseR)|TmpR],
                  TmpR,['Proceed'|RightCode])
        ; E = yes ->
          rp_compile_prologue(RightP,RightR,RRD,
                  RecTail,['Start_Right_Body'(RightSize,Lbl,UseR)|TmpR],
                  TmpR,['Deallocate','Proceed'|RightCode])
        ),
        DownStep = Step, % [sic]
        rp_compile_right_body(R,RightSize,DownStep,Lbl,UseR,
                              RE0,RE1,RightCode,CodeTail),
        sort(UseR,SortedUse),
        length(SortedUse,AlmostEnvSize),
        EnvSize is AlmostEnvSize - 1.

rp_compile_body(L,R,LeftP,RightP,LeftR,RightR,LRD,RRD,
                LeftSize,RightSize,Step,HasEnv,EnvSize,
                LE0,RE0,LE1,RE1,RecCode,RecTail,Code,CodeTail) :- !,
        rp_compile_prologue(LeftP,LeftR,LRD,
                            Code,['Start_Left_Body'(LLbl,UseL)|TmpL],
                            TmpL,RecCode),
        ( HasEnv = no ->
          rp_compile_prologue(RightP,RightR,RRD,
                  RecTail,['Start_Right_Body'(RightSize,RLbl,UseR)|TmpR],
                  TmpR,['Proceed'|LeftCode])
        ; HasEnv = yes ->
          rp_compile_prologue(RightP,RightR,RRD,
                  RecTail,['Start_Right_Body'(RightSize,RLbl,UseR)|TmpR],
                  TmpR,['Deallocate','Proceed'|LeftCode])
        ),
        rp_compile_left_body(L,LeftSize,Step,LLbl,UseL,
                             LE0,LE1,LeftCode,RightCode),
        DownStep = Step, % [sic]
        rp_compile_right_body(R,RightSize,DownStep,RLbl,UseR,
                              RE0,RE1,RightCode,CodeTail),
        sort(UseR,SortedUse),
        length(SortedUse,AlmostEnvSize),
        EnvSize is AlmostEnvSize - 1.

%

trivial_body((G,Gs)) :- !, trivial_goal(G), trivial_body(Gs).
trivial_body(G) :- trivial_goal(G).

trivial_goal(X) :- var(X),!,fail.
trivial_goal(true).
trivial_goal('$choice'(_)).
trivial_goal('$initially_det').
trivial_goal('$initially_nondet').
trivial_goal('$det').
trivial_goal('$nondet').

% The Use parameter collects all vector registers referenced in the
% body. This is filled into the start-instructions and later used by
% the register allocator.

rp_compile_left_body(Body,SizeReg,Step,Lbl,Use,E0,E1,Code,CodeTail) :-
        RawCode = ['Label'(Lbl),
		   'Initialize_Left'(Step),
		   'Label'(ItLbl),
		   'Spawn_Left'(Step,LevelReg,SizeReg)|BodyCode],
        LevelReg = x(_),
        Use = [SizeReg|Use0],
        rp_compile_process_body(Body,LevelReg,Use0,E0,E1,BodyCode,Jmp),
        Jmp = ['Jump'(ItLbl)],
        regalloc(RawCode,0),
        peephole_opt(RawCode,OptimizedCode),
%        heaptest(OptimizedCode,0,HeapTestingCode),
%        safe_code(HeapTestingCode,SafeCode),
	SafeCode = OptimizedCode,
	dlist(SafeCode,Code,CodeTail).

rp_compile_right_body(Body,SizeReg,Step,Lbl,Use,E0,E1,Code,CodeTail) :-
	RawCode = ['Label'(Lbl),
		   'Initialize_Right'(Step,SizeReg),
		   'Label'(ItLbl),
		   'Spawn_Right'(Step,LevelReg)|BodyCode],
        LevelReg = x(_),
	Use = [SizeReg|Use0],
        rp_compile_process_body(Body,LevelReg,Use0,E0,E1,BodyCode,Jmp),
        Jmp = ['Jump'(ItLbl)],
        regalloc(RawCode,0),
        peephole_opt(RawCode,OptimizedCode),
%        heaptest(OptimizedCode,0,HeapTestingCode),
%        safe_code(HeapTestingCode,SafeCode),
	SafeCode = OptimizedCode,
	dlist(SafeCode,Code,CodeTail).

% Here is a replacement for the rp_compile_process_body:
% 1. Classify vars by (a) getting all globals from E0
%    and (b) doing parallel classification
% 2. Group the body, too
% 3. Introduce the classified vars into env (+ level reg)
% 4. Compile each group using the new environment
% 5. Be happy.
%
% *** UNFINISHED ***
% - The first group shouldn't have to get the level counter from
%   a permanent! (This happens right now.)
%
% NB. The code is converted to a normal list first, then into a d-list
%     since the register allocator & subsequent phases work only on
%     terminated lists.
%
% The heap test assumes there are no live regs on entering the code.
%

rp_compile_process_body(Body,Level,Use,E0,E2,Code,CodeT) :-
        vars_of_env(E0,Globals),
        parallel_classify_body(Body,GroupedBody,HasEnv,EnvSize,
                               Globals,Trim,Allocation),
        introduce_classified_vars(Allocation,E0,E1),
        ( HasEnv = yes(EnvSize) ->
	    Level1 = y(0), ActualTrim = Trim
        ; true,
	    Level1 = Level, ActualTrim = [0]
        ),
        D = det,   % Assumed determinate
        Left = no, % Not known to be leftmost
        sh_compile_groups(GroupedBody,ActualTrim,E1,E2,Level1,Use,[],D,
	                  HasEnv,Left,SafeCode,CodeT0),
        ( HasEnv = yes(EnvSize) ->
	    Code = ['Allocate'(EnvSize),'Get_Variable'(y(0),Level)|SafeCode],
	    CodeT0 = CodeT
        ; true,
	    Code = ['Allocate'(0)|SafeCode], CodeT0 = ['Deallocate'|CodeT]
        ).

%

vars_of_env(Dict,Globals) :-
        list_dict(Dict,Xs),
        vars_of_env(Xs,Globals,[]).

vars_of_env([]) --> [].

vars_of_env([(X,_)|Xs]) --> [X], vars_of_env(Xs).

%

introduce_classified_vars([],E,E).

introduce_classified_vars([(X,Reg)|Xs],E0,E2) :-
        insert(X,new(Reg),E0,E1),
        introduce_classified_vars(Xs,E1,E2).

% We wish to convert a list of variables to a list of
% variable indices when entering the body.

vars_to_indices(Dict,IndxDict) :-
        list_dict(Dict,KV),
        vs_to_indices(KV,IxVs),
        empty_dict(Empty),
        insert_list(IxVs,Empty,IndxDict).

vs_to_indices([],[]).
vs_to_indices([('$VAR'(N,_,_,_),V)|Xs],[(N,V)|Ns]) :-
        vs_to_indices(Xs,Ns).
