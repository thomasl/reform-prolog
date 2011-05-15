%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		      COMPILATION OF RECURRENCES
%
% This file compiles the recurrences that appear in the prologue of
% a parallel predicate.
%
% Recurrences appear in three places:
% - as integer recursion
% - before a parallel loop
% - after a parallel loop
%
% the main things: set size reg, preserve values in right body over
% rec call, put results in rec call and head (as appropriate).

rp_compile_int_rec(_X_id,_Y_id,LeftProl,_RightProl,
	           _N,_LeftSize,_RightSize,_Step,_VarsR,_HasEnv,RegL0,RegL1,
	           Hd0,Hd1,Oth0,Oth1,LE0,RE0,LE1,RE1,R,RT) -->
	{ partition_recurrences(LeftProl,IntRec,_PreLoop,_PostLoop),
	  conj_to_group(IntRec,Group),
	  Trim = [0], D = det, NoEnv = no, NonLeft = no,
	  E1 = [],
	  Hd0 = Hd1, Oth0 = Oth1, LE0 = LE1, RE0 = RE1, R = RT,
	  RegL0 = RegL1
	},
	sh_compile_group(Group,Trim,E1,_E2,_Level1,_Use,[],D,NoEnv,NonLeft).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This code segment interfaces the rp-compiler with the standard
% compiler: registers are taken from the rp-environments, used for
% compilation and restored into the rp-envs. This technique is
% used to compile the (possibly three) prologues.

% rp_compile_primops(+Xs,+Regs,-NewRegs) [DCG]
% Compile Xs, a 'group' (here, always a list of primitives)
% using Regs, which is in 'rp' format. The result is code
% and a new register definition set.

rp_compile_primops((G,Gs),Regs,NewRegs) --> !,
	rp_compile_primop(G,Regs,CurrRegs),
	rp_compile_primops(Gs,CurrRegs,NewRegs).

rp_compile_primops(G,Regs,NewRegs) -->
	rp_compile_primop(G,Regs,NewRegs).

% Since rp-primitives never execute in parallel, we can simplify
% the compilation a bit by providing default parameters.

rp_compile_primop(G,Reg,NewReg) -->
	{ Lv0 = no_reg, Left0 = left, D0 = det },
	sh_inline_primitive(G,Reg,NewReg,Lv0,_Lv1,_U0,_U1,D0,_D1,Left0,_Left1).

% In rp-compiler, variables are mapped to registers as:
%   (VarID_list,LeftBodyReg,SaveOverCall,RightBodyReg)
%     list       reg name    perm name     reg name
%
% The rp_to_sh_regs/2 procedure (and sh_to_rp_regs/3) converts
% from and to the rp-representation. (The sh-representation is
% a dictionary.)
%
% NOTE: _all_ registers will appear, even the vector-regs. This
%       means a bug in the reduction translation will not be caught here.

rp_to_sh_regs(RP_reg_list,LR,SH_regs) :-
	empty_dict(E),
	rp_to_sh_regs(RP_reg_list,LR,E,SH_regs).

%

rp_to_sh_regs([],_LR,Sh,Sh).

rp_to_sh_regs([(VarIDs,Left,_Perm,Right)|Xs],LR,Sh0,Sh2) :-
	( LR = left ->
	  rp_to_sh_item(VarIDs,Left,Sh0,Sh1)
	; LR = right ->
	  rp_to_sh_item(VarIDs,Right,Sh0,Sh1)
	),
	rp_to_sh_regs(Xs,LR,Sh1,Sh2).

%

rp_to_sh_item(size_counter,Reg,Sh0,Sh1) :- !,
	insert(size_counter,Reg,Sh0,Sh1).

rp_to_sh_item([],_Reg,Sh,Sh).

rp_to_sh_item([X|Xs],Reg,Sh0,Sh2) :-
	insert(X,Reg,Sh0,Sh1),
	rp_to_sh_item(Xs,Reg,Sh1,Sh2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [$int_rec_begin, Gs, $int_rec_end], Pre, $loop_body, Post

partition_recurrences(Prologue,IntRec,PreLoop,PostLoop) :-
	take_intrec(Prologue,IntRec,RestPrologue),
	take_pre_loop(RestPrologue,PreLoop,PostLoop).

%

take_intrec(('$int_rec_begin',Gs),IntRec,Rest) :- !,
	take_intrec_end(Gs,IntRec,Rest).

take_intrec(G,true,G).

% If intrec, must end in $int_rec_end.

take_intrec_end((G,Gs),IntRec,End) :- !,
	( G = '$int_rec_end' ->
	  IntRec = true, End = Gs
	; IntRec = (G,IR),
	  take_intrec_end(Gs,IR,End)
	).

take_intrec_end('$int_rec_end',true,true).

%

take_pre_loop((G,Gs),Pre,Post) :- !,
	( G = '$loop_body' ->
	  Pre = true, Post = Gs
	; Pre = (G,Pre0),
	  take_pre_loop(Gs,Pre0,Post)
	).

take_pre_loop(G,Pre,Post) :-
	( G = '$loop_body' ->
	  Pre = true
	; Pre = G
	),
	Post = true.
