%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	  ALLOCATING ENVIRONMENTS AND CLASSIFYING VARIABLES
%
% The clause looks like:    H :- L, R, B.
%
% We need to do the following classifications:
% 
% - What variables occur in R?
% - What vars occur in L and R but not in H or R?
% - What vars occur only in L? (subclass void/temp/perm)
% - What vars occur only in R? (subclass void/temp/perm)
%
% The instruction set does not provide instructions that access
% vector registers from environments. When there is a right body, we must
% save all registers occuring in R and restore them after the single
% recursive call. (Hence the first point.)
%
% Variables that only occur in processes (pt 3) are allocated to
% environments in general (if necessary). However, if they occur both in
% L and R we allocate them as vectors on the heap (pt 2). This is because
% we cannot access the environments of L from R, reliably .
%
% Finally, we perform the standard classification stuff on the left and
% right body (pt 4).
%
% This file also implements certain utilities that generate code to
% save and restore vector registers and so on. These are used
% by the compiler per se and are not intended to be called in
% isolation.

:- ensure_loaded('../util/vars').
:- ensure_loaded('../util/sets').

% rp_varclass(+H,+L,+Rec,+R,-VarsR,-BodyVecs,-LocalL,-LocalR)
%
% VarsR is the set of variables occuring in R
% BodyVecs is the set of additional variables to be allocated
%  to vectors
% LocalL and LocalR are the variables occuring locally in left and
%  right bodies, allocated to void (z(_)), temp (x(_)) and permanent (y(_)).
%  (Perms may need to be filled in a bit.)

rp_varclass(Head,Left,Rec,Right,VarsR,BodyVectors,LocalL,LocalR) :-
	var_indices_of_term(Head,HV),
	var_indices_of_term(Left,LV),
	var_indices_of_term(Right,RV),
	var_indices_of_term(Rec,RecV),
	sort(RV,VarsR),           % if allocated in vector, put in perm
	intersect(LV,RV,CommonBodyVars),
	union(HV,RecV,HeadVars),
	diff(CommonBodyVars,HeadVars,BagOfBodyVectors),
	sort(BagOfBodyVectors,BodyVectors), % allocate these in vector
	% Should also remove vector-allocated vars!
	union(BodyVectors,HeadVars,VectorAllocs),
	diff(LV,VectorAllocs,BagOfLocalL),
	sort(BagOfLocalL,LocalL),
	diff(RV,VectorAllocs,BagOfLocalR),
	sort(BagOfLocalR,LocalR).

% The above LocalL and LocalR must be filled out with a proper classification!

% Perform register allocation for a conjunction of goals. Also produce a
% list of stack trimming offsets.

allocate_conj(_,LCO,L,SortL) :-
	LCO = [],
	sort(L,SortL). % Wimp out!

% rp_save_restore_regs(SaveRestoreAction,EnvPosition,FinalEnvPosPlusOne
%                      Code,PreRec,PostRec,CodeTail)
%
% Save and restore registers around recursive call (the rec call gets
% spliced into the code elsewhere). The env slot number + 1 is returned;
% this number fortunately coincides with the stack trimming number to be
% used around the call.

rp_save_restore_regs([],EnvPos,EnvPos,Pre,Pre,Post,Post).

rp_save_restore_regs([(_,LeftR,Perm,RightR)|Rs],N,EnvPos,
	             ['Get_Variable'(Perm,LeftR)|Pre0],Pre,
	             ['Put_Value'(Perm,RightR)|Post0],Post) :-
        Perm = y(N), M is N+1,
        rp_save_restore_regs(Rs,M,EnvPos,Pre0,Pre,Post0,Post).

%

update_right_env_locations([],RE,RE).

update_right_env_locations([(Vars,_LeftR,_Perm,RightR)|Xs],RE0,RE2) :-
	update_location_list(Vars,RightR,RE0,RE1),
	update_right_env_locations(Xs,RE1,RE2).

%

update_location_list([],_NewReg,E,E).

update_location_list([X|Xs],NewReg,E0,E2) :-
	update_location_vector_elt(E0,X,NewReg,E1),
	update_location_list(Xs,NewReg,E1,E2).

%

update_location_vector_elt([(X,Loc)|Xs],Y,Reg,NewE) :-
	( X == Y ->
	  update_register(Loc,Reg,NewLoc),
	  NewE = [(X,NewLoc)|Xs]
	; NewE = [(X,Loc)|Ys],
	  update_location_vector_elt(Xs,Y,Reg,Ys)
	).

%

update_register(X,_Reg,_NewLoc) :- var(X),!,
	sys_error('update_register/3: variable location').

update_register(head(_,K),W1,Loc) :- !, Loc = head(W1,K).

update_register(tail(_,K),W1,Loc) :- !, Loc = tail(W1,K).

update_register(global(_),W1,Loc) :- !, Loc = global(W1).

update_register(X,_Reg,_NewLoc) :-
	sys_error('update_register/3: strange location ~q', [X]).

%

var_indices_of_term(T,Vs) :-
	var_indices_of_term(T,Vs,[]).

%

var_indices_of_term(X) --> {var(X),!,
	sys_error('var_indices_of_term/3: var arg')}.

var_indices_of_term(C) --> { atomic(C),! },[].

var_indices_of_term('$VAR'(X,_,_,_)) --> !,[X].

var_indices_of_term(PX) -->
	{ PX =.. [_|Xs] },
	var_indices_of_term_args(Xs).

%

var_indices_of_term_args([]) --> [].

var_indices_of_term_args([X|Xs]) -->
	var_indices_of_term(X),
	var_indices_of_term_args(Xs).

	
