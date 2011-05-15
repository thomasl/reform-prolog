
% This pass inserts heap tests for the garbage collector.
% This is done by generating 'Require(N)' instructions
% where N is the (maximum) number of words to be written
% on the heap by a sequence.
%
% The PHO removes 'Require'(0) calls.
%
% This pass may be run between register allocation and
% peephole optimization.
%
% Added for recursion parallelism:
%
% First, there are now
% instructions that may cause unbounded allocation; the
% build instructions. This is handled by treating them
% as call-instructions and letting them do GC on their own.
%  This is not necessary for reductions, since they typically
% do NOT require any memory allocation. (Vectors have already
% been created, etc.)
%
% Secondly, jumps etc. cause requires to be emitted just as
% with calls.
%
% Third, we add that start_*_body acts just like a call
% and also collects info on the registers that are live
% (temporaries as well as permanents).

% Why is this here?
% :- ensure_loaded(inline).

heaptest(Code,A,['Require_Using'(K,A)|HeapTestingCode]) :-
	heaptest(Code,K,0,HeapTestingCode).

%

heaptest([],K,K,[]).

heaptest(['Call'(P,N,LCO)|Xs],H,K,['Call'(P,N,LCO),'Require'(H1)|Ys]) :- !,
	H = K,
	heaptest(Xs,H1,0,Ys).

% These should be treated as 'call' instructions really!
% (also, jump(_) should be treated likewise)

heaptest(['Build_Rec_Poslist'(A,S,V,T)|Xs],H,K,
	 ['Build_Rec_Poslist'(A,S,V,T),'Require'(H1)|Ys]) :- !, 
	H = K,
	heaptest(Xs,H1,0,Ys).

heaptest(['Build_Poslist'(A,S,V,T)|Xs],H,K,
	 ['Build_Poslist'(A,S,V,T),'Require'(H1)|Ys]) :- !, 
	H = K,
	heaptest(Xs,H1,0,Ys).

heaptest(['Build_Poslist_Value'(A,S,V,T)|Xs],H,K,
	 ['Build_Poslist_Value'(A,S,V,T),'Require'(H1)|Ys]) :- !, 
	H = K,
	heaptest(Xs,H1,0,Ys).

heaptest(['Build_Neglist'(A,S,V,H)|Xs],H0,K,
	 ['Build_Neglist'(A,S,V,H),'Require'(H1)|Ys]) :- !,
	H0 = K,
	heaptest(Xs,H1,0,Ys).

heaptest(['Build_Neglist_Value'(A,S,FromV,ToV,H)|Xs],H0,K,
	 ['Build_Neglist_Value'(A,S,FromV,ToV,H),'Require'(H1)|Ys]) :- !,
	H0 = K,
	heaptest(Xs,H1,0,Ys).

heaptest(['Build_Variables'(A,S,V,T)|Xs],H,K,
	 ['Build_Variables'(A,S,V,T),'Require'(H1)|Ys]) :- !,
	H = K,
	heaptest(Xs,H1,0,Ys).

heaptest(['Start_Left_Body'(L,Uses)|Xs],H,K,
	 ['Start_Left_Body'(L,Uses),'Require'(H1)|Ys]) :- !,
	H = K,
	heaptest(Xs,H1,0,Ys).

heaptest(['Start_Right_Body'(Size,L,Uses)|Xs],H,K,
	 ['Start_Right_Body'(Size,L,Uses),'Require'(H1)|Ys]) :- !,
	H = K,
	heaptest(Xs,H1,0,Ys).

heaptest(['Proceed'|Xs],H,K,['Proceed'|Ys]) :-
	!,
	H = K,
	heaptest(Xs,_,0,Ys).

heaptest(['Jump'(L)|Xs],H,K,['Jump'(L)|Ys]) :-
	!,
	H = K,
	heaptest(Xs,_,0,Ys).

heaptest(['Execute'(P,N)|Xs],H,K,['Execute'(P,N)|Ys]) :-
	!,
	H = K,
	heaptest(Xs,_,0,Ys).

heaptest([X|Xs],H,K0,[X|Ys]) :-
	requires(X,K1),
	K is K0+K1,
	heaptest(Xs,H,K,Ys).

% requires/2 is a database computing the heap requirements of
% every instruction. The coding below assumes Y-variables are
% put on the stack, taking 0 heap, while X-vars are put on the
% heap, taking 1 word. Lists take 2 words, structures take
% N+1 words, constants are assumed to be written inside structures
% or having space already allocated for them, is that OK?

requires('Put_Unsafe_Value'(_),1) :- !.

requires('Put_Void'(_),1) :- !.

requires('Put_Y_Void'(_),1) :- !.

requires('Put_List'(_),2) :- !.

requires('Put_Structure'(_/N,_),N1) :- !, N1 is N+1.

requires('Put_Variable'(x(_),_),1) :- !.

% requires('Put_Variable'(y(_),_),0) :- !.


requires('Get_List'(_),2) :- !.

requires('Get_Structure'(_/N,_),N1) :- !, N1 is N+1.

requires('Get_Variable'(x(_),_),1) :- !.

% requires('Get_Variable'(y(_),_),0) :- !. % Is it so?


requires('Unify_List',2) :- !.

requires('Unify_Structure'(_/N),N1) :- !, N1 is N+1.


requires('Unify_Nth_Head'(_Vector,_Level,_Offset),N) :- !, N = 1.

requires('Unify_Nth_Tail'(_Vector,_Level,_Offset),N) :- !, N = 1.

requires('Unify_Global_Arg'(_Global),N) :- !, N = 1.


requires('Lock_And_Get_List'(_MatchedReg,_TmpReg),N) :- !, N = 2.

requires('Lock_And_Get_Structure'(F,_MatchedReg,_TmpReg),N) :- !,
	F = _/M, N is M+1.


requires('Builtin'(P/N,_),K) :-	!,
	requires_builtin(P,N,K).

requires('Init'(M,_),N) :- !, M = N.


 % Default case, no heap allocation.

requires(_,0).

