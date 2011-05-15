%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Statistics on analyser:
%
% 1. Count the # of preds and # of args
% 2. Give stats on # det preds, and args as follows
%    - gnd
%    - free
%    - nv
%    - any
%    - list_*_n
%    - list_*_{f,fn}

astat(M0,P0,M_stat,P_stat) :-
	astat(M0,M_stat),
	astat(P0,P_stat).

astat(Dict,Stat) :-
	list_dict(Dict,Lst),
	stat_of_list(Lst,0,Pred,0,Det,0,Args,0,Gnd,0,Nv,0,Free,
	                 0,Any,0,Lst_n,0,Lst_f,0,Sys,0,Loc,0,Robust,0,WBF,0,Frag),
	Stat = stat(pred=Pred,det=Det,args=Args,
	            [gnd=Gnd,nv=Nv,free=Free,lst_n=Lst_n,lst_f=Lst_f,any=Any,sys=Sys],
		    [local=Loc,robust=Robust,wbf=WBF,fragile=Frag]).


stat_of_list([],P,P,D,D,Ar,Ar,G,G,NV,NV,F,F,ANY,ANY,
	        LST_N,LST_N,LST_F,LST_F,SYS,SYS,L,L,R,R,W,W,Fr,Fr).

stat_of_list([X|Xs],P0,P2,D0,D2,Ar0,Ar2,G0,G2,Nv0,Nv2,F0,F2,
	            A0,A2,LN0,LN2,LF0,LF2,Sys0,Sys2,L0,L2,R0,R2,W0,W2,Fr0,Fr2) :-
	stat_of_entry(X,P0,P1,D0,D1,Ar0,Ar1,G0,G1,Nv0,Nv1,F0,F1,
	                A0,A1,LN0,LN1,LF0,LF1,Sys0,Sys1,L0,L1,R0,R1,W0,W1,Fr0,Fr1),
	stat_of_list(Xs,P1,P2,D1,D2,Ar1,Ar2,G1,G2,Nv1,Nv2,F1,F2,
	                A1,A2,LN1,LN2,LF1,LF2,Sys1,Sys2,L1,L2,R1,R2,W1,W2,Fr1,Fr2).

stat_of_entry((_,Cp,_Sp),P0,P1,D0,D1,Ar0,Ar1,G0,G1,Nv0,Nv1,F0,F1,
	                 A0,A1,LN0,LN1,LF0,LF1,Sys0,Sys1,L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
        ( ( Cp == det ; Cp == bot ; Cp == unanalysed ) ->
	    P0 = P1, D0 = D1, Ar0 = Ar1, G0 = G1, Nv0 = Nv1, F0 = F1,
	    A0 = A1, LN0 = LN1, LF0 = LF1, Sys0 = Sys1, L0 = L1, R0 = R1, W0 = W1, Fr0 = Fr1
	; Cp = shr(_,_,Tuple) ->
	    P1 is P0+1,
	    functor(Tuple,Det,Args),
	    ( Det == det ->
		D1 is D0+1
	    ; Det == nondet ->
	        D1 = D0
	    ; true,
	        sys_error('astat det')
	    ),
	    Ar1 is Ar0+Args,
	    Tuple =.. [_|Xs],
	    stat_of_args(Xs,G0,G1,Nv0,Nv1,F0,F1,
	                    A0,A1,LN0,LN1,LF0,LF1,Sys0,Sys1,L0,L1,R0,R1,W0,W1,Fr0,Fr1)
	).

stat_of_args([],G,G,Nv,Nv,F,F,A,A,LN,LN,LF,LF,Sys,Sys,L,L,R,R,W,W,Fr,Fr).
stat_of_args([X|Xs],G0,G2,Nv0,Nv2,F0,F2,A0,A2,LN0,LN2,LF0,LF2,S0,S2,
	     L0,L2,R0,R2,W0,W2,Fr0,Fr2) :-
	stat_of_arg(X,G0,G1,Nv0,Nv1,F0,F1,A0,A1,LN0,LN1,LF0,LF1,S0,S1,
	            L0,L1,R0,R1,W0,W1,Fr0,Fr1),
	stat_of_args(Xs,G1,G2,Nv1,Nv2,F1,F2,A1,A2,LN1,LN2,LF1,LF2,S1,S2,
	             L1,L2,R1,R2,W1,W2,Fr1,Fr2).

stat_of_arg(X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) :- var(X),!,
	sys_error('stat_of_arg var arg').
stat_of_arg(gnd,G0,G1,Nv,Nv,F,F,A,A,LN,LN,LF,LF,S,S,L,L,R,R,W,W,Fr,Fr) :-
	G1 is G0+1.
stat_of_arg(nil,G0,G1,Nv,Nv,F,F,A,A,LN,LN,LF,LF,S,S,L,L,R,R,W,W,Fr,Fr) :-
	G1 is G0+1.
stat_of_arg(nv(_,_,L),G,G,Nv0,Nv1,F,F,A,A,LN,LN,LF,LF,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	Nv1 is Nv0+1, 
	locality_count(L,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(any(_,_,L),G,G,Nv,Nv,F,F,A0,A1,LN,LN,LF,LF,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	A1 is A0+1,
	locality_count(L,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(free_nil(_,_,L),G,G,Nv,Nv,F,F,A0,A1,LN,LN,LF,LF,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	A1 is A0+1,
	locality_count(L,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(free(_,_,L),G,G,Nv,Nv,F0,F1,A,A,LN,LN,LF,LF,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	F1 is F0+1,
	locality_count(L,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(list_g_n,G0,G1,Nv,Nv,F,F,A,A,LN,LN,LF,LF,S,S,
	L,L,R,R,W,W,Fr,Fr) :-
        G1 is G0+1.
stat_of_arg(list_nv_n(_,_,L),G,G,Nv,Nv,F,F,A,A,LN0,LN1,LF,LF,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	LN1 is LN0+1,
	locality_count(L,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(list_a_n(_,_,L),G,G,Nv,Nv,F,F,A,A,LN0,LN1,LF,LF,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	LN1 is LN0+1,
	locality_count(L,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(list_f_n(_,_,L),G,G,Nv,Nv,F,F,A,A,LN0,LN1,LF,LF,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	LN1 is LN0+1,
	locality_count(L,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(list_g_f(_,_,L),G,G,Nv,Nv,F,F,A,A,LN,LN,LF0,LF1,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	LF1 is LF0+1,
	locality_count(L,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(list_nv_f(_,_,La,_,_,Lb),G,G,Nv,Nv,F,F,A,A,LN,LN,LF0,LF1,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	LF1 is LF0+1,
	locality_count(La/Lb,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(list_a_f(_,_,La,_,_,Lb),G,G,Nv,Nv,F,F,A,A,LN,LN,LF0,LF1,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	LF1 is LF0+1,
	locality_count(La/Lb,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(list_f_f(_,_,La,_,_,Lb),G,G,Nv,Nv,F,F,A,A,LN,LN,LF0,LF1,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	LF1 is LF0+1,
	locality_count(La/Lb,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(list_g_fn(_,_,L),G,G,Nv,Nv,F,F,A,A,LN,LN,LF0,LF1,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	LF1 is LF0+1,
	locality_count(L,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(list_nv_fn(_,_,La,_,_,Lb),G,G,Nv,Nv,F,F,A,A,LN,LN,LF0,LF1,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	LF1 is LF0+1,
	locality_count(La/Lb,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(list_a_fn(_,_,La,_,_,Lb),G,G,Nv,Nv,F,F,A,A,LN,LN,LF0,LF1,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	LF1 is LF0+1,
	locality_count(La/Lb,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(list_f_fn(_,_,La,_,_,Lb),G,G,Nv,Nv,F,F,A,A,LN,LN,LF0,LF1,S,S,
	L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	LF1 is LF0+1,
	locality_count(La/Lb,L0,L1,R0,R1,W0,W1,Fr0,Fr1).
stat_of_arg(det,G,G,Nv,Nv,F,F,A,A,LN,LN,LF,LF,S0,S1,L,L,R,R,W,W,Fr,Fr) :-
	S1 is S0+1.
stat_of_arg(shallow,G,G,Nv,Nv,F,F,A,A,LN,LN,LF,LF,S0,S1,L,L,R,R,W,W,Fr,Fr) :-
	S1 is S0+1.
stat_of_arg(nondet,G,G,Nv,Nv,F,F,A,A,LN,LN,LF,LF,S0,S1,L,L,R,R,W,W,Fr,Fr) :-
	S1 is S0+1.

locality_count(L,L0,L2,R0,R2,W0,W2,Fr0,Fr2) :- nonvar(L),
	L = La/Lb, !,
	locality_count(La,L0,L1,R0,R1,W0,W1,Fr0,Fr1),
	locality_count(Lb,L1,L2,R1,R2,W1,W2,Fr1,Fr2).

locality_count(L,L0,L1,R0,R1,W0,W1,Fr0,Fr1) :-
	( local(L) ->
	    L1 is L0+1, R0 = R1, W0 = W1, Fr0 = Fr1
	; shared(L) ->
	    L0 = L1, R1 is R0+1, W0 = W1, Fr0 = Fr1
	; wbf(L) ->
	    L0 = L1, R0 = R1, W1 is W0+1, Fr0 = Fr1
	; fragile(L) ->
	    L0 = L1, R0 = R1, W0 = W1, Fr1 is Fr0+1
	).
