%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Portray interface to pretty print types.
%
%
%

portray(bot) :- write('bot').
portray(nil) :- write('nil').
portray(gnd) :- write('gnd').

portray(nv(P,S,L)) :-
	format('nv(~p,~p,~p)',[alias(P),shr(S),loc(L)]).

portray(any(P,S,L)) :-
	format('any(~p,~p,~p)',[alias(P),shr(S),loc(L)]).

portray(free(C,P,L)) :- 
	format('free(~p,~p,~p)',[alias(C),alias(P),loc(L)]).

portray(free_nil(C,P,L)) :-
	format('free+nil(~p,~p,~p)',[alias(C),alias(P),loc(L)]).

portray(list_g_n) :- write('@[gnd]').

portray(list_nv_n(P,S,L)) :-
	format('@[~p]',[nv(P,S,L)]).

portray(list_a_n(P,S,L)) :- 
	format('@[~p]',[any(P,S,L)]).	

portray(list_f_n(P,S,L)) :- 
	format('@[free(~p,~p,~p)]',[alias(P),shr(S),loc(L)]).

portray(list_g_fn(C,P,L)) :- 
	format('@[gnd|~p]',[free_nil(C,P,L)]).

portray(list_nv_fn(P_hd,S_hd,L_hd,C,P,L)) :-
	format('@[~p|~p]', [nv(P_hd,S_hd,L_hd), free_nil(C,P,L)]).

portray(list_a_fn(P_hd,S_hd,L_hd,C,P,L)) :- 
	format('@[~p|~p]', [any(P_hd,S_hd,L_hd), free_nil(C,P,L)]).

portray(list_f_fn(P_hd,S_hd,L_hd,C,P,L)) :-
	format('@[free(~p,~p,~p)|~p]', [alias(P_hd),shr(S_hd),loc(L_hd), free_nil(C,P,L)]).

portray(list_g_f(C,P,L)) :- 
	format('@[gnd|~p]', [free(C,P,L)]).

portray(list_nv_f(P_hd,S_hd,L_hd,C,P,L)) :- 
	format('@[~p|~p]', [nv(P_hd,S_hd,L_hd), free(C,P,L)]).

portray(list_a_f(P_hd,S_hd,L_hd,C,P,L)) :- 
	format('@[~p|~p]', [any(P_hd,S_hd,L_hd), free(C,P,L)]).

portray(list_f_f(P_hd,S_hd,L_hd,C,P,L)) :-
	format('@[free(~p,~p,~p)|~p]', [alias(P_hd),shr(S_hd),loc(L_hd), free(C,P,L)]).


portray(alias(X)) :-
	var(X) -> write('-') ; write(X).


portray(shr(X)) :-
	name_shr(X,Y), !, write(Y).


portray(loc(X)) :-
	name_loc(X,Y), !, write(Y).


portray('$pat'(P)) :-
	copy_term(P,Px),
	format_i_pattern(Px).

portray('$env'(E)) :-
	copy_term(E,Ex),
	format_environment(Ex).


format_i_pattern(bot) :- write(bot).

format_i_pattern(Pattern) :- functor(Pattern,det,_),!,
	print(Pattern).

format_i_pattern(shr(Alias,_Inst,Tuple)) :-
	install_aliases(Alias,Tuple),
	format_tuple(Tuple).


format_tuple(X) :- var(X),!,
	print('<VAR>').

format_tuple(F_Tuple) :-
	F_Tuple =.. [Det|Xs],
	Cert = 1,
	Poss = 1,
	name_variables(Xs,Cert,Poss,Ys),
	P_Tuple =.. [Det|Ys],
	print(P_Tuple).


format_environment(F_Environment) :-
	F_Environment =.. [Env,EntryDet,CurrState,_Inst,_DestArgs|Xs],
	Cert = 1,
	Poss = 1,
	name_variables(Xs,Cert,Poss,Ys),
	P_Environmnet =.. [Env,EntryDet/CurrState|Ys],
	print(P_Environmnet).


name_variables(    [],_C,_P,    []).

name_variables([X|Xs],C0,P0,[Y|Ys]) :-
	attr_listing(X,T,CertV,PossV,ShrV,LocV),
	name_cert_variables(CertV,C0,C1),
	name_poss_variables(PossV,P0,P1),
	attr_listing(Y,T,CertV,PossV,ShrV,LocV),
	name_variables(Xs,C1,P1,Ys).


name_cert_variables(    [], C, C).

name_cert_variables([X|Xs],C0,C2) :-
	( var(X) ->
	  X = c(C0), C1 is C0+1
	; C1 = C0
	),
	name_cert_variables(Xs,C1,C2).


name_poss_variables(    [], P, P).

name_poss_variables([X|Xs],P0,P2) :-
	( var(X) ->
	  X = p(P0), P1 is P0+1
	; P1 = P0
	),
	name_poss_variables(Xs,P1,P2).


name_shared_variables(    [],    []).

name_shared_variables([X|Xs],[Y|Ys]) :-
	name_shr(X,Y),
	name_shared_variables(Xs,Ys).


name_locality_variables(    [],    []).

name_locality_variables([X|Xs],[Y|Ys]) :-
	name_loc(X,Y),
	name_locality_variables(Xs,Ys).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% attribute_listing(Type,T_id,CertainAliases,PossAliases,SharingVars,LocVars)

attr_listing(nil,nil,[],[],[],[]).
attr_listing(gnd,gnd,[],[],[],[]).
attr_listing(nv(P,S,L),nv,[],[P],[S],[L]).
attr_listing(any(P,S,L),any,[],[P],[S],[L]).
attr_listing(free(C,P,L),free,[C],[P],[],[L]).
attr_listing(free_nil(C,P,L),free_nil,[C],[P],[],[L]).
attr_listing(list_g_n,list_g_n,[],[],[],[]).
attr_listing(list_nv_n(P,S,L),list_nv_n,[],[P],[S],[L]).
attr_listing(list_a_n(P,S,L),list_a_n,[],[P],[S],[L]).
attr_listing(list_f_n(P,S,L),list_f_n,[],[P],[S],[L]).
attr_listing(list_g_fn(C,P,L),list_g_fn,[C],[P],[],[L]).
attr_listing(list_nv_fn(P1,S1,L1,C,P,L),list_nv_fn,[C],[P,P1],[S1],[L1,L]).
attr_listing(list_a_fn(P1,S1,L1,C,P,L),list_a_fn,[C],[P,P1],[S1],[L1,L]).
attr_listing(list_f_fn(P1,S1,L1,C,P,L),list_f_fn,[C],[P,P1],[S1],[L1,L]).
attr_listing(list_g_f(C,P,L),list_g_f,[C],[P],[],[L]).
attr_listing(list_nv_f(P1,S1,L1,C,P,L),list_nv_f,[C],[P,P1],[S1],[L1,L]).
attr_listing(list_a_f(P1,S1,L1,C,P,L),list_a_f,[C],[P,P1],[S1],[L1,L]).
attr_listing(list_f_f(P1,S1,L1,C,P,L),list_f_f,[C],[P,P1],[S1],[L1,L]).
