%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		    ARGUMENTS CHECKED FOR SAFENESS
%
% This file implements a table of safeness information, on the
% form:
%
%  Pred/Arity => [SafenessInfo]
%
% Where SafenessInfo is a list of length Arity where each element is
% 'safe' or 'unsafe', depending on whether it is not bound or bound,
% respectively.
%
% Predicates are on the same form as inlineable.pl .

% safeness_requirements/2:
%  return a list of info on what args are safe/unsafe. A user-call
% is always defined as safe -- since if it is UNSAFE, the annotator
% will complain at that definition instead.

safeness_requirements(P/N,Xs) :-
	( par_info(P,N,Xs,_) ->
	  true
	; safe_user_call(N,Xs)
	).

suspension_args(P/N,Xs) :-
	( par_info(P,N,_,Xs) ->
	  true
	; Xs = []
	).

safeness_and_susp(P/N,Safe,Susp) :-
	functor(Call,P,N),
	( inlineable(Call) ->
	  par_info(P,N,Safe,Susp)
	; Susp = [],
	  safe_user_call(N,Safe)
	).

suspend_map(P,N,Dict) :-
	suspension_args(P/N,Xs),
	init_dummy_dict(Xs,Dict).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

safe_user_call(0,Xs) :- !, Xs = [].
safe_user_call(N,[safe|Xs]) :- N > 0,
	M is N-1,
	safe_user_call(M,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The following database tells us all about the behavior of builtins.
% It has the form:
%   par_info(P,N,SafenessInfo,SuspendInfo)
%
% P,N is functor+arity.
% SafenessInfo is a list of 'safe' or 'unsafe' of length N
% SuspendInfo is a list of argument positions that require suspension.
%   (All of the positions are await_nonvar:ed ...)
%  or the atom 'leftmost', if the primop requires await_leftmost.
%
% NOTE: entries with '***' are unfinished or tentative. They may require
%       special treatment.
%
% NOTE: Some primitives rely on suspending implementations, e.g. @< and
%       others. Some primitives define overwritten arg regs as safe, e.g.
%       all arithmetic.
%
% NOTE: Some primitives (marked ***) might need 'leftmost' in susp.list!

par_info('$choice',1,[safe],[]).   
par_info('$cut',1,[safe],[]).      
par_info('$early_cut',0,[],[]).    
par_info('$early_cut',1,[safe],[]).
par_info(arg,3,[safe,unsafe,unsafe],[1,2]).      % ***   should be OK?
par_info('C',3,[safe,safe,safe],[]).             % ***  is this OK to ignore?
par_info(compare,3,[unsafe,safe,safe],[]).       % ***   should be OK?
par_info(functor,3,[unsafe,unsafe,unsafe],[]).   % ***   here too?
par_info(atom,1,[safe],[1]).       
par_info(atomic,1,[safe],[1]).
par_info(integer,1,[safe],[1]).
par_info(float,1,[safe],[1]).
par_info(nonvar,1,[safe],[1]).
par_info(number,1,[safe],[1]).
par_info(var,1,[safe],[1]).
par_info('=',2,[safe,safe],[]).                  % *** instead chk by compiler.
par_info('=..',2,[unsafe,unsafe],[]).            % *** should be OK?
par_info('==',2,[safe,safe],[]).
par_info('\==',2,[safe,safe],[]).
par_info('@<',2,[safe,safe],[]).
par_info('@>=',2,[safe,safe],[]).
par_info('@>',2,[safe,safe],[]).
par_info('@=<',2,[safe,safe],[]).
par_info('=:=',2,[safe,safe],[1,2]).
par_info('=\=',2,[safe,safe],[1,2]).
par_info('<',2,[safe,safe],[1,2]).
par_info('>',2,[safe,safe],[1,2]).
par_info('=<',2,[safe,safe],[1,2]).
par_info('>=',2,[safe,safe],[1,2]).
par_info('$plus',3,[safe,safe,safe],[2,3]).
par_info('$plus_1',2,[safe,safe],[2]).
par_info('$minus',3,[safe,safe,safe],[2,3]).
par_info('$minus_1',2,[safe,safe],[2]).
par_info('$times',3,[safe,safe,safe],[2,3]).
par_info('$div',3,[safe,safe,safe],[2,3]).
par_info('$intdiv',3,[safe,safe,safe],[2,3]).
par_info('$mod',3,[safe,safe,safe],[2,3]).
par_info('$rshift',3,[safe,safe,safe],[2,3]).
par_info('$lshift',3,[safe,safe,safe],[2,3]).
par_info('$b_or',3,[safe,safe,safe],[2,3]).
par_info('$b_and',3,[safe,safe,safe],[2,3]).
par_info('$b_xor',3,[safe,safe,safe],[2,3]).
par_info('$b_not',2,[safe,safe],[2]).
par_info('$neg',2,[safe,safe],[2]).
par_info('$exp',2,[safe,safe],[2]).
par_info('$pow',2,[safe,safe,safe],[2,3]).
par_info('$log',2,[safe,safe],[2]).
par_info('$sin',2,[safe,safe],[2]).
par_info('$cos',2,[safe,safe],[2]).
par_info('$tan',2,[safe,safe],[2]).
par_info('$asin',2,[safe,safe],[2]).
par_info('$acos',2,[safe,safe],[2]).
par_info('$atan',2,[safe,safe],[2]).
par_info('$sqrt',2,[safe,safe],[2]).
par_info('$cbrt',2,[safe,safe],[2]).
par_info('$msb',2,[safe,safe],[2]).
par_info('$rng',4,[safe,safe,safe,safe],[2,3,4]).
par_info('$tointeger',2,[safe,safe],[2]).
par_info('$tofloat',2,[safe,safe],[2]).
par_info('$aref',3,[safe,safe,safe],[2,3]).   % I think??

par_info('$round',2,[safe,safe],[2]).
par_info('$floor',2,[safe,safe],[2]).
par_info('$ceiling',2,[safe,safe],[2]).
par_info('$truncate',2,[safe,safe],[2]).
par_info('$abs',2,[safe,safe],[2]).

par_info('$min',3,[safe,safe,safe],[2,3]).

par_info('$lifted_u',3,[safe,safe,safe],[]). % as for '='/2

par_info('$det',0,[],[]).
par_info('$nondet',0,[],[]).
par_info('$initially_det',0,[],[]).
par_info('$initially_nondet',0,[],[]).
par_info('$get_level',1,[safe],[]).              % never done in parallel
par_info('$set_size',1,[safe],[]).               % never done in parallel
par_info('$get_size',1,[safe],[]).               % never done in parallel

par_info(true,0,[],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Compute types after suspension, given parallel and sequential types
%

% susp_type(+Par,+Seq,-Susp)

susp_type(Par,Seq,Susp) :-
	norm(Par,_,NormP),
	norm(Seq,_,NormS),
	susp_t(NormP,NormP,NormS,Susp).

susp_t(free(_,_,_),_Par,Seq,Susp) :- !, Susp = Seq.
susp_t(free_nil(_C,_P,_L),Par,Seq,Susp) :- !,
	inst_if_will_be_bound(Seq,Par,Susp).
susp_t(gnd,_Par,_Seq,Susp) :- !, Susp = gnd.
susp_t(nil,_Par,_Seq,Susp) :- !, Susp = nil.
susp_t(any(_P,_S,_L),Par,Seq,Susp) :- !,
	inst_if_will_be_bound(Seq,Par,Susp).
susp_t(nv(P,S,L),_Par,_Seq,Susp) :- !, Susp = nv(P,S,L).
susp_t(list_g_n,_Par,_Seq,Susp) :- !, Susp = list_g_n.
susp_t(list_nv_n(_P,_S,_L),Par,_Seq,Susp) :- !, Susp = Par.
susp_t(list_a_n(_P,_S,_L),Par,_Seq,Susp) :- !, Susp = Par.
susp_t(list_f_n(_P,_S,_L),Par,_Seq,Susp) :- !, Susp = Par.
susp_t(list_g_f(_C,_P,_L),Par,Seq,Susp) :- !,
	inst_if_will_be_bound(Seq,Par,Susp).
susp_t(list_nv_f(_P1,_S1,_L1,_C,_P,_L),Par,Seq,Susp) :- !,
	inst_if_will_be_bound(Seq,Par,Susp).
susp_t(list_a_f(_P1,_S1,_L1,_C,_P,_L),Par,Seq,Susp) :- !,
	inst_if_will_be_bound(Seq,Par,Susp).
susp_t(list_f_f(_P1,_S1,_L1,_C,_P,_L),Par,Seq,Susp) :- !,
	inst_if_will_be_bound(Seq,Par,Susp).
susp_t(list_g_fn(_C,_P,_L),Par,Seq,Susp) :- !,
	inst_if_will_be_bound(Seq,Par,Susp).
susp_t(list_nv_fn(_P1,_S1,_L1,_C,_P,_L),Par,Seq,Susp) :- !,
	inst_if_will_be_bound(Seq,Par,Susp).
susp_t(list_a_fn(_P1,_S1,_L1,_C,_P,_L),Par,Seq,Susp) :- !,
	inst_if_will_be_bound(Seq,Par,Susp).
susp_t(list_f_fn(_P1,_S1,_L1,_C,_P,_L),Par,Seq,Susp) :- !,
	inst_if_will_be_bound(Seq,Par,Susp).

% If the sequential type is instantiated, then the suspension type
% is the 'instantiation' of the parallel type by the sequential type.
%  Otherwise, it is simply the ordinary type.

inst_if_will_be_bound(Seq,Par,Susp) :-
	( bound_type(Seq) ->
	  inst_by_susp_type(Par,Seq,Susp)
	; Susp = Par
	).

% The following types are bound:

bound_type(gnd).
bound_type(nil).
bound_type(nv(_,_,_)).
bound_type(list_g_n).
bound_type(list_nv_n(_,_,_)).
bound_type(list_a_n(_,_,_)).
bound_type(list_f_n(_,_,_)).

% Compute type after suspending until instantiated.
% We know the seq type is bound.
%
% *** UNFINISHED ***
% Do we have to adjust linearity and locality as well?

inst_by_susp_type(free_nil(_C,P,L),Seq,Susp) :- !,
	( Seq = nil ->
	  Susp = nil
	; Susp = nv(P,_,L)
	).
inst_by_susp_type(any(P,S,L),_Seq,Susp) :- !,
	Susp = nv(P,S,L).
inst_by_susp_type(list_g_f(C,P,L),_Seq,Susp) :- !,
	Susp = list_g_fn(C,P,L).
inst_by_susp_type(list_nv_f(P1,S1,L1,C,P,L),_Seq,Susp) :- !,
	Susp = list_nv_fn(P1,S1,L1,C,P,L).
inst_by_susp_type(list_a_f(P1,S1,L1,C,P,L),_Seq,Susp) :- !,
	Susp = list_a_fn(P1,S1,L1,C,P,L).
inst_by_susp_type(list_f_f(P1,S1,L1,C,P,L),_Seq,Susp) :- !,
	Susp = list_f_fn(P1,S1,L1,C,P,L).
inst_by_susp_type(list_g_fn(C,P,L),_Seq,Susp) :- !,
	Susp = list_g_fn(_P1,_S1,_L1,C,P,L).
inst_by_susp_type(list_g_f(C,P,L),_Seq,Susp) :- !,
	Susp = list_g_fn(C,P,L).
inst_by_susp_type(list_nv_f(P1,S1,L1,C,P,L),_Seq,Susp) :- !,
	Susp = list_nv_fn(P1,S1,L1,C,P,L).
inst_by_susp_type(list_a_f(P1,S1,L1,C,P,L),_Seq,Susp) :- !,
	Susp = list_a_fn(P1,S1,L1,C,P,L).
inst_by_susp_type(list_f_f(P1,S1,L1,C,P,L),_Seq,Susp) :- !,
	Susp = list_f_fn(P1,S1,L1,C,P,L).
inst_by_susp_type(list_g_fn(C,P,L),_Seq,Susp) :- !,
	Susp = list_g_fn(_P1,_S1,_L1,C,P,L).
