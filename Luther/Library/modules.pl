%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  modules.pl
%
%
% Description:
%
%   This file contains (the Prolog part of) the implementation of the module
% system supported by Luther Reform Prolog.
%
%
% Interface:
%
%
% Note: We do not actually have a module system. For the time beeing (until our
%       compiler supports modules) we have to be explicit when performing meta-
%       call and the like. Thus, we define 'SYSCALL'/1 and 'USERCALL'/1 to aid
%       in this purpose. These predicates are however considered temporary and
%       their use is not recomended since they will be removed eventually.
%
%
/*
:- module prolog.
*/

:- public
	'SYSCALL'/1,
	'USERCALL'/1,

	':'/2,
	use_module/1,
	current_module/1,
	ensure_loaded/1.



'SYSCALL'(Goal) :-
	call_module(Goal, prolog).

'USERCALL'(Goal) :-
	call_module(Goal, user).



Module : Goal :- call_module(Goal, Module).


use_module(Files) :- ensure_loaded(Files).


current_module(Module) :- '$get_module'(Module).


ensure_loaded(Files) :-
	'$loading_mode'(Mode, Mode),
	ensure_loaded_aux(Mode, Files).


ensure_loaded_aux(load, Files) :- load(Files).

ensure_loaded_aux(qload, Files) :- qload(Files).

ensure_loaded_aux(consult, Files) :- consult(Files).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Private predicates.
%

call_module(Goal, Module) :-
	'$module'(Current, Module),
	( call(Goal) ->
	    '$set_module'(Current)
	; true,
	    '$set_module'(Current), fail
	).

