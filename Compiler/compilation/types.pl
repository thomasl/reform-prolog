%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		      COMPILER HANDLING OF TYPES
%
% The abstract interpretation phase has produced a great amount of
% type information prior to compilation. This file enables the compiler
% to handle this type info intelligently. (Du-uh!)

subterms_have_type(gnd,N,X) :- !,
	list_of(N,gnd,X).

subterms_have_type(nil,N,X) :- !,
	( N =:= 0 ->
	  X = []
	; error('term of ~q arguments has nil type',[N])
	).

subterms_have_type(free,N,X) :- !,
	list_of(N,free,X).

subterms_have_type(free_nil,N,X) :- !, % if N > 0, must be non-nil!
	list_of(N,free,X).

subterms_have_type(Lst,N,X) :- list_type_id(Lst,Head),!,
	M is N-1,
	subterms_have_list_type(M,Head,Lst,X).

subterms_have_type(_,N,X) :- list_of(N,any,X).

%

list_of(0,_E,Xs) :- !, Xs = [].
list_of(N,E,Xs) :- N > 0, Xs = [E|Ys], M is N-1, list_of(M,E,Ys).

%

list_type_id(list_g_n,gnd).
list_type_id(list_nv_n,nv).
list_type_id(list_a_n,any).
list_type_id(list_f_n,free).
list_type_id(list_g_fn,gnd).
list_type_id(list_nv_fn,nv).
list_type_id(list_a_fn,any).
list_type_id(list_f_fn,free).
list_type_id(list_g_f,gnd).
list_type_id(list_nv_f,nv).
list_type_id(list_a_f,any).
list_type_id(list_f_f,free).

% tail elt is 0, others are 'greater than'

subterms_have_list_type(0,_Head,Lst,X) :- !,
	X = [Lst].

subterms_have_list_type(N,Head,Lst,X) :- N > 0,
	M is N-1,
	X = [Head|Xs],
	subterms_have_list_type(M,Head,Lst,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The code generator uses information about what types are possibly
% uninstantiated and what types are possibly instantiated. Note that
% e.g. 'any' falls into both categories. (Since it COULD be a free
% variable, or it COULD be an atom, or ...)

uninst_t(free) :- !.
uninst_t(any) :- !.
uninst_t(free_nil) :- !.
uninst_t(Lst) :-
	list_type(Lst,_,_,Tl,_),
	( Tl = f -> true
	; Tl = fn -> true
	).

binding_t(any) :- !.
binding_t(nv) :- !.
binding_t(gnd) :- !.
binding_t(nil) :- !.
binding_t(free_nil) :- !.
binding_t(Lst) :- list_type(Lst,_,_,_,_).

nonvar_t(nil).
nonvar_t(gnd).
nonvar_t(nv).
nonvar_t(Lst) :-
	list_type(Lst,_,_,n,_).

nonground_t(X) :- \+(ground_type(X)).

ground_t(nil).
ground_t(gnd).
ground_t(list_g_n).

%

nonlocal(robust).
nonlocal(fragile).
