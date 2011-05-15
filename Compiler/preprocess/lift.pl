%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	  LIFT VALUE UNIFICATIONS IN THE HEAD AND REINSTALL
%
% We wish to make the head of clauses linear (no repeated vars) prior
% to analysis, then reinstall the 'lifted' unifications after annotation.
% This is implemented in this file.
%
% A lifted unification looks like
%  '$lifted_u'(RevPath,X,Y)
% e.g., if we have the head 
%   p(A,f(A))
% we will get
%   p(A,f(B)) :- $lifted_u([1,2],B,A)
%
% The 'path' to B is thus reversed.
%
% Use plug_in_lifted_list(LiftedUnifyList,Hd,NewHd)
% to get back the old head.
%
% Usage:
% 1. Lift head  [Assumes 'non-ground representation']
% 2. Analyse
% 3. In annotation, produce initial envs + annotate head
% 4. Plug in lifted unifications (after annotating them as well) ['ground rep']
% 5. Annotate rest of clause

:- ensure_loaded('../util/dict').
:- ensure_loaded('../util/error').
:- ensure_loaded('../util/basics').

% Use this one on non-ground clauses:

lift_clause((H :- B),(NewH :- NewB)) :- !,
	lift_head(H,NewH,NewU,[]),
	prepend_to_body(NewU,NewB,B).

lift_clause(H,(NewH :- NewB)) :-
	lift_head(H,NewH,NewU,[]),
	prepend_to_body(NewU,NewB,true).

prepend_to_body([],B,B).
prepend_to_body([G|Gs],(G,B0),B1) :- prepend_to_body(Gs,B0,B1).

% lift_head(H,NewH) [DCG]
% returns list of unifications and new head
%
% Relies on '1'-based.
%
% NOTE: Assumes variables are represented as variables.

lift_head(H,NewH) -->
	{ H =.. [P|Xs],
	  empty_dict(Seen) },
	lift_unify_list(Xs,1,[],Seen,_NowSeen,NewXs),
	{ NewH =.. [P|NewXs] }.

% lift_unify_list(Args,ArgNo,PathToHere,
%                 SeenBefore,SeenAfter,
%                 VarNoBefore,VarNoAfter,NewArgs) [DCG]
%
% New unifications appear as $lifted_u(ReversedPath,X,Y)
% (X is the new var at position "reverse of path", insert Y there.
%
% NOTE: Assumes variables are represented as variables.

lift_unify_list([],_K,_Path,Seen,Seen,[]) --> [].
lift_unify_list([X|Xs],K,Path,Seen,ThenSeen,[NewX|NewXs]) -->
	lift_unify(X,[K|Path],Seen,NowSeen,NewX),
	{ L is K+1 },
	lift_unify_list(Xs,L,Path,NowSeen,ThenSeen,NewXs).

lift_unify(X,Path,Seen,NowSeen,NewX) -->
	( { var(X) } ->
	  ( { defined(X,Seen) } ->
	    { NowSeen = Seen },
	    ['$lifted_u'(Path,NewX,X)]
	  ; { NewX = X, insert(X,def,Seen,NowSeen) }
	  )
	; { atomic(X) } ->
	  { NewX = X, NowSeen = Seen }
	; { X =.. [P|Xs] },
	  lift_unify_list(Xs,1,Path,Seen,NowSeen,NewXs),
	  { NewX =.. [P|NewXs] }
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Two versions: unlift_clause/2 and unlift_clause_annot_vars/2.
% Former is used for sequential clauses, other for parallel ones.

unlift_clause((H :- B),(NewH :- NewB)) :-
	collect_lifted_unifications(B,NewB,LiftU,[]),
	plug_in_lifted_list(LiftU,H,NewH).

%

collect_lifted_unifications((G,Gs),NewB) --> !,
	( { G = '$lifted_u'(_P,_X,_Y) } ->
	  { NewB = NewB0 },
	  [G]
	; { NewB = (G,NewB0) }
	),
	collect_lifted_unifications(Gs,NewB0).
collect_lifted_unifications(G,NewB) -->
	( { G = '$lifted_u'(_P,_X,_Y) } ->
	  { NewB = true },
	  [G]
	; { NewB = G }
	).
	
% We now consider how to move value unifications back into the
% structure of origin.

plug_in_lifted_list([],Head,Head).
plug_in_lifted_list([U|Us],OldHead,NewHead) :-
	plug_in_lifted(U,OldHead,TmpHead),
	plug_in_lifted_list(Us,TmpHead,NewHead).

%

plug_in_lifted('$lifted_u'(RevPath,NewX,OldX),OldHead,NewHead) :-
	reverse(RevPath,Path),
	( Path = [] ->
	  error('plug_in_lifted: empty path')
	; Path = [P|Ps] ->
	  plug_in_path(Ps,P,NewX,OldX,OldHead,NewHead)
	).

plug_in_path([],P,NewX,OldX,Term,NewTerm) :-
	replace_nth_arg_of_term(P,Term,NewX,OldX,NewTerm).

plug_in_path([P|Ps],P0,NewX,OldX,Term,NewTerm) :-
	select_nth_arg_of_term(P0,Term,NewTerm,SelectedArg,NewSelectedArg),
	plug_in_path(Ps,P,NewX,OldX,SelectedArg,NewSelectedArg).

% replace arg in term
% based on first arg = 1

replace_nth_arg_of_term(N,Term,NewX,OldX,NewTerm) :- !,
	Term =.. [P|Xs],
	replace_nth_arg(N,Xs,NewX,OldX,NewXs),
	NewTerm =.. [P|NewXs].

% select arg in term
% based on first arg = 1

select_nth_arg_of_term(N,Term,NewTerm,SelectedArg,NewSelectedArg) :- !,
	Term =.. [P|Xs],
	select_nth_arg(N,Xs,NewXs,SelectedArg,NewSelectedArg),
	NewTerm =.. [P|NewXs].

% NewX is the 'new variable' that replaced the value unification; OldX
% is the original variable.
% based on first arg = 1

replace_nth_arg(1,Args,NewX,OldX,NewArgs) :- !,
	NewX = '$VAR'(N),
	Args = ['$VAR'(N)|Xs],   % sanity check
	NewArgs = [OldX|Xs].
replace_nth_arg(N,[X|Args],NewX,OldX,[X|NewArgs]) :- N > 1,
	M is N-1,
	replace_nth_arg(M,Args,NewX,OldX,NewArgs).

% based on first arg = 1

select_nth_arg(1,Args,NewArgs,SelectedArg,NewSelectedArg) :- !,
	Args = [SelectedArg|Xs],
	NewArgs = [NewSelectedArg|Xs].
select_nth_arg(N,[X|Args],[X|NewArgs],SelectedArg,NewSelectedArg) :-
	N > 0,
	M is N-1,
	select_nth_arg(M,Args,NewArgs,SelectedArg,NewSelectedArg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This is almost identical to above, except that variables are
%  '$VAR'/4 rather than '$VAR'/1 structures.
%
% Predicates have a suffix _av to distinguish from above, otherwise
% mostly identical.

unlift_clause_annot_vars((H :- B),(NewH :- NewB)) :-
	collect_lifted_unifications_av(B,NewB,LiftU,[]),
	plug_in_lifted_list_av(LiftU,H,NewH).

%

collect_lifted_unifications_av((G,Gs),NewB) --> !,
	( { G = '$lifted_u'(_P,_X,_Y) } ->
	  { NewB = NewB0 },
	  [G]
	; { NewB = (G,NewB0) }
	),
	collect_lifted_unifications_av(Gs,NewB0).
collect_lifted_unifications_av(G,NewB) -->
	( { G = '$lifted_u'(_P,_X,_Y) } ->
	  { NewB = true },
	  [G]
	; { NewB = G }
	).
	
% We now consider how to move value unifications back into the
% structure of origin.

plug_in_lifted_list_av([],Hd,Hd).
plug_in_lifted_list_av([U|Us],Hd0,Hd2) :-
	plug_in_lifted_av(U,Hd0,Hd1),
	plug_in_lifted_list_av(Us,Hd1,Hd2).

%

plug_in_lifted_av('$lifted_u'(RevPath,NewX,OldX),Hd,NewHd) :-
	reverse(RevPath,Path),
	( Path = [] ->
	  error('plug_in_lifted: empty path')
	; Path = [P|Ps] ->
	  plug_in_path_av(Ps,P,NewX,OldX,Hd,NewHd)
	).

plug_in_path_av([],P,NewX,OldX,Term,NewTerm) :-
	replace_nth_arg_of_term_av(P,Term,NewX,OldX,NewTerm).

plug_in_path_av([P|Ps],P0,NewX,OldX,Term,NewTerm) :-
	select_nth_arg_of_term_av(P0,Term,NewTerm,SelectedArg,NewSelectedArg),
	plug_in_path_av(Ps,P,NewX,OldX,SelectedArg,NewSelectedArg).

% replace arg in term
% based on first arg = 1

replace_nth_arg_of_term_av(N,Term,NewX,OldX,NewTerm) :- !,
	Term =.. [P|Xs],
	replace_nth_arg_av(N,Xs,NewX,OldX,NewXs),
	NewTerm =.. [P|NewXs].

% select arg in term
% based on first arg = 1

select_nth_arg_of_term_av(N,Term,NewTerm,SelectedArg,NewSelectedArg) :- !,
	Term =.. [P|Xs],
	select_nth_arg_av(N,Xs,NewXs,SelectedArg,NewSelectedArg),
	NewTerm =.. [P|NewXs].

% NewX is the 'new variable' that replaced the value unification; OldX
% is the original variable.
% based on first arg = 1

replace_nth_arg_av(1,Args,NewX,OldX,NewArgs) :- !,
	NewX = '$VAR'(N,_,_,_),
	Args = ['$VAR'(N,_,_,_)|Xs],   % sanity check
	NewArgs = [OldX|Xs].
replace_nth_arg_av(N,[X|Args],NewX,OldX,[X|NewArgs]) :-
	N > 1,
	M is N-1,
	replace_nth_arg_av(M,Args,NewX,OldX,NewArgs).

% based on first arg = 1

select_nth_arg_av(1,Args,NewArgs,SelectedArg,NewSelectedArg) :- !,
	Args = [SelectedArg|Xs],
	NewArgs = [NewSelectedArg|Xs].
select_nth_arg_av(N,[X|Args],[X|NewArgs],SelectedArg,NewSelectedArg) :-
	N > 0,
	M is N-1,
	select_nth_arg_av(M,Args,NewArgs,SelectedArg,NewSelectedArg).

%

test(Hd) :-
	enumeratevars(Hd,0,N),
	lift_head(Hd,N,_,NewHd,Unifs,[]),
	plug_in_lifted_list(Unifs,NewHd,OtherHd),
	format('original hd: ~p~nlifted hd: ~p~nunifs: ~p~nunlifted hd: ~p~n',
	       [Hd,NewHd,Unifs,OtherHd]).

