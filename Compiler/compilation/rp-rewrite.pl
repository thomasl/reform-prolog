%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%
%   Rewrite the parallel clause into a form that can be compiled into
% vector instructions.
%
% The following forms are allowed (afterwards):
%
%  o  [X|Xs] ->    Xs		X any occurens, Xs first occurens
%  o     Xs  -> [X|Xs]		X any occurens, Xs first occurens
%  o   M     ->  N		M, N first occurens
%  o   N     ->  N		N first occurens
%
%   All others are converted to the form above by lifting out offending
% portions as explicit unifications.
%
% *** UNFINISHED ***
% We should allow list-k classifications as well. This is not done.
%

:- ensure_loaded('../util/vars').
:- ensure_loaded('../util/basics').


rp_rewrite(Head, Rec, LeftBody, NewHead, NewRec, NewLeftBody) :-
	Head =.. [P|Xs],
	Rec  =.. [P|Ys],
	rp_rewrite_list(Xs,Ys, NXs,NYs, [],[], ExtraGoals,[]),
	add_extra_goals(ExtraGoals,NewLeftBody,LeftBody),
	NewRec  =.. [P|NYs],
	NewHead =.. [P|NXs].


rp_rewrite_list(    [],    [],      [],      [],_PrevVars,_PrevHead) --> [].
rp_rewrite_list([X|Xs],[Y|Ys],[NX|NXs],[NY|NYs], PrevVars, PrevHead) -->
	rp_rewrite_arg(X,Y, NX,NY, PrevVars,SeenVars, PrevHead,Head),
	rp_rewrite_list(Xs,Ys, NXs,NYs, SeenVars, Head).


%%%
%%  Rewrite an argument list to suit our taste. The last resort is to simply
% move out the offender to become an explicit unification.
%

rp_rewrite_arg(X,Y, NX,NY, Prev,Seen, PrevHead,ThisHead) -->  % possibly INV
	{
	  var(X),
	  var(Y),
	  X == Y,
	  !,
	  ThisHead = PrevHead
	},
	( { has_occured(X, Prev) } -> [NX = X],
	    {
	      NX = NY, Seen = Prev
	    }
	; { true },
	    {
	      NX = X,
	      NY = Y, Seen = [X|Prev]
	    }
	).

rp_rewrite_arg(X,Y, NX,NY, Prev,Seen, PrevHead,ThisHead) -->  % possibly NONE-NEG
	{
          var(X),
	  var(Y),
	  X \== Y,
	  !,
	  ThisHead = PrevHead
        },
	( { has_occured(X, Prev) } -> [NX = X],
	    {
	      Temp = Prev
	    }
	; { true },
	    {
	      NX = X, Temp = [X|Prev]
	    }
	),
	( { has_occured(Y, Prev) } -> [NY = Y],
	    {
	      Seen = Temp
	    }
	; { true },
	    {
	      NY = Y, Seen = [Y|Temp]
	    }
	).

rp_rewrite_arg(X,Y, NX,NY, Prev,Seen, PrevHead,ThisHead) --> % possibly POSLIST
	{
          nonvar(X),
	  var(Y),
	  X = [A|As], Y == As, !
        },
	( { has_occured(Y, Prev) } -> [NX = X, NY = Y],
	    {
	      ThisHead = PrevHead,
	      vars_find([X,Y], Seen, Prev)
	    }
	; { nonvar(A) } -> [NA = A],
	    {
	      NX = [NA|As],
	      NY = Y,
	      ThisHead = PrevHead,
	      vars_find([NX,NY], Seen, Prev)
	    }
	; { has_occured(A, [Y|Prev]),
	    \+(has_occured(A, PrevHead)) } -> [NA = A],
	    {
	      NX = [NA|As],
	      NY = Y,
	      ThisHead = PrevHead,
	      vars_find([NX,NY], Seen, Prev)
	    }
	; { true },
	    {
	      NX = X,
	      NY = Y,
	      ThisHead = [A|PrevHead],
	      vars_find([NX,NY], Seen, Prev)
	    }
	).

rp_rewrite_arg(X,Y, NX,NY, Prev,Seen, PrevHead,ThisHead) --> % possibly NEGLIST
	{
          nonvar(Y),
	  var(X),
	  Y = [A|As], X == As,
	  !,
	  ThisHead = PrevHead
        },
	( { has_occured(X, Prev) } -> [NX = X, NY = Y],
	    {
	      vars_find([X,Y], Seen, Prev)
	    }
	; { nonvar(A) } -> [NA = A],
	    {
	      NY = [NA|As],
	      NX = X,
	      vars_find([NX,NY], Seen, Prev)
	    }
	; { has_occured(A, [X|Prev]),
	    \+(has_occured(A, PrevHead)) } -> [NA = A],
	    {
	      NY = [NA|As],
	      NX = X,
	      vars_find([NX,NY], Seen, Prev)
	    }
	; { true },
	    {
	      NX = X,
	      NY = Y,
	      vars_find([NX,NY], Seen, Prev)
	    }
	).

rp_rewrite_arg(X,Y, NX,NY, Prev,Seen, PrevHead,ThisHead) --> % otherwise
	[NX = X, NY = Y],
	{
          ThisHead = PrevHead,
	  vars_find([X,Y], Seen, Prev)
        }.


% add_extra_goals(+,-,+)

add_extra_goals(    [], Conj, Conj).
add_extra_goals([G|Gs], Conj, More) :-
	Conj = (G,Next),
	add_extra_goals(Gs, Next, More).

%

has_occured(X, Xs) :-
	soft_member(X, Xs).
