%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Commonly used predicates

% split_body/6 is used to split a clause body into the calls to the
% left and right of the recursive call, and the recursive call itself.

split_body((G,Gs),P,N,L,H,R) :- !,
	( functor(G,P,N) ->
	    L = true,
	    H = G,
	    R = Gs
	; true,
	    split_b(Gs,P,N,G,L,H,R)
	).

split_body(G,P,N,L,H,R) :-
	functor(G,P,N),
	L = true,
	H = G,
	R = true.

%

split_b((G,Gs),P,N,Goal,L,H,R) :- !,
	( functor(G,P,N) ->
	    L = Goal,
	    H = G,
	    R = Gs
	; true,
	    L = (Goal,L0),
	    split_b(Gs,P,N,G,L0,H,R)
        ).

split_b(G,P,N,Goal,L,H,R) :-
	functor(G,P,N),
	L = Goal,
	H = G,
	R = true.

%

lasttail_at(X,Y,M,N) :- var(X),!,Y=X,M=N.

lasttail_at([_|Ys],Y,M,N) :- !, M1 is M+1, lasttail_at(Ys,Y,M1,N).


lasttail_at(X,X,N,N).

%

any_soft_member([X|Xs],Set) :-
	( soft_member(X,Set) -> true ; any_soft_member(Xs,Set) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Classification of variables into temporaries and permanents.
%
%  o  During the forward pass, collect variables of previous groups. We also
%     group each body into chunks, with an LCO tag for each chunk.
%
%  o  Returning, variables that occur among the previously collected are
%     permanent and are allocated slots. The highest slot number as well as
%     variable allocation is returned as a pair.
%
%  Previously allocated vars are dropped.
%
%  Also, for every group the LCO is computed.
%

parallel_classify_body(Goals,GroupedGoals,HasEnv,EnvSize,GlobalVars,Trim,Allocation) :-
	group_body(Goals,GroupedGoals),
	ParStart = 1,
	classify_body(GroupedGoals,[],Trim,NextSlot,_,GlobalVars,Allocation,ParStart),
	( GroupedGoals = g(_) ->   %%% single group -> no environment
	    HasEnv = no, EnvSize = 0
	; true,
	    HasEnv = yes(EnvSize), %%% otherwise -> environment required
	    EnvSize = NextSlot
	).

sequential_classify_body((H :- B),GroupedBody,Trim,Allocation,EnvSize) :-
	group_body(B,GroupedBody),
	SeqStart = 0,
	( GroupedBody = (g(Grp),Rest) ->
	    classify_body((g((H,Grp)),Rest),[],Trim,NextSlot,_,[],Allocation,SeqStart)
	; GroupedBody = g(Grp) ->
	    classify_body(g((H,Grp)),[],Trim,NextSlot,_,[],Allocation,SeqStart)
	),
	EnvSize = NextSlot. %%% Counting [0..PrevSlot,NextSlot].

%%%
%%  classify_body(+Groups,+PrevVars,-Trim,-NextSlot,-SeenVars,-BaseVars,-AllocList,-AllocBase)
%
%	+Groups		the grouping of the body (a conjunction of g(Goals))
%	+PrevVars	the variables occuring before current group
%	-Trim		the environment trimming for every group
%	-NextSlot	next environment slot to use
%	-SeenVars	variables occuring after this group (when returning)
%	-BaseVars	a set of variables that are allocated as default
%	-AllocList	the actual allocation of registers and stack slots,
%			represented as pairs (Var,Reg) where Var is a source
%			variable (occuring once in the list) and Reg is x(_),
%			instantiated y(N) or void.
%	-AllocBase	slot allocation offset (0 or 1 depending on usage).
%
%   We then filter out the globally available variables, and get the locals to be
% allocated to temporaries and stack slots.
%
% Note: The stack slots sometimes start at 1 instead of 0 when we allocate them.
%	This comes from the level counter getting allocated to the first slot;
%	we bet that it will be used in groups after the first.
%
% The algorithm is N^2 in the # of variables and proportional to the size of
% the clause term.
%
% Note: It may be better to remove the first element of Trim, since it is the
%	rest of the list that is interesting.
%

% Allocation 
% Trim is the stack trimming of each group (a bit inelegantly handled
% currently).

classify_body((G,Gs),PrevVars,Trim,NextSlot,SeenVars,BaseVars,AllocList,AllocBase) :- !,
	variable_terms(G,Vs),
	indices_of(Vs,Indices),
	append(Indices,PrevVars,CurrVars),
	classify_body(Gs,CurrVars,PrevTrim,PrevSlot,
	              PrevSeen,BaseVars,PrevAlloc,AllocBase),
	allocate_vars(Vs,PrevVars,PrevSeen,SeenVars,PrevSlot,NextSlot,
		      PrevAlloc,AllocList,PrevSlot,PrevTrim,Trim).

classify_body(G,PrevVars,Trim,NextSlot,SeenVars,BaseVars,AllocList,AllocBase) :-
	variable_terms(G,Vs),
	allocate_vars(Vs,PrevVars,BaseVars,SeenVars,AllocBase,NextSlot,
	              [],AllocList,AllocBase,[],Trim).

%

indices_of([],[]).
indices_of(['$VAR'(N,_,_,_)|Xs],[N|Inds]) :- indices_of(Xs,Inds).

% This one is pretty self-explanatory:
% For every variable, check if it has been allocated, if it occurs
% in a previous block; if so, it's a permanent variable and gets the lowest
% stack slot (to facilitate trimming). Otherwise, it occurs only in the
% current group and is a temporary if it occurs twice (or more) or a
% void variable otherwise.
%
% To avoid allocating several occurences of the same variable in a block
% to different registers, the variable is added to Prev.
%
% Note that permanent variables could be allocated to produce
% a smaller environment by having several permanents allocated
% to the same stack slot, in separate lifetimes. This is not
% done currently.

allocate_vars([],_,Succ,Succ,NextSlot,NextSlot,
	Allocation,Allocation,CurrTrimSlot,CurrTrimList,TrimList) :-
	compute_trimming(CurrTrimSlot,NextSlot,CurrTrimList,TrimList).

allocate_vars([X|Xs],Prev,Succ,FinalSucc,CurrSlot,Slot,
              CurrAlloc,Alloc,TrimSlot,CurrTrimList,TrimList) :-
        X = '$VAR'(N,_,_,_),
	( soft_member(N,Succ) ->                     % X already allocated
	    NextSlot = CurrSlot,
	    NextAlloc = CurrAlloc
	; soft_member(N,Prev) ->                     % X permanent
	    NextAlloc = [(N,y(CurrSlot))|CurrAlloc],
	    NextSlot is CurrSlot + 1
	; soft_member_ix(N,Xs) ->                    % X temporary
	    NextAlloc = [(N,x(_))|CurrAlloc],
	    NextSlot = CurrSlot
	; true,                                           % X void
	    NextAlloc = [(N,void)|CurrAlloc],
	    NextSlot = CurrSlot
	),
	allocate_vars(Xs,Prev,[N|Succ],FinalSucc,NextSlot,Slot,
	              NextAlloc,Alloc,TrimSlot,CurrTrimList,TrimList).

%

soft_member_ix(N,['$VAR'(M,_,_,_)|Xs]) :-
	( N =:= M ->
	  true
	; soft_member_ix(N,Xs)
	).

%

compute_trimming(CurrTrimSlot,_NextTrimSlot,TrimList,[X|TrimList]) :-
	% X is NextTrimSlot - CurrTrimSlot.
	X = CurrTrimSlot.

% group_body smashes body into chunks g(Ch1),...,g(ChN)
% and removes g(true) except if body ONLY consists of that group.

group_body(Body,GroupedBody) :-
	group_goals_of_body(Body,PrelimBody),
	eliminate_useless_groups(PrelimBody,GrpBody),
	( var(GrpBody) ->
	    GroupedBody = g(true)
	; true,
	    GroupedBody = GrpBody
	).

%

group_goals_of_body(Body,GroupedBody) :-
	group_goals_of_body(Body,G,G,GroupedBody).

group_goals_of_body((G,Gs),CurrGroup,Group,GroupedBody) :- !,
	( inlineable(G) ->
	    CurrGroup = (G, CurrGroup0),
	    group_goals_of_body(Gs,CurrGroup0,Group,GroupedBody)
	; true,
	    CurrGroup = G,
	    GroupedBody = (g(Group), RestGroupedBody),
	    group_goals_of_body(Gs,Grp,Grp,RestGroupedBody)
	).

group_goals_of_body(G,G,Grp,g(Grp)).

% If this predicate returns an unbound variable, the body consisted
% of "true, true, ..., true." which must be detected by caller.
%
% If body is just "true" a variable is returned! This must be detected
% by caller.

eliminate_useless_groups((G,Gs),NewB) :-
	eliminate_useless_groups(Gs,RestB),
	( var(RestB) ->
	    NewB = G
	; G = g(true) ->
	    NewB = RestB
	; true,
	    NewB = (G,RestB)
	).

eliminate_useless_groups(G,NewB) :-
	( G = g(true) -> true ; NewB = G ).

%

zip_lists([],[],[]).

zip_lists([X|Xs],[Y|Ys],[(X,Y)|XYs]) :-
	zip_lists(Xs,Ys,XYs).

%

mk_vars([]).
mk_vars([X|Xs]) :-
	X = '$VAR'(1,any,any,local),
	mk_vars(Xs,2).

mk_vars([],_).
mk_vars([X|Xs],N) :-
	X = '$VAR'(N,any,any,local),
	M is N+1,
	mk_vars(Xs,M).
