%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%			 EQUIVALENCE CLASSES
%

equivalence_classes(Pairs,Eqv) :-
	equivalence_lists(Pairs,[],EqvLst),
	equivalence_lists_to_classes(EqvLst,Eqv).

% A quadratic algorithm: for each pair (Key,Val), insert
% Val into Key's list.

equivalence_lists([],Eqv,Eqv).

equivalence_lists([(K,V)|Xs],Eqv0,Eqv2) :-
	equiv_insert(Eqv0,K,V,Eqv1),
	equivalence_lists(Xs,Eqv1,Eqv2).

%

equiv_insert([],K,V,[(K,[V])]).

equiv_insert([(K0,Vs)|Xs],K,V,Eqv) :-
	( K0 == K ->
	  Eqv = [(K0,[V|Vs])|Xs]
	; Eqv = [(K0,Vs)|Eqv0],
	  equiv_insert(Xs,K,V,Eqv0)
	).

% Each (K,Lst) sorts the Lst and throws away K.

equivalence_lists_to_classes([],[]).

equivalence_lists_to_classes([(_K,Lst)|Xs],[Cls|Clss]) :-
	sort(Lst,Cls),
	equivalence_lists_to_classes(Xs,Clss).

%
