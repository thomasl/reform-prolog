%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		       VARIABLE CLASSIFICATION
%
% Used by rpc2.pl to classify variables into poslist etc. in a
% recursion-parallel predicate.
%  Also used by par.pl to classify for locality annotation. These
% have not been annotated and so are standard '$VAR'(N) items.

poslist(['$VAR'(X)|'$VAR'(Xs)],'$VAR'(Xs),'$VAR'(X)) :- 
	integer(X),
	integer(Xs),!.
poslist(['$VAR'(X,P,S,L)|'$VAR'(Xs,_,_,_)],'$VAR'(Xs,_,_,_),X_hd) :-
	integer(X), integer(Xs),
	X_hd = '$VAR'(X,P,S,L).

neglist('$VAR'(Xs),['$VAR'(X)|'$VAR'(Xs)],'$VAR'(X)) :- 
	integer(Xs), integer(X), !.
neglist('$VAR'(Xs,_,_,_),['$VAR'(X,P,S,L)|'$VAR'(Xs,_,_,_)],X_hd) :-
	integer(Xs), integer(X),
	X_hd = '$VAR'(X,P,S,L).

inv('$VAR'(X),'$VAR'(X),X) :- integer(X),!.
inv('$VAR'(X,_,_,_),'$VAR'(X,_,_,_),X) :- integer(X).

none_neg('$VAR'(X),'$VAR'(Y),X,Y) :- X =\= Y, integer(X), integer(Y),!.
none_neg('$VAR'(X,_,_,_),'$VAR'(Y,_,_,_),X,Y) :- 
	X =\= Y, integer(X), integer(Y).

poslist_k(Xs,Y,K,Vs) :-
	traverse_list(Xs,Y,0,K,Vs,[]).

neglist_k(Y,Xs,K,Vs) :-
	traverse_list(Xs,Y,0,K,Vs,[]).

% Traverse a list, compute length + vars in list.
% (NB. some guarantees are made by previous passes so that list has
%      form below.)

traverse_list([X|Xs],Y,M,N) --> !,[X], { M1 is M+1 }, traverse_list(Xs,Y,M1,N).

traverse_list('$VAR'(X),'$VAR'(X),M,N) --> !,
	{ M =\= 0, M = N }. % list must have length > 0
traverse_list('$VAR'(X,_,_,_),'$VAR'(X,_,_,_),M,N) -->
	{ M =\= 0, M = N }. % list must have length > 0
