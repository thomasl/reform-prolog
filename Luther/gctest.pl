%    -*- Prolog -*- 
%    File:	gctest.pl  (~jb/Reform/Luther/gctest.pl)
%    Author:	Johan Bevemyr
%    Created:	Mon Sep 18 11:25:42 1995
%    Purpose:   Measure the parallel GC
 


test :-
	loop(10).

:- parallel([loop/1]).


loop(0) :- !.
loop(N) :-
	N > 0,
	create_large_data(1000,X),
	N2 is N - 1,
	loop(N2).

create_large_data(0,[]) :- !.
create_large_data(N,[N|Ns]) :-
	N2 is N - 1,
	create_large_data(N2,Ns).
