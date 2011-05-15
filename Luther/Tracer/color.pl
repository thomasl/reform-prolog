%    -*- Prolog -*- 
%    File:	color.pl  (~jb/Reform/Luther/Tracer/color.pl)
%    Author:	Johan Bevemyr
%    Created:	Tue May 28 12:35:43 1996
%    Copyright:	(c)Johan Bevemyr
%    Purpose:   
 
max(X,Y,Max) :- Max is min(X,Y).

%max(X,Y,Max) :- 
%	(
%	    X < Y -> Max = X
%	;
%	    Max = Y
%	).

scale_color(rgb(R,G,B),Scale,rgb(Rs,Gs,Bs)) :-
	Rsp is integer(R * Scale),
	Gsp is integer(G * Scale),
	Bsp is integer(B * Scale),
	max(Rsp,255,Rs),
	max(Gsp,255,Gs),
	max(Bsp,255,Bs).

combine_ref_shad(Refl,rgb(R1,G1,B1),rgb(R2,G2,B2),rgb(R3,G3,B3)) :-
	R3 is integer(Refl*R1 + (1-Refl)*R2),
	G3 is integer(Refl*G1 + (1-Refl)*G2),
	B3 is integer(Refl*B1 + (1-Refl)*B2).

color_add(rgb(R1,G1,B1),rgb(R2,G2,B2),rgb(R3,G3,B3)) :-
	R4 is R1 + R2,
	G4 is G1 + G2,
	B4 is B1 + B2,
	max(R4,255,R3),
	max(G4,255,G3),
	max(B4,255,B3).
