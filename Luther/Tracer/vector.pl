%    -*- Prolog -*- 
%    File:	vector.pl  (~jb/Reform/Luther/Tracer/vector.pl)
%    Author:	Johan Bevemyr
%    Created:	Tue May 28 12:35:13 1996
%    Copyright:	(c)Johan Bevemyr
%    Purpose:   
 
vector_mult(v(X1,Y1,Z1),v(X2,Y2,Z2),Mult) :-
	Mult is X1*X2 + Y1*Y2 + Z1*Z2.

vector_scalar_mult(v(X1,Y1,Z1),Scalar,v(X2,Y2,Z2)) :-
	X2 is X1 * Scalar,
	Y2 is Y1 * Scalar,
	Z2 is Z1 * Scalar.

vector_sub(v(X1,Y1,Z1),v(X2,Y2,Z2),v(X3,Y3,Z3)) :-
	X3 is X1-X2,
	Y3 is Y1-Y2,
	Z3 is Z1-Z2.

vector_sub(v(X1,Y1),v(X2,Y2),v(X3,Y3)) :-
	X3 is X1-X2,
	Y3 is Y1-Y2.

vector_scale_length(v(X,Y,Z),Length,v(Xn,Yn,Zn)) :-
	SumSQRT is Length/sqrt(X*X + Y*Y + Z*Z),
	Xn is X * SumSQRT,
	Yn is Y * SumSQRT,
	Zn is Z * SumSQRT.

scale_vector_length(v(X,Y,Z),Length,v(Xn,Yn,Zn)) :-
	SumSQRT is Length/sqrt(X*X + Y*Y + Z*Z),
	Xn is X * SumSQRT,
	Yn is Y * SumSQRT,
	Zn is Z * SumSQRT.

vector_add(v(X1,Y1,Z1),v(X2,Y2,Z2),v(X3,Y3,Z3)) :-
	X3 is X1+X2,
	Y3 is Y1+Y2,
	Z3 is Z1+Z2.

vector_normalize(Vector,Norm) :-
	scale_vector_length(Vector,1,Norm).

normalize_vector(Vector,Norm) :-
	scale_vector_length(Vector,1,Norm).

vector_length_sq(Vector, Length) :-
	vector_mult(Vector,Vector,Length).

vector_length(v(X,Y,Z),Length) :-
	Length is sqrt(X*X+Y*Y+Z*Z).
	
vector_cross_product(v(U1,U2,U3),v(V1,V2,V3),v(W1,W2,W3)) :-
	W1 is U2*V3-U3*V2,
	W2 is U3*V1-U1*V3,
	W3 is U1*V2-U2*V1.

