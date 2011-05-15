%    -*- Prolog -*- 
%    File:	camera.pl  (~jb/Reform/Luther/Tracer/camera.pl)
%    Author:	Johan Bevemyr
%    Created:	Tue May 28 12:36:04 1996
%    Copyright:	(c)Johan Bevemyr
%    Purpose:   


camera_point(X,Y,camera(Eye,Corner,UnitRight,UnitDown,_PixWidth,_PixHeight),
	     ray(Origin,NormDirection)) :-
	Origin = Eye,
	vector_scalar_mult(UnitRight,X,Xlength),
	vector_scalar_mult(UnitDown,Y,Ylength),
	vector_add(Xlength,Ylength,Dist),
	vector_add(Corner,Dist,Direction),
	vector_normalize(Direction,NormDirection).

camera_width(camera(_Eye,_Corner,_UnitRight,_UnitDown,PixWidth,_PixHeight),
	PixWidth).

camera_height(camera(_Eye,_Corner,_UnitRight,_UnitDown,_PixWidth,PixHeight),
	PixHeight).

config_reflection(config(_,reflection(Depth),_),Depth).
config_background(config(Background,_,_),Background).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
reflection_ray(ray(_O,I),ray(HP,N),ray(HP,R)) :-
	vector_mult(N,I,NI),
	NI2 is 2*NI,
	vector_scalar_mult(N,NI2,NIN),
	vector_sub(I,NIN,R).
