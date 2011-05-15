%    -*- Prolog -*- 
%    File:	load.pl  (~jb/Reform/Luther/Tracer/load.pl)
%    Author:	Johan Bevemyr
%    Created:	Tue May 28 16:10:22 1996
%    Copyright:	(c)Johan Bevemyr
%    Purpose:   
 
:- ensure_loaded('ppm').
:- ensure_loaded('vector').
:- ensure_loaded('parser').


load_world(File,Config,Camera,world(Objs,Lights)) :-
	parse(File,RawWorld),
	process_world(RawWorld,Config,Camera,Lights,Objs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_world([],_Config,_Camera,[],[]).
process_world([H|T],Config,Camera,Lights,Objs) :-
	initialize(H,Type,Init),
	( Type = object ->
	    Objs = [Init|Objs2],
	    Lights2 = Lights
	; Type = light ->
	    Objs2 = Objs,
	    Lights = [Init|Lights2]
	; Type = config ->
	    Config = Init,
	    Objs2=Objs,
	    Lights2=Lights
	; Type = camera ->
	    Camera=Init,
	    Objs2=Objs,
	    Lights2=Lights
	),
	process_world(T,Config,Camera,Lights2,Objs2).
	    
%

initialize(config(Background,Reflection,C),config,
	config(Background,Reflection,C)) :-
	default(Background,rgb(0,0,0)),
	default(Reflection,reflection(0)).

initialize(light(C),light,light(C)) :-
	default(C,v(0,100,-100)).

initialize(camera(Eye,Look,Zoom,Up,Width,Height,Res),camera,
	   camera(Eye,Corner,UnitRight,UnitDown,PixWidth,PixHeight)) :-
        default(Eye,v(0,0,-8)),
	default(Look,v(0,0,0)),
	default(Zoom,10),
	default(Up,v(0,1,0)),
	default(Width,4),
	default(Height,4),
	default(Res,0.025),
        vector_sub(Look,Eye,LookDir),
	vector_normalize(LookDir,LookDirNorm),
	vector_scalar_mult(LookDirNorm,Zoom,ZoomVec),
	vector_add(Eye,ZoomVec,LookPoint),
	vector_cross_product(Up,ZoomVec,RightNorm),
	vector_cross_product(RightNorm,ZoomVec,UpNorm),
	vector_scale_length(RightNorm,Res,UnitRight),
	vector_scale_length(UpNorm,Res,UnitDown),
	HalfHeight is Height/2,
	HalfWidth is Width/2,
	vector_scale_length(UnitRight,HalfWidth,LeftDist),
	vector_scale_length(UnitDown,HalfHeight,UpDist),
	vector_add(LeftDist,UpDist,CornerDist),
	vector_sub(LookPoint,CornerDist,CornerPoint),
	vector_sub(CornerPoint,Eye,Corner),
	PixWidth  is integer(Width/Res),
	PixHeight is integer(Height/Res).

initialize(sphere(Radius,Center,Color,Refl,Bitmap,Pole,Equator),object,
	   sphere(Radius,RadRad,Center,Color,Refl,Bits,NormPole,NormEquator)):-
        default(Radius,3),
	default(Center,v(0,0,100)),
	default(Color,rgb(255,0,0)),
	default(Refl,0.0),
	default(Bitmap,no_map),
	default(Pole,v(0,1,0)),
	default(Equator,v(1,0,0)),
	RadRad is Radius*Radius,
	vector_normalize(Pole,NormPole),
	vector_normalize(Equator,NormEquator),
	( Bitmap = no_map ->
	   Bits = no_map
	; Bitmap = bump ->
	   Bits = bump
        ;
	   read_map(Bitmap,Bits)
	).

initialize(plane(Norm,Dist,Color),object,plane(NormNorm,Dist,Color)) :-
	default(Norm,v(-1,0,2)),
	default(Dist,150),
	default(Color,rgb(70,190,70)),
	vector_normalize(Norm,NormNorm).

initialize(triangle(Center,Xaxis,Yaxis,Color,Refl,Bitmap),object,
	   triangle(Center,Xaxis,Yaxis,TNorm,PDist,Color,Refl,Bits)) :-
	default(Center,v(0,0,90)),
	default(Xaxis,v(30,0,30)),
	default(Yaxis,v(30,30,5)),
	default(Color,rgb(40,40,120)),
	default(Refl,0.0),
	default(Bitmap,no_map),
	vector_cross_product(Xaxis,Yaxis,XYcross),
	vector_normalize(XYcross,TNorm),
	vector_mult(Center,TNorm,PDist),
	( Bitmap = no_map ->
	  Bits = no_map
	; 
	  read_map(Bitmap,Bits)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

default(X,X) :- !.
default(_,_).

read_map(File,map(size(Width,Height),Bits)) :-
	read_ppm(File,Bits),
	map_size(Bits,Width,Height).

map_size(Raw,Width,Height) :-
	length(Raw,Height),
	Raw = [Row|_],
	length(Row,Width).

