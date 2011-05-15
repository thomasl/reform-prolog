%    -*- Prolog -*- 
%    File:	parser.pl  (~jb/Reform/Luther/Tracer/parser.pl)
%    Author:	Johan Bevemyr
%    Created:	Tue May 28 12:37:21 1996
%    Copyright:	(c)Johan Bevemyr
%    Purpose:   
 
parse(File,Parsed) :-
	file_to_list(File,List),
	new_camera(Camera),
	new_config(Config),
	specs([Camera,Config],Parsed,List,[]), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

specs(WorldIn,WorldOut) --> ws, spec_no_ws(WorldIn,WorldOut).

spec_no_ws(WorldIn,WorldOut) -->
	spec(WorldIn,World2), !,
	specs(World2,WorldOut).
spec_no_ws(World,World) --> [].

spec(WorldIn,WorldOut) --> camera(WorldIn,WorldOut), !.
spec(WorldIn,WorldOut) --> object(WorldIn,WorldOut), !.
spec(WorldIn,WorldOut) --> config(WorldIn,WorldOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Camera

camera(WorldIn,WorldOut) -->
	"camera", { new_camera(Camera) },
	camera_opts(Camera),
	{ member_add(Camera,WorldIn,WorldOut) }.

camera_opts(Camera) -->
	ws, copt(Camera),
	camera_opts(Camera).
camera_opts(_) --> [].

copt(camera(Eye,_,_,_,_,_,_)) -->    "eyepoint", !, vector(Eye). 
copt(camera(_,Look,_,_,_,_,_)) -->   "lookpoint", !, vector(Look). 
copt(camera(_,_,Zoom,_,_,_,_)) -->   "zoom", !, number(Zoom).
copt(camera(_,_,_,Up,_,_,_)) -->     "up", !, vector(Up). 
copt(camera(_,_,_,_,Width,_,_)) -->  "width", !, number(Width). 
copt(camera(_,_,_,_,_,Height,_)) --> "height", !, number(Height).
copt(camera(_,_,_,_,_,_,Res)) -->    "resolution", !, number(Res). 

new_camera(camera(_,_,_,_,_,_,_)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Objects

object(WorldIn,WorldOut) -->
	"sphere", !, { new_sphere(Sphere) },
	sphere_opts(Sphere),
	{ add_object(Sphere,WorldIn,WorldOut) }.

object(WorldIn,WorldOut) -->
	"plane", !, { new_plane(Plane) },
	plane_opts(Plane),
	{ add_object(Plane,WorldIn,WorldOut) }.

object(WorldIn,WorldOut) -->
	"triangle", !, { new_triangle(Triangle) },
	triangle_opts(Triangle),
	{ add_object(Triangle,WorldIn,WorldOut) }.

object(WorldIn,WorldOut) -->
	"light", !, { new_light(Light) },
	light_opts(Light),
	{ add_object(Light,WorldIn,WorldOut) }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sphere

sphere_opts(Sphere) --> ws, sopt(Sphere), !, sphere_opts(Sphere).
sphere_opts(_) --> [].

sopt(sphere(Rad,_,_,_,_,_,_)) -->  "radius", !, number(Rad).
sopt(sphere(_,Cent,_,_,_,_,_)) --> "center", !, vector(Cent).
sopt(sphere(_,_,Col,_,_,_,_)) -->  "color", !, color(Col).
sopt(sphere(_,_,_,Ref,_,_,_)) -->  "reflection", !, number(Ref).
sopt(sphere(_,_,_,_,Bit,_,_)) -->  "bitmap", !, name(Bit).
sopt(sphere(_,_,_,_,Bit,_,_)) -->  "bump", !, { Bit = bump }.
sopt(sphere(_,_,_,_,_,Pole,_)) --> "pole", !, vector(Pole).
sopt(sphere(_,_,_,_,_,_,Equ)) -->  "equator", !, vector(Equ).

new_sphere(sphere(_,_,_,_,_,_,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plane

plane_opts(Plane) --> ws, popt(Plane), !, plane_opts(Plane).
plane_opts(_) --> [].

popt(plane(Norm,_,_)) --> "normal", !, vector(Norm).
popt(plane(_,Dist,_)) --> "distance", !, number(Dist).
popt(plane(_,_,Col))  --> "color", !, color(Col).

new_plane(plane(_,_,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Triangle

triangle_opts(Triangle) --> ws, topt(Triangle), !, triangle_opts(Triangle).
triangle_opts(_) --> [].

topt(triangle(Cent,_,_,_,_,_)) --> "center", !, vector(Cent).
topt(triangle(_,X,_,_,_,_)) -->    "x-axis", !, vector(X).
topt(triangle(_,_,Y,_,_,_)) -->    "y-axis", !, vector(Y).
topt(triangle(_,_,_,Col,_,_)) -->  "color", !, color(Col).
topt(triangle(_,_,_,_,Ref,_)) -->  "reflection", !, number(Ref).
topt(triangle(_,_,_,_,_,Bit)) -->  "bitmap", !, name(Bit).

new_triangle(triangle(_,_,_,_,_,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Light

light_opts(Light) --> ws, lopt(Light), !, light_opts(Light).
light_opts(_) --> [].

lopt(light(Cent)) --> "center", !, vector(Cent).

new_light(light(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Configure
%

config(WorldIn,WorldOut) --> "background", color(Back),
	{ member_add(config(Back,_,_),WorldIn,WorldOut) }.
config(WorldIn,WorldOut) --> "use", ws, "reflection",
	{ member_add(config(_,reflection(_),_),WorldIn,WorldOut) }.
config(WorldIn,WorldOut) --> "use", ws, "shading",
	{ member_add(config(_,_,shading),WorldIn,WorldOut) }.
config(WorldIn,WorldOut) --> "use", ws, "shadow",
	{ member_add(config(_,_,shading),WorldIn,WorldOut) }.
config(WorldIn,WorldOut) --> "max", ws, "reflection", ws, "depth",
	intnumber(Depth),
	{ member_add(config(_,reflection(Depth),_),WorldIn,WorldOut) }.
			     
new_config(config(_,_,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Support

%%%%%%%%%%%%%%%%%%%%
% Name

name(Name) --> ws, "'", name_inner(NameStr), "'", {name(Name,NameStr)}.

name_inner([C|Cs]) --> [C], { C =\= "'" }, name_inner(Cs).
name_inner([]) --> [].

%%%%%%%%%%%%%%%%%%%%
% Vector

vector(v(X,Y,Z)) --> ws, "[", number(X), ",", number(Y), ",", number(Z),"]".

%%%%%%%%%%%%%%%%%%%%
% Color

color(rgb(R,G,B)) --> ws, "(", intnumber(R), "," , intnumber(G), ",",
	intnumber(B), ")".

%%%%%%%%%%%%%%%%%%%%
% Number

intnumber(X) --> ws,"-",!, num_int(0,N), {X is -1 * N}.
intnumber(X) --> ws,"+",!, num_int(0,X).
intnumber(X) --> ws,num_int(0,X).

number(X) --> intnumber(Int),fraction(Int,X).

fraction(Int,X) --> ".", !, num_frac(10,0,Frac), { X is Int + Frac }.
fraction(X,X) --> [].

num_int(X,Y) --> [D], { digit(D,Dig), !, X2 is X*10 + Dig },  num_int(X2,Y).
num_int(X,X) --> [].

num_frac(Div,X,Y) -->
	[D], { digit(D,Dig), !, X2 is X + Dig/Div, Div2 is Div * 10 },
	num_frac(Div2,X2,Y).
num_frac(_,X,X) --> [].

digit(N,Dig) :- N >= 0'0, N =< 0'9, Dig is N - 0'0.

%%%%%%%%%%%%%%%%%%%%
% Whitespace

ws --> [White], { White =< 32, ! } , ws.
ws --> "#" , !, skip_to_nl, ws.
ws --> [].

ws1 --> [White], { White =< 32 }, ws.

skip_to_nl --> [Ch], { Ch =\= 10 }, !, skip_to_nl.
skip_to_nl --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Misc

add_object(Obj,World,[Obj|World]).

member_add(X,Ys,Zs) :-
	member_add_inx(Ys,X,Zs).

member_add_inx([],X,[X]).
member_add_inx([X|Xs],X, Zs) :- !, Zs = [X|Xs].
member_add_inx([Y|Ys],X, [Y|Zs]) :- member_add_inx(Ys,X,Zs).

file_to_list(Filename,List) :-
	open(Filename,read,Stream),
	( 
	  read_file(Stream,List) ->
	    close(Stream)
	;
	    close(Stream),
	    fail
	).

read_file(Stream,List) :-
	get0(Stream,Ch),
	read_file_ch(Ch,Stream,List).

read_file_ch(-1,_,List) :- !, List = [].
read_file_ch(Ch,Stream,[Ch|Rest]) :-
	get0(Stream,NewCh),
	read_file_ch(NewCh,Stream,Rest).

