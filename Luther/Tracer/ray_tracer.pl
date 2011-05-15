% -*- Prolog -*- 
%    File:	ray_tracer.pl  (~jb/projects/ray-tracer/ray_tracer.pl)
%    Author:	Johan Bevemyr
%    Created:	Mon Jun 26 14:41:14 1995
%    Purpose:   

:- ensure_loaded('vector').
:- ensure_loaded('color').
:- ensure_loaded('camera').
:- ensure_loaded('intersect').

trace_pict(Camera,Config,World,Pict) :-
	camera_width(Camera,Width),
	camera_height(Camera,Height),
	length(Rows,Height),
	trace_rows(Rows,0,Width,Camera,Config,World),
	Pict = Rows.

:- parallel([trace_rows/6]).

trace_rows([],_RowNr,_Width,_Camera,_Config,_World) :- !, Rows = [].
trace_rows([Column|Columns],N,Width,Camera,Config,World) :-
	trace_columns(Width,N,Column,Camera,Config,World),
	N2 is N + 1,
	trace_rows(Columns,N2,Width,Camera,Config,World).

trace_columns(0,_Y,Out,_Camera,_Config,_World) :- !, Out = [].
trace_columns(X,Y,[Pix|Rest],Camera,Config,World) :-
	camera_point(X,Y,Camera,Ray),
	trace_ray(Ray,World,Config,Pix),
	X2 is X - 1,
	trace_columns(X2,Y,Rest,Camera,Config,World).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trace_ray(Ray,World,Config,Color) :-
	config_reflection(Config,ReflDepth),
	config_background(Config,Background),
	trace_ray(Ray,World,Color,ReflDepth,Background).

trace_ray(Ray,world(Obj,Light),Color,Depth,Background) :-
	trace_ray(Ray,Obj,999999,surface(0,Background),surface(Refl,HitColor),
	          nohit,Hit),
        (
	    Hit = ray(HitPoint,Normal) ->
	    (
		Refl =\= 0, Depth > 0 ->
		NewDepth is Depth - 1,
		reflection_ray(Ray,Hit,NewRay),
		trace_ray(NewRay,world(Obj,Light),RefColor,NewDepth)
	    ;
		RefColor = Background
	    ),
	    (
		Refl =\= 1 ->
		trace_shadow(HitPoint,Normal,Obj,Light,HitColor,ShadColor)
	    ;
		ShadColor = rgb(0,0,0)
	    ),
	    (
		Refl = 0 ->
		Color = ShadColor
	    ;
		Refl = 1 ->
		Color = RefColor
	    ;
		number(Refl),
		combine_ref_shad(Refl,RefColor,ShadColor,Color)
	    )
	;
	    Color = Background % background color
	).

%

trace_ray(_Ray,World,_Closest,Color,FinalColor,Hit,FinalHit) :-
	empty_world(World), !,
	FinalColor = Color,
	FinalHit = Hit.

trace_ray(Ray,World,Closest,Color,FinalColor,Hit,FinalHit) :-
	next_object(World,Obj,NewWorld),
	(
	    intersect(Obj,Ray,Dist,NCol,ThisHit), Dist < Closest ->
	    NewClosest = Dist,
	    NewColor = NCol,
	    NewHit = ThisHit
	;
	    NewClosest = Closest,
	    NewColor = Color,
	    NewHit = Hit
	),
	trace_ray(Ray,NewWorld,NewClosest,NewColor,FinalColor,NewHit,
	          FinalHit).

%

empty_world([]).
next_object([O|Os],O,Os).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

trace_shadow(HP,Normal,World,Li,HC,Col) :-
	vector_scalar_mult(Normal,0.0001,ScNormal),
	vector_add(HP,ScNormal,HitP),
	gen_light_rays(Li,HitP,LiRays),
	trace_light_rays(LiRays,World,LightHit),
	combine(LightHit,Normal,HC,Col).

gen_light_rays([],_HP,[]).
gen_light_rays([light(Location)|Lis],HP,[LightRay|Rays]) :-
	LightRay = light(ray(HP,Direction),Distance),
	vector_sub(Location,HP,Dir),
	vector_length(Dir,Distance),
	normalize_vector(Dir,Direction),
	gen_light_rays(Lis,HP,Rays).

trace_light_rays([],_,[]).
trace_light_rays([light(Ray,Distance)|Rays],World,[Hit|Hits]) :-
	shadow_hit_any(World,Ray,Distance,Hit),
	trace_light_rays(Rays,World,Hits).

shadow_hit_any(World,Ray,_Distance,Hit) :-
	empty_world(World),!,
	Hit = Ray.

shadow_hit_any(World,Ray,Distance,Hit) :-
	next_object(World,Obj,NewWorld),
	(
	    intersect(Obj,Ray, HitDist, _,_), HitDist < Distance -> 
	    Hit = hit
	;
	    shadow_hit_any(NewWorld,Ray,Distance,Hit)
	).

combine(HitList,Normal,HC,Col) :-
	combine_hits(HitList,Normal,HC,rgb(0,0,0),Col).

combine_hits([],_Normal,_HC,Col,Col).
combine_hits([Hit|Hits],Normal,HC,InCol,Final) :-
	(
	    Hit = hit -> 
	    combine_hits(Hits,Normal,HC,InCol,Final)
	;
	    % No hit, add color component.
	    % find cos(phi)
	    Hit = ray(_Origin,Direction),
	    vector_mult(Normal,Direction,ND),
	    vector_length(Normal,Nlength),
	    vector_length(Direction,Dlength),
	    CosPhi is abs(ND / (Nlength*Dlength)),
	    scale_color(HC,CosPhi,NewCol),
	    color_add(NewCol,InCol,NextCol),
	    combine_hits(Hits,Normal,HC,NextCol,Final)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_map_color(X,Y,map(size(Width,Height),Map),surface(0,Color)) :-
	Xcord is integer(Width*X),
	Ycord is integer(Height*Y),
	nth(Ycord,Map,Row),
	nth(Xcord,Row,Color).


nth(0,List,Nth) :-
	!,
	List = [Nth|_].
nth(N,[_|List],Nth) :-
	N2 is N - 1,
	nth(N2,List,Nth).
