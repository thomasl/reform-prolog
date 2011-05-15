%    -*- Prolog -*- 
%    File:	intersect.pl  (~jb/Reform/Luther/Tracer/intersect.pl)
%    Author:	Johan Bevemyr
%    Created:	Tue May 28 17:16:43 1996
%    Copyright:	(c)Johan Bevemyr
%    Purpose:   
 
:- ensure_loaded('vector').
:- ensure_loaded('color').

intersect(sphere(Radius,RadRad,Center,Color,Refl,Bits,NormPole,NormEquator),
	  ray(Origin,Direction),
	  Distance,surface(Refl,HitColor),ray(IntersectPoint,HitNormal)) :-

        vector_sub(Center,Origin, OC),
	vector_mult(OC,OC,L2oc),
	vector_mult(OC,Direction,Tca),
	(
	    L2oc >= RadRad ->
	    Tca > 0,
	    T2hc is RadRad - L2oc + Tca*Tca,
	    T2hc > 0,
	    Distance is Tca - sqrt(T2hc)
	;
	    T2hc is RadRad - L2oc + Tca*Tca,
	    Distance is Tca + sqrt(T2hc)
	),
	abs(Distance) > 0.00001,
	vector_scalar_mult(Direction,Distance,SD),
	vector_add(Origin,SD,IntersectPoint),
        vector_sub(IntersectPoint, Center, IC),
	InvRadius is 1/Radius,
	vector_scalar_mult(IC,InvRadius,Normal),
	( Bits = no_map ->
	    HitColor = Color,
	    HitNormal = Normal
	; Bits = bump ->
	    vector_cross_product(NormPole,NormEquator,Cross),
	    vector_mult(Normal,NormPole,NP),
	    Phi is acos(-1*NP),
	    V is Phi / 3.141592654,
	    vector_mult(Normal,NormEquator,NE),
	    Theta is acos(NE/sin(Phi))/(2*3.141592654),
	    vector_mult(Cross,Normal,CN),
	    (
		CN > 0 ->
		U = Theta
	    ;
		U is 1 - Theta
	    ),
	    Xflux1 is sin(U*160),
	    ( Xflux1 > 0 -> Xflux = 0.15 ; Xflux = -0.15),
	    Yflux is abs(1 - abs(Xflux)),
%	    Zflux is min(sin(V*20),0.4),
	    vector_cross_product(Normal,NormPole,NPm),
%	    vector_cross_product(Normal,NormEquator,NEm),
	    vector_scale_length(NPm,Xflux,NPS),
%	    vector_scale_length(NEm,Zflux,NES),
	    vector_add(IntersectPoint,NPS,Xd),
	    vector_scale_length(Normal,Yflux,NF),
	    vector_add(Xd,NF,XYd),
%	    vector_add(XYd,NES,XYZd),
	    vector_sub(XYd,IntersectPoint,XYdI),
	    vector_normalize(XYdI,HitNormal),
	    % format('returned ~w,~w,~w,~w~n',[Xflux,Yflux,Normal,HitNormal]),
	    HitColor = Color
	;
	    vector_cross_product(NormPole,NormEquator,Cross),
	    vector_mult(Normal,NormPole,NP),
	    Phi is acos(-1*NP),
	    V is Phi / 3.141592654,
	    vector_mult(Normal,NormEquator,NE),
	    Theta is acos(NE/sin(Phi))/(2*3.141592654),
	    vector_mult(Cross,Normal,CN),
	    (
		CN > 0 ->
		U = Theta
	    ;
		U is 1 - Theta
	    ),
	    get_map_color(U,V,Bits,Color),
	    HitNormal = Normal
	).

intersect(plane(Norm,Dist,Color),
	  ray(Origin,Direction),
	  Distance,surface(0,Color),ray(IntersectPoint,Normal)) :-
 
        intersect_plane(Norm,Dist,Direction,Origin,Distance, IntersectPoint,
	                Normal).

intersect(triangle(Center,Xaxis,Yaxis,TNorm,PDist,Color,Refl,Bits),
	  ray(Origin,Direction),
	  IDist,surface(Refl,HitColor),ray(IPoint,Normal)) :-
	
        % first, find out if the ray intersects the plane
        intersect_plane(TNorm,PDist,Direction,Origin,IDist,IPoint, Normal),
	vector_sub(IPoint,Center,Q),
	vector_mult(Yaxis,Yaxis,BB),
	vector_mult(Xaxis,Xaxis,AA),
	vector_mult(Q,Q,QQ),
	2*QQ < BB+AA,
	vector_mult(Q,Xaxis,QA),
	vector_mult(Xaxis,Yaxis,AB),
	vector_mult(Q,Yaxis,QB),
	Acoord is (BB*QA - AB*QB)/(AA*BB - AB*AB),
	Acoord >= 0, Acoord =< 1,
	Bcoord is (QB-Acoord*AB)/BB,
	Bcoord >= 0, Bcoord =< 1,
	Acoord + Bcoord =< 1,
	( Bits = no_map ->
	    HitColor = Color
	;   
	    get_map_color(Acoord,Bcoord,Bits,HitColor)
	).
	

intersect_plane(PNormal,PlaneDistance,RDirection,Origin,Distance,
	        IPoint,Normal) :-
	vector_mult(PNormal,RDirection,Vd),
	Vd =\= 0.0,
	vector_mult(PNormal,Origin,V0p),
	V0 is PlaneDistance-V0p,
	Distance is V0/Vd,
	Distance > 0,
	vector_scalar_mult(RDirection,Distance,RDD),
	vector_add(RDD,Origin,IPoint),
	(
	    Vd < 0 ->
	    Normal = PNormal
	;
	    vector_scalar_mult(PNormal,-1,Normal)
	).

