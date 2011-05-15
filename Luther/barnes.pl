% *****************************************************************************
%  Implementation of the Barnes-Hut Algorithm in Computing Gravitational
%                          Effects upon N Bodies.
%
%  File         : barnes.pl
%  Author       : Alexander Jean-Claude Bottema
%  Title        : Implementation of the Barnes-Hut Algorithm in
%                 Computing Gravitional Effects upon N bodies
%  Version      : 0.00
%  First rev.   : Apr 24 1995
%  Revidated    : Apr 24 1995
% -----------------------------------------------------------------------------
%
%  Outline of the algorithm:
%
%  The computation of a time step is diveded into 3 subproblems:
%
%  (0) For each mass body M, insert M into the OCT/QUAD-tree representing
%      the space in which the bodies are present.
%
%  (1) Compute the forces of all bodies M.
%
%  (2) Update the positions of all bodies M.
%
%  An OCT-tree (each parent has 8 children) is used if the space is
%  considered to be 3 dimensional. Likewise, a QUAD-tree (each parent
%  has 4 children) is used if the space is considered to be 2 dimensional
%  (which is easier to illustrate and debug).
%
% ***************************************************************************** 

:- dynamic '$random_val'/1.

dimension(20000.0).
theta(0.30).
time_step(1000).

% ----
% star(1000.0,0.0,0.0,0.0,0.0).
% star(2.0,8000.0,0.0,0.0,0.3).
% star(10.0,-7900.0,0.0,0.0,-0.3).

% -----
% star(0.1, -500.0, 500.0, 0.0, 0.0).
% star(0.1, 0.0, 500.0, 0.0, 0.0).
% star(0.1, 500.0, 500.0, 0.0, 0.0).
% star(0.1, -500.0, 0.0, 0.0, 0.0).
% star(0.1, 0.0, 0.0, 0.0, 0.0).
% star(0.1, 500.0, 0.0, 0.0, 0.0).
% star(0.1, -500.0, -500.0, 0.0, 0.0).
% star(0.1, 0.0, -500.0, 0.0, 0.0).
% star(0.1, 500.0, -500.0, 0.0, 0.0).
%
% star(1, 2000.0, -600.0, -0.1, 0.1).

%% ----
%star(100.0,-500.0,-8000.0,0.0,0.5).
%star(100.0,500.0,-7600.0,0.0,0.8).
%star(100.0,3500.0,-5000.0,0.0,0.4).
%star(10.0,4500.0,-5000.0,0.0,0.5).
%%star(10.0,4500.0,-5000.0,0.0,0.5).
%
%get_stars(Stars) :-
%	findall(star(M,X,Y,VX,VY), star(M,X,Y,VX,VY), Stars).

quadrant_to_pos(1,D,D,D).
quadrant_to_pos(2,D,DX,D) :- DX is -D.
quadrant_to_pos(3,D,DD,DD) :- DD is -D.
quadrant_to_pos(4,D,D,DY) :- DY is -D.

pos_to_quadrant(Q,DX,DY) :- (DX >= 0 -> (DY >= 0 -> Q = 1 ; Q = 4) ;
				        (DY >= 0 -> Q = 2 ; Q = 3)).

quadrant_new_node(1,tree(A,B,C,D,M),AddM,NewTree,AA,NA) :-
	!, NewTree = tree(NA,B,C,D,NM),
	AA = A,
	NM is M + AddM.
quadrant_new_node(2,tree(A,B,C,D,M),AddM,NewTree,BB,NB) :-
	!, NewTree = tree(A,NB,C,D,NM),
	BB = B,
	NM is M + AddM.
quadrant_new_node(3,tree(A,B,C,D,M),AddM,NewTree,CC,NC) :-
	!, NewTree = tree(A,B,NC,D,NM),
	CC = C,
	NM is M + AddM.
quadrant_new_node(4,tree(A,B,C,D,M),AddM,NewTree,DD,ND) :-
	!, NewTree = tree(A,B,C,ND,NM),
	DD = D,
	NM is M + AddM.

insert_2d_bodies(Bodies,Tree,NewTree) :-
	dimension(D),
	insert_2d_bodies0(Bodies,Tree,NewTree,D).

insert_2d_bodies0([],Tree,Tree,_).
insert_2d_bodies0([body(M,X,Y)|Bs],Tree,NewTree,D) :-
	insert_quad_tree(Tree,M,X,Y,D,0.0,0.0,Tree0),
	insert_2d_bodies0(Bs,Tree0,NewTree,D).

insert_quad_tree(empty,M,X,Y,_,_,_,body(M,X,Y)).

insert_quad_tree(tree(AA,BB,CC,DD,MM),M,X,Y,D,OX,OY,NewTree) :-
	DX is X - OX,
	DY is Y - OY,
	pos_to_quadrant(Quadrant,DX,DY),
	DHalved is D / 2,
	quadrant_to_pos(Quadrant,DHalved,ODX,ODY),
	NewOX is OX + ODX,
	NewOY is OY + ODY,
	quadrant_new_node(Quadrant,tree(AA,BB,CC,DD,MM),M,NewTree,Node,NewNode),
	insert_quad_tree(Node,M,X,Y,DHalved,NewOX,NewOY,NewNode).

insert_quad_tree(body(M0,X0,Y0),M1,X1,Y1,D,OX,OY,NewTree) :-
	insert_quad_conflict(M0,X0,Y0,M1,X1,Y1,D,OX,OY,NewTree).

insert_quad_conflict(M0,X0,Y0,M1,X1,Y1,D,OX,OY,NewTree) :-
	DX0 is X0 - OX,
	DY0 is Y0 - OY,
	DX1 is X1 - OX,
	DY1 is Y1 - OY,
	DHalved is D / 2,
	pos_to_quadrant(Quadrant0,DX0,DY0),
	pos_to_quadrant(Quadrant1,DX1,DY1),
	(Quadrant0 == Quadrant1 ->
		quadrant_to_pos(Quadrant0,DHalved,ODX,ODY),
		NewOX is OX + ODX,
		NewOY is OY + ODY,
		MSum is M0 + M1,
		quadrant_new_node(Quadrant0,tree(empty,empty,empty,empty,0.0),
				  MSum,
				  NewTree,_,Node),
		insert_quad_conflict(M0,X0,Y0,M1,X1,Y1,DHalved,NewOX,NewOY,Node)
	;	quadrant_new_node(Quadrant0,tree(empty,empty,empty,empty,0.0),
				  M0,
				  NewTree0,_,body(M0,X0,Y0)),
		quadrant_new_node(Quadrant1,NewTree0,M1,NewTree,_,
				  body(M1,X1,Y1))
	).

%
% compute_acceleration(Tree,Dimension,Theta2,OriginX,OriginY,X,Y,AccX,AccY)
%

compute_acceleration(empty,_,_,_,_,_,_,0.0,0.0) :- !.
compute_acceleration(body(M0,X0,Y0),_,_,_,_,X1,Y1,AX,AY) :-
	!,
	DX is X0 - X1,
	DY is Y0 - Y1,
	R2 is DX*DX + DY*DY,
	Divisor is R2 * sqrt(R2),
	(Divisor < 0.0001 ->
	    AX = 0.0, AY = 0.0
	;   Expr is M0 / Divisor,
	    AX is DX * Expr,
	    AY is DY * Expr
	).

compute_acceleration(Tree,D,Theta2,OX,OY,X,Y,AX,AY) :-
	DX is OX - X,
	DY is OY - Y,
	R2 is DX*DX + DY*DY,
	(D*D < Theta2 * R2 ->   % Ok to approximate
		Tree = tree(_,_,_,_,SumM),
		Expr is SumM / (R2 * sqrt(R2)),
		AX0 is DX * Expr,
		AY0 is DY * Expr,
		(AX0 = +nan -> AX = 0.0 ; AX = AX0),
		(AY0 = +nan -> AY = 0.0 ; AY = AY0)
	;	Tree = tree(AA,BB,CC,DD,_), % Not Ok to approximate,
		%
		% Compute partial accelerations obtained by each quadrant
		%
	        DHalved is D / 2,
		quadrant_to_pos(1,DHalved,ODX1,ODY1),
		OX1 is OX + ODX1, OY1 is OY + ODY1,
		compute_acceleration(AA,DHalved,Theta2,OX1,OY1,X,Y,AX1,AY1),
		quadrant_to_pos(2,DHalved,ODX2,ODY2),
		OX2 is OX + ODX2, OY2 is OY + ODY2,
		compute_acceleration(BB,DHalved,Theta2,OX2,OY2,X,Y,AX2,AY2),
		quadrant_to_pos(3,DHalved,ODX3,ODY3),
		OX3 is OX + ODX3, OY3 is OY + ODY3,
		compute_acceleration(CC,DHalved,Theta2,OX3,OY3,X,Y,AX3,AY3),
		quadrant_to_pos(4,DHalved,ODX4,ODY4),
		OX4 is OX + ODX4, OY4 is OY + ODY4,
		compute_acceleration(DD,DHalved,Theta2,OX4,OY4,X,Y,AX4,AY4),
		AX is AX1 + AX2 + AX3 + AX4,
		AY is AY1 + AY2 + AY3 + AY4
	).

%
% compute_tree
%

compute_tree(Stars,Tree) :-
	dimension(D),
	compute_tree0(Stars,D,empty,Tree).

% :- parallel([compute_tree0/4]).

compute_tree0([],_,Tree,Tree).
compute_tree0([star(M,X,Y,_,_)|Stars],D,Tree,NewTree) :-
	insert_quad_tree(Tree,M,X,Y,D,0.0,0.0,Tree0),
	compute_tree0(Stars,D,Tree0,NewTree).

%
% Compute time step given tree
%

compute_time_step(Stars,Tree,NewStars) :-
	dimension(D),
	theta(Theta),
	time_step(Step),
	Theta2 is Theta*Theta,
	write('compute time step 0'), nl,
	compute_time_step0(Stars,D,Theta2,Step,Tree,NewStars).

% :- parallel([compute_time_step0/6]).

compute_time_step0([],_,_,_,_,[]).
compute_time_step0([star(M,X,Y,VX,VY)|Stars],D,Theta2,Step,Tree,
		   [star(M,NewX,NewY,NewVX,NewVY)|NewStars]) :-
%	write('compute acc'), nl,
	compute_acceleration(Tree,D,Theta2,0.0,0.0,X,Y,AX,AY),
	NewVX is VX + Step * AX,
	NewVY is VY + Step * AY,
	NewX is X + Step * NewVX,
	NewY is Y + Step * NewVY,
	compute_time_step0(Stars,D,Theta2,Step,Tree,NewStars).

% -----------------------------------------------------------------------
%  plot
% -----------------------------------------------------------------------

plot_to_file(Tree,File) :-
	open(File,write,Stream),
	plot_to_stream(Tree,Stream),
	close(Stream).

plot_tree_to_stream(Tree,Stream) :-
	dimension(D),
	plot_to_stream0(Tree,0.0,0.0,D,Stream).

plot_tree_to_stream0(tree(AA,BB,CC,DD,_),X,Y,D,Stream) :-
	DH is D / 2,
	AX is X + DH,
	AY is Y + DH,
	BX is X - DH,
	BY is Y + DH,
	CX is X - DH,
	CY is Y - DH,
	DX is X + DH,
	DY is Y - DH,
	plot_tree_to_stream0(AA,AX,AY,DH,Stream),
	plot_tree_to_stream0(BB,BX,BY,DH,Stream),
	plot_tree_to_stream0(CC,CX,CY,DH,Stream),
	plot_tree_to_stream0(DD,DX,DY,DH,Stream).

plot_tree_to_stream0(empty,X,Y,D,Stream) :-
	X0 is X - D,
	Y0 is Y + D,
	X1 is X + D,
	Y1 is Y - D,
	format(Stream,'cross ~w ~w ~w ~w~N',[X0,Y0,X1,Y1]).

plot_tree_to_stream0(body(_,X,Y),_,_,_,Stream) :-
	format(Stream,'plot ~w ~w~N',[X,Y]).

plot_stars_to_stream([],_).
plot_stars_to_stream([star(_,X,Y,_,_)|Stars],Stream) :-
	format(Stream,'plot ~w ~w~N',[X,Y]),
	plot_stars_to_stream(Stars,Stream).

% -----------------------------------------------------------------------
%                                M A I N
% -----------------------------------------------------------------------

random(NewR) :-
	retract('$random_val'(R)),
	!,
	NewR is (R * (9699690 + 1)) mod 510511,
	assert('$random_val'(NewR)).
random(NewR) :-
	NewR is 4711,
	assert('$random_val'(NewR)).

random_max(Max,R) :-
	random(Random),
	R is Random mod Max.

make_stars(0,[]) :- !.
make_stars(N,[star(1.0,X,Y,VX,VY)|Stars]) :-
 	random_max(7500,XX),
	random_max(7500,YY),
 	random_max(1000,VXX),
	random_max(1000,VYY),
	X is XX - 7500 / 2,
	Y is YY - 7500 / 2,
	VX is (VXX - 500) / 10000,
	VY is (VYY - 500) / 10000,
	N0 is N - 1,
	make_stars(N0,Stars).

make_organized_stars(Step,N,Stars) :-
	make_organized_stars0(-8000.0, -8000.0, Step, Stars),
	length(Stars,N).

make_organized_stars0(X,Y,Step,[star(1.0,X,Y,0.0,0.0)|Stars]) :-
	X0 is X + Step,
	(X0 > 8000.0 -> X00 = -8000.0, Y0 is Y + Step
        ;X00 = X0, Y0 = Y),
	Y0 < 8000,
	!,
	make_organized_stars0(X00,Y0,Step,Stars).
make_organized_stars0(_,_,_,[]).

write_stars(Stars,FileName) :-
	open(FileName,write,Stream),
	write_stars0(Stars,Stream),
	close(Stream).

write_stars0([],_).
write_stars0([Star|Stars],Stream) :-
	write(Stream,Star), write(Stream,'.'), nl(Stream),
	write_stars0(Stars,Stream).

read_stars(Stars,FileName) :-
	open(FileName,read,Stream),
	read_stars0(Stars,Stream),
	close(Stream).

read_stars0([Star|Stars],Stream) :-
	read(Stream,Star),
	Star \== end_of_file,
	!,
	read_stars0(Stars,Stream).
read_stars0([],_).

loop(0,_,_) :- !.
loop(N,Stars,Stream) :-
%	plot_stars_to_stream(Stars,Stream),
%	flush_output(Stream),
	statistics(runtime,_),
	write('Computing tree'), nl,
	compute_tree(Stars,Tree),
%	statistics(runtime,[T0,_]),
%	format('runtime tree ~w~N', [T0]),
	write('Computing time step'), nl,
	compute_time_step(Stars,Tree,NewStars),
	statistics(runtime,[_,T]),
	format('runtime ~w~N', [T]),
	N0 is N - 1,
	loop(N0,NewStars,Stream).

main :-
%	make_stars(10,Stars),
%	get_stars(Stars),
	write('reading stars...'), nl,
	read_stars(Stars,'stars.db'),
	write('stars have been read'), nl,
%	unix(exec('plot', [pipe(Stream),null,null],_)),
%	write(Stream,clear), nl(Stream),
	loop(1,Stars,Stream),
	write_stars(Stars,'lut.output').
%	write_stars(Stars,'sics.output').
