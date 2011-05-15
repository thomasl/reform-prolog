%    -*- Prolog -*- 
%    File:	main.pl  (~jb/Reform/Luther/Tracer/main.pl)
%    Author:	Johan Bevemyr
%    Created:	Wed May 29 09:48:21 1996
%    Copyright:	(c)Johan Bevemyr
%    Purpose:   
 
:- ensure_loaded('ray_tracer').
:- ensure_loaded('ppm').
:- ensure_loaded('load').

trace1 :-
	Camera = camera(v(0,0,-8),v(-2.0,2.0,50.0),v(0.025,0.0,0.0),
	                v(0.0,-0.025,0.0),160,160),
        Config = config(rgb(70,70,70),reflection(0),shading),
        World = world([sphere(3,9,v(0,0,100),rgb(255,0,0),0.0,
	               no_map,v(0.0,1.0,0.0),v(1.0,0.0,0.0))],
		      [light(v(200,100,-100))]),
	statistics(walltime,[Start|_]),
	trace_pict(Camera,Config,World,Pict),
	statistics(walltime,[TraceTime|_]),
	TTime is TraceTime - Start,
	format('tracing: ~w~n',[TTime]),
%	write_ppm_raw(OutFile,Pict),
	statistics(walltime,[End|_]),
	PrintTime is End - TraceTime,
	format('print: ~w~n',[PrintTime]),
	Walltime is End - Start,
	format('picture traced in ~w mseconds user time~n',[Walltime]).

trace(InFile,OutFile) :-
	load_world(InFile,Config,Camera,World),
	statistics(walltime,[Start|_]),
	trace_pict(Camera,Config,World,Pict),
	statistics(walltime,[TraceTime|_]),
	TTime is TraceTime - Start,
	format('tracing: ~w~n',[TTime]),
	write_ppm_raw(OutFile,Pict),
	statistics(walltime,[End|_]),
	PrintTime is End - TraceTime,
	format('print: ~w~n',[PrintTime]),
	Walltime is End - Start,
	format('picture traced in ~w mseconds user time~n',[Walltime]).
