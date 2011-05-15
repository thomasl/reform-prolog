% -*- Prolog -*- 
%    File:	ppm-write.pl  (~jb/projects/ray-tracer/ppm-write.pl)
%    Author:	Johan Bevemyr
%    Created:	Fri Jun 30 12:31:35 1995
%    Purpose:   
 
:- ensure_loaded(support).

write_ppm_ascii(File,Raw) :-
	open(File,write,Stream),
	write_ppm_stream(Raw,Stream),
	close(Stream).

write_ppm_stream(Raw,Stream) :-
	size(Raw,Width,Height),
	write(Stream,'P3'),nl(Stream),
	write(Stream,'# CREATOR: Prolog ray tracer'), nl(Stream),
	format(Stream,'~w ~w~n255~n',[Width,Height]),
	write_ppm_bitmap_rows(Raw,Stream).

write_ppm_bitmap_rows([],_Stream).
write_ppm_bitmap_rows([Row|Rows],Stream) :-
	write_ppm_row(Row,Stream),
	write_ppm_bitmap_rows(Rows,Stream).

write_ppm_row([],Stream) :- nl(Stream).
write_ppm_row([rgb(R,G,B)|Row],Stream) :-
	write(Stream,R), write(Stream,' '),
	write(Stream,G), write(Stream,' '),
	write(Stream,B), write(Stream,' '),
	write_ppm_row(Row,Stream).

%

write_ppm_raw(File,Raw) :-
	open(File,write,Stream),
	write_ppm_stream_raw(Raw,Stream),
	close(Stream).

write_ppm_stream_raw(Raw,Stream) :-
	size(Raw,Width,Height),
	write(Stream,'P6'),nl(Stream),
	write(Stream,'# CREATOR: Prolog ray tracer'), nl(Stream),
	format(Stream,'~w ~w ~n255~n',[Width,Height]),
	write_ppm_bitmap_rows_raw(Raw,Stream).

write_ppm_bitmap_rows_raw([],_Stream).
write_ppm_bitmap_rows_raw([Row|Rows],Stream) :-
	write_ppm_row_raw(Row,Stream),
	write_ppm_bitmap_rows_raw(Rows,Stream).

write_ppm_row_raw([],_Stream).
write_ppm_row_raw([rgb(R,G,B)|Row],Stream) :-
	put(Stream,R),put(Stream,G),put(Stream,B),
	write_ppm_row_raw(Row,Stream).



