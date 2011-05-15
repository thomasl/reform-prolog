%    -*- Prolog -*- 
%    File:	ppm.pl  (~jb/Reform/Luther/Tracer/ppm.pl)
%    Author:	Johan Bevemyr
%    Created:	Tue May 28 17:36:56 1996
%    Copyright:	(c)Johan Bevemyr
%    Purpose:   
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Write PPM
%

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Read PPM
%

read_ppm(File,BitMap) :-
	open(File,read,Stream),
	read_ppm_stream(Stream,BitMap),
	close(Stream).

read_ppm_stream(Stream,BitMap) :-
	read_ppm_spec(Format,Width,Height,Stream),
	(
	    Format = 6 ->
	    read_ppm_raw(Height,Width,BitMap,Stream)
	;
	    read_ppm_ascii(Height,Width,BitMap,Stream)
	).

read_ppm_spec(Format,Width,Height,Stream) :-
	read_digit(Stream,Format),
	skip_to_nl(Stream),
	read_digit(Stream,Width),
	read_digit(Stream,Height),
	read_digit(Stream,_MaxCol).

skip_to_nl(Stream) :-
	skip_to_char(Stream,10).

%

read_ppm_raw(0,_Width,BM,_Stream) :- !, BM = [].
read_ppm_raw(Height,Width,[Row|Rows],Stream) :-
	read_ppm_raw_row(Width,Row,Stream),
	H2 is Height - 1,
	read_ppm_raw(H2,Width,Rows,Stream).

read_ppm_raw_row(0,Row,_Stream) :- !, Row = [].
read_ppm_raw_row(Width,[rgb(R,G,B)|Row],Stream) :-
	get0(Stream,R), get0(Stream,G), get0(Stream,B),
	W2 is Width - 1,
	read_ppm_raw_row(W2,Row,Stream).

%

read_ppm_ascii(0,_Width,BM,_Stream) :- !, BM = [].
read_ppm_ascii(Height,Width,[Row|Rows],Stream) :-
	read_ppm_ascii_row(Width,Row,Stream),
	H2 is Height - 1,
	read_ppm_ascii(H2,Width,Rows,Stream).

read_ppm_ascii_row(0,Row,_Stream) :- !, Row = [].
read_ppm_ascii_row(Width,[rgb(R,G,B)|Row],Stream) :-
	read_digit(Stream,R),
	read_digit(Stream,G),
	read_digit(Stream,B),
	W2 is Width - 1,
	read_ppm_ascii_row(W2,Row,Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%

read_chars(0, _Stream, Chars) :- !, Chars = [].

read_chars(N,Stream,[C|Cs]) :-
	N > 0,
	get0(Stream,C),
	N2 is N - 1,
	read_chars(N2,Stream,Cs).

%

read_digit(Stream,Digit) :-
	skip_nondigits(Stream,FirstDig),
	read_digits(Stream,Digs),
	number_chars(Digit,[FirstDig|Digs]).

%

read_digits(Stream,Digits) :-
	get0(Stream,Char),
	(
	    is_digit(Char) ->
	    Digits = [Char|Rest],
	    read_digits(Stream,Rest)
	;
	    Digits = []
	).

%

skip_nondigits(Stream,FirstDig) :-
	get0(Stream,Char),
	(
	    is_digit(Char) ->
	    FirstDig = Char
	;
	    skip_nondigits(Stream,FirstDig)
	).

%

is_digit(Char) :-
	Char =< "9",
	Char >= "0".

%

skip_to_char(Stream,Char) :-
	get0(Stream,InChar),
	(
	    InChar = Char -> true
	;
	    skip_to_char(Stream,Char)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

size(Raw,Width,Height) :-
	length(Raw,Height),
	Raw = [Row|_],
	length(Row,Width).

