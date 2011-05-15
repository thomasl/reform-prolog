% -*- Prolog -*- 
%    File:	ppm-read.pl  (~jb/projects/ray-tracer/ppm-read.pl)
%    Author:	Johan Bevemyr
%    Created:	Fri Jun 30 14:15:19 1995
%    Purpose:   
 
:- ensure_loaded(support).

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

