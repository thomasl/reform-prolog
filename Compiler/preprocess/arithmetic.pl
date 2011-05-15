%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  arithmetic.pl
%
%
%

arithmetic_goal(_X is  _E).
arithmetic_goal(_X  <  _Y).
arithmetic_goal(_X =<  _Y).
arithmetic_goal(_X  >  _Y).
arithmetic_goal(_X  >= _Y).
arithmetic_goal(_X =:= _Y).
arithmetic_goal(_X =\= _Y).


% Expansion of arithmetic goals is done here.

expand_arithmetic_goal(X is E) -->
	expand_arithmetic(E,R),
	[X = R].

expand_arithmetic_goal(X  < Y) -->
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	[R1 < R2].

expand_arithmetic_goal(X =< Y) -->
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	[R1 =< R2].

expand_arithmetic_goal(X  > Y) -->
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	[R1 > R2].

expand_arithmetic_goal(X >= Y) -->
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	[R1 >= R2].

expand_arithmetic_goal(X =:= Y) -->
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	[R1 =:= R2].

expand_arithmetic_goal(X =\= Y) -->
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	[R1 =\= R2].


% Arithmetic expressions are open-coded and optimized a bit.

expand_arithmetic(X,R) --> { var(X), !, R = X }.

expand_arithmetic(X,R) --> { number(X), !, R = X }.

expand_arithmetic([X],R) --> { number(X), !, R = X }.

expand_arithmetic(+(X),R) --> !,
	expand_arithmetic(X,R).

expand_arithmetic(X+Y,R) --> !,
	( { integer(X) } ->
	  ( { X =:= 1 } ->
	    expand_arithmetic(Y,R2),
	    generate_add_one(R,R2)
	  ; { X =:= 0 } ->
	    expand_arithmetic(Y,R)
	  ; expand_arithmetic(X,R1),
	    expand_arithmetic(Y,R2),
	    generate_add(R,R1,R2)
	  )
	; { integer(Y) } ->
	  ( { Y =:= 1 } ->
	    expand_arithmetic(X,R1),
	    generate_add_one(R,R1)
	  ; { Y =:= 0 } ->
	    expand_arithmetic(X,R)
	  ; expand_arithmetic(X,R1),
	    expand_arithmetic(Y,R2),
	    generate_add(R,R1,R2)
	  )
	; expand_arithmetic(X,R1),
	  expand_arithmetic(Y,R2),
	  generate_add(R,R1,R2)
	).

expand_arithmetic(-(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_neg(R,R1).

expand_arithmetic(X-Y,R) --> !,
	( { number(Y) } ->
	  ( { Y =:= 1 } ->
	    expand_arithmetic(X,R1),
	    generate_sub_one(R,R1)
	  ; { Y =:= 0 } ->
	    expand_arithmetic(X,R)
	  ; expand_arithmetic(X,R1),
	    expand_arithmetic(Y,R2),
	    generate_sub(R,R1,R2)
	  )
	; expand_arithmetic(X,R1),
	  expand_arithmetic(Y,R2),
	  generate_sub(R,R1,R2)
	).

expand_arithmetic(X*Y,R) --> !,
	( { number(X) } ->
	  ( { X =:= 0 } ->
	    { R = 0 }
	  ; { X =:= 1 } ->
	    expand_arithmetic(Y,R)
	  ; expand_arithmetic(X,R1),
	    expand_arithmetic(Y,R2),
	    generate_mul(R,R1,R2)
	  )
	; { number(Y) } ->
	  ( { Y =:= 0 } ->
	    { R = 0 }
	  ; { Y =:= 1 } ->
	    expand_arithmetic(X,R)
	  ; expand_arithmetic(X,R1),
	    expand_arithmetic(Y,R2),
	    generate_mul(R,R1,R2)
	  )
	; expand_arithmetic(X,R1),
	  expand_arithmetic(Y,R2),
	  generate_mul(R,R1,R2)
	).

expand_arithmetic(X/Y,R) --> !,
	( { integer(X), X =:= 0 } ->
	  { R = 0 }
	; { integer(Y), Y =:= 1 } ->
	  expand_arithmetic(X,R)
	; expand_arithmetic(X,R1),
	  expand_arithmetic(Y,R2),
	  generate_div(R,R1,R2)
	).

expand_arithmetic(X//Y,R) --> !,
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	generate_intdiv(R,R1,R2).

expand_arithmetic(min(X,Y),R) --> !,
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	generate_min(R,R1,R2).

expand_arithmetic(max(X,Y),R) --> !,
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	generate_max(R,R1,R2).

expand_arithmetic(X mod Y,R) --> !,
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	generate_mod(R,R1,R2).

expand_arithmetic(abs(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_abs(R,R1).

expand_arithmetic(log(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_log(R,R1).

expand_arithmetic(exp(X,Y),R) --> !,
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	generate_pow(R,R1,R2).

expand_arithmetic(exp(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_exp(R,R1).

expand_arithmetic(sqrt(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_sqrt(R,R1).

expand_arithmetic(cbrt(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_cbrt(R,R1).


expand_arithmetic(sin(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_sin(R,R1).

expand_arithmetic(cos(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_cos(R,R1).

expand_arithmetic(tan(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_tan(R,R1).

expand_arithmetic(asin(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_asin(R,R1).

expand_arithmetic(acos(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_acos(R,R1).

expand_arithmetic(atan(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_atan(R,R1).


expand_arithmetic(X\/Y,R) --> !,
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	generate_b_or(R,R1,R2).

expand_arithmetic(X/\Y,R) --> !,
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	generate_b_and(R,R1,R2).

expand_arithmetic(X#Y,R) --> !,
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	generate_b_xor(R,R1,R2).

expand_arithmetic(\(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_b_not(R,R1).

expand_arithmetic(X<<Y,R) --> !,
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	generate_lshift(R,R1,R2).

expand_arithmetic(X>>Y,R) --> !,
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	generate_rshift(R,R1,R2).

expand_arithmetic(msb(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_msb(R,R1).

expand_arithmetic(rng(X,Y,Z),R) --> !,
	expand_arithmetic(X,R1),
	expand_arithmetic(Y,R2),
	expand_arithmetic(Z,R3),
	generate_rng(R,R1,R2,R3).


expand_arithmetic(integer(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_tointeger(R,R1).

expand_arithmetic(float(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_tofloat(R,R1).

expand_arithmetic(floor(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_floor(R,R1).

expand_arithmetic(ceiling(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_ceiling(R,R1).

expand_arithmetic(truncate(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_truncate(R,R1).

expand_arithmetic(round(X),R) --> !,
	expand_arithmetic(X,R1),
	generate_round(R,R1).


%%% X is A[I] => X is aref(A,I) => $aref(A,I,X). (expanded by the reader, then here.)

expand_arithmetic(aref(Array,Index),R) --> !,
	expand_arithmetic(Index,Ri),
	['$aref'(Array,Ri,R)].

expand_arithmetic(X,_) -->
	{ sys_error('unknown arithmetic op ~q',[X]) }.


%%%
%%
%
generate_add(R,R1,R2) -->
	( { number(R1), number(R2) } ->
	    { R is R1+R2 }
	; ['$plus'(R,R1,R2)] ).

generate_add_one(R,R1) -->
	( { number(R1) } ->
	    { R is R1+1 }
	; ['$plus_1'(R,R1)] ).

generate_sub(R,R1,R2) -->
	( { R1 == R2 } ->
	    { R is 0 }
	; { number(R1), number(R2) } ->
	    { R is R1-R2 }
	; ['$minus'(R,R1,R2)] ).

generate_sub_one(R,R1) -->
	( { number(R1) } ->
	    { R is R1-1 }
	; ['$minus_1'(R,R1)] ).

generate_mul(R,R1,R2) -->
	( { number(R1), number(R2) } ->
	    { R is R1*R2 }
	; ['$times'(R,R1,R2)] ).

generate_div(R,R1,R2) -->
	( { number(R1), number(R2) } ->
	    { R is R1/R2 }
	; ['$div'(R,R1,R2)] ).

generate_intdiv(R,R1,R2) -->
	( { integer(R1), integer(R2) } ->
	    { R is R1//R2 }
	; ['$intdiv'(R,R1,R2)] ).

generate_mod(R,R1,R2) -->
	( { integer(R1), integer(R2) } ->
	    { R is R1 mod R2 }
	; ['$mod'(R,R1,R2)] ).

generate_min(R,R1,R2) -->
	( { number(R1), number(R2) } ->
	    { R is min(R1,R2) }
	; ['$min'(R,R1,R2)] ).

generate_max(R,R1,R2) -->
	( { number(R1), number(R2) } ->
	    { R is max(R1,R2) }
	; ['$max'(R,R1,R2)] ).

generate_abs(R,R1) -->
	( { number(R1) } ->
	    { R is abs(R1) }
	; ['$abs'(R,R1)] ).

generate_neg(R,R1) -->
	( { number(R1) } ->
	    { R is -R1 }
	; ['$neg'(R,R1)] ).

generate_log(R,R1) -->
	( { number(R1) } ->
	    { R is log(R1) }
	; ['$log'(R,R1)] ).

generate_exp(R,R1) -->
	( { number(R1) } ->
	    { R is exp(R1) }
	; ['$exp'(R,R1)] ).

generate_pow(R,R1,R2) -->
	( { number(R1), number(R2) } ->
	    { R is exp(R1,R2) }
	; ['$pow'(R,R1,R2)] ).

generate_sqrt(R,R1) -->
	( { number(R1) } ->
	    { R is sqrt(R1) }
	; ['$sqrt'(R,R1)] ).

generate_cbrt(R,R1) -->
	( { number(R1) } ->
	    { R is cbrt(R1) }
	; ['$cbrt'(R,R1)] ).


generate_sin(R,R1) -->
	( { number(R1) } ->
	    { R is sin(R1) }
	; ['$sin'(R,R1)] ).

generate_cos(R,R1) -->
	( { number(R1) } ->
	    { R is cos(R1) }
	; ['$cos'(R,R1)] ).

generate_tan(R,R1) -->
	( { number(R1) } ->
	    { R is tan(R1) }
	; ['$tan'(R,R1)] ).

generate_asin(R,R1) -->
	( { number(R1) } ->
	    { R is asin(R1) }
	; ['$asin'(R,R1)] ).

generate_acos(R,R1) -->
	( { number(R1) } ->
	    { R is acos(R1) }
	; ['$acos'(R,R1)] ).

generate_atan(R,R1) -->
	( { number(R1) } ->
	    { R is atan(R1) }
	; ['$atan'(R,R1)] ).


generate_b_or(R,R1,R2) -->
	( { integer(R1), integer(R2) } ->
	    { R is R1 \/ R2 }
	; ['$b_or'(R,R1,R2)] ).

generate_b_and(R,R1,R2) -->
	( { integer(R1), integer(R2) } ->
	    { R is R1 /\ R2 }
	; ['$b_and'(R,R1,R2)] ).

generate_b_xor(R,R1,R2) -->
	( { integer(R1), integer(R2) } ->
	    { R is R1 # R2 }
	; ['$b_xor'(R,R1,R2)] ).

generate_b_not(R,R1) -->
	( { integer(R1) } ->
	    { R is \(R1) }
	; ['$b_not'(R,R1)] ).

generate_lshift(R,R1,R2) -->
	( { integer(R1), integer(R2) } ->
	    { R is R1 << R2 }
	; ['$lshift'(R,R1,R2)] ).

generate_rshift(R,R1,R2) -->
	( { integer(R1), integer(R2) } ->
	    { R is R1 >> R2 }
	; ['$rshift'(R,R1,R2)] ).

generate_msb(R,R1) -->
	( { integer(R1) } ->
	    { R is msb(R1) }
	; ['$msb'(R,R1)] ).

generate_rng(R,R1,R2,R3) -->
	( { integer(R1), integer(R2), integer(R3) } ->
	    { R is rng(R1,R2,R3) }
	; ['$rng'(R,R1,R2,R3)] ).


generate_tointeger(R,R1) -->
	( { number(R1) } ->
	    { R is integer(R1) }
	; ['$tointeger'(R,R1)] ).

generate_tofloat(R,R1) -->
	( { number(R1) } ->
	    { R is float(R1) }
	; ['$tofloat'(R,R1)] ).

generate_floor(R,R1) -->
	['$floor'(R,R1)].

generate_ceiling(R,R1) -->
	['$ceiling'(R,R1)].

generate_truncate(R,R1) -->
	['$truncate'(R,R1)].

generate_round(R,R1) -->
	['$round'(R,R1)].
