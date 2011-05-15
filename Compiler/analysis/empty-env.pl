%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		    GENERATE AN EMPTY ENVIRONMENT
%
% The predicate empty_env(+N,-E) is called when a fresh empty
% environment is generated.
%
% We assume:
% (a) an environment is constructed by construct_env/2, with first
%     element a list of initial variable values.
% (b) the initial variable values are 'uninit'.
%
% The empty_env/2 routine has been exhaustively tested up to 250
% variables, which is the max limit. (Terms have max 255 args and
% some are used for 'state'.)

empty_env(N,Env) :-
	empty_var_list(N,Vs,[]),
	construct_env(Vs,Env).

empty_var_list(N) -->
	( { N >= 100 } ->
	  hundred_vars,
	  { M is N-100 },
	  empty_var_list(M)
	; { N >= 50 } ->
	  fifty_vars,
	  { M is N-50 },
	  empty_var_list(M)
	; { N >= 20 } ->
	  twenty_vars,
	  { M is N-20 },
	  empty_var_list(M)
	; small_empty_var_list(N)
	).

twenty_vars -->
	[uninit,uninit,uninit,uninit,uninit,
	 uninit,uninit,uninit,uninit,uninit,
	 uninit,uninit,uninit,uninit,uninit,
	 uninit,uninit,uninit,uninit,uninit].

fifty_vars -->
[uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit].

hundred_vars -->
[uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit,uninit].

small_empty_var_list(X) --> { var(X),!,fail }.
small_empty_var_list(0) --> [].
small_empty_var_list(1) --> [uninit].
small_empty_var_list(2) --> [uninit,uninit].
small_empty_var_list(3) --> [uninit,uninit,uninit].
small_empty_var_list(4) --> [uninit,uninit,uninit,uninit].
small_empty_var_list(5) --> [uninit,uninit,uninit,uninit,uninit].
small_empty_var_list(6) --> [uninit,uninit,uninit,uninit,uninit,uninit].
small_empty_var_list(7) --> [uninit,uninit,uninit,uninit,uninit,uninit,uninit].
small_empty_var_list(8) --> [uninit,uninit,uninit,uninit,uninit,uninit,uninit,
	                     uninit].
small_empty_var_list(9) --> [uninit,uninit,uninit,uninit,uninit,uninit,uninit,
	                     uninit,uninit].
small_empty_var_list(10) --> [uninit,uninit,uninit,uninit,uninit,uninit,uninit,
	                      uninit,uninit,uninit].
small_empty_var_list(11) --> [uninit,uninit,uninit,uninit,uninit,uninit,uninit,
                              uninit,uninit,uninit,uninit].
small_empty_var_list(12) --> [uninit,uninit,uninit,uninit,uninit,uninit,uninit,
	                      uninit,uninit,uninit,uninit,uninit].
small_empty_var_list(13) --> [uninit,uninit,uninit,uninit,uninit,uninit,uninit,
	                      uninit,uninit,uninit,uninit,uninit,uninit].
small_empty_var_list(14) --> [uninit,uninit,uninit,uninit,uninit,uninit,uninit,
	                      uninit,uninit,uninit,uninit,uninit,uninit,uninit
			     ].
small_empty_var_list(15) --> [uninit,uninit,uninit,uninit,uninit,uninit,uninit,
	                      uninit,uninit,uninit,uninit,uninit,uninit,uninit,
			      uninit].
small_empty_var_list(16) --> [uninit,uninit,uninit,uninit,uninit,uninit,uninit,
	                      uninit,uninit,uninit,uninit,uninit,uninit,uninit,
			      uninit,uninit].
small_empty_var_list(17) --> [uninit,uninit,uninit,uninit,uninit,uninit,uninit,
	                      uninit,uninit,uninit,uninit,uninit,uninit,uninit,
			      uninit,uninit,uninit].
small_empty_var_list(18) --> [uninit,uninit,uninit,uninit,uninit,uninit,uninit,
	                      uninit,uninit,uninit,uninit,uninit,uninit,uninit,
                              uninit,uninit,uninit,uninit].
small_empty_var_list(19) --> [uninit,uninit,uninit,uninit,uninit,uninit,uninit,
	                      uninit,uninit,uninit,uninit,uninit,uninit,uninit,
                              uninit,uninit,uninit,uninit,uninit].

