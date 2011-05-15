%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  luther-emit.pl - naive back-end
%
%
%   This is the (fairly application specific) file that emits the 'assembly
% format' accepted by the Luther link-loader. I suppose this could be done
% immediately instead of constructing a list of Luther specific terms to be
% handled (i.e. the luther-asm file isn't very useful)
%
% Note: This format is pretty wild in eating space. A true bytecode format
%       would be better.
%

:- ensure_loaded('inline-opcodes').
:- ensure_loaded('byte-opcodes').


emit_compiled_code(Options, Code, Stream) :-
	( has_opt(ascii_format, Options) ->
	    emit_luther_code(Code, Stream)
	; has_opt(byte_format, Options) ->
	    emit_luther_byte_code(Code, Stream)
	; true,
	    error('No output format specified')
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Ascii format generator.
%
%
emit_luther_code([predicate(P,N)|Xs], Stream) :- !,
	format(Stream, 'Predicate(~q ~d~n',[P,N]),
	emit_code(Xs, Stream).

emit_luther_code([directive(P,N)|Xs], Stream) :- !,
	format(Stream, 'Directive(~q ~d~n',[P,N]),
	emit_code(Xs, Stream).

emit_luther_code([comment(C)|Xs], Stream) :- !,
	emit_comment(C, Stream),
	emit_luther_code(Xs, Stream).


emit_code([], Stream) :- !,
	format(Stream, ')~n',[]).

emit_code([X|Xs], Stream) :-
	emit_i(X, Stream),
	emit_code(Xs, Stream).



temp_reg_name(x(R),R).



emit_i(end_of_pred,_) :- !.

emit_i(comment(C), Stream) :- !,
	emit_comment(C, Stream).


emit_i('Global_Get_Value'(R1,R2), Stream) :- !,
	temp_reg_name(R2,B),
	( R1 = y(A) ->
	    format_i(Stream, 'Global_Get_Y_Value', '~d ~d', [A,B])
	; R1 = x(A) ->
	    format_i(Stream, 'Global_Get_X_Value', '~d ~d', [A,B])
	; true,
	    sys_error('emit_i/2: reg not handled ~q (~w)',[R1,'Global_Get_Value'])
	).


emit_i('Global_Unify_Value'(R1), Stream) :- !,
	( R1 = x(A) ->
	    format_i(Stream, 'Global_Unify_X_Value', '~d', [A])
	; R1 = y(A) ->
	    format_i(Stream, 'Global_Unify_Y_Value', '~d', [A])
	; true,
	    sys_error('emit_i/2: reg not handled ~q (~w)',[R1,'Global_Unify_Value'])
	).

emit_i('Global_Unify_Local_Value'(R1), Stream) :- !,
	( R1 = x(A) ->
	    format_i(Stream, 'Global_Unify_X_Local_Value', '~d', [A])
	; R1 = y(A) ->
	    format_i(Stream, 'Global_Unify_Y_Local_Value', '~d', [A])
	; true,
	    sys_error('emit_i/2: reg not handled ~q (~w)',[R1,'Global_Unify_Local_Value'])
	).


emit_i('Get_Structure'(F/N,R), Stream) :- !,
	temp_reg_name(R,A),
	format_i(Stream, 'Get_Structure', '~q ~d ~d', [F,N,A]).

emit_i('Get_Variable'(R1,R2), Stream) :- !,
	temp_reg_name(R2,B),
	( R1 = y(A) ->
	    format_i(Stream, 'Get_Y_Variable', '~d ~d', [A,B])
	; R1 = x(A) ->
	    format_i(Stream, 'Get_X_Variable', '~d ~d',[A,B])
	; true,
	    sys_error('emit_i/2: reg not handled ~q (~w)',[R1,'Get_Variable'])
	).

emit_i('Get_Value'(R1,R2), Stream) :- !,
	temp_reg_name(R2,B),
	( R1 = y(A) ->
	    format_i(Stream, 'Get_Y_Value', '~d ~d', [A,B])
	; R1 = x(A) ->
	    format_i(Stream, 'Get_X_Value', '~d ~d', [A,B])
	; true,
	    sys_error('emit_i/2: reg not handled ~q (~w)',[R1,'Get_Value'])
	).


emit_i('Unify_Structure'(F/N), Stream) :- !,
	format_i(Stream, 'Unify_Structure', '~q ~d', [F,N]).
	
emit_i('Unify_Constant'([]), Stream) :- !,
	format_i(Stream, 'Unify_Nil').

emit_i('Unify_Variable'(R1), Stream) :- !,
	( R1 = x(A) ->
	    format_i(Stream, 'Unify_X_Variable', '~d', [A])
	; R1 = y(A) ->
	    format_i(Stream, 'Unify_Y_Variable', '~d', [A])
	; true,
	    sys_error('emit_i/2: reg not handled ~q (~w)',[R1,'Unify_Variable'])
	).

emit_i('Unify_Value'(R1), Stream) :- !,
	( R1 = x(A) ->
	    format_i(Stream, 'Unify_X_Value', '~d', [A])
	; R1 = y(A) ->
	    format_i(Stream, 'Unify_Y_Value', '~d', [A])
	; true,
	    sys_error('emit_i/2: reg not handled ~q (~w)',[R1,'Unify_Value'])
	).

emit_i('Unify_Local_Value'(R1), Stream) :- !,
	( R1 = x(A) ->
	    format_i(Stream, 'Unify_X_Local_Value', '~d', [A])
	; R1 = y(A) ->
	    format_i(Stream, 'Unify_Y_Local_Value', '~d', [A])
	; true,
	    sys_error('emit_i/2: reg not handled ~q (~w)',[R1,'Unify_Local_Value'])
	).


emit_i('Put_Structure'(F/N,R), Stream) :- !,
	temp_reg_name(R,A),
	format_i(Stream, 'Put_Structure', '~q ~d ~d', [F,N,A]).

emit_i('Put_Variable'(R1,R2), Stream) :- !,
	temp_reg_name(R2,B),
	( R1 = y(A) ->
	    format_i(Stream, 'Put_Y_Variable', '~d ~d', [A,B])
	; R1 = x(A) ->
	    format_i(Stream, 'Put_X_Variable', '~d ~d', [A,B])
	; true,
	    sys_error('emit_i/2: reg not handled ~q (~w)',[R1,'Put_Variable'])
	).

emit_i('Put_Value'(R1,R2), Stream) :- !,
	temp_reg_name(R2,B),
	( R1 = y(A) ->
	    format_i(Stream, 'Put_Y_Value', '~d ~d', [A,B])
	; R1 = x(A) ->
	    format_i(Stream, 'Put_X_Value', '~d ~d', [A,B])
	; true,
	    sys_error('emit_i/2: reg not handled ~q (~w)',[R1,'Put_Value'])
	).

emit_i('Put_Void'(R1), Stream) :- !,
	( R1 = y(A) ->
	    format_i(Stream, 'Put_Y_Void', '~d', [A])
	; R1 = x(A) ->
	    format_i(Stream, 'Put_X_Void', '~d', [A])
	; true,
	    sys_error('emit_i/2: reg not handled ~q (~w)',[R1,'Put_Void'])
	).

emit_i('Put_Unsafe_Value'(R1,R2), Stream) :- !,
	temp_reg_name(R2,B),
	( R1 = y(A) ->
	    format_i(Stream, 'Put_Y_Unsafe_Value', '~d ~d', [A,B])
	; R1 = x(A) ->
	    format_i(Stream, 'Put_X_Unsafe_Value', '~d ~d', [A,B])
	; true,
	    sys_error('emit_i/2: reg not handled ~q (~w)',[R1,'Put_Unsafe_Value'])
	).

emit_i('Switch_On_Term'(V,C,L,N,S), Stream) :- !,
	format_i(Stream, 'Switch_On_Term', '~q ~q ~q ~q ~q', [V,C,N,L,S]). % Note N!

emit_i('Switch_On_Constant'(Cs,Def), Stream) :- !,
	length(Cs,L),
	format_i(Stream, 'Switch_On_Constant', '~d (', [L]),
	emit_table(Cs, Stream),
	format_i(Stream, ')', '~q', [Def]).

emit_i('Switch_On_Structure'(Strs,Def), Stream) :- !,
	length(Strs,L),
	format_i(Stream, 'Switch_On_Structure', '~d (', [L]),
	emit_table(Strs, Stream),
	format_i(Stream, ')', '~q', [Def]).

emit_i('Allocate'(_), Stream) :- !,
	format_i(Stream, 'Allocate').

emit_i('Label'(X), Stream) :- !,
	( var(X) ->
	    sys_error('Unbound label',[])
	; true,
	    format(Stream, '  Label ~d~n', [X])
	).

emit_i('Par_Builtin'(Op,SuspList,LiveListX,LiveY,ArgList), Stream) :- !,
	( SuspList = [] ->
	    emit_i('Builtin'(Op, ArgList), Stream)
	; true,
	    Op = P/N,
	    inline_opcode(P, N, Number,_Type,_UninitArgs),
	    format(Stream, '% par builtin ~q/~d~n',[P,N]),
	    format_i(Stream, 'Par_Builtin', [Number]),
	    emit_delim_args(SuspList, Stream),
	    emit_arg(LiveY, Stream), nl(Stream),
	    sort(LiveListX, Xs),
	    emit_delim_args(Xs, Stream),
	    emit_args(ArgList, Stream)
	).

emit_i('Builtin'(Op,ArgList), Stream) :- !,
	Op = P/N,
	inline_opcode(P,N,Number,_Type,_UninitArgs),	
	format(Stream, '% builtin ~q/~d~n', [P,N]),
	format_i(Stream, 'Builtin', [Number|ArgList]).

emit_i('Init'(N,RegList), Stream) :- !,
	format_i(Stream, 'Init', [N|RegList]).

emit_i('Meta_Call'(x(R),E), Stream) :- !,
	E = (lco=Trim),
	format_i(Stream, 'Meta_Call', '~d ~d', [R,Trim]).

emit_i('Call'(P,N,E), Stream) :- !,
	E = (lco=Trim),
	format_i(Stream, 'Call', '~q ~d ~d', [P,N,Trim]).

emit_i('Meta_Execute'(x(R)), Stream) :- !,
	format_i(Stream, 'Meta_Execute', '~d', [R]).

emit_i('Execute'(P,N), Stream) :- !,
	format_i(Stream, 'Execute', '~q ~d', [P,N]).

emit_i('Start_Left_Body'(L,LiveT,LiveP,_), Stream) :- !,
	format_i(Stream, 'Start_Left_Body', [L]),
	emit_delim_args(LiveT, Stream),
	emit_arg(LiveP, Stream),	%%% LiveP is -1 if there is no env.
	nl(Stream).

emit_i('Start_Right_Body'(R,L,LiveT,LiveP,_), Stream) :- !,
	format_i(Stream, 'Start_Right_Body', [R,L]),
	emit_delim_args(LiveT, Stream),
	emit_arg(LiveP, Stream),	%%% LiveP is -1 if there is no env.
	nl(Stream).

emit_i(Instruction, Stream) :-
	Instruction =.. [Name|ArgList],
	format_i(Stream, Name, ArgList), !.

emit_i(Instruction,_Stream) :-
	sys_error('emit_i/2: ~q caused failure',[Instruction]).



format_i(Stream, Instruction) :-
	write(Stream, '     '),
	write(Stream, Instruction),
	nl(Stream).

format_i(Stream, Instruction, ArgList) :-
	write(Stream, '     '),
	write(Stream, Instruction),
	emit_args(ArgList, Stream).

format_i(Stream, Instruction, Format, ArgList) :-
	write(Stream, '     '),
	write(Stream, Instruction),
	write(Stream, ' '),
	format(Stream, Format, ArgList),
	nl(Stream).
	
%

emit_delim_args(ArgList, Stream) :-
	length(ArgList, N),
	write(Stream, '       '),
	emit_arg(N, Stream),
	write(Stream, ' {'),
	emit_delim_args_aux(ArgList, Stream),
	write(Stream, ' }').


emit_delim_args_aux(    [],_Stream).
emit_delim_args_aux([X|Xs], Stream) :-
	emit_arg(X, Stream),
	emit_delim_args_aux(Xs, Stream).


emit_args(    [], Stream) :- nl(Stream).
emit_args([X|Xs], Stream) :-
	emit_arg(X, Stream),
	emit_args(Xs, Stream).


emit_arg(N, Stream) :- integer(N), !,
	format(Stream, ' ~d', [N]).

emit_arg(N, Stream) :- float(N), !,
	format(Stream, ' ~16g', [N]).

emit_arg(A, Stream) :- atom(A), !,
	format(Stream, ' ~q', [A]).

emit_arg(x(N), Stream) :-
	format(Stream, ' ~d', [N]).

emit_arg(y(N), Stream) :-
	format(Stream, ' ~d', [N]).

emit_arg(Name/Arity, Stream) :-
	format(Stream, ' ~q ~d', [Name,Arity]).



emit_comment(pragma(Lst), Stream) :- !,
	emit_pragma_list(Lst, Stream).

emit_comment(_,_) :- !.

emit_comment(multiline([C|Cs]), Stream) :- !,
	  emit_comment_lines([C|Cs], Stream).

emit_comment(multiline(C), Stream) :- !,
	  emit_comment_lines([C], Stream).

emit_comment(print(C), Stream) :- !,
	format(Stream, '% ~p~n',[C]).

emit_comment(clause(C), Stream) :- !,
	emit_comment_clause(C, Stream).

emit_comment(C, Stream) :-
	emit_comment_lines([C], Stream).

%

emit_pragma_list(    [],_Stream).
emit_pragma_list([X|Xs], Stream) :-
	( X = info(R,P,S,L) ->
	  ( ( nonvar(R), nonvar(P), nonvar(S), nonvar(L) ) ->
	    format(Stream, '% reg ~q: par=~q, seq=~q, loc=~q~n',[R,P,S,L])
	  ; error('emit_pragma_list/2: info/4 has var args~n')
	  )
	; X = unified_with(P,S,L) ->
	  ( ( nonvar(P), nonvar(S), nonvar(L) ) ->
	    format(Stream, '% heap: par=~q, seq=~q, loc=~q~n',[P,S,L])
	  ; error('emit_pragma_list/2: unified_with/3 has var args~n')
	  )
	; warning('unknown pragma ~q -- emitted verbatim~n',[X]),
	  format(Stream, '% ~q~n',[X])
	),
	emit_pragma_list(Xs, Stream).

%

emit_comment_lines(C, Stream) :- var(C), !,
	format(Stream, '% ~w~n',[C]).

emit_comment_lines([],_).

emit_comment_lines([C|Cs], Stream) :-
	format(Stream, '% ~w~n',[C]),
	emit_comment_lines(Cs, Stream).

%

emit_comment_clause((H :- B), Stream) :-
	format(Stream, '% ~p :- ',[H]),
	emit_first_comment_body(B, Stream).

%

emit_first_comment_goal('!', Stream) :- !,
	format(Stream, ' !',[]).

emit_first_comment_goal(G, Stream) :-
	format(Stream, '~n%   ~p',[G]).

%

emit_first_comment_body((G,Gs), Stream) :- !,
	emit_first_comment_goal(G, Stream),
	emit_comment_body(Gs, Stream).

emit_first_comment_body(G, Stream) :- !,
	emit_first_comment_goal(G, Stream),
	format(Stream, '.~n',[]).

%

emit_comment_body((G,Gs), Stream) :- !,
	emit_comment_goal(G, Stream),
	emit_comment_body(Gs, Stream).

emit_comment_body(G, Stream) :- !,
	emit_comment_goal(G, Stream),
	format(Stream, '.~n',[]).

%

emit_comment_goal('!', Stream) :- !,
	format(Stream, ',!',[]).

emit_comment_goal(G, Stream) :-
	format(Stream, ',~n%   ~p',[G]).

%

emit_table(       [],_Stream).

emit_table([(C,L)|T], Stream) :-
	write(Stream, '       '),
	( C = P/0 -> emit_arg(P, Stream) ; emit_arg(C, Stream) ),
	format(Stream, ' ~q~n',[L]),
	emit_table(T, Stream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  Byte format generator.
%
%
% Note:  We do not actually generate true byte code, merly a more compact ascii
%        representation of the instructions.
%
%

emit_luther_byte_code([predicate(P,N)|Xs], Stream) :- !,
	format(Stream, 'Predicate(~q ~d~n', [P,N]),
	emit_byte_code(Xs, Stream).

emit_luther_byte_code([directive(P,N)|Xs], Stream) :- !,
	format(Stream, 'Directive(~q ~d~n', [P,N]),
	emit_byte_code(Xs, Stream).

emit_luther_byte_code([comment(_)|Xs], Stream) :- !,
	emit_luther_byte_code(Xs, Stream).


emit_byte_code([], Stream) :- !,
	format(Stream, ')~n',[]).

emit_byte_code([X|Xs], Stream) :-
	emit_b(X, Stream),
	emit_byte_code(Xs, Stream).



emit_b(end_of_pred,_Stream) :- !.

emit_b(comment(_), _Stream) :- !.


emit_b('Global_Get_Value'(R1,R2), Stream) :- !,
	temp_reg_name(R2,B),
	( R1 = y(A) ->
	    format_b(Stream, 'Global_Get_Y_Value', '~d ~d', [A,B])
	; R1 = x(A) ->
	    format_b(Stream, 'Global_Get_X_Value', '~d ~d', [A,B])
	; true,
	    sys_error('emit_b/2: reg not handled ~q (~w)',[R1,'Global_Get_Value'])
	).


emit_b('Global_Unify_Value'(R1), Stream) :- !,
	( R1 = x(A) ->
	    format_b(Stream, 'Global_Unify_X_Value', '~d', [A])
	; R1 = y(A) ->
	    format_b(Stream, 'Global_Unify_Y_Value', '~d', [A])
	; true,
	    sys_error('emit_b/2: reg not handled ~q (~w)',[R1,'Global_Unify_Value'])
	).

emit_b('Global_Unify_Local_Value'(R1), Stream) :- !,
	( R1 = x(A) ->
	    format_b(Stream, 'Global_Unify_X_Local_Value', '~d', [A])
	; R1 = y(A) ->
	    format_b(Stream, 'Global_Unify_Y_Local_Value', '~d', [A])
	; true,
	    sys_error('emit_b/2: reg not handled ~q (~w)',[R1,'Global_Unify_Local_Value'])
	).


emit_b('Get_Structure'(F/N,R), Stream) :- !,
	temp_reg_name(R,A),
	format_b(Stream, 'Get_Structure', '~q ~d ~d', [F,N,A]).

emit_b('Get_Variable'(R1,R2), Stream) :- !,
	temp_reg_name(R2,B),
	( R1 = y(A) ->
	    format_b(Stream, 'Get_Y_Variable', '~d ~d', [A,B])
	; R1 = x(A) ->
	    format_b(Stream, 'Get_X_Variable', '~d ~d',[A,B])
	; true,
	    sys_error('emit_b/2: reg not handled ~q (~w)',[R1,'Get_Variable'])
	).

emit_b('Get_Value'(R1,R2), Stream) :- !,
	temp_reg_name(R2,B),
	( R1 = y(A) ->
	    format_b(Stream, 'Get_Y_Value', '~d ~d', [A,B])
	; R1 = x(A) ->
	    format_b(Stream, 'Get_X_Value', '~d ~d', [A,B])
	; true,
	    sys_error('emit_b/2: reg not handled ~q (~w)',[R1,'Get_Value'])
	).


emit_b('Unify_Structure'(F/N), Stream) :- !,
	format_b(Stream, 'Unify_Structure', '~q ~d', [F,N]).
	
emit_b('Unify_Constant'([]), Stream) :- !,
	format_b(Stream, 'Unify_Nil').

emit_b('Unify_Variable'(R1), Stream) :- !,
	( R1 = x(A) ->
	    format_b(Stream, 'Unify_X_Variable', '~d', [A])
	; R1 = y(A) ->
	    format_b(Stream, 'Unify_Y_Variable', '~d', [A])
	; true,
	    sys_error('emit_b/2: reg not handled ~q (~w)',[R1,'Unify_Variable'])
	).

emit_b('Unify_Value'(R1), Stream) :- !,
	( R1 = x(A) ->
	    format_b(Stream, 'Unify_X_Value', '~d', [A])
	; R1 = y(A) ->
	    format_b(Stream, 'Unify_Y_Value', '~d', [A])
	; true,
	    sys_error('emit_b/2: reg not handled ~q (~w)',[R1,'Unify_Value'])
	).

emit_b('Unify_Local_Value'(R1), Stream) :- !,
	( R1 = x(A) ->
	    format_b(Stream, 'Unify_X_Local_Value', '~d', [A])
	; R1 = y(A) ->
	    format_b(Stream, 'Unify_Y_Local_Value', '~d', [A])
	; true,
	    sys_error('emit_b/2: reg not handled ~q (~w)',[R1,'Unify_Local_Value'])
	).


emit_b('Put_Structure'(F/N,R), Stream) :- !,
	temp_reg_name(R,A),
	format_b(Stream, 'Put_Structure', '~q ~d ~d', [F,N,A]).

emit_b('Put_Variable'(R1,R2), Stream) :- !,
	temp_reg_name(R2,B),
	( R1 = y(A) ->
	    format_b(Stream, 'Put_Y_Variable', '~d ~d', [A,B])
	; R1 = x(A) ->
	    format_b(Stream, 'Put_X_Variable', '~d ~d', [A,B])
	; true,
	    sys_error('emit_b/2: reg not handled ~q (~w)',[R1,'Put_Variable'])
	).

emit_b('Put_Value'(R1,R2), Stream) :- !,
	temp_reg_name(R2,B),
	( R1 = y(A) ->
	    format_b(Stream, 'Put_Y_Value', '~d ~d', [A,B])
	; R1 = x(A) ->
	    format_b(Stream, 'Put_X_Value', '~d ~d', [A,B])
	; true,
	    sys_error('emit_b/2: reg not handled ~q (~w)',[R1,'Put_Value'])
	).

emit_b('Put_Void'(R1), Stream) :- !,
	( R1 = y(A) ->
	    format_b(Stream, 'Put_Y_Void', '~d', [A])
	; R1 = x(A) ->
	    format_b(Stream, 'Put_X_Void', '~d', [A])
	; true,
	    sys_error('emit_b/2: reg not handled ~q (~w)',[R1,'Put_Void'])
	).

emit_b('Put_Unsafe_Value'(R1,R2), Stream) :- !,
	temp_reg_name(R2,B),
	( R1 = y(A) ->
	    format_b(Stream, 'Put_Y_Unsafe_Value', '~d ~d', [A,B])
	; R1 = x(A) ->
	    format_b(Stream, 'Put_X_Unsafe_Value', '~d ~d', [A,B])
	; true,
	    sys_error('emit_b/2: reg not handled ~q (~w)',[R1,'Put_Unsafe_Value'])
	).

emit_b('Switch_On_Term'(V,C,L,N,S), Stream) :- !,
	format_b(Stream, 'Switch_On_Term', '~q ~q ~q ~q ~q', [V,C,N,L,S]). % Note N!

emit_b('Switch_On_Constant'(Cs,Def), Stream) :- !,
	length(Cs,L),
	format_b(Stream, 'Switch_On_Constant', '~d (', [L]),
	emit_table(Cs, Stream),
	format(Stream, ') ~q~n', [Def]).

emit_b('Switch_On_Structure'(Strs,Def), Stream) :- !,
	length(Strs,L),
	format_b(Stream, 'Switch_On_Structure', '~d (', [L]),
	emit_table(Strs, Stream),
	format(Stream, ') ~q~n', [Def]).

emit_b('Allocate'(_), Stream) :- !,
	format_b(Stream, 'Allocate').

emit_b('Label'(X), Stream) :- !,
	( var(X) ->
	    sys_error('Unbound label',[])
	; true,
	    format(Stream, 'Label ~d~n', [X])
	).

emit_b('Par_Builtin'(Op,SuspList,LiveListX,LiveY,ArgList), Stream) :- !,
	( SuspList = [] ->
	    emit_b('Builtin'(Op, ArgList), Stream)
	; true,
	    Op = P/N,
	    inline_opcode(P, N, Number,_Type,_UninitArgs),
	    format(Stream, '% par builtin ~q/~d~n',[P,N]),
	    format_b(Stream, 'Par_Builtin', [Number]),
	    emit_delim_args(SuspList, Stream),
	    emit_arg(LiveY, Stream), nl(Stream),
	    sort(LiveListX, Xs),
	    emit_delim_args(Xs, Stream),
	    emit_args(ArgList, Stream)
	).

emit_b('Builtin'(Op,ArgList), Stream) :- !,
	Op = P/N,
	inline_opcode(P,N,Number,_Type,_UninitArgs),	
	format_b(Stream, 'Builtin', [Number|ArgList]).

emit_b('Init'(N,RegList), Stream) :- !,
	format_b(Stream, 'Init', [N|RegList]).

emit_b('Meta_Call'(x(R),E), Stream) :- !,
	E = (lco=Trim),
	format_b(Stream, 'Meta_Call', '~d ~d', [R,Trim]).

emit_b('Call'(P,N,E), Stream) :- !,
	E = (lco=Trim),
	format_b(Stream, 'Call', '~q ~d ~d', [P,N,Trim]).

emit_b('Meta_Execute'(x(R)), Stream) :- !,
	format_b(Stream, 'Meta_Execute', '~d', [R]).

emit_b('Execute'(P,N), Stream) :- !,
	format_b(Stream, 'Execute', '~q ~d', [P,N]).

emit_b('Start_Left_Body'(L,LiveT,LiveP,_), Stream) :- !,
	format_b(Stream, 'Start_Left_Body', [L]),
	emit_delim_args(LiveT, Stream),
	emit_arg(LiveP, Stream),	%%% LiveP is -1 if there is no env.
	nl(Stream).

emit_b('Start_Right_Body'(R,L,LiveT,LiveP,_), Stream) :- !,
	format_b(Stream, 'Start_Right_Body', [R,L]),
	emit_delim_args(LiveT, Stream),
	emit_arg(LiveP, Stream), 	%%% LiveP is -1 if there is no env.
	nl(Stream).

emit_b(Instruction, Stream) :-
	Instruction =.. [Name|ArgList],
	format_b(Stream, Name, ArgList), !.

emit_b(Instruction,_Stream) :-
	sys_error('emit_b/2: ~q caused failure',[Instruction]).



format_b(Stream, Instruction) :-
	byte_opcode(Instruction, ByteCode),
	write(Stream, ' X'),
	write(Stream, ByteCode),
	nl(Stream).

format_b(Stream, Instruction, ArgList) :-
	byte_opcode(Instruction, ByteCode),
	write(Stream, ' X'),
	write(Stream, ByteCode),
	emit_args(ArgList, Stream).

format_b(Stream, Instruction, Format, ArgList) :-
	byte_opcode(Instruction, ByteCode),
	write(Stream, ' X'),
	write(Stream, ByteCode),
	write(Stream, ' '),
	format(Stream, Format, ArgList),
	nl(Stream).
