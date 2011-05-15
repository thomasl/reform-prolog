%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  peep.pl
%
%
% Description:
%
%   Peephole optimizations are done on the code resulting from register
% allocation. We can do the following:
%
% o Condense sequences of set_void or unify_void instructions to a
%   single one (the high-level code generator doesn't do it).
%
% o Remove redundant copying:
%     get_variable Xi,Ai -> nothing
%     put_value    Xi,Ai -> nothing
%
%   The "one stream" approach used during code generation prevents us from
% any major rescheduling of instructions. For this reason we have to spend
% some time here in order to ensure (and possibly alter) the instruction
% sequence generated for certain instructions (currently; Put_List and
% Put_Structure).
%
%   Peephole optimization is done twice, each being a single linear pass over
% the code, where the first pass does most of the work.
%   The second pass eliminates useless 'safeness instructions', which are
% introduced after the first peephole optimization pass.
%

peephole_opt(Code,OptCode) :- pho(Code,OptCode).


peephole_opt2(Code,OptCode) :- fast_pho(Code,OptCode).



pho([],[]).

pho(['Allocate'(no_perms)|Code],Opt) :- !,
	pho(Code,Opt).

pho(['Get_Variable'(x(I),x(I))|Code],Opt) :- !,
	pho(Code,Opt).

pho(['Get_Constant'([],R)|Code],['Get_Nil'(R)|Opt]) :- !,
	pho(Code,Opt).

pho(['Put_Value'(x(I),x(I))|Code],Opt) :- !,
	pho(Code,Opt).

pho(['Put_List'(R)|Code],PreCode) :- !,
	ensure_unify_sequence(2,Code,PreCode,['Put_List'(R)|Opt],PostCode),
	pho(PostCode,Opt).

pho(['Put_Structure'(P/N,R)|Code],PreCode) :- !,
	ensure_unify_sequence(N,Code,PreCode,['Put_Structure'(P/N,R)|Opt],PostCode),
	pho(PostCode,Opt).

pho(['Unify_Void'|Code],['Unify_Void'(N)|Opt]) :- !,
	collapse_unify_void(Code,1,N,Rest),
	pho(Rest,Opt).

pho(['Unify_Constant'([])|Code],['Unify_Nil'|Opt]) :- !,
	pho(Code,Opt).


pho(['Call'(P,N,_),'Proceed'|Code],Opt) :- !,
	pho(['Execute'(P,N)|Code],Opt).

pho(['Call'(P,N,_),'Deallocate','Proceed'|Code],['Deallocate'|Opt]) :- !,
	pho(['Execute'(P,N)|Code],Opt).

pho(['Call'(P,N,_),'Deallocate','Execute'(true,0)|Code],['Deallocate'|Opt]) :- !,
	pho(['Execute'(P,N)|Code],Opt).

pho(['Call'(P,N,_),'Execute'(true,0)|Code],Opt) :- !,
	pho(['Execute'(P,N)|Code],Opt).
	
%%% This takes care of meta-calls being 'almost a call' nicely.

pho(['Call'(call,1,LCO)|Code],['Meta_Call'(x(0),LCO)|Opt]) :- !,
	pho(Code,Opt).

pho(['Execute'(call,1)|Code],['Meta_Execute'(x(0))|Opt]) :- !,
	pho(Code,Opt).


pho([X|Code],[X|Opt]) :- pho(Code,Opt).


%%%
%%  Collapse a sequence of 'Unify_Void' instructions into a single
% 'Unify_Void'(N) instruction.
%
% Note: This code sequence may not be terminating, thus no 'nil' case
%       is required.
%
collapse_unify_void(['Unify_Void'|Xs],M,N,Rest) :- !,
	K is M+1,
	collapse_unify_void(Xs,K,N,Rest).

collapse_unify_void(Xs,N,N,Xs).


%%%
%%  If all labels are the same or 'fail', the switch can be shortcircuited
% into a jump to the label (Y below). If this jump is to the next effective
% instruction, it can be eliminated as well.
%
all_same_or_fail([X|Xs],Y) :-
	( X == fail ->
	    all_same_or_fail(Xs,Y)
	; true,
	    all_same_or_fail_aux(Xs,X,Y)
	).

all_same_or_fail_aux(    [],M,M).
all_same_or_fail_aux([X|Xs],M,N) :-
	( X == fail -> true ; X == M ),
	all_same_or_fail_aux(Xs,M,N).

%%%
%%  This is a test: does a jump(Label) instruction actually refer to the next
% instruction?  The thing is to filter out comments (we assume there is only
% one label per row).
%
jump_to_next_insn([X|Xs],Label) :-
	( X = comment(_) ->
	    jump_to_next_insn(Xs,Label)
	; X = 'Label'(N) -> %%% N may be unbound
	    N == Label
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%  The fast PHO removes redundant heap safeness instructions.
%
%
fast_pho([],[]).

fast_pho(['Switch_On_Term'(V,C,L,N,S)|Code],Opt) :-
	all_same_or_fail([V,C,L,N,S],M),
	!,
	fast_pho(['Jump'(M)|Code],Opt).

fast_pho(['Jump'(L)|Code],Opt) :-
	jump_to_next_insn(Code,L),
	!,
	fast_pho(Code,Opt).

fast_pho(['Require_Using'(0,_)|Code],Opt) :- !,
	fast_pho(Code,Opt).

fast_pho(['Require'(0)|Code],Opt) :- !,
	fast_pho(Code,Opt).

fast_pho(['Init'(0,_)|Code],Opt) :- !,
	fast_pho(Code,Opt).

fast_pho([X|Code],[X|Opt]) :- fast_pho(Code,Opt).



ensure_unify_sequence(0,Code,NIL,NIL,Code) :- !.

ensure_unify_sequence(N,[Instruction|Code],PreCode,Hdl,PostCode) :- N > 0,
	( is_unify_instruction(Instruction) ->
	    PreCode  = MorePreCode,
	    PostCode = [Instruction|MorePostCode]
	; true,
	    PreCode  = [Instruction|MorePreCode],
	    PostCode = MorePostCode
	),
	M is N - 1,
	ensure_unify_sequence(M,Code,MorePreCode,Hdl,MorePostCode).


is_unify_instruction('Unify_Variable'(_)).
is_unify_instruction('Unify_Value'(_)).
is_unify_instruction('Unify_Local_Value'(_)).
is_unify_instruction('Unify_Constant'(_)).
is_unify_instruction('Unify_Nil').
is_unify_instruction('Unify_List').
is_unify_instruction('Unify_Structure'(_)).
is_unify_instruction('Unify_Void').
is_unify_instruction('Unify_Nth_Head'(_,_,_)).
is_unify_instruction('Unify_Nth_Tail'(_,_,_)).
is_unify_instruction('Unify_Global_Arg'(_)).
