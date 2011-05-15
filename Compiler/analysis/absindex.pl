%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                         ABSTRACT INDEXING
%
% This file implements abstract indexing. 
%
% Abstract indexing selects the subset of clauses applicable to all
% calls possible from the call pattern.
%
% Note for subsequent implementations: the work done to get
%  everything precisely right is perhaps better done before the
%  analysis. In that case, add a table that tells what clauses
%  are applicable with what call patterns BEFORE analysis. The
%  table should be 'indexed' by type.
%
% The current implementation is liable to be a bit slow.

:- ensure_loaded(listlattice).

absindex(CP,ix(V,C,L,N,S,Cs),CpsCsEs) :- !,
        ( var(CP) -> sys_error('absindex/3: variable callpat')
        ; atom(CP) -> % predicate has no argument
          Cs =.. [_|Clauses],
          construct_unmodified_cp_ce_pairs(Clauses,CP,CpsCsEs)
        ; CP = shr(Shr,_Inst,Tup) -> % does this ever occur??
          install_aliases(Shr,Tup),
          absindex(Tup,ix(V,C,L,N,S,Cs),CpsCsEs)
        ; CP =.. [Det,IxArg|Args],
	  timedep_if_shared(IxArg),
          index_on(IxArg,Args,Det,V,C,L,N,S,Cs,TmpCpsCsEs),
          construct_cp_ce_pairs(TmpCpsCsEs,CpsCsEs)
        ).

% Used by user annotator:
% (The r.p. case should be expanded to simulate actual
% indexing simulation as in ra4, I think.)

absindex_ret_all_clauses(CP,ix(V,C,L,N,S,Cs),CpsCsEs) :- !,
        ( var(CP) -> sys_error('variable callpat in absindex')
        ; atom(CP) -> % predicate has no argument
          Cs =.. [_|Clauses],
          construct_unmodified_cp_ce_pairs(Clauses,CP,CpsCsEs)
        ; CP = shr(Shr,_Inst,Tup) -> % does this ever occur??
          install_aliases(Shr,Tup),
          absindex_ret_all_clauses(Tup,ix(V,C,L,N,S,Cs),CpsCsEs)
        ; CP =.. [Det,IxArg|Args],
	  timedep_if_shared(IxArg),
          index_on(IxArg,Args,Det,V,C,L,N,S,Cs,TmpCpsCsEs),
          construct_numbered_cp_ce_pairs(TmpCpsCsEs,ModCpsCsEs),
	  add_uncalled_clauses(ModCpsCsEs,Cs,NumCpsCsEs),
	  sort(NumCpsCsEs,SortCpsCsEs),
	  remove_numbering(SortCpsCsEs,CpsCsEs)
        ).

absindex_ret_all_clauses(CP,rp(Base,Rec,Seq,Type,LB,RB),CpsCsEs) :- !,
        ( var(CP) -> sys_error('variable callpat in absindex')
        ; CP = shr(Shr,_Inst,Tup) -> % does this ever occur??
          install_aliases(Shr,Tup),
          absindex_ret_all_clauses(Tup,rp(Base,Rec,Seq,Type,LB,RB),CpsCsEs)
	; Type = listrec ->
	  CpsCsEs = [(CP,Base),(CP,Seq)]
	; Type = intrec ->
	  CpsCsEs = [(CP,Base),(CP,Seq)]
        ).


index_on(nil,Args,Det,_V,C,_L,_N,_S,Cs,CpsCsEs) :-
        clauses_from_table(C,Det,nil,Args,Cs,[],CpsCsEs).

index_on(gnd,Args,Det,_V,C,L,N,S,Cs,CpsCsEs) :-
        clauses_from_table(C,Det,gnd,Args,Cs,[],CRest),
        clauses_from_seq(L,Det,gnd,Args,Cs,CRest,LRest),
        clauses_from_table(N,Det,gnd,Args,Cs,LRest,NRest),
        clauses_from_table(S,Det,gnd,Args,Cs,NRest,CpsCsEs).

 % In the list-sequence case it might be better to make
 % nv(...) into any(...) (!) since any(...) coexists better
 % with lists. A second solution would be to distinguish
 % lists of at least one cons from others. A third
 % solution is to hack around this case by recognizing when
 % the one-consness is a fact for at least concrete terms
 % (e.g. nv(...) vs a list type w. concrete term [_|_] does
 % indeed refine into a list)
 %
 % The basic problem is type refinement interacting with the above.

index_on(nv(X,Y,Z),Args,Det,_V,C,L,N,S,Cs,CpsCsEs) :-
        NV = nv(X,Y,Z),
	GND = gnd,
        clauses_from_table(C,Det,GND,Args,Cs,[],CRest),
        clauses_from_seq(L,Det,NV,Args,Cs,CRest,LRest),
        clauses_from_table(N,Det,GND,Args,Cs,LRest,NRest),
        clauses_from_table(S,Det,NV,Args,Cs,NRest,CpsCsEs).

index_on(any(X,Y,Z),Args,Det,V,C,L,N,S,Cs,CpsCsEs) :-
%	A = any(X,Y,Z),
        NV = nv(X,Y,Z),
	A = NV,
        F = free(_,X,Z),
	GND = gnd,
        clauses_from_seq(V,Det,F,Args,Cs,[],VRest),
        clauses_from_table(C,Det,GND,Args,Cs,VRest,CRest),
        clauses_from_seq(L,Det,A,Args,Cs,CRest,LRest),
        clauses_from_table(N,Det,GND,Args,Cs,LRest,NRest),
        clauses_from_table(S,Det,NV,Args,Cs,NRest,CpsCsEs).

index_on(free(X,Y,Z),Args,Det,V,_C,_L,_N,_S,Cs,CpsCsEs) :-
        F = free(X,Y,Z),
        clauses_from_seq(V,Det,F,Args,Cs,[],CpsCsEs).

index_on(free_nil(X,Y,Z),Args,Det,V,C,_L,_N,_S,Cs,CpsCsEs) :-
        F = free_nil(X,Y,Z),
        clauses_from_seq(V,Det,F,Args,Cs,[],VRest),
        clauses_from_table(C,Det,nil,Args,Cs,VRest,CpsCsEs).

index_on(Lst,Args,Det,V,C,L,_N,_S,Cs,CpsCsEs) :-
        list_type(Lst,_H_id,_H_attr,T_id,T_attr),
        clauses_from_seq(L,Det,Lst,Args,Cs,
                         [],LRest),
        cnv_lub(T_id,Full_T_id),
        the_type(T,Full_T_id,[T_attr]),
        ( T = nil ->
          clauses_from_table(C,Det,T,Args,Cs,LRest,CpsCsEs)
        ; T = free_nil(X,Y,Z) ->
          F = free(X,Y,Z),
          clauses_from_seq(V,Det,F,Args,Cs,LRest,VRest),
          clauses_from_table(C,Det,nil,Args,Cs,VRest,CpsCsEs)
        ; T = free(X,Y,Z) ->
          clauses_from_seq(V,Det,T,Args,Cs,LRest,CpsCsEs)
        ).

%

clauses_from_seq([],_Det,_IxArg,_Args,_Cs,Seq,Seq).

clauses_from_seq([X|Xs],Det,IxArg,Args,Cs,Seq,NewSeq) :-
        cs_from_seq(Xs,X,Det,IxArg,Args,Cs,Seq,NewSeq).

% Gradually insert new clauses to be tried in the sequence.

cs_from_seq([],X,Det,IxArg,Args,Cs,Seq,NewSeq) :-
        arg(X,Cs,ClauseEnv),
        insert_clause(Seq,NewSeq,X,Det,IxArg,Args,ClauseEnv).

cs_from_seq([Y|Ys],X,Det,IxArg,Args,Cs,Seq,NewSeq) :-
        arg(X,Cs,ClauseEnv),
	( Det = nondet ->
	  ClsDet = nondet
	; ClsDet = shallow
	),
        insert_clause(Seq,CurrSeq,X,ClsDet,IxArg,Args,ClauseEnv),
        cs_from_seq(Ys,Y,Det,IxArg,Args,Cs,CurrSeq,NewSeq).

% This predicate inserts all clause sequences applicable from
% a table. A more advanced implementation could index into the
% table; in our case, we prefer simplicity.
% - if the table is a clause sequence, that is the result too
% - if the table is an actual jump table, All contains a list
%   of clause sequences, which is inserted into the Seq. (This
%   is a list like '[[1,2],[2],[2]]' etc, which is what the
%   various entries will use. If all entries are [] or [_],
%   the result will be 'det' call patterns (if possible);
%   [_,_,...,_] will result in some nondets. If All had been
%   a simple clause sequence, the approximation would have had
%   to be nondet mostly.)

clauses_from_table([],_Det,_Ix,_Args,_Cs,Seq,Seq).

clauses_from_table([X|Xs],Det,Ix,Args,Cs,Seq,NewSeq) :-
        clauses_from_seq([X|Xs],Det,Ix,Args,Cs,Seq,NewSeq).

clauses_from_table(table(_Es,_Dflt,All),Det,Ix,Args,Cs,Seq,NewSeq) :-
        clauses_from_sequences(All,Det,Ix,Args,Cs,Seq,NewSeq).

%

clauses_from_sequences([],_Det,_Ix,_Args,_Cs,Seq,Seq).

clauses_from_sequences([X|Xs],Det,Ix,Args,Cs,Seq,NewSeq) :-
        clauses_from_seq(X,Det,Ix,Args,Cs,Seq,CurrSeq),
        clauses_from_sequences(Xs,Det,Ix,Args,Cs,CurrSeq,NewSeq).

% Inserting clauses in the clause sequence is complicated by the
% possibility that a clause is entered from several points. Since
% the call pattern is 'approximately' the same for all the clauses,
% we use a 'pseudo-lub' to get the actual call pattern for the clause.
% This lub only considers types (since that is the only part that
% differs).
%
% Each clause to be indexed has the form:
%    e(ClauseNumber,Determinacy,FirstArg,RestArgs,Clause+Env)
%
% After indexing is finished, we form CP and Clause+Env for future use.
%
% NB. The algorithm used here is quadratic in # of clauses (that are
%     applicable from indexing).

insert_clause([],Seq,ClauseNo,Det,IxType,Args,ClauseEnv) :-
        Seq = [e(ClauseNo,Det,IxType,Args,ClauseEnv)].

insert_clause([X|Xs],Ys,ClauseNo,Det,IxType,Args,ClauseEnv) :-
        X = e(C0,D0,Ix,A0,CE0),
        ( C0 =:= ClauseNo ->
          mod_det(D0,Det,NewDet),
          fast_lub(Ix,IxType,NewIxType),
          Ys = [e(C0,NewDet,NewIxType,A0,CE0)|Xs]
        ; Ys = [X|Zs],
          insert_clause(Xs,Zs,ClauseNo,Det,IxType,Args,ClauseEnv)
        ).

% Determinacy is split into three possible values:
% - det (no chpt pushed)
% - shallow (det outside, pushed chpt due to indexing)
% - nondet (nondet outside, so nondet inside)

mod_det(det,nondet,shallow) :- !.
mod_det(det,shallow,shallow) :- !.
mod_det(det,det,det) :- !.
mod_det(shallow,nondet,nondet) :- !.
mod_det(shallow,_,shallow) :- !.
mod_det(nondet,_,nondet).

% Note that this operation assumes quite a lot on the part of
% the involved types (e.g. that it is ultimately the same callpat
% that is involved in two guises)

fast_lub(T1,T2,T) :- !, lub_types(T1,T2,T).

%

fast_lub(T1,T2,T) :-
        ( simple_type(T1,T1_id,T1_attr) ->
          ( simple_type(T2,T2_id,T2_attr) ->
            lub_type_ids(T1_id,T2_id,T_id),
            fast_lub_attr(T1_attr,T2_attr,T_id,T_attr),
            simple_type(T,T_id,T_attr)
          ; list_type(T2,Hd2_id,Hd2_attr,Tl2_id,Tl2_attr) ->
	    T = T2
          )
        ; list_type(T1,_Hd1_id,_Hd1_attr,_Tl1_id,_Tl1_attr) ->
          ( simple_type(T2,T2_id,T2_attr) ->
	    T = T1
          ; list_type(T2,Hd2_id,Hd2_attr,Tl2_id,Tl2_attr) ->
	    T = T1
          )
        ).

% Here, we know that attributes 'really are the same' (they have been
% constructed from the same type) so the full lubliness need not be
% invoked.

fast_lub_attr(none,X,_,X).
fast_lub_attr(a(P,S,L),_,_,a(P,S,L)).
fast_lub_attr(b(C,P,L),X,T_id,Y) :-
        ( X = a(_,_,_) -> Y = X
        ; ( T_id = any ; T_id = nv ) ->
	  Y = a(P,_,L)
	; Y = b(C,P,L)
        ).

%%%%%%%%%%

construct_cp_ce_pairs(Entries,CpCEs) :-
	construct_numbered_cp_ce_pairs(Entries,NumCpCEs),
	sort(NumCpCEs,SortedCpCEs),
	remove_numbering(SortedCpCEs,CpCEs).

%

construct_numbered_cp_ce_pairs([],[]).

construct_numbered_cp_ce_pairs([e(X,D,Ix,Args,CE)|Xs],[(X,CP,CE)|Ys]) :-
        CP =.. [D,Ix|Args],
        construct_numbered_cp_ce_pairs(Xs,Ys).

%

remove_numbering([],[]).
remove_numbering([(_,Cp,CE)|Xs],[(Cp,CE)|Ys]) :-
	remove_numbering(Xs,Ys).

%

construct_unmodified_cp_ce_pairs([],_CP,[]).

construct_unmodified_cp_ce_pairs([CE|Xs],CP,[(CP,CE)|Ys]) :-
        construct_unmodified_cp_ce_pairs(Xs,CP,Ys).

% Adds uncalled clauses as (bot,CE) after abstract indexing
% NB.
% - order is irrelevant, since the pairs are sorted afterwards
% - we count from N to 1, check if it is already there. If not,
%   add a (K,bot,CE) tuple.

add_uncalled_clauses(NumCpsCsEs,Cs,ModCpsCsEs) :-
	collect_numbers(NumCpsCsEs,Nums,[]),
	functor(Cs,cls,N),
	add_uncalled_cs(N,Nums,Cs,NumCpsCsEs,ModCpsCsEs).

collect_numbers([]) --> [].
collect_numbers([(N,_)|Ns]) --> [N],collect_numbers(Ns).

add_uncalled_cs(0,_Nums,_Cs,CpsCsEs,CpsCsEs1) :- !, CpsCsEs1 = CpsCsEs.
add_uncalled_cs(N,Nums,Cs,CpsCsEs,ModCpsCsEs) :-
	( member(N,Nums) ->
	  ModCpsCsEs = RestCpsCsEs
	; arg(N,Cs,CE),
	  ModCpsCsEs = [(N,bot,CE)|RestCpsCsEs]
	),
	M is N-1,
	add_uncalled_cs(M,Nums,Cs,CpsCsEs,RestCpsCsEs).

