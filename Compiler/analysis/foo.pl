propagate_and_recompute(LR,_Cp,Sp,Env,Head,Rec,BodyCall,M0,M1,P0,P1,WL0,WL1,Mode) :-
	analyse_mode(Mode,parallel),
	propagate_bindings(LR,Sp,Env,Head,Rec,BodyCall,NewCp),
	functor(BodyCall,P,N),
	PN = P/N,
	lookup_cp_sp(Mode,PN,M0,P0,OldCp,_OldSp),
	( less_than(NewCp,OldCp) ->
	    M0 = M1,
	    P0 = P1, WL0 = WL1
	; true,
	    analyse_mode(Temp,sequential),
	    recompute(PN,Temp,WL0,WLx), %%% P/N is entire rp predicate!
	    recompute(PN,Mode,WLx,WL1),
	    lub_tuples(NewCp,OldCp,Cp),
	    update_cp_sp(Mode,PN,Cp,Sp,M0,M1,P0,P1),
	    functor(Head,H,A),
	    propagate_freeze_violation(Cp,H/A,Mode),
	    propagate_freeze_violation(Sp,H/A,Mode)
	).
