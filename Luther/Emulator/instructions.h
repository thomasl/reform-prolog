/*
 * instructions.h
 *
 * Johan Bevemyr.....Fri Mar 26 1993
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

Inst_Def(DUMMY_INSTRUCTION,dummy_instruction,"dummy_instruction")

Inst_Def(SWITCH_ON_TERM,switch_on_term,"switch_on_term")
Inst_Def(SWITCH_ON_CONSTANT,switch_on_constant,"switch_on_constant")
Inst_Def(SWITCH_ON_STRUCTURE,switch_on_structure,"switch_on_structure")

Inst_Def(TRY,try,"try")
Inst_Def(RETRY,retry,"retry")
Inst_Def(TRUST,trust,"trust")
Inst_Def(TRY_ME_ELSE,try_me_else,"try_me_else")
Inst_Def(RETRY_ME_ELSE,retry_me_else,"retry_me_else")
Inst_Def(TRUST_ME,trust_me,"trust_me")

Inst_Def(CHOICE_X,choice_x,"choice_x")
Inst_Def(CHOICE_Y,choice_y,"choice_y")
Inst_Def(CUT,cut,"cut")
Inst_Def(CUT_X,cut_x,"cut_x")
Inst_Def(CUT_Y,cut_y,"cut_y")
Inst_Def(INLINE,in_line,"in_line")
Inst_Def(BUILTIN,builtin,"builtin")
Inst_Def(META_CALL,meta_call,"meta_call")
Inst_Def(META_EXECUTE,meta_execute,"meta_execute")

Inst_Def(REQUIRE,require,"require")
Inst_Def(REQUIRE_USING,require_using,"require_using")
Inst_Def(ALLOCATE,allocate,"allocate")
Inst_Def(ALLOCATE2,allocate2,"allocate2")
Inst_Def(DEALLOCATE,deallocate,"deallocate")
Inst_Def(INIT,init,"init")
Inst_Def(CALL,call,"call")
Inst_Def(EXECUTE,execute,"execute")
Inst_Def(PROCEED,proceed,"proceed")
Inst_Def(FAIL,fail,"fail")

Inst_Def(GET_X_VARIABLE,get_x_variable,"get_x_variable")
Inst_Def(GET_Y_VARIABLE,get_y_variable,"get_y_variable")
Inst_Def(GET_Y_FIRST_VALUE,get_y_first_value,"get_y_first_value")
Inst_Def(GET_X_VALUE,get_x_value,"get_x_value")
Inst_Def(GET_Y_VALUE,get_y_value,"get_y_value")
Inst_Def(GET_CONSTANT,get_constant,"get_constant")
Inst_Def(GET_NIL,get_nil,"get_nil")
Inst_Def(GET_STRUCTURE,get_structure,"get_structure")
Inst_Def(GET_LIST,get_list,"get_list")
Inst_Def(GET_CONSTANT_X0,get_constant_x0,"get_constant_x0")
Inst_Def(GET_NIL_X0,get_nil_x0,"get_nil_x0")
Inst_Def(GET_STRUCTURE_X0,get_structure_x0,"get_structure_x0")
Inst_Def(GET_LIST_X0,get_list_x0,"get_list_x0")

Inst_Def(PUT_X_VOID,put_x_void,"put_x_void")
Inst_Def(PUT_Y_VOID,put_y_void,"put_y_void")
Inst_Def(PUT_X_VARIABLE,put_x_variable,"put_x_variable")
Inst_Def(PUT_Y_VARIABLE,put_y_variable,"put_y_variable")
Inst_Def(PUT_X_VALUE,put_x_value,"put_x_value")
Inst_Def(PUT_Y_VALUE,put_y_value,"put_y_value")
Inst_Def(PUT_X_UNSAFE_VALUE,put_x_unsafe_value,"put_x_unsafe_value")
Inst_Def(PUT_Y_UNSAFE_VALUE,put_y_unsafe_value,"put_y_unsafe_value")
Inst_Def(PUT_CONSTANT,put_constant,"put_constant")
Inst_Def(PUT_NIL,put_nil,"put_nil")
Inst_Def(PUT_STRUCTURE,put_structure,"put_structure")
Inst_Def(PUT_LIST,put_list,"put_list")

Inst_Def(UNIFY_VOID,unify_void,"unify_void")
Inst_Def(UNIFY_X_VARIABLE,unify_x_variable,"unify_x_variable")
Inst_Def(UNIFY_XVAR_XVAR,unify_xvar_xvar,"unify_xvar_xvar")
Inst_Def(UNIFY_Y_VARIABLE,unify_y_variable,"unify_y_variable")
Inst_Def(UNIFY_Y_FIRST_VALUE,unify_y_first_value,"unify_y_first_value")
Inst_Def(UNIFY_X_VALUE,unify_x_value,"unify_x_value")
Inst_Def(UNIFY_Y_VALUE,unify_y_value,"unify_y_value")
Inst_Def(UNIFY_X_LOCAL_VALUE,unify_x_local_value,"unify_x_local_value")
Inst_Def(UNIFY_Y_LOCAL_VALUE,unify_y_local_value,"unify_y_local_value")
Inst_Def(UNIFY_CONSTANT,unify_constant,"unify_constant")
Inst_Def(UNIFY_NIL,unify_nil,"unify_nil")
Inst_Def(UNIFY_STRUCTURE,unify_structure,"unify_structure")
Inst_Def(UNIFY_LIST,unify_list,"unify_list")

#if defined(JUMP_CALL)
Inst_Def(CJUMP,cjump,"cjump")
Inst_Def(EJUMP,ejump,"ejump")    
#endif /* JUMP_CALL */


#if defined(BOUNDED_Q)

Inst_Def(ZEROP,zerop,"zerop")
Inst_Def(LISTP,listp,"listp")
Inst_Def(DETERMINISTIC,deterministic,"deterministic")

Inst_Def(ALLOCATE_STAR,allocate_star,"allocate_star")
Inst_Def(REPEAT,repeat,"repeat")
Inst_Def(ITERATE_INT,iterate_int,"iterate_int")
Inst_Def(ITERATE_INT_STAR,iterate_int_star,"iterate_int_star")
Inst_Def(ITERATE_LIST,iterate_list,"iterate_list")
Inst_Def(ITERATE_LIST_STAR,iterate_list_star,"iterate_list_star")
Inst_Def(ENSURE_LIST_TRY,ensure_list_try,"ensure_list_try")
Inst_Def(ENSURE_LIST_TRUST,ensure_list_trust,"ensure_list_trust")

#endif /* BOUNDED_Q */


#if defined(PARALLEL_BQ)
Inst_Def(SPAWN_LEFT_BQ,spawn_left_bq,"spawn_left_bq")
#endif /* PARALLEL_BQ */

#if defined(JUMP_CODE)
Inst_Def(JUMP,jump,"jump")
#endif 

#if defined(REFORM)

Inst_Def(GLOBAL_GET_X_VALUE,global_get_x_value,"global_get_x_value")
Inst_Def(GLOBAL_GET_Y_VALUE,global_get_y_value,"global_get_y_value")
Inst_Def(GLOBAL_GET_CONSTANT,global_get_constant,"global_get_constant")
Inst_Def(GLOBAL_GET_NIL,global_get_nil,"global_get_nil")
Inst_Def(GLOBAL_GET_STRUCTURE,global_get_structure,"global_get_structure")
Inst_Def(GLOBAL_GET_LIST,global_get_list,"global_get_list")
Inst_Def(GLOBAL_GET_CONSTANT_X0,global_get_constant_x0,
	 "global_get_constant_x0")
Inst_Def(GLOBAL_GET_NIL_X0,global_get_nil_x0,"global_get_nil_x0")
Inst_Def(GLOBAL_GET_STRUCTURE_X0,global_get_structure_x0,
	 "global_get_structure_x0")
Inst_Def(GLOBAL_GET_LIST_X0,global_get_list_x0,"global_get_list_x0")

Inst_Def(GLOBAL_UNIFY_X_VALUE,global_unify_x_value,"global_unify_x_value")
Inst_Def(GLOBAL_UNIFY_Y_VALUE,global_unify_y_value,"global_unify_y_value")
Inst_Def(GLOBAL_UNIFY_X_LOCAL_VALUE,global_unify_x_local_value,
	 "global_unify_x_local_value")
Inst_Def(GLOBAL_UNIFY_Y_LOCAL_VALUE,global_unify_y_local_value,
	 "global_unify_y_local_value")
Inst_Def(GLOBAL_UNIFY_CONSTANT,global_unify_constant,"global_unify_constant")
Inst_Def(GLOBAL_UNIFY_NIL,global_unify_nil,"global_unify_nil")
Inst_Def(GLOBAL_UNIFY_STRUCTURE,global_unify_structure,
	 "global_unify_structure")
Inst_Def(GLOBAL_UNIFY_LIST,global_unify_list,"global_unify_list")

Inst_Def(BUILD_REC_POSLIST,build_rec_poslist,"build_rec_poslist")
Inst_Def(BUILD_POSLIST,build_poslist,"build_poslist")
Inst_Def(BUILD_POSLIST_VALUE,build_poslist_value,"build_poslist_value")
Inst_Def(BUILD_NEGLIST,build_neglist,"build_neglist")
Inst_Def(BUILD_NEGLIST_VALUE,build_neglist_value,"build_neglist_value")
Inst_Def(BUILD_VARIABLES,build_variables,"build_variables")

Inst_Def(PUT_NTH_HEAD,put_nth_head,"put_nth_head")
Inst_Def(PUT_NTH_TAIL,put_nth_tail,"put_nth_tail")
Inst_Def(PUT_GLOBAL_ARG,put_global_arg,"put_global_arg")
Inst_Def(NIL_NTH_HEAD,nil_nth_head,"nil_nth_head")

Inst_Def(UNIFY_NTH_HEAD,unify_nth_head,"unify_nth_head")
Inst_Def(UNIFY_NTH_TAIL,unify_nth_tail,"unify_nth_tail")
Inst_Def(UNIFY_GLOBAL_ARG,unify_global_arg,"unify_global_arg")

Inst_Def(START_RIGHT_BODY,start_right_body,"start_right_body")
Inst_Def(START_LEFT_BODY,start_left_body,"start_left_body")

Inst_Def(INITIALIZE_RIGHT,initialize_right,"initialize_right")
Inst_Def(INITIALIZE_LEFT,initialize_left,"initialize_left")

Inst_Def(SPAWN_RIGHT,spawn_right,"spawn_right")
Inst_Def(SPAWN_LEFT,spawn_left,"spawn_left")

Inst_Def(AWAIT_LEFTMOST,await_leftmost,"await_leftmost")
Inst_Def(AWAIT_NONVAR,await_nonvar,"await_nonvar")
Inst_Def(AWAIT_STRICTLY_NONVAR,await_strictly_nonvar,"await_strictly_nonvar")
Inst_Def(AWAIT_VARIABLE,await_variable,"await_variable")
Inst_Def(AWAIT_NONVAR_UNIFY,await_nonvar_unify,"await_nonvar_unify")
Inst_Def(AWAIT_STRICTLY_NONVAR_UNIFY,await_strictly_nonvar_unify,"await_strictly_nonvar_unify")
Inst_Def(AWAIT_VARIABLE_UNIFY,await_variable_unify,"await_variable_unify")

Inst_Def(PAR_BUILTIN,par_builtin,"par_builtin")
Inst_Def(PAR_INLINE,par_inline,"par_inline")

Inst_Def(LOCK_AND_GET_LIST,lock_and_get_list,"lock_and_get_list")
Inst_Def(LOCK_AND_GET_STRUCTURE,lock_and_get_structure,"lock_and_get_structure")
Inst_Def(UNLOCK,unlock,"unlock")

#endif /* REFORM */

/* Extras */

Inst_Def(HALT,halt,"halt")
Inst_Def(NOOP,noop,"noop")
Inst_Def(END_OF_PRED,end_of_pred,"end_of_pred")

