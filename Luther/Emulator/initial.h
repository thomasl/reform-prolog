/*
 * initial.h
 *
 * Johan Bevemyr.....Tue Jun  4 1991
 * Patric Hedlin.....Thu Feb  9 1995
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef INITIAL_H
#define INITIAL_H


extern int tracing;

extern int initialdebugflag;

extern double ticks_per_msecond;

extern TAGGED atom_nil;			/* []		*/
extern TAGGED atom_list;		/* '.'		*/
extern TAGGED atom_true;		/* true		*/
extern TAGGED atom_fail;		/* fail		*/
extern TAGGED atom_false;		/* false	*/
extern TAGGED atom_user_input;		/* user_input	*/ 
extern TAGGED atom_user_output;	        /* user_output	*/
extern TAGGED atom_user_error;		/* user_error	*/
extern TAGGED atom_user;		/* user		*/
extern TAGGED atom_inf;		        /* inf		*/
extern TAGGED atom_nan;		        /* nan		*/
extern TAGGED atom_equal;               /* '='		*/
extern TAGGED atom_less;                /* '<'		*/
extern TAGGED atom_greater;             /* '>'		*/
extern TAGGED atom_abort;               /* abort	*/
extern TAGGED default_prefix;           /* '$new'	*/

extern TAGGED functor_d_stream;         /* '$stream'/1	*/ 

extern TAGGED functor_colon;            /* ':'/2	*/
extern TAGGED functor_coma;             /* ','/2	*/
extern TAGGED functor_list;             /* '.'/2	*/

extern TAGGED module_user;              /* 'user'	*/
extern TAGGED module_prolog;            /* 'prolog'	*/
extern TAGGED module_public;            /* 'public'	*/

extern int orig_nr_of_workers;

extern void def_c_pred PROTO((int, BOOL f PROTO((Argproto)), int, TAGGED, worker *));

extern worker *initialize PROTO((int));
extern void init_predefined_atoms PROTO((worker *));
extern void initialize_builtins PROTO((worker *));
extern void init_wam PROTO((worker *));
extern void reinitialize PROTO((worker *));

extern u32 new_gen_size;


#endif /* INITIAL_H */
