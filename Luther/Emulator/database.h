/*
 * database.h
 *
 * Johan Bevemyr.....Sat May 25 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef DATABASE_H
#define DATABASE_H


/**********************************************************************
 *
 * Predicate classes
 *
 */
typedef enum {
  ENTER_C,
  ENTER_EMULATED,
  ENTER_TRUE_SPY,
  ENTER_FAKE_SPY,
  ENTER_INTERPRETED,
  ENTER_UNDEFINED
} enter_inst;


#define IsEmptyDynPred(F)   ( (F) == NULL ? 0 == 0 : (F)->clause == NULL )

#define IsLastClause(F)     ( (F)->next == NULL ? 0 == 0 : \
			                         (F)->next->clause == NULL )


/**********************************************************************
 *
 * Predicate definition structure
 *
 */
typedef struct retry_node {
  code *clause;
  struct retry_node *next;
} retry_node;

typedef struct {
  retry_node *var, *last;
} in_switch;

union definfo {
  code *incoreinfo;
  BOOL (*cinfo) PROTO((Argproto));
  in_switch *indexinfo;
};

#if defined(JUMP_CALL)

typedef struct calluse {
  code *where;
  struct calluse *next;
} calluse;

#endif /* JUMP_CALL */


typedef struct definition {
  enter_inst enter_instruction;
  TAGGED name;
  TAGGED module; 
  union definfo entry_code;
#if defined(JUMP_CALL)
  struct calluse *users;
#endif /* JUMP_CALL */    
  struct definition *next;
} definition;

/*******************************************************************************
 *
 * We have experimented with local databases (local to each worker).
 * This was tried in order to measure the slowdown due to data sharing
 * in the code segment. (there was no measurable slowdown due to whis
 * on either the Sun 630 MP nor the Sequent Symmetry).
 *
 */
extern definition **predtable;


/*******************************************************************************
 *
 * Support for predicate listing.
 * 
 */
typedef enum {
  LIST_OPTION_C,
  LIST_OPTION_PL,
  LIST_OPTION_ALL,
  LIST_OPTION_NOT_VALID
} ListOption;


/**********************************************************************
 *
 * Module 
 *
 */
#define PUBLIC module_public
#define PROLOG module_prolog

/**********************************************************************
 *
 * For iterating through the database.
 *
 */
typedef struct {
  definition *def;
  int index;
  TAGGED module;
} database_iterator;

extern void init_database_iterator PROTO((database_iterator *,TAGGED));
extern BOOL next_pred_in_database PROTO((database_iterator *));

/**********************************************************************
 *
 * Functions declared in database.c 
 *
 */
extern void init_atomtable PROTO((worker *));
extern void init_database PROTO((worker *));
extern void store_emulated_predicate PROTO((TAGGED, code *, worker *));
extern void store_c_predicate PROTO((TAGGED, BOOL f PROTO((Argproto)), TAGGED, worker *));

extern void store_dynamic_predicate PROTO((TAGGED,TAGGED,code *,worker *));
extern void add_first_dynamic_predicate PROTO((TAGGED,TAGGED,code *,TAGGED,TAGGED,worker *));
extern void add_last_dynamic_predicate PROTO((TAGGED,TAGGED,code *,TAGGED,TAGGED,worker *));
extern definition *get_definition PROTO((TAGGED, worker *));
extern definition *get_definition_term PROTO((TAGGED, worker *,TAGGED *));
extern definition *get_definition_module PROTO((TAGGED,TAGGED,worker *));
extern definition *get_ex_definition PROTO((TAGGED, worker *));
extern definition *get_c_definition PROTO((TAGGED, worker *, TAGGED *));
extern definition *make_public PROTO((TAGGED,worker *));

extern void reset_dynamic_predicate PROTO((in_switch *, retry_node *));
extern void remove_dynamic_clause PROTO((in_switch *, retry_node *));
extern definition *make_undefined PROTO((TAGGED, definition *, worker *, TAGGED));

extern in_switch *make_empty_index_table PROTO((worker *));
extern retry_node *make_retry_node PROTO((code *, retry_node *, worker *));
extern retry_node *add_dynamic_first PROTO((in_switch *, code *, worker *));
extern retry_node *add_dynamic_last PROTO((in_switch *, code *, worker *));
extern definition *get_def_code PROTO((code *, worker *));

extern void plisting PROTO((worker *, ListOption));


#endif /* DATABASE_H */
