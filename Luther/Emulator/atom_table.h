/*
 * atom_table.h
 *
 * Johan Bevemyr.....Thu Aug  8 1991
 * Patric Hedlin.....Thu Feb  9 1994
 *
 *
 * Description:
 *
 *   These constants are used for indexing into the atom_table_tagged[]
 * table which stores the ids for all built-in atoms. Be careful to add
 * a constant at the same relative location in this table as in the
 * table in atom_table.c.
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef ATOM_TABLE_H
#define ATOM_TABLE_H


/* For some reason we don't want this table to start at zero,
 * I cannot remember why at this point.
 */
typedef enum {
    ATOM_NIL = 1,     
    ATOM_LIST,

    ATOM_TRUE,
    ATOM_FAIL,
    ATOM_FALSE,

    ATOM_USER_INPUT,
    ATOM_USER_OUTPUT,
    ATOM_USER_ERROR,

    ATOM_READ,
    ATOM_WRITE,
    ATOM_APPEND,

    ATOM_USER,
    ATOM_PROLOG,
    ATOM_PUBLIC,

    ATOM_INF,
    ATOM_NAN,

    ATOM_EQUAL,
    ATOM_LESS,
    ATOM_GREATER,

    ATOM_PLUS,
    ATOM_MINUS,
    ATOM_TIMES,
    ATOM_DIV,
    ATOM_INTDIV,
    ATOM_MOD,
    ATOM_MIN,
    ATOM_MAX,
    ATOM_ABS,
    ATOM_LOG,
    ATOM_EXP,
    ATOM_SQRT,
    ATOM_CBRT,

    ATOM_SIN,
    ATOM_COS,
    ATOM_TAN,
    ATOM_ASIN,
    ATOM_ACOS,
    ATOM_ATAN,

    ATOM_INTEGER,
    ATOM_FLOAT,
    ATOM_FLOOR,
    ATOM_CEILING,
    ATOM_TRUNCATE,
    ATOM_ROUND,

    ATOM_B_OR,
    ATOM_B_AND,
    ATOM_B_NOT,
    ATOM_B_XOR,
    ATOM_LSHIFT,
    ATOM_RSHIFT,
    ATOM_MSB,
    ATOM_RNG,

    ATOM_AREF,

    ATOM_IS,

    ATOM_ARRAY,
    ATOM_ARRAY_ARG,
    ATOM_ARRAY_SETARG,
    ATOM_ARRAY_SIZEOF,
    ATOM_ARRAY_REDUCE,
    ATOM_D_ARRAY,	/* Functor used for array representation. */

    ATOM_NUMBER_CHARS,
    ATOM_ATOM_CHARS,
    ATOM_NAME,

    ATOM_UNIQUE_NAME,
    ATOM_PREFIX,	/* Default prefix in unique name. */

    ATOM_SEED,
    ATOM_RANDOM,

    ATOM_HALT,
    ATOM_VERSION,

    ATOM_D_FREEZE,
    ATOM_D_FROZEN,
    ATOM_DEFROST,

    ATOM_SETARG,
    ATOM_ZAPARG,

    ATOM_D_ATOM_MODE,
    ATOM_D_LOADING_MODE,
    ATOM_CONSULT,	/* Atom representation of default load-mode. */

    ATOM_D_MODULE,
    ATOM_D_GET_MODULE,
    ATOM_D_SET_MODULE,
    ATOM_D_PUBLIC,
    ATOM_D_DYNAMIC,

    ATOM_D_ASSERT_DELETE_OTHER,
    ATOM_D_ASSERTA,
    ATOM_D_ASSERTZ,
    ATOM_D_ERASE,
    ATOM_D_REF,		/* Functor representation used by dynamics. */

    ATOM_D_MGU_VARIABLES,

    ATOM_D_CLAUSE,
    ATOM_D_PREDICATE_PROPERTY,
    ATOM_BUILT_IN,
    ATOM_COMPILED,
    ATOM_INTERPRETED,
    ATOM_D_CURRENT_PREDICATE,
    ATOM_CURRENT_PREDICATE,

    ATOM_D_TOPCHOICE,
    ATOM_D_SAVE_CHOICE,
    ATOM_D_LOAD_CHOICE,

    ATOM_D_RETRY_CHOICE,
    ATOM_D_RETRY_CUT,

    ATOM_D_SET_SPY,
    ATOM_D_IMPOSE_SPY,
    ATOM_D_REMOVE_SPY,

    ATOM_D_GET_TRACE_LEVEL,
    ATOM_D_SET_INC_TRACE_LEVEL,
    ATOM_D_SET_DEC_TRACE_LEVEL,

    ATOM_D_TRACE,
    ATOM_D_TRACE_ON,
    ATOM_D_TRACE_SPY,
    ATOM_D_TRACE_OFF,

    ATOM_D_SET_HANDLER,
    ATOM_D_FIND_HANDLER,

    ATOM_GARBAGE_COLLECT,

    ATOM_STRUCT_REDUCE,

    ATOM_COPY_TERM,

    ATOM_WORKER_REST,
    ATOM_ACTIVE_WORKERS,
    ATOM_SCHEDULE,
    ATOM_STATIC,	/* Atom representation of possible */
    ATOM_DYNAMIC,	/* scheduling policies.            */

    ATOM_WAMDEBUG,
    ATOM_WAMDEBUG_FILE,
    ATOM_WAMDEBUG_SOCKET,
    ATOM_WAMDEBUG_PROLOG,
    ATOM_WAMNODEBUG,

    ATOM_PROLOG_FLAG_GC_VERBOSE,
    ATOM_PROLOG_FLAG_LOAD_VERBOSE,
    ATOM_ON,		/* Atom representation of possible */
    ATOM_OFF,		/* prolog flag modes (status).     */

    ATOM_PROCESSOR_IDS,
    ATOM_PROCESSOR_SORT,

    ATOM_D_LOAD,
    ATOM_D_QLOAD,
    ATOM_DIRECTIVE,	/* Atom representation of 'directive'. */
    ATOM_END_OF_FILE,	/* Atom representation of end_of_file. */

    ATOM_STATISTICS,

    ATOM_D_STATISTICS_RUNTIME,
    ATOM_D_STATISTICS_GCTIME,
    ATOM_D_STATISTICS_GCNR,
    ATOM_D_STATISTICS_GCBYTES,
    ATOM_D_STATISTICS_WALLTIME,
    ATOM_D_STATISTICS_PARALLEL_RUNTIME,
    ATOM_D_STATISTICS_MEMORY,
    ATOM_D_STATISTICS_GLOBAL,
    ATOM_D_STATISTICS_LOCAL,
    ATOM_D_STATISTICS_TRAIL,
    ATOM_D_STATISTICS_DEREF,
    ATOM_D_STATISTICS_RESTORE,
    ATOM_D_STATISTICS_INSTR,
    ATOM_D_STATISTICS_INSTR_PROF,
    ATOM_D_STATISTICS_TIDY,
    ATOM_D_STATISTICS_SWAP,
    ATOM_D_STATISTICS_CODE,
    ATOM_D_STATISTICS_ATOM,

    ATOM_PROMPT,

    ATOM_TTYNL,
    ATOM_TTYTAB,
    ATOM_TTYPUT,
    ATOM_TTYGET,
    ATOM_TTYGET0,
    ATOM_TTYSKIP,
    ATOM_TTYFLUSH,

    ATOM_D_TTYGETCH0,

    ATOM_NL,
    ATOM_TAB,
    ATOM_PUT,
    ATOM_GET,
    ATOM_GET0,
    ATOM_SKIP,
    ATOM_FLUSH,

    ATOM_D_GETCH,
    ATOM_D_GETCH0,

    ATOM_D_DISPLAY,
    ATOM_D_WRITE,
    ATOM_D_WRITE_FLOAT,
    ATOM_D_WRITE_RADIX,
    ATOM_D_WRITE_INTEGER,

    ATOM_GET_INPUT,
    ATOM_SET_INPUT,

    ATOM_GET_OUTPUT,
    ATOM_SET_OUTPUT,

    ATOM_OPEN,
    ATOM_CLOSE,
    ATOM_REWIND,

    ATOM_D_STREAM,
    ATOM_D_STREAM_LIST,
    ATOM_D_STREAM_NAME,
    ATOM_D_STREAM_MODE,
    ATOM_D_STREAM_FILE,
    ATOM_D_STREAM_CODE,

    ATOM_D_FILE_CODE,
    ATOM_D_FDOPEN,

    ATOM_D_UNIX_ACCESS,
    ATOM_R_OK,
    ATOM_W_OK,
    ATOM_X_OK,
    ATOM_F_OK,
    ATOM_D_UNIX_STAT_TYPE,
    ATOM_S_IFDIR,
    ATOM_S_IFREG,
    ATOM_S_IFDEF,
    ATOM_D_UNIX_STAT_TIME,
    ATOM_D_UNIX_CD,
    ATOM_D_UNIX_POPEN,

    ATOM_EXPAND_FILE_NAME,

    ATOM_START,
    ATOM_ABORT,

    ATOM_COLON,
    ATOM_COMA,

    ATOM_WAKE,

    ATOM_D_INTERPRET_GOAL,
    ATOM_D_INTERPRET_GOAL_SPY,
    ATOM_D_INTERPRET_GOAL_TRACE,

    ATOM_PMON,
    ATOM_PMON_PRINT,

    ATOM_TABLE_SIZE
} atoms;

#define Print1(A1)               fprintf(stderr,A1)
#define Print2(A1,A2)            fprintf(stderr,A1,A2)
#define Print3(A1,A2,A3)         fprintf(stderr,A1,A2,A3)
#define Print4(A1,A2,A3,A4)      fprintf(stderr,A1,A2,A3,A4)

#define ErrPrint1(A1)            fprintf(stderr,A1)
#define ErrPrint2(A1,A2)         fprintf(stderr,A1,A2)
#define ErrPrint3(A1,A2,A3)      fprintf(stderr,A1,A2,A3)
#define ErrPrint4(A1,A2,A3,A4)   fprintf(stderr,A1,A2,A3,A4)

#define PL_Print1(S,A1)	         fprintf(S,A1)
#define PL_Print2(S,A1,A2)       fprintf(S,A1,A2)
#define PL_Print3(S,A1,A2,A3)    fprintf(S,A1,A2,A3)
#define PL_Print4(S,A1,A2,A3,A4) fprintf(S,A1,A2,A3,A4)

#define FD_Print1(FD,S)							   \
{									   \
  char FDbuf[512];							   \
  sprintf(FDbuf, S);							   \
  writestring(FD, FDbuf);						   \
}

#define FD_Print2(FD,S1,S2)						   \
{									   \
  char FDbuf[512];							   \
  sprintf(FDbuf, S1,S2);						   \
  writestring(FD, FDbuf);						   \
}

#define FD_Print3(FD,S1,S2,S3)						   \
{									   \
  char FDbuf[512];							   \
  sprintf(FDbuf, S1,S2,S3);						   \
  writestring(FD, FDbuf);						   \
}

#define FD_Print4(FD,S1,S2,S3,S4)					   \
{									   \
  char FDbuf[512];							   \
  sprintf(FDbuf, S1,S2,S3,S4);						   \
  writestring(FD, FDbuf);						   \
}


extern char *get_string PROTO((atom, worker *));
extern TAGGED get_mode PROTO((atom, worker *));
extern void init_atom_table PROTO((worker *));
extern void init_node PROTO((worker *));

extern TAGGED atom_table_tagged[ATOM_TABLE_SIZE+1];
extern TAGGED store_atom PROTO((char *, worker *));
extern TAGGED atom_exist PROTO((char *, worker *));


#endif /* ATOM_TABLE_H */
