/*
 * inline.h
 *
 * Johan Bevemyr.....Wed Jun  5 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef INLINE_H
#define INLINE_H


#define INLINE_TABLE_SIZE 68


#define GetInlineArity(F)  inline_table[F].arity
#define GetInlineFnk(F)    inline_table[F].fnk
#define GetInlineName(F)   inline_table[F].pname
#define GetInlineType(F)   inline_table[F].type
#define GetInlineRetarg(F) inline_table[F].retarg


typedef enum { I_FUNC, I_PRED } i_type;

typedef struct {
  char *pname;
  int  arity;
  BOOL (*fnk)PROTO((InArgproto));
  i_type type;
  int retarg;
} inline_entry;

extern inline_entry inline_table[];


extern PROTOTYPE(CompareType, compare_struct, (TAGGED *, TAGGED *, int, worker *));

extern PROTOTYPE(BOOL, luther_is, (Argproto));


extern PROTOTYPE(void, initialize_inline, (worker *));


#endif /* INLINE_H */
