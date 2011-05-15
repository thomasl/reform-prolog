/*
 * qload.h
 *
 * Johan Bevemyr.....Tue Jun 11 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef QLOAD_H
#define QLOAD_H


typedef enum {
  QLOAD_END_MARKER = 0,
  QLOAD_PRED_START,
  QLOAD_WORD,
  QLOAD_N_BYTECODE,
  QLOAD_DEF,
  QLOAD_ATOM,
  QLOAD_FUNCTOR,
  QLOAD_FLOAT,
  QLOAD_INTEGER,
  QLOAD_START_TABLE,
  QLOAD_END_TABLE,
  QLOAD_PRED_END
} qload_codes;


extern char *qload_opcode_names[QLOAD_PRED_END+1];

extern BOOL quickload PROTO((FILE *,worker *));    
extern BOOL luther_qload PROTO((Argproto));


#endif /* QLOAD_H */
