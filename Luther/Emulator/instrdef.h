/*
 * instrdef.h
 *
 * Johan Bevemyr....Thu May 23 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef INSTRDEF_H
#define INSTRDEF_H


#define GetOpString(OP) instruction_table[OP]

#ifdef Inst_Def
#undef Inst_Def
#endif

#define Inst_Def(X,Y,Z) X,

typedef enum {
#include "instructions.h"    
LAST_DUMMY
} opcode;

typedef enum {
    A_GET_X_VARIABLE,
    A_GET_X_VALUE,
    A_GET_CONSTANT,

    /* The new set */
    A_READ_LIST_TOP,
    A_READ_STRUCT_TOP,
    A_READ_LIST,
    A_READ_STRUCT,
    A_READ_LIST_TAIL,
    A_READ_STRUCT_TAIL,

    A_UNIFY_CONSTANT_UP,
    A_UNIFY_X_VARIABLE_UP,
    A_UNIFY_X_VALUE_UP,
    A_UNIFY_X_LOCAL_VALUE_UP,
    A_UNIFY_VOID_UP,  

    /* The old set */
    A_UNIFY_VOID,
    A_UNIFY_X_VARIABLE,
    A_UNIFY_X_VALUE,
    A_UNIFY_X_LOCAL_VALUE,
    A_UNIFY_CONSTANT,

#if defined(PARALLEL)
    A_UNLOCK,
#endif    

    A_DONE
} assertcode;
    
extern char *instruction_table[END_OF_PRED+1];


#endif /* INSTRDEF_H */
