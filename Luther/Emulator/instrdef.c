/*
 * instrdef.c
 *
 * Johan Bevemyr.....Mon Feb 17 1992
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"

#ifdef Inst_Def
#undef Inst_Def
#endif

#define Inst_Def(X,Y,Z) Z,

char *instruction_table[END_OF_PRED+1] = {
#include "instructions.h"
} ;
