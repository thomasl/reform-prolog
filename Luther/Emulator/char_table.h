/*
 * char_table.h
 *
 * Patric Hedlin.....Wed Aug 10 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */

#ifndef CHAR_TABLE_H
#define CHAR_TABLE_H


#include "c.h"


#define IsWhiteChar(C)  (GetCharCode(C) == LAYOUT)
#define IsLowerChar(C)  (GetCharCode(C) == LOWERCASE)
#define IsUpperChar(C)  (GetCharCode(C) == UPPERCASE)
#define IsDigitChar(C)  (GetCharCode(C) == DIGIT)
#define IsSymbolChar(C) (GetCharCode(C) == SYMBOL)
#define IsSoloChar(C)	(GetCharCode(C) == SOLO)
#define IsPunctChar(C)  (GetCharCode(C) == PUNCT)
#define IsQuoteChar(C)  (GetCharCode(C) == QUOTE)
#define IsUnderChar(C)  (GetCharCode(C) == UNDERLINE)

#define GetCharCode(C) ((C) == EOF ? LAYOUT : char_table[(int)(C)])


typedef enum {
  LAYOUT	= 00, LOWERCASE	= 10, UPPERCASE	= 20, DIGIT	= 30,
  SYMBOL	= 40, SOLO	= 50, PUNCT	= 60, QUOTE	= 70,
  UNDERLINE	= 80
} CharType;


PROTOTYPE(void, build_char_table, (void));


extern CharType char_table[];


#endif /* CHAR_TABLE_H */
