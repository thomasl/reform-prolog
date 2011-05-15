/*
 * char_table.c
 *
 * Patric Hedlin.....Wed Aug 10 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */

#include "char_table.h"


CharType char_table[256];


void build_char_table()
{
  register int i;
  register char *ch;

  for (i = 0;   i < 128; i++) char_table[i] = LAYOUT;

  for (i = 128; i < 256; i++) char_table[i] = LOWERCASE;		

  ch = "abcdefghijklmnopqrstuvwxyz";
  while (i = *ch++) char_table[i] = LOWERCASE;

  ch = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  while (i = *ch++) char_table[i] = UPPERCASE;
  
  ch = "0123456789";
  while (i = *ch++) char_table[i] = DIGIT;

  ch = "+-*/\\^<>=`~:.?@#$&";
  while (i = *ch++) char_table[i] = SYMBOL;

  ch = ";!";
  while (i = *ch++) char_table[i] = SOLO;

  ch = "%,()[]{|}";
  while (i = *ch++) char_table[i] = PUNCT;

  ch = "\"'";
  while (i = *ch++) char_table[i] = QUOTE;

  ch = "_";
  while (i = *ch++) char_table[i] = UNDERLINE;
}

