/*
 * load.h
 *
 * Johan Bevemyr.....Sun Jun  2 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef LOAD_H
#define LOAD_H


enum {
  PARSE_EOF,
  PARSE_DIRECTIVE
};

PROTOTYPE(void, load_file, (FILE *, worker *));
PROTOTYPE(BOOL, luther_load, (Argproto));

extern int parser_return_value;


#endif /* LOAD_H */
