/*
 * expand_file_name.h
 *
 * Johan Bevemyr.....Thu Jul  4 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef EXPAND_FILE_NAME_H
#define EXPAND_FILE_NAME_H


extern BOOL expand_file_name PROTO((char *, char *));

extern BOOL luther_expand_file_name PROTO((Argproto));
extern BOOL luther_find_file PROTO((Argproto));


#endif /* EXPAND_FILE_NAME_H */
