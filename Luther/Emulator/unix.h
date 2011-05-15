/*
 * unix.h
 *
 * Johan Bevemyr.....Tue Oct  6 1992
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef UNIX_H
#define UNIX_H


#if defined(SHARED_MEMORY)
extern void get_mmap_filename PROTO((char *, long));
#endif

extern BOOL luther_unix_access PROTO((Argproto));
extern BOOL luther_unix_stat PROTO((Argproto));
extern BOOL luther_unix_cd PROTO((Argproto));
extern BOOL luther_unix_cd_2 PROTO((Argproto));


#endif /* UNIX_H */
