/*
 * socket.h
 *
 * Johan Bevemyr.....Wed Nov  2 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef SOCKET_H
#define SOCKET_H

extern int open_socket_listner PROTO((worker *, char *));
extern int open_socket_listen PROTO((worker *, char *));
extern int open_socket_talk PROTO((worker *, char *));
extern int open_socket_ear PROTO((worker *, int));
extern void start_debug_socket PROTO((int, worker *));


#endif /* SOCKET_H */
