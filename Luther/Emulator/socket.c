/*
 * socket.c
 *
 * Johan Bevemyr.....Wed Nov  2 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"

#if defined(HAVE_SOCKETS)

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include "debug.h"

#endif /* HAVE_SOCKETS */


int open_socket_listner(w, name)
     worker *w;
     char *name;
{
#if defined(HAVE_SOCKETS)
  int sock;
  struct sockaddr_un socketname;	

  /*
   *  open a unix (local) socket, using stream (file-style i/o)
   *  mode, with protocol irrelevant ( == 0 ) (the protocol is
   *  generally determined by the connection style: tcp for stream,
   *  udp for datagrams, but this is not immutable nor all the
   *  protocols for these styles).
   */

  if ((sock = socket( AF_UNIX, SOCK_STREAM, 0 )) < 0 )
    {
      char s[255];
      sprintf(s, "worker %d: can't assign fd for socket", w->pid);
      perror(s);
      return -1;
    }

  socketname.sun_family = AF_UNIX;
  strcpy(socketname.sun_path, name);
	    
  if (bind(sock, (struct sockaddr *) &socketname, sizeof socketname ) < 0 )
    {
      char s[BUFSIZ];
      sprintf( s, "worker %d: can't bind socket (%d) to pathname (%s)",
	      w->pid, sock, socketname.sun_path );
      perror(s);
      return -1;
    }

    /* put an ear to the socket, listening for a knock-knock-knocking */
    listen(sock, 1);				/* 1: only one queue slot */

  return sock;
#else
  return 1;
#endif 
}

int open_socket_ear(w, sock)
     worker *w;
     int sock;
{
#if defined(HAVE_SOCKETS)
  int ear;

  /* ear will be a temporary (non-reusable) socket different from sock */

  if ((ear = accept(sock, (struct sockaddr *)NULL, (int *)NULL )) < 0 )
    {
      perror("worker");
      luther_exit(1);
    }
  
  return ear;
#else /* HAVE_SOCKETS */
  return 1;
#endif /* HAVE_SOCKETS */
}

void start_debug_socket(pid,w)
     int pid;
     worker *w;
{
#if defined(HAVE_SOCKETS)
#if defined(DEBUG)
  int plug;
  int ear;
  char socket_name[256];
  char *debuglistner, *xtermpath;
  char *listnername;


  if ((xtermpath = getenv("LUTHERXTERM")) == 0)
    {
      Error("LUTHERXTERM not defined");
      return;
    }

  if ((listnername = getenv("LUTHERLISTNER")) == 0)
    {
      Error("LUTHERLISTNER not defined");
      return;
    }

  sprintf(socket_name, "Worker%d",pid);

  plug = open_socket_listner(w, socket_name);

  if (plug == -1) 
    return;


  if (fork() == 0)
    {
      int status;

      status = execl(xtermpath, "xterm", "-T", socket_name,
		     "-e", listnername, socket_name, (char *) 0);

      if (status == -1)
	{
	  PL_Print2(stderr,"xtermpath = '%s'\n",xtermpath);
	  Error("could not start xterm");
	  luther_exit(0);
	}
    }

  ear = open_socket_ear(w, plug);

  /* unlink(socket_name); */

  w->debug.debugflag = TRUE;
  w->debug.debugmode = DBG_CREEP;
  w->debug.in = ear;
  w->debug.out = ear;
#endif /* DEBUG */
#endif /* HAVE_SOCKETS */
  return;
}

int open_socket_listen(w,name)
     worker *w;
     char *name;
{
  int plug, ear;

  plug = open_socket_listner(w, name);

  if(plug == -1)
    {
      FatalError("Cannot open icount socket listner");
    }

  ear = open_socket_ear(w, plug);

  return ear;
}

int open_socket_talk(w,name)
     worker *w;
     char *name;
{
#ifdef HAVE_SOCKETS
  int plug;
  struct sockaddr_un sockaddr;	/* sytem's location of the socket */

  /* make a local-unix, stream-i/o, protocol-whatever plug */

  if ((plug = socket( AF_UNIX, SOCK_STREAM, 0 )) < 0)
    {
      perror("debuglistner");
      luther_exit(1);
    }

  sockaddr.sun_family = AF_UNIX;

  strcpy(sockaddr.sun_path, name);

  if (connect(plug, (struct sockaddr *) &sockaddr,
	      sizeof sockaddr ) < 0 )
    {
      perror("debuglistner");
      luther_exit(1);
    }

  return plug;
#else
  return 0;
#endif /* HAVE_SOCKETS */
}
