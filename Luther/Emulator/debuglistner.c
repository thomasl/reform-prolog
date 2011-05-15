/* sockl.c -- set up a UNIX STREAM socket and listen on it */
/* (C) 1991 Blair P. Houghton, All Rights Reserved, copying and */
/* distribution permitted with copyright intact.		*/

/* a stream (a socket opened using SOCK_STREAM) requires the use of
listen() and accept() in a receiver, and connect() in a sender */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <fcntl.h>
#include <sys/time.h>
/* include sockl.h only after <sys/socket.h> */
#include "debuglistner.h" 

#define BUFSIZE 512

int readstring(readbuffer)
     char *readbuffer;
{
  int c_max;

  c_max = read(1,readbuffer,BUFSIZE);

  if(c_max <= 0)
    {
      /* end of file has been reached, exit */
      exit(0);
    }

  return c_max;
}

main(argc,argv)
int argc; char *argv[];
{
  int plug;
  struct sockaddr_un sockaddr;	/* sytem's location of the socket */
  char buf[BUFSIZ];
  int read_ret;
  int f_ret;
  fd_set fdset;

  if(argc < 2) 
    {
      printf("usage: %s <socketname> \n", argv[0]);
      exit(0);
    }

  /* make a local-unix, stream-i/o, protocol-whatever plug */

  if ((plug = socket( AF_UNIX, SOCK_STREAM, 0 )) < 0)
    {
      perror("debuglistner");
      exit(1);
    }

  /* place a filename in the filesystem for other processes to find */
  sockaddr.sun_family = AF_UNIX;
  (void) strcpy(sockaddr.sun_path, argv[1]);

  if (connect(plug, (struct sockaddr *) &sockaddr,
	      sizeof sockaddr ) < 0 )
    {
      perror("debuglistner");
      exit(1);
    }

  printf("WAM debugger started\n");

  while(1)
    {
      FD_ZERO(&fdset);
      FD_SET(plug,&fdset);
      FD_SET(1,&fdset);

      select(10, &fdset, NULL, NULL, NULL);

      if( FD_ISSET(plug, &fdset) )
	{
	  read_ret = read( plug, buf, sizeof buf );
	  if(read_ret > 0)
	    {
	      buf[read_ret] = (char) 0;
	      printf("%s",buf);
	      fflush(stdout);
	    }
	  else
	    {
	      break;
	    }
	}

      if( FD_ISSET(1, &fdset) )
	{
	  char inBuf[BUFSIZE];
	  int bufsize;
	    
	  bufsize = readstring(inBuf);
	  inBuf[bufsize] = (char) 0;
	    
	  write(plug, inBuf, bufsize);
	}
    }

  /*
   *  lots of things will close automatically: 
   *  but, this one must be done cleanly:
   */

  /* rm filename */
  unlink(argv[1]);

  /* loop ended normally:  read() returned NULL */
  exit(0);
}

