/* sockt.c -- open socket and talk into it */
/* (C) 1991 Blair P. Houghton, All Rights Reserved, copying and */
/* distribution permitted with copyright intact.		*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include "debuglistner.h"

#if 0
extern void	perror( char * );
extern int	bind( int, struct sockaddr *, int );
extern int	socket( int, int, int );
extern int	write( int, char *, unsigned );
extern char *	strcpy( char *, char *b );
extern int	strlen( char * );
extern void	exit( int );
extern int	connect( int, struct sockaddr *, int );
#endif

#define BUFSIZE 512

int readstring(in, readbuffer)
     int in;
     char *readbuffer;
{
  int c_max;

  c_max = read(in,readbuffer,BUFSIZE);

  if(c_max <= 0)
    {
      /* end of file has been reached, exit */
      exit(0);
    }

  return c_max;
}

main(argc,argv)
     int argc; 
     char *argv[];
{

    int plug;				/* socket to "plug" into the socket */
    struct sockaddr_un socketname;	/* mode and path data for the socket */
    int i;
    char inBuf[BUFSIZE];
    int bufsize;

    if(argc < 2) 
      {
	printf("usage: %s <pathname>\n",argv[0]);
	exit(0);
      }
      

    /* make a local-unix, stream-i/o, protocol-whatever plug */
    if ( (plug = socket( AF_UNIX, SOCK_STREAM, 0 )) < 0 )
	perror(argv[0]);


    /* plug it into the listening socket */
    socketname.sun_family = AF_UNIX;
    strcpy( socketname.sun_path, argv[1] );

    if ( connect( plug, (struct sockaddr *) &socketname,
		  sizeof socketname ) < 0 )
      {
	perror(argv[0]);
	exit(__LINE__);
      }

    while(1)
      {
	bufsize = readstring(1,inBuf);

	/* say something into it; something historic */
	write( plug, inBuf, bufsize);

	bufsize = readstring(plug, inBuf);
	printf("%s",inBuf);
	fflush(stdout);
      }
}
