/*
 *  File:    config.c
 *  Author:  Patric Hedlin
 *  Created: Fri Jun 17 1994
 *  Purpose: Test program used in order detect any pointer organisation
 *           which claches with the high tag scheme used by Luther.
 */ 

#include <stdio.h>

#define TAG_SIZE 4 /* The number of bits used by the tag scheme. */


main()
{
  static char file[] = "Emulator/malloc_base.h";

  unsigned long int wordsize = sizeof(unsigned long int) * 8; /* relying on bytes of 8 bits */

  char *mp = (char *)malloc(1024);

  unsigned long int word = (unsigned long int)mp >> wordsize - TAG_SIZE;

  FILE *fd;


  if ((fd = fopen(file, "w")) != NULL)
    {
      fprintf(fd, "#define MALLOC_BASE 0x%lx\n", (word << wordsize - TAG_SIZE));

      close(fd);

      exit(0);
    }
  else
    {
      exit(-1);
    }
}

  
