/*
 * system.sequent.c
 *
 * Johan Bevemyr.....Mon Oct  5 1992
 * Patric Hedlin.....Mon Jan 23 1995
 *
 *
 * Implementation:
 *
 *   Sequent Symmetry specific definitions supporting parallel execution.
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"

#include "unix.h"
#include "system.sequent.h"


#if defined(SHARED_MEMORY)

extern char mmap_filename[];


#if defined(HAVE_MMAP) && defined(USE_MMAP)

#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/mman.h>

char *mmap_global(globalsize)
     long globalsize;
{
  int fd;
  char *buf;
  char *globalstart;
  
  globalsize += getpagesize() - (globalsize % getpagesize());

  get_mmap_filename(mmap_filename, globalsize);

  if ((fd = open(mmap_filename, O_RDWR | O_CREAT,0666)) == -1)
    {
      perror("can't open paging file");
      luther_exit(1);
    }

  globalstart = (char *) 0x02000000L;
    
  if ((((long) globalstart) + globalsize) & TAGMASK)
    {
      FatalError("To much memory allocated, tag is nibbled\n");
    }

  if (mmap((long) globalstart, globalsize, PROT_READ | PROT_WRITE,
	   MAP_SHARED, fd, 0)	== -1)
    {
      perror("can't map shared memory");
      luther_exit(1);
    }

  /* Finally, we remove the file to be sure that it disappear
   * even if the program terminates unnatrually
   */

  unlink(mmap_filename);

  return globalstart;
}

void remove_shared_memory()
{
  /* unlink(mmap_filename); */
}

#endif /* defined(HAVE_MMAP) && defined(USE_MMAP) */


#if defined(HAVE_SHMGET) && defined(USE_SHMGET)

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

static int shmid;

char *mmap_global(globalsize)
    long globalsize;
{
  char *globalstart = (char *) 0x02000000L;

  printf("globalsize = %d\n", globalsize);

  globalsize += (globalsize % getpagesize() == 0 ? 0 :
		 getpagesize() - globalsize % getpagesize());

  printf("globalsize = %d\n", globalsize);
    

  if ((shmid = shmget(IPC_PRIVATE, globalsize, 200 | 400 | IPC_CREAT))==-1)
    {
      perror("can't get shared memory");
      luther_exit(1);
    }

  if (shmat(shmid, globalstart, 0) == -1)
    {
      perror("can't at shared memory");
      luther_exit(1);
    }
  return globalstart;
}

void remove_shared_memory()
{
  if(shmctl(shmid, IPC_RMID, 0) == -1)
    {
      perror("can't remove shared memory");
    }
}

#endif /* defined(HAVE_SHMGET) && defined(USE_SHMGET) */


#endif /* defined(SHARED_MEMORY) */

