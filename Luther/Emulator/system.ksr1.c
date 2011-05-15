/*
 * system.ksr1.c
 *
 * Johan Bevemyr.....Tue Aug  9 1994
 * Patric Hedlin.....Mon Jan 23 1995
 *
 *
 * Implementation:
 *
 *   KSR-1 specific definitions supporting parallel execution.
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"

#include "unix.h"
#include "system.ksr1.h"


#ifdef PARALLEL
#if 0
TAGGED swap_il(pos,val)
     TAGGED *pos;
     TAGGED val;
{
  register TAGGED orig_val;

  LockAddress(pos);
  orig_val = *pos;
  *pos = val;
  FreeAddress(pos);

  return orig_val;
}
#endif 
#endif /* PARALLEL */

#if defined(SHARED_MEMORY)

extern char mmap_filename[];

#if defined(HAVE_MMAP) && defined(USE_MMAP)

#include <fcntl.h>

#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/mman.h>

char *mmap_global(globalsize)
     long globalsize;           /* Size of shared memory area     */
{
  int fd;			/* File descriptor of shared file */
  char *globalstart;		/* Start of shared memory area    */
  

  /* Create a new file of size globalsize. Fill in the mmap_filename
   * string with the newly created file.
   */
  get_mmap_filename(mmap_filename, globalsize);

  /* Open the newly created file for reading and writing.
   */
  if ((fd = open(mmap_filename, O_RDWR | O_CREAT, 0666)) == -1)
    {
      perror("can't open paging file");
      luther_exit(1);
    }

  /* We want the shared area to start at 0x2000000. This may
   * be left to the mmap function to deside.
   */
  globalstart = (char *) 0x020000000L;

  /* Check that the tag can be stored at this address.
   */
  if ((((long) globalstart) + globalsize) & TAGMASK )
    {
      FatalError("To much memory allocated, tag is nibbled\n");
    }

  /* Map the file into memory, creating a shared memory area
   */
  if (mmap((caddr_t) globalstart, globalsize,
	   PROT_READ | PROT_WRITE,
	   MAP_SHARED | MAP_FIXED | MAP_FILE, 
	   fd, 0) == (caddr_t) -1)
    {
      perror("can't map shared memory");
      luther_exit(1);
    }

  close(fd);

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

  globalsize +=  (globalsize % getpagesize() == 0 ? 0 :
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
  if (shmctl(shmid, IPC_RMID, 0) == -1)
    {
      perror("can't remove shared memory");
    }
}

#endif /* defined(HAVE_SHMGET) && defined(USE_SHMGET) */
#endif /* defined(SHARED_MEMORY) */

