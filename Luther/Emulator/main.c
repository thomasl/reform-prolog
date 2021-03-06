/*
 * main.c
 *
 * Johan Bevemyr.....Fri Sep 13 1991
 * Patric Hedlin.....Mon Dec 19 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "main.h"
#include "think.h"
#include "threads.h"

#if defined(PARALLEL)
static PROTOTYPE(void, create_children, (worker *w, int number_of_workers));
#endif


/*******************************************************************************
 *
 * main:
 *
 */
void main(argc, argv)
     int argc;
     char **argv;
{
  BOOL verbose = FALSE;
  BOOL listing = FALSE;

  ListOption option;

  long  boot_file_cnt = 0;
  FILE *boot_file_ptr;
  char *boot_file[MAX_BOOT_FILES];

  char path_buffer[MAXPATHLEN+1];

  worker *w;

  int number_of_workers = NUMBER_OF_WORKERS;
  int i;


#if defined(THINK_C)

  char *ArgVec[ArgCnt] = { "luther", "-b", "Luther Library" };
  char think_buffer[255];

  argc = ArgCnt;
  argv = ArgVec;

#if 1	/* Turn this of if you want to run the program inside THINK_C */
  argv[2] = get_application_name(think_buffer);
#endif    

#endif /* THINK_C */


#if ! defined(NDEBUG)
  initialdebugflag = FALSE;
#endif


#if defined(THREADED_CODE)
  (void) engine(NULL, NULL);	/* initiate global_label_table */
#endif


  /* Process command line options.
   */
  for (i = 1; i < argc; i++)
    {
      LineArgument("-b", 1)
	{
	  boot_file[boot_file_cnt++] = argv[++i];

	  continue;
	}

      LineArgument("-list", 1)
	{
	  static char *list_option[] = { "c", "pl", "all" };

	  inc(i);

	  for (option = LIST_OPTION_C; option < LIST_OPTION_NOT_VALID; option++)
	    if (strcmp(argv[i], list_option[option]) == STRCMP_EQUAL)
	      break;

	  if (option == LIST_OPTION_NOT_VALID)
	    {
	      listing = FALSE;

	      fprintf(stderr, "\t%s is not a valid -list <option>\n", argv[i]);
	    }
	  else
	    {
	      listing = TRUE;
	    }
	  continue;
	}

      LineArgument("-verbose", 0)
	{
	  verbose = TRUE;

	  continue;
	}

      LineArgument("-memory", 1)
	{
	  inc(i);
	  total_size = atol(argv[i]);

	  continue;
	}

      LineArgument("-newgen", 1)
	{
	  inc(i);
	  new_gen_size = atol(argv[i]);

	  continue;
	}

#if defined(PARALLEL)
      LineArgument("-wmemory", 1)
	{
	  inc(i);
	  worker_size = atol(argv[i]);

	  continue;
	}

      LineArgument("-n", 1)
	{
	  inc(i);
	  number_of_workers = atoi(argv[i]);

	  continue;
	}

      LineArgument("-w", 1)
	{
	  inc(i);
	  number_of_workers = atoi(argv[i]) + 1;

	  continue;
	}
#endif /* PARALLEL */

#if ! defined(NDEBUG)
      LineArgument("-debug", 0)
	{
	  initialdebugflag = TRUE;

	  continue;
	}
#endif

      LineArgument("-opcode", 0)
	{
	  int j;

	  printf("\n\ninline_opcode(Name,Arity,Op,Type,Ret) :- ");
	  printf("inline_op_code(Name,Arity,Op,Type,Ret),!.\n");
	  printf("inline_opcode(Name,_,_,_,_) :- \n\twrite(user_error,");
	  printf("['inst has no inline opcode: ',Name]),");
	  printf("\n\tnl(user_error), !, fail.\n\n");
	    
	  for (j = 0; j < INLINE_TABLE_SIZE; j++)
	    {
	      printf("inline_op_code('%s',%d,%d,%s,%d).\n", GetInlineName(j),
		     GetInlineArity(j), j,
		     (GetInlineType(j) == I_FUNC ? "function" : "predicate"),
		     GetInlineRetarg(j));
	    }
	  printf("/* Opcodes for the Luther WAM emulator.\n");
	  printf("   Generated by the C program 'opcode'. */\n\n");
	  printf("opcode(X,Y) :- op_code(X,Y),!.\n");
	  printf("opcode(X,_) :- \n\twrite(user_error,");
	  printf("['inst has no opcode: ',X]),");
	  printf("\n\tnl(user_error), !, fail.\n\n");
	    
	  for (j = 0; j <= END_OF_PRED; j++)
	    {
	      printf("op_code('%s',%d).\n", GetOpString(j), j);
	    }
	    
	  printf("\n\nqload_opcode(X,Y) :- qload_op_code(X,Y),!.\n");
	  printf("qload_opcode(X,_) :- \n\twrite(user_error,");
	  printf("['inst has no qload opcode: ',X]),");
	  printf("\n\tnl(user_error), !, fail.\n\n");
	    
	  for (j = 0; j <= QLOAD_PRED_END; j++)
	    {
	      printf("qload_op_code('%s',%d).\n", qload_opcode_names[j], j);
	    }
	    
	  exit(0);
	}

    error:
      {
	printf("usage: %s \n", argv[0]);
	printf("       -b <boot-file>\n");
	printf("       -n <processes>\n"); 
	printf("       -w <workers>\n");
	printf("       -debug\n");
	printf("       -opcode\n ");
	printf("       -verbose\n");
	printf("       -list <all|pl|c>\n");
	printf("       -memory <bytes>\n");
	printf("       -newgen <segments>\n");
	printf("       -wmemory <bytes>\n");

	exit(0);
      }
    }

  /* Proceed with boot process.
   */
  if (boot_file_cnt == 0)
    {
      PL_Print1(stderr, "No boot file\n");
      exit(0);
    }

  PL_Print1(stderr,"Booting...");
  fflush(stderr);

  orig_nr_of_workers = number_of_workers - 1;

  /* Set the new_gen_size to the default if it wasn't given as
   * an option to the program.
   */
  if (new_gen_size == 0)
    {
      new_gen_size = NEW_GEN_SIZE * number_of_workers;
    }

  w = initialize(number_of_workers);

  if (verbose)
    {
      w->global->flags.load_verbose = TRUE;
      PL_Print1(stderr,"\n");
    }

  /* Loading boot files.
   */
  for (i = 0; i < boot_file_cnt; i++)
    {
      if (expand_file_name(boot_file[i], path_buffer) == FALSE) 
	{
	  PL_Print2(stderr, "Corrupted boot path '%s'\n", boot_file[i]);
	  exit(0);
	}
      else if ((boot_file_ptr = fopen(path_buffer, "r")) == NULL)
	{
	  PL_Print2(stderr, "Unable to open boot file '%s'\n", path_buffer);
	  exit(0);
	}
	
      load_file(boot_file_ptr, w);

      fclose(boot_file_ptr);
    }

  PL_Print1(stderr, "wait\n");
    
  if (listing)
    {
      PL_Print1(currout, "The database contains the following predicates:\n");
      PL_Print1(currout, "-----------------------------------------------\n");
      plisting(w, option);
      PL_Print1(currout, "-----------------------------------------------\n");

      exit(0);
    }
    
  worker_set = w;

#if defined(PARALLEL)
  create_children(w, number_of_workers);
#endif

  engine(get_definition(StoreFunctor(atom_table_tagged[ATOM_START],0), w), w);

  exit(1);
}



#if defined(PARALLEL)
#if defined(USE_THREADS)

static void *start_engine(w)
     worker *w;
{
  worker_nr = w->pid;
  
  engine(NULL, w);

  return((void *) 1);
}

static void create_children(w, number_of_workers)
     worker *w;
     int number_of_workers;
{
  /* pthread_t pid; */
  thread_t pid;
  int i;
  int ret;
  
  nr_children = 0;

  child_pid[0] = Thread_Self();
     
  for (i = 1; i < number_of_workers; i++)
    {
      ret = Thread_Create(&child_pid[i],start_engine,&(w[i]));

      if (ret == -1)
	{
	  perror("cannot create threads");
	  luther_exit(1);
	}
    }

  nr_children = number_of_workers - 1;
  worker_nr = 0;
}

#else /* not USE_THREADS */

static void create_children(w, number_of_workers)
     worker *w;
     int number_of_workers;
{
  CHILD_PID_TYPE pid;
  int i;

  nr_children = 0;

  child_pid[0] = getpid();
     
  for (i = 1; i < number_of_workers; i++)
    {
      pid = fork();
      if (pid == 0)
	{
	  worker_nr = i;
	  engine(NULL, w + i);
	  luther_exit(1);
	}
      else
	{
	  if (pid == -1)
	    {
	      nr_children = i - 1;
	      perror("unable to fork");
	      luther_exit(1);
	    }
	  child_pid[i] = pid;
	}
    }
  nr_children = number_of_workers - 1;
  worker_nr = 0;
}

#endif /* USE_THREADS */
#endif /* PARALLEL */
