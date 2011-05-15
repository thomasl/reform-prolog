/*
 * initial.c
 *
 * Johan Bevemyr.....Sun Jun  2 1991
 * Patric Hedlin.....Mon Jan 23 1995
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include <limits.h>

#include <sys/signal.h>

#include "luther.h"
#include "inline.h"
#include "array.h"
#include "builtin.h"
#include "load.h"
#include "qload.h"
#include "display.h"
#include "initial.h"
#include "debug.h"
#include "engine.h"
#include "unix.h"

#include "expand_file_name.h"


#ifndef CLK_TCK
#  define CLK_TCK 1000
#endif


TAGGED atom_nil;		/* []		*/
TAGGED atom_list;		/* '.'		*/
TAGGED atom_true;		/* true		*/
TAGGED atom_fail;		/* fail		*/
TAGGED atom_false;		/* false	*/
TAGGED atom_user_input;		/* user_input	*/ 
TAGGED atom_user_output;	/* user_output	*/
TAGGED atom_user_error;		/* user_error	*/
TAGGED atom_user;		/* user		*/
TAGGED atom_inf;		/* inf		*/
TAGGED atom_nan;		/* nan		*/
TAGGED atom_equal;              /* '='		*/
TAGGED atom_less;               /* '<'		*/
TAGGED atom_greater;            /* '>'		*/
TAGGED atom_abort;              /* abort	*/
TAGGED default_prefix;          /* '$new'	*/

TAGGED functor_d_stream;	/* '$stream'/1	*/

TAGGED functor_colon;           /* ':'/2	*/
TAGGED functor_coma;            /* ','/2	*/
TAGGED functor_list;		/* '.'/2	*/

TAGGED module_user;             /* user		*/
TAGGED module_prolog;           /* prolog	*/
TAGGED module_public;           /* public	*/

int orig_nr_of_workers;
int tracing;                    /* used by the prolog tracer */

int initialdebugflag;

#if defined(DYN_SIZE)
u32 new_gen_size = 50*1024;
#else
u32 new_gen_size = 0; /* Set to zero to indicate that it has not yet
			 been determined how large the new generation 
			 should be */
#endif

int worker_nr;
worker *worker_set;


void def_c_pred(pname, procedure, arity, module, w)
    int pname;
    BOOL (procedure)();
    int arity;
    TAGGED module;
    worker *w;
{
  store_c_predicate(StoreFunctor(atom_table_tagged[pname], arity),
		    procedure, module, w);
}


void initialize_builtins(w)
    worker *w;
{
  /* inline.c */

  def_c_pred(ATOM_IS, luther_is, 2, PUBLIC, w);

  initialize_array(w);

  initialize_builtin(w);

  /* load.c */

  def_c_pred(ATOM_D_LOAD, luther_load, 3, PROLOG, w);

  /* qload.c */

  def_c_pred(ATOM_D_QLOAD, luther_qload, 1, PROLOG, w);

  /* statistics.c */

  def_c_pred(ATOM_STATISTICS, luther_statistics,0, PUBLIC, w);
  def_c_pred(ATOM_STATISTICS, luther_statistics_n,1, PUBLIC, w);
  def_c_pred(ATOM_D_STATISTICS_RUNTIME,
	     luther_statistics_runtime,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_GCTIME,
	     luther_statistics_gctime,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_GCNR,
	     luther_statistics_gcnr,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_GCBYTES,
	     luther_statistics_gcbytes,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_WALLTIME,
	     luther_statistics_walltime,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_PARALLEL_RUNTIME,
	     luther_statistics_parallel_runtime,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_MEMORY,
	     luther_statistics_memory,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_GLOBAL,
	     luther_statistics_global,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_LOCAL, luther_statistics_local,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_TRAIL, luther_statistics_trail,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_DEREF, luther_statistics_deref,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_RESTORE, luther_statistics_restore,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_INSTR, luther_statistics_instr,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_INSTR_PROF, luther_statistics_instr_prof,1, 
	     PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_TIDY, luther_statistics_tidy,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_SWAP, luther_statistics_swap,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_CODE, luther_statistics_code,1, PROLOG, w);
  def_c_pred(ATOM_D_STATISTICS_ATOM, luther_statistics_atom,1, PROLOG, w);

  def_c_pred(ATOM_PMON, luther_pmon, 0, PUBLIC, w);
  def_c_pred(ATOM_PMON_PRINT, luther_pmon_print, 0, PUBLIC, w);


  initialize_inout(w);

  initialize_unix(w);

  /* expand_file_name */

  def_c_pred(ATOM_EXPAND_FILE_NAME, luther_expand_file_name,3, PUBLIC, w);

  init_backtrackable_c(w);
}


void init_predefined_atoms(w)
     worker *w;
{
  atom_nil         = atom_table_tagged[ATOM_NIL];
  atom_list        = atom_table_tagged[ATOM_LIST];

  atom_true        = atom_table_tagged[ATOM_TRUE];
  atom_fail        = atom_table_tagged[ATOM_FAIL];
  atom_false       = atom_table_tagged[ATOM_FALSE];

  atom_user_input  = atom_table_tagged[ATOM_USER_INPUT];
  atom_user_output = atom_table_tagged[ATOM_USER_OUTPUT];
  atom_user_error  = atom_table_tagged[ATOM_USER_ERROR];

  atom_user        = atom_table_tagged[ATOM_USER];

  atom_inf         = atom_table_tagged[ATOM_INF];
  atom_nan         = atom_table_tagged[ATOM_NAN];

  atom_equal       = atom_table_tagged[ATOM_EQUAL];
  atom_less        = atom_table_tagged[ATOM_LESS];
  atom_greater     = atom_table_tagged[ATOM_GREATER];

  atom_abort       = atom_table_tagged[ATOM_ABORT];

  default_prefix   = atom_table_tagged[ATOM_PREFIX];

  module_user      = atom_table_tagged[ATOM_USER];
  module_prolog    = atom_table_tagged[ATOM_PROLOG];
  module_public    = atom_table_tagged[ATOM_PUBLIC];

  functor_d_stream = StoreFunctor(atom_table_tagged[ATOM_D_STREAM], 1);

  functor_colon    = StoreFunctor(atom_table_tagged[ATOM_COLON], 2);
  functor_coma     = StoreFunctor(atom_table_tagged[ATOM_COMA], 2);
  functor_list     = StoreFunctor(atom_list, 2);
}

/* System specific initalizations. */

double ticks_per_msecond;


static void init_system()
{
  ticks_per_msecond = CLK_TCK / 1000.0;
}

/* Initialize the signal handler */

static void init_signals()
{
  install_sigvec(SIGINT, interrupt_handler);
  install_sigvec(SIGUSR1, usr1_interrupt_handler);

  install_sigvec(SIGILL, signal_handler);
  install_sigvec(SIGFPE, signal_handler);
  install_sigvec(SIGBUS, signal_handler);
  install_sigvec(SIGSEGV, signal_handler);
}

/* Initialisations that need to be made once only. */

extern luther_init_buffers PROTO((void));

static worker *init_once(n)
    int n;
{
  worker *w;

  init_system();
  init_signals();

  build_char_table();

  luther_init_buffers();

  w = init_global_memory(n);

  init_semaphores(w,n);
  init_spin_semaphores(w,n);
    
  /* Node specific initalization */

  init_node(w);
    
  init_functortable(w);
    
  init_database(w);
    
  /*   These initializations requires the atom table to be present
   * and can thus not be done in the leaf until the atom table has
   * been recieved by com-engine().
   */
  initialize_inline(w); 

  init_predefined_atoms(w);

  w->global->current_module = module_prolog;

  initialize_builtins(w);

  initialize_flags(w);

  /* start timers */
  {
    int i;
    for (i=0 ; i < n ; i++)
      {
	init_statistics(&w[i]);
      }
  }

#if defined(COPY_GC)
  w->global->ordernr = 0;
#endif

#if defined(PARALLEL)
  w->global->scheduling = STATIC;
  w->global->scheduling_mode = VERTICAL;

  w->global->workers_active = FALSE;

  w->global->debugsig = 0;
#endif

  return w;
}

static void init_each_time(w)
    worker *w;
{
  w->frame  = (environment *)w->stack_start;
  w->choice = (choicepoint *)w->stack_start;
  
#if defined(DEBUG)
  init_debugger(w,initialdebugflag);
#endif

  interpret_conj =
    get_definition(StoreFunctor(atom_table_tagged[ATOM_COMA],2),w);

  interpret_wake =
    get_definition(StoreFunctor(atom_table_tagged[ATOM_WAKE],2),w);

  interpret_goal =
    get_definition(StoreFunctor(atom_table_tagged[ATOM_D_INTERPRET_GOAL],1),w);

  interpret_goal_spy =
    get_definition(StoreFunctor(atom_table_tagged[ATOM_D_INTERPRET_GOAL_SPY],1),w);

  interpret_goal_trace =
    get_definition(StoreFunctor(atom_table_tagged[ATOM_D_INTERPRET_GOAL_TRACE],1),w);

  w->wake_count = 0;

  return;
}

void reinitialize(w)
    worker *w;
{
  register choicepoint *B;

  /* Resets engine */

#if defined(COPY_GC_GEN)
  w->heap_top = w->heap_start + (w->heap_end-w->heap_start)/2;
  w->heap_old_gen_top = w->heap_start;
  w->heap_new_gen_start = w->heap_top;
#else  /* COPY_GC_GEN */
  w->heap_top = w->heap_start;
#endif /* COPY_GC_GEN */

#if defined(TIMESTAMP) || defined(UNBOUND)
  w->time = w->uncond = 0;
#else
  w->uncond = w->heap_top;
#endif
  w->trail_top = w->trail_start;

  init_each_time(w);

  B = w->choice;
  B->trail_top = w->trail_top;
  B->global_top = w->heap_top;
  B->last_choice = NULL;
  B->generation = NEW_GEN;
  B->cont_env = NULL;
  B->next_instr = w->next_instr;
  B->next_clause = NULL;
  B->arity = 0;
    
  w->frame = (environment *) (((char *) w->choice) + sizeof(choicepoint) -
			      sizeof(TAGGED));
  w->frame->cont_env = NULL;
  w->frame->next_instr = w->next_instr;
}

void init_wam(w)
    worker *w;
{
  register choicepoint *B;

  init_local_memory(w);
  init_each_time(w);

#if defined(TIMESTAMP) || defined(UNBOUND)
  w->time = w->uncond = 0;
#else
  w->uncond = w->heap_top;
#endif
  w->event_flag = 0;
  w->trace_mode = LUTHER_TRACE_OFF;
  w->next_instr = &(start_pred)[2];

  B = w->choice;

  B->trail_top = w->trail_top;
  B->global_top = w->heap_top;
  B->last_choice = NULL;
  B->generation = NEW_GEN;
  B->cont_env = NULL;
  B->next_instr = w->next_instr;
  B->next_clause = NULL;
  B->arity = 0;

  w->frame = (environment *) (((char *) w->choice) + sizeof(choicepoint) -
			      sizeof(TAGGED));
  w->frame->cont_env = NULL;
  w->frame->next_instr = w->next_instr;

#if defined(PARALLEL)
  w->global->event_flag = 0;
  w->stats->current_user_time = (int) usertime();
#endif /* PARALLEL */
}

worker *initialize(n)
    int n;
{
  return init_once(n);
}

