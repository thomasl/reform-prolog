/*
 * engine.c
 *
 * Johan Bevemyr.....Wed Apr 14 1993
 * Patric Hedlin.....Wed Aug 17 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"
#include "engine.h"
#include "unify.h"
#include "debug.h"
#include "inline.h"
#include "bignum.h"
#include "labelsort.h"
#include "initial.h"
#include "array.h"
#include "socket.h"


/* Globally accessible WAM variables.
 */
GLOBAL definition *interpret_conj; /* Used by interrupt handler (event.c) */
GLOBAL definition *interpret_wake; /* Used by freeze handler (event.c)    */

GLOBAL definition *interpret_goal;       /* Used by meta_call.              */
GLOBAL definition *interpret_goal_spy;   /* Used by meta_call in spy mode.  */
GLOBAL definition *interpret_goal_trace; /* Used by meta_call in trace mode.*/


/* Table of instruction addresses for threaded code compilation
 */
#if defined(THREADED_CODE)
GLOBAL c_address *global_label_table;
#endif


code start_pred[] = { 0, HALT, HALT, HALT, HALT, HALT, HALT }; 


/*   Compare two tagged numbers (used by switch_on_constant), one of which
 * is a big number (possibly both). Used by Switch_On_Constant.
 */
#define CompareNumber(x, y) \
  (IsBIG(x) ? (IsBIG(y) ? bignum_cmp(GetBignum(x), GetBignum(y)) : 1) : -1)


#if 1
#define PrintInst(I,A)
#else
#if 1                   /* debug stuff */
#define PrintInst(I,A)   						   \
{									   \
  icount++;								   \
}
#else

  if (*position == specialvalue)                                           \
       w->debug.debugflag = 1;                                             \
  if (icount > 40000000)						   \
     w->debug.debugflag = 1;						   \
}


#define PrintInst(I,A)							   \
{									   \
  icount++;                                                                \
                                                                           \
  if (icount > 40000000)						   \
      w->debug.debugflag = 1;						   \
                                                                           \
  if(icount > 0 )						           \
    {									   \
      if(read_sock)							   \
	{								   \
	  char tmp[255];						   \
	  readline(iosock,tmp);						   \
									   \
	  if (atoi(tmp) != Get_Op(I)+(A))				   \
	    {								   \
	      fprintf(stderr,"diff at %lx\n",icount);			   \
	    }								   \
	}								   \
      else			/* write to socket */			   \
	{								   \
	  char tmp[255];						   \
	  sprintf(tmp,"%d\n",Get_Op(I)+(A));				   \
	  writebuff(iosock,tmp);					   \
	}								   \
    }									   \
}
#endif
#endif

/**********************************************************************
 *
 * The Engine (WAM emulator).
 *
 *   If the worker pointer (w) is NULL then the label table is initialized
 * for threaded code. 
 *
 *   If the definition pointer (def) is NULL then the wam is called in a
 * parallel context by a parallel worker and should await commands from the
 * sequential worker.
 * 
 */
void engine(def, w)
     definition *def;
     register worker *w;
{
  register code *pc;

  register TAGGED instruction,       /* current instruction       */
                  *s,                /* structure pointer         */
                  *areg,             /* cached X-register pointer */
                  *yreg,             /* cached Y-register pointer */
                  *heap_top;         /* cached Heap top pointer   */


#if 0
  long icount = 0;
  TAGGED specialvalue = 0;
  TAGGED othervalue = 1;
  TAGGED *position = &othervalue;

  BOOL read_sock = FALSE;
  buff_io_stream *iosock;
#endif

  /* If threaded code emulation, define and export labels.
   */
#if defined(THREADED_CODE)

  register BOOL writemode = FALSE;

#ifdef Inst_Def
#undef Inst_Def
#endif

#define Inst_Def(X,Y,Z)	&&Y,

  static c_address label_table[] = {
#include "instructions.h"
  };
  
  if (w == NULL)
    {
      global_label_table = label_table;
      return;
    }

#endif /* THREADED_CODE */



#if 0
  if (read_sock)
    {
      int fid;
      fid = open_socket_listen(w,"icountsock");
      iosock = init_io_stream(fid,w);
    }
  else
    {
      int fid;
      fid = open_socket_talk(w,"icountsock");
      iosock = init_io_stream(fid,w);
    }
#endif


  /* Initialize worker (registers local to each worker).
   */
  init_wam(w);

  /*   Initial choice-point. If we ever backtrack to this choice-point
   * we know that then entire program has failed. This is used for
   * making a graceful exit.
   */
  w->fail_choice = w->choice;

  /*   Cache variables in registers. This will hopefully speed things
   * up a bit. Macros are used to hide this. 
   */
  s = w->s;           /* No macro used for this register               */
  pc = w->pc;         /* No macro used for this register either        */
  areg = w->regs;     /* Use the macro X(i) for accessing X-register i */

  /*   Use H to access heap top. SaveCache(H) and LoadCache(H) are used
   * for writing to/from heap_top to the worker structure.
   */
  heap_top = w->heap_top;

  /* If this wam is used as a co-worker then it should wait for work.
   */
  if (def == NULL) 
    {
#if defined(PARALLEL)
      w->synch_state = SYNCH_FIRST;
      goto wait_for_work;
#else
      goto done;
#endif /* PARALLEL */
    }

  /**********************************************************************
   *
   *   We start the wam by running the definition the WAM was called with.
   * Put the definition pointer into the start_pred template and jump into
   * the emulator loop.
   *
   */
  start_pred[3] = (TAGGED) def;
  pc = &(start_pred[3]);
  goto execute;


 global_fail:
  {
    /**********************************************************************
     *
     * Global failure handler:
     *
     *   The entire program failed. If co-worker, report failure and wait for
     * more work. If sequential worker, return. 
     *
     */
    if (w->pid != 0)
      {
	Report_Global_Fail;
#if ! defined(NDEBUG)
	PL_Print2(stderr,"global fail in %d\n",w->pid);
#endif
	goto done;
      }
    else
      {
	FatalError("Global Failure");
      }
  }

 done:
  {
#if defined(PARALLEL)

    /**********************************************************************
     *
     *   This is the command loop for the co-workers. The co-workers awaiting
     * commands from the sequential worker. 
     *
     *    W_EXECUTE_CODE
     *       This command is issued by the sequential worker when 
     *       executing a START_*_BODY instruction. The co-worker 
     *       is told to execute a number of iterations.
     *
     *    W_RESET
     *       This command is used for resetting the co-workers. It is
     *       currently not used.
     *
     *    W_BACKTRACK
     *       This command is issued by the sequential worker when it
     *       backtracks across a parallel execution. The co-workers
     *       have to undo all conditional bindings they have done.
     *
     *    W_TIDY
     *       This command is not used. It used to be issued when the
     *       a parallel execution marker was found on the trail while
     *       performing a TidyTrail operation (TidyTrail is performed
     *       during cut to remove unnecessary trail entries, however, 
     *       it turns out that tidying the trail is unnecessary).
     *
     *    W_DEBUG
     *       This command is issued by the wamdebug/1 builtin predicate.
     *       It instructs a given co-worker to trace its execution using
     *       the WAM level debugger.
     *
     *    W_REDUCE_PLUS
     *       This command is used for implementing parallel reduction of
     *       vector lists. A better solution is to compile this into
     *       somewhat specialized WAM code.
     *
     *    W_REDUCE_TIMES
     *       Same as above.
     * 
     *    W_GC_MARK
     *       Issued by the garbage collector when run by the sequential
     *       worker. Further garbage collection commands are handled by
     *       a similar command loop in worker_garbage_collect().
     * 
     *    W_BUILD_VARIABLES
     *       This command is used for implementing an experimental version
     *       of parallel unification (issued during execution of 
     *       the BUILD_VARIABLES instruction). It initializes a vector list
     *       in parallel
     *
     *    There are few more commands but they can only occur during 
     *    garbage_collection. As such they are handled by the command loop
     *    in worker_garbage_collect().
     *
     */

    SaveCache(H);

  wait_for_work:
    {
      /*   Report that all work have been performed and wait for the next
       * parallel command to be issued.
       */
      w->stats->current_user_time = (int) usertime();

      synchronize(w);

      /* Decode command from sequential worker.
       */
      switch (w->global->parallel_start.type) {

      case W_EXECUTE_CODE:
	{
	  /*   Load heap top pointer into cache register (it must be flushed
	   * at end of execution for inspection by the sequential worker).
	   * Save current heap top on trail for backtracking.
	   *   Initialize stack and time counter. The stack is always empty
	   * at start of a parallel execution since no choice points can
	   * remain after a parallel phase (each iteration is deterministic).
	   * Finally, jump into the emulation loop at the pc given in the
	   * command structure.
	   */
	  LoadCache(H);
	  pc = w->global->parallel_start.code;
	  PushOnTrail(w,Tagify(H,NUM));
	  StackAndTimeInit(w);
	  w->fail_choice = w->choice;
	  w->old_gen_uncond = w[-(w->pid)].old_gen_uncond;
	  Execute_Read_Instr;
	}
	
      case W_RESET:
	{
	  reinitialize(w);
	  w->fail_choice = w->choice;
	}
	goto wait_for_work;
	
      case W_BACKTRACK:
	{
	  Worker_Unwind_Trail;      
#if defined(GENERATIONAL)
	  if (w->trail_top < w->trail_old_gen_top)
	    w->trail_old_gen_top = w->trail_top;
#endif  /* GENERATIONAL */
	  
	}
	goto wait_for_work;
	
      case W_TIDY:
	{
	  Worker_Tidy_Trail;
#if defined(GENERATIONAL)
	  if (w->trail_top < w->trail_old_gen_top)
	    w->trail_old_gen_top = w->trail_top;
#endif  /* GENERATIONAL */
	}
	goto wait_for_work;
	
      case W_REST:
	{
	  w->synch_state = SYNCH_SLEEP;
	}
	goto wait_for_work;

      case W_DEBUG:
	{
#if ! defined(NDEBUG)
	  if (((sword) w->global->parallel_start.code) == w->pid)
	    {
	      w->debug.debugflag = TRUE;
	      w->debug.debugmode = DBG_CREEP;
	    }
#endif
	}
	goto wait_for_work;

      case W_DEBUG_FILE:
	{
#if ! defined(NDEBUG)
	  if (((sword) w->global->parallel_start.code) == w->pid)
	    {
	      FILE *newout;
	      char pathBuf[MAXPATHLEN+1];

	      w->debug.debugflag = TRUE;
	      w->debug.debugmode = DBG_TRACE;

	      if (currerr != stderr)
		fclose(currerr);

	      if (!expand_file_name(GetString((TAGGED)w->global->parallel_start.vector,w),
				    pathBuf))
		goto wait_for_work;

	      if ((newout = fopen(pathBuf, "w")) == NULL)
		{
		  fprintf(currerr, "{Error: unabled to open %s for writing}\n", 
			  pathBuf);
		}
	      else
		{
		  currerr = newout;
		}
	    }
#endif
	}
	goto wait_for_work;

      case W_DEBUG_SOCKET:
	{
#if ! defined(NDEBUG)
	  if (((sword) w->global->parallel_start.code) == w->pid)
	    {
	      start_debug_socket(w->pid, w);
	    }
#endif
	}
	goto wait_for_work;
	
      case W_REDUCE_PLUS:
	{
	  w->global->reduction_results[w->pid-1] = reduce_vector_plus(w);
	}
	goto wait_for_work;
	
      case W_REDUCE_TIMES:
	{
	  w->global->reduction_results[w->pid-1] = reduce_vector_times(w);
	}
	goto wait_for_work;
	
      case W_GC_MARK:
	{
	  w->gc_info.arity = 0;
	  w->gc_info.env_size = 0;

	  w->choice = NULL;

	  (void) worker_garbage_collect(w, TRUE);
	}
	goto wait_for_work;
	
      case W_BUILD_VARIABLES:
	{
	  TAGGED *vector_index, *vector_stop;
	  s32 vectorsize;
	  s32 mod;
	  s32 div; 

	  vectorsize = w->global->parallel_start.size;
	  vector_index = w->global->parallel_start.vector;

	  div = vectorsize / w->global->active_workers;
	  mod = vectorsize % w->global->active_workers;
	  vector_index += 2*VARSIZE * (div * (w->pid-1) +
				       (mod < w->pid ? mod : (w->pid-1)));
	  
	  vector_stop = vector_index;
	  vector_stop += 2 * VARSIZE *
	    (div + (w->pid <= mod ? 1 : 0));
	  
	  InitTimeLite(w);

	  while (vector_index < vector_stop)
	    {
	      CreateHVA(vector_index,w);
	      PushOnHeap(vector_index,Tagify(vector_index+VARSIZE,LST));
	    }
	}
	goto wait_for_work;
	
      case W_REASSIGN:
	{
	  s32 *rvector;

	  rvector = (s32 *) w->global->parallel_start.vector;

	  w = &((w-w->pid)[rvector[w->pid]]);
	  areg = w->regs;
	}
	goto wait_for_work;

#if defined( USE_PMON)
      case W_PMON_DELTA:
	{
	  pmon_delta(&(w->global->pmon_data[w->pid]));
	}
	goto wait_for_work;
#endif
      default:
	{
	  Error("No such command");
	}
	goto wait_for_work;
      }
    }
#endif /* PARALLEL */
  }


  /**********************************************************************
   *
   * The main emulator loop.
   *
   *   Instructions are decoded and switched upon. There are two
   * switches, one to use when in read mode (default) and one for
   * write mode. This rely on the observation that once write mode has
   * been entered the emulator will remain in write mode while unify
   * instructions are executed.  However, this optimization cannot be
   * used by the threaded code implementation. In that case, a write
   * mode register (write_mode) is used for read/write mode tracking.
   *
   */
 instructions:
  {
    DisplayInstr("read");	/* WAM level debugger port for read mode. */

#if !defined(NDEBUG)  
    {
      if (H < w->heap_start)
	fprintf(stderr,"heaptop < heapstart in %d: %lx\n", w->pid,H);
    }
#endif    
  
    /* Fetch next instruction (opcode + registers).
     */
    instruction = Get_Code(pc);

    /* statistics (if compiled with -DSTATISTICS) */

    InstrStat(instruction);	

    /* Debugstuff */
    PrintInst(instruction,0);

    switch (Get_Op(instruction)) { /* Dispatch on opcode. */

      /************************
       * DUMMY_INSTRUCTION
       ************************/
    case DUMMY_INSTRUCTION:  /* We do not want an instruction with opcode */
    dummy_instruction:	     /* zero therefore the dummy instruction      */
      {
	FatalError("illegal instruction");
      }
      break;

      /************************
       * SWITCH_ON_TERM
       ************************/
    case SWITCH_ON_TERM:
    switch_on_term:
      {
	DerefNLL(X(0),X(0));

	switch (TagOf(X(0)))
	  {
	  case HVA:
	    CVA_CASE({});
	  case SVA:
	    Dispatch(pc,Var_Label);
	  case NUM:
	  case BOX:
	    Dispatch(pc,NUM_Label);
	  case ATM:
	    Dispatch(pc,ATM_Label);
	  case LST:
	    Dispatch(pc,LST_Label);
	  case STR:
	    Dispatch(pc,STR_Label);
	  case GEN:
	    /*
	     *   This could be extended so that switch_on_term has a special
	     * field for generic objects, and an introduction of
	     * switch_on_generic could also be done. For now, we use the
	     * same as for variables as they are the most general.
	     */
	    Dispatch(pc,Var_Label);
	  default:
	    PL_Print2(currout,"engine: switch_on_term - no such termtype %lu",
		      TagOf(X(0)));
	    luther_exit(0);
	  }
      }
    
      /************************
       * SWITCH_ON_CONSTANT
       ************************/
    case SWITCH_ON_CONSTANT:
    switch_on_constant:
      {
	register indx i;
	register TAGGED constant;
	
	i = Get_Index(pc);
	constant = X(0);
	
	if (i < 5)		/* linear search */
	  { 
	    register BOOL equal;

	    do {
	      register TAGGED c = Get_Tagged(pc);

	      if (IsBOX(c) || IsBOX(constant))
		equal = CompareNumber(c, constant) == 0 ? TRUE : FALSE;
	      else
		equal = constant == c ? TRUE : FALSE;

	      if (equal)
		break;
	      else 
		Inc_Label(pc);

	    } while (--i);
	  }
	else
	  {
	    /*
	     * The binary search:
	     *
	     *   The key (search) table is built in on of two ways:
	     *
	     * o  all keys are atoms (tagged ATM)
	     *
	     * o  all keys are integers (this includes objects tagged as NUM
	     *    as well as BOX:BIG)
	     *
	     *   A table containing small and large integers must be handled
	     * with care in order for the search to work properly. We could
	     * use the tag-ordering but this would only work when taggs are
	     * high. Instead we use a separate comparsion when any of the
	     * objects compared is a big number.
	     */
	    register int l = 0;
	    register int r = i - 1;
	    register labelt *table = (labelt *) pc;

	    register int compare;

	    do {
	      register int x = (l + r) / 2;

	      register TAGGED c = table[x].constant;

	      if (IsBOX(c) || IsBOX(constant))
		compare = CompareNumber(c, constant);
	      else
		compare = c == constant ? 0 : (c < constant ? -1 : 1);

	      if (compare > 0)	/* c > constant */
		r = x - 1;
	      else if (compare < 0) /* c < constant */
		l = x + 1;
	      else
		{
		  pc = ((code *) &(table[x])) + 1;
		  goto sw_done;
		}
	    } while (r >= l);

	    /* default */
	    pc = ((code *) &(table[i]));
	  }
	
      sw_done:
	Dispatch(pc,0);
      }
      
      /************************
       * SWITCH_ON_STRUCTURE
       ************************/
    case SWITCH_ON_STRUCTURE:
    switch_on_structure: 
      {
	register indx i;
	register TAGGED functor;
	
	i = Get_Index(pc);
	functor = GetFunctor(X(0));
	
	if (i < 5)		/* linear search */
	  {
	    register TAGGED str;

	    do {
	      str = Get_Tagged(pc);
	      if (functor != str) 
		Inc_Label(pc);
	      else
		break;
	    } while (--i);
	  }
	else
	  {
	    register labelt *table = (labelt *) pc;
	    register int x, l, r;

	    l = 0; r = i-1;

	    do {
	      x = (l + r) / 2;
	      if (functor < table[x].constant)
		r = x - 1;
	      else if (functor > table[x].constant)
		l = x + 1;
	      else
		{
		  pc = ((code *) &(table[x])) + 1;
		  goto sw_done_s;
		}
	    } while (r >= l);

	    /* default */
	    pc = ((code *) &(table[i]));
	  }
      sw_done_s:
	Dispatch(pc,0);
      }

      /************************
       * TRY
       ************************/
    case TRY: 
    try: 
      {
	register choicepoint *newchoice;
	register int i;
	register int arity;
	
	arity = Get_Index_I(1,instruction);

	newchoice = (choicepoint *) Get_Local_Stack_Top;

	if (newchoice > (choicepoint *) w->stack_margin)
	  {
	    w->gc_info.arity = arity;
	    w->gc_info.env_size = FrameSize(w->next_instr);

	    shift_stack(w, (size_t)newchoice - (size_t)w->stack_margin);

	    newchoice = (choicepoint *) Get_Local_Stack_Top;
	  }

	newchoice->trail_top = w->trail_top;
	newchoice->global_top = H;
	newchoice->last_choice = w->choice;
	newchoice->cont_env = w->frame;
	newchoice->next_instr = w->next_instr;
	newchoice->next_clause = pc+1;
	newchoice->arity = arity;
	newchoice->generation = NEW_GEN;

	for (i = 0; i != arity; i++)
	  newchoice->areg[ARSIZE*i] = X(i);

#if defined(TIMESTAMP) || defined(UNBOUND)
	newchoice->timestamp = w->time;
	w->time += TIMEUNIT;
	w->uncond = w->time;
#else
	w->uncond = H;
#endif /* TIMESTAMP */

	w->choice = newchoice;
	pc = DispatchLabel(pc,0);

	Execute_Read_Instr;
      }
      
      /************************
       * RETRY
       ************************/
    case RETRY:
    retry: 
      {
	w->choice->next_clause = pc+1;
	pc = DispatchLabel(pc,0);
#if defined(TIMESTAMP) || defined(UNBOUND)
	w->time += TIMEUNIT;
#endif
	Execute_Read_Instr;
      }
      
      /************************
       * TRUST
       ************************/
    case TRUST:
    trust: 
      {
	w->choice = w->choice->last_choice;
#if defined(TIMESTAMP) || defined(UNBOUND)
# if defined(GENERATIONAL)
	if (w->choice->timestamp+TIMEUNIT < w->old_gen_uncond)
	  w->uncond = w->old_gen_uncond;
	else
# endif
	  w->uncond = w->choice->timestamp+TIMEUNIT;
#else
	w->uncond = w->choice->global_top;
#endif /* TIMESTAMP */

	TidyTrail;
	
	pc = DispatchLabel(pc,0);
	Execute_Read_Instr;
      }
      
      /************************
       * TRY_ME_ELSE
       ************************/
    case TRY_ME_ELSE:
    try_me_else: 
      {
	register choicepoint *newchoice;
	register int i;
	register int arity;

	Pre_FetchTop(1);
	
	arity = Get_Index_I(1,instruction);

	
	newchoice = (choicepoint *) Get_Local_Stack_Top;

	if (newchoice > (choicepoint *) w->stack_margin)
	  {
	    w->gc_info.arity = arity;
	    w->gc_info.env_size = FrameSize(w->next_instr);

	    shift_stack(w, (size_t)newchoice - (size_t)w->stack_margin);

	    newchoice = (choicepoint *) Get_Local_Stack_Top;
	  }

	newchoice->trail_top = w->trail_top;
	newchoice->global_top = H;
	newchoice->last_choice = w->choice;
	newchoice->cont_env = w->frame;
	newchoice->next_instr = w->next_instr;
	newchoice->next_clause = DispatchLabel(pc,0);
	newchoice->generation = NEW_GEN;
	pc++;
	newchoice->arity = arity;
	
	for (i = 0; i != arity; i++)
	  newchoice->areg[ARSIZE*i] = X(i);

#if defined(TIMESTAMP) || defined(UNBOUND)
	newchoice->timestamp = w->time;
	w->time += TIMEUNIT;
	w->uncond = w->time;
#else
	w->uncond = H;
#endif /* TIMESTAMP */
	
	w->choice = newchoice;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * RETRY_ME_ELSE
       ************************/
    case RETRY_ME_ELSE:
    retry_me_else: 
      {
	Pre_FetchTop(1);
	w->choice->next_clause = DispatchLabel(pc,0);
	pc++;
#if defined(TIMESTAMP) || defined(UNBOUND)
	w->time += TIMEUNIT;
#endif
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * TRUST_ME
       ************************/
    case TRUST_ME:
    trust_me:
      {
	Pre_FetchTop(0);
	w->choice = w->choice->last_choice;
#if defined(TIMESTAMP) || defined(UNBOUND)
# if defined(GENERATIONAL)
	if (w->choice->timestamp+TIMEUNIT < w->old_gen_uncond)
	  w->uncond = w->old_gen_uncond;
	else
# endif
	  w->uncond = w->choice->timestamp+TIMEUNIT;
#else
	w->uncond = w->choice->global_top;
#endif
	TidyTrail;

	Pre_Execute_Read_Instr(1);
      }

      /************************
       * CHOICE_X
       *
       * Note: (cut-level management in Choice_{X|Y} and Cut_{X|Y})
       *
       *   The cut-level is saved as an offset relative the local stack base
       * in order for the stack shifter to be unaware of its presence. Thus,
       * the stack shifter may not compact the stack, using this scheme,
       * since the cut-level offset would turn invalid.
       *   Using a pointer to the choice point, we would have to make the
       * stack shifter identify this reference (since it is an in-stack
       * reference) in order to be updated during stack shifting.
       ************************/
    case CHOICE_X:
    choice_x: 
      {
	register indx n;
	register long offset = (TAGGED *)w->call_choice - w->stack_start;

	Pre_FetchTop(1);
	
	n = Get_Index_I(1,instruction);

	X(n) = Make_Integer(offset);

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * CHOICE_Y
       ************************/
    case CHOICE_Y:
    choice_y: 
      {
	register indx n;
	register long offset = (TAGGED *)w->call_choice - w->stack_start;

	Pre_FetchTop(1);
	
	n = Get_Index_I(1,instruction);
	
	Y(n) = Make_Integer(offset);

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * CUT
       ************************/
    case CUT:
    cut: 
      {
	Pre_FetchTop(0);

	if (w->choice > w->call_choice)
	  {
	    w->choice = w->call_choice;
#if defined(TIMESTAMP) || defined(UNBOUND)
# if defined(GENERATIONAL)
	    if (w->choice->timestamp+TIMEUNIT < w->old_gen_uncond)
	      w->uncond = w->old_gen_uncond;
	    else
# endif
	      w->uncond = w->choice->timestamp+TIMEUNIT;
#else
	    w->uncond = w->choice->global_top;
#endif
	    TidyTrail;
	  }

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * CUT_X
       ************************/
    case CUT_X:
    cut_x:
      {
	register indx n;

	Pre_FetchTop(1);
	
	n = Get_Index_I(1,instruction);
	
	w->choice = (choicepoint *) (w->stack_start + GetNumber(X(n)));

#if defined(TIMESTAMP) || defined(UNBOUND)
# if defined(GENERATIONAL)
	if (w->choice->timestamp+TIMEUNIT < w->old_gen_uncond)
	  w->uncond = w->old_gen_uncond;
	else
# endif
	w->uncond = w->choice->timestamp+TIMEUNIT;
#else
	w->uncond = w->choice->global_top;
#endif
	TidyTrail;
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * CUT_Y
       ************************/
    case CUT_Y:
    cut_y: 
      {
	register indx i;
	TAGGED DY;

	Pre_FetchTop(1);
	
	i = Get_Index_I(1,instruction);
	
	DerefNLL(DY,Y(i));
	w->choice = (choicepoint *) (w->stack_start + GetNumber(DY));

#if defined(TIMESTAMP) || defined(UNBOUND)
# if defined(GENERATIONAL)
	if (w->choice->timestamp+TIMEUNIT < w->old_gen_uncond)
	  w->uncond = w->old_gen_uncond;
	else
# endif
	  w->uncond = w->choice->timestamp+TIMEUNIT;
#else
	w->uncond = w->choice->global_top;
#endif
	TidyTrail;

	Pre_Execute_Read_Instr(1);
      }
      
      
      /************************
       * INLINE
       ************************/
    case INLINE:
    in_line: 
      {
	register int fnk;
	
	BuiltinCallStatistics;
	
	fnk = Get_Index_I(1,instruction);
	
	SaveCache(H);
	
	Inc_Label(pc);

	if ((GetInlineFnk(fnk))(w,(s32 *)Get_UseArgs(pc)) == FALSE)
	  {
	    pc = DispatchLabel(pc,-1);
	  }
	else
	  {
	    pc += GetInlineArity(fnk);
	  }
	
	LoadCache(H);
	
	Execute_Read_Instr;
      }
      
      /************************
       * BUILTIN
       ************************/
    case BUILTIN:
    builtin: 
      {
	register int fnk;
	
	BuiltinCallStatistics;
	
	fnk = Get_Index_I(1,instruction);
	
	SaveCache(H);
	
	if ((GetInlineFnk(fnk))(w,(s32 *)Get_UseArgs(pc)) == FALSE)
	  goto fail;
	
	LoadCache(H);
	
	pc += GetInlineArity(fnk);

	Execute_Read_Instr;
      }
      
      /************************
       * META_CALL
       ************************/
    case META_CALL:
    meta_call:
      Get_Index_I(1,instruction);
      w->next_instr = pc+1;

      /************************
       * META_EXECUTE
       ************************/
    case META_EXECUTE:
    meta_execute:
      {
	register TAGGED goal;
	TAGGED goalbuf;
	register definition *def;
	register indx i;

	Pre_FetchInit;
	
	i = Get_Index_I_M(1,instruction);

	DerefNLL(goal,X(i));
	
	/* Get definition */
	
	def = get_definition_term(goal,w,&goalbuf);
	goal = goalbuf;

	i = ArityOf(def->name);

	switch (def->enter_instruction) {

	case ENTER_C:
	  {
	    /*   Copy arguments from structure, must be struct or atom
	     * since ','(_,_) is not a C defined predicate.
	     */
	    while (i--)
	      {
		X(i) = Ref(GetArg(goal,i));
	      }
	  
	    SaveCache(H);

	    switch ((def->entry_code.cinfo)(w)) {

	    case FALSE:
	      goto fail;

	    case TRUE:
	      {
		pc = w->next_instr;
		Pre_Fetch;
	      }
	      break;
	    }

	    LoadCache(H);
	  }
	  break;

	case ENTER_EMULATED:
	  {
	    /* Copy arguments from structure.
	     */
	    if (IsLST(goal))
	      {
		X(0) = Ref(GetCar(goal));
		X(1) = Ref(GetCdr(goal));
	      }
	    else
	      {
		while (i--)
		  {
		    X(i) = Ref(GetArg(goal,i));
		  }
	      }
	    
	    /* Save the program counter in the continuation.
	     */
	    w->call_choice = w->choice;
	    pc = def->entry_code.incoreinfo;
	    Pre_Fetch;
	  }
	  break;
	    
	case ENTER_TRUE_SPY:
	  {
	    w->call_choice = w->choice;

	    X(0) = goal;

	    switch (w->trace_mode) {

	    case LUTHER_TRACE_ON:
	    case LUTHER_TRACE_SPY:
	      {
		def->enter_instruction = ENTER_FAKE_SPY;

		pc = interpret_goal_spy->entry_code.incoreinfo;
	      }
	      break;

	    case LUTHER_TRACE_OFF:
	      {
		pc = interpret_goal->entry_code.incoreinfo;
	      }
	    }
	    Pre_Fetch;
	  }
	  break;
	    
	case ENTER_FAKE_SPY:
	  {
	    def->enter_instruction = ENTER_TRUE_SPY;
	  }
	  /* Fall through to interpreted call. */

	case ENTER_INTERPRETED:
	  {
	    w->call_choice = w->choice;

	    X(0) = goal;

	    switch (w->trace_mode) {

	    case LUTHER_TRACE_ON:
	      {
		pc = interpret_goal_trace->entry_code.incoreinfo;
	      }
	      break;

	    case LUTHER_TRACE_SPY:
	    case LUTHER_TRACE_OFF:
	      {
		pc = interpret_goal->entry_code.incoreinfo;
	      }
	    }
	    Pre_Fetch;
	  }
	  break;
	    
	case ENTER_UNDEFINED:
	  {
	    (void) luther_error(E_PRED_NOT_DEF, (TAGGED)def, w);
	  }
	  goto fail;
	}
	
	Pre_Execute_Read_Instr(0);
      }
      
      /************************
       * REQUIRE
       ************************/
    case REQUIRE:
    require:
      {
	Inc_Index(pc);

	GC_If_Needed_Require(w,0);

	Execute_Read_Instr;
      }
      
      /************************
       * REQUIRE_USING
       ************************/
    case REQUIRE_USING:
    require_using:
      {
	register s32 nr_live_x;

	Inc_Index(pc);

	nr_live_x = Get_Index_I(1,instruction) + 1;

	GC_If_Needed_Require(w,nr_live_x);
      
	Execute_Read_Instr;
      }
      
      /************************
       * ALLOCATE
       ************************/
    case ALLOCATE:
    allocate: 
      {
	register environment *newframe;
	register int arity;
	
	Pre_FetchTop(1);
	
	arity = Get_Index_I(1,instruction);

	newframe = (environment *) Get_Local_Stack_Top;

	if (newframe > (environment *) w->stack_margin)
	  {
	    w->gc_info.arity = arity;
	    w->gc_info.env_size = FrameSize(w->next_instr);

	    shift_stack(w, (size_t)newframe - (size_t)w->stack_margin);

	    newframe = (environment *) Get_Local_Stack_Top;
	  }
	
	newframe->cont_env = w->frame;
	newframe->next_instr = w->next_instr;
	SetY(newframe->yreg);
	w->frame = newframe;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * ALLOCATE2
       ************************/
    case ALLOCATE2:
    allocate2:
#if defined(BOUNDED_Q)
      {
	register environment *newframe;
	register int arity;
	
	arity = Get_Index_I(1,instruction);
	
	newframe = (environment *) Get_Local_Stack_Top;

	if (newframe > (environment *) w->stack_margin)
	  {
	    w->gc_info.arity = arity;
	    w->gc_info.env_size = FrameSize(w->next_instr);

	    shift_stack(w, (size_t)newframe - (size_t)w->stack_margin);

	    newframe = (environment *) Get_Local_Stack_Top;
	  }
	
	newframe->cont_env = w->frame->cont_env;
	newframe->next_instr = w->frame->next_instr;
	SetY(newframe->yreg);
	w->frame = newframe;

	Execute_Read_Instr;
      }
#else
      {
	FatalError("Instructions not implemented.");
	break;
      }
#endif /* BOUNDED_Q */
      
      /************************
       * DEALLOCATE
       ************************/
    case DEALLOCATE:
    deallocate:
      {
	Pre_FetchTop(0);
	w->next_instr = w->frame->next_instr;
	w->frame = w->frame->cont_env;
	SetY(w->frame->yreg);

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * INIT
       ************************/
    case INIT:
    init:
      {
	register int i;
	register indx n;
	
	i = Get_Index_I(1,instruction);
	
	while (i--)
	  {
	    n = Get_Index(pc);
	    LoadSVA(Y(n),w);
	  }
	
	Execute_Read_Instr;
      }
      
      /************************
       * CALL
       ************************/
    case CALL:
    call: 
      {
	Get_Index_I(1,instruction);

	w->next_instr = pc+1;
      }

      /************************
       * EXECUTE
       ************************/
    case EXECUTE:
    execute: 
      {
	register definition *def;

	Pre_FetchInit;
	
	CallStatistics;
	
	def = Get_Definition(pc);

	/* check event flag (gc, trace, wake suspended, etc) */
	
	GC_If_Needed_Execute(w,def);

#include "event.c"

	/* call definition */
	
	switch (def->enter_instruction) {

	case ENTER_C:
	  {
	    SaveCache(H);

	    switch ((def->entry_code.cinfo)(w)) {

	    case FALSE:
	      goto fail;

	    case TRUE:
	      {
		pc = w->next_instr;
		Pre_Fetch;
	      }
	      break;
	    }

	    LoadCache(H);
	  }
	  break;

	case ENTER_EMULATED:
	  {
	    pc = def->entry_code.incoreinfo;
	    Pre_Fetch;
	    w->call_choice = w->choice;
	  }
	  break;

	case ENTER_TRUE_SPY:
	  {
	    register TAGGED goal;
	    {
	      register int i = ArityOf(def->name);
	    
	      /* Make goal structure on heap.
	       */
	      Make_STR(H,goal,def->name);
	    
	      while (i--)
		{
		  if (IsSVA(X(i)))
		    {
		      WriteLocalValueX(X(i),H);
		    }
		  else
		    {
		      PushOnHeap(H,X(i));
		    }
		}
	    }
	    X(0) = goal;
	      
	    w->call_choice = w->choice;

	    switch (w->trace_mode) {

	    case LUTHER_TRACE_ON:
	    case LUTHER_TRACE_SPY:
	      {
		def->enter_instruction = ENTER_FAKE_SPY;

		pc = interpret_goal_spy->entry_code.incoreinfo;
	      }
	      break;

	    case LUTHER_TRACE_OFF:
	      {
		pc = interpret_goal->entry_code.incoreinfo;
	      }
	    }
	    Pre_Fetch;
	  }	  
	  break;

	case ENTER_FAKE_SPY:
	  {
	    def->enter_instruction = ENTER_TRUE_SPY;
	  }
	  /* Fall through to interpreted call. */

	case ENTER_INTERPRETED:
	  {
	    register TAGGED goal;
	    {
	      register int i = ArityOf(def->name);
	    
	      /* Make goal structure on heap.
	       */
	      Make_STR(H, goal, def->name);
	    
	      while (i--)
		{
		  WriteLocalValueX(X(i),H);
		}
	    }
	    X(0) = goal;
	    
	    w->call_choice = w->choice;

	    switch (w->trace_mode) {

	    case LUTHER_TRACE_ON:
	      {
		pc = interpret_goal_trace->entry_code.incoreinfo;
	      }
	      break;

	    case LUTHER_TRACE_SPY:
	    case LUTHER_TRACE_OFF:
	      {
		pc = interpret_goal->entry_code.incoreinfo;
	      }
	    }
	    Pre_Fetch;
	  }
	  break;

	case ENTER_UNDEFINED:
	  {
	    (void) luther_error(E_PRED_NOT_DEF, (TAGGED)def, w);
	  }
	  goto fail;
	}

	Pre_Execute_Read_Instr(0);
      }
      
      /************************
       * PROCEED
       ************************/
    case PROCEED:
    proceed:
      {
	Pre_FetchInit;

	pc = w->next_instr;
	Pre_Fetch;

	Pre_Execute_Read_Instr(0);
      }
      
      /************************
       * FAIL
       ************************/
    case FAIL:
    fail:
      {
	register int i;
	Pre_FetchInit;
	
	FailStatistics;
	
	if (w->choice <= w->fail_choice) 
	  goto global_fail;	/* The entire program failed */
	
	Unwind_Trail(w->choice->trail_top);
#if defined(GENERATIONAL)
	if (w->trail_top < w->trail_old_gen_top)
	  w->trail_old_gen_top = w->trail_top;
#endif  /* GENERATIONAL */

#if defined(MEASURE_HEAP_RECLAIM)
	w->stats->heap_reclaim += ((u32) H) - ((u32) w->choice->global_top);
#endif /* MEASURE_HEAP_RECLAIM */

#if ! defined(DISABLE_HEAP_RECLAIM)
	H = w->choice->global_top;
#endif /* DISABLE_HEAP_RECLAIM */
	
	w->call_choice = w->choice->last_choice;
	
	i = w->choice->arity;

	RestoreStat(i);

	while (i--)
	  {
	    X(i) = w->choice->areg[ARSIZE*i];
	  }
	
#if defined(TIMESTAMP) || defined(UNBOUND)
	w->uncond = w->time = w->choice->timestamp+TIMEUNIT;
# if defined(GENERATIONAL)
	if(w->uncond < w->old_gen_uncond)
	  w->old_gen_uncond = w->uncond;
# endif
#else
	w->uncond = H;
#endif /* TIMESTAMP */
	pc = w->choice->next_clause;
	Pre_Fetch;
	w->next_instr = w->choice->next_instr;
	w->frame = w->choice->cont_env;

	SetY(w->frame->yreg);
	
	DisplayFail("read");
	
	Pre_Execute_Read_Instr(0);
      }
      
      /************************
       * GET_X_VARIABLE
       ************************/
    case GET_X_VARIABLE:
    get_x_variable: 
      {
	register indx i,n;
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction);
	i = Get_Index_I(2,instruction);
	
	X(n) = X(i);

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GET_Y_VARIABLE
       ************************/
    case GET_Y_VARIABLE:
    get_y_variable: 
      {
	register indx i,n;
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction);
	i = Get_Index_I(2,instruction);
	
	Y(n) = X(i);

	Pre_Execute_Read_Instr(1);
      }

      /************************
       * GET_Y_FIRST_VALUE
       ************************/
    case GET_Y_FIRST_VALUE:
    get_y_first_value: 
      {
	register indx i,n;
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction);
	i = Get_Index_I(2,instruction);
	
	Y(n) = X(i);

	Pre_Execute_Read_Instr(1);
      }
      
#if defined(NUMA)
      /************************
       * GET_X_VALUE
       ************************/
    case GET_X_VALUE:
    get_x_value: 
      {
	register indx i,n;
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction);
	i = Get_Index_I(2,instruction);
	
	Local_Unify(X(n),X(i));

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GET_Y_VALUE
       ************************/
    case GET_Y_VALUE:
    get_y_value:
      {
	register indx i,n;
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction); 
	i = Get_Index_I(2,instruction);
	
	Local_Unify(Y(n),X(i));

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GET_CONSTANT
       ************************/
    case GET_CONSTANT:
    get_constant:
      {
	register TAGGED c, Xi;
	register BOOL equal;
	register indx i;
	Pre_FetchTop(2);
	
	c = Get_Tagged(pc); 
	i = Get_Index_I(1,instruction);
	
	/* Unify1(X(i),c); unfolding yields */
	Xi = X(i);

	DerefLockSwitch (Xi,
			 {
			   Local_Bind(Xi,c);
			 },
			 {
			   if (IsBIG(Xi) && IsBIG(c))
			     equal = bignum_eq(GetBignum(Xi),GetBignum(c));
			   else
			     equal = Xi == c ? TRUE : FALSE;

			   if (not(equal))
			     goto fail;
			 });
	
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GET_NIL
       ************************/
    case GET_NIL:
    get_nil:
      {
	register indx i;
	register TAGGED Xi;
	Pre_FetchTop(1);
	
	i = Get_Index_I(1,instruction);
	
	Xi = X(i);

	DerefLockSwitch (Xi,
			 {
			   Local_Bind(Xi,atom_nil);
			 },
			 {
			   if (Xi != atom_nil) 
			     goto fail;
			 });
	
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GET_STRUCTURE
       ************************/
    case GET_STRUCTURE:
    get_structure:
      {
	register TAGGED Xi,new, f;
	Pre_FetchTop(2);
	
	f = Get_Functor(pc);
	
	Xi = X(Get_Index_I(1,instruction));

	DerefLockSwitch (Xi,
			 {
			   Make_STR(H,new,f);
			   Local_Bind(Xi,new);
			   Pre_Execute_Write_Instr(1);
			 },
			 {
			   if (IsSTR(Xi))
			     {
			       if (GetFunctor(Xi) == f)
				 {
				   s = GetArg(Xi,0);
				   Pre_Execute_Read_Instr(1);
				 } else
				   goto fail;
			     } else
			       goto fail;
			 });
      }
      
      /************************
       * GET_LIST
       ************************/
    case GET_LIST:
    get_list:
      {
	register TAGGED Xi;
	register indx i;      
	Pre_FetchTop(1);
	
	i = Get_Index_I(1,instruction);
	
	Xi = X(i);

	DerefLockSwitch (Xi,
			 {
			   register TAGGED l;
			  
			   Make_LST(H,l);
			   Local_Bind(Xi,l);
			   Pre_Execute_Write_Instr(1);
			 },
			 {
			   if (IsLST(Xi))
			     {
			       s = RemoveTag(Xi,LST);
			       Pre_Execute_Read_Instr(1);
			     } else
			       goto fail;
			 });
      }
      
      /************************
       * GET_CONSTANT_X0
       ************************/
    case GET_CONSTANT_X0:
    get_constant_x0:
      {
	register TAGGED c,X0;
	register BOOL equal;
	Pre_FetchTop(1);
	
	c = Get_Tagged(pc);
	X0 = X(0);      

	DerefLockSwitch (X0,
			 {
			   Local_Bind(X0,c);
			 },
			 {
			   if (IsBIG(X0) && IsBIG(c))
			     equal = bignum_eq(GetBignum(X0),GetBignum(c));
			   else
			     equal = X0 == c ? TRUE : FALSE;

			   if (not(equal))
			     goto fail;
			 });

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GET_NIL_X0
       ************************/
    case GET_NIL_X0:
    get_nil_x0:
      {
	TAGGED X0;
	Pre_FetchTop(0);
	
	X0 = X(0);

	DerefLockSwitch (X0,
			 {
			   Local_Bind(X0,atom_nil);
			 },
			 {
			   if (X0 != atom_nil) 
			     goto fail;
			 });

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GET_STRUCTURE_X0
       ************************/
    case GET_STRUCTURE_X0:
    get_structure_x0:
      {
	register TAGGED f,X0;
	Pre_FetchTop(1);
	
	f = Get_Functor(pc);
	
	X0 = X(0);

	DerefLockSwitch (X0,
			 {
			   register TAGGED new;

			   Make_STR(H,new,f);
			   Local_Bind(X0,new);
			   Pre_Execute_Write_Instr(1);
			 },
			 {
			   if (GetFunctor(X0) != f)
			     goto fail;
			   s = GetArg(X0,0);
			   Pre_Execute_Read_Instr(1);
			 });
      }
      
      /************************
       * GET_LIST_X0
       ************************/
    case GET_LIST_X0:
    get_list_x0:
      {
	register TAGGED X0;
	Pre_FetchTop(0);
	
	X0 = X(0);

	DerefLockSwitch (X0,
			 {
			   register TAGGED l;
			  
			   Make_LST(H,l);
			   Local_Bind(X0,l);
			   Pre_Execute_Write_Instr(1);
			 },
			 {
			   if (IsLST(X0))
			     {
			       s = RemoveTag(X0,LST);
			       Pre_Execute_Read_Instr(1);
			     } else
			       goto fail;
			 });
      }

#else  /* not NUMA */

      /************************
       * GET_X_VALUE
       ************************/
    case GET_X_VALUE:
    get_x_value: 
      {
	register indx i,n;
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction);
	i = Get_Index_I(2,instruction);
	
	Unify(X(n),X(i));

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GET_Y_VALUE
       ************************/
    case GET_Y_VALUE:
    get_y_value:
      {
	register indx i,n;
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction); 
	i = Get_Index_I(2,instruction);
	
	Unify(Y(n),X(i));

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GET_CONSTANT
       ************************/
    case GET_CONSTANT:
    get_constant:
      {
	register TAGGED c, Xi;
	register BOOL equal;
	register indx i;
	Pre_FetchTop(2);
	
	c = Get_Tagged(pc); 
	i = Get_Index_I(1,instruction);
	
	/* Unify1(X(i),c); unfolding yields */
	Xi = X(i);

	DerefLockSwitch (Xi,
			 {
			   Bind(Xi,c,{goto fail;});
			 },
			 {
			   if (IsBIG(Xi) && IsBIG(c))
			     equal = bignum_eq(GetBignum(Xi),GetBignum(c));
			   else
			     equal = Xi == c ? TRUE : FALSE;

			   if (not(equal))
			     goto fail;
			 });
	
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GET_NIL
       ************************/
    case GET_NIL:
    get_nil:
      {
	register indx i;
	register TAGGED Xi;
	Pre_FetchTop(1);
	
	i = Get_Index_I(1,instruction);
	
	Xi = X(i);

	DerefLockSwitch (Xi,
			 {
			   Bind(Xi,atom_nil,{goto fail;});
			 },
			 {
			   if (Xi != atom_nil) 
			     goto fail;
			 });
	
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GET_STRUCTURE
       ************************/
    case GET_STRUCTURE:
    get_structure:
      {
	register TAGGED Xi,new, f;
	Pre_FetchTop(2);
	
	f = Get_Functor(pc);
	
	Xi = X(Get_Index_I(1,instruction));

	DerefLockSwitch (Xi,
			 {
			   Make_STR(H,new,f);
			   Bind(Xi,new,
				{
				  Error("get_structure - unsafe");
				  goto fail;
				});
			   Pre_Execute_Write_Instr(1);
			 },
			 {
			   if (IsSTR(Xi))
			     {
			       if (GetFunctor(Xi) == f)
				 {
				   s = GetArg(Xi,0);
				   Pre_Execute_Read_Instr(1);
				 } else
				   goto fail;
			     } else
			       goto fail;
			 });
      }
      
      /************************
       * GET_LIST
       ************************/
    case GET_LIST:
    get_list:
      {
	register TAGGED Xi;
	register indx i;      
	Pre_FetchTop(1);
	
	i = Get_Index_I(1,instruction);
	
	Xi = X(i);

	DerefLockSwitch (Xi,
			 {
			   register TAGGED l;
			  
			   Make_LST(H,l);
			   Bind(Xi,l,
				{
				  Error("get_list - unsafe");
				  goto fail;
				});
			   Pre_Execute_Write_Instr(1);
			 },
			 {
			   if (IsLST(Xi))
			     {
			       s = RemoveTag(Xi,LST);
			       Pre_Execute_Read_Instr(1);
			     } else
			       goto fail;
			 });
      }
      
      /************************
       * GET_CONSTANT_X0
       ************************/
    case GET_CONSTANT_X0:
    get_constant_x0:
      {
	register TAGGED c,X0;
	register BOOL equal;
	Pre_FetchTop(1);
	
	c = Get_Tagged(pc);
	X0 = X(0);      

	DerefLockSwitch (X0,
			 {
			   Bind(X0,c,{goto fail;});
			 },
			 {
			   if (IsBIG(X0) && IsBIG(c))
			     equal = bignum_eq(GetBignum(X0),GetBignum(c));
			   else
			     equal = X0 == c ? TRUE : FALSE;

			   if (not(equal))
			     goto fail;
			 });

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GET_NIL_X0
       ************************/
    case GET_NIL_X0:
    get_nil_x0:
      {
	TAGGED X0;
	Pre_FetchTop(0);
	
	X0 = X(0);

	DerefLockSwitch (X0,
			 {
			   Bind(X0,atom_nil,{goto fail;});
			 },
			 {
			   if (X0 != atom_nil) 
			     goto fail;
			 });

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GET_STRUCTURE_X0
       ************************/
    case GET_STRUCTURE_X0:
    get_structure_x0:
      {
	register TAGGED f,X0;
	Pre_FetchTop(1);
	
	f = Get_Functor(pc);
	
	X0 = X(0);

	DerefLockSwitch (X0,
			 {
			   register TAGGED new;

			   Make_STR(H,new,f);
			   Bind(X0,new,
				{
				  Error("get_structure_x0 - unsafe");
				  goto fail;
				});
			   Pre_Execute_Write_Instr(1);
			 },
			 {
			   if (GetFunctor(X0) != f)
			     goto fail;
			   s = GetArg(X0,0);
			   Pre_Execute_Read_Instr(1);
			 });
      }
      
      /************************
       * GET_LIST_X0
       ************************/
    case GET_LIST_X0:
    get_list_x0:
      {
	register TAGGED X0;
	Pre_FetchTop(0);
	
	X0 = X(0);

	DerefLockSwitch (X0,
			 {
			   register TAGGED l;
			  
			   Make_LST(H,l);
			   Bind(X0,l,
				{
				  Error("get_list_x0 - unsafe");
				  goto fail;
				});
			   Pre_Execute_Write_Instr(1);
			 },
			 {
			   if (IsLST(X0))
			     {
			       s = RemoveTag(X0,LST);
			       Pre_Execute_Read_Instr(1);
			     } else
			       goto fail;
			 });
      }
#endif /* NUMA */      

      /************************
       * PUT_X_VOID
       ************************/
    case PUT_X_VOID:
    put_x_void:
      {
	register indx i;
	Pre_FetchTop(1);
	
	i = Get_Index_I(1,instruction);
	
	LoadHVA(H,X(i),w);
	
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * PUT_Y_VOID
       ************************/
    case PUT_Y_VOID:
    put_y_void:
      {
	register indx i;      
	Pre_FetchTop(1);
	
	i = Get_Index_I(1,instruction);
	
	LoadSVA(Y(i),w);

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * PUT_X_VARIABLE
       ************************/
    case PUT_X_VARIABLE:
    put_x_variable:
      {
	register indx i,n;      
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction);
	i = Get_Index_I(2,instruction);
	
	LoadHVA(H,X(n),w);

	X(i) = X(n);

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * PUT_Y_VARIABLE
       ************************/
    case PUT_Y_VARIABLE:
    put_y_variable: 
      {
	register indx i,n;      
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction);
	i = Get_Index_I(2,instruction);
	
	LoadSVA(Y(n),w);

	X(i) = Y(n);

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * PUT_X_VALUE
       ************************/
    case PUT_X_VALUE:
    put_x_value:
      {
	register indx i,n;      
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction);
	i = Get_Index_I(2,instruction);
	
	X(i) = X(n);

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * PUT_Y_VALUE
       ************************/
    case PUT_Y_VALUE:
    put_y_value:
      {
	register indx i,n;      
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction);
	i = Get_Index_I(2,instruction);
	
	X(i) = Y(n);

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * PUT_X_UNSAFE_VALUE
       ************************/
    case PUT_X_UNSAFE_VALUE:
    put_x_unsafe_value:
      {
	register indx i,n;      
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction);
	i = Get_Index_I(2,instruction);
	
	RefStackUnsafe(H,X(i),X(n));

	X(n) = X(i);

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * PUT_Y_UNSAFE_VALUE
       ************************/
    case PUT_Y_UNSAFE_VALUE:
    put_y_unsafe_value:
      {
	register indx i,n;
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction);
	i = Get_Index_I(2,instruction);
	
	RefStackUnsafe(H,X(i),Y(n));
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * PUT_CONSTANT
       ************************/
    case PUT_CONSTANT:
    put_constant: 
      {
	register indx i;
	register TAGGED c;
	Pre_FetchTop(2);
	
	c = Get_Tagged(pc);
	i = Get_Index_I(1,instruction);
	
	X(i) = c;

        Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * PUT_NIL
       ************************/
    case PUT_NIL:
    put_nil:
      {
	register indx i;
	Pre_FetchTop(1);
	i = Get_Index_I(1,instruction);
	
	X(i) = atom_nil;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * PUT_STRUCTURE
       ************************/
    case PUT_STRUCTURE:
    put_structure:
      {
	register TAGGED f;
	register indx i;
	Pre_FetchTop(2);
	
	f = Get_Functor(pc);
	i = Get_Index_I(1,instruction);
	
	Make_STR(H,X(i),f);

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * PUT_LIST
       ************************/
    case PUT_LIST:
    put_list:
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);
	
	Make_LST(H,X(n));

	Pre_Execute_Write_Instr(1);
      }    
      
      /************************
       * UNIFY_VOID
       ************************/
    case UNIFY_VOID:
    unify_void:
      WriteModeDispatch(unify_void_write);
      {
	Pre_FetchTop(1);

	s += Get_Index_I(1,instruction) * VARSIZE;
	
	Pre_Execute_Read_Instr(1);
      }    
      
      /************************
       * UNIFY_X_VARIABLE
       ************************/
    case UNIFY_X_VARIABLE: 
    unify_x_variable:
      WriteModeDispatch(unify_x_variable_write);
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);
	
#if defined(UNBOUND)
	X(n) = DRef(s);
#else
	X(n) = Ref(s);
#endif 
	s += VARSIZE;
	Pre_Execute_Read_Instr(1);
      }    

      /************************
       * UNIFY_XVAR_XVAR
       ************************/
    case UNIFY_XVAR_XVAR: 
    unify_xvar_xvar:
      WriteModeDispatch(unify_xvar_xvar_write);
      {
	register indx n,m;
	Pre_FetchTop(2);

	n = Get_Index_I(1,instruction);
	m = Get_Index_I(2,instruction);
	
#if defined(UNBOUND)
	X(n) = DRef(s);
#else
	X(n) = Ref(s);
#endif 

#if defined(UNBOUND)
	X(m) = DRef((s+VARSIZE));
#else
	X(m) = Ref((s+VARSIZE));
#endif 

	s += 2*VARSIZE;
	Pre_Execute_Read_Instr(1);
      }    
      
      /************************
       * UNIFY_Y_VARIABLE
       ************************/
    case UNIFY_Y_VARIABLE:
    unify_y_variable:
      WriteModeDispatch(unify_y_variable_write);
      {
	register indx i;
	Pre_FetchTop(1);

	i = Get_Index_I(1,instruction);
/*******************************************************************************
#if defined(TRAIL_ALL)
	PushOnTrail(w->trail_top, Y(i));
#else
	if ((TAGGED *)yreg < (TAGGED *)w->choice)
	  {
	    PushOnTrail(w->trail_top, Y(i));
	  }
#endif
*******************************************************************************/
#if defined(UNBOUND)
	Y(i) = DRef(s);
#else
	Y(i) = Ref(s);
#endif
	s += VARSIZE;
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * UNIFY_Y_FIRST_VALUE
       ************************/
    case UNIFY_Y_FIRST_VALUE:
    unify_y_first_value:
      WriteModeDispatch(unify_y_first_value_write);
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);
	
	Bind_SVA(Tagify(&(Y(n)),SVA),Ref(s));
	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }
      
#if defined(NUMA)

      /************************
       * UNIFY_X_VALUE
       ************************/
    case UNIFY_X_VALUE:
    unify_x_value:
      WriteModeDispatch(unify_x_value_write);
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);
	
	Local_Unify(X(n),Ref(s));

	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * UNIFY_Y_VALUE
       ************************/
    case UNIFY_Y_VALUE:
    unify_y_value:
      WriteModeDispatch(unify_y_value_write);
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);
	
	Local_Unify(Y(n),Ref(s));

	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * UNIFY_X_LOCAL_VALUE
       ************************/
    case UNIFY_X_LOCAL_VALUE:
    unify_x_local_value:
      WriteModeDispatch(unify_x_local_value_write);
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);

	GlobalizeRegisterX(X(n),H);

	Local_Unify(X(n),Ref(s));

	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * UNIFY_Y_LOCAL_VALUE
       ************************/
    case UNIFY_Y_LOCAL_VALUE:
    unify_y_local_value:
      WriteModeDispatch(unify_y_local_value_write);
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);
	
	GlobalizeRegisterY(Y(n),H);

	Local_Unify(Y(n),Ref(s));

	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }    
      
      /************************
       * UNIFY_CONSTANT
       ************************/
    case UNIFY_CONSTANT:
    unify_constant:
      WriteModeDispatch(unify_constant_write);
      { 
	register TAGGED c, Si;
	register BOOL equal;
	Pre_FetchTop(1);
	
	c = Get_Tagged(pc);
	
	Si = Ref(s);
	s += VARSIZE;
	DerefLockSwitchHVA(Si,
			   {
			     Local_Bind_Heap(Si,c);
			   },
			   {
			     if (IsBIG(Si) && IsBIG(c))
			       equal = bignum_eq(GetBignum(Si),GetBignum(c));
			     else
			       equal = Si == c ? TRUE : FALSE;

			     if (not(equal))
			       goto fail;
			   });
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * UNIFY_NIL
       ************************/
    case UNIFY_NIL:
    unify_nil:
      WriteModeDispatch(unify_nil_write);
      {
	register TAGGED Si;
	Pre_FetchTop(0);
	
	Si = Ref(s);
	s += VARSIZE;
	DerefLockSwitchHVA(Si,
			   {
			     Local_Bind_Heap(Si,atom_nil);
			   },
			   {
			     if (Si != atom_nil)
			       goto fail;
			   });
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * UNIFY_STRUCTURE
       ************************/
    case UNIFY_STRUCTURE:
    unify_structure:
      WriteModeDispatch(unify_structure_write);
      {
	register TAGGED f,Ds,str;
	Pre_FetchTop(1);
	
	f = Get_Functor(pc);
	
	Ds = Ref(s);
	DerefLockSwitchHVA(Ds,
			   {
			     Make_STR(H,str,f);
			     Local_Bind(Ds,str);
			     Pre_Execute_Write_Instr(1);
			   },
			   {
			     if (IsSTR(Ds))
			       {
				 if (GetFunctor(Ds) == f)
				   {
				     s = GetArg(Ds,0);
				     Pre_Execute_Read_Instr(1);
				   }
			       }
			   });
	goto fail;
      }
      
      /************************
       * UNIFY_LIST
       ************************/
    case UNIFY_LIST:
    unify_list:
      WriteModeDispatch(unify_list_write);
      { 
	register TAGGED Ds, lst;
	Pre_FetchTop(0);
	
	Ds = Ref(s);
	DerefLockSwitchHVA(Ds,
			   {
			     Make_LST(H,lst);
			     Local_Bind(Ds,lst);
			     Pre_Execute_Write_Instr(1);
			   },
			   {
			     if (IsLST(Ds))
			       {
				 s = GetCar(Ds);
				 Pre_Execute_Read_Instr(1);
			       } else
				 goto fail;
			   });
      }
#else  /* not NUMA */

      /************************
       * UNIFY_X_VALUE
       ************************/
    case UNIFY_X_VALUE:
    unify_x_value:
      WriteModeDispatch(unify_x_value_write);
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);
	
	Unify(X(n),Ref(s));

	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * UNIFY_Y_VALUE
       ************************/
    case UNIFY_Y_VALUE:
    unify_y_value:
      WriteModeDispatch(unify_y_value_write);
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);
	
	Unify(Y(n),Ref(s));

	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * UNIFY_X_LOCAL_VALUE
       ************************/
    case UNIFY_X_LOCAL_VALUE:
    unify_x_local_value:
      WriteModeDispatch(unify_x_local_value_write);
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);

	GlobalizeRegisterX(X(n),H);

	Unify(X(n),Ref(s));

	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * UNIFY_Y_LOCAL_VALUE
       ************************/
    case UNIFY_Y_LOCAL_VALUE:
    unify_y_local_value:
      WriteModeDispatch(unify_y_local_value_write);
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);
	
	GlobalizeRegisterY(Y(n),H);

	Unify(Y(n),Ref(s));

	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }    
      
      /************************
       * UNIFY_CONSTANT
       ************************/
    case UNIFY_CONSTANT:
    unify_constant:
      WriteModeDispatch(unify_constant_write);
      { 
	register TAGGED c, Si;
	register BOOL equal;
	Pre_FetchTop(1);
	
	c = Get_Tagged(pc);
	
	Si = Ref(s);
	s += VARSIZE;
	DerefLockSwitchHVA(Si,
			   {
			     Bind_Heap(Si,c,{goto fail;});
			   },
			   {
			     if (IsBIG(Si) && IsBIG(c))
			       equal = bignum_eq(GetBignum(Si),GetBignum(c));
			     else
			       equal = Si == c ? TRUE : FALSE;

			     if (not(equal))
			       goto fail;
			   });
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * UNIFY_NIL
       ************************/
    case UNIFY_NIL:
    unify_nil:
      WriteModeDispatch(unify_nil_write);
      {
	register TAGGED Si;
	Pre_FetchTop(0);
	
	Si = Ref(s);
	s += VARSIZE;
	DerefLockSwitchHVA(Si,
			   {
			     Bind_Heap(Si,atom_nil,{goto fail;});
			   },
			   {
			     if (Si != atom_nil)
			       goto fail;
			   });

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * UNIFY_STRUCTURE
       ************************/
    case UNIFY_STRUCTURE:
    unify_structure:
      WriteModeDispatch(unify_structure_write);
      {
	register TAGGED f,Ds,str;
	Pre_FetchTop(1);
	
	f = Get_Functor(pc);
	
	Ds = Ref(s);
	DerefLockSwitchHVA(Ds,
			   {
			     Make_STR(H,str,f);
			     Bind(Ds,str,
				  {
				    Error("unify_structure - unsafe");
				    goto fail;
				  });
			     Pre_Execute_Write_Instr(1);
			   },
			   {
			     if (IsSTR(Ds))
			       {
				 if (GetFunctor(Ds) == f)
				   {
				     s = GetArg(Ds,0);
				     Pre_Execute_Read_Instr(1);
				   }
			       }
			   });
	goto fail;
      }
      
      /************************
       * UNIFY_LIST
       ************************/
    case UNIFY_LIST:
    unify_list:
      WriteModeDispatch(unify_list_write);
      { 
	register TAGGED Ds, lst;
	Pre_FetchTop(0);
	
	Ds = Ref(s);
	DerefLockSwitchHVA(Ds,
			   {
			     Make_LST(H,lst);
			     Bind(Ds,lst,
				  {
				    Error("unify_list - unsafe");
				    goto fail;
				  });
			     Pre_Execute_Write_Instr(1);
			   },
			   {
			     if (IsLST(Ds))
			       {
				 s = GetCar(Ds);
				 Pre_Execute_Read_Instr(1);
			       } else
				 goto fail;
			   });
      }

#endif /* NUMA */
      
#if defined(JUMP_CALL)
      /************************
       * CJUMP
       ************************/
    case CJUMP:
    cjump:
      {
	CallStatistics;
	Get_Index_I_M(1,instruction);
	w->next_instr = pc+1;
	w->call_choice = w->choice;
	pc = Get_PC_No_Inc(pc);

	Execute_Read_Instr;
      }

      /************************
       * EJUMP
       ************************/
    case EJUMP:
    ejump:
      {
	CallStatistics;
	pc = Get_PC_No_Inc(pc);
	w->call_choice = w->choice;

	Execute_Read_Instr;
      }
#endif /* JUMP_CALL */

#if defined(BOUNDED_Q)
      /************************
       * ZEROP
       ************************/
    case ZEROP:
    zerop:
      {
	register indx i;
	register int n;
	Pre_FetchTop(2);
	
	i = Get_Index_I(1,instruction);
	
	DerefNLL(X(i),X(i));
	
	n = GetNumber(X(i));
	
	if (n < 0) goto fail;
	
	if (n == 0)
	  {
	    pc = DispatchLabel(pc,0);
	    Execute_Read_Instr;
	  }
	else
	  {
	    Inc_Label(pc);
	    Pre_Execute_Read_Instr(1);
	  }
      }
      
      /************************
       * LISTP
       ************************/
    case LISTP:
    listp:
      {
	register indx i;
	Pre_FetchTop(2);
	
	i = Get_Index_I(1,instruction);
	
	DerefNLL(X(i),X(i));
	
	if (!IsLST(X(i)))
	  {
	    pc = DispatchLabel(pc,0);
	    Execute_Read_Instr;
	  }
	else
	  {
	    Inc_Label(pc);
	    Pre_Execute_Read_Instr(1);
	  }
      }
      
      /************************
       * DETERMINISTIC
       ************************/
    case DETERMINISTIC:
    deterministic:
      {
	Pre_FetchTop(1);

	if (((void *) w->frame) >= ((void *) w->choice))
	  {
	    pc = DispatchLabel(pc,0);
	    Execute_Read_Instr;
	  }
	else
	  {
	    Inc_Label(pc);
	    Pre_Execute_Read_Instr(1);
	  }
      }
      
      /************************
       * ALLOCATE_STAR
       ************************/
    case ALLOCATE_STAR:
    allocate_star: 
      {
	register environment *newframe;
	Pre_FetchTop(1);
	
	newframe = (environment *) Get_Stack_Top(w,Get_Index_I(1,instruction));
	
	newframe->cont_env = w->frame;
	newframe->next_instr = w->next_instr;
	SetY(newframe->yreg);
	w->frame = newframe;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * REPEAT
       ************************/
    case REPEAT:
    repeat:
      {
	/*   This instruction should copy the environment if the clause
	 * is nondeterministic this far, when done it should jump to the
	 * label.
	 */
	register int size;
	Pre_FetchInit;

	size = FrameSize(w->next_instr);

	pc = DispatchLabel(pc,0);
	Pre_Fetch;

	if (((void *) w->frame) < ((void *) w->choice))
	  {
	    register environment *newframe;
	  
	    newframe = (environment *) Get_Local_Stack_Top;

	    if (newframe > (environment *) w->stack_margin)
	      {
		w->gc_info.arity = 0; /* ALERT: not fully supported */
		w->gc_info.env_size = FrameSize(w->next_instr);

		shift_stack(w, (size_t)newchoice - (size_t)w->stack_margin);

		newframe = (environment *) Get_Local_Stack_Top;
	      }

	    newframe->cont_env = w->frame->cont_env;
	    newframe->next_instr = w->frame->next_instr;
	  
	    size--;
	    while (size--)
	      {
		newframe->yreg[size] = X(size) = Y(size);
	      }
	    
	    SetY(newframe->yreg);
	    w->frame = newframe;
	  }
	else
	  {
	    size--;
	    while (size--)
	      {
		X(size) = Y(size);
	      }
	  }

	Pre_Execute_Read_Instr(0);
      } 
      
      /************************
       * ITERATE_INT
       ************************/
    case ITERATE_INT:
    iterate_int:
      {
	register indx m,n;
	
	m = Get_Index_I_M(1,instruction);
	n = Get_Index_I(2,instruction);
	
	X(m) = Make_Integer(GetNumber(X(m))+1);
	
	if (GetNumber(X(m)) < GetNumber(X(n)))
	  {
	    pc = DispatchLabel(pc,0);
	  }
	else
	  {
	    Inc_Label(pc);
	  }

	Execute_Read_Instr;
      }

      /************************
       * ITERATE_INT_STAR
       ************************/
    case ITERATE_INT_STAR:
    iterate_int_star:
      {
	register indx m,n;
	
	m = Get_Index_I_M(1,instruction);
	n = Get_Index_I(2,instruction);
	
	X(m) = Make_Integer(GetNumber(X(m))+1);
	
	if (GetNumber(X(m)) >= GetNumber(X(n)))
	  {
	    pc = DispatchLabel(pc,0);
	  }
	else
	  {
	    Inc_Label(pc);
	  }

	Execute_Read_Instr;
      }
      
      /************************
       * ITERATE_LIST
       ************************/
    case ITERATE_LIST:
    iterate_list:
      {
	register indx i;
	register TAGGED Xi;
	
	i = Get_Index_I(1,instruction);

	DerefNLL(Xi,X(i));

	if (IsLST(Xi))
	  {
	    pc = DispatchLabel(pc,0);
	  }
	else
	  {
	    Inc_Label(pc);
	  }

	Execute_Read_Instr;
      }
      
      /************************
       * ITERATE_LIST_STAR
       ************************/
    case ITERATE_LIST_STAR:
    iterate_list_star:
      {
	register indx i;
	register TAGGED Xi;
	
	i = Get_Index_I(1,instruction);

	DerefNLL(Xi,X(i));

	if (!IsLST(Xi))
	  {
	    pc = DispatchLabel(pc,0);
	  }
	else
	  {
	    Inc_Label(pc);
	  }

	Execute_Read_Instr;
      }
      
      /************************
       * ENSURE_LIST_TRY
       ************************/
    case ENSURE_LIST_TRY:
    ensure_list_try:
      {
	register indx i;
	register TAGGED Xi;
	
	i = Get_Index_I_M(1,instruction);

	DerefNLL(Xi,X(i));
	
	if (IsVar(Xi)) /* create choice-point */
	  {
	    register choicepoint *newchoice;
	  
	    newchoice = (choicepoint *)
	      Get_Stack_Top(w,Get_Index_I(2,instruction));
	  
	    newchoice->trail_top = w->trail_top;
	    newchoice->global_top = H;
	    newchoice->last_choice = w->choice;
	    newchoice->cont_env = w->frame;
	    newchoice->next_instr = w->next_instr;
	    newchoice->next_clause = pc+1;
	    newchoice->arity = 1;
	    newchoice->areg[0] = Xi;
#if defined(TIMESTAMP) || defined(UNBOUND)
	    newchoice->timestamp = w->time;
	    w->time += TIMEUNIT;
	    w->uncond = w->time;
#else
	    w->uncond = H;
#endif
	    w->choice = newchoice;
	    AlwaysTrail(Xi);
	    AlwaysBind(Xi,atom_nil);
	  }
	
	X(0) = atom_nil;
	pc = DispatchLabel(pc,0);

	Execute_Read_Instr;
      }
      
      /************************
       * ENSURE_LIST_TRUST
       ************************/
    case ENSURE_LIST_TRUST:
    ensure_list_trust:
      {
	register TAGGED lst;
	
	X(0) = w->choice->areg[0];
	w->choice = w->choice->last_choice;
#if defined(TIMESTAMP) || defined(UNBOUND)
	w->uncond = w->choice->timestamp+TIMEUNIT;
#else
	w->uncond = w->choice->global_top;
#endif /* TIMESTAMP */
	
	Make_LST(H,lst);
	CreateHVA(H,w);
	CreateHVA(H,w);
	
	Bind(X(0),lst,{goto fail;});
	
	X(0) = lst;
	
	Execute_Read_Instr;
      }
      
      
#endif /* BOUNDED_Q */

#if defined(PARALLEL_BQ)
      /************************
       * SPAWN_LEFT_BQ
       ************************/
    case SPAWN_LEFT_BQ:
    spawn_left_bq:
      {
	register indx step, i, g;
	register s32 level;
	Pre_FetchTop(3);

#include "global_event.c"

	step = Get_Index_I_M(1,instruction);
	i = Get_Index_I_M(2,instruction);
	g = Get_Index_I_M(3,instruction);
      
	if (w->global->scheduling == STATIC)
	  {
	    level = w->global->level[w->pid-1] + 
	      step*w->global->active_workers;
	  }
	else
	  {
	    GrabLevel(level,w) 
	      w->global->sched_level = level + step;
	  }
	    

	w->global->level[w->pid-1] = level;
	X(i) = Make_Integer(level);

	if (level >= GetNumber(G(g)))
	  {
	    pc = DispatchLabel(pc,0);
	    Execute_Read_Instr;
	  }

	Inc_Label(pc);

	Pre_Execute_Read_Instr(1);
      }
#endif /* PARALLEL_BQ */

#if defined(JUMP_CODE)
      /************************
       * JUMP
       ************************/
    case JUMP:
    jump:
      {
	pc = DispatchLabel(pc,0);
	Execute_Read_Instr;
      }
#endif /* BOUNDED_Q || REFORM */

#if defined(REFORM)
      /************************
       * GLOBAL_GET_X_VALUE
       ************************/
    case GLOBAL_GET_X_VALUE:
    global_get_x_value: 
      {
	register indx i,n;
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction);
	i = Get_Index_I(2,instruction);
	
	Unify(X(n),X(i));
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GLOBAL_GET_Y_VALUE
       ************************/
    case GLOBAL_GET_Y_VALUE:
    global_get_y_value:
      {
	register indx i,n;
	Pre_FetchTop(2);
	
	n = Get_Index_I_M(1,instruction); 
	i = Get_Index_I(2,instruction);
	
	Unify(Y(n),X(i));

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GLOBAL_GET_CONSTANT
       ************************/
    case GLOBAL_GET_CONSTANT:
    global_get_constant:
      {
	register TAGGED c, Xi;
	register BOOL equal;
	register indx i;
	Pre_FetchTop(2);
	
	c = Get_Tagged(pc); 
	i = Get_Index_I(1,instruction);
	
	/* Unify1(X(i),c); unfolding yields */
	Xi = X(i);
	DerefLockSwitch (Xi,
			 {
			   Bind(Xi,c,{goto fail;});
			 },
			 {
			   if (IsBIG(Xi) && IsBIG(c))
			     equal = bignum_eq(GetBignum(Xi),GetBignum(c));
			   else
			     equal = Xi == c ? TRUE : FALSE;

			   if (not(equal))
			     goto fail;
			 });
	
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GLOBAL_GET_NIL
       ************************/
    case GLOBAL_GET_NIL:
    global_get_nil:
      {
	register indx i;
	register TAGGED Xi;
	Pre_FetchTop(1);
	
	i = Get_Index_I(1,instruction);
	
	Xi = X(i);
	DerefLockSwitch (Xi,
			 {
			   Bind(Xi,atom_nil,{goto fail;});
			 },
			 {
			   if (Xi != atom_nil) 
			     goto fail;
			 });
	
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GLOBAL_GET_STRUCTURE
       ************************/
    case GLOBAL_GET_STRUCTURE:
    global_get_structure:
      {
	register TAGGED Xi,new, f;
	Pre_FetchTop(2);
	
	f = Get_Functor(pc);
	
	Xi = X(Get_Index_I(1,instruction));

	DerefLockSwitch (Xi,
			 {
			   Make_STR(H,new,f);
			   Bind(Xi,new,
				{
				  Error("get_structure - unsafe");
				  goto fail;
				});
			   Pre_Execute_Write_Instr(1);
			 },
			 {
			   if (IsSTR(Xi))
			     {
			       if (GetFunctor(Xi) == f)
				 {
				   s = GetArg(Xi,0);
				   Pre_Execute_Read_Instr(1);
				 } else
				   goto fail;
			     } else
			       goto fail;
			 });
      }
      
      /************************
       * GLOBAL_GET_LIST
       ************************/
    case GLOBAL_GET_LIST:
    global_get_list:
      {
	register TAGGED Xi;
	register indx i;      
	Pre_FetchTop(1);
	
	i = Get_Index_I(1,instruction);
	
	Xi = X(i);

	DerefLockSwitch (Xi,
			 {
			   register TAGGED l;
			  
			   Make_LST(H,l);
			   Bind(Xi,l,
				{
				  Error("get_list - unsafe");
				  goto fail;
				});
			   Pre_Execute_Write_Instr(1);
			 },
			 {
			   if (IsLST(Xi))
			     {
			       s = RemoveTag(Xi,LST);
			       Pre_Execute_Read_Instr(1);
			     } else
			       goto fail;
			 });
      }
      
      /************************
       * GLOBAL_GET_CONSTANT_X0
       ************************/
    case GLOBAL_GET_CONSTANT_X0:
    global_get_constant_x0:
      {
	register TAGGED c,X0;
	register BOOL equal;
	Pre_FetchTop(1);
	
	c = Get_Tagged(pc);
	X0 = X(0);      
	DerefLockSwitch (X0,
			 {
			   Bind(X0,c,{goto fail;});
			 },
			 {
			   if (IsBIG(X0) && IsBIG(c))
			     equal = bignum_eq(GetBignum(X0),GetBignum(c));
			   else
			     equal = X0 == c ? TRUE : FALSE;

			   if (not(equal))
			     goto fail;
			 });
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GLOBAL_GET_NIL_X0
       ************************/
    case GLOBAL_GET_NIL_X0:
    global_get_nil_x0:
      {
	TAGGED X0;
	Pre_FetchTop(0);
	
	X0 = X(0);

	DerefLockSwitch (X0,
			 {
			   Bind(X0,atom_nil,{goto fail;});
			 },
			 {
			   if (X0 != atom_nil) 
			     goto fail;
			 });

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GLOBAL_GET_STRUCTURE_X0
       ************************/
    case GLOBAL_GET_STRUCTURE_X0:
    global_get_structure_x0:
      {
	register TAGGED f,X0;
	Pre_FetchTop(1);
	
	f = Get_Functor(pc);
	
	X0 = X(0);

	DerefLockSwitch (X0,
			 {
			   register TAGGED new;

			   Make_STR(H,new,f);
			   Bind(X0,new,
				{
				  Error("get_structure_x0 - unsafe");
				  goto fail;
				});
			   Pre_Execute_Write_Instr(1);
			 },
			 {
			   if (GetFunctor(X0) != f)
			     goto fail;
			   s = GetArg(X0,0);
			   Pre_Execute_Read_Instr(1);
			 });
      }
      
      /************************
       * GLOBAL_GET_LIST_X0
       ************************/
    case GLOBAL_GET_LIST_X0:
    global_get_list_x0:
      {
	register TAGGED X0;
	Pre_FetchTop(0);
	
	X0 = X(0);

	DerefLockSwitch (X0,
			 {
			   register TAGGED l;
			  
			   Make_LST(H,l);
			   Bind(X0,l,
				{
				  Error("get_list_x0 - unsafe");
				  goto fail;
				});
			   Pre_Execute_Write_Instr(1);
			 },
			 {
			   if (IsLST(X0))
			     {
			       s = RemoveTag(X0,LST);
			       Pre_Execute_Read_Instr(1);
			     } else
			       goto fail;
			 });
      }
      
      /************************
       * GLOBAL_UNIFY_X_VALUE
       ************************/
    case GLOBAL_UNIFY_X_VALUE:
    global_unify_x_value:
      WriteModeDispatch(global_unify_x_value_write);
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);
	
	Unify(X(n),Ref(s));

	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GLOBAL_UNIFY_Y_VALUE
       ************************/
    case GLOBAL_UNIFY_Y_VALUE:
    global_unify_y_value:
      WriteModeDispatch(global_unify_y_value_write);
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);
	
	Unify(Y(n),Ref(s));

	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GLOBAL_UNIFY_X_LOCAL_VALUE
       ************************/
    case GLOBAL_UNIFY_X_LOCAL_VALUE:
    global_unify_x_local_value:
      WriteModeDispatch(global_unify_x_local_value_write);
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);

	GlobalizeRegisterX(X(n),H);

	Unify(X(n),Ref(s));

	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GLOBAL_UNIFY_Y_LOCAL_VALUE
       ************************/
    case GLOBAL_UNIFY_Y_LOCAL_VALUE:
    global_unify_y_local_value:
      WriteModeDispatch(global_unify_y_local_value_write);
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);
	
	GlobalizeRegisterY(Y(n),H);

	Unify(Y(n),Ref(s));

	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }    
      
      /************************
       * GLOBAL_UNIFY_CONSTANT
       ************************/
    case GLOBAL_UNIFY_CONSTANT:
    global_unify_constant:
      WriteModeDispatch(global_unify_constant_write);
      { 
	register TAGGED c, Si;
	register BOOL equal;
	Pre_FetchTop(1);
	
	c = Get_Tagged(pc);
	
	Si = Ref(s);
	s += VARSIZE;

	DerefLockSwitchHVA(Si,
			   {
			     Bind_Heap(Si,c,{goto fail;});
			   },
			   {
			     if (IsBIG(Si) && IsBIG(c))
			       equal = bignum_eq(GetBignum(Si),GetBignum(c));
			     else
			       equal = Si == c ? TRUE : FALSE;

			     if (not(equal))
			       goto fail;
			   });

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GLOBAL_UNIFY_NIL
       ************************/
    case GLOBAL_UNIFY_NIL:
    global_unify_nil:
      WriteModeDispatch(global_unify_nil_write);
      {
	register TAGGED Si;
	Pre_FetchTop(0);
	
	Si = Ref(s);
	s += VARSIZE;

	DerefLockSwitchHVA(Si,
			   {
			     Bind_Heap(Si,atom_nil,{goto fail;});
			   },
			   {
			     if (Si != atom_nil)
			       goto fail;
			   });

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * GLOBAL_UNIFY_STRUCTURE
       ************************/
    case GLOBAL_UNIFY_STRUCTURE:
    global_unify_structure:
      WriteModeDispatch(global_unify_structure_write);
      {
	register TAGGED f,Ds,str;
	Pre_FetchTop(1);
	
	f = Get_Functor(pc);
	
	Ds = Ref(s);

	DerefLockSwitchHVA(Ds,
			   {
			     Make_STR(H,str,f);
			     Bind(Ds,str,
				  {
				    Error("global_unify_structure - unsafe");
				    goto fail;
				  });
			     Pre_Execute_Write_Instr(1);
			   },
			   {
			     if (IsSTR(Ds))
			       {
				 if (GetFunctor(Ds) == f)
				   {
				     s = GetArg(Ds,0);
				     Pre_Execute_Read_Instr(1);
				   }
			       }
			   });
	goto fail;
      }
      
      /************************
       * GLOBAL_UNIFY_LIST
       ************************/
    case GLOBAL_UNIFY_LIST:
    global_unify_list:
      WriteModeDispatch(global_unify_list_write);
      { 
	register TAGGED Ds, lst;
	Pre_FetchTop(0);
	
	Ds = Ref(s);

	DerefLockSwitchHVA(Ds,
			   {
			     Make_LST(H,lst);
			     Bind(Ds,lst,
				  {
				    Error("global_unify_list - unsafe");
				    goto fail;
				  });
			     Pre_Execute_Write_Instr(1);
			   },
			   {
			     if (IsLST(Ds))
			       {
				 s = GetCar(Ds);
				 Pre_Execute_Read_Instr(1);
			       } else
				 goto fail;
			   });
      }

      /************************
       * BUILD_REC_POSLIST
       ************************/
    case BUILD_REC_POSLIST:
    build_rec_poslist:
      {
	register indx i, n, v, t;
	Pre_FetchTop(4);

	i = Get_Index_I_M(1,instruction); /* recursion list */
	n = Get_Index_I_M(2,instruction); /* length of recursion list */
	v = Get_Index_I_M(3,instruction); /* vectorized recursion list */
	instruction = Get_Instruction(pc);
	t = Get_Index_I(1,instruction); /* last tail of recursion list */

	/* Traverse recursion list and build vector.
	 */
	Make_Vector_Head(H,X(v),0); /* dummysize */

	/* Make_LST(H,X(v)); */

	/* Build vector. */
	{
	  register TAGGED List;
	  register s32 len;

	  DerefNLL(List, X(i));

	  PushOnHeap(H,Ref(GetCar(List)));
	  DerefNLL(List,Ref(GetCdr(List)));
	  
	  for (len = 1; IsLST(List); len++)
	    {
	      PushOnHeap(H,Tagify(H+VARSIZE,LST));
	      PushOnHeap(H,Ref(GetCar(List)));
	      DerefNLL(List,Ref(GetCdr(List)));
	    }

	  PushOnHeap(H,List);

	  /* Last tail.
	   */
	  X(t) = List;
	  
	  /* Vector length.
	   */
	  X(n) = Make_Integer(len);
	  Patch_Vector_Size(X(v),len);
	}

	/* This check could be refined to check if allocating a new
	 * segment solves the problem.
	 */

	if (H > w->heap_margin)
	  {
	    Error("heap out of memory - failing");
	    goto fail;
	  }

	Pre_Execute_Read_Instr(1);
      }
	    
	

      /************************
       * BUILD_POSLIST
       ************************/
    case BUILD_POSLIST:
    build_poslist:
      {
	register indx i, n, v, t;
	Pre_FetchTop(4);

	i = Get_Index_I_M(1,instruction); /* initial list */
	n = Get_Index_I_M(2,instruction); /* length */
	v = Get_Index_I_M(3,instruction); /* vector */
	instruction = Get_Instruction(pc);
	t = Get_Index_I(1,instruction);	/* last tail */

	/* Traverse recursion list and build vector.
	 */
	/* Make_LST(H,X(v)); */

	/* Build vector. */
	{
	  register TAGGED List;
	  register s32 len, vectorsize;

	  vectorsize = GetNumber(X(n));

	  HeapCheck(vectorsize*VARSIZE*2, goto fail);

	  Make_Vector_Head(H,X(v),vectorsize); /* dummysize */

	  DerefNLL(List, X(i));

	  for (len = 0; IsLST(List) && len < (vectorsize - 1); len++)
	    {
	      PushOnHeap(H,Ref(GetCar(List)));
	      PushOnHeap(H,Tagify(H+VARSIZE,LST));
	      DerefNLL(List,Ref(GetCdr(List)));
	    }

	  if (IsLST(List))
	    {
	      PushOnHeap(H,Ref(GetCar(List)));
	      X(t) = Ref(GetCdr(List));
	      PushOnHeap(H,X(t));
	    }
	  else
	    {
	      if (!unify(List,Tagify(H,LST),w))
		goto fail;

	      while (len < (vectorsize - 1))
		{
		  CreateHVA(H,w);
		  PushOnHeap(H,Tagify(H+VARSIZE,LST));
		  len++;
		}
	      CreateHVA(H,w);
	      LoadHVA(H,X(t),w);
	    }
	}

	Pre_Execute_Read_Instr(1);
      }

      /************************
       * BUILD_POSLIST_VALUE
       ************************/
    case BUILD_POSLIST_VALUE:
    build_poslist_value:
      {
	register indx i, n, v, t;
	Pre_FetchTop(4);

	i = Get_Index_I_M(1,instruction); /* list */
	n = Get_Index_I_M(2,instruction); /* length */
	v = Get_Index_I_M(3,instruction); /* vector */
	instruction = Get_Instruction(pc);
	t = Get_Index_I(1,instruction);	/* last tail */

	/* Match list with vector. */
	{
	  TAGGED VectorList, List;
	  s32 vectorsize;
	  
	  DerefNLL(List,X(i));
	  DerefNLL(VectorList,Ref(GetNthHead(X(v),0)));
	  
	  vectorsize = GetNumber(X(n));
	  
	  HeapCheck(vectorsize*2*VARSIZE, goto fail);

	  while (vectorsize)
	    {
	      if (IsLST(List))
		{
		  Unify(Ref(GetCar(List)),Ref(GetCar(VectorList)));
		  DerefNLL(List,Ref(GetCdr(List)));
		  VectorList = Ref(GetCdr(VectorList));
		}
	      else 
		break;
	      vectorsize--;
	    }
	  
	  if (vectorsize)
	    {
	      register TAGGED Copy;
	    
	      Make_LST(H,Copy);
	      PushOnHeap(H,Ref(GetCar(VectorList)));
	      vectorsize -= 1;
	    
	      while (vectorsize--)
		{
		  PushOnHeap(H,Tagify(H+VARSIZE,LST));
		  VectorList = Ref(GetCdr(VectorList));
		  PushOnHeap(H,Ref(GetCar(VectorList)));
		}
	    
	      LoadHVA(H,X(t),w);
	      if (!unify(List,Copy,w))
		goto fail;
	    
	    }
	  else
	    {
	      X(t) = List;
	    }
	}

	Pre_Execute_Read_Instr(1);
      }

      /************************
       * BUILD_NEGLIST
       ************************/
    case BUILD_NEGLIST:
    build_neglist:
      {
	register indx i, n, v, t;
	register s32 vectorsize;
	register TAGGED *htop;
	Pre_FetchTop(4);

	i = Get_Index_I_M(1,instruction); /* last tail element */
	n = Get_Index_I_M(2,instruction); /* length of vector */
	v = Get_Index_I_M(3,instruction); /* head of vector */
	instruction = Get_Instruction(pc);
	t = Get_Index_I(1,instruction);	/* head of list */

	vectorsize = GetNumber(X(n)) - 1;

	HeapCheck(vectorsize*2*VARSIZE, goto fail);

	Make_Vector_Head(H,X(v),vectorsize);

	/* X(v) = Tagify(H,LST); */

	CreateHVA(H,w);
	PushOnHeap(H,X(i));

	while (vectorsize--)
	  {
	    CreateHVA(H,w);
	    PushOnHeap(H,Tagify(H-3*VARSIZE,LST));
	  }
	    
	X(t) = Tagify(H-2*VARSIZE,LST);
	
	Pre_Execute_Read_Instr(1);
      }

      /************************
       * BUILD_NEGLIST_VALUE
       ************************/
    case BUILD_NEGLIST_VALUE:
    build_neglist_value:
      {
	register indx m, n, v, t, i;
	register s32 vectorsize;
	register TAGGED *htop, *Vector;
	Pre_FetchTop(5);
	
	i = Get_Index_I_M(1,instruction); /* tail element */
	n = Get_Index_I_M(2,instruction); /* length       */
	m = Get_Index_I_M(3,instruction); /* match vector */
	instruction = Get_Instruction(pc);
	v = Get_Index_I_M(1,instruction); /* resulting vector */
	t = Get_Index_I(2,instruction);	/* start of list */


	vectorsize = GetNumber(X(n)) - 1;
	Vector = GetNthHead(X(m),0);

	HeapCheck(vectorsize*2*VARSIZE, goto fail);

	Make_Vector_Head(H,X(v),vectorsize);

	/* X(v) = Tagify(H,LST); */

	PushOnHeap(H,Ref(Vector));
	Vector += 2*VARSIZE;
	PushOnHeap(H,X(i));

	while (vectorsize--)
	  {
	    PushOnHeap(H,Ref(Vector));
	    Vector += 2*VARSIZE;
	    PushOnHeap(H,Tagify(H-3*VARSIZE,LST));
	  }
	    
	X(t) = Tagify(H-2*VARSIZE,LST);
	
	Pre_Execute_Read_Instr(1);
      }

      /************************
       * BUILD_VARIABLES
       ************************/
    case BUILD_VARIABLES:
    build_variables:
      {
	register indx i, n, v, t;
	register s32 vectorsize;
	register int x;
	Pre_FetchTop(4);

	i = Get_Index_I_M(1,instruction); /* first variable */
	n = Get_Index_I_M(2,instruction); /* length of recursion list */
	v = Get_Index_I_M(3,instruction); /* variable vector */
	instruction = Get_Instruction(pc);
	t = Get_Index_I(1,instruction);	/* head of last tail */

	vectorsize = GetNumber(X(n));
	
	HeapCheck(vectorsize*VARSIZE*2,goto fail);

	/* Pointer to the new vector
	 */

	Make_Vector_Head(H,X(v),vectorsize);

	/* Make_LST(H,X(v)); */

	/*
	 *   The parallel version should first allocate the space on the
	 * sequential workers heap, and then proceed to initialize the vector
	 * in parallel. In a native code setting this might be uneccessary if
	 * uninitialized variables are supported (there are some problemes with
	 * garbage collection however).
	 *
	 * The first variable is pushed by the sequential worker.
	 *
	 */
	{
	  register TAGGED varvar;
	  DerefNLL(varvar,X(i));
	  
	  if (IsSVA(varvar))
	    {
	      register TAGGED newvar;

	      LoadHVA(H,newvar,w);
	      Bind_SVA(varvar,newvar);
	    }
	  else
	    PushOnHeap(H,varvar);
	}

	PushOnHeap(H,Tagify(H+VARSIZE,LST));

	w->global->parallel_start.vector = H;
	w->global->parallel_start.size   = vectorsize-1;
	  
	H += (vectorsize-1) * VARSIZE * 2;

	Run_Parallel(W_BUILD_VARIABLES);

	/* Put the vector variable in register "t".
	 */
	LoadHVA(H,X(t),w);
	CreateHVA(H,w);

	Pre_Execute_Read_Instr(1);
      }

      
      /************************
       * PUT_NTH_HEAD
       ************************/
    case PUT_NTH_HEAD:
    put_nth_head:
      {
	register indx v, l, i;
	register s32 o;
	Pre_FetchTop(4);

	v = Get_Index_I_M(1,instruction);
	l = Get_Index_I_M(2,instruction);
	o = Get_Index(pc);
	i = Get_Index_I_M(3,instruction);
	
	X(i) = Ref(GetNthHead(G(v),(GetNumber(X(l)) + o)));

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * PUT_NTH_TAIL
       ************************/
    case PUT_NTH_TAIL:
    put_nth_tail:
      {
	register indx v, l, i;
	register s32 o;
	Pre_FetchTop(4);

	v = Get_Index_I_M(1,instruction);
	l = Get_Index_I_M(2,instruction);
	o = Get_Index(pc);
	i = Get_Index_I_M(3,instruction);
	
	X(i) = Ref(GetNthTail(G(v),(GetNumber(X(l)) + o)));

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * PUT_GLOBAL_ARG
       ************************/
    case PUT_GLOBAL_ARG:
    put_global_arg:
      {
	register indx i, n;
	Pre_FetchTop(2);

	i = Get_Index_I_M(1,instruction);
	n = Get_Index_I(2,instruction);

	X(n) = G(i);

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * NIL_NTH_HEAD
       ************************/
    case NIL_NTH_HEAD:
    nil_nth_head:
      {
	register indx v, l;
	register s32 o;
	Pre_FetchTop(2);

	v = Get_Index_I_M(1,instruction);
	l = Get_Index_I(2,instruction);
	o = Get_Index(pc);

	*GetNthHead(G(v),(GetNumber(X(l)) + o)) = atom_nil;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * UNIFY_NTH_HEAD
       ************************/
    case UNIFY_NTH_HEAD:
    unify_nth_head:
      WriteModeDispatch(unify_nth_head_write);
      {
	register indx v, l;
	register s32 o;
	Pre_FetchTop(3);

	v = Get_Index_I_M(1,instruction);
	l = Get_Index_I(2,instruction);
	o = Get_Index(pc);
	
	Unify(Ref(s),Ref(GetNthHead(G(v),(GetNumber(X(l))+ o))));

	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * UNIFY_NTH_TAIL
       ************************/
    case UNIFY_NTH_TAIL:
    unify_nth_tail:
      WriteModeDispatch(unify_nth_tail_write);
      {
	register indx v, l;
	register s32 o;
	Pre_FetchTop(3);

	v = Get_Index_I_M(1,instruction);
	l = Get_Index_I(2,instruction);
	o = Get_Index(pc);
	
	Unify(Ref(s),Ref(GetNthTail(G(v),(GetNumber(X(l))+o))));

	s += VARSIZE;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * UNIFY_GLOBAL_ARG
       ************************/
    case UNIFY_GLOBAL_ARG:
    unify_global_arg:
      WriteModeDispatch(unify_global_arg_write);
      {
	register indx i;
	Pre_FetchTop(1);

	i = Get_Index_I(1,instruction);

	Unify(Ref(s), G(i));

	s += VARSIZE;
	
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * START_RIGHT_BODY
       ************************/
    case START_RIGHT_BODY:
    start_right_body:
      {
	register int i;
	register TAGGED Num;
	register indx n;

	n = Get_Index_I(1,instruction);

	DerefNLL(Num, X(n));

	if (!IsNUM(Num)) goto fail;
	
	if (w->global->scheduling == DYNAMIC)
	  {
	    w->global->sched_level = GetNumber(Num);
	  }

	/*   Reset level count, this could be done in parallel but
         * would require synchronization.
	 */
	for (i = 0; i < w->global->active_workers; i++)
	  w->global->level[i] = GetNumber(Num);

	goto start_body;
      }

      /************************
       * START_LEFT_BODY
       ************************/
    case START_LEFT_BODY:
    start_left_body:
      {
	register int i;
      
	if (w->global->scheduling == DYNAMIC)
	  {
	    w->global->sched_level = 0;
	  }

	/*   Reset level count, this could be done in parallel but would
         * require synchronization.
	 */
	for (i = 0; i < w->global->active_workers; i++)
	  w->global->level[i] = 0;
      }

    start_body:
      {
	w->global->parallel_start.type = W_EXECUTE_CODE;
	w->global->parallel_start.code = DispatchLabel(pc,0);
	w->global->event_flag = 0;

	/* Activate worker backtracking, mainly to restore their heaps.
	 */
	PushOnTrail(w,Tagify(NULL,STR));

	Inc_Label(pc);

	/*   We need to globalize all registers used by the parallel workers.
	 * There may be stack references hidden in inv-arguments and parallel
	 * workers are only allowed to refer to local stack variables.
	 */
	{
	  register code *cpc = pc;

	  register natural live_list_size = Get_Index(pc);
	  register TAGGED *live_list = (TAGGED *)pc;

	  pc = cpc;

	  while (live_list_size--)
	    {
	      GlobalizeRegisterX(X(live_list[live_list_size]), H);
	    }
	}

	SaveCache(H);

	ActivateWorkersGC(w,
			  {
			    /* The computation may terminate prematurely due to GC.
			     */
			    if (EventTrue(w->global->event_flag, GLOBAL_EVENT_GC))
			      {
				code *cpc = pc;

				natural env_size;
				natural live_list_size = Get_Index(pc);
				TAGGED *live_list = (TAGGED *)pc;
				
				pc += live_list_size;

				env_size = Get_Index(pc);

				if (env_size == NO_ENV_ID)
				  env_size = FrameSize(w->next_instr);

				PreGC_PackLiveList(live_list, live_list_size);

				w->gc_info.arity = live_list_size;
				w->gc_info.env_size = env_size;

				garbage_collect(w);

				PostGC_UnpackLiveList(live_list,
						      live_list_size);
				pc = cpc;

				RemoveGlobalEvent(GLOBAL_EVENT_GC);
				/* GC_Run_Parallel_NoWait(W_GC_FINISH); */
				
				BarrierSynch(w,3); 

				continue;
			      }
			    else
			      {
				break;
			      }
			  });

	LoadCache(H);

	if (EventTrue(w->global->event_flag,GLOBAL_EVENT_FAIL))
	  {
	    RemoveGlobalEvent(GLOBAL_EVENT_FAIL);
	    goto fail;
	  }

#if defined(TIMESTAMP) || defined(UNBOUND)
	{
	  register int i;

	  for(i = w->global->active_workers ;
	      i > 0 ;
	      i--)
	    {
	      if (w[i].time > w->time)
		{
		  w->time = w[i].time;
		}
	    }
	}
#endif	

	pc += Get_Index(pc) + 1; /* live_list_size + 1 */

	Execute_Read_Instr;
      }
      
      /************************
       * INITIALIZE_RIGHT
       ************************/
    case INITIALIZE_RIGHT:
    initialize_right:
      {
	Pre_FetchTop(2);

	if (w->global->scheduling == STATIC)  
	  {
	    register indx step, g;
	    
	    step = Get_Index_I_M(1,instruction);
	    g = Get_Index_I(2,instruction);
	    
	    w->global->level[w->pid-1] = GetNumber(G(g)) - 1 - 
	      step*(w->pid-1) +
		step * w->global->active_workers;
	    
	  }
	else
	  {
	    Get_Index_I_M(1,instruction);
	    Get_Index_I(2,instruction);
	  }
	w->direction = RIGHT_BODY;

	Pre_Execute_Read_Instr(1);
      }

      /************************
       * INITIALIZE_LEFT
       ************************/
    case INITIALIZE_LEFT:
    initialize_left:
      {
	Pre_FetchTop(1);

	if (w->global->scheduling == STATIC)  
	  {
	    register indx step;
	    
	    step = Get_Index_I_M(1,instruction);
	    
	    w->global->level[w->pid-1] = step * (w->pid-1) -
	      step * w->global->active_workers;
	  }
	else
	  {
	    Get_Index_I_M(1,instruction);
	  }
	w->direction = LEFT_BODY;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * SPAWN_RIGHT
       ************************/
    case SPAWN_RIGHT:
    spawn_right:
      {
	register indx step, i;
	register s32 level;
	Pre_FetchTop(2);

#include "global_event.c"

	LightStackInit(w);

	step = Get_Index_I_M(1,instruction);
	i = Get_Index_I_M(2,instruction);

	if (w->global->scheduling == STATIC)
	  {
	    level = w->global->level[w->pid-1] - 
	      step*w->global->active_workers;
	  }
	else
	  {
	    GrabLevel(level,w); 
	    w->global->sched_level = level - step;
	  }
	
	w->global->level[w->pid-1] = level;

	X(i) = Make_Integer(level);

	if (level < 0) goto done;

	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * SPAWN_LEFT
       ************************/
    case SPAWN_LEFT:
    spawn_left:
      {
	register indx step, i, g;
	register s32 level;
	Pre_FetchTop(3);

#include "global_event.c"

	LightStackInit(w);

	step = Get_Index_I_M(1,instruction);
	i = Get_Index_I_M(2,instruction);
	g = Get_Index_I_M(3,instruction);
      
	if (w->global->scheduling == STATIC)
	  {
	    level = w->global->level[w->pid-1] + 
	      step*w->global->active_workers;
	  }
	else
	  {
	    GrabLevel(level,w) 
	      w->global->sched_level = level + step;
	  }

	w->global->level[w->pid-1] = level;

	X(i) = Make_Integer(level);
	    
	if (level >= GetNumber(G(g))) goto done;

	Pre_Execute_Read_Instr(1);
      }

      /************************
       * AWAIT_LEFTMOST
       ************************/
    case AWAIT_LEFTMOST:
    await_leftmost:
#if defined(ACTIVE_CONSUMER)
      WriteModeDispatch(await_leftmost_write);
      {
	register BOOL isfirst;
	Pre_FetchTop(0);


	while (TRUE)
	  {
	    IsLeftmost(isfirst,w);

	    if (isfirst) break;

	    AwaitCountStat(w);

	    if (EventTrue(w->global->event_flag,GLOBAL_EVENT_FAIL))
	      goto done;
	  }

	Pre_Execute_Read_Instr(1);
      }
#endif /* ACTIVE_CONSUMER */

      /************************
       * AWAIT_NONVAR
       ************************/
    case AWAIT_NONVAR:
    await_nonvar:
#if defined(ACTIVE_CONSUMER)
      {
	register indx i;
	register TAGGED Xi;
	Pre_FetchTop(1);

	i = Get_Index_I(1,instruction);
	
	DerefNLL(Xi,X(i));
	
	IsNonVarOrLeftmost(Xi,w,goto done);

	X(i) = Xi;

	Pre_Execute_Read_Instr(1);
      }
#endif /* ACTIVE_CONSUMER */

      /************************
       * AWAIT_STRICTLY_NONVAR
       ************************/
    case AWAIT_STRICTLY_NONVAR:
    await_strictly_nonvar:
#if defined(ACTIVE_CONSUMER)
      {
	register indx i;
	register TAGGED Xi;
	Pre_FetchTop(1);

	i = Get_Index_I(1,instruction);
	
	DerefNLL(Xi,X(i));

	if (IsVar(Xi))
	  {
	    /* Wait until bound or first, if first genereate runtime error.
	     */
	    register BOOL isfirst;

	    AwaitStat(w);

	    while (TRUE)
	      {
		IsLeftmost(isfirst,w);

		DerefNLL(Xi,Xi);

		if (!IsVar(Xi))
		  {
		    X(i) = Xi;
		    Pre_Execute_Read_Instr(1);
		  }
		else if (isfirst)
		  {
		    if (IsSVA(Xi))
		      {
			AddGlobalEvent(GLOBAL_EVENT_FAIL);
			Error("await_strictly_nonvar: stack variable");
			goto done;
		      }

		    AddGlobalEvent(GLOBAL_EVENT_FAIL);
		    Error("await_strictly_nonvar: term is variable when leftmost");
		    goto done;
		  }

		if (EventTrue(w->global->event_flag,GLOBAL_EVENT_FAIL))
		  goto done;

		AwaitCountStat(w);
	      }
	  }
	else
	  {
	    X(i) = Xi;
	  }

	Pre_Execute_Read_Instr(1);
      }
#endif /* ACTIVE_CONSUMER */

      /************************
       * AWAIT_VARIABLE
       ************************/
    case AWAIT_VARIABLE:
    await_variable:
#if defined(ACTIVE_CONSUMER)
      {
	register indx i;
	register TAGGED Xi;
	Pre_FetchTop(1);

	i = Get_Index_I(1,instruction);
	
	DerefNLL(Xi,X(i));
	
	if (IsVar(Xi))
	  {
	    if (IsSVA(Xi))
	      {
		Execute_Read_Instr;
	      }

	    /* Wait until first, fail if bound
	     */
	    AwaitStat(w);

	    while (TRUE)
	      {
		register BOOL isfirst;

		IsLeftmost(isfirst,w);

		DerefNLL(Xi,Xi);

		if (isfirst) 
		  if (IsVar(Xi))
		    {
		      Pre_Execute_Read_Instr(1);
		    }
		  else
		    goto fail;

		if (!IsVar(Xi)) goto fail;

		if (EventTrue(w->global->event_flag,GLOBAL_EVENT_FAIL)) 
		  goto done;

		AwaitCountStat(w);
	      }
	  }
	else
	  goto fail;

	Pre_Execute_Read_Instr(1);
      }

#endif /* ACTIVE_CONSUMER */

      /************************
       * AWAIT_NONVAR_UNIFY
       ************************/
    case AWAIT_NONVAR_UNIFY:
    await_nonvar_unify:
#if defined(ACTIVE_CONSUMER)
      WriteModeDispatch(await_nonvar_unify_write);
      {
	register TAGGED Xi;
	Pre_FetchTop(0);

	DerefNLL(Xi,Ref(s));
	
	IsNonVarOrLeftmost(Xi,w,goto done);

	Pre_Execute_Read_Instr(1);
      }
#endif /* ACTIVE_CONSUMER */

      /************************
       * AWAIT_STRICTLY_NONVAR_UNIFY
       ************************/
    case AWAIT_STRICTLY_NONVAR_UNIFY:
    await_strictly_nonvar_unify:
#if defined(ACTIVE_CONSUMER)
      WriteModeDispatch(await_strictly_nonvar_unify_write);
      {
	register TAGGED Xi;
	Pre_FetchTop(0);

	DerefNLL(Xi,Ref(s));
	
	if (IsVar(Xi))
	  {
	    /* Wait until bound or first, if first genereate runtime error.
	     */
	    register BOOL isfirst;

	    AwaitStat(w);

	    while (TRUE)
	      {
		IsLeftmost(isfirst,w);

		DerefNLL(Xi,Xi);

		if (!IsVar(Xi))
		  {
		    Pre_Execute_Read_Instr(1);
		  }
		else if (isfirst)
		  {
		    if (IsSVA(Xi))
		      {
			AddGlobalEvent(GLOBAL_EVENT_FAIL);
			Error("await_strictly_nonvar: stack variable");
			goto done;
		      }
		    AddGlobalEvent(GLOBAL_EVENT_FAIL);
		    Error("await_strictly_nonvar: level first, term is variable");
		    goto done;
		  }
		if (EventTrue(w->global->event_flag,GLOBAL_EVENT_FAIL)) 
		  goto done;
		AwaitCountStat(w);
	      }
	  }

	Pre_Execute_Read_Instr(1);
      }
#endif /* ACTIVE_CONSUMER */

      /************************
       * AWAIT_VARIABLE_UNIFY
       ************************/
    case AWAIT_VARIABLE_UNIFY:
    await_variable_unify:
#if defined(ACTIVE_CONSUMER)
      WriteModeDispatch(await_variable_unify_write);
      {
	register TAGGED Xi;
	Pre_FetchTop(0);

	DerefNLL(Xi,Ref(s));
	
	if (IsVar(Xi))
	  {
	    /* Wait until first, fail if bound.
	     */
	    AwaitStat(w);

	    while (TRUE)
	      {
		register BOOL isfirst;

		IsLeftmost(isfirst,w);

		DerefNLL(Xi,Xi);

		if (isfirst) 
		  if (IsVar(Xi))
		    {
		      Pre_Execute_Read_Instr(1);
		    }
		  else
		    goto fail;

		if (!IsVar(Xi)) goto fail;

		if (EventTrue(w->global->event_flag,GLOBAL_EVENT_FAIL)) 
		  goto done;

		AwaitCountStat(w);
	      }
	  }
	else
	  goto fail;

	Pre_Execute_Read_Instr(1);
      }

#endif /* ACTIVE_CONSUMER */

      /************************
       * PAR_BUILTIN Op Ns {SuspList} Ny Nx {LiveList} Args
       ************************/
    case PAR_BUILTIN:
    par_builtin:
      {
	register uword func = Get_Index_I_M(1,instruction);

	register natural wait_list_size = Get_Index_I(2,instruction);
	register sword  *wait_list = (sword *)pc;

	integer env_size;

	natural live_list_size;
	sword  *live_list;

	pc += wait_list_size;

	env_size = Get_Index(pc);

	live_list_size = Get_Index(pc);
	live_list = (sword *)pc;

	pc += live_list_size;

	while (wait_list_size--)
	  {
	    register TAGGED Xi;

	    DerefNLL(Xi, X(*wait_list));
	    
	    if (IsHVA(Xi))
	      {
		register BOOL is_first;	/* Wait until first or bound. */

		AwaitStat(w);

		forever
		  {
		    IsLeftmost(is_first, w);

		    DerefNLL(Xi, Xi);

		    if (not(IsHVA(Xi)) || is_first)
		      break;

		    if (EventTrue(w->global->event_flag, GLOBAL_EVENT_FAIL))
		      goto done;

		    if (EventTrue(w->global->event_flag, GLOBAL_EVENT_GC))
		      {
			SaveCache(H);

			PreGC_PackLiveList(live_list, live_list_size);

			X(live_list_size) = Xi;

			w->gc_info.arity = live_list_size + 1;
			w->gc_info.env_size = env_size;

			if (not(worker_garbage_collect(w, FALSE)))
			  goto global_fail;

			Xi = X(live_list_size);

			PostGC_UnpackLiveList(live_list, live_list_size);

			LoadCache(H);
		      }
		    AwaitCountStat(w);
		  }
	      }

	    X(*wait_list) = Xi;

	    wait_list++;
	  }

	{
	  sword *arg_list = (sword *)pc;

	  SaveCache(H);

	  if ((GetInlineFnk(func))(w, arg_list) == FALSE) goto fail;
	
	  LoadCache(H);
	
	  pc += GetInlineArity(func);
	}
	BuiltinCallStatistics;
	
	Execute_Read_Instr;
      }

      /************************
       * PAR_INLINE
       ************************/
    case PAR_INLINE:
    par_inline: 
      {
	register uword func = Get_Index_I_M(1,instruction);

	register natural wait_list_size = Get_Index_I(2,instruction);
	register sword  *wait_list;

	code *fail_label = DispatchLabel(pc,0);
	
	Inc_Label(pc);

	wait_list = (sword *)pc;

	pc += wait_list_size;

	while (wait_list_size--)
	  {
	    register TAGGED Xi;

	    DerefNLL(Xi, X(*wait_list));

	    IsNonVarOrLeftmost(Xi,w,goto done);

	    X(*wait_list) = Xi;

	    wait_list++;
	  }
	
	SaveCache(H);
	
	if ((GetInlineFnk(func))(w, (sword *)pc) == FALSE)
	  {
	    pc = fail_label;
	  }
	else
	  {
	    pc += GetInlineArity(func);
	  }
	
	LoadCache(H);
	
	BuiltinCallStatistics;

	Execute_Read_Instr;
      }

      /************************
       * LOCK_AND_GET_LIST
       ************************/
    case LOCK_AND_GET_LIST:
    lock_and_get_list:
      {
	register TAGGED Xi;
	register indx i,n;      
	Pre_FetchTop(2);
	
	i = Get_Index_I_M(1,instruction);
	n = Get_Index_I(2,instruction);
	
	Xi = X(i);

	DerefLockSwitch (Xi,
			 {
			   Make_LST(H,X(n));
			   Drop_P_Lock(Xi,Xi);
			   Pre_Execute_Write_Instr(1);
			 },
			 {
			   if (IsLST(Xi))
			     {
			       X(i) = Xi;
			       X(n) = Xi;
			       s = RemoveTag(Xi,LST);
			       Pre_Execute_Read_Instr(1);
			     }
			   else
			     goto fail;
			 });
      }

      /************************
       * LOCK_AND_GET_STRUCTURE
       ************************/
    case LOCK_AND_GET_STRUCTURE:
    lock_and_get_structure:
      {
	register TAGGED Xi,new, f;
	register indx i,n;
	Pre_FetchTop(3);

	f = Get_Functor(pc);
	i = Get_Index_I_M(1,instruction);
	n = Get_Index_I(2,instruction);
	
	Xi = X(i);

	DerefLockSwitch (Xi,
			 {
			   Make_STR(H,X(n),f);
			   Drop_P_Lock(Xi,Xi);
			   Pre_Execute_Write_Instr(1);
			 },
			 {
			   if (IsSTR(Xi))
			     {
			       if (GetFunctor(Xi) == f)
				 {
				   X(i) = Xi;
				   X(n) = Xi;
				   s = GetArg(Xi,0);
				   Pre_Execute_Read_Instr(1);
				 }
			       else
				 goto fail;
			     }
			   else
			     goto fail;
			 });
      }

      /************************
       * UNLOCK
       ************************/
    case UNLOCK:
    unlock:
      {
	register indx i, n;
	Pre_FetchTop(2);

	i = Get_Index_I_M(1,instruction);
	n = Get_Index_I(2,instruction);

	if (X(i) != X(n))
	  {
	    Unify1(X(i),X(n));
	  }
	
	Pre_Execute_Read_Instr(1);
      }
#endif /* REFORM */

      /************************
       * HALT
       ************************/
    case HALT:
    halt:
      goto done;
      
      /************************
       * NOOP
       ************************/
    case NOOP:
    noop:
      Execute_Read_Instr;
      
      /************************
       * END_OF_PRED
       ************************/
    case END_OF_PRED:
    end_of_pred:

    default: 
      {
	FatalError("illegal instruction");
      }
    }
  
  } /* End of read instructions. */


  /**********************************************************************
   *
   * Write instructions.
   *
   */
 write_instructions:
  {
    DisplayInstr("write");
  
    instruction = Get_Code(pc);

    InstrStat(instruction);

    PrintInst(instruction,1);
  
    switch (Get_Instr(instruction)) {

      /************************
       * DUMMY_INSTRUCTION
       ************************/
    case DUMMY_INSTRUCTION:
      {
	FatalError("illegal instruction");
      }
      break;
      
      /************************
       * SWITCH_ON_TERM
       ************************/
    case SWITCH_ON_TERM:
      goto switch_on_term;
      
      /************************
       * SWITCH_ON_CONSTANT
       ************************/
    case SWITCH_ON_CONSTANT:
      goto switch_on_constant;
      
      /************************
       * SWITCH_ON_STRUCTURE
       ************************/
    case SWITCH_ON_STRUCTURE:
      goto switch_on_structure;
      
      /************************
       * TRY
       ************************/
    case TRY:
      goto try;
      
      /************************
       * RETRY
       ************************/
    case RETRY:
      goto retry;
      
      /************************
       * TRUST
       ************************/
    case TRUST:
      goto trust;
      
      /************************
       * TRY_ME_ELSE
       ************************/
    case TRY_ME_ELSE:
      goto try_me_else;
      
      /************************
       * RETRY_ME_ELSE
       ************************/
    case RETRY_ME_ELSE:
      goto retry_me_else;
      
      /************************
       * TRUST_ME
       ************************/
    case TRUST_ME:
      goto trust_me;
      
      /************************
       * CHOICE_X
       ************************/
    case CHOICE_X:
      goto choice_x;
      
      /************************
       * CHOICE_Y
       ************************/
    case CHOICE_Y:
      goto choice_y;
      
      /************************
       * CUT
       ************************/
    case CUT:
      goto cut;
      
      /************************
       * CUT_X
       ************************/
    case CUT_X:
      goto cut_x;
      
      /************************
       * CUT_Y
       ************************/
    case CUT_Y:
      goto cut_y;
      
      /************************
       * INLINE
       ************************/
    case INLINE:
      goto in_line;
      /************************
       * BUILTIN
       ************************/
    case BUILTIN:
      goto builtin;
      
      /************************
       * META_CALL
       ************************/
    case META_CALL:
      goto meta_call;
      
      /************************
       * META_EXECUTE
       ************************/
    case META_EXECUTE:
      goto meta_execute;
      
      /************************
       * REQUIRE
       ************************/
    case REQUIRE:
      /************************
       * REQUIRE_USING
       ************************/
    case REQUIRE_USING:
      FatalError("require encountered in write mode");
      break;
      
      /************************
       * ALLOCATE
       ************************/
    case ALLOCATE:
      goto allocate;
      
      /************************
       * ALLOCATE2
       ************************/
    case ALLOCATE2:
      goto allocate2;
      
      /************************
       * DEALLOCATE
       ************************/
    case DEALLOCATE:
      goto deallocate;
      
      /************************
       * INIT
       ************************/
    case INIT:
      goto init;
      
      /************************
       * CALL
       ************************/
    case CALL:
      goto call;
      
      /************************
       * EXECUTE
       ************************/
    case EXECUTE:
      goto execute;
      
      /************************
       * PROCEED
       ************************/
    case PROCEED:
      goto proceed;
      
      /************************
       * FAIL
       ************************/
    case FAIL:
      goto fail;
      
      /************************
       * GET_X_VARIABLE
       ************************/
    case GET_X_VARIABLE:
      goto get_x_variable;
      
      /************************
       * GET_Y_VARIABLE
       ************************/
    case GET_Y_VARIABLE:
      goto get_y_variable;
      
      /************************
       * GET_Y_FIRST_VALUE
       ************************/
    case GET_Y_FIRST_VALUE:
      goto get_y_first_value;
      
      /************************
       * GET_X_VALUE
       ************************/
    case GET_X_VALUE:
      goto get_x_value;
      
      /************************
       * GET_Y_VALUE
       ************************/
    case GET_Y_VALUE:
      goto get_y_value;
      
      /************************
       * GET_CONSTANT
       ************************/
    case GET_CONSTANT:
      goto get_constant;
      
      /************************
       * GET_NIL
       ************************/
    case GET_NIL:
      goto get_nil;
      
      /************************
       * GET_STRUCTURE
       ************************/
    case GET_STRUCTURE:
      goto get_structure;
      
      /************************
       * GET_LIST
       ************************/
    case GET_LIST:
      goto get_list;
      
      /************************
       * GET_CONSTANT_X0
       ************************/
    case GET_CONSTANT_X0:
      goto get_constant_x0;
      
      /************************
       * GET_NIL_X0
       ************************/
    case GET_NIL_X0:
      goto get_nil_x0;
      
      /************************
       * GET_STRUCTURE_X0
       ************************/
    case GET_STRUCTURE_X0:
      goto get_structure_x0;
      
      /************************
       * GET_LIST_X0
       ************************/
    case GET_LIST_X0:
      goto get_list_x0;
      
      /************************
       * PUT_X_VOID
       ************************/
    case PUT_X_VOID:
      goto put_x_void;
      
      /************************
       * PUT_Y_VOID
       ************************/
    case PUT_Y_VOID:
      goto put_y_void;
      
      /************************
       * PUT_X_VARIABLE
       ************************/
    case PUT_X_VARIABLE:
      goto put_x_variable;
      
      /************************
       * PUT_Y_VARIABLE
       ************************/
    case PUT_Y_VARIABLE:
      goto put_y_variable;
      
      /************************
       * PUT_X_VALUE
       ************************/
    case PUT_X_VALUE:
      goto put_x_value;
      
      /************************
       * PUT_Y_VALUE
       ************************/
    case PUT_Y_VALUE:
      goto put_y_value;
      
      /************************
       * PUT_X_UNSAFE_VALUE
       ************************/
    case PUT_X_UNSAFE_VALUE:
      goto put_x_unsafe_value;
      
      /************************
       * PUT_Y_UNSAFE_VALUE
       ************************/
    case PUT_Y_UNSAFE_VALUE:
      goto put_y_unsafe_value;
      
      /************************
       * PUT_CONSTANT
       ************************/
    case PUT_CONSTANT:
      goto put_constant;
      
      /************************
       * PUT_NIL
       ************************/
    case PUT_NIL:
      goto put_nil;
      
      /************************
       * PUT_STRUCTURE
       ************************/
    case PUT_STRUCTURE:
      goto put_structure;
      
      /************************
       * PUT_LIST
       ************************/
    case PUT_LIST:
      goto put_list;
      
      /************************
       * UNIFY_VOID
       ************************/
    case UNIFY_VOID: 
    unify_void_write:
      {
	register indx n;
	Pre_FetchTop(1);
	
	n = Get_Index_I(1,instruction); 
	
	do {
	  CreateHVA(H,w);
	} while (--n);
	
	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * UNIFY_X_VARIABLE
       ************************/
    case UNIFY_X_VARIABLE:
    unify_x_variable_write:
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);

	LoadHVA(H,X(n),w);

	Pre_Execute_Write_Instr(1);
      }

      /************************
       * UNIFY_XVAR_XVAR
       ************************/
    case UNIFY_XVAR_XVAR:
    unify_xvar_xvar_write:
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);

	LoadHVA(H,X(n),w);

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * UNIFY_Y_VARIABLE
       ************************/
    case UNIFY_Y_VARIABLE:
    unify_y_variable_write:
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);

	LoadHVA(H,Y(n),w);

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * UNIFY_Y_FIRST_VALUE
       ************************/
    case UNIFY_Y_FIRST_VALUE:
    unify_y_first_value_write:
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);

	LoadHVA(H,Y(n),w);

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * UNIFY_X_VALUE
       ************************/
    case UNIFY_X_VALUE:
    unify_x_value_write:
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);

	PushOnHeap(H,X(n));

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * UNIFY_Y_VALUE
       ************************/
    case UNIFY_Y_VALUE:
    unify_y_value_write:
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);

	PushOnHeap(H,Y(n));

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * UNIFY_X_LOCAL_VALUE
       ************************/
    case UNIFY_X_LOCAL_VALUE:
    unify_x_local_value_write:
      {
	register indx n;
	Pre_FetchTop(1);
	
	n = Get_Index_I(1,instruction);

	WriteLocalValueX(X(n),H);

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * UNIFY_Y_LOCAL_VALUE
       ************************/
    case UNIFY_Y_LOCAL_VALUE:
    unify_y_local_value_write:
      {
	register indx n;
	Pre_FetchTop(1);
	
	n = Get_Index_I(1,instruction);

	WriteLocalValueY(Y(n),H);

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * UNIFY_CONSTANT
       ************************/
    case UNIFY_CONSTANT:
    unify_constant_write:
      { 
	register TAGGED c;
	Pre_FetchTop(1);
	
	c = Get_Tagged(pc);
	
	PushOnHeap(H,c);

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * UNIFY_NIL
       ************************/
    case UNIFY_NIL:
    unify_nil_write:
      {
	Pre_FetchTop(0);

	PushOnHeap(H,atom_nil);

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * UNIFY_STRUCTURE
       ************************/
    case UNIFY_STRUCTURE:
    unify_structure_write:
      { 
	register TAGGED f, *tmp;
	Pre_FetchTop(1);
	
	f = Get_Functor(pc);
	
	tmp = H + VARSIZE;

	PushOnHeap(H,Tagify(tmp,STR));
	PushOnHeapF(H,f);

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * UNIFY_LIST
       ************************/
    case UNIFY_LIST:
    unify_list_write:
      { 
	register TAGGED *tmp;
	Pre_FetchTop(0);
	
	tmp = H + VARSIZE;

	PushOnHeap(H,Tagify(tmp,LST));

	Pre_Execute_Write_Instr(1);
      }

#if defined(JUMP_CALL)
      /************************
       * CJUMP
       ************************/
    case CJUMP:
      goto cjump;

      /************************
       * EJUMP
       ************************/
    case EJUMP:
      goto ejump;

#endif /* JUMP_CALL */      

#if defined(BOUNDED_Q)
      /************************
       * ZEROP
       ************************/
    case ZEROP:
      goto zerop;
      
      /************************
       * LISTP
       ************************/
    case LISTP:
      goto listp;
      
      /************************
       * DETERMINISTIC
       ************************/
    case DETERMINISTIC:
      goto deterministic;
      
      /************************
       * ALLOCATE_STAR
       ************************/
    case ALLOCATE_STAR:
      goto allocate_star;
      
      /************************
       * REPEAT
       ************************/
    case REPEAT:
      goto repeat;
      
      /************************
       * ITERATE_INT
       ************************/
    case ITERATE_INT:
      goto iterate_int;
      
      /************************
       * ITERATE_INT_STAR
       ************************/
    case ITERATE_INT_STAR:
      goto iterate_int_star;
      
      /************************
       * ITERATE_LIST
       ************************/
    case ITERATE_LIST:
      goto iterate_list;
      
      /************************
       * ITERATE_LIST_STAR
       ************************/
    case ITERATE_LIST_STAR:
      goto iterate_list_star;
      
      /************************
       * ENSURE_LIST_TRY
       ************************/
    case ENSURE_LIST_TRY:
      goto ensure_list_try;
      
      /************************
       * ENSURE_LIST_TRUST
       ************************/
    case ENSURE_LIST_TRUST:
      goto ensure_list_trust;
      
#endif /* BOUNDED_Q */

#if defined(PARALLEL_BQ)
      /************************
       * SPAWN_LEFT_BQ
       ************************/
    case SPAWN_LEFT_BQ:
      goto spawn_left_bq;
#endif /* PARALLEL_BQ */

#if defined(JUMP_CODE)
      /************************
       * JUMP
       ************************/
    case JUMP:
      goto jump;

#endif /* BOUNDED_Q || REFORM */
      
#if defined(REFORM)
      /************************
       * GLOBAL_GET_X_VALUE
       ************************/
    case GLOBAL_GET_X_VALUE:
      goto global_get_x_value;
      
      /************************
       * GLOBAL_GET_Y_VALUE
       ************************/
    case GLOBAL_GET_Y_VALUE:
      goto global_get_y_value;
      
      /************************
       * GLOBAL_GET_CONSTANT
       ************************/
    case GLOBAL_GET_CONSTANT:
      goto global_get_constant;
      
      /************************
       * GLOBAL_GET_NIL
       ************************/
    case GLOBAL_GET_NIL:
      goto global_get_nil;
      
      /************************
       * GLOBAL_GET_STRUCTURE
       ************************/
    case GLOBAL_GET_STRUCTURE:
      goto global_get_structure;
      
      /************************
       * GLOBAL_GET_LIST
       ************************/
    case GLOBAL_GET_LIST:
      goto global_get_list;
      
      /************************
       * GLOBAL_GET_CONSTANT_X0
       ************************/
    case GLOBAL_GET_CONSTANT_X0:
      goto global_get_constant_x0;
      
      /************************
       * GLOBAL_GET_NIL_X0
       ************************/
    case GLOBAL_GET_NIL_X0:
      goto global_get_nil_x0;
      
      /************************
       * GLOBAL_GET_STRUCTURE_X0
       ************************/
    case GLOBAL_GET_STRUCTURE_X0:
      goto global_get_structure_x0;
      
      /************************
       * GLOBAL_GET_LIST_X0
       ************************/
    case GLOBAL_GET_LIST_X0:
      goto global_get_list_x0;
      
      /************************
       * GLOBAL_UNIFY_X_VALUE
       ************************/
    case GLOBAL_UNIFY_X_VALUE:
    global_unify_x_value_write:
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);

	PushOnHeap(H,X(n));

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * GLOBAL_UNIFY_Y_VALUE
       ************************/
    case GLOBAL_UNIFY_Y_VALUE:
    global_unify_y_value_write:
      {
	register indx n;
	Pre_FetchTop(1);

	n = Get_Index_I(1,instruction);

	PushOnHeap(H,Y(n));

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * GLOBAL_UNIFY_X_LOCAL_VALUE
       ************************/
    case GLOBAL_UNIFY_X_LOCAL_VALUE:
    global_unify_x_local_value_write:
      {
	register indx n;
	Pre_FetchTop(1);
	
	n = Get_Index_I(1,instruction);

	WriteLocalValueX(X(n),H);

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * GLOBAL_UNIFY_Y_LOCAL_VALUE
       ************************/
    case GLOBAL_UNIFY_Y_LOCAL_VALUE:
    global_unify_y_local_value_write:
      {
	register indx n;
	Pre_FetchTop(1);
	
	n = Get_Index_I(1,instruction);

	WriteLocalValueY(Y(n),H);

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * GLOBAL_UNIFY_CONSTANT
       ************************/
    case GLOBAL_UNIFY_CONSTANT:
    global_unify_constant_write:
      { 
	register TAGGED c;
	Pre_FetchTop(1);
	
	c = Get_Tagged(pc);
	
	PushOnHeap(H,c);

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * GLOBAL_UNIFY_NIL
       ************************/
    case GLOBAL_UNIFY_NIL:
    global_unify_nil_write:
      {
	Pre_FetchTop(0);

	PushOnHeap(H,atom_nil);

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * GLOBAL_UNIFY_STRUCTURE
       ************************/
    case GLOBAL_UNIFY_STRUCTURE:
    global_unify_structure_write:
      { 
	register TAGGED f, *tmp;
	Pre_FetchTop(1);
	
	f = Get_Functor(pc);
	
	tmp = H + VARSIZE;

	PushOnHeap(H,Tagify(tmp,STR));
	PushOnHeapF(H,f);

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * GLOBAL_UNIFY_LIST
       ************************/
    case GLOBAL_UNIFY_LIST:
    global_unify_list_write:
      { 
	register TAGGED *tmp;
	Pre_FetchTop(0);
	
	tmp = H + VARSIZE;

	PushOnHeap(H,Tagify(tmp,LST));

	Pre_Execute_Write_Instr(1);
      }

      /************************
       * BUILD_REC_POSLIST
       ************************/
    case BUILD_REC_POSLIST:
      goto build_rec_poslist;
      
      /************************
       * BUILD_POSLIST
       ************************/
    case BUILD_POSLIST:
      goto build_poslist;
      
      /************************
       * BUILD_POSLIST_VALUE
       ************************/
    case BUILD_POSLIST_VALUE:
      goto build_poslist_value;
      
      /************************
       * BUILD_NEGLIST
       ************************/
    case BUILD_NEGLIST:
      goto build_neglist;
      
      /************************
       * BUILD_NEGLIST_VALUE
       ************************/
    case BUILD_NEGLIST_VALUE:
      goto build_neglist_value;
      
      /************************
       * BUILD_VARIABLES
       ************************/
    case BUILD_VARIABLES:
      goto build_variables;
      
      /************************
       * PUT_NTH_HEAD
       ************************/
    case PUT_NTH_HEAD:
      goto put_nth_head;
      
      /************************
       * PUT_NTH_TAIL
       ************************/
    case PUT_NTH_TAIL:
      goto put_nth_tail;
      
      /************************
       * PUT_GLOBAL_ARG
       ************************/
    case PUT_GLOBAL_ARG:
      goto put_global_arg;
      
      /************************
       * NIL_NTH_HEAD
       ************************/
    case NIL_NTH_HEAD:
      goto nil_nth_head;
      
      /************************
       * UNIFY_NTH_HEAD
       ************************/
    case UNIFY_NTH_HEAD:
    unify_nth_head_write:
      {
	register indx v, l;
	register s32 o;
	Pre_FetchTop(3);

	v = Get_Index_I_M(1,instruction);
	l = Get_Index_I(2,instruction);
	o = Get_Index(pc);
	
	PushOnHeap(H,Ref(GetNthHead(G(v),(GetNumber(X(l)) + o))));

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * UNIFY_NTH_TAIL
       ************************/
    case UNIFY_NTH_TAIL:
    unify_nth_tail_write:
      {
	register indx v, l;
	register s32 o;
	Pre_FetchTop(3);

	v = Get_Index_I_M(1,instruction);
	l = Get_Index_I(2,instruction);
	o = Get_Index(pc);
	
	PushOnHeap(H,Ref(GetNthTail(G(v),(GetNumber(X(l)) + o))));

	Pre_Execute_Write_Instr(1);
      }
      
      /************************
       * UNIFY_GLOBAL_ARG
       ************************/
    case UNIFY_GLOBAL_ARG:
    unify_global_arg_write:
      {
	register indx i;
	Pre_FetchTop(1);

	i = Get_Index_I(1,instruction);

	PushOnHeap(H,G(i));
	
	Pre_Execute_Read_Instr(1);
      }
      
      /************************
       * START_RIGHT_BODY
       ************************/
    case START_RIGHT_BODY:
      goto start_right_body;
      
      /************************
       * START_LEFT_BODY
       ************************/
    case START_LEFT_BODY:
      goto start_left_body;
      
      /************************
       * INITIALIZE_RIGHT
       ************************/
    case INITIALIZE_RIGHT:
      goto initialize_right;
      
      /************************
       * INITIALIZE_LEFT
       ************************/
    case INITIALIZE_LEFT:
      goto initialize_left;
      
      /************************
       * SPAWN_RIGHT
       ************************/
    case SPAWN_RIGHT:
      goto spawn_right;
      
      /************************
       * SPAWN_LEFT
       ************************/
    case SPAWN_LEFT:
      goto spawn_left;

      /************************
       * AWAIT_LEFTMOST
       ************************/
    case AWAIT_LEFTMOST:
    await_leftmost_write:
#if defined(ACTIVE_CONSUMER)
      {
	register BOOL isfirst;
	Pre_FetchTop(0);

	forever
	  {
	    IsLeftmost(isfirst,w);

	    if (isfirst) break;

	    AwaitCountStat(w);

	    if (EventTrue(w->global->event_flag,GLOBAL_EVENT_FAIL))
	      goto done;
	  }

	Pre_Execute_Write_Instr(1);
      }
#endif /* ACTIVE_CONSUMER */

      /************************
       * AWAIT_NONVAR
       ************************/
    case AWAIT_NONVAR:
      goto await_nonvar;
      
      /************************
       * AWAIT_STRICTLY_NONVAR
       ************************/
    case AWAIT_STRICTLY_NONVAR:
      goto await_strictly_nonvar;
      
      /************************
       * AWAIT_VARIABLE
       ************************/
    case AWAIT_VARIABLE:
    await_variable_write:
      goto await_variable;

      /************************
       * AWAIT_NONVAR_UNIFY
       ************************/
    case AWAIT_NONVAR_UNIFY:
    await_nonvar_unify_write:
      goto await_leftmost_write;

      /************************
       * AWAIT_STRICTLY_NONVAR_UNIFY
       ************************/
    case AWAIT_STRICTLY_NONVAR_UNIFY:
    await_strictly_nonvar_unify_write:
      AddGlobalEvent(GLOBAL_EVENT_FAIL);
      Error("await_strictly_nonvar: level first, term is variable");
      goto done;

      /************************
       * AWAIT_VARIABLE_UNIFY
       ************************/
    case AWAIT_VARIABLE_UNIFY:
    await_variable_unify_write:
      {
	Pre_FetchTop(0);

	Pre_Execute_Write_Instr(1);
      }

      /************************
       * PAR_BUILTIN
       ************************/
    case PAR_BUILTIN:
      goto par_builtin;

      /************************
       * PAR_INLINE
       ************************/
    case PAR_INLINE:
      goto par_inline;

      /************************
       * LOCK_AND_GET_LIST
       ************************/
    case LOCK_AND_GET_LIST:
      goto lock_and_get_list;

      /************************
       * LOCK_AND_GET_STRUCTURE
       ************************/
    case LOCK_AND_GET_STRUCTURE:
      goto lock_and_get_structure;

      /************************
       * UNLOCK
       ************************/
    case UNLOCK:
      goto unlock;
#endif /* REFORM */
      
      /************************
       * HALT
       ************************/
    case HALT:
      goto halt;
      
      /************************
       * NOOP
       ************************/
    case NOOP:
      Execute_Write_Instr;

      /************************
       * END_OF_PRED
       ************************/
    case END_OF_PRED:
      
    default:
      {
	FatalError("illegal instruction");
      }
    }

  } /* End of write instructions. */

  luther_exit(0);
}
