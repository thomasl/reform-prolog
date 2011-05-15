/*
 * statistics.c  
 *
 * Johan Bevemyr.....Mon Jun 10 1991
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
#include "think.h"

/**********************************************************************
 * statistics/0 
 */

BOOL luther_statistics(Arg)
    Argdecl;
{
  int currentusertime, currentsystime;
  
  u32 heapsize, localsize, trailsize, atomsize, backsize, code_size;
  
  heapsize = get_totalheap(w);

  localsize = ((u32) w->stack_end) -
              ((u32) w->stack_start);
  trailsize = ((u32) w->trail_end) -
              ((u32) w->trail_start);
  atomsize = ((u32) w->global->atom_end) -
             ((u32) w->global->atom_start);
  backsize = ((u32) w->global->patch_end) -
             ((u32) w->global->patch_start);
  code_size = ((u32) w->global->code_end) -
              ((u32) w->global->code_start);
  
  currentusertime = usertime();
  currentsystime = systime();
  
  fprintf(currout,"memory(total)      %10lu bytes\n", heapsize + localsize +
	  trailsize + atomsize + backsize + code_size);
  fprintf(currout,"  global stack     %10lu bytes: %10lu in use, %10lu free\n",
	  heapsize, get_usedheap(w), get_freeheap(w));
  fprintf(currout,"  local stack      %10lu bytes: %10lu in use, %10lu free\n",
	  localsize, ((u32) Get_Local_Stack_Top) -
	  ((u32) w->stack_start), ((u32) w->stack_end) -
	  ((u32) Get_Local_Stack_Top));
  fprintf(currout,"  trail stack      %10lu bytes: %10lu in use, %10lu free\n",
	  trailsize, ((u32) w->trail_top) -
	  ((u32) w->trail_start), ((u32) w->trail_end) -
	  ((u32) w->trail_top));
  fprintf(currout,"  code stack       %10lu bytes: %10lu in use, %10lu free\n",
	  code_size, ((u32) w->global->code_current) -
	  ((u32) w->global->code_start),
	  ((u32) w->global->code_end) -
	  ((u32) w->global->code_current));
  fprintf(currout,"  atom stack       %10lu bytes: %10lu in use, %10lu free\n",
	  atomsize, ((u32) w->global->atom_current) -
	  ((u32) w->global->atom_start),
	  ((u32) w->global->atom_end) -
	  ((u32) w->global->atom_current));
  
#if defined(MEASURE_HEAP_RECLAIM)
  fprintf(currout,"\n");

  fprintf(currout,"%10lu bytes allocated\n",
	  ((u32) w->heap_top) - ((u32) w->heap_start) + 
	  w->stats->heap_reclaim + ((u32) w->stats->heap_gc_bytes));
#endif /* MEASURE_HEAP_RECLAIM */

#if defined(TRAILSTAT)
  fprintf(currout,"\n");

  fprintf(currout,"%10lu trail entries\n", w->stats->trail_stat);
  fprintf(currout,"%10lu deref loops\n", w->stats->deref_stat);
#endif /* TRAILSTAT */

  fprintf(currout,"\n");

  fprintf(currout,"%10.3f sec. in prolog\n",
	  (double) (currentusertime-w->stats->user_start_time)/1000);
  fprintf(currout,"%10.3f sec. user time\n", (double) (currentusertime)/1000);
  fprintf(currout,"%10.3f sec. system time\n",
	  (double) (currentsystime)/1000);
  fprintf(currout,"%10.3f sec. (%4.3f sec. wall) for %d garbage collections ",
	  (double) (w->stats->heap_gc_time)/1000,
	  w->stats->heap_gc_walltime, w->stats->heap_gc_nr);
  fprintf(currout,"which collected %d bytes\n", w->stats->heap_gc_bytes);
  fprintf(currout,"\t\t(%4.3f pre; %4.3f mark [%d]; %4.3f copy [%d];\n\t\t %4.3f wait(1);%4.3f wait(2);%4.3f wait(3);%4.3f wait(4);\n\t\t %4.3f post)\n",
	  w->stats->heap_gc_pre_time,
	  w->stats->heap_gc_mark_time,      
	  w->stats->heap_gc_marked,
	  w->stats->heap_gc_copy_time,
	  w->stats->heap_gc_copied,
	  w->stats->heap_gc_wait_time[0],      
	  w->stats->heap_gc_wait_time[1],      
	  w->stats->heap_gc_wait_time[2],      
	  w->stats->heap_gc_wait_time[3],      
	  w->stats->heap_gc_post_time);
  
  fprintf(currout,"\t\t%10d accesses to stack lock\n",
	  w->stats->hva_init_count);


  fprintf(currout,"\n");

#if defined(PARALLEL)
  if(w->pid == 0)
    {
      int i;

      for(i = 1 ; i <= w->global->active_workers ; i++)
	{
	  fprintf(currout,"%10.3f sec. user time in worker nr %d\n",
		  (double) (w[i].stats->current_user_time)/1000, i);

	  fprintf(currout,"%10.3f sec. (%4.3f sec. wall) garbage collections \n",
		  (double) (w[i].stats->heap_gc_time)/1000,
		  w[i].stats->heap_gc_walltime);

	  fprintf(currout,"\t\t(%4.3f pre; %4.3f mark [%d]; %4.3f copy [%d];\n\t\t %4.3f wait(1);%4.3f wait(2);%4.3f wait(3);%4.3f wait(4);\n\t\t %4.3f post)\n",
		  w[i].stats->heap_gc_pre_time,
		  w[i].stats->heap_gc_mark_time,
		  w[i].stats->heap_gc_marked,
		  w[i].stats->heap_gc_copy_time,
		  w[i].stats->heap_gc_copied,
		  w[i].stats->heap_gc_wait_time[0],
		  w[i].stats->heap_gc_wait_time[1],
		  w[i].stats->heap_gc_wait_time[2],
		  w[i].stats->heap_gc_wait_time[3],
		  w[i].stats->heap_gc_post_time);

	  fprintf(currout,"\t\t%10d accesses to stack lock\n",
		  w[i].stats->hva_init_count);

	}

      fprintf(currout,"\n");
    }

#if defined(STAT_AWAIT)
  if(w->pid == 0)
    {
      int i;

      for(i = 1 ; i <= w->global->active_workers ; i++) {
	fprintf(currout,"%10d await %10d count worker nr %d\n",
		w[i].stats->await, w[i].stats->await_count, i);
	w[i].stats->await = w[i].stats->await_count = 0;
      }

      fprintf(currout,"\n");
    }
#endif /* STAT_AWAIT */
#endif /* PARALLEL */

#if defined(unix)
  fprintf(currout,"%10d page faults not requiring physical I/O\n",
	  pagefaults_no_io());
  fprintf(currout,"%10d page faults requiring physical I/O\n",
	  pagefaults_ph_io());
  
  fprintf(currout,"\n");
#endif /* unix */
  
#if defined(STATISTICS)
  fprintf(currout,"%10d calls since prolog start\n", w->stats->calls);
  fprintf(currout,"%10d fails since prolog start\n", w->stats->fails);
  fprintf(currout,"%10d calls to builtin functions since prolog start\n",
	  w->stats->builtin_calls);
  fprintf(currout,"\n");

#if defined(TIMESTAMP)
  fprintf(currout,"%10d variables has been initialized since prolog start\n",
	  w->stats->hva_init_count + w->stats->sva_init_count);
  fprintf(currout,"%10d heap variables \n",
	  w->stats->hva_init_count);
  fprintf(currout,"%10d stack variables\n",
	  w->stats->sva_init_count);
  fprintf(currout,"\n");
#endif

#if defined(SWAP_BIND)
  fprintf(currout,"%10d failed bindings\n", w->stats->swap_fail);
  fprintf(currout,"\n");
#endif

#endif

  return TRUE;
}

#if defined(PARALLEL)
/**********************************************************************
 * statistics/1
 */
BOOL luther_statistics_n(Arg)
    Argdecl;
{
  register TAGGED Worker;
  int work;
  u32 heapsize, localsize, trailsize;

  DerefNLL(Worker,Xw(0));

  if(!IsNUM(Worker) || (w->pid != 0)) return FALSE;

  work = GetNumber(Worker);
  if((work > w->global->active_workers) ||
     (work < 0))
    return FALSE;

  heapsize = get_totalheap(&w[work]);
  localsize = ((u32) w[work].stack_end) -
              ((u32) w[work].stack_start);
  trailsize = ((u32) w[work].trail_end) -
              ((u32) w[work].trail_start);

  fprintf(currout,"memory(total)      %10lu bytes\n", heapsize + localsize +
	  trailsize);
  fprintf(currout,"  global stack     %10lu bytes: %10lu in use, %10lu free\n",
	  heapsize, get_usedheap(&w[work]), get_freeheap(&w[work]));
  fprintf(currout,"  local stack      %10lu bytes: %10lu in use, %10lu free\n",
	  localsize,
	  ((u32) Get_Stack_Top(((worker *) &(w[work])),
			       FrameSize(w[work].next_instr))) -
	  ((u32) w[work].stack_start),
	  ((u32) w[work].stack_end) -
	  ((u32) Get_Stack_Top(((worker *) &(w[work])),
			       FrameSize(w[work].next_instr))));
  fprintf(currout,"  trail stack      %10lu bytes: %10lu in use, %10lu free\n",
	  trailsize, ((u32) w[work].trail_top) -
	  ((u32) w[work].trail_start),
	  ((u32) w[work].trail_end) -
	  ((u32) w[work].trail_top));
  fprintf(currout,"\n");

#if defined(STATISTICS)
  fprintf(currout,"%10d calls since prolog start\n", w[work].stats->calls);
  fprintf(currout,"%10d fails since prolog start\n", w[work].stats->fails);
  fprintf(currout,"%10d calls to builtin functions since prolog start\n",
	  w[work].stats->builtin_calls);
  fprintf(currout,"\n");

#if defined(TIMESTAMP)
  fprintf(currout,"%10d variables has been initialized since prolog start\n",
	  w[work].stats->hva_init_count + w[work].stats->sva_init_count);
  fprintf(currout,"%10d heap variables \n",
	  w[work].stats->hva_init_count);
  fprintf(currout,"%10d stack variables\n",
	  w[work].stats->sva_init_count);
  fprintf(currout,"\n");
#endif

#if defined(SWAP_BIND)
  fprintf(currout,"%10d failed bindings\n", w[work].stats->swap_fail);
  fprintf(currout,"\n");
#endif

#endif

  return TRUE;
}

#else /* Not PARALLEL */
/**********************************************************************/

BOOL luther_statistics_n(Arg)
    Argdecl;
{
  return luther_statistics(Arg);
}
#endif /* PARALLEL */

/**********************************************************************
 * $statistics_runtime/1 
 */
BOOL luther_statistics_runtime(Arg)
    Argdecl;
{
    TAGGED X0, lst;
    int currtime;

    DerefNLL(X0,Xw(0));

    currtime = usertime();

    Make_LST(w->heap_top,lst);

    PushOnHeap(w->heap_top,
	       Make_Integer(currtime - w->stats->user_start_time));
    PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

    PushOnHeap(w->heap_top, Make_Integer(currtime - w->stats->user_last_time));
    PushOnHeap(w->heap_top, atom_nil);

    w->stats->user_last_time = currtime;

    return unify(X0,lst,w);
}

/**********************************************************************
 * $statistics_gctime/1 
 */
BOOL luther_statistics_gctime(Arg)
    Argdecl;
{
    TAGGED X0, lst;
    int currtime;

    DerefNLL(X0,Xw(0));

    currtime = w->stats->heap_gc_time;

    Make_LST(w->heap_top,lst);

    PushOnHeap(w->heap_top, Make_Integer(currtime));
    PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

    PushOnHeap(w->heap_top, Make_Integer(currtime-w->stats->user_last_gctime));
    PushOnHeap(w->heap_top, atom_nil);

    w->stats->user_last_gctime = currtime;

    return unify(X0,lst,w);
}

/**********************************************************************
 * $statistics_gcnr/1 
 */
BOOL luther_statistics_gcnr(Arg)
    Argdecl;
{
    TAGGED X0, lst;
    int currtime;

    DerefNLL(X0,Xw(0));

    currtime = w->stats->heap_gc_nr;

    Make_LST(w->heap_top,lst);

    PushOnHeap(w->heap_top, Make_Integer(currtime));
    PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

    PushOnHeap(w->heap_top, Make_Integer(currtime-w->stats->user_last_gcnr));
    PushOnHeap(w->heap_top, atom_nil);

    w->stats->user_last_gcnr = currtime;

    return unify(X0,lst,w);
}

/**********************************************************************
 * $statistics_gcbytes/1 
 */
BOOL luther_statistics_gcbytes(Arg)
    Argdecl;
{
    TAGGED X0, lst;
    int currbytes;

    DerefNLL(X0,Xw(0));

    currbytes = w->stats->heap_gc_bytes;

    Make_LST(w->heap_top,lst);

    PushOnHeap(w->heap_top, Make_Integer(currbytes));
    PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

    PushOnHeap(w->heap_top, Make_Integer(currbytes-w->stats->user_last_gcbytes));
    PushOnHeap(w->heap_top, atom_nil);

    w->stats->user_last_gcbytes = currbytes;

    return unify(X0,lst,w);
}

/**********************************************************************
 * statistics(walltime,[T1,T2,T3,T4,T5,T6]).
 * 
 *    T1 = total walltime from start of prolog.
 *    
 *    T2 = total walltime since last call to statistics(walltime,...).
 *    
 *    T3 = total walltime in parallel execution since start of prolog.
 *    
 *    T4 = total walltime in parallel execution since last call to 
 *         statistics(walltime,...).
 *    
 *    T5 = total walltime in gc from start of prolog
 *    
 *    T6 = total walltime in gc since last call to statistics(walltime,...).
 *
 * 
 */
BOOL luther_statistics_walltime(Arg)
    Argdecl;
{
    TAGGED X0, lst, t1,t2,t3,t4,t5,t6;
    double currtime;

    DerefNLL(X0,Xw(0));

    currtime = walltime();

    /* floats are stored on the heap, we must create them first */
    t1 = make_float(currtime - w->stats->user_start_walltime, w);
    t2 = make_float(currtime - w->stats->user_last_walltime, w);

#if defined(PARALLEL)
    t3 = make_float(w->stats->in_parallel_walltime, w);
    t4 = make_float(w->stats->in_parallel_walltime -
		    w->stats->in_parallel_walltime_last, w);

    w->stats->in_parallel_walltime_last = w->stats->in_parallel_walltime;
#else
    t3 = Make_Integer(0);
    t4 = Make_Integer(0);
#endif     

    t5 = make_float(w->stats->user_gcwalltime, w);

    t6 = make_float(w->stats->user_gcwalltime -
		    w->stats->user_last_gcwalltime, w);

    Make_LST(w->heap_top,lst);

    PushOnHeap(w->heap_top,t1);
    PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

    PushOnHeap(w->heap_top,t2);
    PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

    PushOnHeap(w->heap_top,t3);
    PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

    PushOnHeap(w->heap_top,t4);
    PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

    PushOnHeap(w->heap_top,t5);
    PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

    PushOnHeap(w->heap_top,t6);
    PushOnHeap(w->heap_top, atom_nil);

    w->stats->user_last_walltime = currtime;
    w->stats->user_last_gcwalltime = w->stats->user_gcwalltime;

    return unify(X0,lst,w);
}

/**********************************************************************
 * $statistics_parallel_runtime/1 
 */
BOOL luther_statistics_parallel_runtime(Arg)
    Argdecl;
{
    TAGGED X0, lst;
    int currtime,i;

#if defined(PARALLEL)

    if(w->pid != 0) return FALSE;
    
    DerefNLL(X0,Xw(0));

    currtime = usertime();

    Make_LST(w->heap_top,lst);

    for(i=1 ; i < w->global->active_workers ; i++)
      {
	PushOnHeap(w->heap_top, Make_Integer(w[i].stats->current_user_time));
	PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));
      }

    if (w->global->active_workers != 0)
      {
	PushOnHeap(w->heap_top, Make_Integer(w[i].stats->current_user_time));
	PushOnHeap(w->heap_top, atom_nil);
      }
    else
      {
	lst = atom_nil;
      }
    return unify(X0,lst,w);

#else

    return FALSE;

#endif /* PARALLEL */
}

/**********************************************************************
 * $statistics_memory/1 
 */
BOOL luther_statistics_memory(Arg)
    Argdecl;
{
  TAGGED X0, lst;
  u32 heapsize, localsize, trailsize, atomsize, backsize, code_size;
  
  DerefNLL(X0,Xw(0));
  
  heapsize = ((u32) w->heap_end) - ((u32) w->heap_start);
  localsize = ((u32) w->stack_end) -
              ((u32) w->stack_start);
  trailsize = ((u32) w->trail_end) - 
              ((u32) w->trail_start);
  atomsize = ((u32) w->global->atom_end) - 
             ((u32) w->global->atom_start);
  backsize = ((u32) w->global->patch_end) - 
             ((u32) w->global->patch_start);
  code_size = ((u32) w->global->code_end) - 
              ((u32) w->global->code_start);
  
  Make_LST(w->heap_top, lst);

  PushOnHeap(w->heap_top, Make_Integer(heapsize + localsize +
				       trailsize + atomsize +
				       backsize + code_size));
  PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

  PushOnHeap(w->heap_top, Make_Integer(0));
  PushOnHeap(w->heap_top, atom_nil);

  return unify(X0,lst,w);
}

/**********************************************************************
 * $statistics_global/1 
 */
BOOL luther_statistics_global(Arg)
    Argdecl;
{
  TAGGED X0, lst, t1, t2, t3, t4;
  
  DerefNLL(X0,Xw(0));

  t1 = Make_Integer(((u32) w->heap_top) -
		    ((u32) w->heap_start));

  t2 = Make_Integer(((u32) w->heap_end) -
		    ((u32) w->heap_top));

  t3 = Make_Integer(w->stats->heap_reclaim);

#if defined(COPY_GC_GEN)
  t4 = Make_Integer(((u32) w->heap_top) - ((u32) w->heap_new_gen_start) + 
		    ((u32) w->heap_old_gen_top) - ((u32) w->heap_start) +
		    w->stats->heap_reclaim + ((u32) w->stats->heap_gc_bytes));
#else
  t4 = Make_Integer(((u32) w->heap_top) - ((u32) w->heap_start) + 
		    w->stats->heap_reclaim + ((u32) w->stats->heap_gc_bytes));
#endif  

  Make_LST(w->heap_top, lst);

  PushOnHeap(w->heap_top, t1);
  PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE, LST));

  PushOnHeap(w->heap_top, t2);
  PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE, LST));

  PushOnHeap(w->heap_top, t3);
  PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE, LST));

  PushOnHeap(w->heap_top, t4);
  PushOnHeap(w->heap_top, atom_nil);


  return unify(X0,lst,w);
}

  
/**********************************************************************
 * $statistics_local/1 
 */
BOOL luther_statistics_local(Arg)
    Argdecl;
{
  TAGGED X0, lst;
  
  DerefNLL(X0,Xw(0));
  
  Make_LST(w->heap_top, lst);

  PushOnHeap(w->heap_top, Make_Integer(((u32) Get_Local_Stack_Top) -
				       ((u32) w->stack_start)));
  PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

  PushOnHeap(w->heap_top, Make_Integer(((u32) w->stack_end) -
				       ((u32) Get_Local_Stack_Top)));
  PushOnHeap(w->heap_top, atom_nil);
	     
  return unify(X0,lst,w);
}

/**********************************************************************
 * $statistics_trail/1 
 */
BOOL luther_statistics_trail(Arg)
    Argdecl;
{
  TAGGED X0, lst;
  
  DerefNLL(X0,Xw(0));
  
  Make_LST(w->heap_top, lst);

  PushOnHeap(w->heap_top, Make_Integer(((u32) w->trail_top) -
				       ((u32) w->trail_start)));
  PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

  PushOnHeap(w->heap_top, Make_Integer(((u32) w->trail_end) -
				       ((u32) w->trail_top)));
#if defined(TRAILSTAT)
  PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

  PushOnHeap(w->heap_top, Make_Integer(w->stats->trail_stat));
#else
  PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

  PushOnHeap(w->heap_top, Make_Integer(0));
#endif /* TRAILSTAT */

  PushOnHeap(w->heap_top, atom_nil);

  return unify(X0,lst,w);
}

/**********************************************************************
 * $statistics_deref/1 
 */
BOOL luther_statistics_deref(Arg)
    Argdecl;
{
  TAGGED X0;

  DerefNLL(X0,Xw(0));
  
#if defined(TRAILSTAT)
  return unify(X0,Make_Integer(w->stats->deref_stat),w);
#else
  return unify(X0,Make_Integer(0),w);
#endif
}

/**********************************************************************
 * $statistics_restore/1 
 */
BOOL luther_statistics_restore(Arg)
    Argdecl;
{
  TAGGED X0;

  DerefNLL(X0,Xw(0));
  
#if defined(TRAILSTAT)
  return unify(X0,Make_Integer(w->stats->restore_stat),w);
#else
  return unify(X0,Make_Integer(0),w);
#endif
}

/**********************************************************************
 * $statistics_instr([Sequential,Parallel])
 */
BOOL luther_statistics_instr(Arg)
    Argdecl;
{
  TAGGED X0;
  TAGGED seq, par, lst;

#if defined(TRAILSTAT)
  seq = Make_Integer(w->stats->instr_stat);

#if defined(PARALLEL)
  {
    int i;
    u32 sum;

    for(i = 1, sum = 0 ; i <= orig_nr_of_workers ; i++)
      sum += w[i].stats->instr_stat;

    par = Make_Integer(sum);
  }
#else /* PARALLEL */
  par = Make_Integer(0);
#endif /* PARALLEL */

#else /* TRAILSTAT */
  seq = Make_Integer(0);
  par = Make_Integer(0);
#endif /* TRAILSTAT */

  DerefNLL(X0,Xw(0));
  
  Make_LST(w->heap_top,lst);

  PushOnHeap(w->heap_top, seq);
  PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));
  PushOnHeap(w->heap_top, par);
  PushOnHeap(w->heap_top, atom_nil);

  return unify(X0,lst,w);
}

/**********************************************************************
 * $statistics_instr_prof(ProfList)
 */

BOOL luther_statistics_instr_prof(Arg)
     Argdecl;
{
#if defined(TRAILSTAT)
  TAGGED ProfList, NewList, Functor, Stat, Seq_Sum, Par_Sum;
  int nr, i, parallel_sum;

  if((w->heap_top + 5*VARSIZE*END_OF_PRED) > w->heap_margin)
    {
      Error("statistics(instr_prof,...) heap out of memory");
      return FALSE;
    }

  NewList = Tagify(w->heap_top+3*VARSIZE,LST);

  for(nr = 1 ; nr < (END_OF_PRED-1) ; nr++)
    {
      for(i = 1, parallel_sum = 0 ; i < orig_nr_of_workers ; i++)
	parallel_sum += w[i].stats->instr_prof[nr];

      Par_Sum = Make_Integer(parallel_sum);
      Seq_Sum = Make_Integer(w->stats->instr_prof[nr]);

      Functor = StoreFunctor(store_atom(instruction_table[nr],w),2);
      Make_STR(w->heap_top,Stat,Functor);
      PushOnHeap(w->heap_top,Seq_Sum);
      PushOnHeap(w->heap_top,Par_Sum);
  
      PushOnHeap(w->heap_top,Stat);
      PushOnHeap(w->heap_top,Tagify(w->heap_top+4*VARSIZE,LST));
    }

  Par_Sum = Make_Integer(0);
  Seq_Sum = Make_Integer(0);

  Functor = StoreFunctor(store_atom(instruction_table[nr],w),2);
  Make_STR(w->heap_top,Stat,Functor);
  PushOnHeap(w->heap_top,Seq_Sum);
  PushOnHeap(w->heap_top,Par_Sum);
  
  PushOnHeap(w->heap_top,Stat);
  PushOnHeap(w->heap_top,atom_nil);

  DerefNLL(ProfList,Xw(0));

  return unify(ProfList,NewList,w);
#else /* TRAILSTAT */
  return TRUE;
#endif
}


/**********************************************************************
 * $statistics_instr/1 
 */
BOOL luther_statistics_tidy(Arg)
    Argdecl;
{
  TAGGED X0;

  DerefNLL(X0,Xw(0));
  
#if defined(TRAILSTAT)
  return unify(X0,Make_Integer(w->stats->tidy_stat),w);
#else
  return unify(X0,Make_Integer(0),w);
#endif
}

/**********************************************************************
 * $statistics_swap([Sequential,Parallel])
 */
BOOL luther_statistics_swap(Arg)
    Argdecl;
{
  TAGGED X0;
  TAGGED seq, par, lst;

#if defined(TRAILSTAT)
  seq = Make_Integer(w->stats->swap_stat);

#if defined(PARALLEL)
  {
    int i;
    u32 sum;

    for(i = 1, sum = 0 ; i <= orig_nr_of_workers ; i++)
      sum += w[i].stats->swap_stat;

    par = Make_Integer(sum);
  }
#else /* PARALLEL */
  par = Make_Integer(0);
#endif /* PARALLEL */
#else /* TRAILSTAT */
  seq = Make_Integer(0);
  par = Make_Integer(0);
#endif /* TRAILSTAT */

  DerefNLL(X0,Xw(0));
  
  Make_LST(w->heap_top,lst);

  PushOnHeap(w->heap_top, seq);
  PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));
  PushOnHeap(w->heap_top, par);
  PushOnHeap(w->heap_top, atom_nil);

  return unify(X0,lst,w);
}

/**********************************************************************
 * $statistics_code/1 
 */
BOOL luther_statistics_code(Arg)
    Argdecl;
{
  TAGGED X0, lst;
  
  DerefNLL(X0,Xw(0));
  
  Make_LST(w->heap_top, lst);

  PushOnHeap(w->heap_top,
	     Make_Integer(((u32) w->global->code_current) -
			  ((u32) w->global->code_start)));
  PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

  PushOnHeap(w->heap_top,
	     Make_Integer(((u32) w->global->code_end) -
			  ((u32) w->global->code_current)));
  PushOnHeap(w->heap_top, atom_nil);

  return unify(X0,lst,w);
}


/**********************************************************************
 * $statistics_atom/1 
 */
BOOL luther_statistics_atom(Arg)
    Argdecl;
{
  TAGGED X0, lst;
  
  DerefNLL(X0,Xw(0));
  
  Make_LST(w->heap_top, lst);

  PushOnHeap(w->heap_top,
	     Make_Integer(((u32) w->global->atom_current) -
			  ((u32) w->global->atom_start)));
  PushOnHeap(w->heap_top, Tagify(w->heap_top+VARSIZE,LST));

  PushOnHeap(w->heap_top,
	     Make_Integer(((u32) w->global->atom_end) -
			  ((u32) w->global->atom_current)));
  PushOnHeap(w->heap_top, atom_nil);

  return unify(X0,lst,w);
}

/**********************************************************************
 * $statistics_pmon/0 
 */
BOOL luther_pmon(Arg)
    Argdecl;
{
#ifdef USE_PMON
  pmon_delta(&(w->global->pmon_data[0]));
  Run_Parallel(W_PMON_DELTA);
#endif
  return TRUE;
}

/**********************************************************************
 * $statistics_pmon_print/0 
 */
BOOL luther_pmon_print(Arg)
    Argdecl;
{
#ifdef USE_PMON
  int i;

  for (i = 0 ; i < (orig_nr_of_workers+1) ; i++)
    {
      PL_Print2(stderr,"PMON_DATA worker %d **********\n",i);
      pmon_print(stderr,&(w->global->pmon_data[i]));
    }

#endif  /* USE_PMON */
  return TRUE;
}

/**********************************************************************/

void init_statistics(w)
    worker *w;
{
    w->stats->user_start_time = usertime();
    w->stats->user_last_time = w->stats->user_start_time;
    w->stats->sys_start_time = w->stats->user_start_time;

    w->stats->user_gcwalltime = (double) 0.0;
    w->stats->user_last_gcwalltime = (double) 0.0;

    w->stats->user_start_walltime = walltime();
    w->stats->user_last_walltime = w->stats->user_start_walltime;

    w->stats->calls = 0;
    w->stats->fails = 0;
    w->stats->builtin_calls = 0;

    w->stats->heap_gc_nr = 0;
    w->stats->heap_gc_bytes = 0;
    w->stats->heap_gc_time = 0;
    w->stats->heap_gc_walltime = 0.0;
    w->stats->heap_gc_mark_time = 0.0;
    w->stats->heap_gc_wait_time[0] = 0.0;
    w->stats->heap_gc_wait_time[1] = 0.0;
    w->stats->heap_gc_wait_time[2] = 0.0;
    w->stats->heap_gc_wait_time[3] = 0.0;
    w->stats->heap_gc_copy_time = 0.0;
    w->stats->heap_gc_post_time = 0.0;
    w->stats->heap_gc_pre_time = 0.0;
    w->stats->heap_gc_marked = 0;
    w->stats->heap_gc_copied = 0;

    w->stats->heap_reclaim = 0;

#if defined(TRAILSTAT)
    w->stats->trail_stat = 0;
    w->stats->deref_stat = 0;
    w->stats->restore_stat = 0;
    w->stats->instr_stat = 0;
    w->stats->tidy_stat = 0;
    w->stats->swap_stat = 0;
    {
      int i;
      for (i = 0; i < END_OF_PRED; i++)
	w->stats->instr_prof[i] = 0;
    }
#endif /* TRAILSTAT */

#if defined(PARALLEL)
    w->stats->in_parallel_walltime = 0.0;
    w->stats->in_parallel_walltime_last = 0.0;

#if defined(STAT_AWAIT)
    w->stats->await = 0;
    w->stats->await_count;
#endif /* STAT_AWAIT */    

#endif /* PARALLEL */

#if defined(TIMESTAMP)
    w->stats->hva_init_count = 0;
    w->stats->sva_init_count = 0;
#endif

#if defined(SWAP_BIND)
    w->stats->swap_fail = 0;
#endif
}
