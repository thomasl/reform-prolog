/*
 * event.c - WAM level event handler.
 *
 * Johan Bevemyr.....Tue Aug 13 1991
 *
 *
 * Note: This file MUST be included by the engine.c inside the wam() 
 *       function to be useful.
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

if (w->event_flag != 0)
{
  register u32 event;
  register int i;

  event = w->event_flag;
  w->event_flag = 0;
    
  i = 0;

    do {                    /* until (event == 0) */

      while (!(event & 1))
	{
	  event = event >> 1;
	  i += 1;
	}
    
      event = event >> 1;

      switch (i)
	{
	case EVENT_INTERRUPT:
	  {
	    register TAGGED goal;
	    register int arity, i;

	    /* Create structure Currpred(Args) and call ','/2.
	     */

	    Make_STR(H,goal,def->name);
	
	    for (i = 0, arity = ArityOf(def->name); i != arity; i++)
	      {
		WriteLocalValueX(X(i),H);
	      }

	    if (def->module == module_prolog)
	      {
		register TAGGED sgoal = goal;
		Make_STR(H,goal,functor_colon);
		PushOnHeap(H,module_prolog);
		PushOnHeap(H,sgoal);
	      }
	
	    X(0) = atom_abort;
	    X(1) = goal;
	    
	    w->call_choice = w->choice;

	    pc = interpret_conj->entry_code.incoreinfo; /* call ','/2 */

	    Pre_Fetch;
	    Pre_Execute_Read_Instr(0);

	    break;
	  }

	case EVENT_WAKE:
#if ! defined(NDEBUG)
	  if (w->debug.debugflag)
	    {
	      fprintf(stderr, "{examining trail for %d goals to wake}\n", w->wake_count);
	    }
#endif /* NDEBUG */

#if defined(CONSTR)
	  /* First, find all woken CVAs on the trail */
	  {
	    register TAGGED woken = atom_nil;
	    register TAGGED *trail_top;
	    
	    trail_top = w->trail_top;

	    while (w->wake_count > 0)
	      {
		trail_top--;
#if defined(UNBOUND)
		if (IsHVA(*trail_top))
		  {
		    trail_top--;
		    continue;
		  }
#endif /* UNBOUND */

		if (!IsCVA(*trail_top))
		  continue;
		else
		  {
		    /* Execute the constraints.
		     */

		    register TAGGED ctl; /* Last tail of CVA goal list */

		    LastTail(ctl,GetCVAGoals(*trail_top));

		    Bind(ctl,woken,{goto fail;});

		    woken = GetCVAGoals(*trail_top);

		    w->wake_count--;
		  }
	      }
	    

	  /* Second, if there is something to wake, save pending goal on heap.
	   */
	    
	    if (woken != atom_nil)
	      {
		register TAGGED goal;
		register int arity, i;

		/* 
		 * Save the current goal on the heap.
		 */

		Make_STR(H,goal,def->name);
	
		for (i=0, arity = ArityOf(def->name) ; i != arity ; i++)
		  {
		    WriteLocalValueX(X(i),H);
		  }

		if (def->module == module_prolog)
		  {
		    register TAGGED sgoal = goal;
		    Make_STR(H,goal,functor_colon);
		    PushOnHeap(H,module_prolog);
		    PushOnHeap(H,sgoal);
		  }

		X(1) = goal; /* goal to execute when the goals in woken
			      * have been executed 
			      */

		X(0) = woken; /* woken goals 
			       */
	    
		w->call_choice = w->choice;

		pc = interpret_wake->entry_code.incoreinfo; /* call 'wake'/2 */

#if ! defined(NDEBUG)
		if (w->debug.debugflag)
		  {
		    fprintf(stderr,"{waking goals : ");
		    display_term(stderr,X(0),w);
		    fprintf(stderr,"}\n");
		  }
#endif /* NDEBUG */

		Pre_Fetch;
		Pre_Execute_Read_Instr(0);
	      }
	  }
#endif /* CONSTR */
	  break;

	default:
	  PL_Print2(stderr,"event handler - no such event %d\n", i);
	}
    } while (event != 0);
}

#if defined(PARALLEL)

if (w->global->event_flag != 0)
{
  register int i = 0;
  register uword event = w->global->event_flag;

  do {				/* until (event == 0) */

    while (!(event & 1))
      {
	event = event >> 1;
	inc(i);
      }
    
    event = event >> 1;

    switch (i) {

    case GLOBAL_EVENT_FAIL:
      goto done;

    case GLOBAL_EVENT_GC:
      {
	SaveCache(H);

	w->gc_info.arity = ArityOf(def->name);
	w->gc_info.env_size = FrameSize(w->next_instr);

	if (not(worker_garbage_collect(w, FALSE)))
	  goto global_fail;

	LoadCache(H);
      }
      break;
	  
    default:
      PL_Print2(stderr,"global event handler - no such event %d\n", i);
    }
  } while (event != 0);
}

#endif /* PARALLEL */
