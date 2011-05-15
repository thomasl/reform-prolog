/*
 * global_event.c
 *
 * Johan Bevemyr.....Wed Jan 12 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

if (w->global->event_flag != 0)
{
  register int i = 0;
  register uword event = w->global->event_flag;
    
  do {                    /* until (event == 0) */

    while (not(event & 1))
      {
	event = event >> 1;
	inc(i);
      }
    
    event = event >> 1;

    switch(i) {

    case GLOBAL_EVENT_FAIL:
      goto done;

    case GLOBAL_EVENT_GC:
      {
	SaveCache(H);

	w->gc_info.arity = 0;
	w->gc_info.env_size = 0;

	w->choice = NULL;

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
