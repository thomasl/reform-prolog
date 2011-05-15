/*
 * trail.c - Trail Shifting.
 *
 * Patric Hedlin.....Mon Jan 23 1995
 *
 *
 * Implementation:
 *
 *   The trail shifting scheme is straight forward. The trail is reallocated
 * if necessary and all trail references from the stack (choice-points) are
 * updated relative the new location.
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */

#include "luther.h"


#define MIN_TRAIL_GROWTH	4096


#define AdjustTrailPointer(trailp, offset)	(trailp = (TAGGED *)((char *)(trailp) + offset))


/*******************************************************************************
 *
 * Prototypes of private functions.
 *
 */
static PROTOTYPE(void, adjust_choicepoint_chain, (choicepoint *b, integer offset));


/*******************************************************************************
 *
 * shift_trail(worker)
 *
 */
void shift_trail(w)
     worker *w;
{
  TAGGED *trail_new = NULL;
  TAGGED *trail_old = w->trail_start;

  size_t  trail_size = (size_t)w->trail_end - (size_t)w->trail_start;
  size_t  trail_more = trail_size;

  if (w->global->flags.gc_verbose == TRUE)
    {
      fprintf(stderr, "{verbose: Trail Shifting...}\n");
    }

  for (trail_new = NULL; trail_more > MIN_TRAIL_GROWTH ;
       trail_more = trail_more / 2)
    {
      trail_new = (TAGGED *) realloc(trail_old, trail_size + trail_more);
      if (trail_new  != NULL)
	{
	  size_t trail_total = trail_size + trail_more;

	  w->trail_start = trail_new;
	  w->trail_end   = trail_new + trail_total / sizeof(TAGGED);

	  break;
	}
    }

  if (trail_new == NULL)
    {
      FatalError("Unable to allocate memory during trail shifting.");
    }
  else if (trail_new != trail_old)
    {
      integer offset = (integer)trail_new - (integer)trail_old;

      AdjustTrailPointer(w->trail_top, offset);

#if defined(GENERATIONAL)
      AdjustTrailPointer(w->trail_old_gen_top, offset);
#endif
      adjust_choicepoint_chain(w->choice, offset);
    }

  if (w->global->flags.gc_verbose == TRUE)
    {
      fprintf(stderr,
	      "{verbose: Trail Shifting...enlarged trail %ld (%ld) bytes}\n",
	      trail_more,
	      trail_more + trail_size);
    }
}


/*******************************************************************************
 *
 * Private functions.
 *
 */

/*
 * adjust_choicepoint_chain("topmost choice point", offset)
 *
 */
static void adjust_choicepoint_chain(b, offset)
     choicepoint *b;
     integer offset;
{
  choicepoint *this_chp = b;

  while (this_chp != NULL)
    {
      AdjustTrailPointer(this_chp->trail_top, offset);

      this_chp = this_chp->last_choice;
    }
}
