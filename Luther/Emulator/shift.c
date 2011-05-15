/*
 * shift.c - Stack Shifting.
 *
 * Patric Hedlin.....Mon Jan 23 1995
 *
 *
 * Implementation:
 *
 *   We implement a simple stack shifting scheme, using reallocated memory to
 * grow the stack, without any attempt to compact the stack contents. With this
 * simplified approach, moving the stack contents into a new memory region only
 * require that we adjust the in-stack references (references to choice points,
 * environments and stack variables) and the to-stack references (trail slots
 * and x-registers containing stack variable references).
 *   This approach avoids all the problems of in-memory copying and the
 * implementation is thus much simpler.
 *
 *   Starting from the top of the stack, using the environment (E) pointer
 * and the choice point (B) pointer, the stack shifter traverses the chains
 * of environments and choice points while adjusting the chain references.
 * This is done for all choice points (via the B register). For environments
 * however, only the environments in the current executional path, i.e those
 * environments reachable from the E register, are adjusted. Any environments
 * not in the current executional path, but reachable from a choice point, are
 * thus not adjusted at first.
 *   When adjusting an environment or choice point, all slots containing stack
 * variables are adjusted into the new stack. No dereferencing is performed, all
 * reference chains are thus left intact (besides being adjusted to the new stack).
 *   
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */

#include "luther.h"
#include "engine.h"


/*******************************************************************************
 *
 * Choice point support macros.
 *
 */
#define AdjustChoicepointp(chpp, offset) (chpp = (choicepoint *)((char *)chpp + offset))


/*******************************************************************************
 *
 * Environment support macros.
 *
 */
#define AdjustEnvironmentp(envp, offset) (envp = (environment *)((char *)envp + offset))

#define ReadjustEnvironmentp(envp) (envp = Readjust(envp))

#define Readjust(envp)		((environment *)ClrLiveMark((uword)envp))
#define Adjusted(envp)		((environment *)SetLiveMark((uword)envp))
#define IsAdjusted(envp)	GetLiveMark((uword)envp->cont_env)


#define AdjustSVA(term, offset) MakeSVA((char *)TagToPointer(term) + offset)


/*******************************************************************************
 *
 * Prototypes of private functions.
 *
 */
static PROTOTYPE(void, adjust_choicepoint, (choicepoint *c, integer offset));
static PROTOTYPE(void, adjust_environment, (environment *e, integer size, integer offset));

static PROTOTYPE(void, readjust_environment, (environment *e, integer size));

static PROTOTYPE(void, adjust_choicepoint_chain, (choicepoint *b, integer offset));
static PROTOTYPE(void, adjust_environment_chain, (environment *e, integer size, integer offset));
static PROTOTYPE(void, adjust_protected_chain, (choicepoint *c, integer offset));

static PROTOTYPE(void, readjust_choicepoint_chain, (choicepoint *b));
static PROTOTYPE(void, readjust_environment_chain, (environment *e, integer size));
static PROTOTYPE(void, readjust_protected_chain, (choicepoint *c));

static PROTOTYPE(void, adjust_trail, (worker *w, integer offset));
static PROTOTYPE(void, adjust_registers, (worker *w, integer offset));


/*******************************************************************************
 *
 * shift_stack(worker, minimum stack growth)
 *
 */
void shift_stack(w, min_growth)
     worker *w;
     size_t  min_growth;
{
  TAGGED *stack_new = NULL;
  TAGGED *stack_old = w->stack_start;

  size_t stack_size = (size_t)w->stack_end - (size_t)w->stack_start;
  size_t stack_more = stack_size;

  if (w->global->flags.gc_verbose == TRUE)
    {
      fprintf(stderr, "{verbose: Stack Shifting...}\n");
    }

  min_growth = min_growth + LUTHER_STACK_MARGIN_DEF;

  for (stack_new = NULL; stack_more > min_growth; stack_more = stack_more / 2)
    {
      if ((stack_new = (TAGGED *) realloc(stack_old, stack_size + stack_more)) != NULL)
	{
	  size_t stack_total = stack_size + stack_more;

	  w->stack_start  = stack_new;
	  w->stack_end    = stack_new + stack_total / sizeof(TAGGED);
	  w->stack_margin = stack_new + (stack_total - LUTHER_STACK_MARGIN_DEF) / sizeof(TAGGED);

	  break;
	}
    }

  if (stack_new == NULL)
    {
      FatalError("Unable to allocate memory during stack shifting.");
    }
  else if (stack_new != stack_old)
    {
      integer size   = w->gc_info.env_size;
      integer offset = (integer)stack_new - (integer)stack_old;

      AdjustEnvironmentp(w->frame,   offset);
      AdjustChoicepointp(w->choice,  offset);
      AdjustChoicepointp(w->call_choice, offset);
      AdjustChoicepointp(w->fail_choice, offset);

      adjust_environment_chain(w->frame, size, offset);
      adjust_choicepoint_chain(w->choice, offset);

      readjust_environment_chain(w->frame, size);
      readjust_choicepoint_chain(w->choice);

      adjust_trail(w, offset);

      adjust_registers(w, offset);
    }

  if (w->global->flags.gc_verbose == TRUE)
    {
      fprintf(stderr, "{verbose: Stack Shifting...enlarged stack %ld (%ld) bytes}\n",
	      stack_more,
	      stack_more + stack_size);
    }
}


/*******************************************************************************
 *
 * Private functions.
 *
 */

/*
 * adjust_choicepoint(choice point, offset)
 *
 */
static void adjust_choicepoint(c, offset)
     choicepoint *c;
     integer offset;
{
  size_t arity = c->arity;

  while (arity--)
    {
      register TAGGED term = c->areg[arity];

      if (IsSVA(term))
	c->areg[arity] = AdjustSVA(term, offset);
    }
}


/*
 * (re)adjust_environment(environment, size, offset)
 *
 *
 * Note: We have to use a marker on each SVA when adjusted in order to prevent
 *       multiple adjustments (this is due to the fact that an environment may
 *       be visited via the environment chain as well as via a protected chain).
 *
 */
static void adjust_environment(e, size, offset)
     environment *e;
     integer size;
     integer offset;
{
  while (size--)
    {
      register TAGGED term = e->yreg[size];

      if (IsSVA(term) && not(HasLinkMark(term)))
	e->yreg[size] = SetLinkMark(AdjustSVA(term, offset));
    }
}


static void readjust_environment(e, size)
     environment *e;
     integer size;
{
  while (size--)
    {
      register TAGGED term = e->yreg[size];

      if (IsSVA(term) && HasLinkMark(term))
	e->yreg[size] = ClrLinkMark(term);
    }
}


/*
 * adjust_choicepoint_chain("topmost choice point", offset)
 *
 *   Adjusting the choice-point chain is straight forward since all choice-
 * points are reached via this chain.
 *
 * Note: The first (oldest) choice point has no environment associated to it.
 *
 */
static void adjust_choicepoint_chain(b, offset)
     choicepoint *b;
     integer offset;
{
  choicepoint *this_chp = b;
  choicepoint *next_chp = b->last_choice;

  while (next_chp != NULL)
    {
      adjust_protected_chain(this_chp, offset);

      adjust_choicepoint(this_chp, offset);

      AdjustChoicepointp(next_chp, offset);
      
      this_chp->last_choice = next_chp;

      this_chp = next_chp;
      next_chp = this_chp->last_choice;
    }
}



/*
 * adjust_environment_chain("topmost environment", size, offset)
 *
 *   Adjusting the (live) environment chain is straight forward, but we do have
 * one complication, the size of an environment is given by the trim information
 * (as part of the call preceeding the next instruction).
 *   The size is thus calculated via the instruction pointer in the preceeding
 * (younger) environment.
 *
 * Note: The first (oldest) environment has no code associated to it.
 *
 */
static void adjust_environment_chain(e, size, offset)
     environment *e;
     integer size;
     integer offset;
{
  environment *this_env = e;
  environment *next_env = e->cont_env;

  while (next_env != NULL)
    {
      adjust_environment(this_env, size, offset);

      AdjustEnvironmentp(next_env, offset);

      this_env->cont_env = Adjusted(next_env);

      /* Get size of next environment.
       */
      size = FrameSize(this_env->next_instr);

      this_env = next_env;
      next_env = this_env->cont_env;
    }
}


/*
 * adjust_protected_chain("protective choicepoint", offset)
 *
 *   Adjusting a protected environment chain requires some caution since the
 * actual size of a protected environment (an environment which is referenced by
 * a choice-point) is not known from one sigle point in the stack. We thus make
 * an extra sweep through the y-registers in the terminating environment
 * (i.e. the first environment which is already adjusted) in order to adjust any
 * stack variables not already updated.
 *
 */
static void adjust_protected_chain(c, offset)
     choicepoint *c;
     integer offset;
{
  /* Get size of the first protected environment.
   */
  integer size = FrameSize(c->next_instr);

  AdjustEnvironmentp(c->cont_env, offset);
  {
    environment *this_env = c->cont_env;

    while (not(IsAdjusted(this_env)))
      {
	environment *next_env = this_env->cont_env;

	adjust_environment(this_env, size, offset);

	AdjustEnvironmentp(next_env, offset);

	this_env->cont_env = Adjusted(next_env);

	/* Get size of next environment.
	 */
	size = FrameSize(this_env->next_instr);

	this_env = next_env;
      }

    /*   Make an extra sweep through the y-registers to check for any stack
     * variables which has not been adjusted in (the possibly trimmed portion
     * of) the environment.
     */
    while (size--)
      {
	register TAGGED term = this_env->yreg[size];

	if (IsSVA(term) && not(HasLinkMark(term)))
	  this_env->yreg[size] = SetLinkMark(AdjustSVA(term, offset));
      }
  }
}


/*
 * readjust_choicepoint_chain("topmost choice point")
 *
 *
 * Note: The first (oldest) choice point has no environment associated to it.
 *
 */
static void readjust_choicepoint_chain(b)
     choicepoint *b;
{
  choicepoint *this_chp = b;
  choicepoint *next_chp = b->last_choice;

  while (next_chp != NULL)
    {
      readjust_protected_chain(this_chp);

      this_chp = next_chp;
      next_chp = this_chp->last_choice;
    }
}


/*
 * readjust_environment_chain("topmost environment", size)
 *
 */
static void readjust_environment_chain(e, size)
     environment *e;
     integer size;
{
  environment *this_env = e;

  while (this_env != NULL)
    {
      readjust_environment(this_env, size);

      ReadjustEnvironmentp(this_env->cont_env);

      /* Get size of next environment.
       */
      size = FrameSize(this_env->next_instr);

      this_env = this_env->cont_env;
    }
}


/*
 * readjust_protected_chain("protective choicepoint")
 *
 */
static void readjust_protected_chain(c)
     choicepoint *c;
{
  /* Get size of the first protected environment.
   */
  integer size = FrameSize(c->next_instr);

  environment *this_env = c->cont_env;

  while (IsAdjusted(this_env))
    {
      readjust_environment(this_env, size);

      ReadjustEnvironmentp(this_env->cont_env);

      /* Get size of next environment.
       */
      size = FrameSize(this_env->next_instr);

      this_env = this_env->cont_env;
    }

    /*   Make an extra sweep through the y-registers to check for any stack
     * variables which has not been readjusted in (the possibly trimmed portion
     * of) the environment.
     */
    while (size--)
      {
	register TAGGED term = this_env->yreg[size];

	if (IsSVA(term) && HasLinkMark(term))
	  this_env->yreg[size] = ClrLinkMark(term);
      }
}


/*
 * adjust_trail(worker, offset)
 *
 */
static void adjust_trail(w, offset)
     worker *w;
     integer offset;
{
  TAGGED *tp;

  for (tp = w->trail_start; tp < w->trail_top; tp++)
    {
      TAGGED slot = *tp;

      if (IsSVA(slot))
	*tp = AdjustSVA(slot, offset);
    }
}


/*
 * adjust_registers(worker, offset)
 *
 */
static void adjust_registers(w, offset)
     worker *w;
     integer offset;
{
  size_t i = w->gc_info.arity;

  while (i--)
    {
      TAGGED term = w->regs[i];

      if (IsSVA(term))
	w->regs[i] = AdjustSVA(term, offset);
    }
}

