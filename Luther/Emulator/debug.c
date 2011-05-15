/*
 * debug.c
 *
 * Johan Bevemyr.....Wed Jun  5 1991
 * Patric Hedlin.....Thu Oct 20 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"

#include "display.h"
#include "display_code.h"
#include "debug.h"
#include "engine.h"
#include "inline.h"
#include "signal.h"


#if defined(DEBUG)

static int in_break PROTO((worker *, code *));
static int in_spy PROTO((worker *, code));
static int in_predspy PROTO((worker *, code, code *pc));
static void add_break PROTO((worker *, code *));
static void add_spy PROTO((worker *, code));
static void add_predspy PROTO((worker *, definition *));
static void delete_break PROTO((worker *, code *));
static void delete_spy PROTO((worker *, code));
static void delete_predspy PROTO((worker *, definition *));


static int in_break(w, pc)
     worker *w;
     code *pc;
{
  int i;

  for (i = 0; i < w->debug.breakc; i++)
    {
      if (pc == w->debug.breakpoints[i]) return TRUE;
    }
  return FALSE;
}

static int in_spy(w, inst)
     worker *w;
     code inst;
{
  int i;

  for (i = 0; i < w->debug.spyc; i++)
    {
      if (inst == w->debug.spypoints[i]) return TRUE;
    }
  return FALSE;
}

static int in_predspy(w, inst, pc)
     worker *w;
     code inst;
     code *pc;
{
  int i;

  if (inst == CALL || inst == EXECUTE)
    {
      definition *p;
      pc ++;
      p = Get_Definition(pc);

      for (i = 0; i < w->debug.predspyc; i++)
	{
	  if (p == w->debug.predspypoints[i]) return TRUE;
	}
    }
  return FALSE;
}


static void add_break(w, pc)
     worker *w;
     code   *pc;
{
  int i;

  for (i = 0; i < w->debug.breakc; i++)
    {
      if (pc == w->debug.breakpoints[i]) return;
    }

  if (w->debug.breakc < MAXBREAKPOINTS)
    w->debug.breakpoints[w->debug.breakc++] = pc;
  else
    DB_Print1("{To many break points}\n");
}

static void add_spy(w, inst)
     worker *w;
     code    inst;
{
  int i;

  for (i = 0; i < w->debug.spyc; i++)
    {
      if (inst == w->debug.spypoints[i]) return;
    }

  if (w->debug.spyc < MAXSPYPOINTS)
    w->debug.spypoints[w->debug.spyc++] = inst;
  else
    DB_Print1("{To many spy points}\n");
}

static void add_predspy(w, pred)
     worker *w;
     definition *pred;
{
  int i;

  for (i = 0; i < w->debug.spyc; i++)
    {
      if (pred == w->debug.predspypoints[i]) return;
    }

  if (w->debug.predspyc < MAXSPYPOINTS)
    w->debug.predspypoints[w->debug.predspyc++] = pred;
  else
    DB_Print1("{To many predicate spy points}\n");
}


static void delete_break(w, pc)
     worker *w;
     code   *pc;
{
  int i,j;

  for (i = 0; i < w->debug.breakc; i++) 
    {
      if (pc == w->debug.breakpoints[i]) 
	{
	  for (j = i; j < w->debug.breakc - 1; j++)
	    w->debug.breakpoints[j] = w->debug.breakpoints[j+1];
	  w->debug.breakc--;

	  return;
	}
    }
}

static void delete_spy(w, inst)
     worker *w;
     code inst;
{
  int i,j;

  for (i = 0; i < w->debug.spyc; i++)
    {
      if (inst == w->debug.spypoints[i]) 
	{
	  for (j = i; j < w->debug.spyc - 1; j++)
	    w->debug.spypoints[j] = w->debug.spypoints[j+1];
	  w->debug.spyc--;

	  return;
	}
    }
}

static void delete_predspy(w, pred)
     worker *w;
     definition *pred;
{
  int i,j;

  for (i = 0; i < w->debug.predspyc; i++) 
    {
      if (pred == w->debug.predspypoints[i]) 
	{
	  for (j = i; j < w->debug.predspyc - 1; j++)
	    w->debug.predspypoints[j] = w->debug.predspypoints[j+1];
	  w->debug.predspyc--;

	  return;
	}
    }
}


void init_debugger(w, deb)
     worker *w;
     int deb;
{
#if defined(DEBUG)
  w->debug.debugflag = deb;

  w->debug.debugmode = DBG_CREEP;
  w->debug.skipbreak = 0;

  w->debug.debugfail = FALSE;
  w->debug.spyc = 0;
  w->debug.breakc = 0;
  w->debug.predspyc = 0;
  w->debug.display_args = TRUE;
  w->debug.display_envs = TRUE;
  w->debug.display_fail = TRUE;
  w->debug.display_prolog = FALSE;
  w->debug.in = 1;
  w->debug.out = 0;
#endif /* DEBUG */
}


static code *display_current_state(pc,e,b,xreg,mode,w)
     code *pc;
     environment *e;
     choicepoint *b;
     TAGGED *xreg;
     char *mode;
     worker *w;
{
  code *next;
  int op;
  int newarity;

  if (w->debug.display_envs)
    {
      DB_Print2("          e: %#-10lx\n", (uword) e);
      DB_Print3("          b: %#-10lx   mode: %s\n", (uword) b, mode);
    }

  next = display_code_inc(pc,w);

  if (w->debug.display_prolog)
    {
      DB_Print1(".");
    }

  if (w->debug.display_args)
    {
      InstToOp(op,Get_Op(*pc));

      switch (op) {

      case CALL:
      case EXECUTE:
	{
	  int i;
	  definition *def;
	
	  Get_Code(pc);

	  if (op == CALL) Get_Index_I(1,0);

	  def = Get_Definition(pc);
	  newarity = ArityOf(def->name);

	  if (newarity > 0)
	    {
	      DB_Print1(" (");

	      for (i = 0; i < newarity - 1; i++) 
		{
		  display_term_fd(w->debug.out, xreg[i], w);
		  DB_Print1(", ");
		}
	      display_term_fd(w->debug.out, xreg[i], w);
	      DB_Print1(")");
	    }
	}
	break;

#if defined(JUMP_CALL)

      case CJUMP:
	{
	  int i;
	  definition *def;
	
	  Get_Code(pc);
	  Get_Index_I(1,0);
	  def = get_def_code(Get_PC_No_Inc(pc), w);

	  DB_Print2(" %s", GetString(FunctorToAtom(def->name),w));

	  newarity = ArityOf(def->name);

	  if (newarity > 0)
	    {
	      DB_Print1(" (");

	      for (i = 0; i < newarity - 1; i++) 
		{
		  display_term_fd(w->debug.out, xreg[i], w);
		  DB_Print1(", ");
		}
	      display_term_fd(w->debug.out, xreg[i], w);
	      DB_Print1(")");
	    }
	}
	break;

      case EJUMP:
	{
	  int i;
	  definition *def;

	  Get_Code(pc);
	
	  def = get_def_code(Get_PC_No_Inc(pc),w);

	  DB_Print2(" %s", GetString(FunctorToAtom(def->name),w));

	  newarity = ArityOf(def->name);

	  if (newarity > 0)
	    {
	      DB_Print1(" (");

	      for (i = 0; i < newarity - 1; i++) 
		{
		  display_term_fd(w->debug.out, xreg[i], w);
		  DB_Print1(", ");
		}
	      display_term_fd(w->debug.out, xreg[i], w);
	      DB_Print1(")");
	    }
	}
	break;

#endif /* JUMP_CALL */

      case META_CALL:
      case META_EXECUTE:
	{
	  int i;

	  Get_Code(pc);

	  if (op == META_CALL) Get_Index_I(1,*pc);

	  i = Get_Index_I_M(1,*pc);

	  DB_Print1(" (");
	  display_term_fd(w->debug.out, xreg[i], w);
	  DB_Print1(")");
	}
	break;

      case BUILTIN:
	{
	  int i, fnk;
	  code instruction = Get_Code(pc);

	  fnk = Get_Index_I(1, instruction);
	  newarity = GetInlineArity(fnk);

	  DB_Print1(" (");

	  for (i = 0; i < newarity; i++)
	    {
	      if ((GetInlineType(fnk) == I_FUNC) && (GetInlineRetarg(fnk) == i+1))
		{
		  DB_Print1("<ret.arg.>");
		  Get_Index(pc);
		}
	      else
		{
		  display_term_fd(w->debug.out, xreg[Get_Index(pc)], w);
		}
	      if (i != newarity - 1)
		DB_Print1(", ");
	    }
	  DB_Print1(")");
	}
	break;

      case INLINE:
	{
	  int i, fnk;
	  code instruction = Get_Code(pc);

	  fnk = Get_Index_I(1,instruction);
	  newarity = GetInlineArity(fnk);

	  Inc_Label(pc);

	  DB_Print1(" (");

	  for (i = 0; i < newarity; i++)
	    {
	      if ((GetInlineType(fnk) == I_FUNC) && (GetInlineRetarg(fnk) == i+1))
		{
		  DB_Print1("<ret.arg.>");
		  Get_Index(pc);
		}
	      else
		{
		  display_term_fd(w->debug.out, xreg[Get_Index(pc)], w);
		}
	      if (i != newarity - 1)
		DB_Print1(", ");
	    }
	  DB_Print1(")");
	}
	break;

      default:
	/* No args to display */
	;
      }
    }
  return next;
}

  
int debug(pc,w,mode)
     code	*pc;
     worker	*w;
     char	*mode;
{
  int i, j, op;
  int newarity;
  code *nextpc;
  char debug_str[255];

  InstToOp(op,Get_Op(*pc));

  switch (w->debug.debugmode) {

  case DBG_SKIP:
    {
      if (in_break(w,pc) || in_spy(w,op))
	break;

      if (pc == w->debug.skipbreak) 
	{
	  w->debug.skipbreak = NULL;
	  break;
	}
    }
    return TRUE;

  case DBG_LEAP:
    {
      if (in_break(w,pc) || in_spy(w,op) || in_predspy(w,op,pc))
	break;

      if (pc == w->debug.skipbreak) 
	{
	  w->debug.skipbreak = NULL;
	  break;
	}
    }
    return TRUE;

  case DBG_TRACE:
    {
      if (in_break(w,pc) || in_spy(w,op) || in_predspy(w,op,pc))
	break;

      (void) display_current_state(pc, w->frame, w->choice, w->regs, mode, w);

      DB_Print1("\n");
    }
    return TRUE;

  case DBG_CREEP:
    break;

  case DBG_CALL_LEAP:
    {  
      if (in_break(w,pc) || in_spy(w,op))
	break;

      if (pc == w->debug.skipbreak) 
	{
	  w->debug.skipbreak = NULL;
	  break;
	}

      switch (op) {

      case CALL:
      case EXECUTE:
      case META_CALL:
      case META_EXECUTE:
	break;

      default:
	return TRUE;
      }
    }
    break;

  case DBG_CALL_TRACE:
    {
      if (in_break(w,pc) || in_spy(w,op) || in_predspy(w,op,pc))
	break;

      switch (op) {

      case META_CALL:
      case META_EXECUTE:
	{
	  display_code(pc,w);

	  if (w->debug.display_args)
	    {
	      int i = Get_Index_I_M(1,*pc);
		
	      display_term_fd(w->debug.out, w->regs[i], w);
	    }
	  DB_Print1("\n");
	}
	break;

      case CALL:
      case EXECUTE:
	{
	  int i;
	  definition *def;
	    
	  display_code(pc,w);

	  if (w->debug.display_args)
	    {
	      Get_Code(pc);
		
	      if (op == CALL)
		Get_Index_I(1,0);

	      def = Get_Definition(pc);
	      newarity= ArityOf(def->name);
		
	      if (newarity > 0)
		{
		  DB_Print1(" (");

		  for (i = 0; i < newarity - 1; i++) 
		    {
		      display_term_fd(w->debug.out, w->regs[i], w);
		      DB_Print1(", ");
		    }
		  display_term_fd(w->debug.out, w->regs[i], w);
		  DB_Print1(")");
		}
	    }
	  DB_Print1("\n");
	}
	break;

      default:
	;
      }
    }
    return TRUE;

  default:
    {  
      DB_Print1("{unknown debug mode}\n");
    }
    return TRUE;
  }

  nextpc = display_current_state(pc,w->frame,w->choice,w->regs,mode,w);
    
 debugstart:
    
  /*
   * Parse interactive debug commands.
   *
   */
  DB_Print2("\n%s > ", DEBUG_PROMPT);

  readstring(w->debug.in, debug_str);
    
  switch (debug_str[0]) {

  case 'b': /**********************/
    {
      add_break(w, pc);
      DB_Print2("{added break point at: %lx}", (uword) pc);
      display_code(pc,w);
    }
    goto debugstart;

  case 'e': /**********************/
    {
      w->debug.display_envs = w->debug.display_envs ? FALSE : TRUE;
 
      DB_Print2("{environment information turned %s}", (w->debug.display_envs ? "on" : "off"));
    }
    goto debugstart;
	
  case 'f': /**********************/
    {
      w->debug.debugfail = w->debug.debugfail ? FALSE : TRUE;

      DB_Print2("{break at fail turned %s}", (w->debug.debugfail ? "on" :
					      "off"));
    }
    goto debugstart;

  case 'r': /**********************/
    {
      delete_break(w,pc);
      DB_Print2("{removed break point at: %lx} ", (uword) pc);
      display_code(pc,w);
    }
    goto debugstart;

  case 's': /**********************/
    {
      switch (op) {

      case EXECUTE:
      case PROCEED:
      case META_CALL:
      case META_EXECUTE:
	{
	  w->debug.skipbreak = w->next_instr;
	  w->debug.debugmode = DBG_SKIP;	    
	}
	break;

      default:
	{
	  w->debug.skipbreak = nextpc;
	  w->debug.debugmode = DBG_SKIP;
	}
      }
    }
    return TRUE;

  case 'q': /**********************/
  case 'a': /**********************/
    return FALSE;

  case '+': /**********************/
    {
      switch (debug_str[1]) {

      case '+': /**********************/
	{
	  char tmp[50];
	  register int i;

	  sscanf(&(debug_str[2])," %s",tmp);

	  for (i = 1; (i < END_OF_PRED) && strcmp(tmp,GetOpString(i)) != 0; i++) { }

	  if (i != END_OF_PRED) 
	    {
	      add_spy(w,i);
	      DB_Print2("{set spy point on instruction %s}", GetOpString(i));
	    }
	  else 
	    {
	      DB_Print2("{unknown instruction (%s)}", tmp);
	    }
	}
	break;

      case 'p': /**********************/
	{
	  char p_name[50];
	  char m_name[50];
	  int arity;

	  TAGGED module, pred;

	  sscanf(&(debug_str[2])," %s %s %d", m_name, p_name, &arity);

	  if (strcmp(m_name, ".") == STRCMP_EQUAL)
	    module = w->global->current_module;
	  else
	    module = store_atom(m_name, w);

	  pred = StoreFunctor(store_atom(p_name, w), arity);
	  {
	    definition *def = get_definition_module(pred, module, w);

	    switch (def->enter_instruction) {

	    case ENTER_C:
	    case ENTER_EMULATED:
	      {
		add_predspy(w, def);
		DB_Print4("{set spy point on predicate %s:%s/%d}", m_name, p_name, arity);
	      }
	      break;

	    case ENTER_TRUE_SPY:
	    case ENTER_FAKE_SPY:
	    case ENTER_INTERPRETED:
	      {
		DB_Print4("{dynamic predicate (%s:%s/%d)}", m_name, p_name, arity);
	      }
	      break;

	    case ENTER_UNDEFINED:
	      {
		DB_Print4("{undefined predicate (%s:%s/%d)}", m_name, p_name, arity);
	      }
	    }
	  }
	}
	break;

      default:
	{
	  add_spy(w,op);
	  DB_Print2("{set spy point on instruction %s}", GetOpString(op));
	}
      }
    }
    goto debugstart;

  case '-': /**********************/
    {
      switch(debug_str[1]) {

      case '-': /**********************/
	{
	  char tmp[50];
	  register int i;

	  sscanf(&(debug_str[2])," %s",tmp);

	  for (i = 1; (i < END_OF_PRED) && strcmp(tmp,GetOpString(i)) != 0; i++) { }

	  if (i != END_OF_PRED) 
	    {
	      delete_spy(w,i);
	      DB_Print2("{removed spy point on instruction %s}", GetOpString(i));
	    }
	  else 
	    {
	      DB_Print2("{unknown instruction (%s)}", tmp);
	    }
	}
	break;

      case 'p': /**********************/
	{
	  char p_name[64];
	  char m_name[64];
	  int arity;

	  TAGGED module, pred;

	  sscanf(&(debug_str[2])," %s %s %d", m_name, p_name, &arity);

	  if (strcmp(m_name, ".") == STRCMP_EQUAL)
	    module = w->global->current_module;
	  else
	    module = store_atom(m_name, w);

	  pred = StoreFunctor(store_atom(p_name, w), arity);
	  {
	    definition *def = get_definition_module(pred, module, w);

	    switch (def->enter_instruction) {

	    case ENTER_C:
	    case ENTER_EMULATED:
	      {
		delete_predspy(w, def);
		DB_Print4("{removed spy point on predicate %s:%s/%d}", m_name, p_name, arity);
	      }
	      break;

	    case ENTER_TRUE_SPY:
	    case ENTER_FAKE_SPY:
	    case ENTER_INTERPRETED:
	      {
		DB_Print4("{dynamic predicate (%s:%s/%d)}", m_name, p_name, arity);
	      }
	      break;

	    case ENTER_UNDEFINED:
	      {
		DB_Print4("{undefined predicate (%s:%s/%d)}", m_name, p_name, arity);
	      }
	    }
	  }
	}
	break;

      default:
	{
	  delete_spy(w, op);
	  DB_Print2("{removed spy point on instruction %s}", GetOpString(op));
	}
      }
    }
    goto debugstart;

  case 'l': /**********************/
    {
      switch(debug_str[1]) {

      case 'c': /**********************/
	{
	  w->debug.debugmode = DBG_CALL_LEAP;
	}
	return TRUE;

      default:
	{
	  w->debug.debugmode = DBG_LEAP;
	}
	return TRUE;
      }
    }

  case 'd': /**********************/
    {
      switch(debug_str[1]) {

      case 't': /**********************/
	{
	  w->debug.display_args = w->debug.display_args ? FALSE : TRUE;

	  DB_Print2("{argument display turned %s}", (w->debug.display_args ? "on" : "off"));
	}
	break;

      case 'b': /**********************/
	{
	  DB_Print2("{current break points are [%d]...}\n", w->debug.breakc);

	  for (i = 0; i < w->debug.breakc; i++) 
	    {
	      display_code(w->debug.breakpoints[i],w);

	      DB_Print1("\n");
	    }
	}
	break;

      case 'f': /**********************/
	{
	  w->debug.display_fail = w->debug.display_fail ? FALSE : TRUE;

	  DB_Print2("{failure information turned %s}", (w->debug.display_fail
							? "on" : "off"));
	}
	goto debugstart;

      case 'p': /**********************/
	{
	  w->debug.display_fail = FALSE;
	  w->debug.display_args = FALSE;
	  w->debug.display_envs = FALSE;
	  w->debug.display_prolog = TRUE;
	}
	goto debugstart;

      case 's': /**********************/
	{
	  DB_Print2("{current spy points are [%d]...}\n", w->debug.spyc);

	  for (i = 0; i < w->debug.spyc; i++) 
	    {
	      DB_Print2("\t%s", instruction_table[w->debug.spypoints[i]]);

	      DB_Print1("\n");
	    }
	}
	break;

      case 'm':
	{
	  DB_Print2("{current module is '%s'}\n",
		    GetString(w->global->current_module, w));
	}
	break;

      case 'a': /**********************/
	{
	  i = atoi(&(debug_str[2]));

	  DB_Print2("\ta[%d] = ", i);

	  if (w->choice->areg[ARSIZE*i] == 0)
	    {
	      DB_Print1("NULL");
	    }
	  else
	    display_term_fd(w->debug.out, w->choice->areg[ARSIZE*i], w);
	}
	break;

#if defined(PARALLEL)

      case 'g': /**********************/
	{
	  i = atoi(&(debug_str[2]));

	  DB_Print2("\tg[%d] = ", i);

	  if (w->global->global_regs[i] == 0)
	    {
	      DB_Print1("NULL");
	    }
	  else
	    display_term_fd(w->debug.out, w->global->global_regs[i], w);
	}
	break;	    

#endif /* PARALLEL */

      case 'x': /**********************/
	{
	  i = atoi(&(debug_str[2]));

	  DB_Print2("\tx[%d] = ", i);

	  if (w->regs[i] == 0)
	    {
	      DB_Print1("NULL");
	    }
	  else
	    display_term_fd(w->debug.out,w ->regs[i], w);
	}
	break;

      case 'y': /**********************/
	{
	  i = atoi(&(debug_str[2]));

	  DB_Print2("\ty[%d] = ", i);

	  if (w->frame->yreg[i] == 0)
	    {
	      DB_Print1("NULL");
	    }
	  else
	    display_term_fd(w->debug.out, w->frame->yreg[i], w);
	}
	break;

      default:
	{
	  DB_Print1("{unknown print directive}");
	}
      }
    }
    goto debugstart;
    
  case 'g': /**********************/
    {
      newarity = atoi(&(debug_str[1]));

      DB_Print1("{garbage collecting...}");

      w->gc_info.arity = newarity;
      w->gc_info.env_size = FrameSize(w->next_instr);

      garbage_collect(w);

      Run_Parallel(W_GC_FINISH);
    }
    goto debugstart;

  case 'h': /* Print heap information. */
    {
      DB_Print2("\t-: %#lx\n", w->heap_end);
      DB_Print2("\tm: %#lx\n", w->heap_margin);
      DB_Print3("\th: %#lx\t%d words to margin\n", w->heap_top,
		w->heap_margin - w->heap_top);
      DB_Print3("\t-: %#lx\t%d words of memory\n", w->heap_start, 
		w->heap_end - w->heap_start);
    }
    goto debugstart;

  case 'n': /**********************/
    {
      w->debug.debugflag = FALSE;

      DB_Print1("{debugger turned off}\n");
    }
    return TRUE;

  case 'p': /**********************/
    {
      switch(debug_str[1]) {

      case 'y': /**********************/ 
	{
	  int ysize = FrameSize(w->next_instr);

	  DB_Print2("{current y registers are [%d]...}\n", ysize);

	  for (i = 0; i < ysize; i++) 
	    {
	      DB_Print2("\ty[%d] = ", i);

	      if (w->frame->yreg[i] == 0)
		{
		  DB_Print1("NULL");
		}
	      else
		display_term_fd(w->debug.out, w->frame->yreg[i], w);

	      DB_Print1("\n");
	    }
	}
	break;

      case 'a': /**********************/ 
	{
	  int arity = w->choice->arity;

	  DB_Print2("{current a registers are [%d]...}\n", arity);

	  for (i = 0; i < arity ; i++) 
	    {
	      DB_Print2("\ta[%d] = ", i);

	      if (w->choice->areg[ARSIZE*i] == 0)
		{
		  DB_Print1("NULL");
		}
	      else
		display_term_fd(w->debug.out, w->choice->areg[ARSIZE*i], w);

	      DB_Print1("\n");
	    }
	}
	break;

      case 'x': /**********************/
	{
	  int xsize = atoi(&(debug_str[2])) + 1;

	  DB_Print2("{current x registers are [%d]...}\n", xsize);

	  for (i = 0; i < xsize; i++) 
	    {
	      DB_Print2("\tx[%d] = ", i);

	      if (w->regs[i] == 0)
		{
		  DB_Print1("NULL");
		}
	      else
		display_term_fd(w->debug.out, w->regs[i], w);

	      DB_Print1("\n");
	    }
	}
	break;

      default:
	{
	  DB_Print1("{unknown print directive}");
	}
      }
    }
    goto debugstart;

  case 't': /**********************/
    {
      switch(debug_str[1]) {

      case 'c': /**********************/
	{
	  w->debug.debugmode = DBG_CALL_TRACE;
	}
	break;

      default:
	{
	  w->debug.debugmode = DBG_TRACE;
	}
      }
    }
    break;

  case '?': /**********************/
    {
      DB_Print1( "+              - spy on instruction\n");
      DB_Print1( "++ <I>         - spy on instruction I\n");
      DB_Print1( "+p <M> <P> <A> - spy on predicate M:P/A\n");
      DB_Print1( "-              - remove spy on instruction\n");
      DB_Print1( "-- <I>         - remove spy on instruction I\n");
      DB_Print1( "-p <M> <P> <A> - remove spy on predicate M:P/A\n");
      DB_Print1( "a              - abort\n");
      DB_Print1( "b              - breakpoint on this instruction\n");
      DB_Print1( "c              - creep\n");
      DB_Print1( "e              - toggle display env info\n");
      DB_Print1( "db             - display break points\n");
      DB_Print1( "ds             - display spy points\n");
      DB_Print1( "dm             - display current module\n");
      DB_Print1( "dt             - toggle display arguments\n");
      DB_Print1( "da <N>         - display a-register nr N in choice-point \n");
      DB_Print1( "df             - toggle display fail\n");
      DB_Print1( "dx <N>         - display x-register nr N\n");
      DB_Print1( "dy <N>         - display y-register nr N\n");
#if defined(PARALLEL)
      DB_Print1( "dg <N>         - display global x-register nr Nr\n");
#endif /* PARALLEL */
      DB_Print1( "dp             - display as prolog terms\n");
      DB_Print1( "f              - toggle break at fail\n");
      DB_Print1( "g <N>          - force garbage collection (N live X)\n");
      DB_Print1( "l              - leap\n");
      DB_Print1( "lc             - leap to next call\n");
      DB_Print1( "n              - turn off debugger\n");
      DB_Print1( "o <File>       - redirect error info to <File>\n");
      DB_Print1( "py             - print y register\n");
      DB_Print1( "pa             - print a register in choicepoint\n");
      DB_Print1( "px <N>         - print x register to register nr N\n");
      DB_Print1( "q              - quit\n");
      DB_Print1( "r              - remove breakpoint at this instruction\n");
      DB_Print1( "s              - skip (break, leap)\n");
      DB_Print1( "t              - trace (leap and print debug info.)\n"); 
      DB_Print1( "tc             - trace (leap and print calls)\n"); 
    }
    goto debugstart;

  case 'c': /**********************/
    {
      w->debug.debugmode = DBG_CREEP;
    }
    return TRUE;

  case 'o': /**********************/
    { 
      register FILE *newout;
      register int i;
      char fname[256];

      for (i = 0; debug_str[i+2] != '\n'; i++)
	fname[i] = debug_str[i+2];

      if ((newout = fopen(fname,"w")) == NULL)
	{
	  DB_Print2( "{Error: unabled to open %s for writing}\n", fname);
	}
      else
	{
	  currerr = newout;
	}
    }
    goto debugstart;

  case '\n':
  case '\000': /**********************/
    {
      w->debug.debugmode = DBG_CREEP;
    }
    break;

  default:
    {
      DB_Print1("{unknown command}");
    }
    goto debugstart;
  }
  return TRUE;
}


int debug_fail(pc,w, mode)
     code   *pc;
     worker *w;
     char   *mode;
{
  switch (w->debug.debugmode) {

  case DBG_LEAP:
    {
      if (w->debug.debugfail) 
	{
	  if (w->debug.display_fail)
	    DB_Print1("------------ FAIL -----------\n");

	  w->debug.debugmode = DBG_CREEP;

	  return debug(pc, w, mode);
	}
    }
    break;

  case DBG_TRACE:
  case DBG_CREEP:
  case DBG_CALL_LEAP:
  case DBG_CALL_TRACE:
    {
      if (w->debug.display_fail)
	DB_Print1("------------ FAIL -----------\n");

      if (w->debug.debugfail)
	{
	  w->debug.debugmode = DBG_CREEP;

	  return debug(pc, w, mode);
	}
    }
  }
  return TRUE;
}

void debug_prolog_terms(w)
     worker *w;
{
  w->debug.debugflag = TRUE;
  w->debug.debugmode = DBG_TRACE;

  w->debug.display_fail = FALSE;
  w->debug.display_args = FALSE;
  w->debug.display_envs = FALSE;
  w->debug.display_prolog = TRUE;
}
#endif


