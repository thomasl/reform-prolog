/*
 * database.c
 *
 * Johan Bevemyr.....Sat May 25 1991
 *
 *
 * Interface:
 *
 *   init_database(worker *)
 *
 *   get_definition() 
 *        returns a pointer the definition structure of the named predicate. 
 *	If the predicate does not exist in the database a pointer to an
 *	"undefined predicate" definition structure is returned.
 *
 *   get_definition_module() 
 *        returns a pointer the definition structure of the named predicate. 
 *	If the predicate does not exist in the database a pointer to an
 *	"undefined predicate" definition structure is returned.
 *
 *   get_ex_definition()
 *        returns a pointer to a definition structure (as above). This
 *	function should be used by parser.y when finding a definition
 *	for the execute WAM instruction.
 *
 *   get_c_definition()
 *        returns a pointer to a definition structure (as above). This
 *	function should be used by parser.y when finding a definition
 *	for the call WAM instruction.
 *
 *   store_emulated_predicate()
 *        store an emulated predicate in the database, delete all
 *	existing definitions of the same predicate.
 *
 *   store_c_predicate()
 *        store a C defined predicate in the database, delete all
 *	existing definitions of the same predicate.
 *	
 *   store_dynamic_predicate()
 *        store a dynamic predicate in the database, delete all
 *	existing definitions of the same predicate.
 *
 *   add_first_dynamic_predicate()
 *        adds an dynamic predicate first (assert)
 *
 *   add_last_dynamic_predicate()
 *        adds an dynamic predicate last (assertz)
 *
 *   remove_dynamic_clause()
 *        removes a dynamic clause (retract)
 *
 *   make_public()
 *        returns a pointer to the predicates definition structure,
 *	it also makes the predicate public (available when in the
 *	user module).
 *
 *   listing()
 *        list all definitions in the database.
 *
 *   emulated_listing()
 *        list all emulated definitions in the database.
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 


/* There are two hash tables: atom and predicate.
 */ 

#include "luther.h"
#include "unify.h"
#include "display_code.h"
#include "engine.h"

definition **predtable;

/**********************************************************************/
definition *make_undefined(name, next,w,module)
     TAGGED name, module;
     definition *next;
     worker *w;
{
  definition *def;

  def = (definition *) atom_alloc(sizeof(definition),w);
  def->enter_instruction = ENTER_UNDEFINED;
  def->name = name;
  def->next = next;
  def->module = module;

#if defined(JUMP_CALL)
  def->users = NULL;
#endif /* JUMP_CALL */

  return def;
}

/**********************************************************************/
in_switch *make_empty_index_table(w)
     worker *w;
{
  in_switch *res = (in_switch *) atom_alloc(sizeof(in_switch),w);

  res->var = NULL;
  res->last = NULL;

  return res;
}

/**********************************************************************/
static void reset_index_table(tab)
     in_switch *tab;
{
  retry_node *tn;

  for (tn = tab->var ; tn != NULL ; tn = tn->next)
    tn->clause = NULL;

  tab->last = NULL;
}
	
/**********************************************************************/
retry_node *make_retry_node(c,next,w)
     code *c;
     retry_node *next;
     worker *w;
{
  register retry_node *res = (retry_node *) atom_alloc(sizeof(retry_node),w);

  res->next = next;
  res->clause = c;

  return res;
}

/**********************************************************************/
void remove_dynamic_clause(tab, n)
     in_switch *tab;
     retry_node *n;
{
  register retry_node *n1, *tlast;

  if ((tab->var == NULL) || (n->clause == NULL)) return;

  n->clause = NULL;

  if (n == tab->var)
    {
      if (tab->last == n)
	tab->last = NULL;
      else
	tab->var = tab->var->next;

      return;
    }
	
  n1 = tab->var;
  tlast = tab->last;
    
 start:
  {
    if (n1->next == n)
      {
	if (tlast == n)
	  {
	    tab->last = n1;
	  }
	else
	  {
	    n1->next = n->next;
	    n->next = tlast->next;
	    tlast->next = n;
	  }
	return;
      }
    else
      {
	n1 = n1->next;
	goto start;
      }    
  }
}

/**********************************************************************/
retry_node *add_dynamic_first(tab, c, w)
     in_switch *tab;
     code *c;
     worker *w;
{
  register retry_node *n1;

  if (tab->var == NULL)
    {
      tab->last = tab->var = make_retry_node(c,NULL,w);
      return tab->var;
    }

  if (tab->last == NULL)
    {
      tab->var->clause = c;
      tab->last = tab->var;
    }
  else if (tab->last->next == NULL)
    {
      tab->var = make_retry_node(c,tab->var,w);
    }
  else
    {
      n1 = tab->last->next;
      tab->last->next = n1->next;
      n1->clause = c;
      n1->next = tab->var;
      tab->var = n1;
    }
  return tab->var;
}

/**********************************************************************/
retry_node *add_dynamic_last(tab, c, w)
     in_switch *tab;
     code *c;
     worker *w;
{
  register retry_node *n1;

  if (tab->var == NULL)
    {
      tab->last = tab->var = make_retry_node(c,NULL,w);
      return tab->last;
    }

  n1 = tab->last;

  if (n1 == NULL)
    {
      tab->var->clause = c;
      tab->last = tab->var;
    }
  else if (n1->next == NULL)
    {
      n1->next = make_retry_node(c,NULL,w);
      tab->last = n1->next;
    }
  else
    {
      n1->next->clause = c;
      tab->last = n1->next;
    }
  return tab->last;
}


#define HashFunctor(F) ((ArityOf(F)+(((unsigned long) F) >> 2)) % PREDHASHLEN)

/**********************************************************************/
void init_database(w)
     worker *w;
{
  int i;
    
  predtable = w->global->predtable;

  for (i = 0; i != PREDHASHLEN; i++)
    predtable[i] = NULL;
}

/**********************************************************************/
definition *get_definition(name,w)
    TAGGED name;
    worker *w;
{
  register definition *pl, **bucket;

  bucket = &(predtable[HashFunctor(name)]);

  for (pl = *bucket; pl != NULL; pl = pl->next)
    {
      if (pl->name == name)
	{
	  if ((pl->module == w->global->current_module) ||
	      (pl->module == module_public))
	    return pl;
	}
    }

  pl = make_undefined(name, *bucket, w, w->global->current_module);
  *bucket = pl;

  return pl;
}

/**********************************************************************/
definition *get_definition_module(name,module,w)
     TAGGED name,module;
     worker *w;
{
  register definition *pl, **bucket;
  
  bucket = &(predtable[HashFunctor(name)]);
  
  for (pl = *bucket; pl != NULL; pl = pl->next)
    {
      if (pl->name == name)
	{
	  if ((pl->module == module) ||
	      (pl->module == module_public))
	    return pl;
	}
    }
  
  pl = make_undefined(name, *bucket, w, module);
  *bucket = pl;
  
  return pl;
}

/**********************************************************************/
definition *get_definition_term(term,w,finalgoal)
     TAGGED term, *finalgoal;
     worker *w;
{
  register definition *pl, **bucket;
  register TAGGED name, module;

  module = w->global->current_module;

 start:
  {
    if (IsSTR(term))
      {
	name = GetFunctor(term);
	if (name == functor_colon)
	  {
	    DerefNLL(module, Ref(GetArg(term, 0)));
	    DerefNLL(term,   Ref(GetArg(term, 1)));
	    goto start;
	  }
      }
    else if (IsATM(term))
      {
	name = StoreFunctor(term,0);
      }
    else if (IsLST(term))
      {
	name = functor_list;
      }
    else 
      {
	(void) luther_error(E_ILLEGAL_GOAL, term, w);
	name = StoreFunctor(atom_fail,0);
      }
  }

  *finalgoal = term;

  bucket = &(predtable[HashFunctor(name)]);

  for (pl = *bucket; pl != NULL; pl = pl->next)
    {
      if (pl->name == name)
	{
	  if ((pl->module == module) || (pl->module == module_public))
	    return pl;
	}
    }

  pl = make_undefined(name, *bucket, w, module);

  *bucket = pl;

  return pl;
}


#if defined(JUMP_CALL)

/**********************************************************************/
void add_user(def, p, w)
     definition *def;
     code *p;
     worker *w;
{
  calluse *cu = (calluse *) atom_alloc(sizeof(calluse),w);

  cu->where = p;
  cu->next = def->users;
  def->users = cu;

  return;
}

/**********************************************************************/
void patch_callers(def)
     definition *def;
{
  calluse *cu = def->users;
  code *new = def->entry_code.incoreinfo; 

  while(cu != NULL)
    {
#if defined(THREADED_CODE)
      if (*(cu->where) == (code)global_label_table[(int)CALL])
	{
	  *(cu->where)   = (code)global_label_table[(int)CJUMP];
	  *(cu->where+2) = (code)new;
	}
      else
	{
	  *(cu->where)   = (code)global_label_table[(int)EJUMP];
	  *(cu->where+1) = (code)new;
	}
#else  /* THREADED_CODE */
      if (Get_Op(*(cu->where)) == CALL)
	{
	  *(cu->where) = (*(cu->where) & (~0xffL)) | CJUMP;
	  *(cu->where+1) = (code)new;
	}
      else
	{
	  *(cu->where) = EJUMP;
	  *(cu->where+1) = (code)new;
	}
#endif /* THREADED_CODE */    
      cu = cu->next;
    }
  return;
}

/**********************************************************************/
void unpatch_callers(def)
     definition *def;
{
  calluse *cu = def->users;

  while(cu != NULL)
    {
#if defined(THREADED_CODE)
      if (*(cu->where) == (code)global_label_table[(int)CJUMP])
	{
	  *(cu->where)   = (code)global_label_table[(int)CALL];
	  *(cu->where+2) = (code)def;
	}
      else
	{
	  *(cu->where)   = (code)global_label_table[(int)EXECUTE];
	  *(cu->where+1) = (code)def;
	}
#else  /* THREADED_CODE */
      if (Get_Op(*(cu->where)) == CJUMP)
	{
	  *(cu->where) = (*(cu->where) & (~0xffL)) | CALL;
	  *(cu->where+1) = (code)def;
	}
      else
	{
	  *(cu->where) = EXECUTE;
	  *(cu->where+1) = (code)def;
	}
#endif /* THREADED_CODE */    
      cu = cu->next;
    }
  return;
}

/**********************************************************************/
void repatch_callers(def)
     definition *def;
{
  calluse *cu = def->users;
  code *new = def->entry_code.incoreinfo; 

  while(cu != NULL)
    {
#if defined(THREADED_CODE)
      if (*(cu->where) == (code)global_label_table[(int)CJUMP])
	*(cu->where+2) = (code)new;
      else
	*(cu->where+1) = (code)new;
#else /* THREADED_CODE */
      *(cu->where+1) = (code)new;
#endif /* THREADED_CODE */    
      cu = cu->next;
    }
  return;
}

/**********************************************************************/
definition *get_ex_definition(name,w)
     TAGGED name;
     worker *w;
{
  definition *def;

  def = get_definition(name, w);

  add_user(def,w->global->code_current-1,w);

  if (def->enter_instruction == ENTER_EMULATED)
    {
#if defined(THREADED_CODE)
      *(w->global->code_current-1) = (code)global_label_table[(int)EJUMP];
#else
      *(w->global->code_current-1) = EJUMP;
#endif
      return (definition *) def->entry_code.incoreinfo;
    }
  else
    {
      return def;
    }
}

/**********************************************************************/
definition *get_c_definition(name,w,inst)
     TAGGED name;
     worker *w;
     s32 *inst;
{
  definition *def;

  def = get_definition(name, w);

#if defined(THREADED_CODE)
  add_user(def,w->global->code_current-1,w);
#else
  add_user(def,w->global->code_current,w);
#endif /* THREADED_CODE */
    
  if (def->enter_instruction == ENTER_EMULATED)
    {
#if defined(THREADED_CODE)
      *(w->global->code_current-1) = (code)global_label_table[CJUMP];
      return (definition *) def->entry_code.incoreinfo;
#else
      *inst = (*inst & (~0xffL)) | CJUMP;
      return (definition *) def->entry_code.incoreinfo;
#endif /* THREADED_CODE */
    }
  else
    {
      return def;
    }
}

/**********************************************************************
 * Given a code pointer this function finds the definition it is
 * associated with.
 */

definition *get_def_code(c,w)
     code *c;
     worker *w;
{
  int i;
  definition *pl;
  code *pc;

  for (i = 0; i != PREDHASHLEN; i++)
    {
      if (predtable[i] != NULL)
	{
	  pl = predtable[i];

	  while (pl != NULL)
	    {

	      switch(pl->enter_instruction) {

	      case ENTER_C:
		break;

	      case ENTER_EMULATED:
		{
		  if (pl->entry_code.incoreinfo == c)
		    return pl;
		}
		break;

	      case ENTER_TRUE_SPY:
	      case ENTER_FAKE_SPY:
	      case ENTER_INTERPRETED:
	      case ENTER_UNDEFINED:
		break;

	      default:
		{
		  PL_Print2(currerr,"get_def_code -- predicate type %d\n",
			    pl->enter_instruction);
		}
		break;
	      }
	      pl = pl->next;
	    }
	}
    }
  PL_Print1(currerr,"get_def_code -- can't find predicate def\n");
  return NULL;
}

#else /* not JUMP_CALL */

/**********************************************************************/
definition *get_ex_definition(name,w)
     TAGGED name;
     worker *w;
{
  return get_definition(name, w);
}

/**********************************************************************/
definition *get_c_definition(name,w,i)
     TAGGED name;
     worker *w;
     TAGGED *i;
{
  return get_definition(name, w);
}

#endif /* JUMP_CALL */


/**********************************************************************/
definition *make_public(name,w)
     TAGGED name;
     worker *w;
{
  register definition **c, *n, **bucket, *ret;

  c = bucket = &(predtable[HashFunctor(name)]);
  n = *c;
  ret = NULL;

  while (n != NULL)
    {
      if (n->name == name)
	{
	  if (n->module == w->global->current_module)
	    {
	      n->module = module_public;
	      ret = n;
	      c = &(n->next);
	      n = *c;
	    }
	  else if (n->module != module_public)
	    {
	      *c = n->next;
	      n = *c;
	    }
	  else
	    {
	      ret = n;
	      c = &(n->next);
	      n = *c;
	    }
	}
      else
	{
	  c = &(n->next);
	  n = *c;
	}
    }
	
  if (ret == NULL)
    {
      ret = make_undefined(name, *bucket, w, w->global->current_module);
      ret->module = module_public;
      *bucket = ret;
    }

  return ret;
}    

/**********************************************************************/

void store_emulated_predicate(name,c,w)
     TAGGED name;
     code *c;
     worker *w;
{
  definition *def = get_definition(name,w);

  def->entry_code.incoreinfo = c;

#if defined(JUMP_CALL)
  if (def->enter_instruction == ENTER_EMULATED)
    repatch_callers(def);
  else
    patch_callers(def);
#endif

  def->enter_instruction = ENTER_EMULATED;
}

/**********************************************************************/
void store_c_predicate(name,func,module,w)
     TAGGED name;
     BOOL (func)();
     TAGGED module;
     worker *w;
{
  definition *def = get_definition(name,w);

#if defined(JUMP_CALL)
  if (def->enter_instruction == ENTER_EMULATED)
    unpatch_callers(def);
#endif

  def->enter_instruction = ENTER_C;
  def->entry_code.cinfo = func;
  def->module = module;
}

/**********************************************************************/
void store_dynamic_predicate(name,module,c,w) /* delete all old definitions */
     TAGGED name,module;
     code *c;
     worker *w;
{
  definition *def = get_definition_module(name,module,w);

#if defined(JUMP_CALL)
  if (def->enter_instruction == ENTER_EMULATED)
    unpatch_callers(def);
#endif

  if (def->enter_instruction == ENTER_TRUE_SPY ||
      def->enter_instruction == ENTER_FAKE_SPY ||
      def->enter_instruction == ENTER_INTERPRETED)
    {
      /* clear old index table
       */
      reset_index_table(def->entry_code.indexinfo);
    }
  else
    {
      def->enter_instruction = ENTER_INTERPRETED;
      def->entry_code.indexinfo = make_empty_index_table(w);
    }

  /*
   * enter new predicate
   */
  (void) add_dynamic_first(def->entry_code.indexinfo, c, w);

  return;
}    

/**********************************************************************/
void add_first_dynamic_predicate(name,module,c,p,r,w) 
     TAGGED name, module, p, r;
     code *c;
     worker *w;
{
  definition *def = get_definition_module(name,module,w);

  if (def->enter_instruction != ENTER_TRUE_SPY &&
      def->enter_instruction != ENTER_FAKE_SPY &&
      def->enter_instruction != ENTER_INTERPRETED)
    {
      def->enter_instruction = ENTER_INTERPRETED;
      def->entry_code.indexinfo = make_empty_index_table(w);
    }

  Bind(p,PointerToTerm(def->entry_code.indexinfo),{return;});

  /* enter new predicate
   */
  Bind(r,PointerToTerm(add_dynamic_first(def->entry_code.indexinfo, c, w)),{return;});
}    

/**********************************************************************/
void add_last_dynamic_predicate(name,module,c,p,r,w) 
     TAGGED name, module,p, r;
     code *c;
     worker *w;
{
  definition *def = get_definition_module(name,module,w);

  if (def->enter_instruction != ENTER_TRUE_SPY &&
      def->enter_instruction != ENTER_FAKE_SPY &&
      def->enter_instruction != ENTER_INTERPRETED)
    {
      def->enter_instruction = ENTER_INTERPRETED;
      def->entry_code.indexinfo = make_empty_index_table(w);
    }

  Bind(p,PointerToTerm(def->entry_code.indexinfo),{return;});

  /* enter new predicate
   */
  Bind(r,PointerToTerm(add_dynamic_last(def->entry_code.indexinfo, c, w)),{return;});
}    

/**********************************************************************/
void init_database_iterator(iterator,module)
     database_iterator *iterator;
     TAGGED module;
{
  register definition *def;
  register int index;

  index = 0;
  while (predtable[index] == NULL) index++;

  def = predtable[index];

  iterator->index = index;
  iterator->def = def;
  iterator->module = module;

  if ((def->enter_instruction != ENTER_UNDEFINED) &&
     ((def->module != module) ||
      (def->module != module_public)))
    {
      return;
    }
  else
    {
      (void) next_pred_in_database(iterator);
      return;
    }
}
/**********************************************************************/
BOOL next_pred_in_database(iterator)
     database_iterator *iterator;
{
  register definition *def;
  register int index;

  index = iterator->index;
  def = iterator->def;

 start:

  /*
   * if the current bucket is empty, try the next;
   */

  if (def->next == NULL)
    {
      index++;
      while (predtable[index] == NULL && index < PREDHASHLEN)
	index++;

      if (index == PREDHASHLEN)
	return FALSE;                    /* no more preds in database */

      def = predtable[index];
    }
  else
    {
      def = def->next;
    }

  if ((def->enter_instruction == ENTER_UNDEFINED) ||
     ((def->module != iterator->module) &&
      (def->module != module_public)))
    {
      goto start;
    }
  else
    {
      iterator->index = index;
      iterator->def = def;
      return TRUE;
    }
}

/*******************************************************************************
 *
 * plisting(worker, option)
 *
 *   Displaying the predicate table (database) contents according to the
 * specified option (i.e. "all", "pl", "c").
 *
 */
void plisting(w, option)
     worker *w;
     ListOption option;
{
  long i;
  definition *def;

  for (i = 0; i < PREDHASHLEN; i++)
    {
      if (predtable[i] != NULL)
	{
	  def = predtable[i];

	  while (def != NULL)
	    {
	      switch (def->enter_instruction) {

	      case ENTER_C:
		{
		  if (option == LIST_OPTION_ALL || option == LIST_OPTION_C)
		    {
		      PL_Print3(currout, "c:predicate(%s/%d)\n",
				GetString(FunctorToAtom(def->name), w),
				ArityOf(def->name));
		    }
		}
		break;

	      case ENTER_EMULATED:
		{
		  code *pc = def->entry_code.incoreinfo;

		  if (option == LIST_OPTION_ALL || option == LIST_OPTION_PL)
		    {
		      PL_Print3(currout, "pl:predicate(%s/%d)\n{\n",
				GetString(FunctorToAtom(def->name), w),
				ArityOf(def->name));

		      while (*pc != END_OF_PRED)
			{
			  fputs("  ", currout);
			  pc = display_code_inc(pc, w);
			  fputs("\n", currout);
			}
		      fputs("}\n", currout);
		    }
		}
		break;

	      case ENTER_TRUE_SPY:
	      case ENTER_FAKE_SPY:
	      case ENTER_INTERPRETED:
		break;

	      case ENTER_UNDEFINED:
		break;

	      default:
		{
		  PL_Print2(currerr, "listing - no such predicate type - %d\n",
			    def->enter_instruction);
		}
		break;
	      }
	      def = def->next;
	    }
	}
    }
}
