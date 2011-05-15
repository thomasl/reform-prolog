/*
 * atom_table.c
 *
 * Johan Bevemyr.....Thu Aug  8 1991
 * Patric Hedlin.....Thu Feb  9 1994
 *
 *
 * Implementation:
 *
 *   All atoms are uniquely stored in a hash table (defined in this file).
 * This enable fast comparison of atoms. The table is implemented using the
 * bucket technique. First a hash function (hash_atom) index into the table
 * to find a bucket, then linear search is performed in the bucket. The size
 * of the hash table is fixed at compile time. It would be a minor change to
 * dynamically change the size of the hash table.
 *
 *   An earlier version stored the hash value in the atom definition, and it
 * could be used efficiently in other hash tables. Currently there is no need
 * for this but it may be reasonalbe to use this approach later.
 *
 *   To get small numbers for all atoms w->global->atomoffset is substracted
 * from the atoms address to get the atom number. Use the macros OffsetAtom()
 * and DeOffsetAtom() if you must manipulate the atom address explicitly.
 *
 *
 * Interface:
 *
 *   init_atom_table()
 *   init_node()
 *   store_atom()
 *   atom_exist()
 *   get_mode()
 *   get_string()
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */

#include "luther.h"
#include "char_table.h"


static TAGGED make_atom PROTO((char *,worker *));


/*******************************************************************************
 *
 *   This table stores the print names of all initial atoms. There is a
 * corresponding table in atom_table.h. If you add an atom to this table be
 * sure to update the table in atom_table.h at the corresponding location,
 * otherwise the table will be mixed up.
 * 
 *   The atoms in this table is stored in the atom hash table at start up time
 * (see init_atom_table() below).
 *
 */
char *atom_table[ATOM_TABLE_SIZE] = {
  "dummy",
  "[]",
  ".",

  "true",
  "fail",
  "false",

  "user_input",
  "user_output",
  "user_error",

  "read",
  "write",
  "append",

  "user",
  "prolog",
  "public",

  "inf",
  "nan",

  "=",
  "<",
  ">",

  "+",
  "-",
  "*",
  "/",
  "//",
  "mod",
  "min",
  "max",
  "abs",
  "log",
  "exp",
  "sqrt",
  "cbrt",
  
  "sin",
  "cos",
  "tan",
  "asin",
  "acos",
  "atan",
  
  "integer",
  "float",
  "floor",
  "ceiling",
  "truncate",
  "round",
  
  "\\/",
  "/\\",
  "\\",
  "#",
  "<<",
  ">>",
  "msb",
  "rng",

  "aref",

  "is",
  
  "array",
  "array_arg",
  "array_setarg",
  "array_sizeof",
  "array_reduce",
  "$array",

  "number_chars",
  "atom_chars",
  "name",

  "unique_name",
  "$new",

  "seed",
  "random",

  "halt",
  "version",

  "$freeze",
  "$frozen",
  "defrost",

  "setarg",
  "zaparg",

  "$atom_mode",
  "$loading_mode",
  "consult",

  "$module",
  "$get_module",
  "$set_module",
  "$public",
  "$dynamic",

  "$assert_delete_other",
  "$asserta",
  "$assertz",
  "$erase",
  "$ref",

  "$mgu_variables",

  "$clause",
  "$predicate_property",
  "built_in",
  "compiled",
  "interpreted",
  "$current_predicate",
  "current_predicate",

  "$topchoice",
  "$save_choice",
  "$load_choice",

  "$retry_choice",
  "$retry_cut",

  "$set_spy",
  "$impose_spy",
  "$remove_spy",

  "$get_trace_level",
  "$set_inc_trace_level",
  "$set_dec_trace_level",

  "$trace",
  "$trace_on",
  "$trace_spy",
  "$trace_off",

  "$set_handler",
  "$find_handler",

  "garbage_collect",

  "struct_reduce",

  "copy_term",

  "worker_rest",
  "active_workers",
  "schedule",
  "static",
  "dynamic",

  "wamdebug",
  "wamdebug_file",
  "wamdebug_socket",
  "wamdebug_prolog",
  "wamnodebug",

  "prolog_flag_gc_verbose",
  "prolog_flag_load_verbose",
  "on",
  "off",

  "processor_ids",
  "processor_sort",

  "$load",
  "$qload",
  "directive",
  "end_of_file",

  "statistics",

  "$statistics_runtime",
  "$statistics_gctime",
  "$statistics_gcnr",
  "$statistics_gcbytes",
  "$statistics_walltime",
  "$statistics_parallel_runtime",
  "$statistics_memory",
  "$statistics_global",
  "$statistics_local",
  "$statistics_trail",
  "$statistics_deref",
  "$statistics_restore",
  "$statistics_instr",
  "$statistics_instr_prof",
  "$statistics_tidy",
  "$statistics_swap",
  "$statistics_code",
  "$statistics_atom",

  "prompt",

  "ttynl",
  "ttytab",
  "ttyput",
  "ttyget",
  "ttyget0",
  "ttyskip",
  "ttyflush",
  
  "$ttygetch0",
  
  "nl",
  "tab",
  "put",
  "get",
  "get0",
  "skip",
  "flush",

  "$getch",
  "$getch0",

  "$display",
  "$write",
  "$write_float",
  "$write_radix",
  "$write_integer",

  "get_input",
  "set_input",

  "get_output",
  "set_output",

  "open",
  "close",
  "rewind",

  "$stream",
  "$stream_list",
  "$stream_name",
  "$stream_mode",
  "$stream_file",
  "$stream_code",

  "$file_code",
  "$fdopen",

  "$unix_access",
  "r_ok",
  "w_ok",
  "x_ok",
  "f_ok",
  "$unix_stat_type",
  "s_ifdir",
  "s_ifreg",
  "s_ifdef",
  "$unix_stat_time",
  "$unix_cd",
  "$unix_popen",

  "expand_file_name",

  "start",
  "abort",

  ":",
  ",",

  "wake",

  "$interpret_goal",
  "$interpret_goal_spy",
  "$interpret_goal_trace",

  "pmon",
  "pmon_print"
};


TAGGED atom_table_tagged[ATOM_TABLE_SIZE+1];

static atom_bucket **atomtable;

/**********************************************************************
 * Creates an atom bucket.
 */

static atom_bucket *make_atom_bucket(a,next,w)
    TAGGED a;
    atom_bucket *next;
    worker *w;
{
  atom_bucket *res;

  res = (atom_bucket *) atom_alloc(sizeof(atom_bucket),w);
  res->a = a;
  res->next = next;
    
  return res;
}

/**********************************************************************
 * This function returns a number in the range 0-(ATOMHASHLEN-1) for
 * indexing into the atom hash table.
 * 
 * Some minor testing was performed to determine that this function
 * produce a reasonable distribution of atoms in the table.
 */

static int hash_atom(str)
    char *str;
{
  long hval;
  int atom_length;
    
  /*
   * The first character is multiplied by 113 (a good number)
   */

  hval = str[0] * 113;

  /* 
   * calculate the length of the atom.
   */
  atom_length = 0;
  while(str[atom_length]!=0) atom_length++;

  /*
   * The last character is multiplied by 109 (another good number)
   */
  if(atom_length != 0)
    hval += str[atom_length-1] * 109;

  hval += atom_length;
    
  return (int) (hval % ATOMHASHLEN);
}

/**********************************************************************
 *
 * Allocates an atom structure in atom space and classifies the
 * atom (if it need to be quoted when printed etc).
 *
 */
static TAGGED make_atom(str,w)
    char *str;
    worker *w;
{
    register char c;
    atom result;
    register int len;
    BOOL seen_alpha  = FALSE;
    BOOL seen_symbol = FALSE;
    BOOL has_dquote  = FALSE;
    BOOL has_squote  = FALSE;
    BOOL has_special = FALSE;

    result = (atom) aligned_atom_alloc(sizeof(struct atom_s),w);

    /*
     * Classify the atom.
     */

    for (len = 0; str[len] != '\0'; len++)
      {
	c = str[len];

	if (c == '\'')
	  {
	    has_squote = has_special = TRUE;
	  }
	else if (IsLowerChar(c) || IsUpperChar(c) || IsDigitChar(c) || IsUnderChar(c))
	  {
	    seen_alpha = TRUE;
	  }
	else if (IsSymbolChar(c))
	  {
	    seen_symbol = TRUE;
	  }
	else
	  {
	    has_special = TRUE;
	  }
      }

    has_dquote = (!has_special & !seen_alpha);
    has_special |= (seen_alpha==seen_symbol);

    if (has_special && !has_squote &&
	((str[0] == '!' && str[1] == 0) ||
	 (str[0] == ';' && str[1] == 0) ||
	 (str[0] == '[' && str[1] == ']' && str[2] == 0) ||
	 (str[0] == '{' && str[1] == '}' && str[2] == 0)))
      {
	has_special = FALSE;
	has_squote = TRUE;
      }
    else if (!has_special &&
	    (IsUnderChar(str[0]) ||
	     IsUpperChar(str[0]) ||
	     IsDigitChar(str[0]) ||
	     (str[0] == '.' && str[1] == 0) ||
	     (str[0] == '*' && str[1] == '/')))
      has_special = TRUE;

    result->pname = (char *) atom_alloc((sizeof(char) * (len + 1)) +
					(sizeof(TAGGED) - (sizeof(char) *
							   (len + 1)) %
					 sizeof(TAGGED)),w);

    (void) strcpy(result->pname,str);

    if (has_special)
      {
	result->mode = Make_Integer(1);
      }
    else if (has_dquote)
      {
	result->mode = Make_Integer(2);
      }
    else if (has_squote)
      {
	result->mode = Make_Integer(4);
      }
    else
      {
	result->mode = Make_Integer(0);
      }

  return Tagify(DeOffsetAtom(result), ATM);
}

/**********************************************************************
 * 
 * Stores an string as an atom in the atom data base. This function 
 * is the interface for storing new atoms in the database (or finding
 * the values for already stored atoms). 
 *
 * Returns: a tagged pointer to an atom.
 *
 */
TAGGED store_atom(str,w)  
    char *str;
    worker *w;
{
  register atom_bucket *bucket;
  register int hv;

  hv = hash_atom(str); 

  for (bucket = atomtable[hv] ;  
       bucket != NULL ;          
       bucket = bucket->next) 
    {
      if (strcmp(str,GetString(bucket->a,w)) == 0)
	return bucket->a;
    }

  /* add atom first in bucket */
  atomtable[hv] = make_atom_bucket(make_atom(str,w),atomtable[hv],w);

  return atomtable[hv]->a;
}

/**********************************************************************
 * Similar to store_atom() except that it returns NULL if the atom
 * already exist in the database. This function is used by unique_name().
 * 
 * Returns: tagged pointer to atom (if not already present in database)
 *          NULL                   (if already present in database)
 */

TAGGED atom_exist(str,w)  
    char *str;
    worker *w;
{
  register atom_bucket *bucket;
  register int hv;

  hv = hash_atom(str); 

  for (bucket = atomtable[hv] ;  
       bucket != NULL ;          
       bucket = bucket->next) 
    {
      if (strcmp(str,GetString(bucket->a,w)) == 0)
	return (TAGGED) NULL;
    }

  /* add atom first in bucket */
  atomtable[hv] = make_atom_bucket(make_atom(str,w), atomtable[hv],w);

  return atomtable[hv]->a;
}

/**********************************************************************
 * 
 * This function returns the print name of an atom. 
 *
 * Returns: char pointer to print name of atom.
 *
 */

char *get_string(a,w)
    atom a;
    worker *w;
{
  return OffsetAtom(a)->pname;
}

/**********************************************************************
 *
 * Returns a TAGGED number indicating the classification of the
 * UNTAGGED atom given as argument.
 *
 */

TAGGED get_mode(a,w)
    atom a;
    worker *w;
{
  return OffsetAtom(a)->mode;
}


/**********************************************************************
 * This procedure initilize the hash table, setting all entries
 * to NULL.
 */

void init_atomtable(w)
    worker *w;
{
  int	i;

  atomtable = w->global->atomtable;

  for (i = 0; i != ATOMHASHLEN; i++)
    atomtable[i] = NULL;
}

/**********************************************************************
 * This procedure store all atoms in the table above (atom_table) 
 * in the atom database. The proper atom identifier can then be
 * found in atom_table_tagged[] at the proper offset (given by the
 * constants defined in atom_table.h).
 */

void init_atom_table(w)
    worker *w;
{
  register int i;

  for (i = 0; i < ATOM_TABLE_SIZE; i++) 
    atom_table_tagged[i] = store_atom(atom_table[i],w);
}

/**********************************************************************
 * Initialises hash table and tagged atom table.
 */

void init_node(w)
    worker *w;
{
  init_atomtable(w);
  init_atom_table(w);
}

