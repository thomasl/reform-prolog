/*
 * qload.c
 *
 * Johan Bevemyr.....Tue Jun 11 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"
#include "engine.h"
#include "bignum.h"
#include "unify.h"
#include "qload.h"
#include "labelsort.h"

#define Store_Tagged(T)				\
{						\
  *(w->global->code_current) = (T);		\
   (w->global->code_current)++;			\
}

#define Fetch_Name(N)				\
{						\
  long i;					\
  char ch;					\
						\
  Getc(ch, infile);				\
						\
  for (i = 0; ch; i++)				\
    {						\
      N[i] = ch;				\
      Getc(ch, infile);				\
    }						\
  N[i] = 0;					\
}
			 
#if defined(THINK_C)
#  define Getw(STREAM)       mygetw(STREAM)
#else
#  define Getw(STREAM)       getw(STREAM)
#endif

#define Getc(To,STREAM)    { To = getc(STREAM); }

char *qload_opcode_names[QLOAD_PRED_END+1] = {
  "qload_end_marker",
  "qload_pred_start",
  "qload_word",
  "qload_n_bytecode",
  "qload_def",
  "qload_atom",
  "qload_functor",
  "qload_float",
  "qload_integer",
  "qload_start_table",
  "qload_end_table",
  "qload_pred_end"
};

static u32 mygetw(stream)
    FILE *stream;
{
  char word[4];

  Getc(word[3],stream);
  Getc(word[2],stream);
  Getc(word[1],stream);
  Getc(word[0],stream);

  return *((u32 *) word);
}

BOOL quickload(infile,w)
    FILE *infile;
    worker *w;
{
  TAGGED predname;
  char name[MAXATOMLEN];
  int arity;
  int instruction;
  code *switch_start;

 start:
  {
    Getc(instruction,infile);

    switch(instruction) {

    case QLOAD_END_MARKER:
	Error("qload - problem with format of qload file");
	return FALSE;

    case QLOAD_PRED_START:
	Fetch_Name(name);

	Getc(arity,infile);
	predname = store_atom(name,w);

	store_emulated_predicate(StoreFunctor(predname,arity),w->global->code_current,w);
	break;

    case QLOAD_WORD:
	Store_Tagged(Getw(infile));
	break;
	
    case QLOAD_N_BYTECODE:
	{
	  register code bar;

	  while((bar = Getw(infile)) != QLOAD_END_MARKER)
	    {
	      Store_Tagged(bar);
	    }
	}
	break;

    case QLOAD_DEF:
	Fetch_Name(name);
	Getc(arity,infile);
	Store_Tagged((TAGGED)
		     get_definition(StoreFunctor(store_atom(name,w),arity),w));
        break;

    case QLOAD_ATOM:
	Fetch_Name(name);
	Store_Tagged(store_atom(name,w));
	break;

    case QLOAD_FUNCTOR:
	Fetch_Name(name);
	Getc(arity,infile);
	Store_Tagged(StoreFunctor(store_atom(name,w),arity));
	break;

    case QLOAD_FLOAT:
	{
	  float tmpflt;
	  Fetch_Name(name);
	  sscanf(name,"%f",&tmpflt);
	  Store_Tagged(make_atomspace_float(tmpflt,w));
	}
	break;

    case QLOAD_INTEGER: 
	Fetch_Name(name);
	Store_Tagged(Make_Atomspace_Integer(w,name));
	break;

    case QLOAD_START_TABLE:
	switch_start = w->global->code_current;
	break;
	
    case QLOAD_END_TABLE:
	/* optional sorting */
	labelsort(switch_start,
		  (((long) w->global->code_current) - ((long) switch_start))/
		  (sizeof(TAGGED) + sizeof(code *)));
	break;

    case QLOAD_PRED_END:
	Store_Tagged(END_OF_PRED);
	if (w->global->flags.load_verbose) 
	    PL_Print3(stderr,"{ %s/%d quickloaded }\n", GetString(predname,w),
		      arity);
	break;

    case EOF:
	return TRUE;

    default:
	Error("qload - problem with format of qload file");
	return FALSE;
    }
    goto start;
  }
}

/* $qload/1 */
BOOL luther_qload(Arg)
    Argdecl;
{
  register TAGGED X0;
  FILE *infile;
  char filename[MAXPATHLEN];
  
  DerefNLL(X0,Xw(0));
  
  if (IsATM(X0))
    {
      if (not(expand_file_name(GetString(X0,w),filename)))
	return FALSE;

      if ((infile = fopen(filename, "rb")) == NULL)
	return luther_error(E_OPEN_FILE, X0,w);

      quickload(infile,w);
      fclose(infile);
      return TRUE;
    }
  else
    {
      return luther_error(E_FILE_SPEC,X0,w);
    }
}


