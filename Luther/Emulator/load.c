/*
 * load.c
 *
 * Johan Bevemyr.....Sun Jun  2 1991
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
#include "load.h"


extern int yyparse PROTO((void));
extern void yyrestart PROTO((FILE *));

extern worker *parse_worker;
extern FILE* yyin;

int parser_return_value;


void load_file(file, w)
    FILE *file;
    worker *w;
{
  if (yyin == NULL)
    {
      yyin = file;
    }
  else
    {
      yyrestart(file);
    }
  parse_worker = w;
  yyparse();
}

extern void luther_switch_to_buffer PROTO((s32));
extern long luther_save_current_buffer PROTO((void));
extern void luther_new_buffer PROTO((FILE *));
extern void luther_delete_buffer PROTO((s32));
extern void luther_save_linecount PROTO((s32));


/* $load/3 */
BOOL luther_load(Arg)
    Argdecl;
{
  register TAGGED State, Stream, Buf;
  FILE *loadfile;
  char filename[MAXPATHLEN];
  s32 buf;
  IO_TYPE tty_io;

  DerefNLL(State,Xw(0));
  DerefNLL(Stream,Xw(1));
  DerefNLL(Buf,Xw(2));

  GetInStream(Stream,loadfile,tty_io);
    
  yyin = loadfile;

  if (!IsNUM(Buf))
    {
      luther_new_buffer(loadfile);
      buf = luther_save_current_buffer();
      yyrestart(loadfile);
    }
  else
    {
      buf = GetNumber(Buf);
      luther_switch_to_buffer(buf);
    }

  parse_worker = w;
  parser_return_value = PARSE_EOF;
  yyparse();

  if (parser_return_value == PARSE_DIRECTIVE)
    {
      luther_save_linecount(buf);
      return (unify(Buf,Make_Integer(buf), w) &&
	      unify(State,atom_table_tagged[ATOM_DIRECTIVE],w));
    }
  else
    {
      luther_delete_buffer(buf);
      return unify(State,atom_table_tagged[ATOM_END_OF_FILE],w);
    }
}
