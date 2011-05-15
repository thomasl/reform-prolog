/*
 * inout.c 
 *
 * Johan Bevemyr.....Thu Jun  6 1991
 * Patric Hedlin.....Fri Sep 23 1994
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
#include "display.h"
#include "expand_file_name.h"
#include "ctype.h"
#include "char_table.h"

#if defined(THINK_C)
#  include <unix.h>
#endif


GLOBAL FILE *currin;
GLOBAL FILE *currout;
GLOBAL FILE *currerr;
GLOBAL IO_TYPE  currtty_io;

GLOBAL struct stream_node streams[MAXSTREAMS];


LOCAL TAGGED prompt;		/* '|: ' */
LOCAL TAGGED currinx;
LOCAL TAGGED curroutx;

LOCAL int first_user_stream;
LOCAL int currin_nl_last = 1;

LOCAL TAGGED atom_read;
LOCAL TAGGED atom_write;
LOCAL TAGGED atom_append;



/* prompt(?OldPrompt, ?NewPrompt)
 */
BOOL luther_prompt(Arg)
    Argdecl;
{
  TAGGED old_prompt, new_prompt;

  DerefNLL(old_prompt, Xw(0));
  DerefNLL(new_prompt, Xw(1));

  if (IsVar(new_prompt))
    {
      return unify(old_prompt, prompt, w);
    }
  else if (IsATM(new_prompt))
    {
      if (unify(old_prompt, prompt, w))
	{
	  prompt = new_prompt;
	  return TRUE;
	}
    }
  return FALSE;
}


/*******************************************************************************
 *
 * Terminal (tty) I/O:
 *
 */

/* ttynl
 */
BOOL luther_ttynl(Arg)
    Argdecl;
{
  putc('\n', stdout);
  return TRUE;
}

/* ttytab
 */
BOOL luther_ttytab(Arg)
    Argdecl;
{
  putc('\t', stdout);
  return TRUE;
}

/* ttyput(+Character)
 */
BOOL luther_ttyput(Arg)
    Argdecl;
{
  register TAGGED X0;

  DerefNLL(X0, Xw(0));

  if (IsNUM(X0))
    putc((char) GetNumber(X0), stdout);
  else
    (void) luther_error(E_PUT_ARG, X0, w);
  
  return TRUE;
}

/* ttyget(?Character)
 */
BOOL luther_ttyget(Arg)
    Argdecl;
{
  register TAGGED X0;
  register int i;

  DerefNLL(X0, Xw(0));

  do {
    i = getc(stdin);
  } while(!isprint(i) && i != EOF);

  if (i == EOF)
    clearerr(stdin);

  return unify(X0,Make_Integer(i), w);
}

/* ttyget0(?Character)
 */
BOOL luther_ttyget0(Arg)
    Argdecl;
{
  register TAGGED X0;
  register int i;

  DerefNLL(X0,Xw(0));
  
  if ((i = getc(stdin)) == EOF)
    clearerr(stdin);

  return unify(X0, Make_Integer(i), w);
}

/* ttyskip(+Character)
 */
BOOL luther_ttyskip(Arg)
    Argdecl;
{
  register TAGGED Ch;
  register int i,skipchar;

  DerefNLL(Ch,Xw(0));

  if (not(IsNUM(Ch)))
    BuiltinError("ttyskip", " - illegal first argument", "");

  skipchar = GetNumber(Ch);
    
  do {
    i = getc(stdin);
  } while(i != skipchar && i != EOF);

  if (i == EOF)
    clearerr(stdin);
    
  return TRUE;
}

/* ttyflush
 */
BOOL luther_ttyflush(Arg)
    Argdecl;
{
  fflush(stdout);
  return TRUE;
}


/* $ttygetch0(?Type,?Character) -- used by interpret.pl
 */
BOOL luther_ttygetch0(Arg)
    Argdecl;
{
  register TAGGED Ch,Ty;
  register int i;
    
  DerefNLL(Ty,Xw(0));
  DerefNLL(Ch,Xw(1));

  if (not(IsNUM(Ty) || IsVar(Ty)))
    BuiltinError("'$ttygetch0'", " - illegal first argument", "");

  if (not(IsNUM(Ch) || IsVar(Ch)))
    BuiltinError("'$ttygetch0'", " - illegal second argument", "");

  if ((currtty_io == LUTHER_IO_TTY) && (currin_nl_last == 1))
    {
      currin_nl_last = 0;
    }

  i = getc(stdin);

  if ((currtty_io == LUTHER_IO_TTY) && (i == '\n'))
    currin_nl_last = 1;

  if (i == EOF)
    clearerr(stdin);
    
  if (unify(Ch,Make_Integer(i),w) &&
      unify(Ty,Make_Integer(GetCharCode(i)),w))
    return TRUE;
  else
    return FALSE;
}


/*******************************************************************************
 *
 * Implicit stream I/O: (using luther_stdin and luther_stdout)
 *
 */

/* nl
 */
BOOL luther_nl(Arg)
    Argdecl;
{
  putc('\n', currout);
  return TRUE;
}

/* tab
 */
BOOL luther_tab(Arg)
    Argdecl;
{
  putc('\t', currout);
  return TRUE;
}

/* put(+Character)
 */
BOOL luther_put(Arg)
    Argdecl;
{
  register TAGGED X0;

  DerefNLL(X0,Xw(0));

  if (IsNUM(X0))
    putc((char) GetNumber(X0), currout);
  else
    (void) luther_error(E_PUT_ARG,X0,w);

  return TRUE;
}

/* get(?Character)
 */
BOOL luther_get(Arg)
    Argdecl;
{
  register TAGGED X0;
  register int i;

  DerefNLL(X0,Xw(0));

  do {
    if ((currtty_io == LUTHER_IO_TTY) && (currin_nl_last == 1))
      {
	display_term(currout, prompt, w);
	fflush(currout);
	currin_nl_last = 0;
      }
    i = getc(currin);

    if ((currtty_io == LUTHER_IO_TTY) && (i == '\n'))
      currin_nl_last = 1;

  } while(!isprint(i) && i != EOF);

  if ((i == EOF) && (currtty_io == LUTHER_IO_TTY)) 
    {
      clearerr(currin);
      currin_nl_last = 1;
    }
  return unify(X0,Make_Integer(i),w);
}

/* get0(?Character)
 */
BOOL luther_get0(Arg)
    Argdecl;
{
  register TAGGED X0;
  register int i;

  DerefNLL(X0,Xw(0));

  if ((currtty_io == LUTHER_IO_TTY) && (currin_nl_last == 1))
    {
      display_term(currout, prompt, w);
      fflush(currout);
      currin_nl_last = 0;
    }

  i = getc(currin);

  if (currtty_io == LUTHER_IO_TTY)
    {
      if (i == '\n')
	{
	  currin_nl_last = 1;
	}
      else if (i == EOF)
	{
	  clearerr(currin);
	  currin_nl_last = 1;
	}
    }
  return unify(X0,Make_Integer(i),w);
}

/* skip(+Character)
 */
BOOL luther_skip(Arg)
    Argdecl;
{
  register TAGGED Ch;
  register int i,skipchar;

  DerefNLL(Ch,Xw(0));

  if (not(IsNUM(Ch)))
    BuiltinError("skip", " - illegal first argument", "");

  skipchar = GetNumber(Ch);
    
  do {
    if ((currtty_io == LUTHER_IO_TTY) && (currin_nl_last == 1))
      {
	display_term(currout, prompt, w);
	fflush(currout);
	currin_nl_last = 0;
      }
    i = getc(currin);

    if ((currtty_io == LUTHER_IO_TTY) && (i == '\n'))
      currin_nl_last = 1;

  } while( i != skipchar && i != EOF);

  if ((i == EOF) && (currtty_io == LUTHER_IO_TTY)) 
    {
      clearerr(currin);
      currin_nl_last = 1;
    }
  return TRUE;
}

/* flush
 */
BOOL luther_flush(Arg)
    Argdecl;
{
  fflush(currout);
  return TRUE;
}


/* '$getch'(?Type,?Character) -- used by tokenizer.pl
 */
BOOL luther_getch(Arg)
    Argdecl;
{
  register TAGGED Ch,Ty;
  register int i;
    
  DerefNLL(Ty,Xw(0));
  DerefNLL(Ch,Xw(1));

  assert(IsNUM(Ty) || IsVar(Ty));
  assert(IsNUM(Ch) || IsVar(Ch));

  do {
    if ((currtty_io == LUTHER_IO_TTY) && (currin_nl_last == 1))
      {
	display_term(currout, prompt, w);
	fflush(currout);
	currin_nl_last = 0;
      }
    i = getc(currin);

    if ((currtty_io == LUTHER_IO_TTY) && (i == '\n'))
      currin_nl_last = 1;

  } while(not(isprint(i)) && i != EOF);

  if ((i == EOF) && (currtty_io == LUTHER_IO_TTY))
    {
      currin_nl_last = 1;
      clearerr(currin);
    }

  if (unify(Ch, Make_Integer(i),w) &&
      unify(Ty, Make_Integer(GetCharCode(i)), w))
    return TRUE;
  else
    return FALSE;
}

/* '$getch0'(?Type,?Character) -- used by tokenizer.pl
 */
BOOL luther_getch0(Arg)
    Argdecl;
{
  register TAGGED Ch,Ty;
  register int i;
    
  DerefNLL(Ty,Xw(0));
  DerefNLL(Ch,Xw(1));

  assert(IsNUM(Ty) || IsVar(Ty));
  assert(IsNUM(Ch) || IsVar(Ch));

  if ((currtty_io == LUTHER_IO_TTY) && (currin_nl_last == 1))
    {
      display_term(currout, prompt, w);
      fflush(currout);
      currin_nl_last = 0;
    }

  i = getc(currin);

  if ((currtty_io == LUTHER_IO_TTY))
    {
      if (i == '\n')
	{
	  currin_nl_last = 1;
	}
      else if (i == EOF)
	{
	  clearerr(currin);
	  currin_nl_last = 1;
	}
    }
    
  if (unify(Ch, Make_Integer(i),w) &&
      unify(Ty, Make_Integer(GetCharCode(i)), w))
    return TRUE;
  else
    return FALSE;
}


/* '$display'(?Term)
 */
BOOL luther_display(Arg)
    Argdecl;
{
  display_term(currout,Xw(0),w);
  return TRUE;
}

/* '$write'(?Term)
 */
BOOL luther_write(Arg)
    Argdecl;
{
  write_term(currout, Xw(0),w);
  return TRUE;
}

/* '$write_float'(+Format,+Number,+Precision)
 */
BOOL luther_write_float(Arg)
    Argdecl;
{
  TAGGED format, number, precision;

  DerefNLL(format,    Xw(0));
  DerefNLL(number,    Xw(1));
  DerefNLL(precision, Xw(2));

  assert(IsNUM(format) && IsNUM(precision));

  write_float(currout, number, GetNumber(format), GetNumber(precision), w);
  return TRUE;
}

/* '$write_radix'(+Format,+Number,+Radix)
 */
BOOL luther_write_radix(Arg)
    Argdecl;
{
  TAGGED format, number, radix;

  DerefNLL(format, Xw(0));
  DerefNLL(number, Xw(1));
  DerefNLL(radix,  Xw(2));

  assert(IsNUM(format) && IsNUM(radix));

  write_radix(currout, number, GetNumber(format), GetNumber(radix), w);
  return TRUE;
}

/* '$write_integer'(+Format,+Number,+Precision)
 */
BOOL luther_write_integer(Arg)
    Argdecl;
{
  TAGGED format, number, precision;

  DerefNLL(format,    Xw(0));
  DerefNLL(number,    Xw(1));
  DerefNLL(precision, Xw(2));

  assert(IsNUM(format) && IsNUM(precision));

  write_integer(currout, number, GetNumber(format), GetNumber(precision), w);
  return TRUE;
}


/***********************************************************************
 *
 * Explicit stream I/O:
 *
 */

/* nl(+Stream)
 */
BOOL luther_nl_stream(Arg)
    Argdecl;
{
  register TAGGED Stream;
  register FILE *stream;
    
  DerefNLL(Stream, Xw(0));
  GetOutStream(Stream, stream);
    
  putc('\n',stream);
  return TRUE;
}

/* tab(+Stream)
 */
BOOL luther_tab_stream(Arg)
    Argdecl;
{
  register TAGGED Stream;
  register FILE *stream;
    
  DerefNLL(Stream, Xw(0));
  GetOutStream(Stream, stream);
    
  putc('\t',stream);
  return TRUE;
}

/* put(+Stream,+Character)
 */
BOOL luther_put_stream(Arg)
    Argdecl;
{
  register TAGGED Stream, Ch;
  register FILE *stream;
  
  DerefNLL(Stream,Xw(0));
  GetOutStream(Stream,stream);

  DerefNLL(Ch,Xw(1));

  if (IsNUM(Ch))
    putc((char) GetNumber(Ch), stream);
  else
    (void) luther_error(E_PUT_ARG,Ch,w);

  return TRUE;
}

/* get(+Stream,?Character)
 */
BOOL luther_get_stream(Arg)
    Argdecl;
{
  register TAGGED Stream, Ch;
  register FILE *stream;
  register int i;
  register IO_TYPE tty_io;
    
  DerefNLL(Stream,Xw(0));
  GetInStream(Stream,stream,tty_io);

  DerefNLL(Ch,Xw(1));

  do {
    i = getc(stream);
  } while(!isprint(i) && i != EOF);

  if (tty_io == LUTHER_IO_TTY)
    {
      if (i == (int) '\n')
	{
	  currin_nl_last = 1;
	}
      else if (i == EOF)
	{
	  clearerr(stream);
	  currin_nl_last = 1;
	}
    }
  return unify(Ch,Make_Integer(i),w);
}

/* get0(+Stream,?Character)
 */
BOOL luther_get0_stream(Arg)
    Argdecl;
{
  register TAGGED Stream, Ch;
  register FILE *stream;
  register IO_TYPE tty_io;
  register int i;
    
  DerefNLL(Stream,Xw(0));
  GetInStream(Stream,stream,tty_io);

  DerefNLL(Ch,Xw(1));
  i = getc(stream);

  if (tty_io == LUTHER_IO_TTY)
    {
      if (i == (int) '\n')
	{
	  currin_nl_last = 1;
	}
      else if (i == EOF)
	{
	  clearerr(stream);
	  currin_nl_last = 1;
	}
    }
  return unify(Ch, Make_Integer(i),w);
}

/* skip(+Stream,+Character)
 */
BOOL luther_skip_stream(Arg)
    Argdecl;
{
  register TAGGED Stream, Ch;
  register FILE *stream;
  register IO_TYPE tty_io;
  register int i, skipchar;

  DerefNLL(Stream, Xw(0));
  GetInStream(Stream, stream, tty_io);

  DerefNLL(Ch, Xw(1));

  if (not(IsNUM(Ch)))
    BuiltinError("skip/2", " - illegal second argument", "");

  skipchar = GetNumber(Ch);
    
  do {
    i = getc(stream);
  } while(i != skipchar && i != EOF);

  if (tty_io == LUTHER_IO_TTY)
    {
      if (i == (int) '\n')
	{
	  currin_nl_last = 1;
	}
      else if (i == EOF)
	{
	  clearerr(stream);
	  currin_nl_last = 1;
	}
    }
  return TRUE;
}

/* flush(+Stream)
 */
BOOL luther_flush_stream(Arg)
    Argdecl;
{
  register TAGGED Stream;
  register FILE *stream;
    
  DerefNLL(Stream,Xw(0));
  GetOutStream(Stream,stream);

  fflush(stream);
  return TRUE;
}


/* '$display'(+Stream,?Term)
 */
BOOL luther_display_stream(Arg)
    Argdecl;
{
  register TAGGED Stream;
  register FILE *stream;
    
  DerefNLL(Stream, Xw(0));
  GetOutStream(Stream, stream);

  display_term(stream, Xw(1), w);
  return TRUE;
}

/* '$write'(+Stream,?Term)
 */
BOOL luther_write_stream(Arg)
    Argdecl;
{
  register TAGGED Stream;
  register FILE *stream;
  
  DerefNLL(Stream, Xw(0));
  GetOutStream(Stream, stream);

  write_term(stream, Xw(1),w);
  return TRUE;
}


/***********************************************************************
 *
 * Primitives for manipulating streams.
 *
 */
    
/* get_input(-Stream)
 */
BOOL luther_get_input(Arg)
    Argdecl;
{
  register TAGGED X0;

  DerefNLL(X0, Xw(0));

  return unify(X0, currinx, w);
}

/* set_input(+Stream)
 */
BOOL luther_set_input(Arg)
    Argdecl;
{
  register TAGGED X0;
  register int index;

  DerefNLL(X0, Xw(0));

  GetStreamInIndex(X0, index);

  currin = streams[index].file;
  currtty_io = streams[index].tty_io;
  currinx = X0;

  return TRUE;
}

/* get_output(-Stream)
 */
BOOL luther_get_output(Arg)
    Argdecl;
{
  register TAGGED X0;

  DerefNLL(X0, Xw(0));

  return unify(X0, curroutx, w);
}

/* set_output(+Stream)
 */
BOOL luther_set_output(Arg)
    Argdecl;
{
  register TAGGED X0;
  register int index;

  DerefNLL(X0, Xw(0));

  GetStreamOutIndex(X0, index);

  currout = streams[index].file;
  curroutx = X0;

  return TRUE;
}


/*******************************************************************************
 *
 * Basic stream support: (open, close and rewind)
 *
 *
 */

/*
 * open(+FileName, +Mode, -Stream)
 *
 *   open/3 opens a file, the first argument is a file name, the second 
 * argument is either 'read', 'write' or 'append', and the third argument 
 * is unified with the resulting stream (on success).
 *
 */
BOOL luther_open(Arg)
    Argdecl;
{
  register TAGGED X0, X1, X2;

  char *file_mode;
  
  DerefNLL(X0, Xw(0));
  DerefNLL(X1, Xw(1));
  DerefNLL(X2, Xw(2));

  if (not(IsATM(X0)))
    BuiltinError("open/3", " - illegal first argument", "");

  if (not(IsATM(X1)))
    BuiltinError("open/3", " - illegal second argument", "");

  
  if (X1 == atom_read)
    file_mode = "r";
  else if (X1 == atom_write)
    file_mode = "w";
  else if (X1 == atom_append)
    file_mode = "a";
  else
    BuiltinError("open/3", " - illegal file mode - ", GetString(X1,w));


  if (X0 == atom_user_output ||
      X0 == atom_user_input  ||
      X0 == atom_user)
    {
      register TAGGED strref;
      register FILE *fp;
      register int i, fd;
	      
      for (i = 0; streams[i].file != NULL && i < MAXSTREAMS; i++);
	      
      if (i == MAXSTREAMS)
	{
	  return luther_error(E_NR_FILES, X0, w);
	}
	      
      if (X1 == atom_read)
	{
	  fd = fileno(streams[LUTHER_STDIN].file);
	}
      else
	{
	  fd = fileno(streams[LUTHER_STDOUT].file);
	}
	      
      if ((fp = fdopen(fd, file_mode)) == NULL)
	{
	  return luther_error(E_OPEN_FILE,X0,w);
	}
	      
      streams[i].file = fp;
      streams[i].name = X0;
      streams[i].mode = X1;
      streams[i].tty_io = LUTHER_IO_TTY;
	      
      Make_STR(w->heap_top, strref, functor_d_stream);
      PushOnHeap(w->heap_top, Make_Integer(i));

      return unify(X2, strref, w);
    }
  else
    {
      register TAGGED strref;
      register FILE *fp;
      register int i;

      char path[MAXPATHLEN+1];
	      
      for (i = 0; streams[i].file != NULL && i < MAXSTREAMS; i++);
	      
      if (i == MAXSTREAMS)
	{
	  return luther_error(E_NR_FILES, X0, w);
	}
	      
      if (not(expand_file_name(GetString(X0, w), path)))
	return FALSE;
	      
      if ((fp = fopen(path, file_mode)) == NULL)
	{
	  return luther_error(E_OPEN_FILE,X0,w);
	}
	      
      streams[i].file = fp;
      streams[i].name = X0;
      streams[i].mode = X1;
      streams[i].tty_io = LUTHER_IO_FILE;
	      
      Make_STR(w->heap_top, strref, functor_d_stream);
      PushOnHeap(w->heap_top, Make_Integer(i));

      return unify(X2,strref,w);
    }
}


/* close(+Stream)
 */
BOOL luther_close(Arg)
    Argdecl;
{
  register TAGGED X0;
  register int index;

  DerefNLL(X0,Xw(0));

  GetStreamIndex(X0,index);

  if ((MAXSTREAMS > index) && (index >= first_user_stream))
    {
      switch (streams[index].tty_io)
	{
	case LUTHER_IO_FILE:
	  fclose(streams[index].file);
	  break;
	case LUTHER_IO_POPEN:
	  pclose(streams[index].file);
	  break;
	}

      streams[index].file = NULL;
      streams[index].name = (TAGGED) NULL;
      streams[index].mode = (TAGGED) NULL;
      streams[index].tty_io = (TAGGED) NULL;
    }
  return TRUE;
}


/* rewind(+Stream)
 */
BOOL luther_rewind(Arg)
    Argdecl;
{
  register TAGGED X0;
  register int index;

  DerefNLL(X0,Xw(0));

  GetStreamIndex(X0,index);

  if (streams[index].file != NULL)
    {
      rewind(streams[index].file);
    }
  return TRUE;
}


/*******************************************************************************
 *
 * Private stream support:
 *
 *
 */

/*
 * stream_list(?StreamIndexList)
 *
 *   stream_list/1 unifies the StreamIndexList with a list containing
 * the indexes of all streams currently open.
 *
 */
BOOL luther_stream_list(Arg)
    Argdecl;
{
  register TAGGED X0;
  TAGGED lst, rest;
  int i;

  extern TAGGED atom_nil;

  DerefNLL(X0, Xw(0));
  rest = atom_nil;

  for (i = first_user_stream; i < MAXSTREAMS; i++)
    {
      if (streams[i].file != NULL)
	{
	  lst = Tagify(w->heap_top,LST);
	  PushOnHeap(w->heap_top,Make_Integer(i));
	  PushOnHeap(w->heap_top,rest);
	  rest = lst;
	}
    }
  return unify(X0, rest,w);
}


BOOL luther_stream_name(Arg)
    Argdecl;
{
  register TAGGED X0, X1;
  register int index;
  
  DerefNLL(X0,Xw(0));
  DerefNLL(X1,Xw(1));
  
  GetStreamIndex(X0,index);
  
  if (streams[index].name != (TAGGED) NULL)
    return unify(X1, streams[index].name,w);
  else
    return FALSE;
}

BOOL luther_stream_mode(Arg)
    Argdecl;
{
  register TAGGED X0,X1;
  register int index;
  
  DerefNLL(X0,Xw(0));
  DerefNLL(X1,Xw(1));
  
  GetStreamIndex(X0,index);
  
  if (streams[index].mode != (TAGGED) NULL)
    return unify(X1,Make_Integer(streams[index].mode),w);
  else
    return FALSE;
}

BOOL luther_stream_file(Arg)
    Argdecl;
{
  register TAGGED X0,X1;
  register int index;

  DerefNLL(X0,Xw(0));
  DerefNLL(X1,Xw(1));

  GetStreamIndex(X0,index);

  if (streams[index].file != NULL)
    return unify(X1,PointerToTerm(streams[index].file),w);
  else
    return FALSE;
}

/* */

#if ! defined(THINK_C)

BOOL luther_stream_code(Arg)
    Argdecl;
{
  register TAGGED X0,X1;
  register int index;

  DerefNLL(X0,Xw(0));
  DerefNLL(X1,Xw(1));

  GetStreamIndex(X0,index);

  if (streams[index].file != NULL)
    return unify(X1,PointerToTerm(fileno(streams[index].file)),w);
  else
    return FALSE;
}

BOOL luther_file_code(Arg)
    Argdecl;
{
  register TAGGED X0,X1;
  register int index;

  DerefNLL(X0,Xw(0));
  DerefNLL(X1,Xw(1));

  GetStreamIndex(X0,index);

  if (streams[index].file != NULL)
    return unify(X1,Make_Integer(fileno(streams[index].file)),w);
  else
    return FALSE;
}


/*
 * '$fdopen'(+FileDescriptor, +Mode, -Stream)
 *
 *   '$fdopen'/3 opens a file, the first argument is a file descripor, the second
 * argument is either 'read', 'write' or 'append', and the third argument is unified
 * with the resulting stream (on success).
 *
 */
BOOL luther_fdopen(Arg)
    Argdecl;
{
  register TAGGED X0, X1, X2, strref;
  int i;
  FILE *fp;

  char *file_mode;

  DerefNLL(X0, Xw(0));
  DerefNLL(X1, Xw(1));
  DerefNLL(X2, Xw(2));

  if (not(IsNUM(X0)))
    BuiltinError("'$fdopen'/3", " - illegal first argument", "");

  if (not(IsATM(X1)))
    BuiltinError("'$fdopen'/3", " - illegal second argument", "");


  if (X1 == atom_read)
    file_mode = "r";
  else if (X1 == atom_write)
    file_mode = "w";
  else if (X1 == atom_append)
    file_mode = "a";
  else
    BuiltinError("'$fdopen'/3", " - illegal file mode - ", GetString(X1,w));


  for (i = 0; streams[i].file != NULL && i < MAXSTREAMS; i++);

  if (i == MAXSTREAMS)
    {
      return luther_error(E_NR_FILES, X0, w);
    }

  if ((fp = fdopen(GetNumber(X0), file_mode)) == NULL)
    {
      return luther_error(E_OPEN_FILE,X0,w);
    }

  streams[i].file = fp;
  streams[i].name = (TAGGED) NULL;
  streams[i].mode = X1;
  streams[i].tty_io = LUTHER_IO_FILE;

  Make_STR(w->heap_top, strref, functor_d_stream);
  PushOnHeap(w->heap_top, Make_Integer(i));

  return unify(X2, strref, w);
}
#endif

/**********************************************************************
 * Buffered IO primitive support
 */

buff_io_stream *init_io_stream(fid,w)
     int fid;
     worker *w;
{
  buff_io_stream *inout;

  inout = (buff_io_stream *) atom_alloc(sizeof(buff_io_stream),w);
  inout->buffsize = IOBUFFSIZE;

  inout->buffer = (char *) atom_alloc(inout->buffsize,w);

  inout->fid = fid;
  inout->count = inout->max = 0;

  return inout;
}

int buff_getchar(in)
     buff_io_stream *in;
{
  if (in->count == in->max)
    {
      in->max = read(in->fid,in->buffer,in->buffsize);

      if (in->max <= 0)
	{
	  /* end of file has been reached, exit */
	  return EOF;
	}

      in->count = 1;
      return (int) in->buffer[0];
    }
  else
    {
      return (int) in->buffer[in->count++];
    }
}


void flush_write_buff(out)
     buff_io_stream *out;
{
  write(out->fid, out->buffer,out->count);
  out->count = 0;
}  

void writebuff(out, x)
     buff_io_stream *out;
     char *x;
{
  register int size, i;

  size = strlen(x);

  if(out->count + size > out->buffsize)
    {
      flush_write_buff(out);
    }

  for(i=0 ; i < size ; i++)
    {
      out->buffer[out->count+i] = x[i];
    }

  out->count += size;
}

void readline(in, x)
     buff_io_stream *in;
     char *x;
{
  int i = 0;

  x[0] = buff_getchar(in);

  while((x[i] != '\n') && (x[i] != EOF) && (i < 255))
    {
      if (x[i] == 8) /* rubbout */
	{
	  if (i>0)
	    i -= 1;
	}
      else
	{
	  i += 1;
	}
      x[i] = buff_getchar(in);
    }

  i += 1;
  x[i] = 0;
}


/**********************************************************************
 *
 * prolog predicates 
 *
 */ 

void initialize_inout(w)
     worker *w;
{
  streams[LUTHER_STDIN].file = stdin;
  streams[LUTHER_STDIN].name = atom_user_input;
  streams[LUTHER_STDIN].mode = atom_read;
  streams[LUTHER_STDIN].tty_io = LUTHER_IO_TTY;
  
  streams[LUTHER_STDOUT].file = stdout;
  streams[LUTHER_STDOUT].name = atom_user_output;
  streams[LUTHER_STDOUT].mode = atom_write;
  streams[LUTHER_STDOUT].tty_io = LUTHER_IO_TTY;
  
  streams[LUTHER_STDERR].file = stderr;
  streams[LUTHER_STDERR].name = atom_user_error;
  streams[LUTHER_STDERR].mode = atom_write;
  streams[LUTHER_STDERR].tty_io = LUTHER_IO_TTY;
  
  first_user_stream = LUTHER_USER_FILE;
  
  currin  = stdin;
  currout = stdout;
  currerr = stderr;

  currtty_io = LUTHER_IO_TTY;
  
  currinx  = atom_user_input;
  curroutx = atom_user_output;

  atom_read   = atom_table_tagged[ATOM_READ];
  atom_write  = atom_table_tagged[ATOM_WRITE];
  atom_append = atom_table_tagged[ATOM_APPEND];

  prompt = store_atom("|: ", w);


  def_c_pred(ATOM_PROMPT, luther_prompt, 2, PUBLIC, w);

  /* terminal (tty) I/O */

  def_c_pred(ATOM_TTYNL, luther_ttynl, 0, PUBLIC, w);
  def_c_pred(ATOM_TTYTAB, luther_ttytab, 0, PUBLIC, w);
  def_c_pred(ATOM_TTYPUT, luther_ttyput, 1, PUBLIC, w);
  def_c_pred(ATOM_TTYGET, luther_ttyget, 1, PUBLIC, w);
  def_c_pred(ATOM_TTYGET0, luther_ttyget0, 1, PUBLIC, w);
  def_c_pred(ATOM_TTYSKIP, luther_ttyskip, 1, PUBLIC, w);
  def_c_pred(ATOM_TTYFLUSH, luther_ttyflush, 0, PUBLIC, w);

  def_c_pred(ATOM_D_TTYGETCH0, luther_ttygetch0, 2, PROLOG, w);

  /* implicit stream I/O */

  def_c_pred(ATOM_NL, luther_nl, 0, PUBLIC, w);
  def_c_pred(ATOM_TAB, luther_tab, 0, PUBLIC, w);
  def_c_pred(ATOM_PUT, luther_put, 1, PUBLIC, w);
  def_c_pred(ATOM_GET, luther_get, 1, PUBLIC, w);
  def_c_pred(ATOM_GET0, luther_get0, 1, PUBLIC, w);
  def_c_pred(ATOM_SKIP, luther_skip, 1, PUBLIC, w);
  def_c_pred(ATOM_FLUSH, luther_flush, 0, PUBLIC, w);

  def_c_pred(ATOM_D_GETCH, luther_getch, 2, PROLOG, w);
  def_c_pred(ATOM_D_GETCH0, luther_getch0, 2, PROLOG, w);

  def_c_pred(ATOM_D_WRITE, luther_write, 1, PROLOG, w);
  def_c_pred(ATOM_D_DISPLAY, luther_display, 1, PROLOG, w);

  def_c_pred(ATOM_D_WRITE_FLOAT, luther_write_float, 3, PROLOG, w);
  def_c_pred(ATOM_D_WRITE_RADIX, luther_write_radix, 3, PROLOG, w);
  def_c_pred(ATOM_D_WRITE_INTEGER, luther_write_integer, 3, PROLOG, w);

  /* explicit stream I/O */

  def_c_pred(ATOM_NL, luther_nl_stream, 1, PUBLIC, w);
  def_c_pred(ATOM_TAB, luther_tab_stream, 1, PUBLIC, w);
  def_c_pred(ATOM_PUT, luther_put_stream, 2, PUBLIC, w);
  def_c_pred(ATOM_GET, luther_get_stream, 2, PUBLIC, w);
  def_c_pred(ATOM_GET0, luther_get0_stream, 2, PUBLIC, w);
  def_c_pred(ATOM_SKIP, luther_skip_stream, 2, PUBLIC, w);
  def_c_pred(ATOM_FLUSH, luther_flush_stream, 1, PUBLIC, w);

  def_c_pred(ATOM_D_WRITE, luther_write_stream, 2, PROLOG, w);
  def_c_pred(ATOM_D_DISPLAY, luther_display_stream, 2, PROLOG, w);

  /* basic stream support */

  def_c_pred(ATOM_GET_INPUT, luther_get_input, 1, PUBLIC, w);
  def_c_pred(ATOM_SET_INPUT, luther_set_input, 1, PUBLIC, w);

  def_c_pred(ATOM_GET_OUTPUT, luther_get_output, 1, PUBLIC, w);
  def_c_pred(ATOM_SET_OUTPUT, luther_set_output, 1, PUBLIC, w);

  def_c_pred(ATOM_OPEN, luther_open, 3, PUBLIC, w);
  def_c_pred(ATOM_CLOSE, luther_close, 1, PUBLIC, w);
  def_c_pred(ATOM_REWIND, luther_rewind, 1, PUBLIC, w);

  /* various stream support */

  def_c_pred(ATOM_D_STREAM_LIST, luther_stream_list, 1, PROLOG, w);
  def_c_pred(ATOM_D_STREAM_NAME, luther_stream_name, 2, PROLOG, w);
  def_c_pred(ATOM_D_STREAM_MODE, luther_stream_mode, 2, PROLOG, w);
  def_c_pred(ATOM_D_STREAM_FILE, luther_stream_file, 2, PROLOG, w);

#if ! defined(THINK_C)
  def_c_pred(ATOM_D_STREAM_CODE, luther_stream_code, 2, PROLOG, w);
  def_c_pred(ATOM_D_FILE_CODE, luther_file_code, 2, PROLOG, w);
  def_c_pred(ATOM_D_FDOPEN, luther_fdopen, 3, PROLOG, w);
#endif
}
