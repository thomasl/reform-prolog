/*
 * inout.h
 *
 * Johan Bevemyr.....Wed Jun  5 1991
 * Patric Hedlin.....Mon Oct 17 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef INOUT_H
#define INOUT_H


enum {
  LUTHER_STDIN,
  LUTHER_STDOUT,
  LUTHER_STDERR,
  LUTHER_USER_FILE
};

typedef enum {
  LUTHER_IO_TTY,
  LUTHER_IO_FILE,
  LUTHER_IO_POPEN
} IO_TYPE;

typedef struct stream_node {
  FILE *file;
  TAGGED name;
  TAGGED mode;
  IO_TYPE tty_io;
} stream_t;

typedef struct {
  int fid;
  int count;
  int max;
  int buffsize;
  char *buffer;
} buff_io_stream;

#define IOBUFFSIZE  16 /* (10 KBYTES) */

#define IsStream(S)  (IsSTR(S) && (GetFunctor(S) == functor_d_stream))

#define IsUserIn(S)  (IsATM(S) && (S == atom_user || S == atom_user_input))
#define IsUserOut(S) (IsATM(S) && (S == atom_user || S == atom_user_output))
#define IsUserErr(S) (IsATM(S) && (S == atom_user_error))


#define GetInStream(Term,FromStream,TTY_IO)	\
{						\
  if (IsStream(Term))				\
    {						\
      TAGGED StIndex;				\
      int index;				\
      DerefNLL(StIndex,*GetArg(Term,0));	\
      index = GetNumber(StIndex);		\
      FromStream = streams[index].file;		\
      TTY_IO = streams[index].tty_io;		\
    }						\
  else if (IsUserIn(Term))			\
    {						\
      FromStream = streams[LUTHER_STDIN].file;	\
      TTY_IO = LUTHER_IO_TTY;			\
    }						\
  else						\
    {						\
      luther_error(E_ILLEGAL_IN_STREAM,Term,w);	\
      return FALSE;				\
    }						\
}

#define GetOutStream(Term,ToStream)			\
{							\
  if (IsStream(Term))					\
    {							\
      TAGGED StIndex;					\
      DerefNLL(StIndex,*GetArg(Term,0));		\
      ToStream = streams[GetNumber(StIndex)].file;	\
    }							\
  else if (IsUserOut(Term))				\
    {							\
      ToStream = streams[LUTHER_STDOUT].file;		\
    }							\
  else if (IsUserErr(Term))				\
    {							\
      ToStream = streams[LUTHER_STDERR].file;		\
    }							\
  else							\
    {							\
      return luther_error(E_ILLEGAL_OUT_STREAM,Term,w);	\
    }							\
}

#define GetStreamIndex(Term,Index)			\
{							\
  if (IsStream(Term))					\
    {							\
      TAGGED StIndex;					\
      DerefNLL(StIndex,*GetArg(Term,0));		\
      Index = GetNumber(StIndex);			\
    }							\
  else if (IsUserOut(Term))				\
    {							\
      Index = LUTHER_STDOUT;				\
    }							\
  else if (IsUserIn(Term))				\
    {							\
      Index = LUTHER_STDIN;				\
    }							\
  else if (IsUserErr(Term))				\
    {							\
      Index = LUTHER_STDERR;				\
    }							\
  else							\
    {							\
      return luther_error(E_ILLEGAL_STREAM,Term,w);	\
    }							\
}
    
#define GetStreamInIndex(Term,Index)			\
{							\
  if (IsStream(Term))					\
    {							\
      TAGGED StIndex;					\
      DerefNLL(StIndex,*GetArg(Term,0));		\
      Index = GetNumber(StIndex);			\
    }							\
  else if (IsUserIn(Term))				\
    {							\
      Index = LUTHER_STDIN;				\
    }							\
  else							\
    {							\
      return luther_error(E_ILLEGAL_IN_STREAM,Term,w);	\
    }							\
}

#define GetStreamOutIndex(Term,Index)			\
{							\
  if (IsStream(Term))					\
    {							\
      TAGGED StIndex;					\
      DerefNLL(StIndex,*GetArg(Term,0));		\
      Index = GetNumber(StIndex);			\
    }							\
  else if (IsUserOut(Term))				\
    {							\
      Index = LUTHER_STDOUT;				\
    }							\
  else if (IsUserErr(Term))				\
    {							\
      Index = LUTHER_STDERR;				\
    }							\
  else							\
    {							\
      return luther_error(E_ILLEGAL_OUT_STREAM,Term,w);	\
    }							\
}


extern FILE *currin;
extern FILE *currout;
extern FILE *currerr;

extern struct stream_node streams[MAXSTREAMS];


void initialize_inout PROTO((Argproto));


extern buff_io_stream *init_io_stream PROTO((int,worker *));
extern int buff_getchar PROTO((buff_io_stream *));
extern void writebuff PROTO((buff_io_stream *, char *));
extern void readline PROTO((buff_io_stream *, char *));


#endif /* INOUT_H */

