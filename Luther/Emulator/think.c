/*
 * think.c - provide Think C specific things (kludges).
 *
 * Johan Bevemyr.....Thu Sep  3 1992
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#if defined(THINK_C)

#include <time.h>
#include <stdio.h>
#include <ansi_private.h>
#include <Files.h>
#include <Resources.h>
#include <Pascal.h>

#include "c.h"
#include "think.h"


long usertime()
{
  return (long)((1000 * clock()) / CLOCKS_PER_SEC);
}

long systime()
{
  return 0L;
}

double walltime()
{
  return 0.0;
}

extern char *get_application_name(buf)
    char *buf;
{
  FCBPBRec pb = {0 };
  pb.ioCompletion = 0;
  pb.ioVRefNum = 0;
  pb.ioRefNum = CurResFile();	/* will be this application */
  pb.ioFCBIndx = 0;
  pb.ioNamePtr = (StringPtr)buf;
  (void) PBGetFCBInfo(&pb, false /* !async */);

  return PtoCstr((StringPtr)buf);
}

#endif
