/*
 * main.h  
 *
 * Johan Bevemyr.....Thu Sep 12 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef MAIN_H
#define MAIN_H


#include "luther.h"
#include "engine.h"
#include "expand_file_name.h"
#include "load.h"
#include "qload.h"
#include "inline.h"
#include "debug.h"
#include "database.h"


#if defined(PARALLEL)
#  define NUMBER_OF_WORKERS	2
#else
#  define NUMBER_OF_WORKERS	1
#endif


#define LineArgument(option, follow)         	\
  if (strcmp(argv[i], option) == STRCMP_EQUAL)	\
    if (argc < i + follow)			\
      goto error; 				\
    else            				\


#endif /* MAIN_H */

