/*
 * functor_table.c
 *
 * Johan Bevemyr.....Tue Feb  8 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"

functor_bucket **functortable;

static functor_bucket *make_functor_bucket(name,arity,next,w)
     TAGGED name,arity;
     functor_bucket *next;
     worker *w;
{
  functor_bucket *res;

  res = (functor_bucket *) atom_alloc(sizeof(functor_bucket),w);
  res->name = name;
  res->arity= arity;
  res->next = next;
    
  return res;
}

/*
static int hash_functor(name,arity)
     TAGGED name,arity;
{
  u32 hval;

  hval = (u32) name + (u32) arity;

  return (int) (hval % FUNCTORHASHLEN);
}
*/

#define hash_functor(Name,Arity) \
                 ((int) ((((u32) Name>>2) + ((u32) Arity)) % FUNCTORHASHLEN))

TAGGED store_functor(name,arity,w)
     TAGGED name,arity;
     worker *w;
{
  register functor_bucket *bucket;
  register int hv;

  hv = hash_functor(name,arity); 

  for (bucket = functortable[hv] ;  
       bucket != NULL ;          
       bucket = bucket->next) 
    {
      if ((bucket->name == name) && (bucket->arity == arity))
	{
	  return PointerToTerm(bucket);
	}
    }

  /* add functor first in bucket */
  functortable[hv] = make_functor_bucket(name, arity, functortable[hv],w);

  return PointerToTerm(functortable[hv]);
}

void init_functortable(w)
    worker *w;
{
  int	i;

  functortable = w->global->functortable;

  for (i = 0; i != FUNCTORHASHLEN; i++)
    functortable[i] = NULL;
}
