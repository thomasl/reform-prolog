/*
 * labelsort.c
 *
 * Johan Bevemyr.....Tue Feb 18 1992
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h" 
#include "labelsort.h"

#define BOUBLE 1

#define Swap(A,B)					\
{							\
  register labelt *a = (A), *b = (B);			\
  register TAGGED  tmpcon = a->constant;		\
  register integer tmpoff = a->offset;			\
							\
  a->constant = b->constant;				\
  a->offset = b->offset + (((long) b) - ((long) a));	\
  b->constant = tmpcon;					\
  b->offset = tmpoff - (((long) b) - ((long) a));	\
}
		    
/* currently this is bubble sort */

#if defined(BOUBLE)

void labelsort(el,size)
    code *el;
    int size;
{
  register labelt *element = (labelt *) el;
  register int i, k;

  /* bouble sort */

  k = size-1;

 start:
  if (k == 0) goto done;
    
  i = 0;
    
 inner:				/* This loop is entered when no shift has occured */
  if (i == k) {
    goto done;
  }
  if (element[i].constant > element[i+1].constant) {
    Swap(&(element[i]),&(element[i+1]));
    i++;
    goto inner_2;
  }
  i++;
  goto inner;

 inner_2:			/* This loop is entered when one shift has occured */
  if (i == k) {
    k--;
    goto start;
  }
  if (element[i].constant > element[i+1].constant) {
    Swap(&(element[i]),&(element[i+1]));
  }
  i++;
  goto inner_2;

 done:
  return;
}

#else

#define SortTwo(A,B)				\
if (element[A].constant > element[B].constant)	\
  {						\
    Swap(&(element[A]),&(element[B]));		\
  }			

void sortfour(element,start,end)
    register labelt *element;
    register int start;
{
  switch((end-start) + 1) {

  case 1:
    break;
  case 2:
    SortTwo(start,start+1);
    break;
  case 3:
    SortTwo(start,start+1);
    SortTwo(start+1,start+2);
    SortTwo(start,start+1);
    break;
  case 4:
    SortTwo(start,start+1);
    SortTwo(start+1,start+2);
    SortTwo(start+2,start+3);
    SortTwo(start,start+1);
    SortTwo(start+1,start+2);
    SortTwo(start,start+1);
    break;
  case 5:
    SortTwo(start,start+1);
    SortTwo(start+1,start+2);
    SortTwo(start+2,start+3);
    SortTwo(start+3,start+4);
    SortTwo(start,start+1);
    SortTwo(start+1,start+2);
    SortTwo(start+2,start+3);
    SortTwo(start,start+1);
    SortTwo(start+1,start+2);
    SortTwo(start,start+1);
    break;
  }
}

void quicksort(element,left,right)
    register labelt *element;
    register int left,right;
{
  register int i,k;
  register TAGGED v;

 start:
  if ((right - left) < 5)
    {
      sortfour(element,left,right);
    }
  else
    {
      v = element[right].constant;
      i = left - 1;
      k = right;

    start_2:
      do { i++; } while (element[i].constant < v);
      do { k--; } while ((element[k].constant > v) && (k > left));
      if (k > i)
	{
	  Swap(&(element[k]),&(element[i]));
	  goto start_2;
	}

      Swap(&(element[i]), &(element[right]));
      
      if (left < (i-1))  quicksort(element,left,i-1);
      if ((i+1) < right)
	{
	  left = i+1;
	  goto start;
	}
    }
}

void labelsort(start,size)
    code *start;
    int size;
{
  quicksort((labelt *) start,0,size-1);
}

#endif /* BOUBLE */

