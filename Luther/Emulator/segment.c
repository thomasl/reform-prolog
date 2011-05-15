/*
 * segment.c - Routines for handling the segmented heap.
 *
 * Johan Bevemyr.....Mon Dec  5 1994
 *
 *
 * Interface:
 *
 *     init_segments(size, global, worker *)
 *          Initializes the global area pointed to by global into
 * 	    a linked list of free segments. w->global->free_segments
 * 	    is set to point to the linked list. w->global->used_segments
 * 	    is set to NULL.
 * 
 *     new_segment(worker *)
 *          Returns a pointer to a new segment. The heap_top,
 * 	    heap_end and heap_margin registers are set properly.
 * 
 *     new_segment_nolock(worker *)
 *          Returns a pointer to a new segment. The heap_top,
 * 	    heap_end and heap_margin registers are set properly.
 * 	    Access to the global list of free segment is not
 * 	    guarded by locking a semaphore.
 * 
 *     new_new_segment(worker *)
 *          Returns a pointer to a new segment. The heap_top,
 * 	    heap_end and heap_margin registers are set properly.
 * 
 *     new_new_segment_nolock(worker *)
 *          Returns a pointer to a new segment. The heap_top,
 * 	    heap_end and heap_margin registers are set properly.
 * 	    Access to the global list of free segment is not
 * 	    guarded by locking a semaphore.
 * 
 *     free_segment(segment, worker *)
 *          Returns a segment to the linked list of free segments.
 *     
 *     new_free_segment(segment, worker *)
 *          Returns a segment to the linked list of free segments.
 *     
 *     return_segment(segment, worker *)
 *          Returns the segments in the linked list to the pool of
 * 	    free segments.
 * 
 *     new_return_segment(segment, worker *)
 *          Returns the segments in the linked list to the pool of
 * 	    free segments.
 * 
 *     print_segments(segmentlist)
 *          Prints information about the segments to standard output.
 * 
 *     used_segment_size(worker *)
 *          Returns the total size of all segments used by the 
 * 	    worker (in bytes).
 * 
 *     used_segment_size(worker *)
 *          Returns the total size of all unused segments available 
 * 	    to the worker (in bytes).
 * 
 * Implementation:
 *
 *   Segments are stored in a sorted linked list. The size of each segment
 * is not fixed it is stored in the second slot of the segment.
 *
 * A segment has the following structure:
 *
 *
 *		----------------
 *		|  next seg    |  (next free or next used)
 *		----------------
 *		|  size        |
 *		----------------
 *		|              |
 *		.              .
 *		.              .
 *		.              .
 *		|              |
 *		----------------
 *
 *
 *   The free segments are stored in a sorted linked list. The list is
 * sorted by their location in memory.
 *
 *   The used segments are stored in a sorted linked list. The ordering
 * is desided by allocation time, the most reasently allocated segment
 * is stored first.
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#include "luther.h"

#if defined(COPY_GC) 

/**********************************************************************
 *
 * Name: init_segments
 *
 * Desc: Devide a large block of memory into segments and link them 
 *       into a 'free-list'. 
 *
 **********************************************************************/

void init_segments(size, new_gen_size, global, w, segment_size)
     u32 size;           /* size of heap (new+old gen)           */
     TAGGED *global;     /* top of allocated area                */
     worker *w;          /* worker structure                     */
     long segment_size;  /* size of each segment                 */
     s32 new_gen_size;   /* number of segments in new generation */

{
  TAGGED *end;                  /* end of heap area             */

  end = (TAGGED *) (((u32) global) + size);
  
  /* Add new generation *******************************/
  w->global->new_free_seg = 0;
  w->global->new_free_segments = NULL;

  add_new_segments_nolock(w, global, segment_size, new_gen_size);

  global += segment_size*new_gen_size;

  w->global->new_gen_limit = global;
  w->global->new_gen_limit_orig = global;

  {
    /* Add old generation *******************************/
    int nr_old_segs;

    nr_old_segs = (end-global)/segment_size;

    w->global->used_seg = 0;
    w->global->free_seg = 0;
    w->global->free_segments = NULL;

    add_old_segments_nolock(w, global, segment_size, nr_old_segs);

    global += segment_size*nr_old_segs;
  }

  return;
}

/**********************************************************************
 *
 * Name: add_old_segments
 * 
 * Desc:
 * 
 **********************************************************************/

inline static add_segments(w,new_mem,size,nr,return_seg)
     worker *w;
     TAGGED *new_mem;
     u32 size;
     int nr;
     void (*return_seg)PROTO((segment *,worker *));
{
  segment *seg;

  while(nr--)
    {
      seg = (segment *) new_mem;
      seg->size = size-SEGMENTHEADERSIZE;
      (return_seg)(seg,w);
      new_mem += size;
    }

  return;
}

void add_old_segments_nolock(w,new_mem,size,nr)
     worker *w;
     TAGGED *new_mem;
     u32 size;
     int nr;
{
  w->global->used_seg += nr;
  add_segments(w,new_mem,size,nr,return_segment_nolock);
}

void add_old_segments(w,new_mem,size,nr)
     worker *w;
     TAGGED *new_mem;
     u32 size;
     int nr;
{
  SpinGrabSema(SEMA_ALLOCATE_SEGMENT,1,w);
  w->global->used_seg += nr;
  SpinDropSema(SEMA_ALLOCATE_SEGMENT,1,w);

  add_segments(w,new_mem,size,nr,return_segment);
}

void add_new_segments_nolock(w,new_mem,size,nr)
     worker *w;
     TAGGED *new_mem;
     u32 size;
     int nr;
{
  add_segments(w,new_mem,size,nr,new_return_segment_nolock);
}

void add_new_segments(w,new_mem,size,nr)
     worker *w;
     TAGGED *new_mem;
     u32 size;
     int nr;
{
  add_segments(w,new_mem,size,nr,new_return_segment);
}

/**********************************************************************
 *
 * Name: allocate_segment
 * 
 * Desc:
 * 
 **********************************************************************/
segment *allocate_segment(w)
     worker *w;
{
  if (w->global->free_segments == NULL)
    {
      FatalError("out of memory");
    }
  else
    {
      segment *ret;

      SpinGrabSema(SEMA_ALLOCATE_SEGMENT,1,w);

      ret = w->global->free_segments;

      w->global->free_segments = ret->next;
      w->global->used_seg += 1;
      w->global->free_seg -= 1;

      SpinDropSema(SEMA_ALLOCATE_SEGMENT,1,w);

      ret->next = w->used_segments;

      w->used_segments = ret;

      return ret;
    }
}

/**********************************************************************
 *
 * Name: new_allocate_segment_nolock
 * 
 * Desc:
 * 
 **********************************************************************/

segment *new_allocate_segment_nolock(w)
     worker *w;
{
  if (w->global->new_free_segments == NULL)
    {
      return NULL;
    }
  else
    {
      segment *ret;

      ret = w->global->new_free_segments;

      w->global->new_free_segments = ret->next;
      w->global->new_free_seg -= 1;

      ret->next = w->new_used_segments;

      w->new_used_segments = ret;

      return ret;
    }
}

/**********************************************************************
 *
 * Name: new_allocate_segment
 * 
 * Desc:
 * 
 **********************************************************************/
segment *new_allocate_segment(w)
     worker *w;
{
  segment *ret;

  SpinGrabSema(SEMA_ALLOCATE_SEGMENT,1,w);

#if !defined(NDEBUG)
  /* Check that seglist is NULL terminated */
  {
    segment *test;

    test = w->new_used_segments;

    while(test != NULL) test = test->next;
  }
#endif

  if (w->global->new_free_segments == NULL)
    {
      ret = NULL;
    }
  else
    {
      ret = w->global->new_free_segments;

      w->global->new_free_segments = ret->next;
      w->global->new_free_seg -= 1;

      ret->next = w->new_used_segments;

      w->new_used_segments = ret;
    }

  SpinDropSema(SEMA_ALLOCATE_SEGMENT,1,w);

  return ret;
}

/**********************************************************************
 * Name: new_segment
 *
 * Desc: Allocates a new memory segment and add it to the current
 *       workers heap. The old segment is terminated by a NULL 
 *       pointer, as a watchdog, to ensure that the GC does not read
 *       past the end of the segment.
 *       
 * Returns: a pointer to the new segment.
 *
 **********************************************************************/

TAGGED new_segment(w)
     worker *w;
{
  segment *seg;
  
  seg = allocate_segment(w);
  
  /* We need properly terminated segments for the GC
   * (structure-cells are marked to cope with internal
   * pointers)...
   */

#ifdef GENERATIONAL
  PushOnHeap(w->old_heap_top, (TAGGED)NULL);

  w->old_heap_top = w->old_heap_start = &seg->data[0];
  w->old_heap_end = ((TAGGED *) seg) + seg->size;
  w->old_heap_margin = w->old_heap_end - (margin_size/sizeof(TAGGED));

  /* ... and properly initiated segments for the same reson. */

  PushOnHeap(w->old_heap_top, (TAGGED)NULL);

#else /* not GENERATIONAL */
  PushOnHeap(w->heap_top, (TAGGED)NULL);

  w->heap_top = w->heap_start = &seg->data[0];
  w->heap_end = ((TAGGED *) seg) + seg->size;
  w->heap_margin = w->heap_end - (margin_size/sizeof(TAGGED));

  /* ... and properly initiated segments for the same reson. */

  PushOnHeap(w->heap_top, (TAGGED)NULL);

#endif /* GENERATIONAL */


  return (TAGGED) seg;
}

/**********************************************************************
 *
 * Name: new_new_segment
 * 
 * Desc:
 * 
 **********************************************************************/

TAGGED new_new_segment(w)
     worker *w;
{
  segment *seg;
  
  seg = new_allocate_segment(w);

  if (seg != NULL)
    {
      /* We need properly terminated segments for the GC
       * (structure-cells are marked to cope with internal
       * pointers)...
       */

      PushOnHeap(w->heap_top, (TAGGED)NULL);

      w->heap_top = w->heap_start = &seg->data[0];
      w->heap_end = ((TAGGED *) seg) + seg->size;
      w->heap_margin = w->heap_end - (margin_size/sizeof(TAGGED));

      /* ... and properly initiated segments for the same reson. */

      PushOnHeap(w->heap_top, (TAGGED)NULL);
    }

  return (TAGGED) seg;
}

/**********************************************************************
 *
 * Name: new_new_segment_nolock
 * 
 * Desc:
 * 
 **********************************************************************/
TAGGED new_new_segment_nolock(w)
     worker *w;
{
  segment *seg;
  
  seg = new_allocate_segment_nolock(w);
  
  if (seg != NULL)
    {
      /* We need properly terminated segments for the GC
       * (structure-cells are marked to cope with internal
       * pointers)...
       *
       *   PushOnHeap(w->heap_top,NULL);
       *
       * However, this routine is only called with uninitialized heap_top
       * (oups).
       */

      w->heap_top = w->heap_start = &seg->data[0];
      w->heap_end = ((TAGGED *) seg) + seg->size;
      w->heap_margin = w->heap_end - (margin_size/sizeof(TAGGED));

      /* ... and properly initiated segments for the same reson. */

      PushOnHeap(w->heap_top, (TAGGED)NULL);

    }

  return (TAGGED) seg;
}


/**********************************************************************
 *
 * Name: allocate_segment_nolock
 * 
 * Desc:
 * 
 **********************************************************************/
segment *allocate_segment_nolock(w)
     worker *w;
{
  segment *ret;

  if (w->global->free_segments == NULL)
    {
      FatalError("cannot allocate segment");
    }
  else
    {
      ret = w->global->free_segments;
      w->global->free_segments = ret->next;
      w->global->used_seg += 1;
      w->global->free_seg -= 1;

      ret->next = w->used_segments;
      w->used_segments = ret;

      return ret;
    }
}

/**********************************************************************
 *
 * Name: allocate_static_segment
 * 
 * Desc:
 * 
 **********************************************************************/
void allocate_static_segment(w)
     worker *w;
{
  segment *ret;

  if (w->global->free_segments == NULL)
    {
      FatalError("cannot allocate segment");
    }
  else
    {
      segment *ret;

      SpinGrabSema(SEMA_ALLOCATE_SEGMENT,1,w);

#ifdef GENERATIONAL 
      /* This is dangerous. What if we run out of segments? */
      ret = w->global->new_free_segments;

      w->global->new_free_segments = ret->next;
      w->global->new_free_seg -= 1;
#else
      ret = w->global->free_segments;

      w->global->free_segments = ret->next;
      w->global->used_seg += 1;
      w->global->free_seg -= 1;
#endif /* GENERATIONAL */

      SpinDropSema(SEMA_ALLOCATE_SEGMENT,1,w);

      ret->next = w->global->compare_seg;

      w->global->compare_seg = ret;
      w->global->compare_bot = w->global->compare_top = &(ret->data[0]);
      w->global->compare_end = &(ret->data[ret->size]);
      w->global->compare_margin = &(ret->data[ret->size-100]);
    }
}

/**********************************************************************
 *
 * Name: allocate_static_segment_nolock
 * 
 * Desc:
 * 
 **********************************************************************/
void allocate_static_segment_nolock(w)
     worker *w;
{
  if (w->global->free_segments == NULL)
    {
      FatalError("cannot allocate segment");
    }
  else
    {
      segment *ret;

#ifdef GENERATIONAL
      ret = w->global->new_free_segments;

      w->global->new_free_segments = ret->next;
      w->global->new_free_seg -= 1;
#else
      ret = w->global->free_segments;

      w->global->free_segments = ret->next;
      w->global->used_seg += 1;
      w->global->free_seg -= 1;
#endif /* GENERATIONAL */

      ret->next = w->global->compare_seg;

      w->global->compare_seg = ret;
      w->global->compare_bot = w->global->compare_top = &(ret->data[0]);
      w->global->compare_end = &(ret->data[ret->size]);
      w->global->compare_margin = &(ret->data[ret->size-100]);
    }
}

/**********************************************************************
 *
 * Name: new_segment_nolock
 * 
 * Desc:
 * 
 **********************************************************************/
TAGGED new_segment_nolock(w)
     worker *w;
{
  segment *seg;
  
  seg = allocate_segment_nolock(w);

  /* We need properly terminated segments for the GC
   * (structure-cells are marked to cope with internal
   * pointers)...
   *
   *   PushOnHeap(w->heap_top,NULL);
   *
   * However, this routine is only called with uninitialized heap_top (oups).
   */

#ifdef GENERATIONAL
  w->old_heap_top = w->old_heap_start = &seg->data[0];
  w->old_heap_end = ((TAGGED *) seg) + seg->size;
  w->old_heap_margin = w->old_heap_end - (margin_size/sizeof(TAGGED));

  /* ... and properly initiated segments for the same reson. */

  PushOnHeap(w->old_heap_top, (TAGGED)NULL);
#else
  w->heap_top = w->heap_start = &seg->data[0];
  w->heap_end = ((TAGGED *) seg) + seg->size;
  w->heap_margin = w->heap_end - (margin_size/sizeof(TAGGED));

  /* ... and properly initiated segments for the same reson. */

  PushOnHeap(w->heap_top, (TAGGED)NULL);
#endif /* GENERATIONAL */

  return (TAGGED) seg;
}


/**********************************************************************
 *
 * Name: free_segment
 * 
 * Desc:
 * 
 **********************************************************************/

/* Insert the segment into the sorted list. I'm not sure that
 * it is necessary to keep the list sorted but it does not hurt.
 * There is a small time penalty during GC and when releasing pages
 * during backtracking.
 */

void free_segment(seg, w)
     segment *seg;
     worker *w;
{
  segment **prev, *next;

  /* remove the segment from the used_segments list
   */

  if (seg == w->used_segments)
    {
      w->used_segments = seg->next;
      w->heap_start = &(w->used_segments->data[0]);
      w->heap_end = ((TAGGED *) w->used_segments) + w->used_segments->size;
      w->heap_margin = w->heap_end - (margin_size/sizeof(TAGGED));
    }
  else
    {
      prev = &(w->used_segments);
      next = w->used_segments;

      while (next != seg && next != NULL)
	{
	  prev = &(next->next);
	  next = *prev;
	}

      if (next == NULL)
	{
	  Error("deallocated segment cannot be found in the used_seg list");
	}
      else
	{
	  *prev = next->next;
	}
    }

  return_segment(seg,w);

  return;
}

/**********************************************************************
 *
 * Name: new_free_segment
 * 
 * Desc:
 * 
 **********************************************************************/
void new_free_segment(seg, w)
     segment *seg;
     worker *w;
{
  segment **prev, *next;

  /* remove the segment from the used_segments list
   */

  if (seg == w->new_used_segments)
    {
      w->new_used_segments = seg->next;
      w->heap_start = &(w->new_used_segments->data[0]);
      w->heap_end = ((TAGGED *) w->new_used_segments) +
	w->new_used_segments->size;
      w->heap_margin = w->heap_end - (margin_size/sizeof(TAGGED));
    }
  else
    {
      prev = &(w->new_used_segments);
      next = w->new_used_segments;

      while (next != seg && next != NULL)
	{
	  prev = &(next->next);
	  next = *prev;
	}

      if (next == NULL)
	{
	  Error("deallocated segment cannot be found in the used_seg list");
	}
      else
	{
	  *prev = next->next;
	}
    }

  new_return_segment(seg,w);

  return;
}

/**********************************************************************
 *
 * Name: return_segment
 * 
 * Desc:
 * 
 **********************************************************************/
void return_segment(seg, w)
     segment *seg;
     worker *w;
{
  SpinGrabSema(SEMA_ALLOCATE_SEGMENT,1,w);

  return_segment_nolock(seg,w);

  SpinDropSema(SEMA_ALLOCATE_SEGMENT,1,w);
}

void return_segment_nolock(seg, w)
     segment *seg;
     worker *w;
{
  segment **prev, *next;

  /* sorted insertion into the free-segment list
   */

  prev = &(w->global->free_segments);
  next = w->global->free_segments;

  while ((next != NULL) && (seg > next))
    {
      prev = &(next->next);
      next = *prev;
    }

  seg->next = *prev;
  *prev = seg;

  w->global->used_seg -= 1;
  w->global->free_seg += 1;

#if  !defined(NDEBUG) 
  /* Explicitly zero a deallocated segment in order to catch dangling pointers.
   */
  {
    int i;

    for (i = 0; i < (seg->size-4); i++)
      seg->data[i] = (TAGGED)NULL;
  }
  
#endif

  return;
}

/**********************************************************************
 *
 * Name: new_return_segment
 * 
 * Desc:
 * 
 **********************************************************************/

void new_return_segment(seg, w)
     segment *seg;
     worker *w;
{
  SpinGrabSema(SEMA_ALLOCATE_SEGMENT,1,w);

  new_return_segment_nolock(seg,w);

  SpinDropSema(SEMA_ALLOCATE_SEGMENT,1,w);

  return;
}

void new_return_segment_nolock(seg, w)
     segment *seg;
     worker *w;
{
  segment **prev, *next;

  /* sorted insertion into the free-segment list
   */

  prev = &(w->global->new_free_segments);
  next = w->global->new_free_segments;

  while ((next != NULL) && (seg > next))
    {
      prev = &(next->next);
      next = *prev;
    }

  seg->next = *prev;
  *prev = seg;

  w->global->new_free_seg += 1;

  return;
}

/**********************************************************************
 *
 * Name: used_segment_size
 * 
 * Desc:
 * 
 **********************************************************************/
unsigned long used_segment_size(w)
     worker *w;
{
  unsigned long size;
  segment *next;

#ifdef GENERATIONAL

  /* first count new generation */

  next = w->new_used_segments;
  size = (w->heap_top - &(next->data[0])) * sizeof(TAGGED);

  next = next->next;

  while (next != NULL)
    {
      size += next->size * sizeof(TAGGED);
      next = next->next;
    }

  /* then old generation */
  next = w->used_segments;

  size += (w->old_heap_top - &(next->data[0])) * sizeof(TAGGED);

#else /* not GENERATIONAL */

  next = w->used_segments;

  size = (w->heap_top - &(next->data[0])) * sizeof(TAGGED);

#endif /* GENERATIONAL */

  next = next->next;
  
  while (next != NULL)
    {
      size += next->size * sizeof(TAGGED);
      next = next->next;
    }

  return size;
}

/**********************************************************************
 *
 * Name: used_segment_size_all
 * 
 * Desc:
 * 
 **********************************************************************/
unsigned long used_segment_size_all(w)
     worker *w;
{
  long ret;
  int i;

  ret = 0;

  for (i = 0; i <= w->global->active_workers ; i++)
    ret += used_segment_size(&(w[i]));

  return ret;
}

/**********************************************************************
 *
 * Name: unused_segment_size
 * 
 * Desc:
 * 
 **********************************************************************/
unsigned long unused_segment_size(w)
     worker *w;
{
  unsigned long size;
  segment *next;

  size = 0;

#ifdef GENERATIONAL
  /* first new generation */
  next = w->global->new_free_segments;

  while (next != NULL)
    {
      size += next->size * sizeof(TAGGED);
      next = next->next;
    }
#endif

  /* then old generation */

  next = w->global->free_segments;

  while (next != NULL)
    {
      size += next->size * sizeof(TAGGED);
      next = next->next;
    }

  return size;
}


/**********************************************************************
 *
 * Name: print_segments
 * 
 * Desc:
 * 
 **********************************************************************/
void print_segments(next)
     segment *next;
{
  while (next != NULL)
    {
      PL_Print4(stdout, "segment %lx: start %lx end %lx",next,
		&next->data[0],&next->data[next->size]);
      PL_Print2(stdout, " (size %ld)\n", next->size);
      next = next->next;
    }
  
  return;
}


/**********************************************************************
 *
 * Name: free_new_seg_list
 * 
 * Desc:
 * 
 **********************************************************************/
void free_new_seg_list(w,ret_segs)
     worker *w;
     segment *ret_segs;
{
  while (ret_segs != NULL)
    {
      segment *seg;

      seg = ret_segs;
      ret_segs = seg->next;

#ifdef GENERATIONAL
      new_return_segment(seg, w);
#else
      return_segment(seg, w);
#endif /* GENERATIONAL */
    }
}

/**********************************************************************
 *
 * Name: free_seg_list
 * 
 * Desc:
 * 
 **********************************************************************/
void free_seg_list(w,ret_segs)
     worker *w;
     segment *ret_segs;
{
  while (ret_segs != NULL)
    {
      segment *seg;

      seg = ret_segs;

      ret_segs = ret_segs->next;

      return_segment(seg, w);
    }
}

#endif /* COPY_GC */
