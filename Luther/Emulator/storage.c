/*
 * storage.c - Luther memory management.
 *
 * Johan Bevemyr.....Sat May 25 1991
 * Patric Hedlin.....Fri Sep 23 1994
 *
 *
 * Implementation:
 *
 *   We have a number of different memory areas.
 *
 *   WAM 
 *   
 *	Global Heap                 
 *	Local Stack
 *	Trail Stack
 *
 *   Runtime system
 *
 *	Atom Heap
 *	Code Area
 *	Backpatch Heap
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */

#include "luther.h"


/*******************************************************************************
 *
 * Global declarations.
 *
 */
GLOBAL long heap_size  = 0;
GLOBAL long local_size = 0;
GLOBAL long trail_size = 0;
 
GLOBAL long code_size;
GLOBAL long atom_size;
GLOBAL long temp_size;
GLOBAL long patch_size;
GLOBAL long margin_size;
GLOBAL long segment_size;

GLOBAL long total_size = 0;


GLOBAL long globalsize;

GLOBAL char *globalstart;


GLOBAL long worker_heap_size;

#if defined(PARALLEL)

GLOBAL long worker_local_size;
GLOBAL long worker_trail_size;

GLOBAL long worker_size = 0;

#endif /* PARALLEL */


/**********************************************************************
 *
 * Initialize workers.
 *
 */
static worker *init_worker_global_storage(globalstart,n)
    char *globalstart;
    int n;
{
  int i;
  worker *w;
  globalvar *glob;

  s32 *reclevel;
  BOOL *gcdone;

  /* Allocate workers.
   */

  w = (worker *) globalstart;
  globalstart += sizeof(worker) * n;

  /**********************************************************************
   *
   * Global variables (atom and code heap etc.) 
   *
   */

  glob = (globalvar *) globalstart;
  globalstart += sizeof(globalvar);

  /* Number of workers.
   */
  glob->active_workers = n - 1;

  /* Atom Heap.
   */
  glob->atom_start = glob->atom_current = (heap *) globalstart;
  globalstart += atom_size;
  glob->atom_end = (heap *) globalstart;
  glob->atomoffset = (heap *) BaseOffset(glob->atom_start);

  /* Patch Heap.
   */
  glob->patch_start = glob->patch_current = (heap *) globalstart;
  globalstart += patch_size;
  glob->patch_end = (heap *) globalstart;

  /* Code Heap.
   */
  glob->code_start = glob->code_current = (code *) globalstart;
  globalstart += code_size;
  glob->code_end = (code *) globalstart;
  glob->code_limit = glob->code_end - (CODELIMITSIZE/sizeof(code));

  /* Predicate Table.
   */
  glob->predtable = (definition **) globalstart;
  globalstart += sizeof(definition *) * PREDHASHLEN;

  /* Atom Table.
   */
  glob->atomtable = (atom_bucket **) globalstart;
  globalstart += sizeof(atom_bucket *) * ATOMHASHLEN;

  /* Functor Table.
   */
  glob->functortable = (functor_bucket **) globalstart;
  globalstart += sizeof(functor_bucket *) * FUNCTORHASHLEN;



#if defined(PARALLEL)
  /* Global registers.
   */
  glob->global_regs = (TAGGED *) globalstart;
  globalstart += temp_size;

  DoubleAlignedAddress(globalstart);
  glob->reduction_results = (double *) globalstart;
  globalstart += sizeof(double) * (n-1);

  glob->collect = (TAGGED *) globalstart;
  globalstart += sizeof(TAGGED) * (n-1);

  /* pid semaphores
   */
  glob->pid_sema = (u32 *) globalstart;
  globalstart += sizeof(u32) * (n-1);

  /* Iteration levels.
   */
  glob->level = (s32 *) globalstart;
  globalstart += sizeof(u32)*(n-1);

  child_pid = (CHILD_PID_TYPE *) globalstart;
  globalstart += sizeof(CHILD_PID_TYPE) * n;

#if defined(USE_PMON)
  glob->pmon_data = (PMON_DATA *) globalstart;
  globalstart += sizeof(PMON_DATA) * n;
#endif

  /* gcdone flags.
   */
  gcdone = (BOOL *) globalstart;
  globalstart += sizeof(BOOL)*(n-1);

#endif /* PARALLEL */

  /*
   *  Some global things are cached in each worker structure, 
   * the following code will initialize these structures.
   *
   */

#if defined(COPY_GC)
  w[0].gc_info.load = (load_dist_t *) globalstart;
  globalstart += sizeof(load_dist_t);
#endif

  for (i = 0; i < n; i++)
    {
#if defined(PARALLEL)
      w[i].gc_info.done = gcdone;
#endif /* PARALLEL */

      w[i].pid = i;
      w[i].global = glob;
      DoubleAlignedAddress(globalstart);
      w[i].stats = (statistics *) globalstart;
      globalstart += sizeof(statistics);

#if defined(COPY_GC)
# if defined(SINGLE_LOAD_STACK)
      w[i].gc_info.load = w[0].gc_info.load;
# else
      w[i].gc_info.load = (load_dist_t *) globalstart;
      globalstart += sizeof(load_dist_t);
# endif
#endif
    }


#if defined(SEGMENTED_MEMORY)

  init_segments(heap_size + (n-1)*worker_heap_size, new_gen_size,
		(TAGGED *) globalstart, w, segment_size);

  glob->compare_seg = NULL;
  allocate_static_segment_nolock(w);

  globalstart += heap_size + (n-1)*worker_heap_size;

  {
    int i;
    for (i = 0; i < n; i++)
      {
	w[i].used_segments = NULL;
	(void) new_segment_nolock(&w[i]);

#ifdef GENERATIONAL
	w[i].new_used_segments = NULL;
	(void) new_new_segment_nolock(&w[i]);
	w[i].trail_old_gen_top = NULL;
	w[i].old_gen_uncond = 0;
#endif /* GENERATIONAL */
      }
  }

#else /* not SEGMENTED_MEMORY */


  /**********************************************************************
   *
   * Allocate heap for the sequential worker.
   *
   */
#if defined(COPY_GC) || defined(COPY_GC_GEN)
  w[0].heap_start = w[0].heap_top = (TAGGED *) globalstart;

# if defined(COPY_GC_GEN)
  w[0].heap_old_gen_top = w[0].heap_start;

#  if defined(DYN_SIZE)
  w[0].heap_top = (TAGGED *) (globalstart+heap_size/2-new_gen_size/4);
#  else
  w[0].heap_top = (TAGGED *) (globalstart+heap_size/4);
#  endif

  w[0].heap_new_gen_start = w[0].heap_top;
# endif /* COPY_GC_GEN */

  globalstart += heap_size/2;
  w[0].heap_end = (TAGGED *) globalstart;
  w[0].heap_margin = (TAGGED *) ((u32) globalstart-(u32) margin_size);

  DoubleAlignedAddress(globalstart);
  w[0].heap_copy_start = w[0].heap_copy_top = (TAGGED *) globalstart;
  globalstart += heap_size/2;
  w[0].heap_copy_end = (TAGGED *) globalstart;
  w[0].heap_copy_margin = (TAGGED *) ((u32) globalstart-(u32) margin_size);

#else  /* COPY_GC */
  w[0].heap_start = w[0].heap_top = (TAGGED *) globalstart;
  globalstart += heap_size;
  w[0].heap_end = (TAGGED *) globalstart;
  w[0].heap_margin = (TAGGED *) ((u32) globalstart-(u32) margin_size);
#endif /* COPY_GC */

  /**********************************************************************
   *
   * Allocate heap for the parallel workers.
   *
   */
#if defined(PARALLEL)
  for (i = 1; i < n; i++)
    {
#if defined(COPY_GC) || defined(COPY_GC_GEN)
      w[i].heap_start = w[i].heap_top = (TAGGED *) globalstart;

#if defined(COPY_GC_GEN)
      w[i].heap_old_gen_top = w[i].heap_start;
      w[i].heap_top = (TAGGED *) (globalstart+heap_size/4);
      w[i].heap_new_gen_start = w[i].heap_top;
# endif /* COPY_GC_GEN */

      globalstart += worker_heap_size/2;
      w[i].heap_end = (TAGGED *) globalstart;
      w[i].heap_margin = (TAGGED *) ((u32) globalstart-(u32) margin_size);

      DoubleAlignedAddress(globalstart);
      w[i].heap_copy_start = w[i].heap_copy_top = (TAGGED *) globalstart;
      globalstart += worker_heap_size/2;
      w[i].heap_copy_end = (TAGGED *) globalstart;
      w[i].heap_copy_margin = (TAGGED *) ((u32) globalstart-
					  (u32) margin_size);
#else  /* COPY_GC */
      w[i].heap_start = w[i].heap_top = (TAGGED *) globalstart;
      globalstart += worker_heap_size;
      w[i].heap_end = (TAGGED *) globalstart;
      w[i].heap_margin = (TAGGED *) ((u32) globalstart-(u32) margin_size);
#endif /* COPY_GC */
    }
#endif /* PARALLEL */

#endif /* SEGMENTED_MEMORY */

  w->global->globalstart = globalstart;

  return w;
}


/**********************************************************************
 *
 * Initialize local stack.
 *
 */
void init_localstack(size,w)
    long size;
    worker *w;
{
  w->stack_start = (TAGGED *) Malloc(size);

  if (w->stack_start == NULL) 
    FatalError("unable to allocate local stack");

  w->stack_end = (TAGGED *) (((char *) w->stack_start) + size);
  w->stack_margin = (TAGGED *) (((char *) w->stack_end) -
				LUTHER_STACK_MARGIN_DEF);
}


/**********************************************************************
 *
 * Initialize trail stack.
 *
 */
void init_trailstack(size,w)
    long size;
    worker *w;
{
  w->trail_top = w->trail_start = (TAGGED *) Malloc(size);

#if defined(COPY_GC_GEN) || defined(GENERATIONAL)
  w->trail_old_gen_top = w->trail_start;
#endif

  if (w->trail_top == NULL)
    FatalError("unable to allocate trail stack");

  w->trail_end = (TAGGED *) (((char *) w->trail_start) + size);
}


/**********************************************************************
 *
 * Reset backpatch heap.
 *
 */
void reset_backpatchheap(w)
    worker *w;
{
    w->global->patch_current = w->global->patch_start;
}


/**********************************************************************
 *
 * Allocate storage from atom space.
 *
 */
heap *atom_alloc(size,w)
    unsigned long size;
    worker *w;
{
    register heap *ret;

    ret = w->global->atom_current;
    w->global->atom_current += size;

    if (w->global->atom_current > w->global->atom_end)
      FatalError("atomheap out of memory");

    return ret;
}


/**********************************************************************
 *
 * Allocate storage from atom space (using word alignment).
 *
 */
heap *aligned_atom_alloc(size,w)
    unsigned long size;
    worker *w;
{
    register heap *ret;

#if defined(LOWTAGS)
    DoubleAlignedAddress(w->global->atom_current);
#endif /* LOWTAGS */

    ret = w->global->atom_current;

    w->global->atom_current += size;

    if (w->global->atom_current > w->global->atom_end)
      FatalError("atomheap out of memory");

    return ret;
}


/**********************************************************************
 * 
 * Allocate patch memory. Used when reading WAM code in parser.y
 *
 */
heap *patch_alloc(size,w)
    unsigned long size;
    worker *w;
{
    register heap *ret;

    ret = w->global->patch_current;
    w->global->patch_current += size;

    if (w->global->patch_current > w->global->patch_end)
      FatalError("patchheap out of memory");

    return ret;
}


/**********************************************************************
 *
 * Initialize x-registers.
 *
 */
void init_temporary_registers(size,w)
    long size;
    worker *w;
{
#if defined(PARALLEL)
    if (w->pid == 0) /* the root process */
      {
	w->regs = w->global->global_regs;
      }
    else
#endif /* PARALLEL */
      {
	w->regs = (TAGGED *) Malloc(size);

	if (w->regs == NULL)
	  FatalError("unable to allocate temporary registers");
      }
    return;
}


/**********************************************************************
 *
 * Allocate global memory.
 *
 */
worker *init_global_memory(n)
    int n;
{
  if (total_size == 0)
    GETENV(total_size,"LUTHER_TOTAL_MEMORY",LUTHER_TOTAL_MEMORY_DEF);

  GETENV(segment_size, "LUTHER_SEGMENT_SIZE", LUTHER_SEGMENT_SIZE_DEF);
  GETENV(margin_size, "LUTHER_HEAP_MARGIN", LUTHER_HEAP_MARGIN_DEF);
  GETENV(patch_size, "LUTHER_BACKPATCHHEAP_SIZE",
	 LUTHER_BACKPATCHHEAP_SIZE_DEF);
  GETENV(temp_size, "LUTHER_TEMP_SIZE" ,LUTHER_TEMP_SIZE_DEF);
  
  temp_size *= sizeof(TAGGED);
  segment_size /= sizeof(TAGGED);

  DoubleAlignedSize(segment_size);
  
  atom_size = (long)(LUTHER_ATOM_SPACE_RATIO * total_size);
  code_size = (long)(LUTHER_CODE_SPACE_RATIO * total_size);
  heap_size = (long)(LUTHER_HEAP_SPACE_RATIO * total_size);
  trail_size = (long)(LUTHER_TRAIL_STACK_RATIO * total_size);
  local_size = (long)(LUTHER_LOCAL_STACK_RATIO * total_size);

  SingleAlignedSize(atom_size);
  SingleAlignedSize(code_size);

  heap_size = AlignSize(heap_size, (segment_size*sizeof(TAGGED)));

  SingleAlignedSize(trail_size);
  SingleAlignedSize(local_size);
  
  
#if defined(PARALLEL)
  
  if (worker_size == 0) 
    GETENV(worker_size, "LUTHER_WORKER_MEMORY", LUTHER_WORKER_MEMORY_DEF);
  
  worker_heap_size  = (long)(WORKER_HEAP_SPACE_RATIO * worker_size);
  worker_trail_size = (long)(WORKER_TRAIL_STACK_RATIO * worker_size);
  worker_local_size = (long)(WORKER_LOCAL_STACK_RATIO * worker_size);
  
  worker_heap_size = AlignSize(worker_heap_size, segment_size);

  SingleAlignedSize(worker_trail_size);
  SingleAlignedSize(worker_local_size);
  
#endif /* PARALLEL */

  /**********************************************************************
   *
   * Calculate total size of shared memory.
   *
   */
  globalsize = sizeof(globalvar) +
               temp_size +
	       atom_size +
	       code_size +
	       heap_size +
	       patch_size +
#if defined(COPY_GC) && defined(GENERATIONAL)
               (new_gen_size * segment_size * sizeof(TAGGED)) +
	       sizeof(load_dist_t)
#if !defined(SINGLE_LOAD_STACK)
	       * n
#endif
	       +
#endif
	       sizeof(statistics) +
	       sizeof(double) +
	       sizeof(worker) +

#if defined(PARALLEL)
		   
	       (n-1) *
	       (
		sizeof(u32) +                               /* pid_sema */
		sizeof(statistics) +                      /* statistics */
		sizeof(double) +                   /*  reduction result */
		sizeof(s32) +                        /* iteration level */
		sizeof(worker) +                              /* worker */
		sizeof(double) +                        /* align buffer */
		sizeof(TAGGED) +             /* inline reduction result */
		worker_heap_size                           /* heap size */
	       ) + n * (sizeof(CHILD_PID_TYPE)

#if defined(USE_PMON)
		        + sizeof(PMON_DATA)
#endif

			) +

#endif /* PARALLEL */
			   /* Note: we add an extra spill buffer (1024).
			    */
			   sizeof(definition *) * PREDHASHLEN +
			   sizeof(atom_bucket *) * ATOMHASHLEN +
			   sizeof(functor_bucket *) * FUNCTORHASHLEN + 1024;


  /**********************************************************************
   *
   * Allocate the memory, in shared memory or local.
   *
   */
#if defined(SHARED_MEMORY)

  globalstart = mmap_global(globalsize);

#else

  if ((globalstart = (char *) Malloc(globalsize)) == NULL)
    FatalError("unable to allocate global memory");
    
#endif

#if !defined(NDEBUG)
  {
    integer *p = (integer *) globalstart;
    integer  i = globalsize / sizeof(integer);
    
    while (i--)
      p[i] = 0x07777770;
  }
#endif

  return init_worker_global_storage(globalstart, n);
}


/**********************************************************************
 *
 * Initialize local memory.
 *
 */
void init_local_memory(w)
    worker *w;
{
#if defined(PARALLEL)
  if (w->pid != 0)
    {
      init_localstack(worker_local_size,w);
      init_trailstack(worker_trail_size,w);
    }
  else 
#endif /* PARALLEL */
    {
      init_localstack(local_size,w);
      init_trailstack(trail_size,w);
    }

  init_temporary_registers(temp_size,w);
}


/**********************************************************************
 *
 * Force malloc to return alligned blocks of data.
 *
 */
#if defined(ALIGN_MALLOC)

char *aligned_malloc(size)
     long size;
{
  register char *res = malloc(size + sizeof(uword));
  
  SingleAlignedAddress(res);
  
  return res;
}

#endif /* ALIGN_MALLOC */


u32 get_usedheap(w)
     worker *w;
{
#ifdef SEGMENTED_MEMORY
  return used_segment_size(w);
#else
  return ((u32) w->heap_top)-((u32) w->heap_start);
#endif /* SEGMENTED_MEMORY */
}

u32 get_freeheap(w)
     worker *w;
{
#ifdef SEGMENTED_MEMORY
  return unused_segment_size(w);
#else
  return ((u32) w->heap_end)-((u32) w->heap_top);
#endif /* SEGMENTED_MEMORY */
}


u32 get_totalheap(w)
     worker *w;
{
  return get_usedheap(w) + get_freeheap(w);
}

void extend_heap(w)
worker *w;
{
  TAGGED *new_memory;
  int nr;

  nr = 20;

#if defined(SHARED_MEMORY) || !defined(SEGMENTED_MEMORY)

  /* to hard to extend memory when mmap is used */
  FatalError("more than half memory used in old gen");

#else /* not SHARED_MEMORY */

  new_memory = (TAGGED *) Malloc(nr * segment_size * sizeof(TAGGED));

  if (new_memory == NULL)
    {
      FatalError("out of memory - unable to extend global heap");
    }
  else
    {
      int before = get_totalheap(w);

      add_old_segments(w,new_memory,segment_size,nr);

      if (w->global->flags.gc_verbose == TRUE)
	{
	  fprintf(stderr,"{verbose: Extending global heap from %ld to %ld}\n",
		  before, get_totalheap(w));
	}

      w->global->globalstart = (char *) (new_memory + nr*segment_size);
    }

#endif /* USE_THREADS */
}

void extend_codespace(w)
worker *w;
{
  code *new_code;
#if defined(SHARED_MEMORY)

  /* to hard to extend memory when mmap is used */
  FatalError("code space exhausted");

#else /* not SHARED_MEMORY */

  new_code = (code *) Malloc(CODESPACEUNIT);

  if (new_code == NULL)
    {
      FatalError("out of memory - unable to extend code space");
    }
  else
    {
      if (w->global->flags.gc_verbose == TRUE)
	{
	  fprintf(stderr,"{verbose: Extending code area by %ld}\n", CODESPACEUNIT);
	}

      w->global->code_current = new_code;
      w->global->code_end = new_code + (CODESPACEUNIT/sizeof(code));
      w->global->code_limit = w->global->code_end - 
	                      (CODELIMITSIZE/sizeof(code));
    }

#endif /* USE_THREADS */
}

/**********************************************************************
 * Debug support 
 */

TAGGED *mem_search(target,start,end)
     TAGGED target;
     TAGGED *start;
     TAGGED *end;
{
  TAGGED *current;
  
  for(current = start ; current < end ; current++)
    {
      if (*current == target) return current;
    }
  
  return NULL;
}
