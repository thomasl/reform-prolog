/*
 * worker.h
 *
 * Johan Bevemyr.....Thu Oct  8 1992
 * Patric Hedlin.....Mon Oct  3 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef WORKER_H
#define WORKER_H

/**********************************************************************
 * Debug info
 */

typedef struct {
  int debugflag;
  int debugmode;
  int debugfail;
  int spypoints[MAXSPYPOINTS];
  code *breakpoints[MAXBREAKPOINTS];
  struct definition *predspypoints[MAXSPYPOINTS];
  int spyc;
  int breakc;
  int predspyc;
  code *skipbreak;
  BOOL display_args;
  BOOL display_envs;
  BOOL display_fail;
  BOOL display_prolog;
  int in, out;
} debug_t;

/**********************************************************************
 *   Choicepoint
 */

typedef enum {
  NEW_GEN = 0,
  OLD_GEN
} generation_t;

typedef struct choicepoint {
    TAGGED             *trail_top;       /* (TR) top of trail stack        */
    TAGGED             *global_top;      /* (H)  top of global stack       */
    struct choicepoint *last_choice;     /* (B)  previous choice pt.       */
    struct environment *cont_env;        /* (CE) cont. environment         */
    code               *next_instr;      /* (CP) cont. code                */
    code               *next_clause;     /* (BP) next clause to try        */
    indx               arity;            /* (n)  number of saved aregs     */
    generation_t       generation;       /* Old or new choice point        */
#if defined(TIMESTAMP) || defined(UNBOUND)
    s32                timestamp;        /* current timestamp              */
#else
#  if defined(LOWTAGS)
    TAGGED             dummy;
#  endif
#endif 
    TAGGED             areg[ANY];        /* (Ai) saved argument registers  */
} choicepoint;


/**********************************************************************
 *   Environment
 */

typedef struct environment {
    struct environment *cont_env;        /* (CE) cont. environment        */
    code               *next_instr;      /* (CP) cont. code               */
    TAGGED yreg[ANY];
} environment;


#define sizeofChoicepoint(arity) \
  (sizeof(choicepoint) - sizeof(TAGGED) + sizeof(TAGGED) * arity * ARSIZE)

#define sizeofEnvironment(arity) \
  (sizeof(environment) - sizeof(TAGGED) + sizeof(TAGGED) * arity * SVASIZE)


/**********************************************************************
 *   Worker support
 */

/****************************************
 *   Statistics
 */

typedef struct {
    double user_start_walltime;
    double user_last_walltime;
    double user_gcwalltime;
    double user_last_gcwalltime;
#if defined(PARALLEL)
    double in_parallel_walltime;
    double in_parallel_walltime_last;
    int current_user_time;
#  if defined(STAT_AWAIT)
    int await;
    int await_count;
#  endif /* STAT_AWAIT */
#endif /* PARALLEL */
    int user_start_time;
    int user_last_time;
    int user_last_gctime;
    int user_last_gcnr;
    int user_last_gcbytes;
    int sys_start_time;
    int calls;
    int fails;
    int builtin_calls;
    int heap_gc_nr;
    u32 heap_gc_bytes;
    int heap_gc_time;
    double heap_gc_walltime;
    double heap_gc_mark_time;
    double heap_gc_copy_time;
    double heap_gc_wait_time[4];
    double heap_gc_post_time;
    double heap_gc_pre_time;
    int heap_gc_marked;
    int heap_gc_copied;
    u32 heap_reclaim;
#if defined(TRAILSTAT)
    u32 trail_stat;
    u32 deref_stat;
    u32 restore_stat;
    u32 swap_stat;
    u32 tidy_stat;
    u32 instr_stat;
    u32 instr_prof[END_OF_PRED];
#endif /* TRAILSTAT */
#if defined(TIMESTAMP)
    int sva_init_count;
#endif
    int hva_init_count;
#if defined(SWAP_BIND)
    int swap_fail;
#endif
} statistics;

/****************************************
 *   Worker command structure
 */

#if defined(PARALLEL)

enum {
  W_EXECUTE_CODE,
  W_RESET,
  W_BACKTRACK,
  W_TIDY,
  W_DEBUG,
  W_DEBUG_FILE,
  W_DEBUG_SOCKET,
  W_REDUCE_PLUS,
  W_REDUCE_TIMES,
  W_REST,
  W_GC_MARK,
  W_GC_MARK_LATE,
#if defined(COPY_ALI)
  W_GC_COPY,
  W_GC_UPDATECP,
#else
  W_GC_MARK_TRAIL,
  W_GC_COMP_TRAIL,
  W_GC_SWEEP,
  W_GC_UP,
  W_GC_DOWN,
#endif
  W_GC_FINISH,
  W_BUILD_VARIABLES,
  W_REASSIGN
#if defined(USE_PMON)
    ,W_PMON_DELTA
#endif      
};

typedef struct {
  s32 type;
  code *code;
  TAGGED *vector;
  s32 size;
} worker_command;

#endif /* PARALLEL */

/****************************************
 *   Prolog flags
 */

typedef struct prologflag {
  BOOL gc_verbose;
  BOOL load_verbose;
} prologflag;

/****************************************
 *   Global worker variables, i.e. variables shared between workers.
 */

/* Types declared in storage.c */
typedef char heap;

typedef struct {
    heap *atom_start, *atom_end, *atom_current, *atomoffset;
    heap *patch_start, *patch_current, *patch_end;
    code *code_start, *code_end, *code_current, *code_limit;
    struct segment *free_segments;
    struct segment *new_free_segments;
    TAGGED *new_gen_limit;
    TAGGED *new_gen_limit_orig;
    s32 free_seg;
    s32 new_free_seg;
    s32 used_seg;
    struct definition **predtable;
    atom_bucket **atomtable;
    functor_bucket **functortable;
    s32 active_workers;
    prologflag flags;
    char* globalstart;
    code *luther_on_exception_clause;
    BOOL workers_active;
#if defined(COPY_GC)
    struct segment *compare_seg;
    TAGGED *compare_top;
    TAGGED *compare_bot;
    TAGGED *compare_margin;
    TAGGED *compare_end;
    uword ordernr;
#endif
    TAGGED current_module;
#if defined(PARALLEL)
    TAGGED *global_regs;
    worker_command parallel_start;
    s32 event_flag;
    s32 scheduling;
    s32 scheduling_mode;
    s32 sched_level;
    double *reduction_results;
    TAGGED *collect;
    s32 debugsig;
    u32 semaphores[MAX_SPIN_SEM_NR];
    u32 *pid_sema;
    s32 *level;
    volatile s32 barrier;
    volatile s32 barrier_wait;
#endif /* PARALLEL */
#if defined(USE_KSR_THREADS)
    pthread_barrier_t *worker_barrier;
#endif /* USE_KSR_THREADS */
#if defined(USE_PMON)
    PMON_DATA *pmon_data;
#endif /* USE_PMON */
} globalvar;

/*************************************************************
 *   Information needed for each worker by the GC-algorithm
 */

typedef struct {
  u32 size;
  TAGGED *arg;
} load_entry_t;

typedef struct {
  BOOL markdone;
  BOOL copydone;
  u32 lock;
  load_entry_t buf[LOADBUFFERSIZE+1];
  u32 start,stop;
} load_dist_t;

typedef struct {
  size_t arity;		/* Current number of live x-registers. */
  size_t env_size;	/* Current (environment) frame size.   */

  TAGGED *trail;

  size_t wake_count;

  BOOL *done;

  choicepoint *last_safe;

  load_dist_t *load;

} gc_info_t;

/*************************************************************
 *   Local worker variables
 */

typedef enum {
  LUTHER_TRACE_ON,
  LUTHER_TRACE_SPY,
  LUTHER_TRACE_OFF
} TraceMode;

typedef struct worker {
    int         pid;
    code 	*pc;
    code 	*next_instr;

#if defined(TIMESTAMP) || defined(UNBOUND)
    s32         time;
    s32         uncond;
#else
    TAGGED      *uncond;
#endif /* TIMESTAMP | UNBOUND */    

    environment *frame;

    choicepoint *choice;
    choicepoint *call_choice;	/* Choice-point at last call.      */
    choicepoint *fail_choice;	/* Choice-point used to detect global fail. */

    u32 	event_flag;
    TraceMode 	trace_mode;

    TAGGED 	*s;
    TAGGED 	*regs;

    TAGGED      *trail_start;
    TAGGED 	*trail_top;
    TAGGED      *trail_end; 

    TAGGED      *heap_start;
    TAGGED 	*heap_top;
    TAGGED      *heap_margin;
    TAGGED      *heap_end;

    TAGGED 	*stack_start;
    TAGGED      *stack_margin;
    TAGGED      *stack_end;

    struct segment     *used_segments;
    struct segment     *new_used_segments;

    statistics  *stats;
    globalvar   *global;

#if defined(PARALLEL)
    s32         direction;
    s32         synch_state;
    s32         barrier_count;
#endif /* PARALLEL */

    gc_info_t	gc_info;
    u32         wake_count;

#if defined(COPY_GC) || defined(COPY_GC_GEN)
    TAGGED      *heap_copy_start;
    TAGGED 	*heap_copy_top;
    TAGGED      *heap_copy_margin;
    TAGGED      *heap_copy_end;

#  if defined(COPY_GC_GEN) 
    TAGGED      *trail_old_gen_top;
    TAGGED      *heap_old_gen_top;
    TAGGED      *heap_new_gen_start;
#  endif /* COPY_GC_GEN */

#endif /* COPY_GC || COPY_GC_GEN */

#  if defined(GENERATIONAL)
    TAGGED      *trail_old_gen_top;
    TAGGED      *old_heap_start;
    TAGGED 	*old_heap_top;
    TAGGED      *old_heap_margin;
    TAGGED      *old_heap_end;
    s32         old_gen_uncond;
#  endif /* GENERATIONAL */

#if defined(DEBUG)
    debug_t     debug;
#endif

} worker;


/**********************************************************************
 *   Macros for cacheing of worker variables in machine registers
 */

#define H               heap_top

#define LoadCache(x)	(x = w->x)
#define SaveCache(x)	(w->x = x)

#define SeqWorker(W)   ((W)->pid == 0)

/**********************************************************************
 *   Prototypes
 */

#define Arg          w
#define Argdecl      register worker *w
#define Argproto     worker *

#define InArg        w, regs
#define InArgdecl    register worker *w; register s32 *regs
#define InArgproto   worker *, s32 *

extern int worker_nr;
extern worker *worker_set;


#endif /* WORKER_H */
