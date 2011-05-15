/*
 * segment.h
 *
 * Johan Bevemyr.....Mon Dec  5 1994
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef SEGMENT_H
#define SEGMENT_H

#define SEGMENTHEADERSIZE ((sizeof(segment)-ANY*sizeof(TAGGED))/sizeof(TAGGED))

#define IsNewSegmentMarker(T)	IsATM(T)


typedef struct segment {
  s32 size;
  struct segment *next;
  TAGGED data[ANY];
} segment;

extern void init_segments PROTO((u32, s32, TAGGED *, worker *, long));
extern segment *allocate_segment PROTO((worker *));
extern segment *allocate_segment_nolock PROTO((worker *));
extern segment *new_allocate_segment PROTO((worker *));
extern segment *new_allocate_segment_nolock PROTO((worker *));
extern void free_segment PROTO((segment *, worker *));
extern void new_free_segment PROTO((segment *, worker *));
extern TAGGED new_segment PROTO((worker *));
extern TAGGED new_segment_nolock PROTO((worker *));
extern TAGGED new_new_segment PROTO((worker *));
extern TAGGED new_new_segment_nolock PROTO((worker *));
extern void allocate_static_segment PROTO((worker *));
extern void allocate_static_segment_nolock PROTO((worker *));
extern void print_segments PROTO((segment *));
extern void return_segment PROTO((segment *, worker *));
extern void return_segment_nolock PROTO((segment *, worker *));
extern void free_seg_list PROTO((worker *,segment *));
extern void free_new_seg_list PROTO((worker *,segment *));
extern void new_return_segment PROTO((segment *, worker *));
extern void new_return_segment_nolock PROTO((segment *, worker *));
extern unsigned long used_segment_size PROTO((worker *));
extern unsigned long used_segment_size_all PROTO((worker *));
extern unsigned long unused_segment_size PROTO((worker *));

extern void add_old_segments PROTO((worker *, TAGGED *, u32, int));
extern void add_old_segments_nolock PROTO((worker *, TAGGED *, u32, int));
extern void add_new_segments PROTO((worker *, TAGGED *, u32, int));
extern void add_new_segments_nolock PROTO((worker *, TAGGED *, u32, int));

#if defined(SEGMENTED_MEMORY)


# ifdef GENERATIONAL

#  define Free_Segment(X,Y) new_free_segment(X,Y)

# else /* not GENERATIONAL */

#  define Free_Segment(X,Y) free_segment(X,Y)

# endif /* GENERATIONAL */


#else

#  define Free_Segment(X,Y)

#endif

#endif /* SEGMENT_H */
