/*
 * event.h
 *
 * Johan Bevemyr.....Tue Aug 13 1991
 *
 *
 * Copyright (C) 1991 - 1995
 *
 *   H}kan Millroth / Johan Bevemyr / Thomas Lindgren / Patric Hedlin
 *
 */ 

#ifndef EVENT_H
#define EVENT_H


/* events in order of priority */

typedef enum {
  EVENT_INTERRUPT = 0,
  EVENT_GC,
  EVENT_WAKE
} events;

typedef enum {
  GLOBAL_EVENT_FAIL = 0,
  GLOBAL_EVENT_GC
} global_events;


#if defined(SEMAPHORE)

#define AddEvent(E)				\
{						\
  SpinGrabSema(SEMA_EVENT,1,w);			\
  worker_set->event_flag |= 1 << (E);		\
  SpinDropSema(SEMA_EVENT,1,w);			\
}

#define RemoveEvent(E)				\
{						\
  SpinGrabSema(SEMA_EVENT,1,w);			\
  worker_set->event_flag &= ~(1 << (E));	\
  SpinDropSema(SEMA_EVENT,1,w);			\
}

#define AddGlobalEvent(E)			\
{						\
  SpinGrabSema(SEMA_EVENT,1,w);			\
  worker_set->global->event_flag |= 1 << (E);	\
  SpinDropSema(SEMA_EVENT,1,w);			\
}

#if defined(PARALLEL)
#define RemoveGlobalEvent(E)				\
{							\
  SpinGrabSema(SEMA_EVENT,1,w);				\
  worker_set->global->event_flag &= ~(1 << (E));	\
  SpinDropSema(SEMA_EVENT,1,w);				\
}
#else
#define RemoveGlobalEvent(E)
#endif /* PARALLEL */

#else /* not SEMAPHORE */

#define AddEvent(E)       (worker_set->event_flag |= 1 << (E))
#define RemoveEvent(E)    (worker_set->event_flag &= ~(1 << (E)))

#define AddGlobalEvent(E) (worker_set->global->event_flag |= 1 << (E))
#define AddGlobalEventCode(E) (worker_set->global->event_flag |= 1 << (E))

#if defined(PARALLEL)
#define RemoveGlobalEvent(E) (worker_set->global->event_flag &= ~(1 << (E)))
#else 
#define RemoveGlobalEvent(E) 
#endif /* PARALLEL */

#endif /* SEMAPHORE */

#define EventTrue(F,E)    (((F) != 0) && (F & (1 << (E))))


#endif /* EVENT_H */
