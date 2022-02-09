#ifndef GC_WORKER_H
#define GC_WORKER_H

#define GC_WORKER_THREADS 2

#ifdef LISP_FEATURE_PARALLEL_GC
extern void pgc_init(void);
extern void pgc_fork(void (*action)(void));
extern void pgc_join(void);
#endif

#endif
