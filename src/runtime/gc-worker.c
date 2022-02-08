#include <stdbool.h>
#include <stdio.h>
#include <semaphore.h>
#include <pthread.h>
#include "interr.h"
#include "gc-internal.h"
#include "gencgc-alloc-region.h"
#include "gencgc-internal.h"
#include "gc-worker.h"

#ifdef LISP_FEATURE_PARALLEL_GC
_Thread_local boolean is_gc_thread = false;
_Thread_local struct alloc_region gc_thread_alloc_region[3];
static sem_t new_work;
static sem_t finished_work;
static void (*worker_action)(void);

static void pgc_init_thread(void)
{
    is_gc_thread = true;
    gc_init_region(&tl_mixed_region);
    gc_init_region(&tl_unboxed_region);
    gc_init_region(&tl_code_region);
}

static void pgc_close_thread_regions(void)
{
    ensure_region_closed(&tl_code_region, PAGE_TYPE_CODE);
    ensure_region_closed(&tl_unboxed_region, PAGE_TYPE_UNBOXED);
    ensure_region_closed(&tl_mixed_region, PAGE_TYPE_MIXED);
}

static void* pgc_gc_worker(void* uninteresting_argument)
{
    (void)uninteresting_argument;
    pgc_init_thread();
    while (true) {
      sem_wait(&new_work);
      worker_action();
      pgc_close_thread_regions();
      sem_post(&finished_work);
    }
    return NULL;
}

#define for_pgc_threads for (int i = 0; i < GC_WORKER_THREADS; i++)
void pgc_init(void)
{
    pthread_t thread_id;
    if (sem_init(&new_work, 0, 0) != 0)
        lose("sem_init failed");
    if (sem_init(&finished_work, 0, 0) != 0)
        lose("sem_init failed");
    for_pgc_threads {
        if (pthread_create(&thread_id,
                           NULL,
                           pgc_gc_worker,
                           NULL) != 0)
            lose("pthread_create failed");
        pthread_setname_np(thread_id, "Parallel GC");
    }
}

void pgc_fork(void (*action)(void)) {
    worker_action = action;
    for_pgc_threads
        sem_post(&new_work);
}

void pgc_join(void) {
    for_pgc_threads
        sem_wait(&finished_work);
}
#undef for_pgc_threads
#endif
