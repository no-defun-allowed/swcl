#include <stdbool.h>
#include <semaphore.h>
#include "interr.h"
#include "gc-internal.h"
#include "gencgc-alloc-region.h"
#include "gencgc-internal.h"

#define GC_WORKER_THREADS 8

#ifdef LISP_FEATURE_PARALLEL_GC
__thread boolean is_gc_thread = false;
__thread struct alloc_region gc_thread_alloc_region[3];
static sem_t new_work;
static sem_t finished_work;
static void (*worker_action)(void);

static void pgc_init_thread()
{
    gc_init_region(&tl_mixed_region);
    gc_init_region(&tl_unboxed_region);
    gc_init_region(&tl_code_region);
}

static void pgc_close_thread_regions()
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
      sem_post(&finished_work);
    }
    return NULL;
}

static void pgc_init()
{
    pthread_t uninteresting_thread_id;
    if (sem_init(&new_work, 0, 0) != 0)
        lose("sem_init failed");
    if (sem_init(&finished_work, 0, 0) != 0)
        lose("sem_init failed");
    for (int i = 0; i < GC_WORKER_THREADS; i++)
      if (pthread_create(&uninteresting_thread_id,
                         NULL,
                         pgc_gc_worker,
                         NULL) != 0)
        lose("pthread_create failed");
}
#endif
