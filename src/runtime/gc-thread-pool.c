#define _GNU_SOURCE
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <semaphore.h>
#include "interr.h"
#include "gc-thread-pool.h"

unsigned int gc_threads;
#define helper_threads (gc_threads - 1)
static os_sem_t *start_semaphores;
static os_sem_t join_semaphore;
static void (*action)(void);
struct collector_tls *collector_tlses;
_Thread_local struct collector_tls *collector_tls;

static void *worker(void *index) {
  uword_t i = (uword_t)index;
  collector_tls = &collector_tlses[i];
  while (1) {
    os_sem_wait(start_semaphores + i);
    /* Only the main thread runs compaction, so workers don't need
     * pthread_jit_write_protect_np on arm64-darwin. */
    action();
    os_sem_post(&join_semaphore);
  }
  return NULL;
}

void thread_pool_init() {
  char *str = getenv("GC_THREADS"), *tail;
  if (str == NULL) {
    gc_threads = 4;
  } else {
    unsigned long parse = strtoul(str, &tail, 10);
    if (tail == str || parse == 0 || parse >= 256) lose("%s isn't a number of GC threads", str);
    gc_threads = parse;
  }

#ifdef LISP_FEATURE_DARWIN
  if (1) { // pre-existing semaphores aren't visible in a forked child
#else
  if (!start_semaphores) {
#endif
    start_semaphores = successful_malloc(sizeof(os_sem_t) * helper_threads);
    for (unsigned int i = 0; i < helper_threads; i++)
      os_sem_init(start_semaphores + i, 0);
    os_sem_init(&join_semaphore, 0);
  }

  collector_tlses = aligned_alloc(CACHE_LINE_SIZE, sizeof(struct collector_tls) * gc_threads);
  if (!collector_tlses)
    lose("Failed to allocate GC thread TLSes");
  for (uword_t i = 0; i < gc_threads; i++)
    collector_tlses[i].remset = NULL;

  for (uword_t i = 0; i < helper_threads; i++) {
    pthread_t thread;
    if (pthread_create(&thread, NULL, worker, (void*)i))
      lose("Failed to create GC thread #%ld", i);
    else {
#ifdef LISP_FEATURE_LINUX
      char buffer[32];
      snprintf(buffer, 32, "Parallel GC #%ld", i);
      pthread_setname_np(thread, buffer);
#endif
    }
  }
}

static void wake_gc_threads() {
  for (unsigned int i = 0; i < helper_threads; i++) os_sem_post(start_semaphores + i);
}

static void join_gc_threads() {
  for (unsigned int i = 0; i < helper_threads; i++) os_sem_wait(&join_semaphore);
}

void run_on_thread_pool(void (*act)(void)) {
  action = act;
  wake_gc_threads();
  collector_tls = &collector_tlses[gc_threads - 1];
  act();
  join_gc_threads();
  action = 0; // tidy up
}
