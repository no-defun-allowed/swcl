#define _GNU_SOURCE
#include <pthread.h>
#include <semaphore.h>
#include "interr.h"

#define GC_THREADS 3
static pthread_t threads[GC_THREADS];
static sem_t start_semaphore;
static sem_t join_semaphore;
static void (*action)(void);

static void *worker(void *nothing) {
  (void)nothing;
  while (1) {
    sem_wait(&start_semaphore);
    action();
    sem_post(&join_semaphore);
  }
  return NULL;
}

void thread_pool_init() {
  sem_init(&start_semaphore, 0, 0);
  sem_init(&join_semaphore, 0, 0);

  for (int i = 0; i < GC_THREADS; i++)
    if (pthread_create(threads + i, NULL, worker, NULL))
      lose("Failed to create GC thread #%d", i);
    else
      pthread_setname_np(threads[i], "Parallel GC");
}

static void wake_gc_threads() {
  for (int i = 0; i < GC_THREADS; i++) sem_post(&start_semaphore);
}

static void join_gc_threads() {
  for (int i = 0; i < GC_THREADS; i++) sem_wait(&join_semaphore);
}

void run_on_thread_pool(void (*act)(void)) {
  action = act;
  wake_gc_threads();
  act();
  join_gc_threads();
}
