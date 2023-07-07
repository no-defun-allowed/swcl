#define _GNU_SOURCE
#include <errno.h>
#include <pthread.h>
#include <stdlib.h>
#include <semaphore.h>
#include "interr.h"

static unsigned int gc_threads;
static pthread_t *threads;
static sem_t *start_semaphores;
static sem_t join_semaphore;
static void (*action)(void);

static void *worker(void *index) {
  uword_t i = (uword_t)index;
  while (1) {
    sem_wait(start_semaphores + i);
    action();
    sem_post(&join_semaphore);
  }
  return NULL;
}

void thread_pool_init() {

  char *str = getenv("GC_THREADS"), *tail;
  if (str == NULL) {
    gc_threads = 3;
  } else {
    unsigned long parse = strtoul(str, &tail, 10);
    if (tail == str || parse >= 256) lose("%s isn't a number of GC threads", str);
    gc_threads = parse;
  }

  threads = successful_malloc(sizeof(pthread_t) * gc_threads);
  if (!start_semaphores) {
    start_semaphores = successful_malloc(sizeof(sem_t) * gc_threads);
    for (unsigned int i = 0; i < gc_threads; i++)
      sem_init(start_semaphores + i, 0, 0);
    sem_init(&join_semaphore, 0, 0);
  }

  for (uword_t i = 0; i < gc_threads; i++)
    if (pthread_create(threads + i, NULL, worker, (void*)i))
      lose("Failed to create GC thread #%ld", i);
    else
      pthread_setname_np(threads[i], "Parallel GC");
}

static void wake_gc_threads() {
  for (unsigned int i = 0; i < gc_threads; i++) sem_post(start_semaphores + i);
}

static void join_gc_threads() {
  for (unsigned int i = 0; i < gc_threads; i++) sem_wait(&join_semaphore);
}

void run_on_thread_pool(void (*act)(void)) {
  action = act;
  wake_gc_threads();
  act();
  join_gc_threads();
}
