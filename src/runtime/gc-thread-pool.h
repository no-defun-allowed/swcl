#ifndef GC_THREAD_POOL_H
#define GC_THREAD_POOL_H
#include "gc.h"
#include "queue.h"

#define CACHE_LINE_SIZE 64

extern void thread_pool_init();
extern void run_on_thread_pool(void (*act)(void));

/* A thread-local state which the collector may snoop into. */
struct collector_tls {
  struct Qblock *remset;
  page_words_t *words;
} __attribute__((aligned(CACHE_LINE_SIZE)));

extern unsigned int gc_threads;
extern _Thread_local struct collector_tls *collector_tls;
extern struct collector_tls *collector_tlses;

#endif
