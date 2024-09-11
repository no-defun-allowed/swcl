#ifndef GC_THREAD_POOL_H
#define GC_THREAD_POOL_H

extern void thread_pool_init();
extern void run_on_thread_pool(void (*act)(void));
extern unsigned int _Thread_local gc_thread_id;
extern unsigned int gc_threads;

#endif
