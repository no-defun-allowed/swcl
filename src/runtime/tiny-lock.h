/* A lock that has the sole benefit of not being the pthreads lock. 
 * And the sole downside of not being the pthreads lock. */
struct lock { int grabbed; };

static void acquire_lock(struct lock *l) {
  int expected = 0, turns = 0;
  while (atomic_compare_exchange_strong(&l->grabbed, &expected, 1))
    /* Start yielding if we spin for too long. */
    if (turns++ > 1000) pthread_yield();
}
static void release_lock(struct lock *l) {
  atomic_store(&l->grabbed, 0);
}
