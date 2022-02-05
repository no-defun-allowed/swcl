#include "lispobj.h"
#include "interr.h"
#ifdef LISP_FEATURE_PARALLEL_GC
uword_t *gc_object_lock_table;

void gc_allocate_lock_table(void)
{
  /* Objects are double word aligned, so we need one bit per two
     words of heap. */
  gc_object_lock_table = calloc(dynamic_space_size / N_WORD_BYTES / 2 / 8, 1);
  if (!gc_object_lock_table)
    lose("failed to calloc() %ld bytes", gc_object_lock_table);
}
#else

void gc_allocate_lock_table() { }
#endif
