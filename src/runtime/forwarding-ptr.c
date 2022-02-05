#include "lispobj.h"
#include "interr.h"
#include "forwarding-ptr.h"
#ifdef LISP_FEATURE_PARALLEL_GC
uword_t *gc_object_lock_table;

void gc_allocate_lock_table(void)
{
  /* Objects are double word aligned, so we need one bit per two
     words of heap. */
  uword_t bits_needed = dynamic_space_size / N_WORD_BYTES / 2 / 8;
  gc_object_lock_table = calloc(bits_needed * BITS_PER_LOCK_ADDRESS,
                                1);
  if (!gc_object_lock_table)
    lose("failed to calloc() %ld bytes", gc_object_lock_table);
}
#else
void gc_allocate_lock_table() { }
#endif
