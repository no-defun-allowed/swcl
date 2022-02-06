#include "lispobj.h"
#include "interr.h"
#include "forwarding-ptr.h"
#include "target-os.h"
#ifdef LISP_FEATURE_PARALLEL_GC
uword_t *gc_object_lock_table;
uword_t gc_object_lock_table_words;

void gc_allocate_lock_table(void)
{
    extern os_vm_size_t dynamic_space_size;
    /* Objects are double word aligned, so we need one bit per two
       words of heap. */
    uword_t bits_needed = dynamic_space_size / N_WORD_BYTES / 2 / 8;
    gc_object_lock_table = calloc(bits_needed * BITS_PER_LOCK_ADDRESS,
                                  1);
    gc_object_lock_table_words = bits_needed * BITS_PER_LOCK_ADDRESS;
    if (!gc_object_lock_table)
      lose("failed to calloc() %ln bytes", gc_object_lock_table);
}
#else
void gc_allocate_lock_table() { }
#endif
