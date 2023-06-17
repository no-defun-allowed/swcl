#ifdef LISP_FEATURE_MARK_REGION_GC
#ifndef INCREMENTAL_COMPACT_H
#define INCREMENTAL_COMPACT_H
#include "os.h"
#include "lispobj.h"
#include <stdatomic.h>

extern boolean compacting;
extern void compactor_init();
extern void consider_compaction(generation_index_t gen);
extern void run_compaction(uword_t *copy_meter, uword_t *fix_meter);

extern unsigned char *target_pages;
extern void log_relevant_slot(lispobj *where, lispobj *source_object, enum source source_type);
extern unsigned char *target_pages;
/* Avoid a full call unless the slot is relevant. */
static inline void log_slot(lispobj target, lispobj *where,
                            lispobj *source_object, enum source source_type) {
  if (!compacting) return;
  page_index_t p = find_page_index(native_pointer(target));
  if (p != -1 && target_pages[p])
    log_relevant_slot(where, source_object, source_type);
}
extern void commit_thread_local_remset();

#endif
#endif
