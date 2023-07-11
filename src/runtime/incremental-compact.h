#ifndef INCREMENTAL_COMPACT_H
#define INCREMENTAL_COMPACT_H

#ifdef LISP_FEATURE_MARK_REGION_GC
#include "os.h"
#include "lispobj.h"

/* Compactor tuning */
extern boolean force_compaction;
extern float page_overhead_threshold, page_utilisation_threshold;
extern generation_index_t minimum_compact_gen;
extern uword_t bytes_to_copy;

/* Entry-points for compactor */
extern boolean compacting;
extern void compactor_init();
extern void consider_compaction(generation_index_t gen);
extern void run_compaction(uword_t *copy_meter, uword_t *fix_meter);

/* Tracing/logging interface */
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

#else
#define log_slot(target, where, so, st) 0
#endif

#endif
