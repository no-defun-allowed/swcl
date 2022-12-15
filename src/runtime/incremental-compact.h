#ifndef INCREMENTAL_COMPACT_H
#define INCREMENTAL_COMPACT_H
#include "os.h"
#include "lispobj.h"
#include <stdatomic.h>

extern boolean compacting;
extern void compactor_init();
extern void consider_compaction(generation_index_t gen);
extern void run_compaction();

extern unsigned char *target_pages;
extern void log_relevant_slot(lispobj *where, enum source source);
extern unsigned char *target_pages;
/* Avoid a full call unless the slot is relevant. */
static void log_slot(lispobj target, lispobj *where, enum source source) {
  /* mark() can provide NULL when there is no easy way to pass
   * around the source. */
  if (where && target_pages[find_page_index(native_pointer(target))])
    log_relevant_slot(where, source);
}
extern void commit_thread_local_remset();

#endif
