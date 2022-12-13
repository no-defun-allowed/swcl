#ifndef INCREMENTAL_COMPACT_H
#define INCREMENTAL_COMPACT_H
#include "os.h"
#include "lispobj.h"

extern boolean compacting;
extern void compactor_init();
extern void consider_compaction(generation_index_t gen);
extern void run_compaction();
extern void log_for_compacter(lispobj source, lispobj *where, enum source type);

extern unsigned char *target_pages;
static boolean should_log_slot(lispobj *where) {
  return target_pages[find_page_index(where)];
}

#endif
