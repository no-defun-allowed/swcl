#ifndef INCREMENTAL_COMPACT_H
#define INCREMENTAL_COMPACT_H
#include "os.h"
#include "lispobj.h"

/* The various sorts of pointer swizzling in SBCL. */
enum source { SOURCE_NORMAL, SOURCE_LOCKFREE_NODE,
              SOURCE_HASH_TABLE, SOURCE_CLOSURE };
static lispobj tag_source(lispobj *base, enum source s) {
  return make_lispobj(base, (int)s);
}

extern void consider_compaction();
extern void run_compaction();
extern void log_for_compacter(lispobj source, lispobj *where);

#endif
