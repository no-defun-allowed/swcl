#ifndef INCREMENTAL_COMPACT_H
#define INCREMENTAL_COMPACT_H
#include "os.h"
#include "lispobj.h"

extern void consider_compaction();
extern void run_compaction();
extern void log_for_compacter(lispobj source, lispobj *where);

#endif
