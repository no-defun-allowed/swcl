#include <stdbool.h>
#include "gc-internal.h"
#include "gencgc-alloc-region.h"
#include "gencgc-internal.h"

#ifdef LISP_FEATURE_PARALLEL_GC
__thread boolean is_gc_thread = false;
__thread struct alloc_region gc_thread_alloc_region[3];

static void pgc_init_thread ()
{
    gc_init_region(&tl_mixed_region);
    gc_init_region(&tl_unboxed_region);
    gc_init_region(&tl_code_region);
}

static void pgc_close_thread_regions()
{
    ensure_region_closed(&tl_code_region, PAGE_TYPE_CODE);
    ensure_region_closed(&tl_unboxed_region, PAGE_TYPE_UNBOXED);
    ensure_region_closed(&tl_mixed_region, PAGE_TYPE_MIXED);
}
#endif
