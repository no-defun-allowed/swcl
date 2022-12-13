#include <stdio.h>

#include "os.h"
#include "gc.h"
#include "lispobj.h"
#include "gc-assert.h"
#include "gc-internal.h"
#include "gencgc-internal.h"
#include "gencgc-private.h"
#include "queue.h"
#include "incremental-compact.h"

/* Maximum ratio between pages used and pages "needed" to compact. */
float page_overhead_threshold = 1.3;
/* Minimum fraction of bytes used on a page to compact it. */
float page_utilisation_threshold = 0.5;
/* Maximum number of bytes to copy in one collection. */
uword_t bytes_to_copy = 2000000;
/* Minimum generation to consider compacting when collecting. */
generation_index_t minimum_compact_gen = 1;

boolean compacting = 1;
/* A queue of sources and pointers to interesting slots. */
static struct Qblock *remset = NULL;
unsigned char *target_pages;

void compactor_init() {
  target_pages = calloc(dynamic_space_size / GENCGC_PAGE_BYTES, 1);
  if (!target_pages)
    lose("Failed to allocate target pages table");
}

static boolean should_compact() {
  /* If there are many more small-object pages than there could
   * be, start compacting. */
  uword_t pages = 0, bytes = 0;
  for (page_index_t p = 0; p < page_table_pages; p++) {
    if (!page_free_p(p) && !page_single_obj_p(p)) {
      pages++;
      bytes += page_bytes_used(p);
    }
  }
  float ratio = (float)(pages * GENCGC_PAGE_BYTES) / bytes;
  return ratio > page_overhead_threshold;
}

static void pick_targets() {
  uword_t bytes_moving = 0, pages_moving = 0;
  page_index_t p = page_table_pages - 1;
  while (bytes_moving < bytes_to_copy) {
    if (!page_single_obj_p(p) &&
        !page_free_p(p) &&
        !gc_page_pins[p] &&
        (float)(page_bytes_used(p)) / GENCGC_PAGE_BYTES < page_utilisation_threshold) {
      bytes_moving += page_bytes_used(p);
      pages_moving++;
    }
    if (p == 0) break;
    p--;
  }
  fprintf(stderr, "Moving %ld pages with %ld/%ld bytes used\n",
          pages_moving, bytes_moving, bytes_to_copy);
}

void consider_compaction(generation_index_t gen) {
  if (gen >= minimum_compact_gen && should_compact()) {
    compacting = 1;
    pick_targets();
  } else {
    compacting = 0;
  }
}

void run_compaction() {
  if (compacting) {
  }
  compacting = 0;
}

static lispobj tag_source(lispobj *base, enum source s) {
  return make_lispobj(base, (int)s);
}

void log_for_compactor(lispobj source, lispobj *where, enum source type) {
  (void)source; (void)where; (void)type;
}
