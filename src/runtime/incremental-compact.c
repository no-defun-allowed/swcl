#include <stdio.h>

#include "os.h"
#include "gc.h"
#include "lispobj.h"
#include "gc-assert.h"
#include "gc-internal.h"
#include "gencgc-internal.h"
#include "gencgc-private.h"

/* Maximum ratio between pages used and pages "needed" to compact. */
float page_overhead_threshold = 1.3;
/* Minimum ratio of bytes used on a page to move it. */
float page_utilisation_threshold = 0.5;
/* Maximum number of bytes to copy in one collection. */
uword_t byte_target = 2000000;
/* Minimum generation to consider compacting when collecting. */
generation_index_t minimum_compact_gen = 1;

boolean compacting = 1;

boolean should_compact() {
  /* If there are many more small-object pages than there could 
   * be, start compacting. */
  uword_t pages = 0, bytes = 0;
  for (page_index_t p = 0; p < page_table_pages; p++) {
    if (!page_free_p(p) && !page_single_obj_p(p)) {
      pages++;
      bytes += page_bytes_used(p);
    }
  }
  float ratio = (float)(pages * GENCGC_PAGE_BYTES)/bytes;
  /* fprintf(stderr, "ratio = %.2f\n", ratio); */
  return ratio > page_overhead_threshold;
}

void pick_targets() {
  uword_t bytes_remaining = byte_target;
  page_index_t p = page_table_pages - 1;
  while (1) {
    if (!page_single_obj_p(p) &&
        !page_free_p(p) && 
        (float)(page_bytes_used(p)) / GENCGC_PAGE_BYTES < page_utilisation_threshold) {
      if (page_bytes_used(p) > bytes_remaining) break;
      /* fprintf(stderr, "Moving page #%ld with %d bytes used\n", */
      /*         p, page_bytes_used(p)); */
      bytes_remaining -= page_bytes_used(p);
    }
    if (p == 1) break;
    p--;
  }
}

void consider_compaction(generation_index_t gen) {
  if (gen >= minimum_compact_gen && should_compact()) {
    compacting = 1;
    pick_targets();
  } else {
    compacting = 0;
  }
}

void run_compaction() {}
