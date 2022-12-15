#include <stdio.h>

#include "os.h"
#include "gc.h"
#include "lispobj.h"
#include "gc-assert.h"
#include "gc-internal.h"
#include "gencgc-internal.h"
#include "gencgc-private.h"
#include "queue.h"
#include "tiny-lock.h"
#include "queue-suballocator.h"
#include "incremental-compact.h"
#include "walk-heap.h"

/* Maximum ratio between pages used and pages "needed" to compact. */
float page_overhead_threshold = 1.3;
/* Minimum fraction of bytes used on a page to compact it. */
float page_utilisation_threshold = 0.5;
/* Maximum number of bytes to copy in one collection. */
uword_t bytes_to_copy = 2000000;
/* Minimum generation to consider compacting when collecting. */
generation_index_t minimum_compact_gen = 1;

static generation_index_t target_generation;
/* A queue of interesting slots. */
static struct Qblock *remset;
static lock_t remset_lock;
static struct suballocator remset_suballocator = SUBALLOCATOR_INITIALIZER;
boolean compacting;
unsigned char *target_pages;

void compactor_init() {
  target_pages = calloc(dynamic_space_size / GENCGC_PAGE_BYTES, 1);
  if (!target_pages)
    lose("Failed to allocate target pages table");
}

/* Deciding how to compact */

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
  if (ratio > page_overhead_threshold)
    fprintf(stderr, "Triggering compaction, ratio = %.2f\n", ratio);
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
      target_pages[p] = 1;
    }
    if (p == 0) break;
    p--;
  }
}


void consider_compaction(generation_index_t gen) {
  if (gen >= minimum_compact_gen && should_compact()) {
    compacting = 1;
    target_generation = gen;
    pick_targets();
  } else {
    compacting = 0;
  }
}

/* Remset */

static lispobj tag_source(lispobj *where, enum source s) {
  return make_lispobj(where, (int)s);
}

static _Thread_local struct Qblock *remset_block = NULL;

void commit_thread_local_remset() {
  if (remset_block && remset_block->count) {
    acquire_lock(&remset_lock);
    remset_block->next = remset;
    remset = remset_block;
    remset_block = NULL;
    release_lock(&remset_lock);
  }
}

void log_relevant_slot(lispobj *where, enum source source) {
  if (!remset_block) remset_block = suballoc_allocate(&remset_suballocator);
  if (remset_block->count == QBLOCK_CAPACITY) {
    commit_thread_local_remset();
    remset_block = suballoc_allocate(&remset_suballocator);
  }
  remset_block->elements[remset_block->count++] = 42; // tag_source(where, source);
}

/* Compacting */

static void count_pages() {
  uword_t bytes = 0, objects = 0;
  for (page_index_t p = 0; p < page_table_pages; p++)
    if (target_pages[p]) {
      lispobj *limit = (lispobj*)page_address(p + 1);
      for (lispobj *where = next_object((lispobj*)page_address(p), 0, limit);
           where;
           where = next_object(where, object_size(where), limit))
        if (gc_gen_of((lispobj)where, 0) <= target_generation) {
          bytes += N_WORD_BYTES * object_size(where);
          objects++;
        }
    }
  uword_t pointers = 0;
  for (struct Qblock *b = remset; b; b = b->next)
    pointers += b->count;
  fprintf(stderr, "gen %d: Saw %ld pointers, copied %ld bytes %ld objects, %.1f pointers/object\n",
          target_generation, pointers, bytes, objects, (float)pointers/objects);
}

void run_compaction() {
  if (compacting) {
    count_pages();
    memset(target_pages, 0, page_table_pages);
    remset = NULL;
    suballoc_release(&remset_suballocator);
  }
  compacting = 0;
}
