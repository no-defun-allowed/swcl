#include <stdio.h>

#include "os.h"
#include "gc.h"
#include "lispobj.h"
#include "gc-assert.h"
#include "gc-internal.h"
#include "gc-private.h"
#include "gencgc-internal.h"
#include "gencgc-private.h"
#include "forwarding-ptr.h"
#include "queue.h"
#include "tiny-lock.h"
#include "queue-suballocator.h"
#include "incremental-compact.h"
#include "walk-heap.h"

#include "genesis/closure.h"
#include "genesis/gc-tables.h"
#include "genesis/symbol.h"

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
static struct suballocator remset_suballocator = SUBALLOCATOR_INITIALIZER("compaction remset");
boolean compacting;
unsigned char *target_pages;

void compactor_init() {
  target_pages = calloc(dynamic_space_size / GENCGC_PAGE_BYTES, 1);
  if (!target_pages)
    lose("Failed to allocate target pages table");
}

/* Deciding how to compact */

static boolean should_compact(char *why) {
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
    fprintf(stderr, "%s, ratio = %.2f\n", why, ratio);
  else
    fprintf(stderr, "ratio = %.2f\n", ratio);
  return ratio > page_overhead_threshold;
}

static void pick_targets() {
  uword_t bytes_moving = 0, pages_moving = 0;
  page_index_t p = page_table_pages - 1;
  /* Ideally we'd like to avoid selecting pinned pages here, but the phase
   * ordering is tricky. We need to know (an over-estimation of) the pages
   * to move for mark() to log, we only find out which pages are pinned
   * while marking from the roots. */
  while (bytes_moving < bytes_to_copy) {
    if (!page_single_obj_p(p) &&
        !page_free_p(p) &&
        page_table[p].gen < PSEUDO_STATIC_GENERATION &&
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
  if (gen >= minimum_compact_gen && should_compact("Enabling remset")) {
    compacting = 1;
    target_generation = gen;
    pick_targets();
  } else {
    compacting = 0;
  }
}

/* Remset */

/* We cram a source into the low bits of a pointer, to save space in the
 * remset. */
static inline lispobj tag_source(lispobj *where, enum source s) { return (lispobj)where | (lispobj)s; }
static inline enum source source_from_tagged(lispobj t) { return t & 7; }
static inline lispobj *lispobj_from_tagged(lispobj t) { return (lispobj*)(t &~ 7); }

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

/* We need to know which object a slot resides in for two reasons:
 * - If the object has been moved, we need to adjust the location of
 *   the slot to update accordingly. We assume the slot does not move
 *   w.r.t the object, which seems fine to do.
 * - We need to inform some hash tables that they need to be rehashed,
 *   if we just invalidated some address-based hash.
 * We could figure out the start of an object from its slot, but I'd
 * prefer not to.
 */
void log_relevant_slot(lispobj *slot, lispobj *source, enum source source_type) {
  if (!remset_block) remset_block = suballoc_allocate(&remset_suballocator);
  if (remset_block->count == QBLOCK_CAPACITY) {
    commit_thread_local_remset();
    remset_block = suballoc_allocate(&remset_suballocator);
  }
  remset_block->elements[remset_block->count] = tag_source(slot, source_type);
  remset_block->elements[remset_block->count+1] = (lispobj)source;
  remset_block->count += 2;
}

/* Compacting */
static void move_objects() {
  /* Note that this function is very un-thread-safe; list linearisation
   * can cause a thread to copy any objects. But if it weren't for list
   * linearisation, no synchronisation between threads mightn't be needed, as
   * each thread would be confined to the pages it claimed.
   * But early experiements in parallel copying suggested we're bottlenecked
   * by refilling TLABs too. */
  for (page_index_t p = 0; p < page_table_pages; p++)
    if (target_pages[p] & !gc_page_pins[p]) {
      lispobj *end = (lispobj*)page_address(p + 1);
      /* Move every object in this page in the right generation. */
      for (lispobj *where = next_object((lispobj*)page_address(p), 0, end);
           where;
           /* We install forwarding pointers, so we'll conservatively assume
            * the object is at least two words large. */
           where = next_object(where, 2, end)) {
        if (gc_gen_of((lispobj)where, -1) == target_generation) {
          lispobj bogus = compute_lispobj(where);
          scavenge(&bogus, 1);
        }
      }
      /* Free all lines we just copied from. */
      uword_t decrement = 0;
      char *allocation = (char*)allocation_bitmap;
      for_lines_in_page (l, p)
        if (DECODE_GEN(line_bytemap[l]) == target_generation) {
          line_bytemap[l] = 0;
          allocation[l] = 0;
          decrement++;
        }
      set_page_bytes_used(p, page_bytes_used(p) - LINE_SIZE * decrement);
      generations[target_generation].bytes_allocated -= LINE_SIZE * decrement;
      bytes_allocated -= LINE_SIZE * decrement;
      if (page_words_used(p) == 0) {
        page_table[p].type = FREE_PAGE_FLAG;
        page_table[p].scan_start_offset_ = 0;
      }
    }
}

/* Fix up the address of a slot, when the object containing the slot
 * may have been forwarded. */
static inline lispobj *forward_slot(lispobj *slot, lispobj *source) {
  lispobj *forwarded_source = native_pointer(follow_fp((lispobj)source));
  return (lispobj*)((lispobj)forwarded_source + ((lispobj)slot - (lispobj)source));
}

static void fix_slot(lispobj *slot, lispobj *source, enum source source_type) {
  slot = forward_slot(slot, source);
  source = native_pointer(follow_fp((lispobj)source));
  switch (source_type) {
  case SOURCE_NORMAL:
    *slot = follow_maybe_fp(*slot);
    if (widetag_of(source) == SIMPLE_VECTOR_WIDETAG &&
        vector_flagp(*source, VectorHashing)) {
      /* Tell any hash tables to rehash. This causes unnecessary rehashing,
       * but we compact infrequently and incrementally, so it shouldn't
       * hurt much. */
      /* TODO: should "rehash a hash table" be part of the source type? */
      struct vector* kv_vector = (struct vector*)source;
      KV_PAIRS_REHASH(kv_vector->data) |= make_fixnum(1);
    }
    break;
  case SOURCE_ZERO_TAG:
    /* follow_maybe_fp doesn't care what the tag is, just that it
     * satisfies is_lisp_pointer. */
    *slot = (lispobj)native_pointer(follow_maybe_fp(*slot | INSTANCE_POINTER_LOWTAG));
    break;
  case SOURCE_CLOSURE: {
    /* SOURCE_CLOSURE can only be the source type of the taggedptr of
     * a closure. */
    struct closure *closure = (struct closure*)source;
    closure->fun = fun_self_from_taggedptr(follow_fp(fun_taggedptr_from_self(closure->fun)));
    break;
  }
  case SOURCE_SYMBOL_NAME: {
    /* SOURCE_SYMBOL_NAME can only be the source type of the s->name
     * slot. */
    struct symbol *s = (struct symbol*)source;
    set_symbol_name(s, follow_fp(decode_symbol_name(s->name)));
    break;
  }
  case SOURCE_FDEFN_RAW: {
    /* SOURCE_FDEFN_RAW can only be the source type of the fdefn->raw_addr
     * slot. */
    struct fdefn *f = (struct fdefn*)source;
    lispobj obj = decode_fdefn_rawfun(f);
    f->raw_addr += (sword_t)(follow_fp(obj) - obj);
  }
  }
}

static void fix_slots() {
  int c = 0;
  for (; remset; remset = remset->next) {
    for (int n = 0; n < remset->count; n += 2) {
      lispobj *slot = lispobj_from_tagged(remset->elements[n]),
              *source = (lispobj*)remset->elements[n + 1];
      enum source source_type = source_from_tagged(remset->elements[n]);
      fix_slot(slot, source, source_type);
      c++;
    }
  }
}

void run_compaction() {
  if (compacting) {
    /* Check again, in case fragmentation somehow improves.
     * Not likely, but it's a cheap test which avoids effort. */
    if (should_compact("Performing compaction")) {
      move_objects();
      fix_slots();
      gc_close_collector_regions(0);
      should_compact("I just moved, but still");
    }
    memset(target_pages, 0, page_table_pages);
    remset = NULL;
    suballoc_release(&remset_suballocator);
  }
  compacting = 0;
}
