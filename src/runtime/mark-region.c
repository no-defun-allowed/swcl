#define _GNU_SOURCE
#include <pthread.h>
#include <unistd.h>
#include <semaphore.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdatomic.h>
#include <string.h>
#include <sys/time.h>

#include "align.h"
#include "os.h"
#include "gc.h"
#include "code.h"
#include "genesis/symbol.h"
#include "runtime.h"
#include "validate.h"
#include "gc-assert.h"
#include "core.h"
#include "interr.h"
#include "globals.h"
#include "lispobj.h"
#include "queue.h"
#include "incremental-compact.h"
#include "tiny-lock.h"
#include "gc-thread-pool.h"
#include "queue-suballocator.h"
#include "mpk.h"

#include "genesis/cons.h"
#include "genesis/gc-tables.h"
#include "genesis/instance.h"
#include "genesis/closure.h"
#include "genesis/hash-table.h"
#include "genesis/split-ordered-list.h"

#define PAGES_CLAIMED_PER_THREAD 128
#define PREFETCH_DISTANCE 32

//#define LOG_COLLECTIONS
#ifndef ENABLE_COMPACTION
#define ENABLE_COMPACTION 1
#endif

/* The idea of the mark-region collector is to avoid copying where
 * possible, and instead reclaim as much memory in-place as possible.
 * The result is that we save time on copying objects (especially leaf
 * objects), and parallelisation is easier.
 * The design is loosely inspired by the Immix collector designed by
 * Blackburn and McKinley, mostly in being able to reclaim both smaller
 * lines and larger pages. But we plan to have a separate compaction
 * phase, rather than copying and marking in one pass, to make parallel
 * compacting easier, and to enable concurrent marking. */

/* Metering */
static struct {
  _Atomic(uword_t) consider, scavenge, cards, prefix, sort;
  _Atomic(uword_t) trace, trace_alive, trace_running;
  _Atomic(uword_t) sweep, weak, sweep_lines, sweep_pages;
  _Atomic(uword_t) compact, copy, fix, compact_resweep, raise;
  uword_t fresh_pointers; uword_t pinned_pages; uword_t compacts;
} meters = { 0 };
static unsigned int collection = 0;
#define METER(name, action) \
  { uword_t before = get_time(); \
  action; \
  atomic_fetch_add(&meters.name, get_time() - before); }

void mr_print_meters() {
#define NORM(x) (collection ? meters.x / collection : 0)
  fprintf(stderr,
          "collection %d (%.0f%% compacting):\n"
          "  %ldus consider\n"
          "  %ld scavenge (%ld prefixes, %ld sorting, %ld cards) %ld trace (%ld alive %ld running)\n"
          "  %ld sweep (%ld lines %ld pages) %ld compact (%ld copy %ld fix %ld resweep)\n"
          "  %ld raise; %ldB fresh %ldpg pinned\n",
          collection,
          collection ? 100.0 *  (float)meters.compacts / collection : 0.0,
          NORM(consider), NORM(scavenge), NORM(prefix), NORM(sort), NORM(cards),
          NORM(trace), NORM(trace_alive), NORM(trace_running),
          NORM(sweep), NORM(sweep_lines), NORM(sweep_pages),
          NORM(compact), NORM(copy), NORM(fix), NORM(compact_resweep),
          NORM(raise), NORM(fresh_pointers), NORM(pinned_pages));
#undef NORM
}
void mr_reset_meters() { meters = (typeof(meters)){ 0 }; collection = 0; }

static uword_t get_time() {
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return t.tv_sec * 1000000 + t.tv_nsec/1000;
}

static void allocate_bitmap(uword_t **bitmap, uword_t size,
                            const char *description) {
  *bitmap = calloc(size, 1);
  if (!*bitmap)
    lose("Failed to allocate %s (of %lu bytes)", description, size);
}

/* Initialisation */
uword_t *allocation_bitmap;
_Atomic(uword_t) *mark_bitmap;
unsigned char *line_bytemap;
line_index_t line_count;
uword_t mark_bitmap_size;
static struct free_pages_t {
  int count;
  page_index_t *indices;
} free_pages_by_type[8];

static void allocate_bitmaps() {
  uword_t pages = dynamic_space_size / GENCGC_PAGE_BYTES;
  mark_bitmap_size = bitmap_size(pages);
  allocate_bitmap(&allocation_bitmap, mark_bitmap_size, "allocation bitmap");
  allocate_bitmap((uword_t**)&mark_bitmap, mark_bitmap_size, "mark bitmap");
  line_count = dynamic_space_size / LINE_SIZE;
  allocate_bitmap((uword_t**)&line_bytemap, line_count, "line bytemap");
  for (int i = 0; i < 8; i++)
    allocate_bitmap((uword_t**)&free_pages_by_type[i].indices,
                    pages * sizeof(page_index_t),
                    "free page array");
}

uword_t lines_used() {
  uword_t count = 0;
  for (line_index_t line = 0; line < line_count; line++)
    if (line_bytemap[line]) count++;
  return count;
}

bool line_marked(void *pointer) {
  return line_bytemap[address_line(pointer)];
}

/* Pages are allocated by consulting arrays of free pages. */

void rebuild_free_arrays() {
  for (int i = 0; i < 8; i++) free_pages_by_type[i].count = 0;
  for (page_index_t p = 0; p < page_table_pages; p++) {
    if (page_bytes_used(p) < GENCGC_PAGE_BYTES && !page_single_obj_p(p)) {
      unsigned char ty = page_table[p].type;
      free_pages_by_type[ty].indices[free_pages_by_type[ty].count++] = p;
    }
  }
}

/* Allocation slow-path */

/* Lines have generations in small pages, rather than pages. This is
 * necessary in order to allow reclaimed memory to be reused,
 * without having to compact old pages. */

generation_index_t gc_gen_of(lispobj obj, int defaultval) {
  page_index_t p = find_page_index((void*)obj);
  if (p < 0) return defaultval;
  if (page_single_obj_p(p))
    return page_table[p].gen;
  char c = line_bytemap[address_line((void*)obj)];
  if (UNMARK_GEN(c) == 0)
    return IS_FRESH(c) ? 0 : defaultval;
  return DECODE_GEN(c);
}

/* Allocation of small objects is done by finding contiguous lines
 * that can fit the object to allocate. Small objects can span lines
 * but cannot pages, so we examine lines in each page separately. */
#define DEF_FINDER(name, type, test, fail)              \
  static type name(type start, type end) {              \
    for (type where = start; where < end; where++)      \
      if (test) return where;                           \
    return fail; }

static line_index_t find_free_line(line_index_t start, line_index_t end) {
  /* memchr tends to have some vectorisation (e.g. GNU) or SWAR behind
   * it (musl, FreeBSD 12), so we use that instead of a naive loop. */
  unsigned char *where = memchr(line_bytemap + start, 0, end - start);
  if (!where) return end;
  return where - line_bytemap;
}
DEF_FINDER(find_used_line, line_index_t, line_bytemap[where], end);

/* Dirty cards so that the write barrier does not run the slow path
 * on young objects which we don't care about. */
#ifdef LISP_FEATURE_LOG_CARD_MARKS
static void pre_dirty_cards(void *start, void *end) {
  if (!gc_active_p) mpk_unlock_card_table();
  unsigned char state = gc_active_p ? CARD_UNMARKED : CARD_MARKED;
  //fprintf(stderr, "pre-dirty %x %p to %p\n", state, start, end);
  for (unsigned int card = addr_to_card_index(start);
       card < addr_to_card_index(end);
       card++) {
    gc_card_mark[card] = state;
  }
  if (!gc_active_p) mpk_lock_card_table();
}
#else
#define pre_dirty_cards(start, end)
#endif

/* Try to find space to fit a new object in the lines between `start`
 * and `end`. Updates `region` and returns true if we succeed, keeps
 * `region` untouched and returns false if we fail. The caller must
 * zero memory itself, if it wants zeroed memory. */
bool try_allocate_small(sword_t nbytes, struct alloc_region *region,
                        line_index_t start, line_index_t end) {
  sword_t nlines = ALIGN_UP(nbytes, LINE_SIZE) / LINE_SIZE;
  line_index_t where = start;
  while (1) {
    line_index_t chunk_start = find_free_line(where, end);
    if (chunk_start == -1) return false;
    line_index_t chunk_end = find_used_line(chunk_start, end);
    if (chunk_end - chunk_start >= nlines) {
      region->start_addr = line_address(chunk_start);
      region->free_pointer = line_address(chunk_start) + nbytes;
      region->end_addr = line_address(chunk_end);
      /* Rather odd to fill in the bytemap here and in
       * mr_update_closed_region. This is nice to ensure things
       * are immediately visible, m_u_c_r ensures we round off
       * properly. Do we need to round off then? Perhaps bump
       * bytes used here for each chunk; we have exclusive access
       * to the page and its state in the page table.. */
      for (line_index_t c = chunk_start; c < chunk_end; c++)
        line_bytemap[c] = gc_active_p ? 0 : FRESHEN_GEN(0);
      pre_dirty_cards(region->start_addr, region->end_addr);
      return true;
    }
    if (chunk_end == end) return false;
    where = chunk_end;
  }
}

/* Medium path for allocation, wherein we use another chunk that the
 * thread already claimed. */
bool try_allocate_small_after_region(sword_t nbytes, struct alloc_region *region) {
  /* Can't do this if we have no page. */
  if (!region->start_addr) return 0;
  /* We search to the end of this page. */
  line_index_t end = address_line(PTR_ALIGN_UP(region->end_addr, GENCGC_PAGE_BYTES));
  return try_allocate_small(nbytes, region, address_line(region->end_addr), end);
}

extern generation_index_t get_alloc_generation();

/* try_allocate_small_from_pages updates the start pointer to after the
 * claimed page. */
bool try_allocate_small_from_pages(sword_t nbytes, struct alloc_region *region,
                                   int page_type, generation_index_t gen,
                                   struct allocator_state *start) {
  gc_assert(gen != SCRATCH_GENERATION);
 again:
  int search_type = start->use_free_pages ? FREE_PAGE_FLAG : page_type;
  for (int i = start->index; i < free_pages_by_type[search_type].count; i++) {
    page_index_t where = free_pages_by_type[search_type].indices[i];
    if (page_bytes_used(where) <= GENCGC_PAGE_BYTES - nbytes &&
        !target_pages[where] &&
        (page_table[where].type == page_type || page_free_p(where)) &&
        page_table[where].gen != PSEUDO_STATIC_GENERATION &&
        try_allocate_small(nbytes, region,
                           page_to_line(where), page_to_line(where + 1))) {
      // mark-region has a different way of zeroing, so just tell prepare_pages
      // that the page is unboxed if it's boxed, so that it doesn't try to zero.
      if (!page_table[where].type)
          prepare_pages(1, where, where, page_type==PAGE_TYPE_CODE?page_type:0,
                        get_alloc_generation());
      set_page_type(page_table[where], page_type | OPEN_REGION_PAGE_FLAG);
      page_table[where].gen = 0;
      set_page_scan_start_offset(where, 0);
      start->index = i + 1;
      /* Update residency statistics. mr_update_closed_region will
       * enliven all lines on this page, so it's correct to set the
       * page bytes used like this. */
      page_bytes_t used = page_bytes_used(where), claimed = GENCGC_PAGE_BYTES - used;
      bytes_allocated += claimed;
      generations[gen].bytes_allocated += claimed;
      set_page_bytes_used(where, GENCGC_PAGE_BYTES);
      if (where + 1 > next_free_page) next_free_page = where + 1;
      return true;
    }
  }
  if (!start->use_free_pages) {
    start->use_free_pages = true;
    start->index = 0;
    goto again;
  }
  return false;
}

/* try_allocate_small_from_pages updates the start pointer to after the
 * claimed page. */
bool try_allocate_one_line(struct alloc_region *region,
                           int page_type, generation_index_t gen,
                           struct allocator_state *start) {
  gc_assert(gen != SCRATCH_GENERATION);
 again:
  int search_type = start->use_free_pages ? FREE_PAGE_FLAG : page_type;
  for (int i = start->index; i < free_pages_by_type[search_type].count; i++) {
    page_index_t where = free_pages_by_type[search_type].indices[i];
    if (page_bytes_used(where) < GENCGC_PAGE_BYTES &&
        !target_pages[where] &&
        (page_table[where].type == page_type || page_free_p(where)) &&
        page_table[where].gen != PSEUDO_STATIC_GENERATION) {
      // mark-region has a different way of zeroing, so just tell prepare_pages
      // that the page is unboxed if it's boxed, so that it doesn't try to zero.
      if (!page_table[where].type)
          prepare_pages(1, where, where, page_type==PAGE_TYPE_CODE?page_type:0,
                        get_alloc_generation());
      set_page_type(page_table[where], page_type | OPEN_REGION_PAGE_FLAG);
      page_table[where].gen = 0;
      set_page_scan_start_offset(where, 0);
      start->index = i + 1;
      /* Update residency statistics. mr_update_closed_region will
       * enliven all lines on this page, so it's correct to set the
       * page bytes used like this. */
      page_bytes_t used = page_bytes_used(where), claimed = GENCGC_PAGE_BYTES - used;
      bytes_allocated += claimed;
      generations[gen].bytes_allocated += claimed;
      set_page_bytes_used(where, GENCGC_PAGE_BYTES);
      if (where + 1 > next_free_page) next_free_page = where + 1;
      region->start_addr = region->free_pointer = region->end_addr = page_address(where);
      return true;
    }
  }
  if (!start->use_free_pages) {
    start->use_free_pages = true;
    start->index = 0;
    goto again;
  }
  return false;
}

/* Medium object allocation */

bool try_allocate_one_page(struct alloc_region *region,
                           int page_type, generation_index_t gen,
                           page_index_t *start) {
  gc_assert(gen != SCRATCH_GENERATION);
  struct free_pages_t fully_free_pages = free_pages_by_type[FREE_PAGE_FLAG];
  for (int i = *start; i < fully_free_pages.count; i++) {
    page_index_t p = fully_free_pages.indices[i];
    if (page_free_p(p)) {
      set_page_type(page_table[p], page_type | OPEN_REGION_PAGE_FLAG);
      page_table[p].gen = gen;
      set_page_scan_start_offset(p, 0);
      bytes_allocated += GENCGC_PAGE_BYTES;
            set_page_bytes_used(p, GENCGC_PAGE_BYTES);
      generations[gen].bytes_allocated += GENCGC_PAGE_BYTES;
      *start = i + 1;
      for_lines_in_page (l, p)
        line_bytemap[l] = gc_active_p ? 0 : FRESHEN_GEN(0);
      region->start_addr = region->free_pointer = page_address(p);
      region->end_addr = page_address(p + 1);
      pre_dirty_cards(region->start_addr, region->end_addr);
      if (p + 1 > next_free_page) next_free_page = p + 1;
      return true;
    }
  }
  return false;
}

/* Large object allocation */

DEF_FINDER(find_free_page, page_index_t, page_free_p(where), -1);
DEF_FINDER(find_used_page, page_index_t, !page_free_p(where), end);

page_index_t try_allocate_large(uword_t nbytes,
                                int page_type, generation_index_t gen,
                                struct allocator_state *start, page_index_t end,
                                uword_t *largest_hole) {
  gc_assert(gen != SCRATCH_GENERATION);
  uword_t pages_needed = ALIGN_UP(nbytes, GENCGC_PAGE_BYTES) / GENCGC_PAGE_BYTES;
  uword_t remainder = nbytes % GENCGC_PAGE_BYTES;
  page_index_t where = start->large_page;
  uword_t largest_hole_seen = 0;
  while (1) {
    page_index_t chunk_start = find_free_page(where, end);
    if (chunk_start == -1) return -1;
    /* TODO: this is suboptimal - the full extent of the free space is irrelevant
     * as long as it's at _least_ pages_needed. So find_used_page is a poor choice
     * of algorithm for this. It's not wrong, though I've seen it say (via added
     * printing) that it needed 2 pages but found 40000 pages. So it scanned
     * the entire page table before deciding yup, we have enough to work with..
     */
    page_index_t chunk_end = find_used_page(chunk_start, end);
    uword_t hole_size = chunk_end - chunk_start;
    if (hole_size >= pages_needed) {
      page_index_t last_page = chunk_start + pages_needed - 1;
      prepare_pages(1, chunk_start, last_page, page_type==PAGE_TYPE_CODE?page_type:0,
                    get_alloc_generation());
      for (page_index_t p = chunk_start; p <= last_page; p++) {
        set_page_type(page_table[p], SINGLE_OBJECT_FLAG | page_type);
        page_table[p].gen = gen;
        set_page_bytes_used(p,
                            (p == last_page && remainder > 0) ? remainder
                            : GENCGC_PAGE_BYTES);
        set_page_scan_start_offset(p,
                                   GENCGC_PAGE_BYTES * (p - chunk_start));
      }
      start->large_page = chunk_start + pages_needed;
      bytes_allocated += nbytes;
      generations[gen].bytes_allocated += nbytes;
      if (last_page + 1 > next_free_page) next_free_page = last_page + 1;
      if (page_type != PAGE_TYPE_UNBOXED)
        pre_dirty_cards(page_address(chunk_start), page_address(chunk_start + pages_needed));
      return chunk_start;
    }
    if (hole_size > largest_hole_seen) largest_hole_seen = hole_size;
    if (chunk_end == end) {
      *largest_hole = largest_hole_seen * GENCGC_PAGE_BYTES;
      return -1;
    }
    where = chunk_end;
  }
  /* Shouldn't end up here, really. */
  return -1;
}

CPU_SPLIT
void mr_update_closed_region(struct alloc_region *region, generation_index_t gen) {
  /* alloc_regions never span multiple pages. */
  page_index_t the_page = find_page_index(region->start_addr);
  if (!(page_table[the_page].type & OPEN_REGION_PAGE_FLAG))
    lose("Page %d wasn't open", the_page);

  /* Mark the lines as allocated. */
  unsigned char *lines = line_bytemap;
  for_lines_in_page (l, the_page) {
    /* Remember to copy mark bits set by GC and fresh bits set
     * by the allocator. */
    unsigned char copied = COPY_MARK(lines[l], ENCODE_GEN(gen));
    lines[l] = UNMARK_GEN(lines[l]) ? lines[l] : copied;
  }
  page_table[the_page].type &= ~(OPEN_REGION_PAGE_FLAG);
  gc_set_region_empty(region);
}

/* Marking */

static generation_index_t generation_to_collect = 0;

static inline uword_t object_index(lispobj object) {
  return (uword_t)((object - DYNAMIC_SPACE_START) >> N_LOWTAG_BITS);
}
static inline lispobj* index_to_object(uword_t index) {
  return index*2 + (lispobj*)DYNAMIC_SPACE_START;
}
static inline uword_t mark_bitmap_word_index(void *where) {
  return object_index((uword_t)where) / N_WORD_BITS;
}
static bool object_marked_p(lispobj object) {
  uword_t index = object_index(object);
  return (mark_bitmap[index / N_WORD_BITS] >> (index % N_WORD_BITS)) & 1;
}
static bool set_mark_bit(lispobj object) {
  uword_t index = object_index(object);
  uword_t bit_index = index % N_WORD_BITS, word_index = index / N_WORD_BITS;
  uword_t bit = ((uword_t)(1) << bit_index);
  /* Avoid doing an atomic op if we're obviously not going to win it. */
  if (mark_bitmap[word_index] & bit) return 0;
  /* Return if we claimed successfully i.e. the bit was 0 before. */
  return ((~atomic_fetch_or(mark_bitmap + word_index, bit)) >> bit_index) & 1;
}

static bool in_dynamic_space(lispobj object) {
  return find_page_index((void*)object) != -1;
}

bool taggedptr_alivep_impl(lispobj object) {
  return !in_dynamic_space(object) || object_marked_p(object) || gc_gen_of(object, 0) > generation_to_collect;
}

/* The number of blocks on the grey list and being processed.
 * Tracing terminates when we end up with 0 blocks in flight again. */
static _Atomic(sword_t) blocks_in_flight = 0;
static lock_t grey_list_lock = LOCK_INITIALIZER;
static struct Qblock *grey_list = NULL;
static struct suballocator grey_suballocator = SUBALLOCATOR_INITIALIZER("grey stack");

/* Thanks to Larry Masinter for suggesting that I use per-thread
 * free lists, rather than hurting my head on lock-free free lists.
 * (Say that five times fast.) */
static _Thread_local struct Qblock *recycle_list = NULL;
/* The "output packet" from "A Parallel, Incremental and Concurrent GC
 * for Servers". The "input packet" is block in trace_everything. */
static _Thread_local struct Qblock *output_block;

static struct Qblock *grab_qblock() {
  struct Qblock *block;
  if (recycle_list) {
    block = recycle_list;
    recycle_list = block->next;
    block->count = 0;
  } else {
    block = suballoc_allocate(&grey_suballocator);
  }
  atomic_fetch_add(&blocks_in_flight, 1);
  return block;
}
static void recycle_qblock(struct Qblock *block) {
  if (block->count == -1) lose("%p is already dead", block);
  block->count = -1;
  block->next = recycle_list;
  recycle_list = block;
  atomic_fetch_add(&blocks_in_flight, -1);
}

static void add_words_used(void *where, uword_t count) {
  page_index_t p = find_page_index(where);
  if (page_single_obj_p(p)) {
    uword_t byte_count = count * N_WORD_BYTES;
    while (byte_count >= GENCGC_PAGE_BYTES) {
      set_page_bytes_used(p, GENCGC_PAGE_BYTES);
      byte_count -= GENCGC_PAGE_BYTES;
      p++;
    }
    if (byte_count)
      set_page_bytes_used(p, byte_count);
  }
}

static void mark_cons_line(struct cons *c) {
  /* CONS cells never span lines, because they are aligned on
   * cons pages. */
  line_bytemap[address_line(c)] = MARK_GEN(line_bytemap[address_line(c)]);
}
static void mark_lines(lispobj *p) {
  uword_t word_count = object_size(p);
  if (!page_single_obj_p(find_page_index(p))) {
    line_index_t first = address_line(p), last = address_line(p + word_count - 1);
    for (line_index_t line = first; line <= last; line++)
      line_bytemap[line] = MARK_GEN(line_bytemap[line]);
  }
  add_words_used(p, word_count);
}

/* Generation of the object being scavenged,
 * for finding old->young pointers */
static _Thread_local generation_index_t dirty_generation_source = 0;
static _Thread_local bool dirty = 0;
static _Thread_local lispobj *source_object;

static void mark(lispobj object, lispobj *where, enum source source_type) {
  if (is_lisp_pointer(object) && in_dynamic_space(object)) {

    lispobj *np = native_pointer(object);
    if (gc_gen_of(object, 0) < dirty_generation_source)
      /* Used to find dirty pages in mr_scavenge_root_gens. */
      dirty = 1;
    if (gc_gen_of(object, 0) > generation_to_collect)
      return;

    /* Fix up embedded simple-fun objects. */
    if (functionp(object) && embedded_obj_p(widetag_of(np))) {
      lispobj *base = (lispobj*)fun_code_header((struct simple_fun*)np);
      object = make_lispobj(base, OTHER_POINTER_LOWTAG);
    }
#if ENABLE_COMPACTION
    if (where)
      log_slot(object, where, source_object, source_type);
#endif
    /* Enqueue onto mark queue */
    if (set_mark_bit(object)) {
      if (!output_block || output_block->count == QBLOCK_CAPACITY) {
        struct Qblock *next = grab_qblock();
        if (output_block) {
          acquire_lock(&grey_list_lock);
          output_block->next = grey_list;
          grey_list = output_block;
          release_lock(&grey_list_lock);
        }
        output_block = next;
      }
      output_block->elements[output_block->count++] = object;
    }
  }
}

/* Tracing configuration */

/* A lock protecting structures to do with weak references. */
static lock_t weak_lists_lock = LOCK_INITIALIZER;
static bool interesting_pointer_p(lispobj object) {
  return in_dynamic_space(object);
}
static inline bool pointer_survived_gc_yet(lispobj obj) {
    return taggedptr_alivep_impl(obj);
}
static void watch_deferred(lispobj *where, uword_t start, uword_t end);
#define LOCK acquire_lock(&weak_lists_lock)
#define UNLOCK release_lock(&weak_lists_lock)
#define ACTION mark
#define WATCH_DEFERRED watch_deferred
#define TRACE_NAME trace_other_object
#define HT_ENTRY_LIVENESS_FUN_ARRAY_NAME mr_alivep_funs
#define STRENGTHEN_WEAK_REFS 0
#include "var-io.h"
#include "trace-object.inc"

static void trace_object(lispobj object) {
 again:
  source_object = native_pointer(object);
  if (listp(object)) {
    struct cons *c = CONS(object);
    mark(c->car, &c->car, SOURCE_NORMAL);
    lispobj next = c->cdr;
    /* "Tail-recurse" on the cdr, unless we're recording dirty cards.
     * This saves us from continuously writing into grey blocks,
     * but loses memory parallelism. */
    if (is_lisp_pointer(next)) {
      if (!dirty_generation_source) {
        /* Fix up embedded simple-fun objects. */
        lispobj *np = native_pointer(next);
        if (functionp(next) && embedded_obj_p(widetag_of(np))) {
          lispobj *base = (lispobj*)fun_code_header((struct simple_fun*)np);
          next = make_lispobj(base, OTHER_POINTER_LOWTAG);
          np = base;
        }
#if ENABLE_COMPACTION
        /* Inlined logic from mark() */
        log_slot(c->cdr, &c->cdr, native_pointer(object), SOURCE_NORMAL);
#endif
        if (!pointer_survived_gc_yet(next)) {
          if (set_mark_bit(next)) {
            if (listp(next))
              mark_cons_line(CONS(next));
            else
              mark_lines(np);
            object = next;
            goto again;
          }
        }
      } else {
        mark(next, &c->cdr, SOURCE_NORMAL);
      }
    }
  } else {
    lispobj *p = native_pointer(object);
    trace_other_object(p);
  }
}

static bool work_to_do(struct Qblock **where) {
  if (output_block) {
    *where = output_block;
    output_block = NULL;
    return 1;
  } else {
    acquire_lock(&grey_list_lock);
    if (grey_list) {
      *where = grey_list;
      grey_list = grey_list->next;
      release_lock(&grey_list_lock);
      return 1;
    }
    release_lock(&grey_list_lock);
    return 0;
  }
}

static _Atomic(uword_t) traced;          /* Number of objects traced. */
static bool threads_did_any_work;
static void trace_step() {
  uword_t local_traced = 0, start_time = get_time(), running_time = 0;
  bool did_anything = 0;
  uword_t backoff = 1;
  while (atomic_load(&blocks_in_flight)) {
    /* Back off if we're out of work, since there isn't anything
     * more intelligent we can do, I think. */
    struct Qblock *block;
    if (!work_to_do(&block)) {
      usleep(backoff);
      backoff *= 2;
      /* Don't wait too long. */
      if (backoff > 100) backoff = 100;
      continue;
    }
    uword_t trace_start = get_time();
    backoff = 1;
    did_anything = 1;
    int count = block->count;
    for (int n = 0; n < count; n++) {
      lispobj obj = block->elements[n];
      if (n + PREFETCH_DISTANCE < count)
        __builtin_prefetch(native_pointer(block->elements[n + PREFETCH_DISTANCE]));
      local_traced++;
      /* Per the Immix paper, we mark lines while tracing, to
       * cover memory latency. We also would need to compute object size,
       * which reads object data most of the time, so doing it while tracing
       * an object (when we also have to read data) is better than doing it
       * while marking a pointer (when we don't have to read data). */
      if (listp(obj))
        mark_cons_line(CONS(obj));
      else
        mark_lines(native_pointer(obj));
      trace_object(obj);
    }
    recycle_qblock(block);
    running_time += get_time() - trace_start;
  }
  if (did_anything) threads_did_any_work = 1;
  atomic_fetch_add(&traced, local_traced);
  atomic_fetch_add(&meters.trace_alive, get_time() - start_time);
  atomic_fetch_add(&meters.trace_running, running_time);
  recycle_list = NULL;
}

static bool parallel_trace_step() {
  threads_did_any_work = 0;
  run_on_thread_pool(trace_step);
  suballoc_release(&grey_suballocator);
  return threads_did_any_work;
}

/* We logged interesting pointers already, when tracing weak objects.
 * So not having a source is okay here. */
static void mark_weak(lispobj obj) { mark(obj, NULL, SOURCE_NORMAL); }

static void __attribute__((noinline)) trace_everything() {
  while (parallel_trace_step()) test_weak_triggers(pointer_survived_gc_yet, mark_weak);
}

/* Conservative pointer scanning */

bool allocation_bit_marked(void *address) {
  uword_t i = object_index((uword_t)address);
  return (allocation_bitmap[i / N_WORD_BITS] >> (i % N_WORD_BITS)) & 1;
}

void set_allocation_bit_mark(void *address) {
  uword_t i = object_index((uword_t)address);
  allocation_bitmap[i / N_WORD_BITS] |= (uword_t)1 << (i % N_WORD_BITS);
}

static void compute_allocations(void *address) {
  line_index_t l = address_line(address), start, end;
  page_index_t this_page = find_page_index(address);
  /* Spans of fresh lines exist inside pages, so don't search outside the
   * bounds of this page. */
  line_index_t first_line = page_to_line(this_page),
               last_line = first_line + LINES_PER_PAGE;
  /* Don't unfreshen lines when the mutator could still be
   * allocating into them. Forgetting this causes
   * brothertree.impure.lisp to fail. */
  /* TODO: We can unfreshen if the page is not in a TLAB, right?
   * But I daren't race if another thread begins allocating into the page. */
  bool unfreshen = gc_active_p;
  /* Find the last previous unfresh line. */
  for (start = l; start != first_line - 1 && IS_FRESH(line_bytemap[start]); start--)
    if (unfreshen) line_bytemap[start] = UNFRESHEN_GEN(line_bytemap[start]);
  start++;                       /* Go back to first fresh line. */
  /* Find the first subsequent unfresh line. */
  for (end = l + 1; end != last_line && IS_FRESH(line_bytemap[end]); end++)
    if (unfreshen) line_bytemap[end] = UNFRESHEN_GEN(line_bytemap[end]);
  if (gc_active_p)
    meters.fresh_pointers += (end - start) * LINE_SIZE;
  /* Now we have found the span of fresh objects which encloses the address,
   * and we mark each contiguous object in the allocation bitmap. */
  unsigned char *allocations = (unsigned char*)allocation_bitmap;
  if (page_table[this_page].type == PAGE_TYPE_CONS) {
    /* Assume that every possible cons cell is live. The worst we can do
     * is to create a (0 . 0) cell out of nowhere, as the allocator pre-zeroes
     * memory. */
    for (line_index_t l = start; l < end; l++) allocations[l] = 0xFF;
  } else {
    /* Walk the span to find object starts. */
    lispobj *where = (lispobj*)line_address(start), *limit = (lispobj*)line_address(end);
    while (where < limit) {
      /* The first word can only be zero if we are looking at a cons
       * cell, and we don't have cons cells on non-cons pages, so just
       * skip it. */
      if (*where != 0)
        set_allocation_bit_mark(where);
      where += object_size(where);
    }
  }
}

static lispobj *find_object(uword_t address, uword_t start) {
  lispobj *np = native_pointer(address);
  page_index_t p = find_page_index(np);
  if (p == -1) return 0;
  bool fresh = IS_FRESH(line_bytemap[address_line(np)]);
  if (page_free_p(p)) return 0;
  if (page_table[p].type == PAGE_TYPE_CONS) {
    if (fresh) return np;
    /* CONS cells are always aligned, and the mutator is allowed to be lazy
     * w.r.t putting down allocation bits, so just use alignment. */
    return allocation_bit_marked(np) ? np : 0;
  } else {
    if (fresh) compute_allocations(np);
    uword_t first_bit_index = object_index(address);
    sword_t first_word_index = first_bit_index / N_WORD_BITS;
    sword_t last_word_index = mark_bitmap_word_index((void*)start);
    for (sword_t i = first_word_index; i >= last_word_index; i--) {
      uword_t word = allocation_bitmap[i];
      /* Find the last object which is not after this pointer. */
      while (word) {
        int last_bit_set = N_WORD_BITS - 1 - __builtin_clzl(word);
        lispobj *location = index_to_object(N_WORD_BITS * i + last_bit_set);
        if (location <= np) {
          /* Found a candidate - now check that the pointer is inside
           * this object, and make sure not to produce an embedded
           * object. */
          if (embedded_obj_p(widetag_of(location)))
            location = (lispobj*)fun_code_header((struct simple_fun*)location);
          if (np >= location + object_size(location))
            return 0;
          return location;
        }
        /* Remove the bit, try again */
        word &= ~((uword_t)(1) << last_bit_set);
      }
    }
    return 0;
  }
}

lispobj *search_dynamic_space(void *pointer) {
  return find_object((uword_t)pointer, DYNAMIC_SPACE_START);
}

/* Sweeping ("regioning"?) */

static void local_smash_weak_pointers()
{
    struct weak_pointer *wp, *next_wp;
    for (wp = weak_pointer_chain; wp != WEAK_POINTER_CHAIN_END; wp = next_wp) {
        gc_assert(widetag_of(&wp->header) == WEAK_POINTER_WIDETAG);
        next_wp = get_weak_pointer_next(wp);
        reset_weak_pointer_next(wp);
        lispobj pointee = wp->value;
        gc_assert(is_lisp_pointer(pointee));
        if (!pointer_survived_gc_yet(pointee))
            wp->value = UNBOUND_MARKER_WIDETAG;
    }
    weak_pointer_chain = WEAK_POINTER_CHAIN_END;

    struct cons* vectors = weak_vectors;
    while (vectors) {
        struct vector* vector = (struct vector*)vectors->car;
        vectors = (struct cons*)vectors->cdr;
        UNSET_WEAK_VECTOR_VISITED(vector);
        sword_t len = vector_len(vector);
        sword_t i;
        for (i = 0; i<len; ++i) {
            lispobj obj = vector->data[i];
            // Ignore non-pointers
            if (is_lisp_pointer(obj) && !pointer_survived_gc_yet(obj))
                vector->data[i] = NIL;
        }
    }
    weak_vectors = 0;
}

static void reset_statistics() {
  traced = 0;
  for (page_index_t p = 0; p <= page_table_pages; p++) {
    if (page_single_obj_p(p) &&
        (page_table[p].gen == generation_to_collect || generation_to_collect == PSEUDO_STATIC_GENERATION)) {
      generations[page_table[p].gen].bytes_allocated -= page_bytes_used(p);
      set_page_bytes_used(p, 0);
    }
  }
}

/* Pulled out these functions to clue auto-vectorisation. */
CPU_SPLIT
static page_bytes_t count_dead_bytes(page_index_t p) {
  unsigned char dead = ENCODE_GEN(generation_to_collect);
  page_bytes_t n = 0;
  unsigned char *lines = line_bytemap;
  for_lines_in_page(l, p)
    if (UNFRESHEN_GEN(lines[l]) == dead) n++;
  return n * LINE_SIZE;
}

CPU_SPLIT
static void sweep_small_page(page_index_t p) {
  unsigned char unmarked = ENCODE_GEN(generation_to_collect),
                marked = MARK_GEN(unmarked);
  /* Some of the other algorithms make sense with words, this one
   * makes sense with bytes. Go figure. */
  unsigned char *marks = (unsigned char*)mark_bitmap,
                *allocs = (unsigned char*)allocation_bitmap,
                *lines = line_bytemap;
  for_lines_in_page(l, p) {
    unsigned char new = marks[l], old = allocs[l];
    allocs[l] = (UNMARK_GEN(lines[l]) == unmarked) ? new : old;
  }
  for_lines_in_page(l, p) {
    unsigned char line = UNFRESHEN_GEN(lines[l]);
    lines[l] = (line == unmarked) ? 0 : (line == marked) ? unmarked : line;
  }
  for_lines_in_page(l, p)
    marks[l] = 0;
}

static _Atomic(page_index_t) last_page_processed;
#define for_each_claim(claim, limit)                                    \
  while ((claim = atomic_fetch_add(&last_page_processed, PAGES_CLAIMED_PER_THREAD)) < page_table_pages && \
         (limit = claim + PAGES_CLAIMED_PER_THREAD, limit = (limit >= page_table_pages) ? page_table_pages - 1 : limit, 1))

static void sweep_lines() {
  /* Free this gen, and work out how much space is used on each small
   * page. */
  os_vm_size_t total_decrement = 0;
  page_index_t claim, limit;
  for_each_claim (claim, limit) {
    for (page_index_t p = claim; p < limit; p++) {
      if (!page_free_p(p) && !page_single_obj_p(p)) {
        if (generation_to_collect == PSEUDO_STATIC_GENERATION) {
          unsigned char *marks = (unsigned char*)mark_bitmap,
                        *allocs = (unsigned char*)allocation_bitmap,
                        *lines = line_bytemap;
          page_bytes_t used = 0;
          for_lines_in_page(l, p) {
            allocs[l] = marks[l];
            lines[l] = IS_MARKED(lines[l]) ? UNMARK_GEN(lines[l]) : 0;
            if (lines[l]) {
              generations[DECODE_GEN(lines[l])].bytes_allocated += LINE_SIZE;
              used += LINE_SIZE;
            }
            marks[l] = 0;
          }
          set_page_bytes_used(p, used);
        } else if (page_table[p].gen != PSEUDO_STATIC_GENERATION) {
          page_bytes_t decrement = count_dead_bytes(p);
          if (page_bytes_used(p) < decrement)
            lose("Decrement of %d on page #%d, with only %d bytes to spare.",
                 decrement, p, page_bytes_used(p));
          total_decrement += decrement;
          set_page_bytes_used(p, page_bytes_used(p) - decrement);
          sweep_small_page(p);
        }
      }
    }
  }
  atomic_fetch_add(&generations[generation_to_collect].bytes_allocated, -total_decrement);
}

static void reset_pinned_pages() {
  uword_t pinned_pages = 0;
  for (page_index_t p = 0; p < page_table_pages; p++)
    if (gc_page_pins[p]) pinned_pages++;
  meters.pinned_pages += pinned_pages;
  memset(gc_page_pins, 0, page_table_pages);
}

static void add_page_to_free_list(page_index_t p, unsigned char type) {
  free_pages_by_type[type].indices[free_pages_by_type[type].count++] = p;
}

uword_t bytes_wasted = 0;
static void __attribute__((noinline)) sweep_pages() {
  /* next_free_page is only maintained for page walking - we
   * reuse partially filled pages, so it's not useful for allocation */
  next_free_page = page_table_pages;
  bytes_wasted = 0;
  for (int i = 0; i < 8; i++) free_pages_by_type[i].count = 0;
  for (page_index_t p = 0; p < page_table_pages; p++) {
    /* Rather than clearing marks for every page, we only clear marks for
     * pages which were live before, as a dead page cannot have any marks
     * that we need to clear. */
    if (!page_free_p(p) &&
        (generation_to_collect == PSEUDO_STATIC_GENERATION ||
         page_table[p].gen < PSEUDO_STATIC_GENERATION)) {
      if (page_single_obj_p(p))
        /* There can only be one mark on a large-object page. */
        mark_bitmap[mark_bitmap_word_index(page_address(p))] = 0;
    }
    if (page_words_used(p) == 0) {
      /* Remove allocation bit for the large object here. */
      if (page_single_obj_p(p))
        allocation_bitmap[mark_bitmap_word_index(page_address(p))] = 0;
      /* Why is reset_page_flags(p) much slower here? It does other stuff
       * for gencgc, sure, but not that much more stuff. */
      set_page_need_to_zero(p, 1);
      set_page_type(page_table[p], FREE_PAGE_FLAG);
      page_table[p].scan_start_offset_ = 0;
      add_page_to_free_list(p, FREE_PAGE_FLAG);
    } else {
      bytes_allocated += page_bytes_used(p);
      if (page_single_obj_p(p) &&
          (page_table[p].gen == generation_to_collect || generation_to_collect == PSEUDO_STATIC_GENERATION))
        generations[page_table[p].gen].bytes_allocated += page_bytes_used(p);
      next_free_page = p + 1;
      if (!page_single_obj_p(p) && page_bytes_used(p) < GENCGC_PAGE_BYTES) {
        bytes_wasted += GENCGC_PAGE_BYTES - page_bytes_used(p);
        add_page_to_free_list(p, page_table[p].type);
      }
    }
  }
}

static void __attribute__((noinline)) sweep() {
  /* Handle weak pointers. */
  local_smash_weak_pointers();
  gc_dispose_private_pages();
  cull_weak_hash_tables(mr_alivep_funs);
  /* Reset values we're about to recompute */
  bytes_allocated = 0;
  /* We recompute bytes allocated from scratch when doing full GC */
  if (generation_to_collect == PSEUDO_STATIC_GENERATION)
    for (generation_index_t g = 0; g <= PSEUDO_STATIC_GENERATION; g++)
      generations[g].bytes_allocated = 0;
  /* Currently I haven't made full GC sweeping parallel, but as you have
   * to trigger that manually, its performance isn't that important. */
  last_page_processed = 0;
  if (generation_to_collect == PSEUDO_STATIC_GENERATION)
    sweep_lines();
  else
    METER(sweep_lines, run_on_thread_pool(sweep_lines));
  METER(sweep_pages, sweep_pages());
}

/* Trace a bump-allocated range, e.g. static space or an arena. */
void mr_trace_bump_range(lispobj* start, lispobj *end) {
  lispobj *where = start;
  while (where < end) {
    source_object = where;
    lispobj obj = compute_lispobj(where);
    trace_object(obj);
    where += listp(obj) ? 2 : headerobj_size(where);
  }
}

extern lispobj lisp_init_function;
static void trace_static_roots() {
  source_object = native_pointer(NIL) - 1;
  trace_other_object((lispobj*)NIL_SYMBOL_SLOTS_START);
  mr_trace_bump_range((lispobj*)STATIC_SPACE_OBJECTS_START,
                      static_space_free_pointer);
#ifdef LISP_FEATURE_PERMGEN
  if (new_space == PSEUDO_STATIC_GENERATION) {
    remember_all_permgen();
  }
  // Remembered objects below the core permgen end, and all objects above it, are roots.
  mr_trace_bump_range((lispobj*)permgen_bounds[1], permgen_space_free_pointer);
  int i, n = permgen_remset_count;
  if (gencgc_verbose)
    printf("remset count: %d, permgen new-obj-range %p..%p (%d words)\n", n,
           (void*)permgen_bounds[1], permgen_space_free_pointer,
           (int)(permgen_space_free_pointer - (lispobj*)(permgen_bounds[1])));
  for (i=0; i<n; ++i) {
    lispobj o = permgen_remset[i];
    source_object = native_pointer(o);
    trace_object(o);
  }
#endif

  // TODO: use an explicit remembered set of modified objects in this range
  if (TEXT_SPACE_START) mr_trace_bump_range((lispobj*)TEXT_SPACE_START, text_space_highwatermark);
#ifdef LISP_FEATURE_SYSTEM_TLABS
  extern void gc_scavenge_arenas();
  gc_scavenge_arenas();
#endif
#define MARK(x) mark(x, &x, SOURCE_NORMAL)
  MARK(lisp_package_vector);
  MARK(lisp_init_function);
  MARK(alloc_profile_data);
#undef MARK
}

/* Entry points to convince the GC of different liveness */

/* Preserve an ambiguous pointer, pinning it.
 * Used for pointers in registers and the stack. */
void mr_preserve_ambiguous(uword_t address) {
  page_index_t p = find_page_index(native_pointer(address));
  if (p > -1) {
    lispobj *obj = find_object(address, DYNAMIC_SPACE_START);
    if (obj) {
      mark(compute_lispobj(obj), NULL, SOURCE_NORMAL);
      gc_page_pins[p] = 0xFF;
    }
  }
}

/* Preserve exact pointers in an array.
 * Used for scanning thread-local storage for roots. */
void mr_preserve_range(lispobj *from, sword_t nwords) {
  source_object = NULL;
  for (sword_t n = 0; n < nwords; n++) {
    mark(from[n], from + n, SOURCE_NORMAL);
  }
}

/* Preserve an exact pointer, without attempting to trace it.
 * Used by weak hash table culling, for the list of culled values.
 * We never have to deal with fixing pointers here, as we don't
 * allocate into pages we intend to evacuate. */
void mr_preserve_leaf(lispobj obj) {
  if (is_lisp_pointer(obj) && in_dynamic_space(obj)) {
    set_mark_bit(obj);
    lispobj *n = native_pointer(obj);
    mark_lines(n);
  }
}

/* Preserve an exact pointer, trace and pin it.
 * Used by pin_exact_root. */
void mr_preserve_object(lispobj obj) {
  page_index_t p = find_page_index(native_pointer(obj));
  if (p != -1) {
    mark(obj, NULL, SOURCE_NORMAL);
    gc_page_pins[p] = 0xFF;
  }
}

/* Scavenging older generations */

#ifdef LISP_FEATURE_LOG_CARD_MARKS
/* Card indices are 32-bit, lispobjs are 64-bit on 64-bit, so this wastes
 * space to reuse the Qblock machinery. */
static struct suballocator current_log_suballocator = SUBALLOCATOR_INITIALIZER("Log A");
static struct suballocator next_log_suballocator = SUBALLOCATOR_INITIALIZER("Log B");
static struct Qblock *_Atomic current_log = NULL, *_Atomic next_log = NULL;

static void print_log() {
  for (struct Qblock *block = current_log; block; block = block->next)
    for (int i = 0; i < block->count; i++) {
      fprintf(stderr, "%d ", (int)block->elements[i]);
    }
  fprintf(stderr, "\n");
}
static void swap_logs() {
  gc_assert(!current_log);
  current_log = next_log;
  next_log = NULL;
  suballoc_release(&current_log_suballocator);
  /* Swap suballocators */
  struct suballocator temp = current_log_suballocator;
  current_log_suballocator = next_log_suballocator;
  next_log_suballocator = temp;
}

#define CARD_LOG(thread) ((struct Qblock*)(thread->card_log))

static void atomic_push(struct Qblock *_Atomic *list, struct Qblock *block) {
  if (block) {
    struct Qblock *prior;
    do {
      prior = *list;
      block->next = prior;
    } while (!atomic_compare_exchange_strong(list, &prior, block));
  }
}

void commit_card_log(struct thread *thread) {
  struct Qblock *log = CARD_LOG(thread);
  atomic_push(&current_log, log);
  thread->card_log = NULL;
}

void dirty_card(int index) {
  if (!gc_active_p) mpk_unlock_card_table();
  gc_card_mark[index] = CARD_MARKED;
  if (!gc_active_p) mpk_lock_card_table();
  struct thread *me = get_sb_vm_thread();
  struct Qblock *log = CARD_LOG(me);
  if (!log || log->count == QBLOCK_CAPACITY) {
    commit_card_log(me);
    struct Qblock *next = suballoc_allocate(&current_log_suballocator);
    me->card_log = log = next;
  }
  log->elements[log->count++] = index;
}

static _Thread_local struct Qblock *next_log_block;
static uword_t *log_array;
static int log_length, *log_boundaries = NULL;

/* Roughly sort the log at least to page granularity, so that we can
 * partition the work, then let each worker thread sort the rest.
 * The important part of quicksort() is that it is fast with duplicates.
 * Another option could be a radix/bucket sort, but I'm not sure how that
 * would distribute the work - card indices appear non-uniformly in the log. */
#define SORT_GRANULARITY (32 * CARDS_PER_PAGE)
#define SORT_KEY(a) ((a) / SORT_GRANULARITY)
#define SWAP(a,i,j) { uword_t temp=a[i];a[i]=a[j];a[j]=temp; }

static void quicksort(uword_t *a, int start, int end) {
  if (end <= start) return;
  uword_t pivot = a[start];
  int lt = start, i = start + 1, gt = end;
  while (i <= gt) {
    if (SORT_KEY(a[i]) < SORT_KEY(pivot)) {
      SWAP(a, i, lt); lt++; i++;
    } else if (SORT_KEY(a[i]) > SORT_KEY(pivot)) {
      SWAP(a, i, gt); gt--;
    } else {
      i++;
    }
  }
  quicksort(a, start, lt - 1);
  quicksort(a, gt + 1, end);
}

static void flatten_log() {
  log_length = 0;
  for (struct Qblock *block = current_log; block; block = block->next)
    log_length += block->count;
  log_array = (uword_t*)os_allocate(log_length * sizeof(uword_t));
  int cursor = 0;
  for (struct Qblock *block = current_log; block; block = block->next)
    for (int i = 0; i < block->count; i++)
      log_array[cursor++] = block->elements[i];

  int total_threads = gc_threads + 1;
  if (!log_boundaries) log_boundaries = (int*)os_allocate((total_threads + 1) * sizeof(int));
  log_boundaries[0] = 0;
  log_boundaries[total_threads] = log_length;
  /* If there's only one GC worker, it's going to sort everything anyway,
   * so there is no need to do a rough sort beforehand. */
  if (total_threads > 1) quicksort(log_array, 0, log_length - 1);
  /* Align each boundary to a page, so that threads will never accidentally
   * overlap their scans due to prefix processing. */
  for (int thread = 1; thread < total_threads; thread++) {
    int n = thread * log_length / total_threads;
    if (n != 0)
      for (; n < log_length; n++)
        if (SORT_KEY(log_array[n - 1]) != SORT_KEY(log_array[n]))
          break;
    log_boundaries[thread] = n;
  }
}

#endif

static void update_card_mark(int card, bool dirty) {
  if (gc_card_mark[card] != STICKY_MARK)
    gc_card_mark[card] = dirty ? CARD_MARKED : CARD_UNMARKED;
#ifdef LISP_FEATURE_LOG_CARD_MARKS
  /* Write dirty cards into next_log, for the next GC to read. */
  if (gc_card_mark[card] != CARD_UNMARKED) {
    if (!next_log_block || next_log_block->count == QBLOCK_CAPACITY) {
      atomic_push(&next_log, next_log_block);
      next_log_block = suballoc_allocate(&next_log_suballocator);
    }
    next_log_block->elements[next_log_block->count++] = card;
  }
#endif
}

/* Check if an object is dirty in some way that tracing wouldn't uncover.
 * This happens specifically with weak vectors and weak values, as we
 * don't actually trace those when "tracing" them. We only record dirtiness
 * without tracing, however, in order to allow weak values to be culled
 * without a (more) major GC, and to update the remembered set for
 * compaction. We still treat large weak vectors as non-weak though -
 * I'm not sure if that's for the better. */
static void watch_deferred(lispobj *where, uword_t start, uword_t end) {
  generation_index_t gen = dirty_generation_source;
  for (uword_t i = start; i < end; i++) {
    if (is_lisp_pointer(where[i])) {
#if ENABLE_COMPACTION
      log_slot(where[i], where + i, where, SOURCE_NORMAL);
#endif
      if (gc_gen_of(where[i], 0) < gen) {
        dirty = 1;
      }
    }
  }
}

#define WORDS_PER_CARD (GENCGC_CARD_BYTES/N_WORD_BYTES)
static void scavenge_root_object(generation_index_t gen, lispobj *where) {
  source_object = where;
  dirty_generation_source = gen;
  trace_object(compute_lispobj(where));
}

static void scavenge_single_object_card(int card, lispobj *limit) {
  lispobj *start = card_index_to_addr(card), *card_end = start + WORDS_PER_CARD;
  lispobj *end = (limit < card_end) ? limit : card_end;
  dirty = 0;
  for (lispobj *p = start; p < end; p++) {
    mark(*p, p, SOURCE_NORMAL);
  }
  update_card_mark(card, dirty);
}

static void scavenge_code_card(page_index_t i, int card) {
  source_object = (lispobj*)page_address(i);
  dirty_generation_source = page_table[i].gen;
  dirty = 0;
  trace_other_object((lispobj*)page_address(i));
  update_card_mark(card, dirty);
}

static bool scavenge_small_card(line_index_t line, int card, line_index_t last_seen) {
  unsigned char *allocations = (unsigned char*)allocation_bitmap;
  unsigned char a = allocations[line], gen = DECODE_GEN(line_bytemap[line]);
  page_index_t page = line / LINES_PER_PAGE;
  bool worked = 0;
  dirty = 0;
  /* Check if there's a new->old word belonging to a
   * SIMPLE-VECTOR overlapping this card. */
  for (int word = 0; word < 2 * (a ? __builtin_ctz(a) : 8); word++)
    if (gc_gen_of(((lispobj*)(line_address(line)))[word], PSEUDO_STATIC_GENERATION) < gen) {
      lispobj *before = find_object((uword_t)line_address(line), (uword_t)page_address(page));
      /* Check if we already scavenged this vector before, too. */
      if (before
          && widetag_of(before) == SIMPLE_VECTOR_WIDETAG
          && address_line(before) > last_seen)
        scavenge_root_object(gen, before);
      /* Always dirty this card regardless of avoiding a
       * re-scan or not, as we already found an interesting
       * pointer. */
      dirty = 1, worked = 1;
      break;
    }
  while (a) {
    worked = 1;
    int bit = __builtin_ctzl(a);
    scavenge_root_object(gen, (lispobj*)(line_address(line)) + 2 * bit);
    a &= ~(1 << bit);
  }
  update_card_mark(card, dirty);
  return worked;
}

#ifdef LISP_FEATURE_LOG_CARD_MARKS
/* Scan the log. */
void verify_log() {
  static unsigned char *card_visited_bytemap;
  if (!card_visited_bytemap) {
    card_visited_bytemap = calloc(line_count, 1);
    if (!card_visited_bytemap) lose("Couldn't allocate card_visited_bytemap");
  }

  /* Mark all cards in the log */
  for (struct Qblock *block = current_log; block; block = block->next)
    for (int i = 0; i < block->count; i++) {
      card_visited_bytemap[block->elements[i]] = 1;
    }
  /* Check all dirty cards are either gen0 or logged */
  int failures = 0;
  for (line_index_t line = 0; line < line_count; line++) {
    int card = addr_to_card_index(line_address(line));
    unsigned char gen = gc_gen_of((lispobj)line_address(line), 255);
    if (card_dirtyp(card) && gen > 0 && gen != 255 && !card_visited_bytemap[card]) {
      if (failures < 20)
        fprintf(stderr, "Card #%d/page #%d/%p is dirty but not logged: gen=%d line=%x card=%x\n",
                card, find_page_index(line_address(line)), line_address(line),
                gen, line_bytemap[line], gc_card_mark[card]);
      failures++;
    }
  }
  if (failures)
    lose("Card log is inconsistent with %d errors", failures);
  /* Clear marks */
  for (struct Qblock *block = current_log; block; block = block->next)
    for (int i = 0; i < block->count; i++)
      card_visited_bytemap[block->elements[i]] = 0;
}

static void scavenge_root_gens_worker() {
  int cards_seen = 0;
  next_log_block = NULL;

  int start = log_boundaries[gc_thread_id], limit = log_boundaries[gc_thread_id + 1];
  /* Hints for scavenge_small_card to cut prefix scanning short. */
  int last_card = -1;
  page_index_t last_interesting_page = -1;
  line_index_t last_interesting_line = -1;
  extern void gc_heapsort_uwords(uword_t*, int);
  gc_heapsort_uwords(log_array + start, limit - start);
  for (int i = start; i < limit; i++) {
#define SKIP_IF(cond) if (cond) { update_card_mark(card, 0); continue; }
    int card = (int)log_array[i];
    lispobj *addr = card_index_to_addr(card);
    page_index_t page = find_page_index(addr);
    if (page != -1 && card != last_card) {
      last_card = card;
      cards_seen++;
      unsigned char page_type = page_table[page].type & PAGE_TYPE_MASK;
      SKIP_IF(page_type == PAGE_TYPE_UNBOXED || !page_words_used(page));
      if (page_single_obj_p(page)) {
        SKIP_IF(page_table[page].gen <= generation_to_collect);
        source_object = (lispobj*)(page_address(page) - page_scan_start_offset(page));
        dirty_generation_source = page_table[page].gen;
        int widetag = widetag_of(source_object);
        switch (widetag) {
        case SIMPLE_VECTOR_WIDETAG:
        case WEAK_POINTER_WIDETAG: {
          lispobj *limit = (lispobj*)page_address(page) + page_words_used(page);
          scavenge_single_object_card(card, limit);
          break;
        }
        case CODE_HEADER_WIDETAG:
          if (page_starts_contiguous_block_p(page))
            scavenge_code_card(page, card);
          break;
        default:
          update_card_mark(card, 0);
        }
      } else {
        line_index_t line = address_line(addr);
        SKIP_IF(DECODE_GEN(line_bytemap[line]) <= generation_to_collect);
        /* Use the last_interesting_line hint if it pertains to the right page. */
        line_index_t last_seen = last_interesting_page == page ? last_interesting_line : -1;
        if (scavenge_small_card(line, card, last_seen)) {
          last_interesting_page = page;
          last_interesting_line = line;
        }
      }
    }
#undef SKIP_IF
  }
  atomic_push(&next_log, next_log_block);
  atomic_fetch_add(&meters.cards, cards_seen);
}

static void mr_scavenge_root_gens() {
  struct thread *th;
  for_each_thread(th) commit_card_log(th);
  METER(sort, flatten_log());
  last_page_processed = 0;
  run_on_thread_pool(scavenge_root_gens_worker);
  current_log = NULL;
  os_deallocate((char*)log_array, log_length * sizeof(uword_t));
  swap_logs();
}
#else
/* Scan the card table. */
static void CPU_SPLIT scavenge_root_gens_worker(void) {
  page_index_t claim, limit;
  uword_t cards_checked = 0, prefixes_checked = 0;
  for_each_claim (claim, limit) {
    for (page_index_t i = claim; i < limit; i++) {
      unsigned char page_type = page_table[i].type & PAGE_TYPE_MASK;
      if (page_type == PAGE_TYPE_UNBOXED || !page_words_used(i)) continue;
      if (page_single_obj_p(i)) {
        if (page_table[i].gen > generation_to_collect) {
          int widetag = widetag_of((lispobj*)(page_address(i) - page_scan_start_offset(i)));
          switch (widetag) {
          case SIMPLE_VECTOR_WIDETAG:
          case WEAK_POINTER_WIDETAG: {
            /* Scavenge a page of a vector. */
            source_object = (lispobj*)(page_address(i) - page_scan_start_offset(i));
            dirty_generation_source = page_table[i].gen;
            /* page_address(i) + page_words_used(i) only demarcates
             * the end of a (sole) object on the page with this heap
             * layout when the object is large. */
            lispobj *start = (lispobj*)page_address(i),
                    *limit = (lispobj*)page_address(i) + page_words_used(i);
            for (int j = 0, card = addr_to_card_index(start);
                 j < CARDS_PER_PAGE;
                 j++, card++) {
              if (card_dirtyp(card)) {
                cards_checked++;
                scavenge_single_object_card(card, limit);
              }
            }
            break;
          }
          case CODE_HEADER_WIDETAG: {
            int card = addr_to_card_index(page_address(i));
            if (page_starts_contiguous_block_p(i) && card_dirtyp(card)) {
              cards_checked++;
              scavenge_code_card(i, card);
            }
            break;
          }
          default:
            /* How odd. Just remove the card marks. */
            for (int j = 0, card = page_to_card_index(i); j < CARDS_PER_PAGE; j++, card++)
              gc_card_mark[card] = CARD_UNMARKED;
          }
        }
      } else {
        /* Scavenge every object in every card and try to re-protect. */
        lispobj *start = (lispobj*)page_address(i);
        int first_card = page_to_card_index(i);
        line_index_t first_line = address_line(start);
        /* As cards are as large as lines, we can blast through
         * and make a bitmap of interesting objects to scavenge. */
        unsigned char mask[CARDS_PER_PAGE];
        unsigned char *cards = gc_card_mark + first_card,
                      *lines = line_bytemap + first_line;
        int gen = generation_to_collect;
        for (unsigned int n = 0; n < CARDS_PER_PAGE; n++) {
          unsigned char line = lines[n], mark = cards[n];
          mask[n] = (DECODE_GEN(line) > gen && mark != CARD_UNMARKED) ? 0xFF : 0x00;
        }
        /* Reset mark, which scavenging might re-instate. */
        for (unsigned int n = 0; n < CARDS_PER_PAGE; n++)
          cards[n] = (cards[n] == STICKY_MARK) ? STICKY_MARK : CARD_UNMARKED;

        line_index_t last_seen = -1;
        for (unsigned int n = 0; n < CARDS_PER_PAGE; n++)
          if (mask[n]) {
            cards_checked++;
            line_index_t this_line = address_line(start) + n;
            bool worked = scavenge_small_card(this_line, first_card + n, last_seen);
            /* Only advance last_seen if we did any work here.
             * If we always advance, we can confuse prefix scanning.
             * Suppose a simple-vector spans cards 0, 1 and 2, and 1
             * and 2 are dirtied. Card 1 has no interesting pointers on it,
             * but sets last_seen = 1. Then we don't search the prefix of 2
             * because address_line(before) == 0 which is less than 1. So
             * we need last_seen to actually reflect the work we did. */
            if (worked) last_seen = this_line;
          }
      }
    }
  }
  dirty_generation_source = 0;
  atomic_fetch_add(&meters.prefix, prefixes_checked);
  atomic_fetch_add(&meters.cards, cards_checked);
}

static void __attribute__((noinline)) mr_scavenge_root_gens() {
  last_page_processed = 0;
  run_on_thread_pool(scavenge_root_gens_worker);
}
#endif

static void CPU_SPLIT raise_survivors(void) {
  unsigned char *bytemap = line_bytemap;
  generation_index_t gen = generation_to_collect;
  unsigned char expected = ENCODE_GEN((unsigned char)gen);
  unsigned char target = ENCODE_GEN((unsigned char)gen + 1);
  for (page_index_t p = 0; p < next_free_page; p++)
    if (!page_free_p(p)) {
#ifdef LISP_FEATURE_LOG_CARD_MARKS
      /* Undo pre-dirtying, so that the next major GC doesn't waste time
       * scavenging newly promoted cards. */
      if (gen == 0) {
        int n = 0, card = page_to_card_index(p);
        if (page_single_obj_p(p)) {
          if (page_table[p].gen == 0) {
            for (; n < CARDS_PER_PAGE; card++, n++)
              gc_card_mark[card] = CARD_UNMARKED;
          }
        } else {
          void *start = page_address(p);
          line_index_t line = address_line(start);
          /* GCC can't auto-vectorise the straight-forward way to write
           * this loop. */
          unsigned char *cards = gc_card_mark + card, *lines = bytemap + line;
          for (; n < CARDS_PER_PAGE; n++) {
            cards[n] = (lines[n] == expected) ? CARD_UNMARKED : cards[n];
          }
        }
      }
#endif
      for_lines_in_page(l, p)
        bytemap[l] = (bytemap[l] == expected) ? target : bytemap[l];
    }
  for (page_index_t p = 0; p < next_free_page; p++)
    if (page_table[p].gen == gen && page_single_obj_p(p))
      page_table[p].gen++;
  generations[gen + 1].bytes_allocated += generations[gen].bytes_allocated;
  generations[gen].bytes_allocated = 0;
}

/* Main entrypoints into GC */

void mrgc_init() {
  allocate_bitmaps();
  thread_pool_init();
  compactor_init();
}

void mr_pre_gc(generation_index_t generation) {
  collection++;
#ifdef LOG_COLLECTIONS
  fprintf(stderr, "\n[GC #%4d gen %d %5luM / %5luM ", collection, generation,
          generations[generation].bytes_allocated >> 20,
          bytes_allocated >> 20);
#endif
  generation_to_collect = generation;
  reset_statistics();
#if ENABLE_COMPACTION
  if (generation != PSEUDO_STATIC_GENERATION)
    METER(consider, consider_compaction(generation_to_collect));
#endif
}

void mr_collect_garbage(bool raise) {
  extern void reset_alloc_start_pages(bool allow_free_pages);
  if (generation_to_collect != PSEUDO_STATIC_GENERATION) {
    METER(scavenge, mr_scavenge_root_gens());
  }
  trace_static_roots();
  METER(trace, trace_everything());
  METER(sweep, sweep());
#if ENABLE_COMPACTION
  if (compacting) {
    meters.compacts++;
    /* This isn't a lot of work to wake up every thread for. Perhaps
     * we could snoop TLS of each GC thread instead. */
    run_on_thread_pool(commit_thread_local_remset);
    reset_alloc_start_pages(true);
    METER(compact, run_compaction(&meters.copy, &meters.fix, &meters.compact_resweep));
  }
#endif
  /* scan_finalizers checks forwarding pointers, so we need to
   * ensure it is called after compaction. */
  scan_finalizers();
  if (raise) {
    METER(raise, raise_survivors());
  }
#ifdef LOG_COLLECTIONS
  fprintf(stderr,
          "-> %5luM / %5luM, %8lu traced, %8lu / %8lu scavenged, page hwm = %8ld%s]\n",
          generations[generation_to_collect].bytes_allocated >> 20,
          bytes_allocated >> 20, traced,
          dirty_root_objects, root_objects_checked,
          next_free_page, raise ? ", raised" : "");
#endif
  if (gencgc_verbose>1) mr_print_meters();
  reset_alloc_start_pages(false);
  reset_pinned_pages();
}

void zero_all_free_ranges() {
  for (page_index_t p = 0; p < page_table_pages; p++) {
    char* addr = page_address(p);
    char* limit = addr + GENCGC_PAGE_BYTES;
    // mark-region frequently leaves empty pages below the high water mark.
    // We avoid operating on free pages. The same function in gencgc iterates only
    // up to next_free_page, I don't know why this can't do likewise.
    if (page_single_obj_p(p) || (page_free_p(p) && p < next_free_page)) {
      // all bytes beyond the bytes_used must be 0
      char* unused = addr + page_bytes_used(p);
      memset(unused, 0, limit-unused);
    } else if (!page_free_p(p)) {
      lispobj* where = (void*)addr;
      while ( (char*)where < limit )
        if (allocation_bit_marked(where)) // skip all bits of this object
          where += object_size(where);
        else {
          where[0] = where[1] = 0;
          where += 2;
        }
      set_page_bytes_used(p, GENCGC_PAGE_BYTES);
      for_lines_in_page (l, p)
        if (!line_bytemap[l]) memset(line_address(l), 0, LINE_SIZE);
    }
  }
}

void prepare_lines_for_final_gc() {
  for (line_index_t l = 0; l < line_count; l++) {
    unsigned char line = line_bytemap[l];
    /* Line might still be fresh here. */
    line_bytemap[l] = line == 0 ? 0 : COPY_MARK(line, ENCODE_GEN(0));
  }

  // Now that we've unleashed pseudo-static pages onto GC, recompute
  // everything (as we would during a full GC).
  for (generation_index_t g = 0; g <= PSEUDO_STATIC_GENERATION; g++)
    generations[g].bytes_allocated = 0;
  for (page_index_t p = 0; p < page_table_pages; p++) {
    if (page_single_obj_p(p)) {
      generations[0].bytes_allocated += page_bytes_used(p);
    } else {
      page_bytes_t new_size = 0;
      set_page_bytes_used(p, 0);
      for_lines_in_page(l, p) {
        if (line_bytemap[l]) {
          new_size += LINE_SIZE;
        }
      }
      generations[0].bytes_allocated += new_size;
      set_page_bytes_used(p, new_size);
    }
  }
}

/* Useful hacky stuff */

void find_references_to(lispobj something) {
  for (uword_t i = 0; i < (dynamic_space_size / N_WORD_BYTES); i++) {
    lispobj *p = (lispobj*)(DYNAMIC_SPACE_START + i * N_WORD_BYTES);
    // mark-region.c:1324:9: warning: taking the absolute value of unsigned
    // type 'unsigned long' has no effect [-Wabsolute-value]
    if (/*labs*/(*p - something) < 16)
      printf("%p: %lx\n", p, *p);
  }
}

/* Print out some of the page table.
 * Colour indicates page type, letter type and flags,
 * numbers generation and occupancy. */
void draw_page_table(int from, int to) {
  fprintf(stderr, "\n       ");
  for (int i = 0; i < 50; i++)
    fprintf(stderr, "%3d ", i);
  for (int i = from; i < to; i++) {
    if (i % 50 == 0) fprintf(stderr, "\n%6d ", i);
    fprintf(stderr,
            "\033[%c;9%cm%c%c%c\033[0m ",
            (page_table[i].type & SINGLE_OBJECT_FLAG) ? '1' : '0',
            '0' + (page_table[i].type & 7),
            64 + page_table[i].type,
            '0' + page_table[i].gen,
            '0' + (unsigned char)(10.0 * (double)page_bytes_used(i) / (double)GENCGC_PAGE_BYTES));
  }
}

/* Save a PBM image of the line bytemap.
 * Red channel stores page type, green channel stores flags and mark,
 * blue channel stores generation. */
int drawing_count = 0;
#define BYTEMAP_WIDTH 4096
void draw_line_bytemap() {
  char name[30];
  snprintf(name, 30, "/tmp/bytemap%d.pbm", drawing_count++);
  FILE *f = fopen(name, "w");
  fprintf(f, "P3 %d %ld 8\n", BYTEMAP_WIDTH, line_count / BYTEMAP_WIDTH);
  for (line_index_t i = 0; i < line_count; i++) {
    page_index_t p = find_page_index(line_address(i));
    int m = line_bytemap[i] > 0, r = page_table[p].type & 7,
        g = (page_table[p].type >> 3) * 2 | (IS_MARKED(line_bytemap[i]) * 4),
        b = gc_gen_of((lispobj)line_address(i), 0);
    fprintf(f, "%d %d %d ", r * m, g * m, b * m);
  }
  fclose(f);
}

void count_line_values(char *why) {
  fprintf(stderr, "\033[1m%s:\033[0m\n", why);
  int counts[256] = { 0 };
  for (line_index_t i = 0; i < line_count; i++)
    counts[line_bytemap[i]]++;
  for (int n = 0; n < 256; n++)
    if (counts[n])
      fprintf(stderr, "%x: %d\n", n, counts[n]);
}

/* Check that page occupancy makes sense. (Put this in verify?) */
void check_weird_pages() {
  for (page_index_t p = 0; p < page_table_pages; p++)
    if (page_words_used(p) > GENCGC_PAGE_WORDS)
      fprintf(stderr, "Page #%d has %d words used\n", p, page_words_used(p));
  bool fail = 0;
  for (page_index_t p = 0; p < page_table_pages; p++)
    if (!page_single_obj_p(p)) {
      page_bytes_t size = 0;
      for_lines_in_page(l, p) {
        if (line_bytemap[l]) {
          size += LINE_SIZE;
        }
      }
      if (size != page_bytes_used(p)) {
        fail = 1;
        fprintf(stderr, "Page #%d (%x %d) has %d bytes, not %d\n",
                p, page_table[p].type, page_table[p].gen, size, page_bytes_used(p));
      }
      if (fail) lose("Errors checking line/page usage, as above.");
    }
}
