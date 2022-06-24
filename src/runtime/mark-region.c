#include <stdbool.h>
#include <string.h>
#include <unistd.h>

#include "align.h"
#include "os.h"
#include "gc.h"
#include "runtime.h"
#include "mark-region.h"
#include "validate.h"
#include "gc-assert.h"
#include "gc-internal.h"
#include "gencgc-internal.h"
#include "gencgc-private.h"
#include "core.h"
#include "interr.h"
#include "globals.h"
#include "lispobj.h"
#include "queue.h"

#include "genesis/cons.h" 
#include "genesis/gc-tables.h"
#include "genesis/hash-table.h"
#include "genesis/closure.h"
#include "gc-private.h"

/* The idea of the mark-region collector is to avoid copying where
 * possible, and instead reclaim as much memory in-place as possible.
 * The result is that we save time on copying objects (especially leaf
 * objects), and parallelisation is easier.
 * The design is loosely inspired by the Immix collector designed by
 * Blackburn and McKinley, mostly in being able to reclaim both smaller
 * lines and larger pages. But we plan to have a separate compaction
 * phase, rather than copying and marking in one pass, to make parallel
 * compacting easier. */

/* Initialisation */
uword_t *allocation_bitmap, *mark_bitmap;
char *line_bytemap;
line_index_t line_count;

static void allocate_bitmap(uword_t **bitmap, int divisor,
                            const char *description) {
  *bitmap = calloc(dynamic_space_size / divisor, 1);
  if (!*bitmap)
    lose("Failed to allocate %s (of %lu bytes)", description,
         dynamic_space_size / divisor);
}

void mrgc_init() {
  int bytes_per_heap_byte = 8 /* bits/byte */ << N_LOWTAG_BITS;
  allocate_bitmap(&allocation_bitmap, bytes_per_heap_byte,
                  "allocation bitmap");
  allocate_bitmap(&mark_bitmap, bytes_per_heap_byte,
                  "mark bitmap");
  allocate_bitmap((uword_t**)&line_bytemap, LINE_SIZE, "line bytemap");
  line_count = dynamic_space_size / LINE_SIZE;
}

/* Line arithmetic */

static inline char *line_address(line_index_t line) {
  return (char*)(DYNAMIC_SPACE_START + (line * LINE_SIZE));
}

static inline line_index_t address_line(void *address) {
  return ((uintptr_t)address - DYNAMIC_SPACE_START) / LINE_SIZE;
}

uword_t lines_used() {
  uword_t count = 0;
  for (line_index_t line = 0; line < line_count; line++)
    if (line_bytemap[line]) count++;
  return count;
}

/* Allocation slow-path */

/* Allocation of small objects is done by finding contiguous lines
 * that can fit the object to allocate. Small objects can span lines
 * but cannot pages, so we examine lines in each page separately. */
#define DEF_FINDER(name, type, test, fail) \
  static type name(type start, type end) { \
    for (type where = start; where < end; where++) \
      if (test) return where; \
    return fail; }

DEF_FINDER(find_free_line, line_index_t, !line_bytemap[where], -1);
DEF_FINDER(find_used_line, line_index_t, line_bytemap[where], end);

/* Try to find space to fit a new object in the lines between `start`
 * and `end`. Updates `region` and returns true if we succeed, keeps
 * `region` untouched and returns false if we fail. */
boolean try_allocate_small(sword_t nbytes, struct alloc_region *region,
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
      memset(region->start_addr, 0, addr_diff(region->end_addr, region->start_addr));
      return true;
    }
    if (chunk_end == end) return false;
    where = chunk_end;
  }
}

/* Medium path for allocation, wherein we use another chunk that the
 * thread already claimed. */
boolean try_allocate_small_after_region(sword_t nbytes, struct alloc_region *region) {
  /* We search to the end of this page. */
  line_index_t end = address_line(PTR_ALIGN_UP(region->end_addr, GENCGC_PAGE_BYTES));
  return try_allocate_small(nbytes, region, address_line(region->end_addr), end);
}

/* try_allocate_small_from_pages updates the start pointer to after the
 * claimed page. */
boolean try_allocate_small_from_pages(sword_t nbytes, struct alloc_region *region,
                                      int page_type, generation_index_t gen,
                                      page_index_t *start, page_index_t end) {
  for (page_index_t where = *start; where < end; where++)
    if (page_bytes_used(where) <= GENCGC_PAGE_BYTES - nbytes &&
        (page_free_p(where) || page_extensible_p(where, gen, page_type)) &&
        try_allocate_small(nbytes, region,
                           address_line(page_address(where)),
                           address_line(page_address(where + 1)))) {
      page_table[where].type = page_type | OPEN_REGION_PAGE_FLAG;
      page_table[where].gen = gen;
      set_page_scan_start_offset(where, 0);
      *start = where + 1;
      return true;
    }
  return false;
}

/* Allocation annoying-path */

DEF_FINDER(find_free_page, page_index_t, page_free_p(where), -1);
DEF_FINDER(find_used_page, page_index_t, !page_free_p(where), end);

page_index_t try_allocate_large(sword_t nbytes,
                                int page_type, generation_index_t gen,
                                page_index_t *start, page_index_t end) {
  int pages_needed = ALIGN_UP(nbytes, GENCGC_PAGE_BYTES) / GENCGC_PAGE_BYTES;
  uword_t remainder = nbytes % GENCGC_PAGE_BYTES;
  page_index_t where = *start;
  while (1) {
    page_index_t chunk_start = find_free_page(where, end);
    if (chunk_start == -1) return -1;
    page_index_t chunk_end = find_used_page(chunk_start, end);
    if (chunk_end - chunk_start >= pages_needed) {
      page_index_t last_page = chunk_start + pages_needed - 1;
      for (page_index_t p = chunk_start; p < last_page; p++) {
        page_table[where].type = SINGLE_OBJECT_FLAG | page_type;
        page_table[where].gen = gen;
        set_page_bytes_used(where,
                            (p == last_page && remainder > 0) ? remainder
                            : GENCGC_PAGE_BYTES);
      }
      *start = chunk_start + pages_needed;
      return chunk_start;
    }
    if (chunk_end == end) return -1;
    where = chunk_end;
  }
  return -1;
}

void mr_update_closed_region(struct alloc_region *region) {
  /* alloc_regions never span multiple pages. */
  page_index_t the_page = find_page_index(region->start_addr);
  if (region->free_pointer > region->start_addr) {
    /* Did allocate something. We just say we used the whole page;
     * in terms of pressure on the GC, we basically did, as we can't reuse
     * the page before a collection to provide accurate line marks
     * (currently). This could be alleviated if we track which lines
     * were actually used by the mutator. */
    page_table[the_page].type &= ~(OPEN_REGION_PAGE_FLAG);
    page_bytes_t page_bytes_used = page_bytes_used(the_page);
    set_page_bytes_used(the_page, GENCGC_PAGE_BYTES);
    generation_index_t gen = page_table[the_page].gen;
    /* Report that we used the rest of the page. */
    os_vm_size_t just_used = GENCGC_PAGE_BYTES - page_bytes_used;
    generations[gen].bytes_allocated += just_used;
    bytes_allocated += just_used;
  } else {
    /* Didn't actually allocate anything. */
    reset_page_flags(the_page);
  }
}

/* Core file I/O */

void bitmap_sizes(core_entry_elt_t n_ptes, sword_t *where) {
  sword_t bytes_of_heap = n_ptes * GENCGC_PAGE_BYTES;
  where[0] = bytes_of_heap / (8 << N_LOWTAG_BITS); /* allocation bitmap size */
  where[1] = bytes_of_heap / LINE_SIZE;         /* line bytemap size */
}

void load_corefile_bitmaps(int fd, core_entry_elt_t n_ptes) {
  sword_t sizes[2];
  bitmap_sizes(n_ptes, sizes);
  sword_t allocation_bitmap_size = sizes[0], line_bytemap_size = sizes[1];
  if (read(fd, allocation_bitmap, allocation_bitmap_size) != allocation_bitmap_size)
    lose("failed to read allocation bitmap from core");
  if (read(fd, line_bytemap, line_bytemap_size) != line_bytemap_size)
    lose("failed to read line bytemap from core");
}

/* Marking */

static generation_index_t generation_being_collected;
static bool object_marked_p(lispobj object) {
  uword_t index = (uword_t)(object >> N_LOWTAG_BITS);
  uword_t bit_index = index % N_WORD_BITS, word_index = index / N_WORD_BITS;
  return mark_bitmap[word_index] & (1 << bit_index);
}
static void set_mark_bit(lispobj object) {
  uword_t index = (uword_t)(object >> N_LOWTAG_BITS);
  uword_t bit_index = index % N_WORD_BITS, word_index = index / N_WORD_BITS;
  mark_bitmap[word_index] |= (1 << bit_index);
}
static int pointer_survived_gc_yet(lispobj object) {
  return page_table[find_page_index((void*)object)].gen != generation_being_collected ||
         object_marked_p(object);
}

static struct Qblock *mark_queue;
static struct Qblock *recycle_queue;

static struct Qblock *grab_qblock() {
  struct Qblock *block;
  if (recycle_queue) {
    block = recycle_queue;
    recycle_queue = recycle_queue->next;
  } else {
    block = malloc(QBLOCK_BYTES);
    if (!block) lose("Failed to allocate new mark-queue block");
  }
  block->count = 0;
  return block;
}
static void recycle_qblock(struct Qblock *block) {
  block->next = recycle_queue;
  recycle_queue = block;
}

static void add_words_used(void *where, page_words_t count) {
  page_index_t p = find_page_index(where);
  uword_t byte_count = count * N_WORD_BYTES;
  for (; byte_count >= GENCGC_PAGE_BYTES;
       byte_count -= GENCGC_PAGE_BYTES, p++)
    set_page_bytes_used(p, GENCGC_PAGE_BYTES);
  set_page_bytes_used(p, page_bytes_used(p) + byte_count);
}

static void mark_cons_line(struct cons *c) {
  /* CONS cells never span lines, because they are aligned on
   * cons pages. */
  line_bytemap[address_line(c)] = 1;
  add_words_used(c, 2);
}
static void mark_lines(lispobj *p) {
  /* TODO: Don't really need to mark lines if the object is large, as
   * we don't try to reuse single object pages. */
  page_words_t word_count = OBJECT_SIZE(*p, p);
  line_index_t first = address_line(p), last = address_line(p + word_count - 1);
  for (line_index_t line = first; line <= last; line++) line_bytemap[line] = 1;
  add_words_used(p, word_count);
}

static void mark1(lispobj object) {
  if (!pointer_survived_gc_yet(object)) {
    set_mark_bit(object);
    if (mark_queue == NULL || mark_queue->count == QBLOCK_CAPACITY) {
      /* How would this look if we do parallel marking? At the moment I
       * would guess that we link blocks up to mark_queue only when they
       * are "published" to the global queue, not upon creating them. */
      struct Qblock *n = grab_qblock();
      n->next = mark_queue;
      mark_queue = n;
    }
    mark_queue->elements[mark_queue->count++] = object;
  }
}

static void mark(lispobj object) {
  if (is_lisp_pointer(object)) mark1((lispobj)native_pointer(object));
}

static bool interesting_pointer_p(lispobj object) {
  page_index_t p = find_page_index(native_pointer(object));
  return p >= 0 && page_table[p].gen == generation_being_collected;
}

#define ACTION mark
#define TRACE_NAME trace_other_object
/* I don't see why this table should be an .inc thingy. It isn't
 * paramaterized like we have done for trace-object.inc, so there
 * isn't really a reason to have multiple copies of the functions and
 * table laying around. */
#define HT_ENTRY_LIVENESS_FUN_ARRAY_NAME mr_alivep_funs
#include "trace-object.inc"

static void trace_object(lispobj object) {
  if (listp(object)) {
    struct cons *c = CONS(object);
    mark_cons_line(c);
    mark(c->car); mark(c->cdr);
  } else {
    lispobj *p = native_pointer(object);
    mark_lines(p);
    trace_other_object(p);
  }
}

static lispobj dequeue() {
  gc_assert(mark_queue != NULL);
  gc_assert(mark_queue->count);
  lispobj v = mark_queue->elements[mark_queue->count--];
  if (mark_queue->count == 0) mark_queue = mark_queue->next;
  return v;
}

static void trace_everything() {
  while (mark_queue != NULL ||
         (test_weak_triggers(pointer_survived_gc_yet, mark) && mark_queue != NULL)) {
    trace_object(dequeue());
  }
}

/* Conservative pointer scanning */
static lispobj last_address_in(uword_t bitword, uword_t word_index) {
  /* TODO: Make this portable? MSVC uses _BitScanForward64 and
   * we should probably have a portable fallback too. */
  return DYNAMIC_SPACE_START + ((word_index * N_WORD_BITS + __builtin_ctz(bitword)) << N_LOWTAG_BITS);
}

static lispobj find_object(uword_t address) {
  page_index_t p = find_page_index((void*)address);
  if (page_table[p].type == PAGE_TYPE_CONS) {
    /* CONS cells are always aligned, and the mutator is allowed to be lazy
     * w.r.t putting down allocation bits, so just use alignment. */
    return ALIGN_DOWN(address, 1 << N_LOWTAG_BITS) + LIST_POINTER_LOWTAG;
  } else {
    /* Go scanning for the object. */
    uword_t first_bit_index = (address - DYNAMIC_SPACE_START) >> N_LOWTAG_BITS;
    uword_t first_word_index = first_bit_index / N_WORD_BITS;
    /* Something a bit odd here: as we use mark1 on the result
     * of find_object, we just have to produce something that isn't
     * a list pointer; even though we're really producing fixnums here.
     * The rest of marking doesn't mind what the lowtag is, as it
     * operates on native pointers, after deciding the pointer isn't a
     * list pointer. */
    if (mark_bitmap[first_word_index]) {
      /* Return the last location not after the address provided. */
      /* Supposing first_bit_index = 5, we compute
       * all_not_after = (1 << 6) - 1 = ...000111111
       * i.e. all bits not above bit #5 set. */
      uword_t all_not_after = (1 << (first_bit_index % N_WORD_BITS + 1)) - 1;
      return last_address_in(mark_bitmap[first_word_index] & all_not_after, first_word_index);
    }
    for (uword_t i = first_word_index - 1; i > 0; i--)
      if (mark_bitmap[i])
        /* Return the last location. */
        return last_address_in(mark_bitmap[i], i);
    lose("find_object fell through on %ld?", address);
  }
}

void mr_preserve_pointer(uword_t address) {
  mark1(find_object(address));
}
