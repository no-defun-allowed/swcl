#include <string.h>
#include <unistd.h>
#include <stdio.h>

#include "align.h"
#include "os.h"
#include "gc.h"
#include "runtime.h"
#include "validate.h"
#include "gc-assert.h"
#include "gc-internal.h"
#include "gencgc-internal.h"
#include "gencgc-private.h"
#include "mark-region.h"
#include "core.h"
#include "interr.h"
#include "globals.h"
#include "lispobj.h"
#include "queue.h"
#include "walk-heap.h"

#include "genesis/cons.h"
#include "genesis/gc-tables.h"
#include "genesis/hash-table.h"
#include "genesis/closure.h"
#include "gc-private.h"

#define WORDS_PER_CARD (GENCGC_CARD_BYTES/N_WORD_BYTES)
#define ATOMIC_INC(where, amount) __atomic_add_fetch(&(where), (amount), __ATOMIC_SEQ_CST)

/* The idea of the mark-region collector is to avoid copying where
 * possible, and instead reclaim as much memory in-place as possible.
 * The result is that we save time on copying objects (especially leaf
 * objects), and parallelisation is easier.
 * The design is loosely inspired by the Immix collector designed by
 * Blackburn and McKinley, mostly in being able to reclaim both smaller
 * lines and larger pages. But we plan to have a separate compaction
 * phase, rather than copying and marking in one pass, to make parallel
 * compacting easier, and to enable concurrent marking. */

/* Initialisation */
uword_t *allocation_bitmap, *mark_bitmap;
unsigned char *line_bytemap;             /* 1 if line used, 0 if not used */
line_index_t line_count;
uword_t mark_bitmap_size;

static void allocate_bitmap(uword_t **bitmap, uword_t size,
                            const char *description) {
  *bitmap = calloc(size, 1);
  if (!*bitmap)
    lose("Failed to allocate %s (of %lu bytes)", description, size);
}

void mrgc_init() {
  int bytes_per_heap_byte = 8 /* bits/byte */ << N_LOWTAG_BITS;
  mark_bitmap_size = dynamic_space_size / bytes_per_heap_byte;
  allocate_bitmap(&allocation_bitmap, mark_bitmap_size, "allocation bitmap");
  allocate_bitmap(&mark_bitmap, mark_bitmap_size, "mark bitmap");
  line_count = dynamic_space_size / LINE_SIZE;
  allocate_bitmap((uword_t**)&line_bytemap, line_count, "line bytemap");
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

boolean line_marked(void *pointer) {
  return line_bytemap[address_line(pointer)];
}

/* Allocation slow-path */

/* Lines have generations in small pages, rather than pages. This is
 * necessary in order to allow reclaimed memory to be reused,
 * without having to compact old pages. */
#define MARK_GEN(l) ((l) | 16)
#define UNMARK_GEN(l) ((l) & 15)
#define ENCODE_GEN(g) ((g) + 1)
#define DECODE_GEN(l) ((l) - 1)

generation_index_t gc_gen_of(lispobj obj, int defaultval) {
  page_index_t p = find_page_index((void*)obj);
  if (p < 0) return defaultval;
  if (page_single_obj_p(p))
    return page_table[p].gen;
  char c = UNMARK_GEN(line_bytemap[address_line((void*)obj)]);
  if (c == 0)
    return defaultval;
  return DECODE_GEN(c);
}

/* Allocation of small objects is done by finding contiguous lines
 * that can fit the object to allocate. Small objects can span lines
 * but cannot pages, so we examine lines in each page separately. */
#define DEF_FINDER(name, type, test, fail)         \
  static type name(type start, type end) {         \
    for (type where = start; where < end; where++) \
      if (test) return where;                      \
    return fail; }

DEF_FINDER(find_free_line, line_index_t, !line_bytemap[where], -1);
DEF_FINDER(find_used_line, line_index_t, line_bytemap[where], end);

/* Try to find space to fit a new object in the lines between `start`
 * and `end`. Updates `region` and returns true if we succeed, keeps
 * `region` untouched and returns false if we fail. */
boolean try_allocate_small(sword_t nbytes, struct alloc_region *region,
                           page_index_t page,
                           line_index_t start, line_index_t end,
                           generation_index_t gen) {
  sword_t nlines = ALIGN_UP(nbytes, LINE_SIZE) / LINE_SIZE;
  line_index_t where = start;
  while (1) {
    line_index_t chunk_start = find_free_line(where, end);
    if (chunk_start == -1) return 0;
    line_index_t chunk_end = find_used_line(chunk_start, end);
    if (chunk_end - chunk_start >= nlines) {
      region->start_addr = line_address(chunk_start);
      region->free_pointer = line_address(chunk_start) + nbytes;
      region->end_addr = line_address(chunk_end);
      /* TODO: We currently always zero memory, even when it's not
       * really necessary per the page table. We'd need to decide what
       * "page needs zeroing" means when we are allocating parts of
       * pages. Always zeroing is perhaps not that bad, as we have to
       * zero after reusing memory, which is the stable state of the
       * system. */
      os_vm_size_t claimed = addr_diff(region->end_addr, region->start_addr);
      memset(region->start_addr, 0, claimed);
      memset(line_bytemap + chunk_start, ENCODE_GEN(gen), chunk_end - chunk_start);
      ATOMIC_INC(bytes_allocated, claimed);
      ATOMIC_INC(generations[gen].bytes_allocated, claimed);
      set_page_bytes_used(page, page_bytes_used(page) + claimed);
      return 1;
    }
    if (chunk_end == end) return 0;
    where = chunk_end;
  }
}

/* Medium path for allocation, wherein we use another chunk that the
 * thread already claimed. */
boolean try_allocate_small_after_region(sword_t nbytes, struct alloc_region *region,
                                        generation_index_t gen) {
  /* Can't do this if we have no page. */
  if (!region->start_addr) return 0;
  page_index_t index = find_page_index(region->start_addr);
  gc_assert(!page_free_p(index));
  /* We search to the end of this page. */
  line_index_t end = address_line(PTR_ALIGN_UP(region->end_addr, GENCGC_PAGE_BYTES));
  return try_allocate_small(nbytes, region, index, address_line(region->end_addr), end, gen);
}

/* We try not to allocate small objects from free pages, to reduce
 * fragmentation. Something like "wilderness preservation". */
boolean allow_free_pages[16] = {0};

/* try_allocate_small_from_pages updates the start pointer to after the
 * claimed page. */
boolean try_allocate_small_from_pages(sword_t nbytes, struct alloc_region *region,
                                      int page_type, generation_index_t gen,
                                      page_index_t *start, page_index_t end) {
  gc_assert(gen != SCRATCH_GENERATION);
  // printf("try_allocate_small_f_p(%lu, %d, %d, %ld, %ld)\n", nbytes, page_type, gen, *start, end);
 again:
  for (page_index_t where = *start; where < end; where++) {
    if (page_bytes_used(where) <= GENCGC_PAGE_BYTES - nbytes &&
        ((allow_free_pages[page_type & PAGE_TYPE_MASK] && page_free_p(where)) ||
         (page_table[where].type == page_type &&
          page_table[where].gen != PSEUDO_STATIC_GENERATION)) &&
        try_allocate_small(nbytes, region, where,
                           address_line(page_address(where)),
                           address_line(page_address(where + 1)),
                           gen)) {
      page_table[where].type = page_type | OPEN_REGION_PAGE_FLAG;
      page_table[where].gen = 0;
      set_page_scan_start_offset(where, 0);
      *start = where + 1;
      // printf(" => %ld\n", where);
      return 1;
    }
  }
  if (!allow_free_pages[page_type & PAGE_TYPE_MASK]) {
    allow_free_pages[page_type & PAGE_TYPE_MASK] = 1;
    *start = 0;
    goto again;
  }
  return 0;
}

/* Allocation annoying-path */

DEF_FINDER(find_free_page, page_index_t, page_free_p(where), -1);
DEF_FINDER(find_used_page, page_index_t, !page_free_p(where), end);

page_index_t try_allocate_large(sword_t nbytes,
                                int page_type, generation_index_t gen,
                                page_index_t *start, page_index_t end) {
  gc_assert(gen != SCRATCH_GENERATION);
  // printf("try_allocate_large(%lu, %d, %d, %ld, %ld)\n", nbytes, page_type, gen, *start, end);
  int pages_needed = ALIGN_UP(nbytes, GENCGC_PAGE_BYTES) / GENCGC_PAGE_BYTES;
  uword_t remainder = nbytes % GENCGC_PAGE_BYTES;
  page_index_t where = *start;
  while (1) {
    page_index_t chunk_start = find_free_page(where, end);
    if (chunk_start == -1) return -1;
    page_index_t chunk_end = find_used_page(chunk_start, end);
    if (chunk_end - chunk_start >= pages_needed) {
      page_index_t last_page = chunk_start + pages_needed - 1;
      for (page_index_t p = chunk_start; p <= last_page; p++) {
        page_table[p].type = SINGLE_OBJECT_FLAG | page_type;
        page_table[p].gen = gen;
        set_page_bytes_used(p,
                            (p == last_page && remainder > 0) ? remainder
                            : GENCGC_PAGE_BYTES);
        set_page_scan_start_offset(p,
                                   GENCGC_PAGE_BYTES * (p - chunk_start));
      }
      *start = chunk_start + pages_needed;
      memset(page_address(chunk_start), 0, pages_needed * GENCGC_PAGE_BYTES);
      // printf(" => %ld\n", chunk_start);
      ATOMIC_INC(bytes_allocated, nbytes);
      ATOMIC_INC(generations[gen].bytes_allocated, nbytes);
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
  if (!(page_table[the_page].type & OPEN_REGION_PAGE_FLAG))
    lose("Page %lu wasn't open", the_page);
  //printf("closing %lu\n", the_page);
  page_table[the_page].type &= ~(OPEN_REGION_PAGE_FLAG);
  gc_set_region_empty(region);
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
  lseek(fd, ALIGN_UP(lseek(fd, 0, SEEK_CUR), N_WORD_BYTES), SEEK_SET);
  printf("loading allocations for %lu pages from %016lx\n", n_ptes, lseek(fd, 0, SEEK_CUR));
  sword_t allocation_bitmap_size = sizes[0];
  if (read(fd, allocation_bitmap, allocation_bitmap_size) != allocation_bitmap_size)
    lose("failed to read allocation bitmap from core");
  printf("now at %016lx\n", lseek(fd, 0, SEEK_CUR));
  for (int i = 0; i < 10; i++)
    printf("%016lx\n", allocation_bitmap[i]);

  for (page_index_t p = 0; p < page_table_pages; p++)
    if (page_table[p].gen == PSEUDO_STATIC_GENERATION)
      memset(line_bytemap + address_line(page_address(p)),
             ENCODE_GEN(PSEUDO_STATIC_GENERATION),
             GENCGC_PAGE_BYTES / LINE_SIZE);
}

/* Marking */

/* Old generation for finding old->young pointers */
static generation_index_t dirty_generation_source = 0;
static boolean dirty = 0;
static generation_index_t generation_to_collect = 0;

#define ANY(x) ((x) != 0)
static boolean object_marked_p(lispobj object) {
  uword_t index = (uword_t)((object - DYNAMIC_SPACE_START) >> N_LOWTAG_BITS);
  uword_t bit_index = index % N_WORD_BITS, word_index = index / N_WORD_BITS;
  return ANY(mark_bitmap[word_index] & ((uword_t)(1) << bit_index));
}
static void set_mark_bit(lispobj object) {
  uword_t index = (uword_t)((object - DYNAMIC_SPACE_START) >> N_LOWTAG_BITS);
  uword_t bit_index = index % N_WORD_BITS, word_index = index / N_WORD_BITS;
  mark_bitmap[word_index] |= ((uword_t)(1) << bit_index);
}

static boolean in_dynamic_space(lispobj object) {
  return find_page_index((void*)object) != -1;
}

static boolean pointer_survived_gc_yet(lispobj object) {
  return !in_dynamic_space(object) || object_marked_p(object) || gc_gen_of(object, 0) > generation_to_collect;
}

static struct Qblock *grey_list;
static struct Qblock *recycle_list;

/* The "input packet" and "output packet" from "A Parallel, Incremental
 * and Concurrent GC for Servers". */
static struct Qblock *input_block;
static struct Qblock *output_block;

static struct Qblock *grab_qblock() {
  struct Qblock *block;
  if (recycle_list) {
    block = recycle_list;
    recycle_list = recycle_list->next;
  } else {
    block = (struct Qblock*)os_allocate(QBLOCK_BYTES);
    if (!block) lose("Failed to allocate new mark-queue block");
  }
  block->count = 0;
  return block;
}
static void recycle_qblock(struct Qblock *block) {
  block->next = recycle_list;
  recycle_list = block;
}
static void free_mark_list() {
  gc_assert(grey_list == NULL);
  while (recycle_list) {
    struct Qblock *old = recycle_list;
    recycle_list = recycle_list->next;
    os_deallocate((char*)old, QBLOCK_BYTES);
  }
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
  line_bytemap[address_line(c)] = MARK_GEN(ENCODE_GEN(generation_to_collect));
  add_words_used(c, 2);
}
static void mark_lines(lispobj *p) {
  uword_t word_count = object_size(p);
  if (!page_single_obj_p(find_page_index(p))) {
    line_index_t first = address_line(p), last = address_line(p + word_count - 1);
    for (line_index_t line = first; line <= last; line++)
      line_bytemap[line] = MARK_GEN(ENCODE_GEN(generation_to_collect));
  }
  add_words_used(p, word_count);
}

static void mark(lispobj object) {
  if (is_lisp_pointer(object) && in_dynamic_space(object)) {
    if (page_free_p(find_page_index(native_pointer(object))))
      lose("%lx is on a free page (#%ld)", object, find_page_index(native_pointer(object)));

    lispobj *np = native_pointer(object);
    if (gc_gen_of(object, 0) < dirty_generation_source)
      /* Used to find dirty pages in mr_scavenge_root_gens. */
      dirty = 1;
    if (gc_gen_of(object, 0) != generation_to_collect)
      return;

    /* Fix up embedded simple-fun objects. */
    if (functionp(object) && embedded_obj_p(widetag_of(np))) {
      lispobj *base = fun_code_header(np);
      object = make_lispobj(base, OTHER_POINTER_LOWTAG);
    }
    if (!allocation_bit_marked(native_pointer(object)))
      lose("No allocation bit for 0x%lx", object);

    /* Enqueue onto mark queue */
    if (!pointer_survived_gc_yet(object)) {
      set_mark_bit(object);
      if (!output_block || output_block->count == QBLOCK_CAPACITY) {
        struct Qblock *n = grab_qblock();
        if (output_block) {
          output_block->next = grey_list;
          grey_list = output_block;
        }
        output_block = n;
      }
      output_block->elements[output_block->count++] = object;
    }
  }
}

static boolean interesting_pointer_p(lispobj object) {
  return in_dynamic_space(object) && gc_gen_of(object, 0) == generation_to_collect;
}

#define ACTION mark
#define TRACE_NAME trace_other_object
#define HT_ENTRY_LIVENESS_FUN_ARRAY_NAME mr_alivep_funs
#include "trace-object.inc"

static void trace_object(lispobj object) {
  if (listp(object)) {
    struct cons *c = CONS(object);
    mark(c->car); mark(c->cdr);
  } else {
    lispobj *p = native_pointer(object);
    trace_other_object(p);
  }
}

static boolean work_to_do() {
  return ANY((input_block && input_block->count) || output_block || grey_list);
}

#define PREFETCH_DISTANCE 16
static lispobj dequeue() {
  if (!input_block || !input_block->count) {
    if (input_block) recycle_qblock(input_block);
    if (output_block) {
      /* Reuse input block */
      input_block = output_block;
      output_block = NULL;
    } else if (grey_list) {
      /* Take from grey block */
      input_block = grey_list;
      grey_list = grey_list->next;
    } else {
      /* No more work to do */
      input_block = NULL;
    }
  }

  if (!input_block) lose("Called dequeue() with no work to do");
  lispobj v = input_block->elements[--input_block->count];
  if (input_block->count > PREFETCH_DISTANCE)
    __builtin_prefetch(native_pointer(input_block->elements[input_block->count - PREFETCH_DISTANCE]));
  return v;
}

uword_t traced;
static void trace_everything() {
  while (work_to_do() ||
         (test_weak_triggers(pointer_survived_gc_yet, mark) && work_to_do())) {
    traced++;
    lispobj obj = dequeue();
    trace_object(obj);
    if (listp(obj))
      mark_cons_line(CONS(obj));
    else
      mark_lines(native_pointer(obj));
  }
}

/* Conservative pointer scanning */
static lispobj *last_address_in(uword_t bitword, uword_t word_index) {
  /* TODO: Make this portable? MSVC uses _BitScanReverse64 and
   * we should probably have a portable fallback too. */
  int last_bit = N_WORD_BITS - 1 - __builtin_clzl(bitword);
  lispobj x = DYNAMIC_SPACE_START + ((word_index * N_WORD_BITS + last_bit) << N_LOWTAG_BITS);
  return (lispobj*)x;
}

static lispobj fix_pointer(lispobj *p, uword_t original) {
  if (embedded_obj_p(widetag_of(p))) {
      p = fun_code_header(p);
  }
  if (native_pointer(original) >= p + object_size(p)) {
    return 0;
  }
  lispobj l = compute_lispobj(p);
  return l;
}

boolean allocation_bit_marked(void *address) {
  uword_t first_bit_index = ((uword_t)(address) - DYNAMIC_SPACE_START) >> N_LOWTAG_BITS;
  uword_t first_word_index = first_bit_index / N_WORD_BITS;
  uword_t masked_out = allocation_bitmap[first_word_index] & ((uword_t)(1) << (first_bit_index % N_WORD_BITS));
  return ANY(masked_out);
}

void set_allocation_bit_mark(void *address) {
  uword_t first_bit_index = ((uword_t)(address) - DYNAMIC_SPACE_START) >> N_LOWTAG_BITS;
  uword_t first_word_index = first_bit_index / N_WORD_BITS;
  allocation_bitmap[first_word_index] |= ((uword_t)(1) << (first_bit_index % N_WORD_BITS));
}

static uword_t mark_bitmap_word_index(void *where) {
  return ((uword_t)where - DYNAMIC_SPACE_START) / (N_WORD_BITS << N_LOWTAG_BITS);
}

static lispobj find_object(uword_t address, uword_t start) {
  page_index_t p = find_page_index((void*)address);
  if (page_free_p(p)) return 0;
  if (page_table[p].type == PAGE_TYPE_CONS) {
    /* CONS cells are always aligned, and the mutator is allowed to be lazy
     * w.r.t putting down allocation bits, so just use alignment. */
    if (allocation_bit_marked(native_pointer(address)))
      return ALIGN_DOWN(address, 1 << N_LOWTAG_BITS) + LIST_POINTER_LOWTAG;
    else
      return 0;
  } else {
    /* Go scanning for the object. */
    uword_t first_bit_index = (address - DYNAMIC_SPACE_START) >> N_LOWTAG_BITS;
    uword_t first_word_index = first_bit_index / N_WORD_BITS;
    uword_t start_word_index = mark_bitmap_word_index((void*)start);
    /* Return the last location not after the address provided. */
    /* Supposing first_bit_index = 5, we compute
     * all_not_after = (1 << 6) - 1 = ...000111111
     * i.e. all bits not above bit #5 set. */
    uword_t all_not_after;
    /* Need to ensure we don't overflow while trying to generate
     * all bits set. */
    if (first_bit_index % N_WORD_BITS == N_WORD_BITS - 1)
      all_not_after = ~0;
    else
      all_not_after = ((uword_t)(1) << ((first_bit_index % N_WORD_BITS) + 1)) - 1;
    if (allocation_bitmap[first_word_index] & all_not_after)
      return fix_pointer(last_address_in(allocation_bitmap[first_word_index] & all_not_after, first_word_index),
                         address);
    uword_t i = first_word_index - 1;
    while (i >= start_word_index) {
      if (allocation_bitmap[i])
        /* Return the last location. */
        return fix_pointer(last_address_in(allocation_bitmap[i], i), address);
      /* Don't underflow */
      if (i == 0) break;
      i--;
    }
    return 0;
  }
}

lispobj *search_dynamic_space(void *pointer) {
  lispobj o = find_object((uword_t)pointer, DYNAMIC_SPACE_START);
  return o ? native_pointer(o) : NULL;
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
    if (page_single_obj_p(p)) {
      if (page_table[p].gen == generation_to_collect)
        set_page_bytes_used(p, 0);
    }
  }
}

#define for_lines_in_page(l, p) \
  for (line_index_t l = address_line(page_address(p)); \
       l < address_line(page_address(p + 1)); \
       l++)
/* Use AVX2 versions of code when we can, since blasting bytes faster
 * is always nice */
#define CPU_SPLIT __attribute__((target_clones("default,avx2")))

/* Pulled out these functions to clue auto-vectorisation. */
CPU_SPLIT
static page_bytes_t count_dead_bytes(page_index_t p) {
  unsigned char dead = ENCODE_GEN(generation_to_collect);
  page_bytes_t n = 0;
  unsigned char *lines = line_bytemap;
  for_lines_in_page(l, p)
    if (lines[l] == dead) n++;
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
  for_lines_in_page(l, p)
    lines[l] = (lines[l] == unmarked) ? 0 : (lines[l] == marked) ? unmarked : lines[l];
}

static void sweep_lines() {
  /* Free this gen, and work out how much space is used on each small
   * page. */
  for (page_index_t p = 0; p <= page_table_pages; p++)
    if (!page_free_p(p) && !page_single_obj_p(p) &&
        page_table[p].gen != PSEUDO_STATIC_GENERATION) {
      page_bytes_t decrement = count_dead_bytes(p);
      generations[generation_to_collect].bytes_allocated -= decrement;
      set_page_bytes_used(p, page_bytes_used(p) - decrement);
      sweep_small_page(p);
    }
}

static void sweep() {
  local_smash_weak_pointers();
  gc_dispose_private_pages();
  cull_weak_hash_tables(mr_alivep_funs);

  /* Reset values we're about to recompute */
  bytes_allocated = 0;
  sweep_lines();

  for (page_index_t p = 0; p < page_table_pages; p++) {
    if (page_words_used(p) == 0) {
      /* Remove allocation bit for the large object here. */
      if (page_single_obj_p(p))
        allocation_bitmap[mark_bitmap_word_index(page_address(p))] = 0;
      reset_page_flags(p);
      page_table[p].gen = 0;
    } else {
      bytes_allocated += page_bytes_used(p);
      if (page_single_obj_p(p))
        generations[page_table[p].gen].bytes_allocated += page_bytes_used(p);
      /* next_free_page is only maintained for page walking - we
       * reuse partially filled pages, so it's not useful for allocation */
      next_free_page = p + 1;
    }
  }
  memset(mark_bitmap, 0, mark_bitmap_size);
}

extern lispobj lisp_init_function, gc_object_watcher;
static void trace_static_roots() {
  trace_other_object((lispobj*)NIL_SYMBOL_SLOTS_START);
  lispobj *where = (lispobj*)STATIC_SPACE_OBJECTS_START;
  lispobj *end = static_space_free_pointer;
  while (where < end) {
    lispobj obj = compute_lispobj(where);
    trace_object(obj);
    where += listp(obj) ? 2 : headerobj_size(where);
  }
  mark(lisp_package_vector);
  if (lisp_init_function) mark(lisp_init_function);
  if (gc_object_watcher) mark(gc_object_watcher);
  if (alloc_profile_data) mark(alloc_profile_data);
}

/* Entry points */

void mr_preserve_pointer(uword_t address) {
  if (find_page_index((void*)address) > -1) {
    lispobj obj = find_object(address, DYNAMIC_SPACE_START);
    if (obj) mark(obj);
  }
}

void mr_preserve_range(lispobj *from, sword_t nwords) {
  for (sword_t n = 0; n < nwords; n++) {
    mark(from[n]);
  }
}

void mr_preserve_object(lispobj obj) {
  if (is_lisp_pointer(obj) && in_dynamic_space(obj)) {
    set_mark_bit(obj);
    lispobj *n = native_pointer(obj);
    add_words_used(n, object_size(n));
    mark_lines(n);
  }
}

static void update_card_mark(int card, boolean dirty) {
  if (gc_card_mark[card] != STICKY_MARK)
    gc_card_mark[card] = dirty ? CARD_MARKED : CARD_UNMARKED;
}

/* Check if an object is dirty in some way that tracing wouldn't uncover.
 * This happens specifically with weak vectors and weak values, as we
 * don't actually trace those when "tracing" them. We only record dirtyness
 * without tracing, however, in order to allow weak values to be culled
 * without a (more) major GC. */

static void check_otherwise_dirty(lispobj *where) {
  uword_t header = *where;
  int widetag = header_widetag(header);
  generation_index_t gen = gc_gen_of((lispobj)where, 0);
  switch (widetag) {
  case SIMPLE_VECTOR_WIDETAG:
    if (vector_flagp(header, VectorWeak)) {
      for (sword_t i = 1; i < headerobj_size2(where, header); i++)
        if (is_lisp_pointer(where[i]) && gc_gen_of(where[i], 0) < gen) {
          dirty = 1;
          return;
        }
    }
    break;
  case WEAK_POINTER_WIDETAG:
    struct weak_pointer *weakptr = (struct weak_pointer*)where;
    if (is_lisp_pointer(weakptr->value) && gc_gen_of(weakptr->value, 0) < gen) {
      dirty = 1;
      return;
    }
    break;
  }
}

static void mr_scavenge_root_gens() {
  page_index_t i = 0;
  int checked = 0;
  /* Keep this around, to avoid scanning objects which overlap cards
   * more than once. */
  lispobj *last_scavenged = 0;
  while (i < page_table_pages) {
    if ((page_table[i].type & PAGE_TYPE_MASK) == PAGE_TYPE_UNBOXED ||
        !page_words_used(i) ||
        !cardseq_any_marked(page_to_card_index(i))) {
      i++; continue;
    }
    checked++;
    // fprintf(stderr, "Scavenging page %ld\n", i);
    if (page_single_obj_p(i)) {
      if (page_table[i].gen > generation_to_collect) {
        /* The only time that page_address + page_words_used actually
         * demarcates the end of a (sole) object on the page, with this
         * heap layout. */
        generation_index_t gen = page_table[i].gen;
        lispobj *limit = (lispobj*)page_address(i) + page_words_used(i);
        lispobj *start = (lispobj*)page_address(i);
        for (int j = 0, card = addr_to_card_index(start);
             j < CARDS_PER_PAGE;
             j++, card++, start += WORDS_PER_CARD) {
          if (card_dirtyp(card)) {
            lispobj *card_end = start + WORDS_PER_CARD;
            lispobj *end = (limit < card_end) ? limit : card_end;
            // fprintf(stderr, "Scavenging large page %ld from %p to %p: ", i, start, end);
            dirty_generation_source = gen, dirty = 0;
            for (lispobj *p = start; p < end; p++)
              mark(*p);
            // fprintf(stderr, "%s\n", dirty ? "dirty" : "clean");
            update_card_mark(card, dirty);
          }
        }
      }
    } else {
      boolean cons_page = page_table[i].type == PAGE_TYPE_CONS;
      /* Scavenge every object in every card and try to re-protect. */
      lispobj *page_start = (lispobj*)page_address(i), *start = page_start;
      for (int j = 0, card = addr_to_card_index(start);
           j < CARDS_PER_PAGE;
           j++, card++, start += WORDS_PER_CARD) {
        if (card_dirtyp(card)) {
          /* Check if an object overlaps the start of the card. Due to
           * alignment this cannot happen with cons pages. */
          lispobj *first_object = cons_page ? 0 : native_pointer(find_object((lispobj)start, DYNAMIC_SPACE_START));
          lispobj *end = start + WORDS_PER_CARD;
          if (first_object && first_object < start && first_object != last_scavenged) {
            /* In principle, we can mark any card that intersects the
             * object, but we try to normalise and mark the first card. */
            dirty = 0;
            generation_index_t gen = gc_gen_of((lispobj)first_object, 0);
            if (gen > generation_to_collect) {
              dirty_generation_source = gen;
              trace_object(compute_lispobj(first_object));
              check_otherwise_dirty(first_object);
              if (dirty)
                update_card_mark(addr_to_card_index(first_object), dirty);
              last_scavenged = first_object;
            }
          }
          // fprintf(stderr, "Scavenging page %ld from %p to %p: ", i, start, end);
          dirty = 0;
          /* TODO: navigate between lines with the right generations. */
          lispobj *where = next_object(start, 0, end);
          while (where) {
            generation_index_t gen = gc_gen_of((lispobj)where, 0);
            if (gen > generation_to_collect) {
              dirty_generation_source = gen;
              trace_object(compute_lispobj(where));
              check_otherwise_dirty(where);
            }
            last_scavenged = where;
            where = next_object(where, cons_page ? 2 : object_size(where), end);
          }
          // fprintf(stderr, "%s\n", dirty ? "dirty" : "clean");
          update_card_mark(card, dirty);
        }
      }
    }
    i++;
  }
  dirty_generation_source = 0;
  // fprintf(stderr, "Scavenged %d pages\n", checked);
}

/* Everything has to be an argument here, in order to convince
 * auto-vectorisation to do its thing. */
CPU_SPLIT
static void raise_survivors(unsigned char *bytemap, line_index_t count, generation_index_t gen) {
  unsigned char line = ENCODE_GEN((unsigned char)gen);
  unsigned char target = ENCODE_GEN((unsigned char)gen + 1);
  for (line_index_t l = 0; l < count; l++)
    bytemap[l] = (bytemap[l] == line) ? target : bytemap[l];
  for (page_index_t p = 0; p < page_table_pages; p++)
    if (page_table[p].gen == generation_to_collect && page_single_obj_p(p))
      page_table[p].gen++;
  generations[generation_to_collect + 1].bytes_allocated += generations[generation_to_collect].bytes_allocated;
  generations[generation_to_collect].bytes_allocated = 0;
}

static unsigned int collection = 0;
void mr_pre_gc(generation_index_t generation) {
  uword_t prior_bytes = bytes_allocated;
  //fprintf(stderr, "[GC #%d gen %d %luM ", ++collection, generation, prior_bytes >> 20);
  generation_to_collect = generation;
  reset_statistics();
}

void mr_collect_garbage(boolean raise) {
  mr_scavenge_root_gens();
  trace_static_roots();
  trace_everything();
  sweep();
  free_mark_list();
  if (raise)
    raise_survivors(line_bytemap, line_count, generation_to_collect);

#if 0
  fprintf(stderr,
          "-> %luM, %lu traced, page hwm = %ld%s]\n",
          bytes_allocated >> 20, traced,
          next_free_page, raise ? ", raised" : "");
  for (generation_index_t g = 0; g <= PSEUDO_STATIC_GENERATION; g++)
    fprintf(stderr, "%d: %ld\n", g, generations[g].bytes_allocated);
#endif
  memset(allow_free_pages, 0, sizeof(allow_free_pages));
}

void zero_all_free_ranges() {
#if 0
  for (page_index_t p = 0; p < page_table_pages; p++)
    if (!page_single_obj_p(p))
      for (line_index_t l = address_line(page_address(p));
           l < address_line(page_address(p + 1));
           l++)
        if (!line_bytemap[l])
          memset(line_address(l), 0, LINE_SIZE);
#endif
}

/* Useful hacky stuff */

void find_references_to(lispobj something) {
  for (uword_t i = 0; i < (dynamic_space_size / N_WORD_BYTES); i++) {
    lispobj *p = (lispobj*)(DYNAMIC_SPACE_START + i * N_WORD_BYTES);
    if (labs(*p - something) < 64)
      printf("%p: %lx\n", p, *p);
  }
}

void draw_page_table(int from, int to) {
  for (int i = from; i < to; i++) {
    if (i % 50 == 0) fprintf(stderr, "\n%4d ", i);
    fprintf(stderr,
            "\033[%c;9%cm%c%c%c\033[0m",
            (page_table[i].type & SINGLE_OBJECT_FLAG) ? '1' : '0',
            '0' + (page_table[i].type & 7),
            64 + page_table[i].type,
            '0' + page_table[i].gen,
            '0' + (unsigned char)(10.0 * (double)page_bytes_used(i) / (double)GENCGC_PAGE_BYTES));
  }
}

int drawing_count = 0;
void draw_line_bytemap(int type) {
  char name[30];
  snprintf(name, 30, "/tmp/bytemap%d.pbm", drawing_count++);
  FILE *f = fopen(name, "w");
  fprintf(f, "P2 512 %ld 3\n", dynamic_space_size / GENCGC_PAGE_BYTES);
  for (line_index_t i = 0; i < line_count; i++) {
    page_index_t p = find_page_index(line_address(i));
    int n = line_bytemap[i] * 3;
    if (page_table[p].type != type || page_bytes_used(p) == GENCGC_PAGE_BYTES)
      n = 1;
    if (page_table[p].gen != 0)
      n = 2;
    fprintf(f, "%d ", n);
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
