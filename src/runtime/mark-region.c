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
char *line_bytemap;             /* 1 if line used, 0 if not used */
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
                           line_index_t start, line_index_t end) {
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
      memset(region->start_addr, 0, addr_diff(region->end_addr, region->start_addr));
      return 1;
    }
    if (chunk_end == end) return 0;
    where = chunk_end;
  }
}

/* Medium path for allocation, wherein we use another chunk that the
 * thread already claimed. */
boolean try_allocate_small_after_region(sword_t nbytes, struct alloc_region *region) {
  /* Can't do this if we have no page. */
  if (!region->start_addr) return 0;
  /* We search to the end of this page. */
  line_index_t end = address_line(PTR_ALIGN_UP(region->end_addr, GENCGC_PAGE_BYTES));
  return try_allocate_small(nbytes, region, address_line(region->end_addr), end);
}

/* try_allocate_small_from_pages updates the start pointer to after the
 * claimed page. */
boolean try_allocate_small_from_pages(sword_t nbytes, struct alloc_region *region,
                                      int page_type, generation_index_t gen,
                                      page_index_t *start, page_index_t end) {
  // printf("try_allocate_small_f_p(%lu, %d, %d, %ld, %ld)\n", nbytes, page_type, gen, *start, end);
  for (page_index_t where = *start; where < end; where++)
    if (page_bytes_used(where) <= GENCGC_PAGE_BYTES - nbytes &&
        (page_free_p(where) || page_extensible_p(where, gen, page_type)) &&
        try_allocate_small(nbytes, region,
                           address_line(page_address(where)),
                           address_line(page_address(where + 1)))) {
      gc_assert(page_table[where].type == FREE_PAGE_FLAG || page_table[where].type == page_type);
      page_table[where].type = page_type | OPEN_REGION_PAGE_FLAG;
      page_table[where].gen = gen;
      set_page_scan_start_offset(where, 0);
      *start = where + 1;
      // printf(" => %ld\n", where);
      return 1;
    }
  return 0;
}

/* Allocation annoying-path */

DEF_FINDER(find_free_page, page_index_t, page_free_p(where), -1);
DEF_FINDER(find_used_page, page_index_t, !page_free_p(where), end);

page_index_t try_allocate_large(sword_t nbytes,
                                int page_type, generation_index_t gen,
                                page_index_t *start, page_index_t end) {
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
      bytes_allocated += nbytes;
      generations[gen].bytes_allocated += nbytes;
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
  //printf("closing %lu\n", the_page);
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
}

/* Marking */

static generation_index_t generation_being_collected;

#define ANY(x) !!(x)
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
  return !in_dynamic_space(object) || object_marked_p(object);
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
static void free_mark_list() {
  gc_assert(mark_queue == NULL);
  while (recycle_queue) {
    struct Qblock *old = recycle_queue;
    recycle_queue = recycle_queue->next;
    free(old);
  }
}

static void add_words_used(void *where, uword_t count) {
  page_index_t p = find_page_index(where);
  uword_t byte_count = count * N_WORD_BYTES;
  while (byte_count >= GENCGC_PAGE_BYTES) {
    set_page_bytes_used(p, GENCGC_PAGE_BYTES);
    byte_count -= GENCGC_PAGE_BYTES;
    p++;
  }
  if (byte_count)
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
  uword_t word_count = object_size(p);
  line_index_t first = address_line(p), last = address_line(p + word_count - 1);
  for (line_index_t line = first; line <= last; line++) line_bytemap[line] = 1;
  add_words_used(p, word_count);
}

static void mark(lispobj object) {
  if (is_lisp_pointer(object) && in_dynamic_space(object)) {
    /*
    unsigned char page_type = page_table[find_page_index(native_pointer(object))].type & PAGE_TYPE_MASK;
    if (!listp(object) && page_type == PAGE_TYPE_CONS)
      lose("Non-cons pointer %lx to cons page", object);
    if (listp(object) && page_type != PAGE_TYPE_CONS)
      lose("Cons pointer %lx to non-cons page", object);
    */
    
    lispobj *np = native_pointer(object);
    if (functionp(object) && embedded_obj_p(widetag_of(np))) {
      set_mark_bit(object);     /* This makes verify happy. */
      lispobj *base = fun_code_header(np);
      object = make_lispobj(base, OTHER_POINTER_LOWTAG);
    }
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
}

static boolean interesting_pointer_p(lispobj object) {
  return in_dynamic_space(object);
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
    if (in_dynamic_space(object))
      mark_cons_line(c);
    mark(c->car); mark(c->cdr);
  } else {
    lispobj *p = native_pointer(object);
    if (in_dynamic_space(object))
      mark_lines(p);
    trace_other_object(p);
  }
}

static lispobj dequeue() {
  gc_assert(mark_queue != NULL);
  gc_assert(mark_queue->count);
  lispobj v = mark_queue->elements[--mark_queue->count];
  if (mark_queue->count == 0) {
    struct Qblock *next = mark_queue->next;
    recycle_qblock(mark_queue);
    mark_queue = next;
  }
  return v;
}

uword_t traced;
static void trace_everything() {
  while (mark_queue != NULL ||
         (test_weak_triggers(pointer_survived_gc_yet, mark) && mark_queue != NULL)) {
    traced++;
    trace_object(dequeue());
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
      lispobj *base = fun_code_header(p);
      return make_lispobj(base, OTHER_POINTER_LOWTAG);
  }
  lispobj l = compute_lispobj(p);
  if (listp(l))
    lose("CONS on non-cons page, %p on page %lu, header %lx, derived from %lx",
         p, find_page_index(p), *p, original);
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
    for (uword_t i = first_word_index - 1; i > 0; i--)
      if (allocation_bitmap[i])
        /* Return the last location. */
        return fix_pointer(last_address_in(allocation_bitmap[i], i), address);
    lose("find_object fell through on %ld?", address);
  }
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
  bytes_allocated = 0;
  traced = 0;
  for (generation_index_t g = 0; g <= PSEUDO_STATIC_GENERATION; g++)
    generations[g].bytes_allocated = 0;
  for (page_index_t p = 0; p <= page_table_pages; p++)
    set_page_bytes_used(p, 0);
  memset(line_bytemap, 0, line_count);
}

extern void gc_close_collector_regions(int flag);
static void sweep() {
  local_smash_weak_pointers();
  gc_dispose_private_pages();
  cull_weak_hash_tables(mr_alivep_funs);
  /* Culling hash tables may allocate, so make sure we don't
   * lose those allocations. */
  gc_close_collector_regions(0);
  
  /* Calculate byte counts */
  bytes_allocated = 0;
  for (generation_index_t g = 0; g <= PSEUDO_STATIC_GENERATION; g++)
    generations[g].bytes_allocated = 0;

  for (page_index_t p = 0; p < page_table_pages; p++) {
    if (page_words_used(p) == 0 && !page_free_p(p)) {
      reset_page_flags(p);
    } else {
      bytes_allocated += page_bytes_used(p);
      generations[page_table[p].gen].bytes_allocated += page_bytes_used(p);
      /* next_free_page is only maintained for page walking - we
       * reuse partially filled pages, so it's not useful for allocation */
      next_free_page = p + 1;
    }
  }

  /* Prune allocation bitmap */
  memcpy(allocation_bitmap, mark_bitmap, mark_bitmap_size);
  memset(mark_bitmap, 0, mark_bitmap_size);
}

extern lispobj lisp_init_function, gc_object_watcher;
void trace_static_roots() {
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
  if (is_lisp_pointer(address) && find_page_index((void*)address) > -1)
    mark(find_object(address));
}

void mr_preserve_range(lispobj *from, sword_t nwords) {
  for (sword_t n = 0; n < nwords; n++)
    mark(from[n]);
}

void mr_preserve_object(lispobj obj) {
  set_mark_bit(obj);
  lispobj *n = native_pointer(obj);
  add_words_used(n, object_size(n));
  mark_lines(n);
}

static unsigned int collection = 0;
void mr_collect_garbage() {
  uword_t prior_bytes = bytes_allocated;
  printf("[GC #%d", ++collection);
  reset_statistics();
  trace_static_roots();
  trace_everything();
  sweep();
  free_mark_list();
  printf(" %luM -> %luM, %lu traced, fragmentation = %.4f, page hwm = %ld]\n",
         prior_bytes >> 20, bytes_allocated >> 20, traced,
         (double)(lines_used() * LINE_SIZE) / (double)(bytes_allocated),
         next_free_page);
}

/* Useful hacky stuff */

void find_references_to(lispobj something) {
  for (uword_t i = 0; i < (dynamic_space_size / N_WORD_BYTES); i++) {
    lispobj *p = (lispobj*)(DYNAMIC_SPACE_START + i * N_WORD_BYTES);
    if (labs(*p - something) < 64)
      printf("%p: %lx\n", p, *p);
  }
}

void draw_page_table() {
  for (int i = 0; i < 4000; i++) {
    if (i % 50 == 0) printf("\n%4d ", i);
    printf("\033[%c;9%cm%c%c\033[0m",
           (page_table[i].type & SINGLE_OBJECT_FLAG) ? '1' : '0',
           '0' + (page_table[i].type & 7),
           64 + page_table[i].type,
           // '0' + page_table[i].gen
           '0' + (unsigned char)(10.0 * (double)page_bytes_used(i) / (double)GENCGC_PAGE_BYTES)
           );
  }
}
