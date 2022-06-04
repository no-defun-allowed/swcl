#include <stdbool.h>
#include "align.h"
#include "os.h"
#include "gc.h"
#include "runtime.h"
#include "mark-region.h"
#include "validate.h"
#include "gc-assert.h"
#include "gencgc-internal.h"
#include "gencgc-private.h"
#include "core.h"
#include "interr.h"
#include "globals.h"
#include "lispobj.h"

/* The idea of the mark-region collector is to avoid copying where
 * possible, and instead reclaim as much memory in-place as possible.
 * The result is that we save time on copying objects (especially leaf
 * objects), and parallelisation is easier.
 * The design is loosely inspired by the Immix collector designed by
 * Blackburn and McKinley, mostly in being able to reclaim both smaller
 * lines and larger pages. But we plan to have a separate compaction
 * phase, rather than copying and marking in one pass, to make parallel
 * compacting easier. */

uword_t *allocation_bitmap, *mark_bitmap, *line_bytemap;
uword_t line_count;

static void allocate_bitmap(uword_t **bitmap, int divisor,
                            const char *description) {
  *bitmap = calloc(dynamic_space_size / divisor, 1);
  if (!*bitmap)
    lose("Failed to allocate %s (of %lu bytes)", description,
         dynamic_space_size / divisor);
}

void mrgc_init() {
  int bytes_per_heap_byte = N_WORD_BYTES * 2 * 8;
  allocate_bitmap(&allocation_bitmap, bytes_per_heap_byte,
                  "allocation bitmap");
  allocate_bitmap(&mark_bitmap, bytes_per_heap_byte,
                  "mark bitmap");
  allocate_bitmap(&line_bytemap, LINE_SIZE, "line bytemap");
  line_count = dynamic_space_size / LINE_SIZE;
}

static inline char *line_address(line_index_t line) {
  return (char*)(DYNAMIC_SPACE_START + (line * LINE_SIZE));
}

static inline line_index_t address_line(void *address) {
  return ((uintptr_t)address - DYNAMIC_SPACE_START) / LINE_SIZE;
}

/* Allocation of small objects is done by finding contiguous lines
 * that can fit the object to allocate. Small objects can span lines
 * but cannot pages, so we examine lines in each page separately. */
static line_index_t find_free_line(line_index_t start, line_index_t end) {
  for (line_index_t where = start; where < end; where++)
    if (!line_bytemap[where]) return where;
  return -1;
}
static line_index_t find_used_line(line_index_t start, line_index_t end) {
  for (line_index_t where = start; where < end; where++)
    if (line_bytemap[where]) return where;
  return end;
}

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
      return true;
    }
  }
}

/* Fast path for allocation, wherein we use another chunk that the
 * thread already claimed. */
boolean try_allocate_small_after_region(sword_t nbytes, struct alloc_region *region) {
  return try_allocate_small(nbytes, region,
                            address_line(region->end_addr),
                            address_line(page_address(1 + find_page_index(region->end_addr))));
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
      page_table[where].type |= OPEN_REGION_PAGE_FLAG;
      *start = where + 1;
      return true;
    }
  return false;
}

boolean try_allocate_large(sword_t nbytes, struct alloc_region *region,
                           int page_type, generation_index_t gen,
                           page_index_t *start, page_index_t end) {
  /* We need at least one not-entirely-filled page to ensure
   * page_ends_contiguous_block_p will still work. */
  int pages_needed = ALIGN_UP(nbytes + 1, GENCGC_PAGE_BYTES) / GENCGC_PAGE_BYTES;

  page_index_t where = *start;
  while (1) {

  }
  return false;
}
