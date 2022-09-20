/* I have no idea what to name this header, honestly.
 * But this header defines a sort of "iterator" over contiguous
 * and non-contiguous heaps. */

#include "gencgc-internal.h"
#include "gencgc-private.h"

#ifdef LISP_FEATURE_MARK_REGION_GC
#include "mark-region.h"

static lispobj *next_object(lispobj *previous, uword_t size, lispobj *end) {
  /* Dynamic space is non-contiguous, static space is contiguous */
  if (find_page_index(previous) != -1) {
    for (lispobj *where = previous + size; where < end; where += 2)
      if (allocation_bit_marked(where))
        return where;
    return NULL;
  } else {
    if (previous + size >= end) return NULL;
    return previous + size;
  }
}

static lispobj __attribute__((unused)) *page_limit(page_index_t page) {
  return (lispobj*)page_address(page + 1);
}

#else

static lispobj *next_object(lispobj *previous, uword_t size, lispobj *limit) {
  if (previous + size >= end) return NULL;
  return previous + size;
}

static lispobj __attribute__((unused)) *page_limit(page_index_t page) {
  return page_address(page) + page_words_used(page);
}

#endif
