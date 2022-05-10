#include "os.h"
#include "gc.h"
#include "runtime.h"
#include "mark-region.h"
#include "core.h"
#include "interr.h"
#include "globals.h"
#include "lispobj.h"

uword_t *allocation_bitmap, *mark_bitmap, *line_bytemap;

static void allocate_bitmap(uword_t **bitmap, int divisor,
                            const char *description) {
  *bitmap = calloc(dynamic_space_size / divisor, 1);
  if (!*bitmap)
    lose("Failed to allocate %s", description);
}

void mrgc_init() {
  int bytes_per_heap_byte = N_WORD_BYTES * 2 * 8;
  allocate_bitmap(&allocation_bitmap, bytes_per_heap_byte,
                  "allocation bitmap");
  allocate_bitmap(&black_bitmap, bytes_per_heap_byte,
                  "mark bitmap");
  allocate_bitmap(&line_bytemap, LINE_SIZE, "line bytemap");
}
