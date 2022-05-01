#include "os.h"
#include "gc.h"
#include "runtime.h"
#include "mark-region.h"
#include "core.h"
#include "interr.h"
#include "globals.h"

char *allocation_bitmap, *grey_bitmap, *black_bitmap;

static void allocate_bitmap(char *bitmap, const char *description) {
  bitmap = calloc(dynamic_space_size / N_WORD_BYTES / 2, 1);
  if (!bitmap)
    lose("Failed to allocate %s", description);
}

void mrgc_init() {
  allocate_bitmap(allocation_bitmap, "allocation bitmap");
  allocate_bitmap(grey_bitmap, "grey bitmap");
  allocate_bitmap(black_bitmap, "black bitmap");
}
