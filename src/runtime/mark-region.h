#ifndef MARK_REGION_H
#define MARK_REGION_H
#ifdef LISP_FEATURE_MARK_REGION_GC
#include "lispobj.h"
#include "core.h"

#define LINE_SIZE 128

extern uword_t *allocation_bitmap, *mark_bitmap;
extern char *line_bytemap;
extern void mrgc_init();

typedef intptr_t line_index_t;

extern void load_corefile_bitmaps(int fd, core_entry_elt_t n_ptes);
extern void bitmap_sizes(core_entry_elt_t n_ptes, sword_t *where);
#endif
#endif
