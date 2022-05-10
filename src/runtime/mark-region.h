#ifndef MARK_REGION_H
#define MARK_REGION_H
#ifdef LISP_FEATURE_MARK_REGION_GC
#include "lispobj.h"

#define LINE_SIZE 128

extern uword_t *allocation_bitmap, *mark_bitmap, *line_bytemap;
extern void mrgc_init();
#endif
#endif
