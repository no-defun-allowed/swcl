#ifndef MARK_REGION_H
#define MARK_REGION_H

#ifdef LISP_FEATURE_MARK_REGION_GC
extern char *allocation_bitmap, *grey_bitmap, *black_bitmap;
extern void mrgc_init();
#endif

#endif
