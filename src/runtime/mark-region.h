#ifndef MARK_REGION_H
#define MARK_REGION_H
#ifdef LISP_FEATURE_MARK_REGION_GC
#include "lispobj.h"
#include "core.h"

/* Set line size so that every line byte corresponds to one mark
 * bitmap byte. */
#define LINE_SIZE (8 << N_LOWTAG_BITS)

extern uword_t *allocation_bitmap, *mark_bitmap;
extern unsigned char *line_bytemap;
typedef intptr_t line_index_t;

extern void mrgc_init();
extern void load_corefile_bitmaps(int fd, core_entry_elt_t n_ptes);
extern sword_t bitmap_sizes(core_entry_elt_t n_ptes);

extern boolean try_allocate_small_from_pages(sword_t nbytes, struct alloc_region *region,
                                             int page_type, generation_index_t gen,
                                             page_index_t *start, page_index_t end);
extern boolean try_allocate_small_after_region(sword_t nbytes,
                                               struct alloc_region *region);
extern page_index_t try_allocate_large(sword_t nbytes,
                                       int page_type, generation_index_t gen,
                                       page_index_t *start, page_index_t end);
extern void mr_update_closed_region(struct alloc_region *region, generation_index_t gen);

extern boolean allocation_bit_marked(void *pointer);
extern void set_allocation_bit_mark(void *pointer);
extern void clear_allocation_bit_mark(void *pointer);
extern boolean line_marked(void *pointer);

extern lispobj *search_dynamic_space(void *pointer);

extern void mr_preserve_pointer(uword_t address);
extern void mr_preserve_range(lispobj *from, sword_t nwords);
extern void mr_preserve_leaf(lispobj obj);
extern void mr_preserve_object(lispobj obj);
extern void mr_pre_gc(generation_index_t generation);
extern void mr_collect_garbage(boolean raise);
extern void zero_all_free_ranges();
extern void prepare_lines_for_final_gc();
#endif
#endif
