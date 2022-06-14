/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

#ifndef _GENCGC_PRIVATE_H_
#define _GENCGC_PRIVATE_H_

void zeroize_pages_if_needed(page_index_t start, page_index_t end, int page_type);

typedef unsigned int page_bytes_t;
#define page_words_used(index) page_table[index].words_used_
#define page_bytes_used(index) ((page_bytes_t)page_table[index].words_used_<<WORD_SHIFT)
#if defined LISP_FEATURE_RISCV && defined LISP_FEATURE_LINUX // KLUDGE
#define page_need_to_zero(index) (mmap_does_not_zero || (*(uword_t*)page_address(index) != 0))
#else
#define page_need_to_zero(index) (*(uword_t*)page_address(index) != 0)
#endif
#define set_page_bytes_used(index,val) page_table[index].words_used_ = ((val)>>WORD_SHIFT)
#define set_page_needs_zerofill(index) *(uword_t*)page_address(index) = 0xBADBAD

#if !CONDENSED_PAGE_TABLE

// 32-bit doesn't need magic to reduce the size of scan_start_offset.
#define set_page_scan_start_offset(index,val) \
  page_table[index].scan_start_offset_ = val
#define page_scan_start_offset(index) page_table[index].scan_start_offset_

#else

/// A "condensed" offset reduces page table size, which improves scan locality.
/// As stored, the offset is scaled down either by card size or double-lispwords.
/// If the offset is the maximum, then we must check if the page pointed to by
/// that offset is actually the start of a region, and retry if not.
/// For debugging the iterative algorithm it helps to use a max value
/// that is less than UINT_MAX to get a pass/fail more quickly.

//#define SCAN_START_OFS_MAX 0x3fff
#define SCAN_START_OFS_MAX UINT_MAX

static void __attribute__((unused))
set_page_scan_start_offset(page_index_t index, os_vm_size_t offset)
{
    // If the offset is nonzero and page-aligned
    unsigned int lsb = offset !=0 && IS_ALIGNED(offset, GENCGC_PAGE_BYTES);
    os_vm_size_t scaled = (offset >> (lsb ? GENCGC_CARD_SHIFT-1 : WORD_SHIFT)) | lsb;
    if (scaled > SCAN_START_OFS_MAX) {
        // Assert that if offset exceed the max representable value,
        // then it is a page-aligned offset, not a cons-aligned offset.
        gc_assert(lsb == 1);
        // Clip it to the maximum representable value.
        // The accessor will have to iterate to find the true start of region.
        scaled = SCAN_START_OFS_MAX;
    }
    page_table[index].scan_start_offset_ = scaled;
}

static os_vm_size_t scan_start_offset_iterated(page_index_t index)
{
    // The low bit of the MAX is the 'scale' bit. The max pages we can look
    // backwards is therefore the max shifted right by 1 bit.
    page_index_t tot_offset_in_pages = 0;
    unsigned int offset;
    do {
        page_index_t lookback_page = index - tot_offset_in_pages;
        offset = page_table[lookback_page].scan_start_offset_;
        tot_offset_in_pages += offset >> 1;
    } while (offset == SCAN_START_OFS_MAX);
    return (os_vm_size_t)tot_offset_in_pages << GENCGC_CARD_SHIFT;
}

static os_vm_size_t  __attribute__((unused)) page_scan_start_offset(page_index_t index)
{
    return page_table[index].scan_start_offset_ != SCAN_START_OFS_MAX
        ? (os_vm_size_t)(page_table[index].scan_start_offset_ & ~1)
          << ((page_table[index].scan_start_offset_ & 1) ?
              (GENCGC_CARD_SHIFT-1) : WORD_SHIFT)
        : scan_start_offset_iterated(index);
}

#endif

#define is_code(type) ((type & PAGE_TYPE_MASK) == PAGE_TYPE_CODE)

// If *all* pages use soft card marks, then protection_mode() is not a thing.
// Otherwise, only pages of code use soft card marks; return an enum indicating
// whether the page protection for the specified page is applied in harware.
#ifndef LISP_FEATURE_SOFT_CARD_MARKS
enum prot_mode { PHYSICAL, LOGICAL };
static inline enum prot_mode protection_mode(page_index_t page) {
    // code pages can be marked as logically read-only without OS protection,
    // and everything else uses hardware-based protection where applicable.
    return is_code(page_table[page].type) ? LOGICAL : PHYSICAL;
}
#endif

static inline boolean page_free_p(page_index_t page) {
    return (page_table[page].type == FREE_PAGE_FLAG);
}

/* SIMD-within-a-register algorithms
 *
 * from https://graphics.stanford.edu/~seander/bithacks.html
 */
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
static inline uword_t word_haszero(uword_t word) {
  return ((word - 0x0101010101010101LL) & ~word & 0x8080808080808080LL) != 0;
}
static inline uword_t word_has_stickymark(uword_t word) {
  return word_haszero(word ^ 0x0202020202020202LL);
}
#include "genesis/cardmarks.h"
static int page_cards_all_marked_nonsticky(page_index_t page) {
    return cardseq_all_marked_nonsticky(page_to_card_index(page));
}
#endif

/* Test whether page 'index' can continue a non-large-object region
 * having specified 'gen' and 'type' values. It must not be pinned
 * and must be marked but not referenced from the stack */
static inline boolean
page_extensible_p(page_index_t index, generation_index_t gen, int type) {
#ifdef LISP_FEATURE_BIG_ENDIAN /* TODO: implement this as single comparison */
    int attributes_match =
           page_table[index].type == type
        && page_table[index].gen == gen
        && !gc_page_pins[index];
#else
    /* Test 'gen' and 'type' as one comparison.
     * The type is at 1 byte prior to 'gen' in the page structure.
     */
    int attributes_match =
        *(int16_t*)(&page_table[index].gen-1) == ((gen<<8)|type);
#endif
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
    return attributes_match && page_cards_all_marked_nonsticky(index);
#else
    return attributes_match && !PAGE_WRITEPROTECTED_P(index);
#endif
}

enum {
    SCRATCH_GENERATION = PSEUDO_STATIC_GENERATION+1,
    NUM_GENERATIONS
};

extern struct generation generations[NUM_GENERATIONS];

void reset_page_flags(page_index_t page);
#endif /* _GENCGC_PRIVATE_H_ */
