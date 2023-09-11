/*
 * Conservative Parallelized Mark-Region Garbage Collector for SBCL
 */

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

#include <stdlib.h>
#include <stdio.h>
#include "sbcl.h"
#include "lispregs.h"
#include "gc.h"
#include "code.h"
#include "genesis/fdefn.h"
#include "incremental-compact.h"
#include "pseudo-atomic.h"
#include "genesis/instance.h"
#include "genesis/gc-tables.h"
#include "genesis/list-node.h"
#include "thread.h"

/* forward declarations */
extern FILE *gc_activitylog();


/* Largest allocation seen since last GC. */
os_vm_size_t large_allocation = 0;
int n_gcs;


/*
 * debugging
 */

/* the verbosity level. All non-error messages are disabled at level 0;
 * and only a few rare messages are printed at level 1. */
int gencgc_verbose = 1;

/* FIXME: At some point enable the various error-checking things below
 * and see what they say. */

/* We hunt for pointers to old-space, when GCing generations >= verify_gen.
 * Set verify_gens to HIGHEST_NORMAL_GENERATION + 2 to disable this kind of
 * check. */
generation_index_t verify_gens = 0; // HIGHEST_NORMAL_GENERATION + 2;

/* Should we do a pre-scan of the heap before it's GCed? */
int pre_verify_gen_0 = 0; // FIXME: should be named 'pre_verify_gc'


/*
 * GC structures and variables
 */

/* the total bytes allocated. These are seen by Lisp DYNAMIC-USAGE. */
os_vm_size_t bytes_allocated = 0;
os_vm_size_t auto_gc_trigger = 0;

/* the source and destination generations. These are set before a GC starts
 * scavenging. */
generation_index_t from_space;
generation_index_t new_space;

/* Set to 1 when in GC */
bool gc_active_p = 0;

/* should the GC be conservative on stack. If false (only right before
 * saving a core), don't scan the stack / mark pages pinned. */
bool conservative_stack = 1;
int save_lisp_gc_iteration;

/* An array of page structures is allocated on gc initialization.
 * This helps to quickly map between an address and its page structure.
 * page_table_pages is set from the size of the dynamic space. */
page_index_t page_table_pages;
struct page *page_table;
unsigned char *gc_page_pins;
unsigned char *gc_card_mark;

/* This is always 0 except during gc_and_save() */
lispobj lisp_init_function;

static inline bool boxed_type_p(int type) { return type > 1; }
static inline bool page_boxed_p(page_index_t page) {
    // ignore SINGLE_OBJECT_FLAG and OPEN_REGION_PAGE_FLAG
    return boxed_type_p(page_table[page].type & PAGE_TYPE_MASK);
}

/* Calculate the start address for the given page number. */
inline char *page_address(page_index_t page_num)
{
    return (void*)(DYNAMIC_SPACE_START + (page_num * GENCGC_PAGE_BYTES));
}

/* Calculate the address where the allocation region associated with
 * the page starts. */
static inline void *
page_scan_start(page_index_t page_index)
{
    return page_address(page_index)-page_scan_start_offset(page_index);
}

/* We maintain the invariant that pages with FREE_PAGE_FLAG have
 * scan_start of zero, to optimize page_ends_contiguous_block_p().
 * Particularly the 'need_zerofill' bit MUST remain as-is */
void reset_page_flags(page_index_t page) {
    page_table[page].scan_start_offset_ = 0;
#ifdef LISP_FEATURE_DARWIN_JIT
    // Whenever a page was mapped as code, it potentially needs to be remapped on the next use.
    // This avoids any affect of pthread_jit_write_protect_np when next used.
    if (page_table[page].type == PAGE_TYPE_CODE) set_page_need_to_zero(page, 1);
#endif
    page_table[page].type = 0;
    gc_page_pins[page] = 0;
}

/// External function for calling from Lisp.
page_index_t ext_find_page_index(void *addr) { return find_page_index(addr); }

/* an array of generation structures. There needs to be one more
 * generation structure than actual generations as the oldest
 * generation is temporarily raised then lowered. */
struct generation generations[NUM_GENERATIONS];

/* the oldest generation that is will currently be GCed by default.
 * Valid values are: 0, 1, ... HIGHEST_NORMAL_GENERATION
 *
 * The default of HIGHEST_NORMAL_GENERATION enables GC on all generations.
 *
 * Setting this to 0 effectively disables the generational nature of
 * the GC. In some applications generational GC may not be useful
 * because there are no long-lived objects.
 *
 * An intermediate value could be handy after moving long-lived data
 * into an older generation so an unnecessary GC of this long-lived
 * data can be avoided. */
generation_index_t gencgc_oldest_gen_to_gc = HIGHEST_NORMAL_GENERATION;

page_index_t next_free_page; // upper (exclusive) bound on used page range

#ifdef LISP_FEATURE_SB_THREAD
/* This lock is to prevent multiple threads from simultaneously
 * allocating new regions which overlap each other.  This lock must be
 * seized before all accesses to generations[] or to parts of
 * page_table[] that other threads may want to see */
#ifdef LISP_FEATURE_WIN32
static CRITICAL_SECTION free_pages_lock;
#else
static pthread_mutex_t free_pages_lock = PTHREAD_MUTEX_INITIALIZER;
#endif
#endif

void acquire_gc_page_table_lock() { ignore_value(mutex_acquire(&free_pages_lock)); }
void release_gc_page_table_lock() { ignore_value(mutex_release(&free_pages_lock)); }

extern os_vm_size_t gencgc_release_granularity;
os_vm_size_t gencgc_release_granularity = GENCGC_RELEASE_GRANULARITY;


/* Zero the memory at ADDR for LENGTH bytes, but use mmap/munmap instead
 * of zeroing it ourselves, i.e. in practice give the memory back to the
 * OS. Generally done after a large GC.
 */
static void __attribute__((unused))
zero_range_with_mmap(os_vm_address_t addr, os_vm_size_t length) {
#ifdef LISP_FEATURE_WIN32
    os_decommit_mem(addr, length);
#elif defined LISP_FEATURE_LINUX
    // We use MADV_DONTNEED only on Linux due to differing semantics from BSD.
    // Linux treats it as a demand that the memory be 0-filled, or refreshed
    // from a file that backs the range. BSD takes it as a hint that you don't
    // care if the memory has to brought in from swap when next accessed,
    // i.e. it's not a request to make a user-visible alteration to memory.
    // So in theory this can bring a page in from the core file, if we happen
    // to hit a page that resides in the portion of memory mapped by coreparse.
    // In practice this should not happen because objects from a core file can't
    // become garbage. Except in save-lisp-and-die they can, and we must be
    // cautious not to resurrect bytes that originally came from the file.
    if ((os_vm_address_t)addr >= anon_dynamic_space_start) {
        if (madvise(addr, length, MADV_DONTNEED) != 0)
            lose("madvise failed");
    } else { // See doc/internals-notes/zero-with-mmap-bug.txt
        // Trying to see how often this happens.
        // fprintf(stderr, "zero_range_with_mmap: fallback to memset()\n");
        memset(addr, 0, length);
    }
#else
    void *new_addr;
    os_deallocate(addr, length);
    new_addr = os_alloc_gc_space(DYNAMIC_CORE_SPACE_ID, NOT_MOVABLE, addr, length);
    if (new_addr == NULL || new_addr != addr) {
        lose("remap_free_pages: page moved, %p ==> %p",
             addr, new_addr);
    }
#endif
}

/* Zero the pages from START to END (inclusive). Generally done just after
 * a new region has been allocated.
 */
static inline void zero_pages(page_index_t start, page_index_t end) {
    if (start <= end)
#ifdef LISP_FEATURE_DARWIN_JIT
        zero_range_with_mmap(page_address(start), npage_bytes(1+end-start));
#else
        memset(page_address(start), 0, npage_bytes(1+end-start));
#endif
}

/* The generation currently being allocated to. */
static generation_index_t gc_alloc_generation;

/*
 * To support quick and inline allocation, regions of memory can be
 * allocated and then allocated from with just a free pointer and a
 * check against an end address.
 *
 * Since objects can be allocated to spaces with different properties
 * e.g. boxed/unboxed, generation, ages; there may need to be many
 * allocation regions.
 *
 * Each allocation region may start within a partly used page. Many
 * features of memory use are noted on a page wise basis, e.g. the
 * generation; so if a region starts within an existing allocated page
 * it must be consistent with this page.
 *
 * During the scavenging of the newspace, objects will be transported
 * into an allocation region, and pointers updated to point to this
 * allocation region. It is possible that these pointers will be
 * scavenged again before the allocation region is closed, e.g. due to
 * trans_list which jumps all over the place to cleanup the list. It
 * is important to be able to determine properties of all objects
 * pointed to when scavenging, e.g to detect pointers to the oldspace.
 * Thus it's important that the allocation regions have the correct
 * properties set when allocated, and not just set when closed. The
 * region allocation routines return regions with the specified
 * properties, and grab all the pages, setting their properties
 * appropriately, except that the amount used is not known.
 *
 * These regions are used to support quicker allocation using just a
 * free pointer. The actual space used by the region is not reflected
 * in the pages tables until it is closed. It can't be scavenged until
 * closed.
 *
 * When finished with the region it should be closed, which will
 * update the page tables for the actual space used returning unused
 * space. Further it may be noted in the new regions which is
 * necessary when scavenging the newspace.
 *
 * Large objects may be allocated directly without an allocation
 * region, the page table is updated immediately.
 *
 * Unboxed objects don't contain pointers to other objects and so
 * don't need scavenging. Further they can't contain pointers to
 * younger generations so WP is not needed. By allocating pages to
 * unboxed objects the whole page never needs scavenging or
 * write-protecting. */

/* We use five regions for the current newspace generation. */
struct alloc_region gc_alloc_region[6];

static page_index_t
  alloc_start_pages[8], // one for each value of PAGE_TYPE_x
  max_alloc_start_page; // the largest of any array element
page_index_t gencgc_alloc_start_page; // initializer for the preceding array

/* Each 'start_page' informs the region-opening logic where it should
 * attempt to continue allocating after closing a region associated
 * with a particular page type. We aren't very clever about this -
 * either the start_page has space remaining or it doesn't, and when it
 * doesn't, then we should hop over *all* allocated pages regardless of
 * type that intercede between the page we couldn't use up to next_free_page.
 * It's kind of dumb that there is one start_page per type,
 * other than it serves its purpose for picking up where it left off
 * on a partially full page during GC */
#define RESET_ALLOC_START_PAGES() \
        alloc_start_pages[0] = gencgc_alloc_start_page; \
        alloc_start_pages[1] = gencgc_alloc_start_page; \
        alloc_start_pages[2] = gencgc_alloc_start_page; \
        alloc_start_pages[3] = gencgc_alloc_start_page; \
        alloc_start_pages[4] = gencgc_alloc_start_page; \
        alloc_start_pages[5] = gencgc_alloc_start_page; \
        alloc_start_pages[6] = gencgc_alloc_start_page; \
        alloc_start_pages[7] = gencgc_alloc_start_page; \
        max_alloc_start_page = gencgc_alloc_start_page;

static page_index_t
get_alloc_start_page(unsigned int page_type)
{
    if (page_type > 7) lose("bad page_type: %d", page_type);
    struct thread* th = get_sb_vm_thread();
    page_index_t global_start = alloc_start_pages[page_type];
    page_index_t hint;
    switch (page_type) {
    case PAGE_TYPE_MIXED:
        if ((hint = thread_extra_data(th)->mixed_page_hint) > 0 && hint <= global_start) {
            thread_extra_data(th)->mixed_page_hint = - 1;
            return hint;
        }
        break;
    case PAGE_TYPE_CONS:
        if ((hint = thread_extra_data(th)->cons_page_hint) > 0 && hint <= global_start) {
            thread_extra_data(th)->cons_page_hint = - 1;
            return hint;
        }
        break;
    }
    return global_start;
}

static inline void
set_alloc_start_page(unsigned int page_type, page_index_t page)
{
    if (page_type > 7) lose("bad page_type: %d", page_type);
    if (page > max_alloc_start_page) max_alloc_start_page = page;
    alloc_start_pages[page_type] = page;
}
#include "private-cons.inc"

void gc_close_region(struct alloc_region *alloc_region,
                     __attribute__((unused)) int page_type)
{
    mr_update_closed_region(alloc_region, gc_alloc_generation);
}

/* Allocate bytes.  The fast path of gc_general_alloc() calls this
 * when it can't fit in the open region.
 * This entry point is only for use within the GC itself. */
void *collector_alloc_fallback(struct alloc_region* region, sword_t nbytes, int page_type) {
    page_index_t alloc_start = get_alloc_start_page(page_type);
    void *new_obj;
    if ((uword_t)nbytes >= (GENCGC_PAGE_BYTES / 4 * 3)) {
        uword_t largest_hole;
        page_index_t new_page = try_allocate_large(nbytes, page_type, gc_alloc_generation,
                                                   &alloc_start, page_table_pages, &largest_hole);
        if (new_page == -1) gc_heap_exhausted_error_or_lose(largest_hole, nbytes);
        new_obj = page_address(new_page);
        set_allocation_bit_mark(new_obj);
    } else {
        ensure_region_closed(region, page_type);
        bool success =
            try_allocate_small_from_pages(nbytes, region, page_type,
                                          gc_alloc_generation,
                                          &alloc_start, page_table_pages);
        if (!success) gc_heap_exhausted_error_or_lose(0, nbytes);
        new_obj = region->start_addr;
    }
    set_alloc_start_page(page_type, alloc_start);
    return new_obj;
}

/* "Copy" a large object. If the object is on large object pages,
 * and satisifies the condition to remain where it is,
 * it is simply promoted, else it is copied.
 * To stay on large-object pages, the object must either be at least
 * LARGE_OBJECT_SIZE, or must waste fewer than about 1% of the space
 * on its allocated pages. Using 32k pages as a reference point:
 *   3 pages - ok if size >= 97552
 *   2 pages - ...   size >= 65040
 *   1 page  - ...   size >= 32528
 *
 * Bignums and vectors may have shrunk. If the object is not copied,
 * the slack needs to be reclaimed, and the page_tables corrected.
 *
 * Code objects can't shrink, but it's not worth adding an extra test
 * for large code just to avoid the loop that performs adjustment, so
 * go through the adjustment motions even though nothing happens.
 *
 */
lispobj
copy_potential_large_object(lispobj object, sword_t nwords,
                           struct alloc_region* region, int page_type)
{
    page_index_t first_page;

    CHECK_COPY_PRECONDITIONS(object, nwords);

    /* Check whether it's a large object. */
    first_page = find_page_index((void *)object);
    gc_dcheck(first_page >= 0);

    os_vm_size_t nbytes = nwords * N_WORD_BYTES;
    os_vm_size_t rounded = ALIGN_UP(nbytes, GENCGC_PAGE_BYTES);
    if (page_single_obj_p(first_page) &&
        (nbytes >= LARGE_OBJECT_SIZE || (rounded - nbytes < rounded / 128))) {

        // Large BOXED would serve no purpose beyond MIXED, and "small large" is illogical.
        if (page_type == PAGE_TYPE_BOXED || page_type == PAGE_TYPE_SMALL_MIXED)
            page_type = PAGE_TYPE_MIXED;
        /* was: adjust_obj_ptes(first_page, nwords, new_space, SINGLE_OBJECT_FLAG | page_type); */
        os_vm_size_t bytes_freed = 0;

        generations[from_space].bytes_allocated -= (bytes_freed + nbytes);
        generations[new_space].bytes_allocated += nbytes;
        bytes_allocated -= bytes_freed;

        /* Add the region to the new_areas if requested. */
        gc_in_situ_live_nwords += nbytes>>WORD_SHIFT;
        /* UNREACHABLE? */
        /* if (boxed_type_p(page_type)) add_new_area(first_page, 0, nbytes); */

        return object;
    }
    return gc_copy_object(object, nwords, region, page_type);
}

/* to copy unboxed objects */
lispobj
copy_unboxed_object(lispobj object, sword_t nwords)
{
    return gc_copy_object(object, nwords, unboxed_region, PAGE_TYPE_UNBOXED);
}

/* This WILL NOT reliably work for objects in a currently open allocation region,
 * because page_words_used() is not sync'ed to the free pointer until closing.
 * However it should work reliably for codeblobs, because if you can hold
 * a reference to the codeblob, then either you'll find it in the generation 0
 * tree, or else can linearly scan for it in an older generation */
static lispobj dynspace_codeblob_tree_snapshot; // valid only during GC

int pin_all_dynamic_space_code;

static inline __attribute__((unused))
int lowtag_ok_for_page_type(__attribute__((unused)) lispobj ptr,
                                          __attribute__((unused)) int page_type) {
    // If the young generation goes to mixed-region, this filter is not valid
#ifdef LISP_FEATURE_USE_CONS_REGION
    // This doesn't currently decide on acceptability for code/non-code
    if (lowtag_of(ptr) == LIST_POINTER_LOWTAG) {
        if (page_type != PAGE_TYPE_CONS) return 0;
    } else {
        if (page_type == PAGE_TYPE_CONS) return 0;
    }
#endif
    return 1;
}

int sb_introspect_pinnedp(__attribute__((unused)) lispobj obj) { return 0; }

static void preserve_pointer(os_context_register_t object,
                             __attribute__((unused)) void* arg) {
    /* The mark-region GC never filters based on type tags,
     * so it already covers the special case of untagged instance
     * pointers in registers. */
    mr_preserve_ambiguous(object);
}

/* Additional logic for soft marks: any word that is potentially a
 * tagged pointer to a page being written must preserve the mark regardless
 * of what update_writeprotection() thinks. That's because the mark is set
 * prior to storing. If GC occurs in between setting the mark and storing,
 * then resetting the mark would be wrong if the subsequent store
 * creates an old->young pointer.
 * Mark stickiness is checked only once per invocation of collect_garbage(),
 * so it when scanning interrupt contexts for generation 0 but not higher gens.
 * Also note the two scenarios:
 * (1) tagged pointer to a large simple-vector, but we scan card-by-card
 *     for specifically the marked cards.  This has to be checked first
 *     so as not to fail to see subsequent cards if the first is marked.
 * (2) tagged pointer to an object that marks only the page containing
 *     the object base.
 * And note a subtle point: only an already-marked card can acquire stick
 * status. So we can ignore any unmarked (a/k/a WRITEPROTECTED_P) card
 * regardless of a context register pointing to it, because if a mark was not
 * stored, then the pointer was not stored. Without examining the next few
 * instructions, there's no reason even to suppose that a store occurs.
 * It seems like the stop-for-GC handler must be enforcing that GC sees things
 * stored in the correct order for out-of-order memory models */
// registers can be wider than words. This could accept uword_t as the arg type
// but I like it to be directly callable with os_context_register.
static void sticky_preserve_pointer(os_context_register_t register_word, void* arg)
{
    uword_t word = register_word;
    if (is_lisp_pointer(word)) {
        page_index_t page = find_page_index((void*)word);
        if (page >= 0 && page_boxed_p(page) // stores to raw bytes are uninteresting
            && lowtag_ok_for_page_type(word, page_table[page].type)
            && plausible_tag_p(word)) { // "plausible" is good enough
            if (lowtag_of(word) == OTHER_POINTER_LOWTAG &&
                widetag_of(native_pointer(word)) == SIMPLE_VECTOR_WIDETAG) {
                /* if 'word' is the correctly-tagged pointer to the base of a SIMPLE-VECTOR,
                 * then set the sticky mark on every marked page. The only other large
                 * objects are CODE (writes to which are pseudo-atomic),
                 * and BIGNUM (which aren't on boxed pages) */
                generation_index_t gen = page_table[page].gen;
                while (1) {
                    long card = page_to_card_index(page);
                    int i;
                    for(i=0; i<CARDS_PER_PAGE; ++i)
                        if (gc_card_mark[card+i]==CARD_MARKED) gc_card_mark[card+i]=STICKY_MARK;
                    if (page_ends_contiguous_block_p(page, gen)) break;
                    ++page;
                }
            } else if (gc_card_mark[addr_to_card_index((void*)word)] == CARD_MARKED) {
                gc_card_mark[addr_to_card_index((void*)word)] = STICKY_MARK;
            }
        }
    }
    preserve_pointer(word, arg);
}

#define pin_exact_root(r) mr_preserve_object(r)


#define WORDS_PER_CARD (GENCGC_CARD_BYTES/N_WORD_BYTES)

void gc_close_collector_regions(int flag)
{
    ensure_region_closed(code_region, flag|PAGE_TYPE_CODE);
    ensure_region_closed(boxed_region, PAGE_TYPE_BOXED);
    ensure_region_closed(unboxed_region, PAGE_TYPE_UNBOXED);
    ensure_region_closed(mixed_region, PAGE_TYPE_MIXED);
    ensure_region_closed(small_mixed_region, PAGE_TYPE_SMALL_MIXED);
    ensure_region_closed(cons_region, PAGE_TYPE_CONS);
}


/* Un-write-protect all the pages in from_space. */
static void
unprotect_oldspace(void)
{
    page_index_t i;

    /* Gen0 never has protection applied, so we can usually skip the un-protect step,
     * however, in the final GC, because everything got moved to gen0 by brute force
     * adjustment of the page table, we don't know the state of the protection.
     * Therefore only skip out if NOT in the final GC */
    if (conservative_stack && from_space == 0) return;

    for (i = 0; i < next_free_page; i++) {
        /* Why does this even matter? Obviously it did for physical protection
         * (storing the forwarding pointers shouldn't fault)
         * but there's no physical protection, so ... why bother?
         * But I tried removing it and got assertion failures */
        if (page_words_used(i) && page_table[i].gen == from_space)
            assign_page_card_marks(i, CARD_MARKED);
    }
}


/* Call 'proc' with pairs of addresses demarcating ranges in the
 * specified generation.
 * Stop if any invocation returns non-zero, and return that value */
uword_t
walk_generation(uword_t (*proc)(lispobj*,lispobj*,uword_t),
                generation_index_t generation, uword_t extra)
{
    page_index_t i;
    int genmask = generation >= 0 ? 1 << generation : ~0;

    for (i = 0; i < next_free_page; i++) {
        if ((page_words_used(i) != 0) && ((1 << page_table[i].gen) & genmask)) {
            page_index_t last_page;

            /* This should be the start of a contiguous block */
            /* Why oh why does genesis seem to make a page table
            * that trips this assertion? */
            gc_assert(page_starts_contiguous_block_p(i));

            /* Need to find the full extent of this contiguous block in case
               objects span pages. */

            /* Now work forward until the end of this contiguous area is
               found. */
            for (last_page = i; ;last_page++)
                /* Check whether this is the last page in this contiguous
                 * block. */
                if (page_ends_contiguous_block_p(last_page, page_table[i].gen))
                    break;

            uword_t result =
                proc((lispobj*)page_address(i),
                     (lispobj*)page_limit(last_page),
                     extra);
            if (result) return result;

            i = last_page;
        }
    }
    return 0;
}

extern void visit_context_registers(void (*proc)(os_context_register_t, void*),
                                    os_context_t *context, void*);
static void NO_SANITIZE_ADDRESS NO_SANITIZE_MEMORY
conservative_stack_scan(struct thread* th,
                        __attribute__((unused)) generation_index_t gen,
                        // #+sb-safepoint uses os_get_csp() and not this arg
                        __attribute__((unused)) lispobj* cur_thread_approx_stackptr)
{
    /* there are potentially two stacks for each thread: the main
     * stack, which may contain Lisp pointers, and the alternate stack.
     * We don't ever run Lisp code on the altstack, but it may
     * host a sigcontext with lisp objects in it.
     * Actually, STOP_FOR_GC has a signal context on the main stack,
     * and the values it in will be *above* the stack-pointer in it
     * at the point of interruption, so we would not scan all registers
     * unless the context is scanned.
     *
     * For the thread which initiates GC there will usually not be a
     * sigcontext, though there could, in theory be if it performs
     * GC while handling an interruption */

    void (*context_method)(os_context_register_t,void*) =
        gen == 0 ? sticky_preserve_pointer : preserve_pointer;

    void* esp = (void*)-1;
# if defined(LISP_FEATURE_SB_SAFEPOINT)
    /* Conservative collect_garbage is always invoked with a
     * foreign C call or an interrupt handler on top of every
     * existing thread, so the stored SP in each thread
     * structure is valid, no matter which thread we are looking
     * at.  For threads that were running Lisp code, the pitstop
     * and edge functions maintain this value within the
     * interrupt or exception handler. */
    esp = os_get_csp(th);
    assert_on_stack(th, esp);

    /* And on platforms with interrupts: scavenge ctx registers. */

    /* Disabled on Windows, because it does not have an explicit
     * stack of `interrupt_contexts'.  The reported CSP has been
     * chosen so that the current context on the stack is
     * covered by the stack scan.  See also set_csp_from_context(). */
#  ifndef LISP_FEATURE_WIN32
    if (th != get_sb_vm_thread()) {
        int k = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th));
        while (k > 0) {
            os_context_t* context = nth_interrupt_context(--k, th);
            if (context)
                visit_context_registers(context_method, context, (void*)1);
        }
    }
#  endif
# elif defined(LISP_FEATURE_SB_THREAD)
    int i;
    for (i = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th))-1; i>=0; i--) {
        os_context_t *c = nth_interrupt_context(i, th);
        visit_context_registers(context_method, c, (void*)1);
        lispobj* esp1 = (lispobj*) *os_context_register_addr(c,reg_SP);
        if (esp1 >= th->control_stack_start && esp1 < th->control_stack_end && (void*)esp1 < esp)
            esp = esp1;
    }
    if (th == get_sb_vm_thread()) {
        if ((void*)cur_thread_approx_stackptr < esp) esp = cur_thread_approx_stackptr;
    }
# else
    esp = cur_thread_approx_stackptr;
# endif
    if (!esp || esp == (void*) -1)
        UNKNOWN_STACK_POINTER_ERROR("garbage_collect", th);

    // Words on the stack which point into the stack are likely
    // frame pointers or alien or DX object pointers. In any case
    // there's no need to call preserve_pointer on them since
    // they definitely don't point to the heap.
    // See the picture at alloc_thread_struct() as a reminder.
#ifdef LISP_FEATURE_UNIX
    lispobj exclude_from = (lispobj)th->control_stack_start;
    lispobj exclude_to = (lispobj)th + dynamic_values_bytes;
#define potential_heap_pointer(word) !(exclude_from <= word && word < exclude_to)
#else
    // We can't use the heuristic of excluding words that appear to point into
    // 'struct thread' on win32 because ... I don't know why.
    // See https://groups.google.com/g/sbcl-devel/c/8s7mrapq56s/m/UaAjYPqKBAAJ
#define potential_heap_pointer(word) 1
#endif

    lispobj* ptr;
    for (ptr = esp; ptr < th->control_stack_end; ptr++) {
        lispobj word = *ptr;
        // Also note that we can eliminate small fixnums from consideration
        // since there is no memory on the 0th page.
        // (most OSes don't let users map memory there, though they used to).
        if (word >= BACKEND_PAGE_BYTES && potential_heap_pointer(word)) {
          preserve_pointer(word, 0);
        }
    }
}

static void scan_explicit_pins(__attribute__((unused)) struct thread* th)
{
    lispobj pin_list = read_TLS(PINNED_OBJECTS, th);
    for ( ; pin_list != NIL ; pin_list = CONS(pin_list)->cdr ) {
        lispobj object = CONS(pin_list)->car;
        pin_exact_root(object);
        if (lowtag_of(object) == INSTANCE_POINTER_LOWTAG) {
            struct instance* instance = INSTANCE(object);
            lispobj layout = instance_layout((lispobj*)instance);
            // Since we're still in the pinning phase of GC, layouts can't have moved yet,
            // so there is no forwarding check needed here.
            if (layout && lockfree_list_node_layout_p(LAYOUT(layout))) {
                /* A logically-deleted explicitly-pinned lockfree list node pins its
                 * successor too, since Lisp reconstructs the next node's tagged pointer
                 * from an untagged pointer currently stored in %NEXT of this node. */
                lispobj successor = ((struct list_node*)instance)->_node_next;
                // Be sure to ignore an uninitialized word containing 0.
                if (successor && fixnump(successor))
                    pin_exact_root(successor | INSTANCE_POINTER_LOWTAG);
            }
        }
    }
}

/* Given the slightly asymmetric formulation of page_ends_contiguous_block_p()
 * you might think that it could cause the next page's assertion about start_block_p()
 * to fail, but it does not seem to. That's really weird! */
__attribute__((unused)) static void check_contiguity()
{
      page_index_t first = 0;
      while (first < next_free_page) {
        if (!page_words_used(first)) { ++first; continue; }
        gc_assert(page_starts_contiguous_block_p(first));
        page_index_t last = first;
        while (!page_ends_contiguous_block_p(last, page_table[first].gen)) ++last;
        first = last + 1;
      }
}

#define PAGE_PINNED 0xFF

/* Garbage collect a generation. If raise is 0 then the remains of the
 * generation are not raised to the next generation. */
void NO_SANITIZE_ADDRESS NO_SANITIZE_MEMORY
garbage_collect_generation(generation_index_t generation, int raise,
                           void* cur_thread_approx_stackptr)
{
    struct thread *th;

    if (gencgc_verbose > 2) fprintf(stderr, "BEGIN gc_gen(%d,%d)\n", generation, raise);

#ifdef COLLECT_GC_STATS
    struct timespec t0;
    clock_gettime(CLOCK_MONOTONIC, &t0);
    uword_t gen_usage_at_start = generations[generation].bytes_allocated;
    uword_t higher_gen_usage_at_start =
      raise ? generations[generation+1].bytes_allocated : 0;
#endif

    gc_assert(generation <= PSEUDO_STATIC_GENERATION);

    /* The oldest generation can't be raised. */
    gc_assert(!raise || generation < HIGHEST_NORMAL_GENERATION);

    /* Check that weak hash tables were processed in the previous GC. */
    gc_assert(weak_hash_tables == NULL);

    /* When a generation is not being raised it is transported to a
     * temporary generation (NUM_GENERATIONS), and lowered when
     * done. Set up this new generation. There should be no pages
     * allocated to it yet. */
    if (!raise) {
         gc_assert(generations[SCRATCH_GENERATION].bytes_allocated == 0);
    }

#ifdef LISP_FEATURE_SB_THREAD
    pin_all_dynamic_space_code = 0;
    for_each_thread(th) {
        if (th->state_word.state != STATE_DEAD && \
            (read_TLS(GC_PIN_CODE_PAGES, th) & make_fixnum(1))) {
            pin_all_dynamic_space_code = 1;
            break;
        }
    }
#else
    pin_all_dynamic_space_code = read_TLS(GC_PIN_CODE_PAGES, 0) & make_fixnum(1);
#endif

    /* Set the global src and dest. generations */
    generation_index_t original_alloc_generation = gc_alloc_generation;

    from_space = generation;
    new_space = generation;

    /* Change to a new space for allocation, resetting the alloc_start_page */
        gc_alloc_generation = new_space;
        RESET_ALLOC_START_PAGES();

        /* Don't try to allocate into pseudo-static, when we collect it */
        if (generation == PSEUDO_STATIC_GENERATION)
          gc_alloc_generation = 0;
        mr_pre_gc(generation);

        if (pin_all_dynamic_space_code) {
          /* This needs to happen before ambiguous root pinning, as the mechanisms
           * overlap in a way that all-code pinning wouldn't do the right thing if flipped.
           * FIXME: why would it not? More explanation needed!
           * Code objects should never get into the pins table in this case */
            page_index_t i;
            for (i = 0; i < next_free_page; i++) {
                if (page_table[i].gen == from_space
                    && is_code(page_table[i].type) && page_words_used(i))
                    gc_page_pins[i] = PAGE_PINNED;
            }
        }

    /* Un-write-protect the old-space pages. This is essential for the
     * promoted pages as they may contain pointers into the old-space
     * which need to be scavenged. It also helps avoid unnecessary page
     * faults as forwarding pointers are written into them. They need to
     * be un-protected anyway before unmapping later. */
        unprotect_oldspace();


    /* Possibly pin stack roots and/or *PINNED-OBJECTS*, unless saving a core.
     * Scavenging (fixing up pointers) will occur later on */

    if (conservative_stack) {
        for_each_thread(th) {
            if (th->state_word.state == STATE_DEAD) continue;
            scan_explicit_pins(th);
#if !GENCGC_IS_PRECISE
            /* Pin everything in fromspace with a stack root, and also set the
             * sticky card mark on any page (in any generation)
             * referenced from the stack. */
            conservative_stack_scan(th, generation, cur_thread_approx_stackptr);
#elif defined LISP_FEATURE_MIPS || defined LISP_FEATURE_PPC64
            // Pin code if needed
            semiconservative_pin_stack(th, generation);
#elif defined REG_RA
            conservative_pin_code_from_return_addresses(th);
#elif !defined(reg_CODE)
            pin_call_chain_and_boxed_registers(th);
#endif
        }
    }

    // Thread creation optionally no longer synchronizes the creating and
    // created thread. When synchronized, the parent thread is responsible
    // for pinning the start function for handoff to the created thread.
    // When not synchronized, The startup parameters are pinned via this list
    // which will always be NIL if the feature is not enabled.

    // I think this can be removed. From a liveness perspective *STARTING-THREADS*
    // preserves the SB-THREAD:THREAD instance and its startup function,
    // neither of which will move.

#if 0 // was: ifdef STARTING_THREADS
    lispobj pin_list = SYMBOL(STARTING_THREADS)->value;
    for ( ; pin_list != NIL ; pin_list = CONS(pin_list)->cdr ) {
        lispobj thing = CONS(pin_list)->car;
        // It might be tempting to say that only the SB-THREAD:THREAD instance
        // requires pinning - because right after we access it to extract the
        // primitive thread, we link into all_threads - but it may be that the code
        // emitted by the C compiler in new_thread_trampoline computes untagged pointers
        // when accessing the vector and the start function, so those would not be
        // seen as valid lisp pointers by the implicit pinning logic.
        // And the precisely GC'd platforms would not pin anything from C code.
        // The tests in 'threads.impure.lisp' are good at detecting omissions here.
        if (thing) { // Nothing to worry about when 'thing' is already smashed
            gc_assert(instancep(thing));
            struct thread_instance *lispthread = (void*)(thing - INSTANCE_POINTER_LOWTAG);
            lispobj info = lispthread->startup_info;
            // INFO gets set to a fixnum when the thread is exiting. I *think* it won't
            // ever be seen in the starting-threads list, but let's be cautious.
            if (is_lisp_pointer(info)) {
                gc_assert(simple_vector_p(info));
                gc_assert(vector_len(VECTOR(info)) >= 1);
                lispobj fun = VECTOR(info)->data[0];
                gc_assert(functionp(fun));
#ifdef LISP_FEATURE_X86_64
                /* FIXME: re. the following remark that pin_exact_root() "does not
                 * work", does it have to be that way? It seems the issue is that
                 * pin_exact_root does absolutely nothing for objects in immobile space.
                 * Are there other objects we call it on which could be in immobile-space
                 * and should it be made to deal with them? */
                // slight KLUDGE: 'fun' is a simple-fun in immobile-space,
                // and pin_exact_root() doesn't work. In all probability 'fun'
                // is pseudo-static, but let's use the right pinning function.
                // (This line of code is so rarely executed that it doesn't
                // impact performance to search for the object)
                preserve_pointer(fun, 0);
#else
                pin_exact_root(fun);
#endif
                // pin_exact_root is more efficient than preserve_pointer()
                // because it does not search for the object.
                pin_exact_root(thing);
                pin_exact_root(info);
            }
        }
    }
#endif


    /* Scavenge all the rest of the roots. */

#if GENCGC_IS_PRECISE
    /*
     * If not x86, we need to scavenge the interrupt context(s) and the
     * control stack, unless in final GC then don't.
     */
    if (conservative_stack) {
        struct thread *th;
        for_each_thread(th) {
#if !defined(LISP_FEATURE_MIPS) && defined(reg_CODE) // interrupt contexts already pinned everything they see
            scavenge_interrupt_contexts(th);
#endif
            scavenge_control_stack(th);
        }

# ifdef LISP_FEATURE_SB_SAFEPOINT
        /* In this case, scrub all stacks right here from the GCing thread
         * instead of doing what the comment below says.  Suboptimal, but
         * easier. */
        for_each_thread(th)
            scrub_thread_control_stack(th);
# else
        /* Scrub the unscavenged control stack space, so that we can't run
         * into any stale pointers in a later GC (this is done by the
         * stop-for-gc handler in the other threads). */
        scrub_control_stack();
# endif
    }
#endif

    /* Scavenge the Lisp functions of the interrupt handlers */
    if (GC_LOGGING) fprintf(gc_activitylog(), "begin scavenge sighandlers\n");
    mr_preserve_range(lisp_sig_handlers, NSIG);

    /* Scavenge the binding stacks. */
    if (GC_LOGGING) fprintf(gc_activitylog(), "begin scavenge thread roots\n");
    {
        struct thread *th;
        for_each_thread(th) {
            scav_binding_stack((lispobj*)th->binding_stack_start,
                               (lispobj*)get_binding_stack_pointer(th),
                               mr_preserve_ambiguous);
            /* do the tls as well */
            lispobj* from = &th->lisp_thread;
            lispobj* to = (lispobj*)(SymbolValue(FREE_TLS_INDEX,0) + (char*)th);
            sword_t nwords = to - from;
            mr_preserve_range(from, nwords);
        }
    }

    mr_collect_garbage(raise);
    RESET_ALLOC_START_PAGES();
    struct generation* g = &generations[generation];
    /* Set the new gc trigger for the GCed generation. */
    g->gc_trigger = g->bytes_allocated + g->bytes_consed_between_gc;
    g->num_gc = raise ? 0 : (1 + g->num_gc);

    // Have to kill this structure from its root, because any of the nodes would have
    // been on pages that got freed by free_oldspace.
    dynspace_codeblob_tree_snapshot = 0;
    if (generation >= verify_gens)
        verify_heap(cur_thread_approx_stackptr, VERIFY_POST_GC | (generation<<16));

    extern int n_unboxed_instances;
    n_unboxed_instances = 0;
    gc_alloc_generation = original_alloc_generation;
}

static page_index_t
find_next_free_page(void)
{
    page_index_t last_page = -1, i;

    for (i = 0; i < next_free_page; i++)
        if (page_words_used(i) != 0)
            last_page = i;

    /* 1 page beyond the last used page is the next free page */
    return last_page + 1;
}

/*
 * Supposing the OS can only operate on ranges of a certain granularity
 * (which we call 'gencgc_release_granularity'), then given any page rage,
 * align the lower bound up and the upper down to match the granularity.
 *
 *     |-->| OS page | OS page |<--|
 *
 * If the interior of the aligned range is nonempty,
 * perform three operations: unmap/remap, fill before, fill after.
 * Otherwise, just one operation to fill the whole range.
 */
static void
remap_page_range (page_index_t from, page_index_t to)
{
    /* There's a mysterious Solaris/x86 problem with using mmap
     * tricks for memory zeroing. See sbcl-devel thread
     * "Re: patch: standalone executable redux".
     */
    /* I have no idea what the issue with Haiku is, but using the simpler
     * zero_pages() works where the unmap,map technique does not. Yet the
     * trick plus a post-check that the pages were correctly zeroed finds
     * no problem at that time. So what's failing later and why??? */
#if defined LISP_FEATURE_SUNOS || defined LISP_FEATURE_HAIKU
    zero_pages(from, to);
#else
    size_t granularity = gencgc_release_granularity;
    // page_address "works" even if 'to' == page_table_pages-1
    char* start = page_address(from);
    char* end   = page_address(to+1);
    char* aligned_start = PTR_ALIGN_UP(start, granularity);
    char* aligned_end   = PTR_ALIGN_DOWN(end, granularity);

    /* NOTE: this is largely pointless because gencgc-release-granularity
     * is everywhere defined to be EXACTLY +backend-page-bytes+
     * which by definition is the quantum at which we'll unmap/map.
     * Maybe we should remove the needless complexity? */
    if (aligned_start < aligned_end) {
        zero_range_with_mmap(aligned_start, aligned_end-aligned_start);
        memset(start, 0, aligned_start - start);
        memset(aligned_end, 0, end - aligned_end);
    } else {
        zero_pages(from, to);
    }
#endif
    page_index_t i;
    for (i = from; i <= to; i++) set_page_need_to_zero(i, 0);
}

static void
remap_free_pages (page_index_t from, page_index_t to)
{
    page_index_t first_page, last_page;

    for (first_page = from; first_page <= to; first_page++) {
        if (!page_free_p(first_page) || !page_need_to_zero(first_page))
            continue;

        last_page = first_page + 1;
        while (page_free_p(last_page) &&
               (last_page <= to) &&
               (page_need_to_zero(last_page)))
            last_page++;

        remap_page_range(first_page, last_page-1);

        first_page = last_page;
    }
}

generation_index_t small_generation_limit = 1;

extern int finalizer_thread_runflag;

/* GC all generations newer than last_gen, raising the objects in each
 * to the next older generation - we finish when all generations below
 * last_gen are empty.  Then if last_gen is due for a GC, or if
 * last_gen==NUM_GENERATIONS (the scratch generation?  eh?) we GC that
 * too.  The valid range for last_gen is: 0,1,...,NUM_GENERATIONS.
 *
 * We stop collecting at gencgc_oldest_gen_to_gc, even if this is less than
 * last_gen (oh, and note that by default it is NUM_GENERATIONS-1) */
long tot_gc_nsec;
void NO_SANITIZE_ADDRESS NO_SANITIZE_MEMORY
collect_garbage(generation_index_t last_gen)
{
    ++n_gcs;
    THREAD_JIT(0);
    generation_index_t gen = 0;
    bool gc_mark_only = 0;
    int raise, more = 0;
    /* The largest value of next_free_page seen since the time
     * remap_free_pages was called. */
    static page_index_t high_water_mark = 0;

#ifdef COLLECT_GC_STATS
    struct timespec t_gc_start;
    clock_gettime(CLOCK_MONOTONIC, &t_gc_start);
#endif
    log_generation_stats(gc_logfile, "=== GC Start ===");

    gc_active_p = 1;

    if (last_gen == 1+PSEUDO_STATIC_GENERATION) {
        // Pseudostatic space undergoes a non-moving collection
        last_gen = PSEUDO_STATIC_GENERATION;
        gc_mark_only = 1;
    } else if (last_gen > 1+PSEUDO_STATIC_GENERATION) {
        // This is a completely non-obvious thing to do, but whatever...
        last_gen = 0;
    }

    /* Flush the alloc regions updating the page table.
     *
     * GC is single-threaded and all memory allocations during a collection
     * happen in the GC thread, so it is sufficient to update PTEs for the
     * per-thread regions exactly once at the beginning of a collection
     * and update only from the GC's regions thereafter during collection.
     *
     * The GC's regions are probably empty already, except:
     * - The code region is shared across all threads
     * - The boxed region is used in lieu of thread-specific regions
     *   in a unithread build.
     * So we need to close them for those two cases.
     */
    struct thread *th;
    for_each_thread(th) gc_close_thread_regions(th, 0);
    ensure_region_closed(code_region, PAGE_TYPE_CODE);
    if (gencgc_verbose > 2) fprintf(stderr, "[%d] BEGIN gc(%d)\n", n_gcs, last_gen);

#ifdef LISP_FEATURE_IMMOBILE_SPACE
  if (ENABLE_PAGE_PROTECTION) {
      // Unprotect the in-use ranges. Any page could be written during scavenge
      os_protect((os_vm_address_t)FIXEDOBJ_SPACE_START,
                 (lispobj)fixedobj_free_pointer - FIXEDOBJ_SPACE_START,
                 OS_VM_PROT_ALL);
  }
#endif

    lispobj* cur_thread_approx_stackptr =
        (lispobj*)ALIGN_DOWN((uword_t)&last_gen, N_WORD_BYTES);
    /* Verify the new objects created by Lisp code. */
    if (pre_verify_gen_0)
        verify_heap(cur_thread_approx_stackptr, VERIFY_PRE_GC);

    if (gencgc_verbose > 1) {
        fprintf(stderr, "Pre-GC:\n");
        print_generation_stats();
    }

    /* After a GC, pages of code are safe to linearly scan because
     * there won't be random junk on them below page_bytes_used.
     * But generation 0 pages are _not_ safe to linearly scan because they aren't
     * pre-zeroed. The SIGPROF handler could have a bad time if were to misread
     * the header of an object mid-creation. Therefore, codeblobs newly made by Lisp
     * are kept in a lock-free and threadsafe datastructure. But we don't want to
     * enliven nodes of that structure for Lisp to see (absent any other references)
     * because the whole thing becomes garbage after this GC. So capture the tree
     * for GC's benefit, and delete the view of it from Lisp.
     * Incidentally, immobile text pages have their own tree, for other purposes
     * (among them being to find page scan start offsets) which is pruned as
     * needed by a finalizer. */
    dynspace_codeblob_tree_snapshot = SYMBOL(DYNSPACE_CODEBLOB_TREE)->value;
    SYMBOL(DYNSPACE_CODEBLOB_TREE)->value = NIL;

    page_index_t initial_nfp = next_free_page;
    if (gc_mark_only) {
        garbage_collect_generation(PSEUDO_STATIC_GENERATION, 0,
                                   cur_thread_approx_stackptr);
        goto finish;
    }

    do {
        /* Collect the generation. */

        if (more || (gen >= gencgc_oldest_gen_to_gc)) {
            /* Never raise the oldest generation. Never raise the extra generation
             * collected due to more-flag. */
            raise = 0;
            more = 0;
        } else {
            raise =
                (gen < last_gen)
                || (generations[gen].num_gc >= generations[gen].number_of_gcs_before_promotion);
            /* If we would not normally raise this one, but we're
             * running low on space in comparison to the object-sizes
             * we've been seeing, raise it and collect the next one
             * too. */
            if (!raise && gen == last_gen) {
                more = (2*large_allocation) >= (dynamic_space_size - bytes_allocated);
                raise = more;
            }
        }
        /* Collect more aggressively if we're running low on space. */
        bool panic = 0;
        if (!raise &&
            (float)bytes_allocated / (float)dynamic_space_size > PANIC_THRESHOLD &&
            gen < gencgc_oldest_gen_to_gc) {
          raise = 1;
          more = 1;
          panic = 1;
        }

        /* If an older generation is being filled, then update its
         * memory age. */
        if (raise == 1) {
            generations[gen+1].cum_sum_bytes_allocated +=
                generations[gen+1].bytes_allocated;
        }

        garbage_collect_generation(gen, raise, cur_thread_approx_stackptr);

        /* Don't keep panicking if we freed enough now. */
        if (panic && (float)bytes_allocated / (float)dynamic_space_size < PANIC_THRESHOLD) {
          more = 0;
        }

        /* Reset the memory age cum_sum. */
        generations[gen].cum_sum_bytes_allocated = 0;

        if (gencgc_verbose > 1) {
            fprintf(stderr, "Post-GC(gen=%d):\n", gen);
            print_generation_stats();
        }

        gen++;
    } while ((gen <= gencgc_oldest_gen_to_gc)
             && ((gen < last_gen)
                 || more
                 || (raise
                     && (generations[gen].bytes_allocated
                         > generations[gen].gc_trigger)
                     && (generation_average_age(gen)
                         > generations[gen].minimum_age_before_gc))));

#ifdef LISP_FEATURE_SOFT_CARD_MARKS
    {
    // Turn sticky cards marks to the regular mark.
    page_index_t page;
    unsigned char *gcm = gc_card_mark;
    for (page=0; page<next_free_page; ++page) {
        long card = page_to_card_index(page);
        int j;
        for (j=0; j<CARDS_PER_PAGE; ++j, ++card)
            gcm[card] = (gcm[card] == STICKY_MARK) ? CARD_MARKED : gcm[card];
    }
    }
#endif

    /* Save the high-water mark before updating next_free_page */
    if (next_free_page > high_water_mark)
        high_water_mark = next_free_page;

    next_free_page = find_next_free_page();
    /* Update auto_gc_trigger. Make sure we trigger the next GC before
     * running out of heap! */
    if (bytes_consed_between_gcs / FRAGMENTATION_COMPENSATION <= (dynamic_space_size - bytes_allocated))
        auto_gc_trigger = bytes_allocated + bytes_consed_between_gcs;
    else
        auto_gc_trigger = bytes_allocated + (os_vm_size_t)((dynamic_space_size - bytes_allocated)/2 * FRAGMENTATION_COMPENSATION);

    if(gencgc_verbose) {
#define MESSAGE ("Next gc when %"OS_VM_SIZE_FMT" bytes have been consed\n")
        char buf[64];
        int n;
        // fprintf() can - and does - cause deadlock here.
        // snprintf() seems to work fine.
        n = snprintf(buf, sizeof buf, MESSAGE, (uintptr_t)auto_gc_trigger);
        ignore_value(write(2, buf, n));
#undef MESSAGE
    }

    /* If we did a big GC (arbitrarily defined as gen > 1), release memory
     * back to the OS.
     */
    if (gen > small_generation_limit) {
        if (next_free_page > high_water_mark)
            high_water_mark = next_free_page;
        // BUG? high_water_mark is the highest value of next_free_page,
        // which means that page_table[high_water_mark] was actually NOT ever
        // used, because next_free_page is an exclusive bound on the range
        // of pages used. But remap_free_pages takes to 'to' as an *inclusive*
        // bound. The only reason it's not an array overrun error is that
        // the page_table has one more element than there are pages.
        remap_free_pages(0, high_water_mark);
        high_water_mark = 0;
    }

    large_allocation = 0;
 finish:
    write_protect_immobile_space();
    gc_active_p = 0;

#ifdef COLLECT_GC_STATS
    struct timespec t_gc_done;
    clock_gettime(CLOCK_MONOTONIC, &t_gc_done);
    long et_nsec = (t_gc_done.tv_sec - t_gc_start.tv_sec)*1000000000
      + (t_gc_done.tv_nsec - t_gc_start.tv_nsec);
    tot_gc_nsec += et_nsec;
#endif

    log_generation_stats(gc_logfile, "=== GC End ===");
    // Increment the finalizer runflag.  This acts as a count of the number
    // of GCs as well as a notification to wake the finalizer thread.
    if (finalizer_thread_runflag != 0) {
        int newval = 1 + finalizer_thread_runflag;
        // check if counter wrapped around. Don't store 0 as the new value,
        // as that causes the thread to exit.
        finalizer_thread_runflag = newval ? newval : 1;
    }
    THREAD_JIT(1);
    // Clear all pin bits for the next GC cycle.
    // This could be done in the background somehow maybe.
    page_index_t max_nfp = initial_nfp > next_free_page ? initial_nfp : next_free_page;
    memset(gc_page_pins, 0, max_nfp);
}

/* Initialization of gencgc metadata is split into two steps:
 * 1. gc_init() - allocation of a fixed-address space via mmap(),
 *    failing which there's no reason to go on. (safepoint only)
 * 2. gc_allocate_ptes() - page table entries
 */
void
gc_init(void)
{
#ifdef LISP_FEATURE_WIN32
    InitializeCriticalSection(&free_pages_lock);
#endif
#if defined(LISP_FEATURE_SB_SAFEPOINT)
    extern void safepoint_init(void);
    safepoint_init();
#endif
    mrgc_init();
}

int gc_card_table_nbits;
long gc_card_table_mask;


/*
 * The vops that allocate assume that the returned space is zero-filled.
 * (E.g. the most significant word of a 2-word bignum in MOVE-FROM-UNSIGNED.)
 *
 * The check for a GC trigger is only performed when the current
 * region is full, so in most cases it's not needed. */

/* Make this easy for Lisp to read. */
int small_allocation_count = 0;

int gencgc_alloc_profiler;

NO_SANITIZE_MEMORY lispobj*
lisp_alloc(__attribute__((unused)) int flags,
           struct alloc_region *region, sword_t nbytes,
           int page_type, struct thread *thread)
{
    os_vm_size_t trigger_bytes = 0;

    gc_assert(nbytes > 0);

    /* Check for alignment allocation problems. */
    gc_assert((((uword_t)region->free_pointer & LOWTAG_MASK) == 0)
              && ((nbytes & LOWTAG_MASK) == 0));

#define SYSTEM_ALLOCATION_FLAG 2
#ifdef LISP_FEATURE_SYSTEM_TLABS
    lispobj* handle_arena_alloc(struct thread*, struct alloc_region *, int, sword_t);
    if (page_type != PAGE_TYPE_CODE && thread->arena && !(flags & SYSTEM_ALLOCATION_FLAG))
        return handle_arena_alloc(thread, region, page_type, nbytes);
#endif

    ++thread->slow_path_allocs;
    if ((os_vm_size_t) nbytes > large_allocation)
        large_allocation = nbytes;

    /* maybe we can do this quickly ... */
    void *new_obj = region->free_pointer;
    char *new_free_pointer = (char*)new_obj + nbytes;
    if (new_free_pointer <= (char*)region->end_addr) {
        region->free_pointer = new_free_pointer;
        return new_obj;
    }

    if (try_allocate_small_after_region(nbytes, region)) {
      memset(region->start_addr, 0, addr_diff(region->end_addr, region->start_addr));
      return region->start_addr;
    }

    /* We don't want to count nbytes against auto_gc_trigger unless we
     * have to: it speeds up the tenuring of objects and slows down
     * allocation. However, unless we do so when allocating _very_
     * large objects we are in danger of exhausting the heap without
     * running sufficient GCs.
     */
    if ((os_vm_size_t) nbytes >= bytes_consed_between_gcs)
        trigger_bytes = nbytes;

    /* we have to go the long way around, it seems. Check whether we
     * should GC in the near future
     */
    if (auto_gc_trigger && (bytes_allocated+trigger_bytes > auto_gc_trigger)) {
        /* Don't flood the system with interrupts if the need to gc is
         * already noted. This can happen for example when SUB-GC
         * allocates or after a gc triggered in a WITHOUT-GCING. */
        if (read_TLS(GC_PENDING,thread) == NIL) {
            /* set things up so that GC happens when we finish the PA
             * section */
            write_TLS(GC_PENDING, LISP_T, thread);
            if (read_TLS(GC_INHIBIT,thread) == NIL) {
#ifdef LISP_FEATURE_SB_SAFEPOINT
                thread_register_gc_trigger();
#else
                set_pseudo_atomic_interrupted(thread);
                maybe_save_gc_mask_and_block_deferrables
# if HAVE_ALLOCATION_TRAP_CONTEXT
                    (thread_interrupt_data(thread).allocation_trap_context);
# else
                    (0);
# endif
#endif
            }
        }
    }

    /* For the architectures which do NOT use a trap instruction for allocation,
     * overflow, record a backtrace now if statistical profiling is enabled.
     * The ones which use a trap will backtrace from the signal handler.
     * Code allocations are ignored, because every code allocation
     * comes through lisp_alloc() which makes this not a statistical
     * sample. Also the trapping ones don't trap for code.
     * #+win32 doesn't seem to work, but neither does CPU profiling */
#if !(defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64 \
      || defined LISP_FEATURE_SPARC || defined LISP_FEATURE_WIN32)
    extern void allocator_record_backtrace(void*, struct thread*);
    if (page_type != PAGE_TYPE_CODE && gencgc_alloc_profiler
        && thread->state_word.sprof_enable)
        allocator_record_backtrace(__builtin_frame_address(0), thread);
#endif

    ensure_region_closed(region, page_type);
    int __attribute__((unused)) ret = mutex_acquire(&free_pages_lock);
    gc_assert(ret);
    page_index_t alloc_start = get_alloc_start_page(page_type);
    bool largep = nbytes >= LARGE_OBJECT_SIZE && page_type != PAGE_TYPE_CONS;
    if (largep) {
        uword_t largest_hole;
        page_index_t new_page = try_allocate_large(nbytes, page_type, gc_alloc_generation,
                                                   &alloc_start, page_table_pages, &largest_hole);
        if (new_page == -1) gc_heap_exhausted_error_or_lose(largest_hole, nbytes);
        set_alloc_start_page(page_type, alloc_start);
        ret = mutex_release(&free_pages_lock);
        gc_assert(ret);
        new_obj = page_address(new_page);
        set_allocation_bit_mark(new_obj);
        memset(new_obj, 0, nbytes);
    } else {
        if (!gc_active_p) small_allocation_count++;
        bool success =
            try_allocate_small_from_pages(nbytes, region, page_type,
                                          gc_alloc_generation,
                                          &alloc_start, page_table_pages);
        if (!success) gc_heap_exhausted_error_or_lose(0, nbytes);
        set_alloc_start_page(page_type, alloc_start);
        ret = mutex_release(&free_pages_lock);
        gc_assert(ret);
        new_obj = region->start_addr;
        memset(new_obj, 0, addr_diff(region->end_addr, new_obj));
    }

    return new_obj;
}

/*
 * shared support for the OS-dependent signal handlers which
 * catch GENCGC-related write-protect violations
 */
void unhandled_sigmemoryfault(void* addr);

/* Depending on which OS we're running under, different signals might
 * be raised for a violation of write protection in the heap. This
 * function factors out the common generational GC magic which needs
 * to invoked in this case, and should be called from whatever signal
 * handler is appropriate for the OS we're running under.
 *
 * Return true if this signal is a normal generational GC thing that
 * we were able to handle, or false if it was abnormal and control
 * should fall through to the general SIGSEGV/SIGBUS/whatever logic.
 *
 * We have two control flags for this: one causes us to ignore faults
 * on unprotected pages completely, and the second complains to stderr
 * but allows us to continue without losing.
 */
extern bool ignore_memoryfaults_on_unprotected_pages;
bool ignore_memoryfaults_on_unprotected_pages = 0;

extern bool continue_after_memoryfault_on_unprotected_pages;
bool continue_after_memoryfault_on_unprotected_pages = 0;

int gencgc_handle_wp_violation(__attribute__((unused)) void* context, void* fault_addr)
{
    page_index_t page_index = find_page_index(fault_addr);

    /* Check whether the fault is within the dynamic space. */
    if (page_index == (-1)) {
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        extern int immobile_space_handle_wp_violation(void*);
        if (immobile_space_handle_wp_violation(fault_addr))
            return 1;
#endif
        /* It can be helpful to be able to put a breakpoint on this
         * case to help diagnose low-level problems. */
        unhandled_sigmemoryfault(fault_addr);
        /* not within the dynamic space -- not our responsibility */
        return 0;
    }
    lose("misuse of mprotect() on dynamic space @ %p", fault_addr);
    return 1; // Handled
}
/* This is to be called when we catch a SIGSEGV/SIGBUS, determine that
 * it's not just a case of the program hitting the write barrier, and
 * are about to let Lisp deal with it. It's basically just a
 * convenient place to set a gdb breakpoint. */
void
unhandled_sigmemoryfault(void __attribute__((unused)) *addr)
{}

/** code blob scavenging **/

/* Return 1 if 'a' is strictly younger than 'b'.
 * This asserts that 'a' is pinned if in 'from_space' because it is
 * specifically a helper function for scav_code_blob(), where this is
 * called after scavenging the header. So if something didn't get moved
 * out of from_space, then it must have been pinned.
 * So don't call this for anything except that use-case. */
static inline bool obj_gen_lessp(lispobj obj, generation_index_t b)
{
    generation_index_t a = gc_gen_of(obj, ARTIFICIALLY_HIGH_GEN);
    if (a == from_space) {
        gc_assert(pinned_p(obj, find_page_index((void*)obj)));
        a  = new_space;
    }
    return ((a==SCRATCH_GENERATION) ? from_space : a) < b;
}

sword_t scav_code_blob(lispobj *object, lispobj header)
{
    struct code* code = (struct code*)object;
    int nboxed = code_header_words(code);
    if (!nboxed) goto done;

    int my_gen = gc_gen_of((lispobj)object, ARTIFICIALLY_HIGH_GEN);
    if (my_gen < ARTIFICIALLY_HIGH_GEN && ((my_gen & 7) == from_space)) {
        // Since 'from_space' objects are not directly scavenged - they can
        // only be scavenged after moving to newspace, then this object
        // must be pinned. (It's logically in newspace). Assert that.
        gc_assert(pinned_p(make_lispobj(object, OTHER_POINTER_LOWTAG),
                           find_page_index(object)));
        my_gen = new_space;
    }

    // If the header's 'written' flag is off and it was not copied by GC
    // into newspace, then the object should be ignored.

    // This test could stand to be tightened up: in a GC promotion cycle
    // (e.g. 0 becomes 1), we can't discern objects that got copied to newspace
    // from objects that started out there. Of the ones that were already there,
    // we need only scavenge those marked as written. All the copied one
    // should always be scavenged. So really what we could do is mark anything
    // that got copied as written, which would allow dropping the second half
    // of the OR condition. As is, we scavenge "too much" of newspace which
    // is not an issue of correctness but rather efficiency.
    if (header_rememberedp(header) || (my_gen == new_space) ||
        ((uword_t)object >= STATIC_SPACE_START && object < static_space_free_pointer)) {
        // FIXME: We sometimes scavenge protected pages.
        // This assertion fails, but things work nonetheless.
        // gc_assert(!card_protected_p(object));

        /* Scavenge the boxed section of the code data block. */
        sword_t n_header_words = code_header_words((struct code *)object);
        scavenge(object + 2, n_header_words - 2);

#if defined LISP_FEATURE_64_BIT && !defined LISP_FEATURE_DARWIN_JIT
        /* If any function in this code object redirects to a function outside
         * the object, then scavenge all entry points. Otherwise there is no need,
         * as trans_code() made necessary adjustments to internal entry points.
         * This test is just an optimization to avoid some work */
        if (((*object >> 16) & 0xff) == CODE_IS_TRACED) {
#else
        { /* Not enough spare bits in the header to hold random flags.
           * Just do the extra work always */
#endif
            for_each_simple_fun(i, fun, code, 1, {
                if (simplefun_is_wrapped(fun)) {
                    lispobj target_fun = fun_taggedptr_from_self(fun->self);
                    lispobj new = target_fun;
                    scavenge(&new, 1);
                    if (new != target_fun) fun->self = fun_self_from_taggedptr(new);
                }
            })
        }

        /* If my_gen is other than newspace, then scan for old->young
         * pointers. If my_gen is newspace, there can be no such pointers
         * because newspace is the lowest numbered generation post-GC
         * (regardless of whether this is a promotion cycle) */
        if (my_gen != new_space) {
            lispobj *where, *end = object + n_header_words, ptr;
            for (where= object + 2; where < end; ++where)
                if (is_lisp_pointer(ptr = *where) && obj_gen_lessp(ptr, my_gen))
                    goto done;
        }
        CLEAR_WRITTEN_FLAG(object);
    }
done:
    return code_total_nwords(code);
}

/** heap invariant checker **/
#include <stdatomic.h>
#include "tiny-lock.h"
#include "gc-thread-pool.h"
lock_t failure_lock = LOCK_INITIALIZER;
#define lock() acquire_lock(&failure_lock)
#define unlock() release_lock(&failure_lock)

#define ERROR_LIMIT 25

static bool card_markedp(void* addr)
{
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (immobile_space_p((lispobj)addr))
        return !immobile_card_protected_p(addr);
#endif
    return gc_card_mark[addr_to_card_index(addr)] != CARD_UNMARKED;
}

char *page_card_mark_string(page_index_t page, char *result)
{
    long card = addr_to_card_index(page_address(page));
    if (cardseq_all_marked_nonsticky(card))
        result[0] = '*', result[1] = 0;
    else if (!cardseq_any_marked(card))
        result[0] = '-', result[1] = 0;
    else {
        int i;
        for(i=0; i<CARDS_PER_PAGE; ++i)
        switch (gc_card_mark[card+i] & MARK_BYTE_MASK) {
        case CARD_MARKED: result[i] = '*'; break;
        case CARD_UNMARKED: result[i] = '-'; break;
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        case STICKY_MARK: result[i] = 'S'; break;
#else
        case WP_CLEARED_AND_MARKED: result[i] = 'd'; break; // "d" is for dirty
#endif
        default: result[i] = '?'; break; // illegal value
        }
        result[CARDS_PER_PAGE] = 0;
    }
    return result;
}

#define PRINT_HEADER_ON_FAILURE 2048

// Check a single pointer. Return 1 if we should stop verifying due to too many errors.
// (Otherwise continue showing errors until then)
// NOTE: This function can produces false failure indications,
// usually related to dynamic space pointing to the stack of a
// dead thread, but there may be other reasons as well.
static void note_failure(lispobj thing, lispobj *where, struct verify_state *state,
                         char *str)
{
    lock();
    if (state->flags & PRINT_HEADER_ON_FAILURE) {
        if (state->flags & VERIFY_PRE_GC) fprintf(stderr, "pre-GC failure\n");
        if (state->flags & VERIFY_POST_GC) fprintf(stderr, "post-GC failure\n");
        state->flags &= ~PRINT_HEADER_ON_FAILURE;
    }
    if (state->object_addr) {
        lispobj obj = compute_lispobj(state->object_addr);
        page_index_t pg = find_page_index(state->object_addr);
        fprintf(stderr, "Ptr %p @ %"OBJ_FMTX" (lispobj %"OBJ_FMTX",pg%d) sees %s\n",
                (void*)thing, (uword_t)where, obj, (int)pg, str);
        // Record this in state->err_objs if possible
        int i;
        for(i=0; i<MAX_ERR_OBJS; ++i)
            if (!state->err_objs[i]) {
                state->err_objs[i] = (uword_t)state->object_addr;
                break;
            }
    } else {
        fprintf(stderr, "Ptr %p @ %"OBJ_FMTX" sees %s\n", (void*)thing, (uword_t)where, str);
    }
    unlock();
}

static int
verify_pointer(lispobj thing, lispobj *where, struct verify_state *state)
{
    extern int valid_lisp_pointer_p(lispobj);

    /* Strict containment: no pointer from a heap space may point
     * to anything outside of a heap space. */
    // bool strict_containment = state->flags & VERIFY_FINAL;

#define FAIL_IF(cond, why) \
    if (cond) { if (++state->nerrors >= ERROR_LIMIT) return 1; note_failure(thing,where,state,why); }

    if (!is_lisp_pointer(thing)) {
        FAIL_IF(!is_lisp_immediate(thing), "strange non-pointer");
        return 0;
    }
    // if (strict_containment && !gc_managed_heap_space_p(thing)) GC_WARN("non-Lisp memory");
    page_index_t source_page_index = find_page_index(where);
    page_index_t target_page_index = find_page_index((void*)thing);
    if (!(target_page_index >= 0 || immobile_space_p(thing))) return 0; // can't do much with it
    if ((state->flags & VERIFY_TAGS) && target_page_index >= 0) {
        if (listp(thing)) {
            FAIL_IF(!(is_cons_half(CONS(thing)->car) && is_cons_half(CONS(thing)->cdr)),
                    "non-cons");
        } else {
            FAIL_IF(LOWTAG_FOR_WIDETAG(widetag_of(native_pointer(thing))) != lowtag_of(thing),
                    "incompatible widetag");
        }
    }
    generation_index_t to_gen =
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        points_to_asm_code_p((uword_t)thing)?
        gc_gen_of(make_lispobj((void*)asm_routines_start,OTHER_POINTER_LOWTAG),0):
#endif
        gc_gen_of(thing, ARTIFICIALLY_HIGH_GEN);
    if (to_gen < state->min_pointee_gen) state->min_pointee_gen = to_gen;
    if (state->flags & VERIFY_QUICK) return 0;
    if (target_page_index >= 0) {
        // If it's within the dynamic space it should point to a used page.
        FAIL_IF(page_free_p(target_page_index), "free page");
        lispobj *native = native_pointer(thing);
        bool embedded = embedded_obj_p(widetag_of(native));
        /* Embedded objects don't get marked, the enclosing object does. */
        lispobj *target = embedded ? (lispobj*)fun_code_header((struct simple_fun*)native)
                          : native;
        if ((state->flags & VERIFY_POST_GC))
          FAIL_IF(!allocation_bit_marked(target), "no allocation mark");
#if 0
        /* Line marks are often out of date before GC */
        if ((state->flags & VERIFY_POST_GC) &&
            to_gen != PSEUDO_STATIC_GENERATION &&
            !page_single_obj_p(target_page_index)) {
            /* Don't try to call object_size on embedded objects */
            if (!embedded) {
                uword_t size = object_size(native);
                for (uword_t word = 0; word < size; word++) {
                  if (!line_marked(native + word)) {
                    char why[50];
                    snprintf(why, sizeof(why), "no line mark on word %lx", word);
                    FAIL_IF(!line_marked(native + word), why);
                  }
                }
            }
        }
#endif
    } else {
        // The object pointed to must not have been discarded as garbage.
        FAIL_IF(!other_immediate_lowtag_p(*native_pointer(thing)), "trashed object");
    }
    // Must not point to a forwarding pointer
    FAIL_IF(*native_pointer(thing) == FORWARDING_HEADER, "forwarding ptr");
    // Forbid pointers from R/O space into a GCed space
    FAIL_IF((READ_ONLY_SPACE_START <= (uword_t)where && where < read_only_space_free_pointer),
            "dynamic space from RO space");
    // Card marking invariant check, but only if the source of pointer is a heap object
    if (header_widetag(state->object_header) == CODE_HEADER_WIDETAG
        && ! is_in_static_space(state->object_addr)
        && to_gen < state->object_gen) {
        // two things must be true:
        // 1. the card containing the code must be marked
        FAIL_IF(!card_markedp(state->object_addr), "younger obj from WP'd code header page");
        // 2. the object header must be marked as written
        if (!header_rememberedp(state->object_header))
            lose("code @ %p (g%d). word @ %p -> %"OBJ_FMTX" (g%d)",
                 state->object_addr, state->object_gen, where, thing, to_gen);
    } else if ((state->flags & VERIFYING_GENERATIONAL) && to_gen < state->object_gen
               && source_page_index >= 0) {
        /* The WP criteria are:
         *  - CONS marks the exact card since it can't span cards
         *  - SIMPLE-VECTOR marks the card containing the cell with the old->young pointer.
         *  - Everything else marks the object header -OR- the card with the pointer.
         *    (either/or because Lisp marks the header card,
         *     but the collector marks the cell's card.) */
        int marked = card_markedp(where)
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        || (state->object_header
            && header_widetag(state->object_header) != SIMPLE_VECTOR_WIDETAG
            && card_markedp(state->object_addr))
#elif defined LISP_FEATURE_EXECUTABLE_FUNINSTANCES
        /* #+(and (not soft-card-marks) executable-funinstances) could find the mark
         * for a page-spanning funinstance on the preceding page, because it uses
         * logical marking, not physical protection of the page holding the pointer */
        || (header_widetag(state->object_header) == FUNCALLABLE_INSTANCE_WIDETAG
            && card_markedp(state->object_addr))
#endif
        ;
        FAIL_IF(!marked, "younger obj from WP page");
    }
    int valid;
    if (state->flags & VERIFY_AGGRESSIVE) // Extreme paranoia mode
        valid = valid_lisp_pointer_p(thing);
    else {
        /* Efficiently decide whether 'thing' is plausible.
         * This MUST NOT use properly_tagged_descriptor_p() which
         * assumes a known good object base address, and would
         * "dangerously" scan a code component for embedded funs. */
        valid = plausible_tag_p(thing);
    }
    /* If 'thing' points to a stack, we can only hope that the stack
     * frame is ok, or the object at 'where' is unreachable. */
    FAIL_IF(!valid && !is_in_stack_space(thing), "junk");
    return 0;
}
#define CHECK(pointer, where) if (verify_pointer(pointer, where, state)) return 1

/* Return 0 if good, 1 if bad */
static int verify_headered_object(lispobj* object, sword_t nwords,
                                  struct verify_state *state)
{
    long i;
    int widetag = widetag_of(object);
    if (instanceoid_widetag_p(widetag)) {
        lispobj layout = layout_of(object);
        if (layout) {
            CHECK(layout, object);
            struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
            if (widetag_of(object) == FUNCALLABLE_INSTANCE_WIDETAG) {
#ifndef LISP_FEATURE_COMPACT_INSTANCE_HEADER
                gc_assert(bitmap.bits[0] == (sword_t)-4);
#endif
            }
            if (lockfree_list_node_layout_p(LAYOUT(layout))) {
                struct list_node* node = (void*)object;
                lispobj next = node->_node_next;
                if (fixnump(next) && next)
                  CHECK(next | INSTANCE_POINTER_LOWTAG, &node->_node_next);
            }
            for (i=0; i<(nwords-1); ++i)
                if (bitmap_logbitp(i, bitmap)) CHECK(object[1+i], object+1+i);
        }
        return 0;
    }
    if (widetag == CODE_HEADER_WIDETAG) {
        struct code *code = (struct code *)object;
        gc_assert(fixnump(object[1])); // boxed size, needed for code_header_words()
        sword_t nheader_words = code_header_words(code);
        /* Verify the boxed section of the code data block */
        state->min_pointee_gen = ARTIFICIALLY_HIGH_GEN;
#ifdef LISP_FEATURE_UNTAGGED_FDEFNS
        {
        lispobj* pfdefn = code->constants + code_n_funs(code) * CODE_SLOTS_PER_SIMPLE_FUN;
        lispobj* end = pfdefn + code_n_named_calls(code);
        for ( ; pfdefn < end ; ++pfdefn)
            if (*pfdefn) CHECK(*pfdefn | OTHER_POINTER_LOWTAG, pfdefn);
        }
#endif
        for (i=2; i <nheader_words; ++i) CHECK(object[i], object+i);
        // Check the SIMPLE-FUN headers
        for_each_simple_fun(i, fheaderp, code, 1, {
#if defined LISP_FEATURE_COMPACT_INSTANCE_HEADER
            lispobj __attribute__((unused)) layout = funinstance_layout((lispobj*)fheaderp);
            gc_assert(!layout || layout == LAYOUT_OF_FUNCTION);
#elif defined LISP_FEATURE_64_BIT
            gc_assert((fheaderp->header >> 32) == 0);
#endif
        });
#if 0 // this looks redundant. It's checked with each pointer, no?
        bool rememberedp = header_rememberedp(code->header);
        /* The remembered set invariant is that an object is marked "written"
         * if and only if either it points to a younger object or is pointed
         * to by a register or stack. (The pointed-to case assumes that the
         * very next instruction on return from GC would store an old->young
         * pointer into that object). Non-compacting GC does not have the
         * "only if" part of that, nor does pre-GC verification because we
         * don't test the generation of the newval when storing into code. */
        if (is_in_static_space(object)) { }
        else if (compacting_p() && (state->flags & VERIFY_POST_GC) ?
            (state->min_pointee_gen < state->object_gen) != rememberedp :
            (state->min_pointee_gen < state->object_gen) && !rememberedp)
            lose("object @ %p is gen%d min_pointee=gen%d %s",
                 (void*)state->tagged_object, state->object_gen, state->min_pointee_gen,
                 rememberedp ? "written" : "not written");
#endif
        return 0;
    }
    if (widetag == SYMBOL_WIDETAG) {
        struct symbol* s = (void*)object;
        CHECK(s->value, &s->value);
        CHECK(s->fdefn, &s->fdefn);
        CHECK(s->info, &s->info);
        CHECK(decode_symbol_name(s->name), &s->name);
        return 0;
    }
    if (widetag == FDEFN_WIDETAG) {
        struct fdefn* f = (void*)object;
        CHECK(f->name, &f->name);
        CHECK(f->fun, &f->fun);
        CHECK(decode_fdefn_rawfun(f), (lispobj*)&f->raw_addr);
        return 0;
    }
    for (i=1; i<nwords; ++i) CHECK(object[i], object+i);
    return 0;
}

static __attribute__((unused)) bool acceptable_filler_cons_p(lispobj* where)
{
    if (where[0] == 0 && where[1] == 0) return 1;
    // These "conses" can result from bignum multiplication-
    // trailing insigificant sign bits which get chopped.
    if (where[0] == (uword_t)-1 && where[1] == (uword_t)-1) return 1;
    if (where[0] == (uword_t)-1 && where[1] == 0) return 1;
    return 0;
}
static int verify_range(lispobj* start, lispobj* end, struct verify_state* state)
{
    lispobj* where = start;
    if ((state->flags & VERIFYING_UNFORMATTED)) {
        while (where < end) {
            if (*where != NO_TLS_VALUE_MARKER) {
                int result = verify_pointer(*where, where, state);
                if (result) return result;
            }
            ++where;
        }
        return 0;
    }
    where = next_object(start, 0, end); /* find first marked object */
    while (where) {
        int widetag = is_header(*where) ? header_widetag(*where) : LIST_POINTER_LOWTAG;
        /* Technically we should wait until after performing the widetag validity
         * tests before calling the sizer.  Otherwise the lossage message would
         * not be as good as it could be. I guess that failure doesn't happen much */
        sword_t nwords = object_size(where);
        if (find_page_index(where) != -1)
          /* Check that no allocation bits are interior to this object  */
          for (lispobj *interior = where + 2; interior < where + nwords; interior += 2)
            if (allocation_bit_marked(interior))
                lose("object @ %p has an allocation marked interior to it @ %p", where, interior);
        state->object_addr = where;
        state->object_header = is_cons_half(*where) ? 0 : *where;
        if (state->flags & VERIFYING_GENERATIONAL) {
            page_index_t pg = find_page_index(where);
            state->object_gen = gc_gen_of((lispobj)where, ARTIFICIALLY_HIGH_GEN);
            if (state->object_gen == ARTIFICIALLY_HIGH_GEN)
              lose("object @ %p has no generation", where);
#ifdef LISP_FEATURE_PPC64
            // Cons fillers (two words of all 1s) cause failure of
            // the default verification logic, so brute-force skip them
            // regardless of whether the page type is PAGE_TYPE_CONS.
            if (*where == (uword_t)-1 && where[1] == (uword_t)-1) {
                where = next_object(where, 2, end);
                continue;
            }
#endif
            if (widetag != FILLER_WIDETAG && pg >= 0) {
                    // Assert proper page type
                    if (state->object_header) // is not a cons
                        gc_assert(page_table[pg].type != PAGE_TYPE_CONS);
#ifdef LISP_FEATURE_USE_CONS_REGION
                    else if (page_table[pg].type != PAGE_TYPE_CONS) {
                      if (is_cons_half(where[0]))
                          gc_assert(acceptable_filler_cons_p(where));
                    }
#endif
                    if (widetag == CODE_HEADER_WIDETAG) {
                        if (!is_code(page_table[pg].type))
                            lose("object @ %p is code on non-code page", where);
                    } else if (widetag == FUNCALLABLE_INSTANCE_WIDETAG) {
                        // where these reside depends on the architecture
                    } else {
                        if (is_code(page_table[pg].type))
                            lose("object @ %p is non-code on code page", where);
                    }
            }
        }
        if (!state->object_header) {
            if (verify_pointer(where[0], where+0, state) ||
                verify_pointer(where[1], where+1, state)) break;
        } else if (widetag == FILLER_WIDETAG) { // do nothing
        } else if (!(other_immediate_lowtag_p(widetag) && LOWTAG_FOR_WIDETAG(widetag))) {
            lose("Unhandled widetag %"OBJ_FMTX" @ %p", *where, where);
        } else if (leaf_obj_widetag_p(widetag)) {
#ifdef LISP_FEATURE_UBSAN
            if (specialized_vector_widetag_p(widetag)) {
                if (is_lisp_pointer(object[1])) {
                    struct vector* bits = (void*)native_pointer(object[1]);
                    if (header_widetag(bits->header) != SIMPLE_BIT_VECTOR_WIDETAG)
                      lose("bad shadow bits for %p", where);
                    gc_assert(header_widetag(bits->header) == SIMPLE_BIT_VECTOR_WIDETAG);
                    gc_assert(vector_len(bits) >= vector_len((struct vector*)object));
                }
            }
#endif
            bool strict_containment = state->flags & VERIFY_FINAL;
            if (strict_containment && gencgc_verbose && widetag == SAP_WIDETAG && where[1])
                fprintf(stderr, "\nStrange SAP %p -> %p\n", where, (void*)where[1]);
        } else {
            if (verify_headered_object(where, nwords, state)) break;
        }
        where = next_object(where, nwords, end);
    }
    return 0;
}

static int verify(lispobj start, lispobj* end, struct verify_state* state, int flags)
{
    int savedflags = state->flags;
    state->flags |= flags;
    int result = verify_range((lispobj*)start, end, state);
    state->flags = savedflags;
    return result;
}

static _Atomic(uword_t) walk_halted;
static _Atomic(uword_t) walk_cursor;
static uword_t parallel_walk_extra;
struct span { lispobj *start; lispobj *end; };
static uword_t table_spans;
static struct span *span_table;
static uword_t (*parallel_proc)(lispobj*,lispobj*,uword_t);

static uword_t fill_table(lispobj *start, lispobj *end, __attribute__((unused)) uword_t unused) {
    span_table[table_spans++] = (struct span){start, end};
    return 0;
}

static void parallel_walk_worker() {
    /* Copy the verifier state. */
    struct verify_state *original = (struct verify_state*)parallel_walk_extra, copy = *original;
    uword_t index;
    while ((index = atomic_fetch_add(&walk_cursor, 1)) < table_spans && !atomic_load(&walk_halted)) {
        uword_t ret = parallel_proc(span_table[index].start, span_table[index].end, (uword_t)&copy);
        if (ret) atomic_store(&walk_halted, ret);
    }
    atomic_fetch_add(&original->nerrors, copy.nerrors);
}

static void parallel_walk_generation(uword_t (*proc)(lispobj*,lispobj*,uword_t),
                                     generation_index_t generation, uword_t extra)
{
    walk_halted = 0;
    walk_cursor = 0;
    parallel_walk_extra = extra;
    parallel_proc = proc;
    /* Compute a table of spans for the thread pool to walk.
     * There can't be more spans than pages, but there can be
     * fewer due to large objects. */
    table_spans = 0;
    uword_t table_size = sizeof(struct span) * next_free_page;
    span_table = (struct span*)os_allocate(table_size);
    walk_generation(fill_table, generation, 0);
    run_on_thread_pool(parallel_walk_worker);
    os_deallocate((char*)span_table, table_size);
}

static void check_free_pages()
{
    unsigned char *allocs = (unsigned char*)allocation_bitmap;
    for (page_index_t p = 0; p < page_table_pages; p++)
        if (page_free_p(p))
            for_lines_in_page(l, p)
                if (allocs[l])
                    lose("You call page #%ld free, despite the fact it's obviously got allocation bits.", p);
}

/* Return the number of verification errors found.
 * You might want to use that as a deciding factor for dump the heap
 * to a file (which takes time, and generally isn't needed).
 * But if a particular verification fails, then do dump it */
int verify_heap(__attribute__((unused)) lispobj* cur_thread_approx_stackptr,
                int flags)
{
    int verbose = gencgc_verbose | ((flags & VERIFY_VERBOSE) != 0);

    struct verify_state state;
    memset(&state, 0, sizeof state);
    state.flags = flags;

    if (verbose)
        fprintf(stderr,
                flags & VERIFY_PRE_GC ? "Verify before GC" :
                flags & VERIFY_POST_GC ? "Verify after GC(%d)" :
                "Heap check", // if called at a random time
                (int)(flags>>16)); // generation number
    else
        state.flags |= PRINT_HEADER_ON_FAILURE;

#ifdef LISP_FEATURE_IMMOBILE_SPACE
#  ifdef __linux__
    // Try this verification if immobile-space was compiled with extra debugging.
    // But weak symbols don't work on macOS.
    extern void __attribute__((weak)) check_text_pages();
    if (&check_text_pages) check_text_pages();
#  endif
    if (verbose)
        fprintf(stderr, " [immobile]");
    if (verify(FIXEDOBJ_SPACE_START,
               fixedobj_free_pointer, &state,
               flags | VERIFYING_GENERATIONAL)) goto out;
    if (verify(TEXT_SPACE_START,
               text_space_highwatermark, &state,
               flags | VERIFYING_GENERATIONAL)) goto out;
#endif
    struct thread *th;
    if (verbose)
        fprintf(stderr, " [threads]");
    state.object_addr = 0;
    state.object_gen = 0;
    for_each_thread(th) {
        if (verbose)
            fprintf(stderr, " [thread bindings]");
        if (verify((lispobj)th->binding_stack_start,
                   (lispobj*)get_binding_stack_pointer(th), &state,
                   VERIFYING_UNFORMATTED)) goto out;
        if (verbose)
            fprintf(stderr, " [thread TLS]");
        if (verify((lispobj)&th->lisp_thread,
                   (lispobj*)(SymbolValue(FREE_TLS_INDEX,0) + (char*)th), &state,
                   VERIFYING_UNFORMATTED))
            goto out;
    }
    if (verbose)
        fprintf(stderr, " [RO]");
    if (verify(READ_ONLY_SPACE_START, read_only_space_free_pointer, &state, 0)) goto out;
    if (verbose)
        fprintf(stderr, " [static]");
    // Just don't worry about NIL, it's seldom the problem
    // if (verify(NIL_SYMBOL_SLOTS_START, (lispobj*)NIL_SYMBOL_SLOTS_END, &state, 0)) goto out;
    if (verify(STATIC_SPACE_OBJECTS_START, static_space_free_pointer, &state, 0)) goto out;
    if (verbose)
        fprintf(stderr, " [dynamic]");
    state.flags |= VERIFYING_GENERATIONAL;
    parallel_walk_generation((uword_t(*)(lispobj*,lispobj*,uword_t))verify_range,
                             -1, (uword_t)&state);
    check_free_pages();
    if (verbose && state.nerrors==0) fprintf(stderr, " passed\n");
 out:
    if (state.nerrors && !(flags & VERIFY_DONT_LOSE)) {
        // dump_spaces(&state, "verify failed");
        lose("Verify failed: %d errors", state.nerrors);
    }
    return state.nerrors;
}

void gc_show_pte(lispobj obj)
{
    char marks[1+CARDS_PER_PAGE];
    page_index_t page = find_page_index((void*)obj);
    if (page>=0) {
        printf("page %"PAGE_INDEX_FMT" base %p gen %d type %x ss %p used %x",
               page, page_address(page), page_table[page].gen, page_table[page].type,
               page_scan_start(page), page_bytes_used(page));
        if (page_starts_contiguous_block_p(page)) printf(" startsblock");
        if (page_ends_contiguous_block_p(page, page_table[page].gen)) printf(" endsblock");
        printf(" (%s)\n", page_card_mark_string(page, marks));
        return;
    }
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    page = find_text_page_index((void*)obj);
    if (page>=0) {
        lispobj* text_page_scan_start(low_page_index_t page);
        int gens = text_page_genmask[page];
        char genstring[9];
        int i;
        for (i=0;i<8;++i) genstring[i] = (gens & (1<<i)) ? '0'+i : '-';
        genstring[8] = 0;
        printf("page %d (v) base %p gens %s ss=%p%s\n",
               (int)page, text_page_address(page), genstring,
               text_page_scan_start(page),
               card_markedp((void*)obj)?"":" WP");
        return;
    }
    page = find_fixedobj_page_index((void*)obj);
    if (page>=0) {
        printf("page %d (f) align %d gens %x%s\n", (int)page,
               fixedobj_pages[page].attr.parts.obj_align,
               fixedobj_pages[page].attr.parts.gens_,
               card_markedp((void*)obj)?"": " WP");
        return;
    }
#endif
    printf("not in GC'ed space\n");
}

int hexdump_and_verify_heap(lispobj* cur_thread_approx_stackptr, int flags)
{
    return verify_heap(cur_thread_approx_stackptr, flags);
}

/* The other implementation of gc_gen_report_to_file gets the generations
 * of small objects wrong, as it only checks the generations of pages. */
void gc_gen_report_to_file(int filedes, FILE *file) {
  uword_t small_generation_type[PSEUDO_STATIC_GENERATION + 1][8] = { 0 };
  uword_t small_generation_total[8] = { 0 };
  uword_t large_generation_type[PSEUDO_STATIC_GENERATION + 1][8] = { 0 };
  uword_t large_generation_total[8] = { 0 };
  for (page_index_t p = 0; p < page_table_pages; p++) {
    unsigned char type = page_table[p].type & PAGE_TYPE_MASK;
    if (page_free_p(p)) continue;
    else if (page_single_obj_p(p)) {
      generation_index_t gen = page_table[p].gen;
      large_generation_type[gen][type] += GENCGC_PAGE_BYTES;
      large_generation_total[gen] += GENCGC_PAGE_BYTES;
    } else {
      for_lines_in_page (l, p) {
        if (line_bytemap[l]) {
          generation_index_t gen = DECODE_GEN(line_bytemap[l]);
          small_generation_type[gen][type] += LINE_SIZE;
          small_generation_total[gen] += LINE_SIZE;
        }
      }
    }
  }
#define OUTPUT(str) \
  {if (file) fwrite(str, 1, strlen(str), file); if (filedes>=0) ignore_value(write(filedes, str, strlen(str)));}
  char buffer[180];
#define FORMAT(control, ...) snprintf(buffer, sizeof(buffer), control, __VA_ARGS__); OUTPUT(buffer)
#define MB(n) (int)((n) >> 20)
  FORMAT("  %4s %6s %6s %6s %6s %6s %6s\n",
         "Gen#", "Raw", "Boxed", "Mixed", "Cons", "Code", "Total");
  OUTPUT("Small objects (in MiB):\n");
  for (generation_index_t g = 0; g <= HIGHEST_NORMAL_GENERATION; g++)
    if (small_generation_total[g]) {
      uword_t *gen = small_generation_type[g];
      FORMAT("  %4d %6d %6d %6d %6d %6d %6d\n",
             g,
             MB(gen[PAGE_TYPE_UNBOXED]),
             MB(gen[PAGE_TYPE_BOXED]),
             MB(gen[PAGE_TYPE_MIXED]),
             MB(gen[PAGE_TYPE_CONS]),
             MB(gen[PAGE_TYPE_CODE]),
             MB(small_generation_total[g]));
    }
  OUTPUT("Large objects:\n");
  for (generation_index_t g = 0; g <= PSEUDO_STATIC_GENERATION; g++)
    if (large_generation_total[g]) {
      uword_t *gen = large_generation_type[g];
      FORMAT("  %4d %6d %6d %6d %6d %6d %6d\n",
             g,
             MB(gen[PAGE_TYPE_UNBOXED]),
             MB(gen[PAGE_TYPE_BOXED]),
             MB(gen[PAGE_TYPE_MIXED]),
             MB(gen[PAGE_TYPE_CONS]),
             MB(gen[PAGE_TYPE_CODE]),
             MB(large_generation_total[g]));
    }
#undef FORMAT
#undef OUTPUT
}
