#ifndef _FORWARDING_PTR_H_
#define _FORWARDING_PTR_H_
#include <stdbool.h>

extern void gc_allocate_lock_table(void);
extern uword_t *gc_object_lock_table;

#ifndef LISP_FEATURE_GENCGC
inline static boolean
in_gc_p(void) {
    return current_dynamic_space == from_space;
}
#endif

inline static boolean
forwarding_pointer_p(lispobj *pointer) {
#ifdef LISP_FEATURE_PARALLEL_GC
    lispobj first_word=__atomic_load_n(pointer, __ATOMIC_ACQUIRE);
#else
    lispobj first_word=*pointer;
#endif
#ifdef LISP_FEATURE_GENCGC
    return (first_word == 0x01);
#else
    // FIXME: change 5c0d71f92c371769f911e6a2ac60b2dd9fbde349 added
    // an extra test here, which theoretically slowed things down.
    // This was in response to 044e22192c25578efceedba042554dc9a96124c6
    // which caused cheneygc to break. But now the latter revision has been
    // reverted due to performance degradation in gencgc.
    // The right fix is probably for gc_search_all_spaces() to use a
    // special version of gc_search_space for ldb. That is unfortunately
    // made difficult by the call chain:
    //   search_all_gc_spaces() -> search_{foo}_space() -> gc_search_space().
    // which requires informing gc_search_space() to be more careful,
    // and similarly forwarding_pointer_p().
    return (is_lisp_pointer(first_word)
            && in_gc_p() /* cheneygc new_space_p() is broken when not in gc */
            && new_space_p(first_word));
#endif
}

static inline lispobj
forwarding_pointer_value(lispobj *pointer) {
#ifdef LISP_FEATURE_GENCGC
# ifdef LISP_FEATURE_PARALLEL_GC
    /* Make sure we don't reorder around reading the header and
       forwarding pointer value...somehow. */
    return __atomic_load_n(pointer + 1, __ATOMIC_ACQUIRE);
# else
    return pointer[1];
#endif
#else
    return pointer[0];
#endif
}
static inline lispobj
set_forwarding_pointer(lispobj *pointer, lispobj newspace_copy) {
  // The object at 'pointer' might already have been forwarded,
  // but that's ok. Such occurs primarily when dealing with
  // code components, because code can be forwarded by scavenging any
  // pointer to a function that resides within the code.
  // Testing whether the object had been forwarded would just slow
  // things down, so we blindly stomp on whatever was there.
  // Unfortunately this also implies we can't assert
  // that we're operating on a not-yet-forwarded object here.
#ifdef LISP_FEATURE_GENCGC
    gc_dcheck(compacting_p());
    pointer[1]=newspace_copy;
# ifdef LISP_FEATURE_PARALLEL_GC
    /* This write better only be seen _after_ the write of the actual
       forwarding pointer, or we're hosed. */
    __atomic_store_n(pointer, 0x01, __ATOMIC_RELEASE);
# else
    pointer[0]=0x01;
# endif
#else
    pointer[0]=newspace_copy;
#endif
    return newspace_copy;
}

/// Chase the pointer in 'word' if it points to a forwarded object.
static inline lispobj follow_maybe_fp(lispobj word)
{
    return (is_lisp_pointer(word) && forwarding_pointer_p(native_pointer(word)))
        ? forwarding_pointer_value(native_pointer(word)) : word;
}
/// As above, but 'ptr' MUST be a pointer.
static inline lispobj follow_fp(lispobj ptr)
{
  return forwarding_pointer_p(native_pointer(ptr))
      ? forwarding_pointer_value(native_pointer(ptr)) : ptr;
}

#define BITS_PER_LOCK_ADDRESS 1
#ifdef LISP_FEATURE_PARALLEL_GC
// Play with this to vary the space-sharing tradeoff.
extern uword_t DYNAMIC_SPACE_START;
#include <stdio.h>

static inline void
release_forwarding_lock(lispobj *pointer) {
  uword_t delta = ((char*)pointer - (char*)DYNAMIC_SPACE_START) / N_WORD_BYTES / 2 * BITS_PER_LOCK_ADDRESS;
  uword_t word_index = delta / N_WORD_BITS;
  uword_t bit_index = delta % N_WORD_BITS;
  uword_t bit_to_set = (uword_t)(1) << bit_index;
  __atomic_fetch_and(gc_object_lock_table + word_index, ~bit_to_set, __ATOMIC_RELEASE);
}

static inline boolean
grab_forwarding_lock(lispobj *pointer) {
  uword_t delta = ((char*)pointer - (char*)DYNAMIC_SPACE_START) / N_WORD_BYTES / 2 * BITS_PER_LOCK_ADDRESS;
  uword_t word_index = delta / N_WORD_BITS;
  uword_t bit_index = delta % N_WORD_BITS;
  uword_t bit_to_set = (uword_t)(1) << bit_index;
  while (1) {
    if (forwarding_pointer_p(pointer))
      /* Lost the race. */
      return false;
    if (!(__atomic_fetch_or(gc_object_lock_table + word_index, bit_to_set, __ATOMIC_ACQUIRE) & bit_to_set))
      /* We successfully flipped the bit; this object belongs to us
         now. */ {
      if (forwarding_pointer_p(pointer)) {
        /* Still lost the race in the end. */
        release_forwarding_lock(pointer);
        return false;
      }
      return true;
    }
  }
}

static inline boolean
is_locked_p(lispobj *pointer) {
  uword_t delta = ((char*)pointer - (char*)DYNAMIC_SPACE_START) / N_WORD_BYTES / 2 * BITS_PER_LOCK_ADDRESS;
  uword_t word_index = delta / N_WORD_BITS;
  uword_t bit_index = delta % N_WORD_BITS;
  uword_t bit_to_set = (uword_t)(1) << bit_index;
  return gc_object_lock_table[word_index] & bit_to_set;
}

#else
static inline boolean
grab_forwarding_lock(lispobj *pointer) {
  /* Only this thread has access, so don't actually lock. */
  return !forwarding_pointer_p(pointer);
}
static inline void
release_forwarding_lock(lispobj *pointer) {
  /* Ditto - don't actually unlock either. */
  (void)pointer;
}
static inline boolean
is_locked_p(lispobj *pointer) {
  (void)pointer;
  return false;
}
#endif
#endif
