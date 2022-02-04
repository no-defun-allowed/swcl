#ifndef _FORWARDING_PTR_H_
#define _FORWARDING_PTR_H_

#define FORWARDING_POINTER_MAGIC 0x01
#define TRANSPORT_LOCK_MAGIC     0x02

#ifndef LISP_FEATURE_GENCGC
inline static boolean
in_gc_p(void) {
    return current_dynamic_space == from_space;
}
#endif

inline static boolean
forwarding_pointer_p(lispobj *pointer) {
    lispobj first_word=*pointer;
#ifdef LISP_FEATURE_GENCGC
# ifdef LISP_FEATURE_PARALLEL_GC
    return (first_word == FORWARDING_POINTER_MAGIC || first_word == TRANSPORT_LOCK_MAGIC);
# else
    return (first_word == FORWARDING_POINTER_MAGIC);
# endif
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
    /* We do an atomic load so that we will eventually read the write. */
    while (__atomic_load_n(pointer, __ATOMIC_SEQ_CST) == TRANSPORT_LOCK_MAGIC);
    return pointer[1];
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
# ifdef LISP_FEATURE_PARALLEL_GC
    pointer[1]=newspace_copy;
    /* This really must happen after storing the forwarding pointer. */
    __atomic_store(pointer, FORWARDING_POINTER_MAGIC, __ATOMIC_SEQ_CST);
# else
    pointer[0]=FORWARDING_POINTER_MAGIC;
    pointer[1]=newspace_copy;
#endif
#else
    pointer[0]=newspace_copy;
#endif
    return newspace_copy;
}

/* Try to claim the object at 'pointer', and if we win, optionally
   write the clobbered header to 'write_header'. */
static inline boolean
grab_forwarding_pointer(lispobj *pointer, lispobj *write_header) {
#ifdef LISP_FEATURE_PARALLEL_GC
  while (1) {
    lispobj read = __atomic_load_n(pointer, __ATOMIC_SEQ_CST);
    if (read == FORWARDING_POINTER_MAGIC)
      /* Another thread already installed a forwarding pointer. */
      return false;
    if (read != TRANSPORT_LOCK_MAGIC &&
        __atomic_compare_exchange_n(pointer, &read, TRANSPORT_LOCK_MAGIC,
                                    false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) {
      /* We just claimed this object. */
      if (write_header) *write_header = read;
      return true;
    }
  }
#else
  if (write_header) *write_header = pointer[0];
  return !forwarding_pointer_p(pointer);
#endif
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

#endif
