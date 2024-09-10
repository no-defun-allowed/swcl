#define _GNU_SOURCE
#include <sys/mman.h>
#include <immintrin.h>
#include <stdio.h>
#include "interr.h"
#include "runtime.h"

int cards_pkey;

#ifdef LISP_FEATURE_MPK
void init_lisp_pkey(unsigned char* start, uword_t length) {
  cards_pkey = pkey_alloc(0, 0);
  fprintf(stderr, "cards_pkey = %d, start = %p, length = %lx\n", cards_pkey, start, length);
  if (!cards_pkey) lose("Couldn't allocate a MPK key");
  if (pkey_mprotect(start, length, PROT_READ|PROT_WRITE, cards_pkey)) {
    perror("pkey_mprotect");
    lose("Couldn't pkey_mprotect the card table");
  }
}

static void wrpkru(int pkey) {
  //__asm__ __volatile__ ("wrpkru" : : "a"(pkey), "c"(0), "d"(0));
  __builtin_ia32_wrpkru(pkey);
  _mm_lfence();
}

extern bool gc_active_p;
void mpk_lock_card_table() {
  if (gc_active_p) lose("Whyever would you lock in GC?");
  wrpkru(PKEY_DISABLE_WRITE << (2 * cards_pkey));
}
void mpk_unlock_card_table() {
  if (gc_active_p) lose("Whyever would you unlock in GC?");
  wrpkru(0);
}
#endif
