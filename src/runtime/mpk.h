#ifndef MPK_H
#define MPK_H
#include "runtime.h"

#ifdef LISP_FEATURE_MPK
extern void init_lisp_pkey(unsigned char* start, uword_t length);
extern void mpk_lock_card_table(void);
extern void mpk_unlock_card_table(void);
#else
#define init_lisp_pkey(start, length)
#define mpk_lock_card_table()
#define mpk_unlock_card_table()
#endif

#endif
