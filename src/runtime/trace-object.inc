// -*- mode: c -*-

#include "weak-hash-pred.inc"

/* Function name to call with every pointer */
#ifndef ACTION
#error "Need ACTION to be defined"
#endif
#define NORMAL(x) ACTION(x, &x, SOURCE_NORMAL)

/* Function name to call with weak pointers that have been deferred */
#ifndef WATCH_DEFERRED
#define WATCH_DEFERRED(where, from, to) (void)(0)
#endif

/* Base name for generated tracing functions */
#ifndef TRACE_NAME
#error "Need TRACE_NAME to be defined"
#endif

/* Mutex stuff */
#ifndef LOCK
#define LOCK (void)(0)
#define UNLOCK (void)(0)
#endif

/* Generated names */
#define cat2(x, y) x ## y
#define cat(x, y) cat2(x, y)
#define USING_LAYOUT cat(TRACE_NAME, _using_layout)
#define TRACE_PAIR cat(TRACE_NAME, _pair)

/* Ignore weak vectors and pointers */
#ifndef DO_WEAKNESS_P
#define DO_WEAKNESS_P 1
#endif

static void TRACE_PAIR(lispobj* where)
{
    NORMAL(where[0]);
    NORMAL(where[1]);
}

static void USING_LAYOUT(lispobj layout, lispobj* where, int nslots)
{
    // Apart from the allowance for untagged pointers in lockfree list nodes,
    // this contains almost none of the special cases that gencgc does.
    if (!layout) return;
    ACTION(layout, &layout_of(where), SOURCE_NORMAL);
    if (lockfree_list_node_layout_p(LAYOUT(layout))) { // allow untagged 'next'
        struct list_node* node = (void*)where;
        lispobj next = node->_node_next;
        // ignore if 0
        if (fixnump(next) && next)
            ACTION(next|INSTANCE_POINTER_LOWTAG, &node->_node_next, SOURCE_ZERO_TAG);
    }
    struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
    int i;
    lispobj* slots = where+1;
    for (i=0; i<nslots; ++i)
        if (bitmap_logbitp(i, bitmap) && is_lisp_pointer(slots[i]))
            NORMAL(slots[i]);
}

static void TRACE_NAME(lispobj* where)
{
    lispobj header = *where;
    int widetag = header_widetag(header);

    if (instanceoid_widetag_p(widetag))
        return USING_LAYOUT(layout_of(where), where,
                            instanceoid_length(header));
    sword_t scan_from = 1;
    sword_t scan_to = sizetab[widetag](where);
    sword_t i;
    struct weak_pointer *weakptr;
    switch (widetag) {
    case SIMPLE_VECTOR_WIDETAG:
#ifdef LISP_FEATURE_UBSAN
        if (is_lisp_pointer(where[1])) NORMAL(where[1]);
#endif
        /* Would be more precise to only make hash-tables rehash when
         * some key actually moves, and we don't really trace large and
         * old tables when scavenging properly. So we _don't_ touch
         * rehash bits here. */
        // non-weak hashtable kv vectors are trivial in fullcgc. Keys don't move
        // so the table will not need rehash as a result of gc.
        // Ergo, those may be treated just like ordinary simple vectors.
        // However, weakness remains as a special case.
        if (DO_WEAKNESS_P && vector_flagp(header, VectorWeak)) {
            WATCH_DEFERRED(where, 1, scan_to);
            if (!vector_flagp(header, VectorHashing)) {
                LOCK;
                add_to_weak_vector_list(where, header);
                UNLOCK;
                return;
            }
            // Ok, we're looking at a weak hash-table.
            struct vector* v = (struct vector*)where;
            lispobj *lhash_table = &v->data[vector_len(v)-1];
            gc_dcheck(instancep(*lhash_table));
            ACTION(*lhash_table, lhash_table, SOURCE_NORMAL);
            struct hash_table* hash_table
              = (struct hash_table *)native_pointer(*lhash_table);
            gc_assert(hashtable_weakp(hash_table));
            // An object can only be removed from the queue once.
            // Therefore the 'next' pointer has got to be nil.
            gc_assert(hash_table->next_weak_hash_table == NIL);
            int weakness = hashtable_weakness(hash_table);
            boolean defer = 1;
            LOCK;
            if (weakness != WEAKNESS_KEY_AND_VALUE)
                defer = scan_weak_hashtable(hash_table, HT_ENTRY_LIVENESS_FUN_ARRAY_NAME[weakness],
                                            TRACE_PAIR);
            if (defer) {
                hash_table->next_weak_hash_table = (lispobj)weak_hash_tables;
                weak_hash_tables = hash_table;
            }
            UNLOCK;
            return;
        }
        break;
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64) || defined (LISP_FEATURE_ARM64)
    /* on x86[-64], closure->fun is a fixnum-qua-pointer. Convert it to a lisp
     * pointer to mark it, but not on platforms where it's already a descriptor */
    case CLOSURE_WIDETAG: {
        lispobj *self = &((struct closure*)where)->fun;
        ACTION(fun_taggedptr_from_self(*self), self, SOURCE_CLOSURE);
        scan_from = 2;
        break; // scan slots normally
    }
#endif
    case CODE_HEADER_WIDETAG:
        scan_to = code_header_words((struct code*)where);
#ifdef LISP_FEATURE_UNTAGGED_FDEFNS
        struct code* code = (struct code*)where;
        lispobj* fdefns_start = code->constants
                                + code_n_funs(code) * CODE_SLOTS_PER_SIMPLE_FUN;
        lispobj* fdefns_end  = fdefns_start + code_n_named_calls(code);
        lispobj* limit = where + scan_to;
        where = where + scan_from;
        while (where < limit) {
            lispobj word = *where;
            enum source source = SOURCE_NORMAL;
            if (where >= fdefns_start && where < fdefns_end) {
                word |= OTHER_POINTER_LOWTAG;
                source = SOURCE_ZERO_TAG;
            }
            ACTION(word, where, source);
            ++where;
        }
        return;
#endif
        break;
    case SYMBOL_WIDETAG:
        {
        struct symbol* s = (void*)where;
        ACTION(decode_symbol_name(s->name), &s->name, SOURCE_SYMBOL_NAME);
        NORMAL(s->value);
        NORMAL(s->info);
        NORMAL(s->fdefn);
        }
        return;
    case FDEFN_WIDETAG: {
        struct fdefn *fdefn = (struct fdefn*)where;
        ACTION(decode_fdefn_rawfun(fdefn), (lispobj*)&fdefn->raw_addr, SOURCE_FDEFN_RAW);
        scan_to = 3;
        break;
    }
    case WEAK_POINTER_WIDETAG:
        weakptr = (struct weak_pointer*)where;
        if (!DO_WEAKNESS_P) ACTION(weakptr->value, &weakptr->value, SOURCE_NORMAL);
        else if (is_lisp_pointer(weakptr->value) && interesting_pointer_p(weakptr->value)) {
            /* XXX: This hard-codes the layout of a weak pointer. Make a scalar WATCH_DEFERRED? */
            WATCH_DEFERRED(where, 1, 2);
            LOCK;
            add_to_weak_pointer_chain(weakptr);
            UNLOCK;
        }
        return;
    default:
        if (leaf_obj_widetag_p(widetag)) return;
    }
    for(i=scan_from; i<scan_to; ++i)
        NORMAL(where[i]);
}

#undef NORMAL