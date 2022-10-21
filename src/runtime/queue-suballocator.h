/* To allocate queue blocks without mmaping every time, we allocate larger and
 * larger "chunks" of memory. As the size of each subsequent chunk grows
 * exponentially, the metadata needed to represent a mark stack of any size
 * and the number of mmaps grow logarithmically. */
#define INITIAL_SIZE (QBLOCK_BYTES * 16)
#define CHUNKS 20

struct suballocator_chunk {
  uword_t start;
  uword_t free;
  uword_t size;
};
/* suballoc_allocate is lazily written and skips over the first chunk, so
 * add one here to make CHUNKS make sense. No big loss. */
static struct suballocator_chunk chunks[CHUNKS + 1] = { 0 };
static unsigned int current_chunk = 0;
static lock_t suballocator_lock = LOCK_INITIALIZER;

/* Free all memory used by the mark stack, giving back to the OS. */
static void suballoc_release() {
  for (int i = 0; i < CHUNKS; i++) {
    if (chunks[i].start) os_deallocate(chunks[i].start, chunks[i].size);
    chunks[i].start = chunks[i].free = chunks[i].size = 0;
  }
  current_chunk = 0;
}

static struct Qblock *suballoc_allocate() {
  acquire_lock(&suballocator_lock);
  struct suballocator_chunk *chunk = chunks + current_chunk;
  /* Check if we need to make a new chunk first. */
  if (chunk->free == chunk->start + chunk->size) {
    /* This skips over chunk[0]. Oh well. */
    if (current_chunk == CHUNKS) lose("Ran out of suballocator chunks.");
    uword_t size = INITIAL_SIZE << current_chunk++;
    uword_t address = os_allocate(size);
    if (!address) lose("Failed to allocate suballocator chunk with %ld bytes.", size);
    // fprintf(stderr, "alloc %ld at %ld\n", size, address);
    chunk = chunks + current_chunk;
    chunk->start = chunk->free = address;
    chunk->size = size;
  }

  /* Now get another block from the current chunk. */
  uword_t where = chunk->free;
  chunk->free += QBLOCK_BYTES;
  release_lock(&suballocator_lock);
  return (struct Qblock*)where;
}
