/* Incremental Generational Garbage Collector
 *
 * This is a two-generation copying/mark-sweep GC based on the generational hypothesis:
 * "Most objects die young." By separating objects by age, we can collect young objects
 * frequently (which is fast) and old objects rarely (which is slow).
 *
 * ARCHITECTURE:
 *
 * 1. YOUNG GENERATION (512 KB)
 *    - Uses Cheney's copying collector algorithm
 *    - Two semispaces: from-space and to-space
 *    - Only one is active at a time
 *    - During GC, live objects are copied from from-space to to-space
 *    - After copying, the spaces are swapped
 *    - Very fast collection (pause time proportional to live set, not total heap)
 *
 * 2. OLD GENERATION (4 MB)
 *    - Uses mark-sweep with compaction
 *    - Objects promoted here after surviving 5 young collections
 *    - Collected less frequently (only when full)
 *    - Slower but handles long-lived objects efficiently
 *
 * 3. WRITE BARRIERS
 *    - Track old→young pointers in a "remembered set"
 *    - Ensures young objects referenced only from old gen aren't collected
 *    - Called automatically on every pointer store
 *
 * 4. ROOT REGISTRATION
 *    - Roots are starting points for GC (stack, globals, registers)
 *    - Explicit API: gc_add_root() / gc_remove_root()
 *    - During GC, all objects reachable from roots are kept alive
 */

#define _POSIX_C_SOURCE 200809L
#include "habu.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* Heap configuration */

#define YOUNG_GEN_SIZE (512 * 1024)    /* 512 KB - one semispace */
#define OLD_GEN_SIZE (4 * 1024 * 1024) /* 4 MB */

/* GC states for incremental collection (not currently used for incremental GC,
 * but reserved for future work where GC can be paused and resumed) */
typedef enum {
    GC_STATE_IDLE,      /* Not currently collecting */
    GC_STATE_MARKING,   /* Marking reachable objects */
    GC_STATE_SWEEPING   /* Reclaiming dead objects */
} gc_state_t;

/* Main GC heap structure
 *
 * This structure manages both generations, the gray stack for marking,
 * roots for GC, and the remembered set for write barriers.
 */
typedef struct gc_heap {
    /* YOUNG GENERATION - Copying collector with two semispaces
     *
     * Allocation is a simple bump-pointer:
     *   - Allocate at young_free
     *   - Increment young_free by object size
     *   - When young_free hits young_end, trigger GC
     *
     * During GC:
     *   - Live objects copied from 'from-space' to 'to-space'
     *   - After copying, swap from/to pointers
     *   - Old from-space becomes new to-space (and is now empty)
     */
    void *young_start;   /* Start of current active semispace */
    void *young_end;     /* End of current active semispace */
    void *young_free;    /* Next free address in active semispace */
    void *young_from;    /* Current from-space (being allocated into) */
    void *young_to;      /* Current to-space (evacuate live objects here) */
    void *young_base;    /* Original allocation base (for free()) */

    /* OLD GENERATION - Mark-sweep collector
     *
     * Allocation is also bump-pointer:
     *   - Allocate at old_free
     *   - Increment old_free by object size
     *   - When old_free hits old_end, trigger old GC
     *
     * During old GC:
     *   - Mark phase: mark all reachable objects
     *   - Sweep phase: scan heap, reclaim dead objects, compact live ones
     */
    void *old_start;     /* Start of old generation heap */
    void *old_end;       /* End of old generation heap */
    void *old_free;      /* Next free address in old generation */

    /* TRICOLOR MARKING STATE
     *
     * Gray stack implements the "gray" set in tricolor marking:
     *   - WHITE: Not yet seen (default state)
     *   - GRAY: Seen, but children not yet processed (on gray stack)
     *   - BLACK: Seen and children processed (popped from gray stack)
     *
     * Algorithm:
     *   1. Start: all objects WHITE, roots become GRAY
     *   2. Pop object from gray stack, mark BLACK
     *   3. For each child: if WHITE, mark GRAY and push to gray stack
     *   4. Repeat until gray stack empty
     *   5. All BLACK objects are live, all WHITE objects are garbage
     */
    gc_state_t state;
    void **gray_stack;          /* Stack of objects to process */
    size_t gray_stack_size;     /* Current stack depth */
    size_t gray_stack_capacity; /* Stack capacity (grows as needed) */

    /* ROOT SET
     *
     * Roots are starting points for garbage collection. Any object
     * reachable from a root is kept alive. Roots include:
     *   - Stack-allocated pointers
     *   - Global variables
     *   - Register values
     *
     * Since we don't have automatic stack scanning yet, roots must be
     * registered explicitly using gc_add_root().
     *
     * IMPORTANT: roots is an array of POINTERS to habu_value_t locations.
     * This allows us to update the caller's variable when GC relocates objects.
     */
    habu_value_t **roots;       /* Array of pointers to root locations */
    size_t roots_size;          /* Number of registered roots */
    size_t roots_capacity;      /* Capacity (grows as needed) */

    /* REMEMBERED SET - Write barrier support
     *
     * The generational hypothesis says most objects die young. We collect
     * young generation frequently. But what if an old object points to a
     * young object that has no other references?
     *
     * Without tracking, we'd incorrectly collect the young object!
     *
     * Solution: Track all old→young pointers in the "remembered set".
     * During young GC, treat remembered set as additional roots.
     *
     * The write barrier (habu_write_barrier) is called on every pointer
     * store to detect and record old→young pointers.
     */
    void **remembered_set;          /* Old objects that point to young */
    size_t remembered_set_size;     /* Number of entries */
    size_t remembered_set_capacity; /* Capacity */

    /* STATISTICS AND METRICS */
    habu_gc_stats_t stats;

    /* ALLOCATION TRACKING
     *
     * We trigger young GC when enough allocation has occurred, not just
     * when the heap is full. This spreads GC pauses more evenly.
     */
    size_t bytes_allocated_since_gc; /* Bytes allocated since last young GC */
    size_t gc_threshold;             /* Trigger young GC after this many bytes */
    size_t old_bytes_allocated;      /* Total bytes in old generation */
} gc_heap_t;

/* Global GC heap - only one heap per process */
static gc_heap_t *gc_heap = NULL;

/* ============================================================================
 * SYMBOL INTERNING TABLE
 * ============================================================================
 *
 * Symbols are interned so that (eq 'foo 'foo) returns true. Each unique
 * symbol name maps to exactly one symbol object.
 *
 * Implementation: Simple hash table with chaining. Entries are malloc'd
 * (not GC-managed) so they're stable across collections. The symbol values
 * themselves are GC-managed and must be treated as roots.
 */

#define SYMBOL_TABLE_SIZE 1024

typedef struct symbol_entry {
    char *name;                    /* malloc'd copy of symbol name */
    habu_value_t symbol;           /* The interned symbol value */
    struct symbol_entry *next;     /* Hash chain */
} symbol_entry_t;

static symbol_entry_t *symbol_table[SYMBOL_TABLE_SIZE];
static size_t symbol_count = 0;

/* djb2 hash function */
static unsigned int hash_string(const char *str) {
    unsigned int hash = 5381;
    int c;
    while ((c = *str++)) {
        hash = ((hash << 5) + hash) + c;
    }
    return hash % SYMBOL_TABLE_SIZE;
}

/* Look up a symbol by name, returns NIL if not found */
static habu_value_t symbol_table_lookup(const char *name) {
    unsigned int idx = hash_string(name);
    for (symbol_entry_t *e = symbol_table[idx]; e != NULL; e = e->next) {
        if (strcmp(e->name, name) == 0) {
            return e->symbol;
        }
    }
    return NIL;
}

/* Add a symbol to the table (assumes it doesn't already exist) */
static void symbol_table_insert(const char *name, habu_value_t symbol) {
    unsigned int idx = hash_string(name);
    symbol_entry_t *entry = malloc(sizeof(symbol_entry_t));
    assert(entry != NULL);
    entry->name = strdup(name);
    assert(entry->name != NULL);
    entry->symbol = symbol;
    entry->next = symbol_table[idx];
    symbol_table[idx] = entry;
    symbol_count++;
}

/* Forward declarations for GC integration */
static void *copy_object(void *obj);
static habu_value_t forward_value(habu_value_t value);
static void push_gray(void *obj);

/* Forward all symbols in the intern table during young GC (copying collector) */
static void forward_symbol_table(void) {
    for (int i = 0; i < SYMBOL_TABLE_SIZE; i++) {
        for (symbol_entry_t *e = symbol_table[i]; e != NULL; e = e->next) {
            if (e->symbol != NIL && is_pointer(e->symbol)) {
                e->symbol = forward_value(e->symbol);
            }
        }
    }
}

/* Mark all symbols in the intern table during old GC (mark-sweep) */
static void mark_symbol_table(void) {
    for (int i = 0; i < SYMBOL_TABLE_SIZE; i++) {
        for (symbol_entry_t *e = symbol_table[i]; e != NULL; e = e->next) {
            if (e->symbol != NIL && is_pointer(e->symbol)) {
                void *obj = untag_pointer(e->symbol);
                if (get_gc_color(obj) == GC_WHITE) {
                    set_gc_color(obj, GC_GRAY);
                    push_gray(obj);
                }
            }
        }
    }
}

/* Fast-path allocation globals for inline code generation
 *
 * These pointers-to-pointers allow generated machine code to perform
 * bump-pointer allocation directly without function call overhead.
 *
 * Implementation note: We export pointers to the fields in gc_heap,
 * not the values themselves. This allows the GC to update these during
 * collection (e.g., when swapping semispaces) without needing to notify
 * generated code.
 */
void **young_free_ptr = NULL;
void **young_end_ptr = NULL;

/* ============================================================================
 * INITIALIZATION AND SHUTDOWN
 * ============================================================================ */

/* Initialize the GC heap
 *
 * This allocates memory for both generations, the gray stack, roots array,
 * and remembered set. Must be called before any heap allocation.
 *
 * Parameters:
 *   heap_size - Requested heap size (currently ignored, uses fixed sizes)
 */
void init(size_t heap_size) {
    (void)heap_size;  /* Use fixed sizes for now */

    /* Allocate the main GC structure */
    gc_heap = malloc(sizeof(gc_heap_t));
    assert(gc_heap != NULL);

    /* Zero-initialize all fields */
    memset(gc_heap, 0, sizeof(gc_heap_t));

    /* YOUNG GENERATION SETUP
     *
     * We allocate TWO semispaces of YOUNG_GEN_SIZE each.
     * Total allocation: 512 KB * 2 = 1 MB
     *
     * Layout in memory:
     *   [from-space: 512KB][to-space: 512KB]
     *
     * Initially:
     *   - from-space is active (we allocate here)
     *   - to-space is empty (we evacuate live objects here during GC)
     *
     * After first GC:
     *   - Spaces are swapped
     *   - Old from-space (now to-space) is empty
     *   - Old to-space (now from-space) contains only live objects
     */
    size_t young_total = YOUNG_GEN_SIZE * 2;
    void *young_mem = aligned_alloc(16, young_total);
    assert(young_mem != NULL);

    gc_heap->young_base = young_mem;  /* Save base pointer for free() */
    gc_heap->young_from = young_mem;  /* First half = from-space */
    gc_heap->young_to = (char *)young_mem + YOUNG_GEN_SIZE;  /* Second half = to-space */
    gc_heap->young_start = gc_heap->young_from;  /* Active semispace start */
    gc_heap->young_end = (char *)gc_heap->young_from + YOUNG_GEN_SIZE;  /* Active semispace end */
    gc_heap->young_free = gc_heap->young_start;  /* Bump pointer starts at beginning */

    /* FAST-PATH ALLOCATION POINTERS
     *
     * Export pointers to young_free and young_end so generated machine code
     * can perform inline bump-pointer allocation. These are pointers-to-pointers
     * so they remain valid even when the GC swaps semispaces.
     */
    young_free_ptr = &gc_heap->young_free;
    young_end_ptr = &gc_heap->young_end;

    /* OLD GENERATION SETUP
     *
     * Single contiguous region of OLD_GEN_SIZE (4 MB).
     * Uses mark-sweep with compaction instead of copying.
     */
    void *old_mem = aligned_alloc(16, OLD_GEN_SIZE);
    assert(old_mem != NULL);

    gc_heap->old_start = old_mem;
    gc_heap->old_end = (char *)old_mem + OLD_GEN_SIZE;
    gc_heap->old_free = old_mem;  /* Bump pointer starts at beginning */

    /* GRAY STACK SETUP
     *
     * Used during marking to track objects that have been seen but whose
     * children haven't been processed yet. Grows dynamically as needed.
     */
    gc_heap->gray_stack_capacity = 1024;
    gc_heap->gray_stack = malloc(gc_heap->gray_stack_capacity * sizeof(void *));
    assert(gc_heap->gray_stack != NULL);
    gc_heap->gray_stack_size = 0;

    /* ROOT SET SETUP
     *
     * Roots must be registered explicitly using gc_add_root().
     * Starts with capacity for 256 roots, grows as needed.
     */
    gc_heap->roots_capacity = 256;
    gc_heap->roots = malloc(gc_heap->roots_capacity * sizeof(void *));
    assert(gc_heap->roots != NULL);
    gc_heap->roots_size = 0;

    /* REMEMBERED SET SETUP
     *
     * Tracks old generation objects that contain pointers to young objects.
     * Populated by write barrier, cleared after each young GC.
     */
    gc_heap->remembered_set_capacity = 256;
    gc_heap->remembered_set = malloc(gc_heap->remembered_set_capacity * sizeof(void *));
    assert(gc_heap->remembered_set != NULL);
    gc_heap->remembered_set_size = 0;

    /* Initial state and thresholds */
    gc_heap->state = GC_STATE_IDLE;
    gc_heap->gc_threshold = YOUNG_GEN_SIZE / 2;  /* Trigger GC after 256 KB allocated */
    gc_heap->old_bytes_allocated = 0;

    /* Initialize I/O system */
    io_init();
}

/* Shutdown and free all GC memory
 *
 * Call this before program exit to clean up all GC-related allocations.
 * After this, gc_alloc() will fail until init() is called again.
 */
void shutdown(void) {
    /* Shutdown I/O system */
    io_shutdown();

    if (gc_heap) {
        /* Free heap regions */
        free(gc_heap->young_base);  /* Free entire young gen (both semispaces) */
        free(gc_heap->old_start);   /* Free old generation */

        /* Free GC data structures */
        free(gc_heap->gray_stack);
        free(gc_heap->roots);
        free(gc_heap->remembered_set);

        /* Free GC structure itself */
        free(gc_heap);
        gc_heap = NULL;
    }
}

/* ============================================================================
 * GRAY STACK OPERATIONS (for tricolor marking)
 * ============================================================================ */

/* Push an object onto the gray stack
 *
 * Called when we discover a new object during marking. The object is
 * added to the gray stack so we can later process its children.
 *
 * The stack grows dynamically (doubles in size when full).
 */
static void push_gray(void *obj) {
    /* Grow stack if needed */
    if (gc_heap->gray_stack_size >= gc_heap->gray_stack_capacity) {
        gc_heap->gray_stack_capacity *= 2;
        gc_heap->gray_stack = realloc(gc_heap->gray_stack,
            gc_heap->gray_stack_capacity * sizeof(void *));
        assert(gc_heap->gray_stack != NULL);
    }

    /* Push object */
    gc_heap->gray_stack[gc_heap->gray_stack_size++] = obj;
}

/* Pop an object from the gray stack
 *
 * Returns the next object to process, or NULL if stack is empty.
 * When this returns NULL, marking is complete.
 */
static void *pop_gray(void) {
    if (gc_heap->gray_stack_size == 0) {
        return NULL;  /* Stack empty - marking complete */
    }
    return gc_heap->gray_stack[--gc_heap->gray_stack_size];
}

/* ============================================================================
 * OBJECT TRAVERSAL (marking children during GC)
 * ============================================================================ */

/* Mark all children of an object
 *
 * This is called during the mark phase for each object popped from the
 * gray stack. For each child pointer:
 *   1. Check if it's a pointer (not a fixnum)
 *   2. Untag the pointer to get the actual object address
 *   3. If the child is WHITE (not yet seen), mark it GRAY and push to gray stack
 *
 * This implements the "mark" step of mark-sweep GC and is also used
 * during old generation collection.
 */
static void mark_children(void *obj) {
    habu_header_t *header = get_header(obj);
    uint64_t type = header->type;

    /* Dispatch based on object type - different types have different pointer fields */
    switch (type) {
        case TYPE_CONS: {
            /* Cons cells have two fields: car and cdr */
            habu_cons_t *cons = (habu_cons_t *)obj;

            /* Check car */
            if (is_pointer(cons->car)) {  /* Skip if it's a fixnum */
                void *car_obj = untag_pointer(cons->car);
                if (get_gc_color(car_obj) == GC_WHITE) {
                    set_gc_color(car_obj, GC_GRAY);
                    push_gray(car_obj);  /* Will process its children later */
                }
            }

            /* Check cdr */
            if (is_pointer(cons->cdr)) {
                void *cdr_obj = untag_pointer(cons->cdr);
                if (get_gc_color(cdr_obj) == GC_WHITE) {
                    set_gc_color(cdr_obj, GC_GRAY);
                    push_gray(cdr_obj);
                }
            }
            break;
        }

        case TYPE_VECTOR: {
            habu_vector_t *vec = (habu_vector_t *)obj;
            for (size_t i = 0; i < vec->length; i++) {
                if (is_pointer(vec->data[i])) {
                    void *elem = untag_pointer(vec->data[i]);
                    if (get_gc_color(elem) == GC_WHITE) {
                        set_gc_color(elem, GC_GRAY);
                        push_gray(elem);
                    }
                }
            }
            break;
        }

        case TYPE_SYMBOL: {
            habu_symbol_t *sym = (habu_symbol_t *)obj;
            if (is_pointer(sym->name)) {
                void *name_obj = untag_pointer(sym->name);
                if (get_gc_color(name_obj) == GC_WHITE) {
                    set_gc_color(name_obj, GC_GRAY);
                    push_gray(name_obj);
                }
            }
            if (is_pointer(sym->value)) {
                void *value_obj = untag_pointer(sym->value);
                if (get_gc_color(value_obj) == GC_WHITE) {
                    set_gc_color(value_obj, GC_GRAY);
                    push_gray(value_obj);
                }
            }
            if (is_pointer(sym->plist)) {
                void *plist_obj = untag_pointer(sym->plist);
                if (get_gc_color(plist_obj) == GC_WHITE) {
                    set_gc_color(plist_obj, GC_GRAY);
                    push_gray(plist_obj);
                }
            }
            break;
        }

        case TYPE_CLOSURE: {
            habu_closure_t *closure = (habu_closure_t *)obj;
            if (is_pointer(closure->env)) {
                void *env_obj = untag_pointer(closure->env);
                if (get_gc_color(env_obj) == GC_WHITE) {
                    set_gc_color(env_obj, GC_GRAY);
                    push_gray(env_obj);
                }
            }
            break;
        }

        case TYPE_HASHTABLE: {
            habu_hashtable_t *ht = (habu_hashtable_t *)obj;
            if (is_pointer(ht->buckets)) {
                void *buckets_obj = untag_pointer(ht->buckets);
                if (get_gc_color(buckets_obj) == GC_WHITE) {
                    set_gc_color(buckets_obj, GC_GRAY);
                    push_gray(buckets_obj);
                }
            }
            break;
        }

        case TYPE_STRING:
        case TYPE_FLOAT:
        case TYPE_BIGNUM:
            /* Strings, floats, and bignums have no outgoing pointers */
            break;

        case TYPE_ARRAY: {
            habu_array_t *arr = (habu_array_t *)obj;
            /* Mark dims vector */
            if (is_pointer(arr->dims)) {
                void *elem = untag_pointer(arr->dims);
                if (get_gc_color(elem) == GC_WHITE) {
                    set_gc_color(elem, GC_GRAY);
                    push_gray(elem);
                }
            }
            /* Mark data vector */
            if (is_pointer(arr->data)) {
                void *elem = untag_pointer(arr->data);
                if (get_gc_color(elem) == GC_WHITE) {
                    set_gc_color(elem, GC_GRAY);
                    push_gray(elem);
                }
            }
            break;
        }

        default:
            break;
    }
}

/* Incremental marking */

void gc_mark_increment(int work_units) {
    for (int i = 0; i < work_units && gc_heap->gray_stack_size > 0; i++) {
        void *obj = pop_gray();
        if (obj == NULL) break;

        /* Mark black */
        set_gc_color(obj, GC_BLACK);

        /* Mark children */
        mark_children(obj);
    }

    if (gc_heap->gray_stack_size == 0) {
        gc_heap->state = GC_STATE_SWEEPING;
    }
}

/* Mark roots */

static void mark_roots(void) {
    for (size_t i = 0; i < gc_heap->roots_size; i++) {
        void *root = gc_heap->roots[i];
        if (root && get_gc_color(root) == GC_WHITE) {
            set_gc_color(root, GC_GRAY);
            push_gray(root);
        }
    }
}

/* Remembered set management */

static void add_to_remembered_set(void *obj) {
    if (!gc_heap || !obj) return;

    /* Check if already in remembered set */
    for (size_t i = 0; i < gc_heap->remembered_set_size; i++) {
        if (gc_heap->remembered_set[i] == obj) {
            return;  /* Already remembered */
        }
    }

    /* Grow if needed */
    if (gc_heap->remembered_set_size >= gc_heap->remembered_set_capacity) {
        size_t new_capacity = gc_heap->remembered_set_capacity * 2;
        void **new_set = realloc(gc_heap->remembered_set, new_capacity * sizeof(void *));
        if (!new_set) return;  /* Failed to grow */
        gc_heap->remembered_set = new_set;
        gc_heap->remembered_set_capacity = new_capacity;
    }

    gc_heap->remembered_set[gc_heap->remembered_set_size++] = obj;
}

static void clear_remembered_set(void) {
    if (gc_heap) {
        gc_heap->remembered_set_size = 0;
    }
}

/* Write barrier - call when storing pointer into old gen object */
void write_barrier(void *obj, habu_value_t value) {
    if (!gc_heap || !obj || !is_pointer(value)) return;

    /* Only track old→young pointers */
    if (in_old_gen(obj)) {
        void *target = untag_pointer(value);
        if (target && !in_old_gen(target)) {
            add_to_remembered_set(obj);
        }
    }
}

/* Mark objects in old generation */
static void mark_old_generation(void) {
    /* Mark from roots */
    mark_roots();

    /* Mark interned symbols - they must survive */
    mark_symbol_table();

    /* Process gray stack */
    while (gc_heap->gray_stack_size > 0) {
        void *obj = pop_gray();
        if (!obj) continue;

        set_gc_color(obj, GC_BLACK);
        mark_children(obj);
    }
}

/* Sweep old generation, freeing unmarked objects */
static size_t sweep_old_generation(void) {
    size_t freed = 0;
    void *scan = gc_heap->old_start;
    void *compact_to = gc_heap->old_start;

    while (scan < gc_heap->old_free) {
        habu_header_t *header = (habu_header_t *)scan;
        size_t obj_size = sizeof(habu_header_t) + header->size;
        obj_size = (obj_size + 15) & ~15;

        void *obj = (char *)scan + sizeof(habu_header_t);

        if (get_gc_color(obj) == GC_WHITE) {
            /* Dead object - don't copy it */
            freed += obj_size;
        } else {
            /* Live object - compact it */
            if (scan != compact_to) {
                memmove(compact_to, scan, obj_size);
            }
            /* Reset color for next cycle */
            habu_header_t *new_header = (habu_header_t *)compact_to;
            new_header->gc_color = GC_WHITE;

            compact_to = (char *)compact_to + obj_size;
        }

        scan = (char *)scan + obj_size;
    }

    /* Update free pointer */
    gc_heap->old_free = compact_to;

    return freed;
}

/* Full old generation collection */
static void collect_old_generation(void) {
    if (!gc_heap) return;

    uint64_t start = time_ns();

    /* Mark phase */
    gc_heap->state = GC_STATE_MARKING;
    mark_old_generation();

    /* Sweep and compact phase */
    gc_heap->state = GC_STATE_SWEEPING;
    size_t freed = sweep_old_generation();

    gc_heap->stats.old_collections++;
    gc_heap->stats.total_freed += freed;
    gc_heap->old_bytes_allocated = (char *)gc_heap->old_free - (char *)gc_heap->old_start;

    gc_heap->state = GC_STATE_IDLE;

    uint64_t end = time_ns();
    uint64_t pause = end - start;

    gc_heap->stats.last_pause_ns = pause;
    if (pause > gc_heap->stats.max_pause_ns) {
        gc_heap->stats.max_pause_ns = pause;
    }
}

/* ============================================================================
 * COPYING COLLECTOR (Cheney's algorithm for young generation)
 * ============================================================================ */

/* Forward declarations */
static void *copy_object(void *obj);
static habu_value_t forward_value(habu_value_t value);

/* Copy an object from from-space to to-space (or old generation)
 *
 * This is the heart of Cheney's copying collector. When we encounter an
 * object during young GC, we must:
 *   1. Check if it's already been copied (avoid duplicates)
 *   2. Allocate space in to-space (or old gen if promoting)
 *   3. Copy the object bytes
 *   4. Leave a "forwarding pointer" at the old location
 *   5. Return the new address
 *
 * FORWARDING POINTERS:
 * When an object is copied, we leave a breadcrumb at the old location.
 * If we encounter the same object again (via multiple pointers to it),
 * we follow the forwarding pointer instead of copying again.
 *
 * We use gc_color=3 as a "forwarded" flag, and store the new address
 * in the padding field of the header.
 *
 * PROMOTION:
 * Objects that have survived GEN_YOUNG_MAX (5) collections are "old enough"
 * and get promoted to old generation instead of being copied to to-space.
 */
static void *copy_object(void *obj) {
    if (!obj) return NULL;

    habu_header_t *header = get_header(obj);

    /* STEP 1: Check if already forwarded
     *
     * If this object was already copied during this GC, its old location
     * contains a forwarding pointer to the new location.
     */
    if (header->gc_color == 3) {
        /* Already forwarded - return new address */
        return (void *)header->padding;
    }

    /* STEP 2: Calculate object size (header + data, aligned to 16 bytes) */
    size_t obj_size = sizeof(habu_header_t) + header->size;
    obj_size = (obj_size + 15) & ~15;  /* Align to 16-byte boundary */

    void *new_addr;

    /* STEP 3: Decide where to copy - to-space or old generation?
     *
     * PROMOTION LOGIC:
     * Objects that have survived GEN_YOUNG_MAX (5) young GCs are "tenured"
     * and promoted to old generation. This implements the generational
     * hypothesis: objects that survive this long are likely long-lived.
     */
    if (header->gen_age >= GEN_YOUNG_MAX) {
        /* PROMOTION PATH: Copy to old generation */

        new_addr = gc_heap->old_free;
        void *new_free = (char *)new_addr + obj_size;

        /* Check if old generation has space */
        if (new_free > gc_heap->old_end) {
            /* Old gen full - trigger old gen collection to make space */
            collect_old_generation();

            /* Try again after collection */
            new_addr = gc_heap->old_free;
            new_free = (char *)new_addr + obj_size;

            if (new_free > gc_heap->old_end) {
                /* Still no space - heap exhausted */
                return NULL;
            }
        }

        /* Allocate in old generation */
        gc_heap->old_free = new_free;
        gc_heap->old_bytes_allocated += obj_size;
    } else {
        /* NORMAL PATH: Copy to young to-space */

        new_addr = gc_heap->young_free;
        void *new_free = (char *)new_addr + obj_size;

        /* Check if to-space has room */
        if (new_free > gc_heap->young_end) {
            /* To-space full - EMERGENCY PROMOTION
             *
             * Normally objects are promoted based on age. But if to-space
             * is full, we have no choice but to promote young objects early.
             * This can happen if the live set is larger than one semispace.
             */
            new_addr = gc_heap->old_free;
            new_free = (char *)new_addr + obj_size;

            if (new_free > gc_heap->old_end) {
                /* Old gen also full - trigger old gen collection */
                collect_old_generation();

                new_addr = gc_heap->old_free;
                new_free = (char *)new_addr + obj_size;

                if (new_free > gc_heap->old_end) {
                    /* Both spaces full - heap exhausted */
                    return NULL;
                }
            }

            /* Emergency promote to old generation */
            gc_heap->old_free = new_free;
            gc_heap->old_bytes_allocated += obj_size;
            header->gen_age = GEN_OLD;  /* Force to old gen */
        } else {
            /* Room in to-space - allocate there */
            gc_heap->young_free = new_free;
        }
    }

    /* STEP 4: Copy the object
     *
     * Use memcpy to copy the entire object (header + data) to new location.
     */
    memcpy(new_addr, header, obj_size);

    /* STEP 5: Update metadata in the new copy */
    habu_header_t *new_header = (habu_header_t *)new_addr;

    /* Increment generation age (unless already old) */
    if (new_header->gen_age < GEN_OLD) {
        new_header->gen_age++;
    }

    /* Reset color for next GC cycle */
    new_header->gc_color = GC_WHITE;

    /* STEP 6: Leave forwarding pointer at old location
     *
     * Mark the old copy as "forwarded" (gc_color=3) and store the new
     * address in the padding field. This prevents duplicate copying if
     * we encounter this object again via another pointer.
     */
    header->gc_color = 3;  /* Forwarded marker */
    header->padding = (uint64_t)((char *)new_addr + sizeof(habu_header_t));

    /* Return address of new object (skip past header) */
    return (char *)new_addr + sizeof(habu_header_t);
}

/* Forward a tagged value (habu_value_t)
 *
 * Tagged values can be either:
 *   - Fixnums (immediate values, no pointer) - return unchanged
 *   - Pointers (heap objects) - untag, copy, retag with new address
 */

/* Forward a tagged value */
static habu_value_t forward_value(habu_value_t value) {
    if (!is_pointer(value)) {
        return value;  /* Fixnums don't need forwarding */
    }

    void *obj = untag_pointer(value);
    void *new_obj = copy_object(obj);

    if (!new_obj) {
        return value;  /* Failed to copy, keep old value */
    }

    return tag_pointer(new_obj, get_tag(value));
}

/* Update pointers in a copied object */
static void update_object_pointers(void *obj) {
    habu_header_t *header = get_header(obj);
    uint64_t type = header->type;

    switch (type) {
        case TYPE_CONS: {
            habu_cons_t *cons = (habu_cons_t *)obj;
            cons->car = forward_value(cons->car);
            cons->cdr = forward_value(cons->cdr);
            break;
        }

        case TYPE_VECTOR: {
            habu_vector_t *vec = (habu_vector_t *)obj;
            for (size_t i = 0; i < vec->length; i++) {
                vec->data[i] = forward_value(vec->data[i]);
            }
            break;
        }

        case TYPE_SYMBOL: {
            habu_symbol_t *sym = (habu_symbol_t *)obj;
            sym->name = forward_value(sym->name);
            sym->value = forward_value(sym->value);
            sym->plist = forward_value(sym->plist);
            break;
        }

        case TYPE_CLOSURE: {
            habu_closure_t *closure = (habu_closure_t *)obj;
            closure->env = forward_value(closure->env);
            break;
        }

        case TYPE_HASHTABLE: {
            habu_hashtable_t *ht = (habu_hashtable_t *)obj;
            ht->buckets = forward_value(ht->buckets);
            break;
        }

        case TYPE_STRING:
        case TYPE_FLOAT:
        case TYPE_BIGNUM:
            /* Strings, floats, and bignums have no outgoing pointers */
            break;

        case TYPE_ARRAY: {
            habu_array_t *arr = (habu_array_t *)obj;
            arr->dims = forward_value(arr->dims);
            arr->data = forward_value(arr->data);
            break;
        }

        default:
            break;
    }
}

/* ============================================================================
 * YOUNG GENERATION COLLECTION (Cheney's Algorithm)
 * ============================================================================
 *
 * This function implements a full young generation collection using
 * Cheney's copying algorithm. The algorithm works in phases:
 *
 * PHASE 1: FLIP - Swap from-space and to-space
 *   Old from-space becomes new to-space (now empty)
 *   Old to-space becomes new from-space (will be scanned)
 *   Reset allocation pointer to start of new from-space
 *
 * PHASE 2: COPY ROOTS - Evacuate objects reachable from roots
 *   Copy each root object from old from-space to new from-space
 *   This initializes the "scan queue" (implicit in Cheney's algorithm)
 *
 * PHASE 3: PROCESS REMEMBERED SET - Handle old→young pointers
 *   Update pointers in old gen objects that reference young objects
 *   This ensures young objects referenced only from old gen survive
 *
 * PHASE 4: SCAN - Process copied objects (Cheney's "scan" phase)
 *   Scan through new from-space from start to young_free
 *   For each object, forward all its pointers
 *   Forwarding may copy more objects, extending young_free
 *   Continue until scan pointer catches up to young_free
 *
 * After collection:
 *   - All live objects are in new from-space (compacted at start)
 *   - All dead objects were left behind in old from-space (now to-space)
 *   - To-space is completely empty and ready for next GC
 */
void gc_collect(void) {
    /* Safety checks */
    if (!gc_heap || !gc_enabled) {
        return;
    }

    /* Start timing */
    uint64_t start = time_ns();
    size_t young_before = (char *)gc_heap->young_free - (char *)gc_heap->young_start;

    /* PHASE 1: FLIP THE SEMISPACES
     *
     * This is the "flip" step of the copying collector.
     * We swap which semispace is active.
     *
     * Before:
     *   from-space: [live objects + dead objects]  (being allocated into)
     *   to-space:   [empty]                        (reserved for next GC)
     *
     * After:
     *   from-space: [empty]                        (will receive copied objects)
     *   to-space:   [live objects + dead objects]  (old data, will be abandoned)
     */
    void *old_from = gc_heap->young_from;
    void *old_to = gc_heap->young_to;

    gc_heap->young_from = old_to;   /* New from = old to (now empty) */
    gc_heap->young_to = old_from;   /* New to = old from (has old data) */
    gc_heap->young_start = gc_heap->young_from;
    gc_heap->young_end = (char *)gc_heap->young_from + YOUNG_GEN_SIZE;
    gc_heap->young_free = gc_heap->young_start;  /* Start allocating at beginning */

    /* PHASE 2: COPY ROOT OBJECTS
     *
     * Roots are the starting points for reachability. Any object not
     * reachable from a root is garbage. We copy all root objects first.
     *
     * CRITICAL: roots[i] is a POINTER to a habu_value_t location (e.g., &variable).
     * We must dereference it to get the value, then UPDATE the caller's location.
     */
    for (size_t i = 0; i < gc_heap->roots_size; i++) {
        habu_value_t *root_location = gc_heap->roots[i];
        if (root_location && *root_location != NIL) {
            habu_value_t root_value = *root_location;

            /* Only process if it's a pointer (not a fixnum) */
            if (is_pointer(root_value)) {
                void *root_ptr = untag_pointer(root_value);
                uint64_t tag = get_tag(root_value);

                /* Copy root object to new from-space */
                void *new_ptr = copy_object(root_ptr);
                if (new_ptr) {
                    /* CRITICAL: Update the CALLER'S variable, not our internal array */
                    *root_location = tag_pointer(new_ptr, tag);
                }
            }
        }
    }

    /* PHASE 2b: FORWARD SYMBOL TABLE
     *
     * Interned symbols must survive GC - forward all symbols in the intern table.
     */
    forward_symbol_table();

    /* PHASE 3: PROCESS REMEMBERED SET
     *
     * The remembered set contains old generation objects that have pointers
     * to young generation objects. We need to update these pointers so they
     * point to the new locations after copying.
     *
     * This is critical for correctness: without this, old objects would
     * point to dead memory in the old from-space!
     */
    for (size_t i = 0; i < gc_heap->remembered_set_size; i++) {
        void *old_obj = gc_heap->remembered_set[i];
        if (old_obj) {
            /* Update all pointers in this old gen object */
            update_object_pointers(old_obj);
        }
    }

    /* Clear remembered set - will be repopulated by write barriers */
    clear_remembered_set();

    /* PHASE 4: SCAN - Cheney's scan phase
     *
     * This is the heart of Cheney's algorithm. We scan through the copied
     * objects and forward their pointers. Key insight: as we forward pointers,
     * we may copy more objects, which extends young_free. We continue scanning
     * until scan pointer catches up to young_free.
     *
     * This implements a breadth-first traversal of the object graph without
     * needing an explicit queue - the to-space itself IS the queue!
     */
    void *scan = gc_heap->young_start;
    while (scan < gc_heap->young_free) {
        habu_header_t *header = (habu_header_t *)scan;
        void *obj = (char *)scan + sizeof(habu_header_t);

        /* Forward all pointers in this object */
        update_object_pointers(obj);

        /* Move to next object */
        size_t obj_size = sizeof(habu_header_t) + header->size;
        obj_size = (obj_size + 15) & ~15;
        scan = (char *)scan + obj_size;
    }

    /* Update statistics */
    size_t young_after = (char *)gc_heap->young_free - (char *)gc_heap->young_start;
    gc_heap->stats.young_collections++;
    gc_heap->stats.total_freed += (young_before > young_after) ?
                                    (young_before - young_after) : 0;

    gc_heap->state = GC_STATE_IDLE;

    uint64_t end = time_ns();
    uint64_t pause = end - start;

    gc_heap->stats.last_pause_ns = pause;
    if (pause > gc_heap->stats.max_pause_ns) {
        gc_heap->stats.max_pause_ns = pause;
    }
}

/* Root registration API
 *
 * Takes a POINTER to a habu_value_t, not the value itself.
 * This allows GC to update the caller's variable when objects are relocated.
 *
 * Example usage:
 *   habu_value_t my_obj = cons(a, b);
 *   gc_add_root(&my_obj);  // Pass ADDRESS of variable
 *   // ... allocations that might trigger GC ...
 *   gc_remove_root(&my_obj);
 */

void gc_add_root(habu_value_t *root_location) {
    if (!gc_heap || !root_location) {
        return;
    }

    /* Check if already registered */
    for (size_t i = 0; i < gc_heap->roots_size; i++) {
        if (gc_heap->roots[i] == root_location) {
            return;  /* Already registered */
        }
    }

    /* Grow array if needed */
    if (gc_heap->roots_size >= gc_heap->roots_capacity) {
        size_t new_capacity = gc_heap->roots_capacity * 2;
        habu_value_t **new_roots = realloc(gc_heap->roots, new_capacity * sizeof(habu_value_t *));
        if (!new_roots) {
            return;  /* Failed to grow, silently ignore */
        }
        gc_heap->roots = new_roots;
        gc_heap->roots_capacity = new_capacity;
    }

    gc_heap->roots[gc_heap->roots_size++] = root_location;
}

void gc_remove_root(habu_value_t *root_location) {
    if (!gc_heap || !root_location) {
        return;
    }

    /* Find and remove the root */
    for (size_t i = 0; i < gc_heap->roots_size; i++) {
        if (gc_heap->roots[i] == root_location) {
            /* Shift remaining elements down */
            for (size_t j = i; j < gc_heap->roots_size - 1; j++) {
                gc_heap->roots[j] = gc_heap->roots[j + 1];
            }
            gc_heap->roots_size--;
            return;
        }
    }
}

/* Allocation */

void *gc_alloc(size_t bytes, uint64_t type) {
    if (!gc_heap) {
        return NULL;
    }

    size_t total_size = sizeof(habu_header_t) + bytes;
    total_size = (total_size + 15) & ~15;  /* Align to 16 bytes */

    /* Try young generation first */
    void *addr = gc_heap->young_free;
    void *new_free = (char *)addr + total_size;

    if (new_free <= gc_heap->young_end) {
        gc_heap->young_free = new_free;

        /* Set header */
        habu_header_t *header = (habu_header_t *)addr;
        header->type = type;
        header->size = bytes;
        header->gc_color = GC_WHITE;
        header->gen_age = 0;
        header->reserved = 0;
        header->padding = 0;

        gc_heap->bytes_allocated_since_gc += total_size;
        gc_heap->stats.total_allocated += total_size;

        /* Check if GC needed */
        if (gc_enabled &&
            gc_heap->bytes_allocated_since_gc >= gc_heap->gc_threshold) {
            gc_collect();
            gc_heap->bytes_allocated_since_gc = 0;
        }

        return (char *)addr + sizeof(habu_header_t);
    }

    /* Young gen full - try GC first */
    if (gc_enabled) {
        gc_collect();
        gc_heap->bytes_allocated_since_gc = 0;

        /* Try young gen again after GC */
        addr = gc_heap->young_free;
        new_free = (char *)addr + total_size;

        if (new_free <= gc_heap->young_end) {
            gc_heap->young_free = new_free;

            habu_header_t *header = (habu_header_t *)addr;
            header->type = type;
            header->size = bytes;
            header->gc_color = GC_WHITE;
            header->gen_age = 0;
            header->reserved = 0;
            header->padding = 0;

            gc_heap->stats.total_allocated += total_size;
            return (char *)addr + sizeof(habu_header_t);
        }
    }

    /* Still no space, try old generation */
    addr = gc_heap->old_free;
    new_free = (char *)addr + total_size;

    if (new_free > gc_heap->old_end) {
        /* Old gen full - trigger old gen collection */
        collect_old_generation();

        addr = gc_heap->old_free;
        new_free = (char *)addr + total_size;
    }

    if (new_free <= gc_heap->old_end) {
        gc_heap->old_free = new_free;

        /* Set header */
        habu_header_t *header = (habu_header_t *)addr;
        header->type = type;
        header->size = bytes;
        header->gc_color = GC_WHITE;
        header->gen_age = GEN_OLD;  /* Allocated directly in old gen */
        header->reserved = 0;
        header->padding = 0;

        gc_heap->stats.total_allocated += total_size;
        gc_heap->old_bytes_allocated += total_size;
        return (char *)addr + sizeof(habu_header_t);
    }

    /* Heap exhausted */
    return NULL;
}

/* Slow-path allocation for inline code generation
 *
 * This function is called by generated machine code when the fast-path
 * allocation fails (young generation is full). It triggers GC and retries
 * allocation, falling back to old generation if needed.
 *
 * Parameters:
 *   bytes - Size of object data (NOT including header)
 *   type - Object type (TYPE_CONS, TYPE_VECTOR, etc.)
 *
 * Returns:
 *   Pointer to object data (after header), or NULL if allocation fails
 */
void *gc_alloc_slow(size_t bytes, uint64_t type) {
    return gc_alloc(bytes, type);
}

/* GC-allocated object constructors */

habu_value_t cons(habu_value_t car, habu_value_t cdr) {
    void *mem = gc_alloc(sizeof(habu_cons_t), TYPE_CONS);
    if (!mem) {
        return NIL;
    }

    habu_cons_t *cons = (habu_cons_t *)mem;
    cons->car = car;
    cons->cdr = cdr;

    return tag_pointer(cons, TAG_CONS);
}

habu_value_t make_vector(habu_value_t length_val) {
    if (!is_fixnum(length_val)) {
        return NIL;
    }
    size_t length = (size_t)value_to_fixnum(length_val);
    size_t size = sizeof(habu_vector_t) + length * sizeof(habu_value_t);
    void *mem = gc_alloc(size, TYPE_VECTOR);
    if (!mem) {
        return NIL;
    }

    habu_vector_t *vec = (habu_vector_t *)mem;
    vec->length = length;
    for (size_t i = 0; i < length; i++) {
        vec->data[i] = NIL;
    }

    return tag_pointer(vec, TAG_VECTOR);
}

habu_value_t make_string(const char *str, size_t length) {
    size_t size = sizeof(habu_string_t) + length + 1;
    void *mem = gc_alloc(size, TYPE_STRING);
    if (!mem) {
        return NIL;
    }

    habu_string_t *string = (habu_string_t *)mem;
    string->length = length;
    memcpy(string->data, str, length);
    string->data[length] = '\0';

    return tag_pointer(string, TAG_STRING);
}

habu_value_t make_symbol(const char *name) {
    /* Check if symbol already exists in the intern table */
    habu_value_t existing = symbol_table_lookup(name);
    if (existing != NIL) {
        return existing;  /* Return cached symbol */
    }

    /* Symbol not found - create new one */
    void *mem = gc_alloc(sizeof(habu_symbol_t), TYPE_SYMBOL);
    if (!mem) {
        return NIL;
    }

    /* CRITICAL: Root the symbol before nested allocation
     * habu_make_string can trigger GC, which would invalidate 'mem' */
    habu_symbol_t *sym = (habu_symbol_t *)mem;
    habu_value_t sym_value = tag_pointer(sym, TAG_SYMBOL);
    gc_add_root(&sym_value);

    /* Now safe to allocate string - if GC runs, sym_value will be updated */
    sym = (habu_symbol_t *)untag_pointer(sym_value);  /* Refresh pointer */
    sym->name = make_string(name, strlen(name));
    sym->value = NIL;
    sym->plist = NIL;

    /* Unroot before returning */
    gc_remove_root(&sym_value);

    /* Add to intern table for future lookups */
    symbol_table_insert(name, sym_value);

    return sym_value;
}

/* Hash tables */

#define DEFAULT_HASH_CAPACITY 16

/* Compute hash code for a Habu value */
static uint64_t hash_code(habu_value_t key) {
    uint64_t tag = get_tag(key);

    switch (tag) {
    case TAG_FIXNUM:
        /* Use fixnum value directly */
        return (uint64_t)value_to_fixnum(key);

    case TAG_STRING: {
        /* DJB2 hash for strings */
        habu_string_t *str = value_to_string(key);
        uint64_t hash = 5381;
        for (uint64_t i = 0; i < str->length; i++) {
            hash = ((hash << 5) + hash) + (uint8_t)str->data[i];
        }
        return hash;
    }

    case TAG_SYMBOL: {
        /* Hash the symbol's name */
        habu_symbol_t *sym = value_to_symbol(key);
        return hash_code(sym->name);
    }

    default:
        /* Use address as hash for other types */
        return (uint64_t)key >> 4;
    }
}

/* Check if two keys are equal (for hash table lookup) */
static bool hash_equal(habu_value_t a, habu_value_t b) {
    /* Fast path: identical values */
    if (a == b) return true;

    uint64_t tag_a = get_tag(a);
    uint64_t tag_b = get_tag(b);

    /* Different tags -> not equal */
    if (tag_a != tag_b) return false;

    /* String comparison */
    if (tag_a == TAG_STRING) {
        habu_string_t *str_a = value_to_string(a);
        habu_string_t *str_b = value_to_string(b);
        if (str_a->length != str_b->length) return false;
        return memcmp(str_a->data, str_b->data, str_a->length) == 0;
    }

    /* For other types, identity comparison (already done above) */
    return false;
}

habu_value_t make_hash_table(habu_value_t capacity_val) {
    /* Extract capacity from tagged fixnum, use default if 0 */
    size_t capacity = capacity_val == NIL ? DEFAULT_HASH_CAPACITY
                    : (size_t)value_to_fixnum(capacity_val);
    if (capacity == 0) capacity = DEFAULT_HASH_CAPACITY;

    /* Allocate the hash table structure */
    void *mem = gc_alloc(sizeof(habu_hashtable_t), TYPE_HASHTABLE);
    if (!mem) return NIL;

    habu_hashtable_t *ht = (habu_hashtable_t *)mem;
    habu_value_t ht_val = tag_pointer(ht, TAG_HASHTABLE);

    /* Root the hash table before allocating buckets vector */
    gc_add_root(&ht_val);

    /* Allocate buckets vector (may trigger GC) */
    habu_value_t buckets = make_vector(fixnum_to_value(capacity));

    /* Refresh pointer after potential GC */
    ht = value_to_hashtable(ht_val);
    ht->count = 0;
    ht->capacity = capacity;
    ht->buckets = buckets;

    gc_remove_root(&ht_val);

    return ht_val;
}

habu_value_t gethash(habu_value_t key, habu_value_t ht_val, habu_value_t default_val) {
    if (get_tag(ht_val) != TAG_HASHTABLE) return default_val;

    habu_hashtable_t *ht = value_to_hashtable(ht_val);
    uint64_t hash = hash_code(key);
    uint64_t bucket_idx = hash % ht->capacity;

    /* Get bucket (association list) */
    habu_vector_t *buckets = value_to_vector(ht->buckets);
    habu_value_t bucket = buckets->data[bucket_idx];

    /* Search the association list */
    while (get_tag(bucket) == TAG_CONS) {
        habu_cons_t *cell = value_to_cons(bucket);
        habu_value_t pair = cell->car;

        if (get_tag(pair) == TAG_CONS) {
            habu_cons_t *kv = value_to_cons(pair);
            if (hash_equal(key, kv->car)) {
                return kv->cdr;  /* Found it */
            }
        }

        bucket = cell->cdr;
    }

    return default_val;  /* Not found */
}

habu_value_t puthash(habu_value_t key, habu_value_t value, habu_value_t ht_val) {
    if (get_tag(ht_val) != TAG_HASHTABLE) return NIL;

    /* Root all values since we'll be allocating */
    gc_add_root(&key);
    gc_add_root(&value);
    gc_add_root(&ht_val);

    habu_hashtable_t *ht = value_to_hashtable(ht_val);
    uint64_t hash = hash_code(key);
    uint64_t bucket_idx = hash % ht->capacity;

    /* Get bucket */
    habu_vector_t *buckets = value_to_vector(ht->buckets);
    habu_value_t bucket = buckets->data[bucket_idx];

    /* Search for existing key */
    habu_value_t current = bucket;
    while (get_tag(current) == TAG_CONS) {
        habu_cons_t *cell = value_to_cons(current);
        habu_value_t pair = cell->car;

        if (get_tag(pair) == TAG_CONS) {
            habu_cons_t *kv = value_to_cons(pair);
            if (hash_equal(key, kv->car)) {
                /* Update existing entry */
                kv->cdr = value;
                gc_remove_root(&ht_val);
                gc_remove_root(&value);
                gc_remove_root(&key);
                return value;
            }
        }

        current = cell->cdr;
    }

    /* Key not found - create new entry */
    /* cons(key, value) */
    habu_value_t pair = cons(key, value);
    gc_add_root(&pair);

    /* cons(pair, old_bucket) */
    /* Refresh pointers after allocation */
    ht = value_to_hashtable(ht_val);
    buckets = value_to_vector(ht->buckets);
    bucket = buckets->data[bucket_idx];

    habu_value_t new_bucket = cons(pair, bucket);

    /* Refresh and update */
    ht = value_to_hashtable(ht_val);
    buckets = value_to_vector(ht->buckets);
    buckets->data[bucket_idx] = new_bucket;
    ht->count++;

    gc_remove_root(&pair);
    gc_remove_root(&ht_val);
    gc_remove_root(&value);
    gc_remove_root(&key);

    return value;
}

habu_value_t remhash(habu_value_t key, habu_value_t ht_val) {
    if (get_tag(ht_val) != TAG_HASHTABLE) return NIL;

    habu_hashtable_t *ht = value_to_hashtable(ht_val);
    uint64_t hash = hash_code(key);
    uint64_t bucket_idx = hash % ht->capacity;

    habu_vector_t *buckets = value_to_vector(ht->buckets);
    habu_value_t bucket = buckets->data[bucket_idx];
    habu_value_t prev = NIL;

    while (get_tag(bucket) == TAG_CONS) {
        habu_cons_t *cell = value_to_cons(bucket);
        habu_value_t pair = cell->car;

        if (get_tag(pair) == TAG_CONS) {
            habu_cons_t *kv = value_to_cons(pair);
            if (hash_equal(key, kv->car)) {
                /* Found it - remove from list */
                if (prev == NIL) {
                    /* First element */
                    buckets->data[bucket_idx] = cell->cdr;
                } else {
                    /* Middle/end element */
                    value_to_cons(prev)->cdr = cell->cdr;
                }
                ht->count--;
                return fixnum_to_value(1);  /* True - was removed */
            }
        }

        prev = bucket;
        bucket = cell->cdr;
    }

    return NIL;  /* Not found */
}

habu_value_t hash_table_count(habu_value_t ht_val) {
    if (get_tag(ht_val) != TAG_HASHTABLE) return NIL;
    habu_hashtable_t *ht = value_to_hashtable(ht_val);
    return fixnum_to_value(ht->count);
}

/* Float operations (IEEE 754 double precision) */

static inline habu_float_t *value_to_float(habu_value_t v) {
    return (habu_float_t *)untag_pointer(v);
}

habu_value_t make_float(double value) {
    size_t size = sizeof(habu_float_t);
    habu_float_t *f = gc_alloc(size, TYPE_FLOAT);
    if (!f) {
        return NIL;
    }
    f->value = value;
    return tag_pointer(f, TAG_FLOAT);
}

double float_value(habu_value_t float_val) {
    if (get_tag(float_val) != TAG_FLOAT) {
        return 0.0;
    }
    habu_float_t *f = value_to_float(float_val);
    return f->value;
}

/* Helper to get numeric value as double (works for both fixnum and float) */
static inline double to_double(habu_value_t v) {
    if (is_fixnum(v)) {
        return (double)value_to_fixnum(v);
    } else if (get_tag(v) == TAG_FLOAT) {
        return value_to_float(v)->value;
    }
    return 0.0;
}

habu_value_t float_add(habu_value_t a, habu_value_t b) {
    return make_float(to_double(a) + to_double(b));
}

habu_value_t float_sub(habu_value_t a, habu_value_t b) {
    return make_float(to_double(a) - to_double(b));
}

habu_value_t float_mul(habu_value_t a, habu_value_t b) {
    return make_float(to_double(a) * to_double(b));
}

habu_value_t float_div(habu_value_t a, habu_value_t b) {
    double bval = to_double(b);
    if (bval == 0.0) return NIL;  /* Division by zero */
    return make_float(to_double(a) / bval);
}

habu_value_t float_lt(habu_value_t a, habu_value_t b) {
    return to_double(a) < to_double(b) ? fixnum_to_value(1) : NIL;
}

habu_value_t float_gt(habu_value_t a, habu_value_t b) {
    return to_double(a) > to_double(b) ? fixnum_to_value(1) : NIL;
}

habu_value_t float_le(habu_value_t a, habu_value_t b) {
    return to_double(a) <= to_double(b) ? fixnum_to_value(1) : NIL;
}

habu_value_t float_ge(habu_value_t a, habu_value_t b) {
    return to_double(a) >= to_double(b) ? fixnum_to_value(1) : NIL;
}

habu_value_t float_eq(habu_value_t a, habu_value_t b) {
    return to_double(a) == to_double(b) ? fixnum_to_value(1) : NIL;
}

habu_value_t fixnum_to_float(habu_value_t fixnum) {
    if (!is_fixnum(fixnum)) return NIL;
    return make_float((double)value_to_fixnum(fixnum));
}

habu_value_t float_to_fixnum(habu_value_t float_val) {
    if (get_tag(float_val) != TAG_FLOAT) return NIL;
    double d = value_to_float(float_val)->value;
    return fixnum_to_value((int64_t)d);  /* Truncate toward zero */
}

/* ============================================================================
 * BIGNUM OPERATIONS
 * ============================================================================ */

bool is_bignum(habu_value_t v) {
    return get_tag(v) == TAG_BIGNUM;
}

habu_value_t make_bignum_from_fixnum(habu_value_t fixnum) {
    if (!is_fixnum(fixnum)) return NIL;

    int64_t val = value_to_fixnum(fixnum);
    int64_t sign = (val >= 0) ? 1 : -1;
    uint64_t abs_val = (val >= 0) ? (uint64_t)val : (uint64_t)(-val);

    /* Allocate bignum with 1 limb */
    size_t size = sizeof(habu_bignum_t) + sizeof(uint64_t);
    habu_bignum_t *bn = gc_alloc(size, TYPE_BIGNUM);
    if (!bn) return NIL;

    bn->sign = sign;
    bn->length = 1;
    bn->limbs[0] = abs_val;

    return tag_pointer(bn, TAG_BIGNUM);
}

habu_value_t bignum_to_fixnum(habu_value_t bignum) {
    if (get_tag(bignum) != TAG_BIGNUM) return NIL;

    habu_bignum_t *bn = value_to_bignum(bignum);

    /* Only convert single-limb bignums that fit in fixnum range */
    if (bn->length != 1) return NIL;

    uint64_t val = bn->limbs[0];
    /* Check if fits in 60-bit signed range */
    if (val > ((uint64_t)1 << 59) - 1) return NIL;

    int64_t result = (int64_t)val * bn->sign;
    return fixnum_to_value(result);
}

/* Helper: compare absolute values */
static int bignum_cmp_abs(habu_bignum_t *a, habu_bignum_t *b) {
    if (a->length != b->length) {
        return (a->length > b->length) ? 1 : -1;
    }
    for (int64_t i = a->length - 1; i >= 0; i--) {
        if (a->limbs[i] != b->limbs[i]) {
            return (a->limbs[i] > b->limbs[i]) ? 1 : -1;
        }
    }
    return 0;
}

habu_value_t bignum_add(habu_value_t a, habu_value_t b) {
    /* Convert fixnums to bignums if needed */
    if (is_fixnum(a)) a = make_bignum_from_fixnum(a);
    if (is_fixnum(b)) b = make_bignum_from_fixnum(b);
    if (get_tag(a) != TAG_BIGNUM || get_tag(b) != TAG_BIGNUM) return NIL;

    habu_bignum_t *ba = value_to_bignum(a);
    habu_bignum_t *bb = value_to_bignum(b);

    /* Simple case: single limb addition */
    if (ba->length == 1 && bb->length == 1 && ba->sign == bb->sign) {
        uint64_t sum = ba->limbs[0] + bb->limbs[0];
        if (sum >= ba->limbs[0]) {  /* No overflow */
            size_t size = sizeof(habu_bignum_t) + sizeof(uint64_t);
            habu_bignum_t *result = gc_alloc(size, TYPE_BIGNUM);
            if (!result) return NIL;
            result->sign = ba->sign;
            result->length = 1;
            result->limbs[0] = sum;
            return tag_pointer(result, TAG_BIGNUM);
        }
        /* Overflow - need 2 limbs */
        size_t size = sizeof(habu_bignum_t) + 2 * sizeof(uint64_t);
        habu_bignum_t *result = gc_alloc(size, TYPE_BIGNUM);
        if (!result) return NIL;
        result->sign = ba->sign;
        result->length = 2;
        result->limbs[0] = sum;
        result->limbs[1] = 1;
        return tag_pointer(result, TAG_BIGNUM);
    }

    /* For now, return NIL for complex cases */
    return NIL;
}

habu_value_t bignum_sub(habu_value_t a, habu_value_t b) {
    /* Stub: negate b and add */
    if (is_fixnum(b)) b = make_bignum_from_fixnum(b);
    if (get_tag(b) != TAG_BIGNUM) return NIL;

    habu_bignum_t *bb = value_to_bignum(b);
    /* Create negated copy */
    size_t size = sizeof(habu_bignum_t) + bb->length * sizeof(uint64_t);
    habu_bignum_t *neg = gc_alloc(size, TYPE_BIGNUM);
    if (!neg) return NIL;
    neg->sign = -bb->sign;
    neg->length = bb->length;
    for (uint64_t i = 0; i < bb->length; i++) {
        neg->limbs[i] = bb->limbs[i];
    }

    return bignum_add(a, tag_pointer(neg, TAG_BIGNUM));
}

habu_value_t bignum_mul(habu_value_t a, habu_value_t b) {
    if (is_fixnum(a)) a = make_bignum_from_fixnum(a);
    if (is_fixnum(b)) b = make_bignum_from_fixnum(b);
    if (get_tag(a) != TAG_BIGNUM || get_tag(b) != TAG_BIGNUM) return NIL;

    habu_bignum_t *ba = value_to_bignum(a);
    habu_bignum_t *bb = value_to_bignum(b);

    /* Simple case: single limb multiplication */
    if (ba->length == 1 && bb->length == 1) {
        __uint128_t prod = (__uint128_t)ba->limbs[0] * bb->limbs[0];
        uint64_t lo = (uint64_t)prod;
        uint64_t hi = (uint64_t)(prod >> 64);

        if (hi == 0) {
            size_t size = sizeof(habu_bignum_t) + sizeof(uint64_t);
            habu_bignum_t *result = gc_alloc(size, TYPE_BIGNUM);
            if (!result) return NIL;
            result->sign = ba->sign * bb->sign;
            result->length = 1;
            result->limbs[0] = lo;
            return tag_pointer(result, TAG_BIGNUM);
        } else {
            size_t size = sizeof(habu_bignum_t) + 2 * sizeof(uint64_t);
            habu_bignum_t *result = gc_alloc(size, TYPE_BIGNUM);
            if (!result) return NIL;
            result->sign = ba->sign * bb->sign;
            result->length = 2;
            result->limbs[0] = lo;
            result->limbs[1] = hi;
            return tag_pointer(result, TAG_BIGNUM);
        }
    }

    return NIL;  /* Complex case not implemented */
}

habu_value_t bignum_div(habu_value_t a, habu_value_t b) {
    if (is_fixnum(a)) a = make_bignum_from_fixnum(a);
    if (is_fixnum(b)) b = make_bignum_from_fixnum(b);
    if (get_tag(a) != TAG_BIGNUM || get_tag(b) != TAG_BIGNUM) return NIL;

    habu_bignum_t *ba = value_to_bignum(a);
    habu_bignum_t *bb = value_to_bignum(b);

    /* Division by zero check */
    if (bb->length == 1 && bb->limbs[0] == 0) return NIL;

    /* Simple case: single limb division */
    if (ba->length == 1 && bb->length == 1) {
        uint64_t quot = ba->limbs[0] / bb->limbs[0];
        size_t size = sizeof(habu_bignum_t) + sizeof(uint64_t);
        habu_bignum_t *result = gc_alloc(size, TYPE_BIGNUM);
        if (!result) return NIL;
        result->sign = ba->sign * bb->sign;
        result->length = 1;
        result->limbs[0] = quot;
        return tag_pointer(result, TAG_BIGNUM);
    }

    return NIL;  /* Complex case not implemented */
}

/* ============================================================================
 * MULTI-DIMENSIONAL ARRAY OPERATIONS
 * ============================================================================ */

habu_value_t make_array(habu_value_t dims, habu_value_t initial) {
    if (get_tag(dims) != TAG_VECTOR && get_tag(dims) != TAG_CONS) return NIL;

    /* Calculate total size and copy dimensions */
    uint64_t rank = 0;
    uint64_t total_size = 1;

    if (get_tag(dims) == TAG_VECTOR) {
        habu_vector_t *dim_vec = value_to_vector(dims);
        rank = dim_vec->length;
        for (uint64_t i = 0; i < rank; i++) {
            if (!is_fixnum(dim_vec->data[i])) return NIL;
            total_size *= value_to_fixnum(dim_vec->data[i]);
        }
    } else {
        /* Count list length and compute size */
        habu_value_t curr = dims;
        while (!is_nil(curr) && get_tag(curr) == TAG_CONS) {
            habu_cons_t *c = value_to_cons(curr);
            if (!is_fixnum(c->car)) return NIL;
            total_size *= value_to_fixnum(c->car);
            rank++;
            curr = c->cdr;
        }
    }

    if (rank == 0 || total_size == 0) return NIL;

    /* Create dimension vector */
    habu_value_t dim_vector = make_vector(fixnum_to_value(rank));
    if (is_nil(dim_vector)) return NIL;
    habu_vector_t *dv = value_to_vector(dim_vector);

    if (get_tag(dims) == TAG_VECTOR) {
        habu_vector_t *src = value_to_vector(dims);
        for (uint64_t i = 0; i < rank; i++) {
            dv->data[i] = src->data[i];
        }
    } else {
        habu_value_t curr = dims;
        for (uint64_t i = 0; i < rank; i++) {
            habu_cons_t *c = value_to_cons(curr);
            dv->data[i] = c->car;
            curr = c->cdr;
        }
    }

    /* Create data vector */
    habu_value_t data_vector = make_vector(fixnum_to_value(total_size));
    if (is_nil(data_vector)) return NIL;
    habu_vector_t *data = value_to_vector(data_vector);

    /* Initialize with initial value */
    for (uint64_t i = 0; i < total_size; i++) {
        data->data[i] = initial;
    }

    /* Allocate array structure */
    size_t size = sizeof(habu_array_t);
    habu_array_t *arr = gc_alloc(size, TYPE_ARRAY);
    if (!arr) return NIL;

    arr->rank = rank;
    arr->total_size = total_size;
    arr->dims = dim_vector;
    arr->data = data_vector;

    return tag_pointer(arr, TAG_ARRAY);
}

/* Helper: compute linear index from multi-dimensional indices */
static uint64_t compute_linear_index(habu_array_t *arr, habu_value_t indices) {
    habu_vector_t *dims = value_to_vector(arr->dims);
    uint64_t linear = 0;
    uint64_t multiplier = 1;

    /* indices can be a vector or list */
    if (get_tag(indices) == TAG_VECTOR) {
        habu_vector_t *idx_vec = value_to_vector(indices);
        for (int64_t i = arr->rank - 1; i >= 0; i--) {
            uint64_t idx = value_to_fixnum(idx_vec->data[i]);
            linear += idx * multiplier;
            multiplier *= value_to_fixnum(dims->data[i]);
        }
    } else {
        /* Convert list to array of indices first */
        uint64_t idx_arr[16];  /* Max 16 dimensions */
        uint64_t n = 0;
        habu_value_t curr = indices;
        while (!is_nil(curr) && get_tag(curr) == TAG_CONS && n < 16) {
            habu_cons_t *c = value_to_cons(curr);
            idx_arr[n++] = value_to_fixnum(c->car);
            curr = c->cdr;
        }
        for (int64_t i = n - 1; i >= 0; i--) {
            linear += idx_arr[i] * multiplier;
            multiplier *= value_to_fixnum(dims->data[i]);
        }
    }

    return linear;
}

habu_value_t aref(habu_value_t array, habu_value_t indices) {
    if (get_tag(array) != TAG_ARRAY) return NIL;

    habu_array_t *arr = value_to_array(array);
    uint64_t linear = compute_linear_index(arr, indices);

    if (linear >= arr->total_size) return NIL;

    habu_vector_t *data = value_to_vector(arr->data);
    return data->data[linear];
}

habu_value_t aset(habu_value_t array, habu_value_t indices, habu_value_t value) {
    if (get_tag(array) != TAG_ARRAY) return NIL;

    habu_array_t *arr = value_to_array(array);
    uint64_t linear = compute_linear_index(arr, indices);

    if (linear >= arr->total_size) return NIL;

    habu_vector_t *data = value_to_vector(arr->data);
    data->data[linear] = value;
    return value;
}

habu_value_t array_dimensions(habu_value_t array) {
    if (get_tag(array) != TAG_ARRAY) return NIL;
    return value_to_array(array)->dims;
}

habu_value_t array_rank(habu_value_t array) {
    if (get_tag(array) != TAG_ARRAY) return NIL;
    return fixnum_to_value(value_to_array(array)->rank);
}

habu_value_t array_total_size(habu_value_t array) {
    if (get_tag(array) != TAG_ARRAY) return NIL;
    return fixnum_to_value(value_to_array(array)->total_size);
}

/* Statistics */

size_t gc_heap_size(void) {
    if (!gc_heap) return 0;
    return YOUNG_GEN_SIZE + OLD_GEN_SIZE;
}

size_t gc_heap_used(void) {
    if (!gc_heap) return 0;
    size_t young_used = (char *)gc_heap->young_free - (char *)gc_heap->young_start;
    size_t old_used = (char *)gc_heap->old_free - (char *)gc_heap->old_start;
    return young_used + old_used;
}

void gc_get_stats(habu_gc_stats_t *stats) {
    if (!gc_heap || !stats) return;
    memcpy(stats, &gc_heap->stats, sizeof(habu_gc_stats_t));
}

void gc_reset_stats(void) {
    if (!gc_heap) return;
    memset(&gc_heap->stats, 0, sizeof(habu_gc_stats_t));
}
