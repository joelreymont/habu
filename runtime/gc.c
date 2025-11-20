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
 *    - Explicit API: habu_gc_add_root() / habu_gc_remove_root()
 *    - During GC, all objects reachable from roots are kept alive
 */

#define _POSIX_C_SOURCE 199309L
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
     * registered explicitly using habu_gc_add_root().
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
void **habu_young_free_ptr = NULL;
void **habu_young_end_ptr = NULL;

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
void habu_init(size_t heap_size) {
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
    habu_young_free_ptr = &gc_heap->young_free;
    habu_young_end_ptr = &gc_heap->young_end;

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
     * Roots must be registered explicitly using habu_gc_add_root().
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
    habu_io_init();
}

/* Shutdown and free all GC memory
 *
 * Call this before program exit to clean up all GC-related allocations.
 * After this, habu_gc_alloc() will fail until habu_init() is called again.
 */
void habu_shutdown(void) {
    /* Shutdown I/O system */
    habu_io_shutdown();

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

        case TYPE_STRING:
            /* Strings have no outgoing pointers */
            break;

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
void habu_write_barrier(void *obj, habu_value_t value) {
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

    uint64_t start = habu_time_ns();

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

    uint64_t end = habu_time_ns();
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

        case TYPE_STRING:
            /* Strings have no outgoing pointers */
            break;

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
void habu_gc_collect(void) {
    /* Safety checks */
    if (!gc_heap || !habu_gc_enabled) {
        return;
    }

    /* Start timing */
    uint64_t start = habu_time_ns();
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

    uint64_t end = habu_time_ns();
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
 *   habu_value_t my_obj = habu_cons(a, b);
 *   habu_gc_add_root(&my_obj);  // Pass ADDRESS of variable
 *   // ... allocations that might trigger GC ...
 *   habu_gc_remove_root(&my_obj);
 */

void habu_gc_add_root(habu_value_t *root_location) {
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

void habu_gc_remove_root(habu_value_t *root_location) {
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

void *habu_gc_alloc(size_t bytes, uint64_t type) {
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
        if (habu_gc_enabled &&
            gc_heap->bytes_allocated_since_gc >= gc_heap->gc_threshold) {
            habu_gc_collect();
            gc_heap->bytes_allocated_since_gc = 0;
        }

        return (char *)addr + sizeof(habu_header_t);
    }

    /* Young gen full - try GC first */
    if (habu_gc_enabled) {
        habu_gc_collect();
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
void *habu_gc_alloc_slow(size_t bytes, uint64_t type) {
    return habu_gc_alloc(bytes, type);
}

/* GC-allocated object constructors */

habu_value_t habu_cons(habu_value_t car, habu_value_t cdr) {
    void *mem = habu_gc_alloc(sizeof(habu_cons_t), TYPE_CONS);
    if (!mem) {
        return NIL;
    }

    habu_cons_t *cons = (habu_cons_t *)mem;
    cons->car = car;
    cons->cdr = cdr;

    return tag_pointer(cons, TAG_CONS);
}

habu_value_t habu_make_vector(size_t length) {
    size_t size = sizeof(habu_vector_t) + length * sizeof(habu_value_t);
    void *mem = habu_gc_alloc(size, TYPE_VECTOR);
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

habu_value_t habu_make_string(const char *str, size_t length) {
    size_t size = sizeof(habu_string_t) + length + 1;
    void *mem = habu_gc_alloc(size, TYPE_STRING);
    if (!mem) {
        return NIL;
    }

    habu_string_t *string = (habu_string_t *)mem;
    string->length = length;
    memcpy(string->data, str, length);
    string->data[length] = '\0';

    return tag_pointer(string, TAG_STRING);
}

habu_value_t habu_make_symbol(const char *name) {
    void *mem = habu_gc_alloc(sizeof(habu_symbol_t), TYPE_SYMBOL);
    if (!mem) {
        return NIL;
    }

    habu_symbol_t *sym = (habu_symbol_t *)mem;
    sym->name = habu_make_string(name, strlen(name));
    sym->value = NIL;
    sym->plist = NIL;

    return tag_pointer(sym, TAG_SYMBOL);
}

/* Statistics */

size_t habu_gc_heap_size(void) {
    if (!gc_heap) return 0;
    return YOUNG_GEN_SIZE + OLD_GEN_SIZE;
}

size_t habu_gc_heap_used(void) {
    if (!gc_heap) return 0;
    size_t young_used = (char *)gc_heap->young_free - (char *)gc_heap->young_start;
    size_t old_used = (char *)gc_heap->old_free - (char *)gc_heap->old_start;
    return young_used + old_used;
}

void habu_gc_get_stats(habu_gc_stats_t *stats) {
    if (!gc_heap || !stats) return;
    memcpy(stats, &gc_heap->stats, sizeof(habu_gc_stats_t));
}

void habu_gc_reset_stats(void) {
    if (!gc_heap) return;
    memset(&gc_heap->stats, 0, sizeof(habu_gc_stats_t));
}
