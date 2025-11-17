/* Incremental generational garbage collector */

#define _POSIX_C_SOURCE 199309L
#include "habu.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* GC heap structure */

#define YOUNG_GEN_SIZE (512 * 1024)    /* 512 KB */
#define OLD_GEN_SIZE (4 * 1024 * 1024) /* 4 MB */

typedef enum {
    GC_STATE_IDLE,
    GC_STATE_MARKING,
    GC_STATE_SWEEPING
} gc_state_t;

typedef struct gc_heap {
    /* Young generation (copying collector) */
    void *young_start;
    void *young_end;
    void *young_free;
    void *young_from;  /* Fromspace for copying */
    void *young_to;    /* Tospace for copying */

    /* Old generation (mark-sweep) */
    void *old_start;
    void *old_end;
    void *old_free;

    /* GC state */
    gc_state_t state;
    void **gray_stack;
    size_t gray_stack_size;
    size_t gray_stack_capacity;

    /* Roots */
    void **roots;
    size_t roots_size;
    size_t roots_capacity;

    /* Statistics */
    habu_gc_stats_t stats;

    /* Allocation tracking */
    size_t bytes_allocated_since_gc;
    size_t gc_threshold;
} gc_heap_t;

static gc_heap_t *gc_heap = NULL;

/* Initialize GC heap */

void habu_init(size_t heap_size) {
    (void)heap_size;  /* Use fixed sizes for now */

    gc_heap = malloc(sizeof(gc_heap_t));
    assert(gc_heap != NULL);

    memset(gc_heap, 0, sizeof(gc_heap_t));

    /* Allocate young generation (two semispaces) */
    size_t young_total = YOUNG_GEN_SIZE * 2;
    void *young_mem = aligned_alloc(16, young_total);
    assert(young_mem != NULL);

    gc_heap->young_from = young_mem;
    gc_heap->young_to = (char *)young_mem + YOUNG_GEN_SIZE;
    gc_heap->young_start = gc_heap->young_from;
    gc_heap->young_end = (char *)gc_heap->young_from + YOUNG_GEN_SIZE;
    gc_heap->young_free = gc_heap->young_start;

    /* Allocate old generation */
    void *old_mem = aligned_alloc(16, OLD_GEN_SIZE);
    assert(old_mem != NULL);

    gc_heap->old_start = old_mem;
    gc_heap->old_end = (char *)old_mem + OLD_GEN_SIZE;
    gc_heap->old_free = old_mem;

    /* Initialize gray stack */
    gc_heap->gray_stack_capacity = 1024;
    gc_heap->gray_stack = malloc(gc_heap->gray_stack_capacity * sizeof(void *));
    assert(gc_heap->gray_stack != NULL);
    gc_heap->gray_stack_size = 0;

    /* Initialize roots */
    gc_heap->roots_capacity = 256;
    gc_heap->roots = malloc(gc_heap->roots_capacity * sizeof(void *));
    assert(gc_heap->roots != NULL);
    gc_heap->roots_size = 0;

    gc_heap->state = GC_STATE_IDLE;
    gc_heap->gc_threshold = YOUNG_GEN_SIZE / 2;
}

void habu_shutdown(void) {
    if (gc_heap) {
        free(gc_heap->young_from);
        free(gc_heap->old_start);
        free(gc_heap->gray_stack);
        free(gc_heap->roots);
        free(gc_heap);
        gc_heap = NULL;
    }
}

/* Gray stack operations */

static void push_gray(void *obj) {
    if (gc_heap->gray_stack_size >= gc_heap->gray_stack_capacity) {
        gc_heap->gray_stack_capacity *= 2;
        gc_heap->gray_stack = realloc(gc_heap->gray_stack,
            gc_heap->gray_stack_capacity * sizeof(void *));
        assert(gc_heap->gray_stack != NULL);
    }
    gc_heap->gray_stack[gc_heap->gray_stack_size++] = obj;
}

static void *pop_gray(void) {
    if (gc_heap->gray_stack_size == 0) {
        return NULL;
    }
    return gc_heap->gray_stack[--gc_heap->gray_stack_size];
}

/* Object traversal */

static void mark_children(void *obj) {
    habu_header_t *header = get_header(obj);
    uint64_t type = header->type;

    switch (type) {
        case TYPE_CONS: {
            habu_cons_t *cons = (habu_cons_t *)obj;
            if (is_pointer(cons->car)) {
                void *car_obj = untag_pointer(cons->car);
                if (get_gc_color(car_obj) == GC_WHITE) {
                    set_gc_color(car_obj, GC_GRAY);
                    push_gray(car_obj);
                }
            }
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

/* Full collection (for testing) */

void habu_gc_collect(void) {
    if (!gc_heap || !habu_gc_enabled) {
        return;
    }

    uint64_t start = habu_time_ns();

    /* Mark phase */
    gc_heap->state = GC_STATE_MARKING;
    mark_roots();

    while (gc_heap->gray_stack_size > 0) {
        gc_mark_increment(32);
    }

    /* Sweep phase - TODO */
    gc_heap->state = GC_STATE_IDLE;

    uint64_t end = habu_time_ns();
    uint64_t pause = end - start;

    gc_heap->stats.last_pause_ns = pause;
    if (pause > gc_heap->stats.max_pause_ns) {
        gc_heap->stats.max_pause_ns = pause;
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

    /* Young gen full - TODO: copy collection */
    return NULL;
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
