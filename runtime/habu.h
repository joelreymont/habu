#ifndef HABU_H
#define HABU_H

#include "object.h"
#include <stddef.h>
#include <stdbool.h>

/* Runtime initialization and shutdown */
void habu_init(size_t heap_size);
void habu_shutdown(void);

/* Memory management modes */
extern bool habu_gc_enabled;

static inline void habu_enable_gc(void) {
    habu_gc_enabled = true;
}

static inline void habu_disable_gc(void) {
    habu_gc_enabled = false;
}

/* Region allocator */
typedef struct habu_region habu_region_t;

habu_region_t *habu_region_create(size_t size);
void habu_region_destroy(habu_region_t *region);
void *habu_region_alloc(habu_region_t *region, size_t bytes);
void habu_region_reset(habu_region_t *region);
size_t habu_region_used(habu_region_t *region);

/* GC heap allocator */
void *habu_gc_alloc(size_t bytes, uint64_t type);

/* Explicit GC control */
void habu_gc_collect(void);
size_t habu_gc_heap_size(void);
size_t habu_gc_heap_used(void);

/* GC statistics */
typedef struct {
    uint64_t young_collections;
    uint64_t old_collections;
    uint64_t total_allocated;
    uint64_t total_freed;
    uint64_t max_pause_ns;
    uint64_t last_pause_ns;
} habu_gc_stats_t;

void habu_gc_get_stats(habu_gc_stats_t *stats);
void habu_gc_reset_stats(void);

/* Object allocation functions */
habu_value_t habu_cons(habu_value_t car, habu_value_t cdr);
habu_value_t habu_make_vector(size_t length);
habu_value_t habu_make_string(const char *str, size_t length);
habu_value_t habu_make_symbol(const char *name);

/* Region-based allocation */
habu_value_t habu_region_cons(habu_region_t *region, habu_value_t car, habu_value_t cdr);
habu_value_t habu_region_make_vector(habu_region_t *region, size_t length);
habu_value_t habu_region_make_string(habu_region_t *region, const char *str, size_t length);

/* Accessors */
habu_value_t habu_car(habu_value_t cons);
habu_value_t habu_cdr(habu_value_t cons);
void habu_set_car(habu_value_t cons, habu_value_t value);
void habu_set_cdr(habu_value_t cons, habu_value_t value);
habu_value_t habu_vector_ref(habu_value_t vector, size_t index);
void habu_vector_set(habu_value_t vector, size_t index, habu_value_t value);

/* I/O */
void habu_write_byte(uint8_t byte);
uint8_t habu_read_byte(void);
void habu_print(const char *str);

/* Time measurement (for benchmarks) */
uint64_t habu_time_ns(void);

#endif /* HABU_H */
