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

/* Fast-path allocation globals for inline code generation
 *
 * These globals are exported so that generated machine code can
 * perform inline bump-pointer allocation without function calls.
 *
 * FAST PATH (inline in generated code):
 *   1. Load young_free_ptr: ptr = *habu_young_free_ptr
 *   2. Check space: if ptr + size > *habu_young_end_ptr goto slow_path
 *   3. Allocate: write header, write data at ptr
 *   4. Update: *habu_young_free_ptr = ptr + size
 *   5. Return: tagged_ptr = (ptr + 16) | tag
 *
 * SLOW PATH (call when out of space):
 *   Call habu_gc_alloc_slow(size, type) which triggers GC and allocates
 */
extern void **habu_young_free_ptr;  /* Pointer to young_free (for bump allocation) */
extern void **habu_young_end_ptr;   /* Pointer to young_end (for bounds check) */
void *habu_gc_alloc_slow(size_t bytes, uint64_t type);  /* Slow path with GC */

/* Explicit GC control */
void habu_gc_collect(void);
size_t habu_gc_heap_size(void);
size_t habu_gc_heap_used(void);

/* Root registration API
 * Pass POINTER to habu_value_t (e.g., &my_variable)
 * This allows GC to update your variable when objects are relocated
 */
void habu_gc_add_root(habu_value_t *root_location);
void habu_gc_remove_root(habu_value_t *root_location);

/* Scoped root helpers - RAII style root management
 *
 * Example usage:
 *   HABU_ROOT(obj, habu_cons(a, b));
 *   // obj is now rooted for this scope
 *   // ... use obj, can trigger GC safely ...
 *   HABU_UNROOT(obj);
 */
#define HABU_ROOT(var, value) \
    habu_value_t var = (value); \
    habu_gc_add_root(&var)

#define HABU_UNROOT(var) \
    habu_gc_remove_root(&var)

/* Multi-value rooting - root up to 4 values at once */
#define HABU_ROOT2(v1, val1, v2, val2) \
    habu_value_t v1 = (val1); \
    habu_value_t v2 = (val2); \
    habu_gc_add_root(&v1); \
    habu_gc_add_root(&v2)

#define HABU_UNROOT2(v1, v2) \
    habu_gc_remove_root(&v2); \
    habu_gc_remove_root(&v1)

/* Write barrier - call when storing pointer into object */
void habu_write_barrier(void *obj, habu_value_t value);

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
habu_value_t habu_runtime_make_string(const char *str);
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

/* String operations */
habu_value_t habu_string_ref(habu_value_t str_val, size_t index);
size_t habu_string_length_raw(habu_value_t str_val);
const char* habu_string_to_cstr(habu_value_t str_val);
habu_value_t habu_make_string_from_vector(habu_value_t vec_val);  /* For reader - convert char codes to string */
habu_value_t habu_string_concat(habu_value_t str1_val, habu_value_t str2_val);  /* Concatenate two strings */
habu_value_t habu_string_substring(habu_value_t str_val, habu_value_t start_val, habu_value_t end_val);  /* Extract substring */
habu_value_t habu_fixnum_to_string(habu_value_t num_val);  /* Convert fixnum to string */

/* Symbol operations */
habu_value_t habu_make_symbol_from_string(habu_value_t str_val);
habu_value_t habu_runtime_find_symbol(const char *name);
habu_value_t habu_symbol_name(habu_value_t sym_val);

/* Closure support */
habu_value_t habu_make_closure(void *code_ptr, habu_value_t env);
void *habu_closure_code(habu_value_t closure_val);
habu_value_t habu_closure_env(habu_value_t closure_val);

/* Tag access - fundamental primitive for type checking in Lisp */
habu_value_t habu_get_tag(habu_value_t val);

/* Type tag constants (for use in Lisp predicates) */
/* These match the TAG_* values in object.h:
 *   TAG_FIXNUM  = 0x0
 *   TAG_CONS    = 0x1
 *   TAG_SYMBOL  = 0x2
 *   TAG_VECTOR  = 0x3
 *   TAG_STRING  = 0x4
 *   TAG_CLOSURE = 0x5
 */

/* I/O System */
void habu_io_init(void);
void habu_io_shutdown(void);

/* File I/O */
habu_value_t habu_open_file(habu_value_t path_str, habu_value_t mode_str);
habu_value_t habu_close_file(habu_value_t handle);
habu_value_t habu_read_line(habu_value_t handle);
habu_value_t habu_write_string(habu_value_t handle, habu_value_t str);
habu_value_t habu_read_file(habu_value_t path_str);
habu_value_t habu_write_file(habu_value_t path_str, habu_value_t content_str);

/* Printing */
habu_value_t habu_print_value(habu_value_t value);
habu_value_t habu_println_value(habu_value_t value);

/* Input primitives - read line from stdin */
char* habu_fgets_line(void);  /* Simple line input */
char* lineedit_readline(const char *prompt);  /* Line editing with readline features */

/* Legacy I/O (for compatibility) */
void habu_write_byte(uint8_t byte);
uint8_t habu_read_byte(void);
void habu_print(const char *str);

/* Time measurement (for benchmarks) */
uint64_t habu_time_ns(void);

#endif /* HABU_H */
