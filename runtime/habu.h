#ifndef HABU_H
#define HABU_H

#include "object.h"
#include <stddef.h>
#include <stdbool.h>

/* Runtime initialization and shutdown */
void init(size_t heap_size);
void shutdown(void);

/* Memory management modes */
extern bool gc_enabled;

static inline void enable_gc(void) {
    gc_enabled = true;
}

static inline void disable_gc(void) {
    gc_enabled = false;
}

/* Region allocator */
typedef struct habu_region habu_region_t;

habu_region_t *region_create(size_t size);
void region_destroy(habu_region_t *region);
void *region_alloc(habu_region_t *region, size_t bytes);
void region_reset(habu_region_t *region);
size_t region_used(habu_region_t *region);

/* GC heap allocator */
void *gc_alloc(size_t bytes, uint64_t type);

/* Fast-path allocation globals for inline code generation
 *
 * These globals are exported so that generated machine code can
 * perform inline bump-pointer allocation without function calls.
 *
 * FAST PATH (inline in generated code):
 *   1. Load young_free_ptr: ptr = *young_free_ptr
 *   2. Check space: if ptr + size > *young_end_ptr goto slow_path
 *   3. Allocate: write header, write data at ptr
 *   4. Update: *young_free_ptr = ptr + size
 *   5. Return: tagged_ptr = (ptr + 16) | tag
 *
 * SLOW PATH (call when out of space):
 *   Call gc_alloc_slow(size, type) which triggers GC and allocates
 */
extern void **young_free_ptr;  /* Pointer to young_free (for bump allocation) */
extern void **young_end_ptr;   /* Pointer to young_end (for bounds check) */
void *gc_alloc_slow(size_t bytes, uint64_t type);  /* Slow path with GC */

/* Explicit GC control */
void gc_collect(void);
size_t gc_heap_size(void);
size_t gc_heap_used(void);

/* Root registration API
 * Pass POINTER to habu_value_t (e.g., &my_variable)
 * This allows GC to update your variable when objects are relocated
 */
void gc_add_root(habu_value_t *root_location);
void gc_remove_root(habu_value_t *root_location);

/* Scoped root helpers - RAII style root management
 *
 * Example usage:
 *   HABU_ROOT(obj, cons(a, b));
 *   // obj is now rooted for this scope
 *   // ... use obj, can trigger GC safely ...
 *   HABU_UNROOT(obj);
 */
#define HABU_ROOT(var, value) \
    habu_value_t var = (value); \
    gc_add_root(&var)

#define HABU_UNROOT(var) \
    gc_remove_root(&var)

/* Multi-value rooting - root up to 4 values at once */
#define HABU_ROOT2(v1, val1, v2, val2) \
    habu_value_t v1 = (val1); \
    habu_value_t v2 = (val2); \
    gc_add_root(&v1); \
    gc_add_root(&v2)

#define HABU_UNROOT2(v1, v2) \
    gc_remove_root(&v2); \
    gc_remove_root(&v1)

/* Write barrier - call when storing pointer into object */
void write_barrier(void *obj, habu_value_t value);

/* GC statistics */
typedef struct {
    uint64_t young_collections;
    uint64_t old_collections;
    uint64_t total_allocated;
    uint64_t total_freed;
    uint64_t max_pause_ns;
    uint64_t last_pause_ns;
} habu_gc_stats_t;

void gc_get_stats(habu_gc_stats_t *stats);
void gc_reset_stats(void);

/* Object allocation functions */
habu_value_t cons(habu_value_t car, habu_value_t cdr);
habu_value_t make_vector(size_t length);
habu_value_t make_string(const char *str, size_t length);
habu_value_t make_symbol(const char *name);

/* Region-based allocation */
habu_value_t region_cons(habu_region_t *region, habu_value_t car, habu_value_t cdr);
habu_value_t region_make_vector(habu_region_t *region, size_t length);
habu_value_t region_make_string(habu_region_t *region, const char *str, size_t length);

/* Accessors */
habu_value_t car(habu_value_t cons);
habu_value_t cdr(habu_value_t cons);
void set_car(habu_value_t cons, habu_value_t value);
void set_cdr(habu_value_t cons, habu_value_t value);
habu_value_t vector_ref(habu_value_t vector, size_t index);
void vector_set(habu_value_t vector, size_t index, habu_value_t value);

/* String operations */
habu_value_t string_ref(habu_value_t str_val, size_t index);
size_t string_length_raw(habu_value_t str_val);
const char* string_to_cstr(habu_value_t str_val);
habu_value_t make_string_from_vector(habu_value_t vec_val);  /* For reader - convert char codes to string */
habu_value_t string_concat(habu_value_t str1_val, habu_value_t str2_val);  /* Concatenate two strings */
habu_value_t string_substring(habu_value_t str_val, habu_value_t start_val, habu_value_t end_val);  /* Extract substring */
habu_value_t fixnum_to_string(habu_value_t num_val);  /* Convert fixnum to string */
habu_value_t string_equal(habu_value_t str1_val, habu_value_t str2_val);  /* Compare strings for equality */

/* Symbol operations */
habu_value_t make_symbol_from_string(habu_value_t str_val);
habu_value_t symbol_name(habu_value_t sym_val);
habu_value_t gensym(habu_value_t prefix_str);  /* Generate unique uninterned symbol */

/* Closure support */
habu_value_t make_closure(void *code_ptr, habu_value_t env);
void *closure_code(habu_value_t closure_val);
habu_value_t closure_env(habu_value_t closure_val);

/* Tag access - fundamental primitive for type checking in Lisp */
habu_value_t lisp_get_tag(habu_value_t val);

/* Hash table operations */
habu_value_t make_hash_table(habu_value_t capacity);
habu_value_t gethash(habu_value_t key, habu_value_t ht, habu_value_t default_val);
habu_value_t puthash(habu_value_t key, habu_value_t value, habu_value_t ht);
habu_value_t remhash(habu_value_t key, habu_value_t ht);
habu_value_t hash_table_count(habu_value_t ht);

/* Float (IEEE 754 double precision) operations */
habu_value_t make_float(double value);
double float_value(habu_value_t float_val);
habu_value_t float_add(habu_value_t a, habu_value_t b);
habu_value_t float_sub(habu_value_t a, habu_value_t b);
habu_value_t float_mul(habu_value_t a, habu_value_t b);
habu_value_t float_div(habu_value_t a, habu_value_t b);
habu_value_t float_lt(habu_value_t a, habu_value_t b);
habu_value_t float_gt(habu_value_t a, habu_value_t b);
habu_value_t float_le(habu_value_t a, habu_value_t b);
habu_value_t float_ge(habu_value_t a, habu_value_t b);
habu_value_t float_eq(habu_value_t a, habu_value_t b);
habu_value_t fixnum_to_float(habu_value_t fixnum);
habu_value_t float_to_fixnum(habu_value_t float_val);  /* truncate */

/* Multiple values support
 * Up to 4 values supported (primary + 3 secondary)
 * Secondary values stored in global array
 */
extern int64_t values_count;           /* Number of values (0-4) */
extern habu_value_t values_array[4];   /* Storage for secondary values */

/* Set multiple values - stores v1-v3 in array, returns v0 (primary) */
habu_value_t values_set(int64_t count, habu_value_t v0, habu_value_t v1,
                              habu_value_t v2, habu_value_t v3);

/* Get Nth value - index 0 returns primary, index 1-3 from array */
habu_value_t values_get(int64_t index, habu_value_t primary);

/* Get number of values from last values call */
int64_t values_count_get(void);

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
void io_init(void);
void io_shutdown(void);

/* File I/O */
habu_value_t open_file(habu_value_t path_str, habu_value_t mode_str);
habu_value_t close_file(habu_value_t handle);
habu_value_t read_line(habu_value_t handle);
habu_value_t write_string(habu_value_t handle, habu_value_t str);
habu_value_t read_file(habu_value_t path_str);
habu_value_t write_file(habu_value_t path_str, habu_value_t content_str);

/* Printing */
habu_value_t print_value(habu_value_t value);
habu_value_t println_value(habu_value_t value);

/* Profiling support */
habu_value_t get_time_ns(void);  /* High-resolution timer for profiling */

/* System operations */
habu_value_t system_cmd(habu_value_t cmd_str);  /* Execute shell command */
habu_value_t write_bytes(habu_value_t path_str, habu_value_t byte_vec);  /* Write vector of bytes to file */

/* Bignum operations (arbitrary precision integers) */
habu_value_t make_bignum_from_fixnum(habu_value_t fixnum);
habu_value_t bignum_add(habu_value_t a, habu_value_t b);
habu_value_t bignum_sub(habu_value_t a, habu_value_t b);
habu_value_t bignum_mul(habu_value_t a, habu_value_t b);
habu_value_t bignum_div(habu_value_t a, habu_value_t b);
habu_value_t bignum_to_fixnum(habu_value_t bignum);  /* Returns NIL if doesn't fit */
bool is_bignum(habu_value_t v);

/* Multi-dimensional array operations */
habu_value_t make_array(habu_value_t dims, habu_value_t initial);
habu_value_t aref(habu_value_t array, habu_value_t indices);
habu_value_t aset(habu_value_t array, habu_value_t indices, habu_value_t value);
habu_value_t array_dimensions(habu_value_t array);
habu_value_t array_rank(habu_value_t array);
habu_value_t array_total_size(habu_value_t array);

/* Input primitives - read line from stdin */
char* fgets_line(void);  /* Simple line input */
char* lineedit_readline(const char *prompt);  /* Line editing with readline features */

/* Legacy I/O (for compatibility) */
void write_byte(uint8_t byte);
uint8_t read_byte(void);
void print(const char *str);

/* Time measurement (for benchmarks) */
uint64_t time_ns(void);

#endif /* HABU_H */
