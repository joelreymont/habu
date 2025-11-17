#ifndef HABU_OBJECT_H
#define HABU_OBJECT_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* Object representation for Habu
 *
 * 64-bit tagged pointers:
 * - Fixnums: [60-bit value][0000]
 * - Pointers: [60-bit address][type-tag]
 *
 * All heap objects have an 8-byte header before the object pointer.
 */

typedef uint64_t habu_value_t;
typedef int64_t habu_fixnum_t;

/* Type tags (lower 4 bits) */
#define TAG_FIXNUM  0x0
#define TAG_CONS    0x1
#define TAG_SYMBOL  0x2
#define TAG_VECTOR  0x3
#define TAG_STRING  0x4
#define TAG_CLOSURE 0x5
#define TAG_MASK    0xF

/* Object header
 *
 * Layout (16 bytes total for proper alignment):
 * [type:8][size:40][gc_color:2][gen_age:6][reserved:8][padding:64]
 */
typedef struct {
    uint64_t type : 8;      /* Detailed type */
    uint64_t size : 40;     /* Size in bytes */
    uint64_t gc_color : 2;  /* GC color (white=0, gray=1, black=2) */
    uint64_t gen_age : 6;   /* Generation age (0-5=young, 6+=old) */
    uint64_t reserved : 8;  /* Reserved for future use */
    uint64_t padding;       /* Pad to 16 bytes for object alignment */
} habu_header_t;

/* GC colors */
#define GC_WHITE 0
#define GC_GRAY  1
#define GC_BLACK 2

/* Generation ages */
#define GEN_YOUNG_MAX 5
#define GEN_OLD 6

/* Object types (detailed) */
#define TYPE_CONS    1
#define TYPE_SYMBOL  2
#define TYPE_VECTOR  3
#define TYPE_STRING  4
#define TYPE_CLOSURE 5

/* Cons cell */
typedef struct {
    habu_value_t car;
    habu_value_t cdr;
} habu_cons_t;

/* Symbol */
typedef struct {
    habu_value_t name;   /* String */
    habu_value_t value;  /* Global value */
    habu_value_t plist;  /* Property list */
} habu_symbol_t;

/* Vector */
typedef struct {
    uint64_t length;
    habu_value_t data[];  /* Flexible array */
} habu_vector_t;

/* String */
typedef struct {
    uint64_t length;
    char data[];  /* Flexible array */
} habu_string_t;

/* Closure */
typedef struct {
    void *code;           /* Function pointer */
    habu_value_t env;     /* Captured environment */
} habu_closure_t;

/* Value operations */
static inline bool is_fixnum(habu_value_t v) {
    return (v & TAG_MASK) == TAG_FIXNUM;
}

static inline bool is_pointer(habu_value_t v) {
    return (v & TAG_MASK) != TAG_FIXNUM;
}

static inline uint64_t get_tag(habu_value_t v) {
    return v & TAG_MASK;
}

static inline void *untag_pointer(habu_value_t v) {
    return (void *)(v & ~TAG_MASK);
}

static inline habu_value_t tag_pointer(void *ptr, uint64_t tag) {
    return (habu_value_t)ptr | tag;
}

static inline habu_fixnum_t value_to_fixnum(habu_value_t v) {
    return (habu_fixnum_t)v >> 4;
}

static inline habu_value_t fixnum_to_value(habu_fixnum_t n) {
    return ((habu_value_t)n << 4) | TAG_FIXNUM;
}

/* Header operations */
static inline habu_header_t *get_header(void *obj) {
    return (habu_header_t *)((uint64_t)obj - sizeof(habu_header_t));
}

static inline uint64_t get_gc_color(void *obj) {
    return get_header(obj)->gc_color;
}

static inline void set_gc_color(void *obj, uint64_t color) {
    get_header(obj)->gc_color = color;
}

static inline uint64_t get_type(void *obj) {
    return get_header(obj)->type;
}

static inline uint64_t get_size(void *obj) {
    return get_header(obj)->size;
}

static inline uint64_t get_gen_age(void *obj) {
    return get_header(obj)->gen_age;
}

static inline void set_gen_age(void *obj, uint64_t age) {
    get_header(obj)->gen_age = age;
}

static inline bool in_old_gen(void *obj) {
    return get_gen_age(obj) >= GEN_OLD;
}

/* Type-specific operations */
static inline habu_cons_t *value_to_cons(habu_value_t v) {
    return (habu_cons_t *)untag_pointer(v);
}

static inline habu_symbol_t *value_to_symbol(habu_value_t v) {
    return (habu_symbol_t *)untag_pointer(v);
}

static inline habu_vector_t *value_to_vector(habu_value_t v) {
    return (habu_vector_t *)untag_pointer(v);
}

static inline habu_string_t *value_to_string(habu_value_t v) {
    return (habu_string_t *)untag_pointer(v);
}

static inline habu_closure_t *value_to_closure(habu_value_t v) {
    return (habu_closure_t *)untag_pointer(v);
}

/* NIL representation */
#define NIL fixnum_to_value(0)

static inline bool is_nil(habu_value_t v) {
    return v == NIL;
}

#endif /* HABU_OBJECT_H */
