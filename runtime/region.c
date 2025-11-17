#include "habu.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

struct habu_region {
    void *start;
    void *end;
    void *free_ptr;
    size_t size;
};

habu_region_t *habu_region_create(size_t size) {
    habu_region_t *region = malloc(sizeof(habu_region_t));
    if (!region) {
        return NULL;
    }

    /* Allocate aligned memory for the region */
    void *mem = aligned_alloc(16, size);
    if (!mem) {
        free(region);
        return NULL;
    }

    region->start = mem;
    region->end = (char *)mem + size;
    region->free_ptr = mem;
    region->size = size;

    return region;
}

void habu_region_destroy(habu_region_t *region) {
    if (region) {
        free(region->start);
        free(region);
    }
}

void *habu_region_alloc(habu_region_t *region, size_t bytes) {
    assert(region != NULL);

    /* Align to 16 bytes */
    size_t aligned_bytes = (bytes + 15) & ~15;

    void *addr = region->free_ptr;
    void *new_free = (char *)addr + aligned_bytes;

    if (new_free > region->end) {
        return NULL;  /* Region exhausted */
    }

    region->free_ptr = new_free;
    return addr;
}

void habu_region_reset(habu_region_t *region) {
    assert(region != NULL);
    region->free_ptr = region->start;
}

size_t habu_region_used(habu_region_t *region) {
    assert(region != NULL);
    return (char *)region->free_ptr - (char *)region->start;
}

/* Region-based object allocation */

habu_value_t habu_region_cons(habu_region_t *region, habu_value_t car, habu_value_t cdr) {
    size_t total_size = sizeof(habu_header_t) + sizeof(habu_cons_t);
    void *mem = habu_region_alloc(region, total_size);
    if (!mem) {
        return NIL;  /* Out of memory */
    }

    /* Set header */
    habu_header_t *header = (habu_header_t *)mem;
    header->type = TYPE_CONS;
    header->size = sizeof(habu_cons_t);
    header->gc_color = GC_WHITE;
    header->gen_age = GEN_OLD;  /* Region objects don't participate in GC */
    header->reserved = 0;

    /* Set cons data */
    habu_cons_t *cons = (habu_cons_t *)((char *)mem + sizeof(habu_header_t));
    cons->car = car;
    cons->cdr = cdr;

    return tag_pointer(cons, TAG_CONS);
}

habu_value_t habu_region_make_vector(habu_region_t *region, size_t length) {
    size_t data_size = sizeof(habu_vector_t) + length * sizeof(habu_value_t);
    size_t total_size = sizeof(habu_header_t) + data_size;

    void *mem = habu_region_alloc(region, total_size);
    if (!mem) {
        return NIL;
    }

    habu_header_t *header = (habu_header_t *)mem;
    header->type = TYPE_VECTOR;
    header->size = data_size;
    header->gc_color = GC_WHITE;
    header->gen_age = GEN_OLD;
    header->reserved = 0;

    habu_vector_t *vector = (habu_vector_t *)((char *)mem + sizeof(habu_header_t));
    vector->length = length;

    /* Initialize to NIL */
    for (size_t i = 0; i < length; i++) {
        vector->data[i] = NIL;
    }

    return tag_pointer(vector, TAG_VECTOR);
}

habu_value_t habu_region_make_string(habu_region_t *region, const char *str, size_t length) {
    size_t data_size = sizeof(habu_string_t) + length + 1;  /* +1 for null terminator */
    size_t total_size = sizeof(habu_header_t) + data_size;

    void *mem = habu_region_alloc(region, total_size);
    if (!mem) {
        return NIL;
    }

    habu_header_t *header = (habu_header_t *)mem;
    header->type = TYPE_STRING;
    header->size = data_size;
    header->gc_color = GC_WHITE;
    header->gen_age = GEN_OLD;
    header->reserved = 0;

    habu_string_t *string = (habu_string_t *)((char *)mem + sizeof(habu_header_t));
    string->length = length;
    memcpy(string->data, str, length);
    string->data[length] = '\0';

    return tag_pointer(string, TAG_STRING);
}
