#define _POSIX_C_SOURCE 199309L
#include "habu.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

bool habu_gc_enabled = true;

/* I/O functions */

void habu_write_byte(uint8_t byte) {
    putchar(byte);
}

uint8_t habu_read_byte(void) {
    return (uint8_t)getchar();
}

void habu_print(const char *str) {
    fputs(str, stdout);
    fflush(stdout);
}

/* Time measurement */

uint64_t habu_time_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}

/* Accessors (will use GC allocation later) */

habu_value_t habu_car(habu_value_t cons) {
    if (is_nil(cons)) {
        return NIL;
    }
    habu_cons_t *c = value_to_cons(cons);
    return c->car;
}

habu_value_t habu_cdr(habu_value_t cons) {
    if (is_nil(cons)) {
        return NIL;
    }
    habu_cons_t *c = value_to_cons(cons);
    return c->cdr;
}

void habu_set_car(habu_value_t cons, habu_value_t value) {
    if (is_nil(cons)) {
        return;
    }
    habu_cons_t *c = value_to_cons(cons);
    c->car = value;
    /* TODO: Write barrier */
}

void habu_set_cdr(habu_value_t cons, habu_value_t value) {
    if (is_nil(cons)) {
        return;
    }
    habu_cons_t *c = value_to_cons(cons);
    c->cdr = value;
    /* TODO: Write barrier */
}

habu_value_t habu_vector_ref(habu_value_t vector, size_t index) {
    habu_vector_t *v = value_to_vector(vector);
    if (index >= v->length) {
        return NIL;  /* Out of bounds */
    }
    return v->data[index];
}

void habu_vector_set(habu_value_t vector, size_t index, habu_value_t value) {
    habu_vector_t *v = value_to_vector(vector);
    if (index >= v->length) {
        return;  /* Out of bounds */
    }
    v->data[index] = value;
    /* TODO: Write barrier */
}

/* GC functions implemented in gc.c */
