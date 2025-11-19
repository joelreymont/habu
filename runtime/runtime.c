#define _POSIX_C_SOURCE 199309L
#include "habu.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <unistd.h>

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

/* Read a line from stdin (replaces lineedit for minimal REPL) */
char* habu_fgets_line(void) {
    char *line = NULL;
    size_t len = 0;

    #ifdef __APPLE__
    // macOS: use fgets with fixed buffer
    size_t bufsize = 1024;
    line = malloc(bufsize);
    if (!line) return NULL;

    if (fgets(line, bufsize, stdin) == NULL) {
        free(line);
        return NULL;
    }

    // Remove trailing newline
    len = strlen(line);
    if (len > 0 && line[len-1] == '\n') {
        line[len-1] = '\0';
    }
    #else
    // Linux: use getline
    ssize_t read = getline(&line, &len, stdin);
    if (read == -1) {
        if (line) free(line);
        return NULL;
    }

    // Remove trailing newline
    if (read > 0 && line[read-1] == '\n') {
        line[read-1] = '\0';
    }
    #endif

    return line;
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
    habu_write_barrier(c, value);
}

void habu_set_cdr(habu_value_t cons, habu_value_t value) {
    if (is_nil(cons)) {
        return;
    }
    habu_cons_t *c = value_to_cons(cons);
    c->cdr = value;
    habu_write_barrier(c, value);
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
    habu_write_barrier(v, value);
}

/* Closure support */

habu_value_t habu_make_closure(void *code_ptr, habu_value_t env) {
    size_t size = sizeof(habu_closure_t);
    habu_closure_t *closure = habu_gc_alloc(size, TYPE_CLOSURE);
    closure->code = code_ptr;
    closure->env = env;
    return tag_pointer(closure, TAG_CLOSURE);
}

void *habu_closure_code(habu_value_t closure_val) {
    if (get_tag(closure_val) != TAG_CLOSURE) {
        return NULL;
    }
    habu_closure_t *closure = value_to_closure(closure_val);
    return closure->code;
}

habu_value_t habu_closure_env(habu_value_t closure_val) {
    if (get_tag(closure_val) != TAG_CLOSURE) {
        return NIL;
    }
    habu_closure_t *closure = value_to_closure(closure_val);
    return closure->env;
}

/* Tag access - fundamental primitive for implementing type predicates in Lisp */

habu_value_t habu_get_tag(habu_value_t val) {
    return fixnum_to_value(get_tag(val));
}

/* String operations for implementing reader in Lisp */

habu_value_t habu_string_ref(habu_value_t str_val, size_t index) {
    if (get_tag(str_val) != TAG_STRING) {
        return NIL;
    }
    habu_string_t *str = value_to_string(str_val);
    if (index >= str->length) {
        return NIL;
    }
    // Return character as fixnum
    return fixnum_to_value((int64_t)(unsigned char)str->data[index]);
}

size_t habu_string_length_raw(habu_value_t str_val) {
    if (get_tag(str_val) != TAG_STRING) {
        return 0;
    }
    habu_string_t *str = value_to_string(str_val);
    return str->length;
}

const char* habu_string_to_cstr(habu_value_t str_val) {
    if (get_tag(str_val) != TAG_STRING) {
        return "";
    }
    habu_string_t *str = value_to_string(str_val);
    // Note: Habu strings are null-terminated
    return str->data;
}

/* Symbol operations */

habu_value_t habu_make_symbol_from_string(habu_value_t str_val) {
    if (get_tag(str_val) != TAG_STRING) {
        return NIL;
    }
    habu_string_t *str = value_to_string(str_val);
    return habu_make_symbol(str->data);
}

habu_value_t habu_symbol_name(habu_value_t sym_val) {
    if (get_tag(sym_val) != TAG_SYMBOL) {
        return NIL;
    }
    habu_symbol_t *sym = value_to_symbol(sym_val);
    return sym->name;
}

/* GC functions implemented in gc.c */
