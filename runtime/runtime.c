#define _POSIX_C_SOURCE 200112L  /* POSIX.1-2001 for snprintf */
#include "habu.h"
#include <stdio.h>

/* Export runtime function addresses for host/JIT plumbing */
void habu_print_runtime_addrs(void) {
    /* Memory allocation */
    printf("HABU_CONS_ADDR=0x%lx\n", (unsigned long)(void*)habu_cons);
    printf("HABU_MAKE_VECTOR_ADDR=0x%lx\n", (unsigned long)(void*)habu_make_vector);
    printf("HABU_MAKE_STRING_ADDR=0x%lx\n", (unsigned long)(void*)habu_make_string);
    printf("HABU_RUNTIME_MAKE_STRING_ADDR=0x%lx\n", (unsigned long)(void*)habu_runtime_make_string);
    printf("HABU_MAKE_SYMBOL_ADDR=0x%lx\n", (unsigned long)(void*)habu_make_symbol);
    printf("HABU_RUNTIME_FIND_SYMBOL_ADDR=0x%lx\n", (unsigned long)(void*)habu_runtime_find_symbol);

    /* List accessors */
    printf("HABU_CAR_ADDR=0x%lx\n", (unsigned long)(void*)habu_car);
    printf("HABU_CDR_ADDR=0x%lx\n", (unsigned long)(void*)habu_cdr);
    printf("HABU_SET_CAR_ADDR=0x%lx\n", (unsigned long)(void*)habu_set_car);
    printf("HABU_SET_CDR_ADDR=0x%lx\n", (unsigned long)(void*)habu_set_cdr);

    /* Vector operations */
    printf("HABU_VECTOR_REF_ADDR=0x%lx\n", (unsigned long)(void*)habu_vector_ref);
    printf("HABU_VECTOR_SET_ADDR=0x%lx\n", (unsigned long)(void*)habu_vector_set);

    /* String operations */
    printf("HABU_STRING_REF_ADDR=0x%lx\n", (unsigned long)(void*)habu_string_ref);
    printf("HABU_STRING_LENGTH_RAW_ADDR=0x%lx\n", (unsigned long)(void*)habu_string_length_raw);
    printf("HABU_STRING_CONCAT_ADDR=0x%lx\n", (unsigned long)(void*)habu_string_concat);
    printf("HABU_STRING_SUBSTRING_ADDR=0x%lx\n", (unsigned long)(void*)habu_string_substring);
    printf("HABU_FIXNUM_TO_STRING_ADDR=0x%lx\n", (unsigned long)(void*)habu_fixnum_to_string);
    printf("HABU_MAKE_STRING_FROM_VECTOR_ADDR=0x%lx\n", (unsigned long)(void*)habu_make_string_from_vector);

    /* Symbol operations */
    printf("HABU_MAKE_SYMBOL_FROM_STRING_ADDR=0x%lx\n", (unsigned long)(void*)habu_make_symbol_from_string);
    printf("HABU_SYMBOL_NAME_ADDR=0x%lx\n", (unsigned long)(void*)habu_symbol_name);

    /* Closure operations */
    printf("HABU_MAKE_CLOSURE_ADDR=0x%lx\n", (unsigned long)(void*)habu_make_closure);
    printf("HABU_CLOSURE_CODE_ADDR=0x%lx\n", (unsigned long)(void*)habu_closure_code);
    printf("HABU_CLOSURE_ENV_ADDR=0x%lx\n", (unsigned long)(void*)habu_closure_env);

    /* Type operations */
    printf("HABU_GET_TAG_ADDR=0x%lx\n", (unsigned long)(void*)habu_get_tag);

    /* I/O operations */
    printf("HABU_PRINT_ADDR=0x%lx\n", (unsigned long)(void*)habu_print);
    printf("HABU_WRITE_BYTE_ADDR=0x%lx\n", (unsigned long)(void*)habu_write_byte);
    printf("HABU_READ_BYTE_ADDR=0x%lx\n", (unsigned long)(void*)habu_read_byte);
    printf("HABU_FGETS_LINE_ADDR=0x%lx\n", (unsigned long)(void*)habu_fgets_line);
}
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
    if (get_tag(vector) != TAG_VECTOR) {
        return NIL;
    }
    habu_vector_t *v = value_to_vector(vector);
    if (index >= v->length) {
        return NIL;  /* Out of bounds */
    }
    return v->data[index];
}

void habu_vector_set(habu_value_t vector, size_t index, habu_value_t value) {
    if (get_tag(vector) != TAG_VECTOR) {
        return;
    }
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
    if (!closure) {
        fprintf(stderr, "ERROR: Out of memory in habu_make_closure\n");
        abort();
    }
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

/* Create string from vector of character codes (for reader) */
habu_value_t habu_make_string_from_vector(habu_value_t vec_val) {
    if (get_tag(vec_val) != TAG_VECTOR) {
        return NIL;
    }
    habu_vector_t *vec = value_to_vector(vec_val);
    size_t len = vec->length;

    // Allocate string buffer
    char *buf = (char*)malloc(len + 1);
    if (!buf) return NIL;

    // Convert fixnums to characters
    for (size_t i = 0; i < len; i++) {
        habu_value_t ch_val = vec->data[i];
        if (!is_fixnum(ch_val)) {
            free(buf);
            return NIL;
        }
        buf[i] = (char)value_to_fixnum(ch_val);
    }
    buf[len] = '\0';

    // Create string
    habu_value_t result = habu_make_string(buf, len);
    free(buf);
    return result;
}

/* String concatenation */
habu_value_t habu_string_concat(habu_value_t str1_val, habu_value_t str2_val) {
    if (get_tag(str1_val) != TAG_STRING || get_tag(str2_val) != TAG_STRING) {
        return NIL;
    }

    /* Root inputs - concatenation triggers allocation which can trigger GC */
    habu_gc_add_root(&str1_val);
    habu_gc_add_root(&str2_val);

    habu_string_t *str1 = value_to_string(str1_val);
    habu_string_t *str2 = value_to_string(str2_val);

    size_t len1 = str1->length;
    size_t len2 = str2->length;
    size_t total_len = len1 + len2;

    /* Allocate concatenated string buffer */
    char *buf = (char*)malloc(total_len + 1);
    if (!buf) {
        habu_gc_remove_root(&str2_val);
        habu_gc_remove_root(&str1_val);
        return NIL;
    }

    /* Copy both strings */
    memcpy(buf, str1->data, len1);
    memcpy(buf + len1, str2->data, len2);
    buf[total_len] = '\0';

    /* Create result string */
    habu_value_t result = habu_make_string(buf, total_len);
    free(buf);

    habu_gc_remove_root(&str2_val);
    habu_gc_remove_root(&str1_val);
    return result;
}

/* String substring */
habu_value_t habu_string_substring(habu_value_t str_val, habu_value_t start_val, habu_value_t end_val) {
    if (get_tag(str_val) != TAG_STRING || !is_fixnum(start_val) || !is_fixnum(end_val)) {
        return NIL;
    }

    /* Root input string */
    habu_gc_add_root(&str_val);

    habu_string_t *str = value_to_string(str_val);
    int64_t start = value_to_fixnum(start_val);
    int64_t end = value_to_fixnum(end_val);

    /* Bounds checking */
    if (start < 0) start = 0;
    if (end > (int64_t)str->length) end = str->length;
    if (start >= end) {
        habu_gc_remove_root(&str_val);
        return habu_make_string("", 0);  /* Empty string */
    }

    size_t sub_len = end - start;

    /* Allocate substring buffer */
    char *buf = (char*)malloc(sub_len + 1);
    if (!buf) {
        habu_gc_remove_root(&str_val);
        return NIL;
    }

    /* Copy substring */
    memcpy(buf, str->data + start, sub_len);
    buf[sub_len] = '\0';

    /* Create result string */
    habu_value_t result = habu_make_string(buf, sub_len);
    free(buf);

    habu_gc_remove_root(&str_val);
    return result;
}

/* Convert fixnum to string */
habu_value_t habu_fixnum_to_string(habu_value_t num_val) {
    if (!is_fixnum(num_val)) {
        return NIL;
    }

    int64_t num = value_to_fixnum(num_val);
    char buf[32];  /* Enough for 64-bit integer */
    snprintf(buf, sizeof(buf), "%lld", (long long)num);

    return habu_make_string(buf, strlen(buf));
}

/* Symbol operations */

habu_value_t habu_make_symbol_from_string(habu_value_t str_val) {
    if (get_tag(str_val) != TAG_STRING) {
        return NIL;
    }

    /* Root the input string - habu_make_symbol can trigger GC */
    habu_gc_add_root(&str_val);

    habu_string_t *str = value_to_string(str_val);
    habu_value_t result = habu_make_symbol(str->data);

    habu_gc_remove_root(&str_val);
    return result;
}

habu_value_t habu_runtime_make_string(const char *str) {
    return habu_make_string(str, strlen(str));
}

habu_value_t habu_runtime_find_symbol(const char *name) {
    return habu_make_symbol(name);
}

habu_value_t habu_symbol_name(habu_value_t sym_val) {
    if (get_tag(sym_val) != TAG_SYMBOL) {
        return NIL;
    }
    habu_symbol_t *sym = value_to_symbol(sym_val);
    return sym->name;
}

/* GC functions implemented in gc.c */
