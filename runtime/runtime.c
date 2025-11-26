#define _POSIX_C_SOURCE 200112L  /* POSIX.1-2001 for snprintf */
#include "habu.h"
#include <stdio.h>

/* Export runtime function addresses for host/JIT plumbing */
void print_runtime_addrs(void) {
    /* Memory allocation */
    printf("CONS_ADDR=0x%lx\n", (unsigned long)(void*)cons);
    printf("MAKE_VECTOR_ADDR=0x%lx\n", (unsigned long)(void*)make_vector);
    printf("MAKE_STRING_ADDR=0x%lx\n", (unsigned long)(void*)make_string);
    printf("MAKE_SYMBOL_ADDR=0x%lx\n", (unsigned long)(void*)make_symbol);

    /* List accessors */
    printf("CAR_ADDR=0x%lx\n", (unsigned long)(void*)car);
    printf("CDR_ADDR=0x%lx\n", (unsigned long)(void*)cdr);
    printf("SET_CAR_ADDR=0x%lx\n", (unsigned long)(void*)set_car);
    printf("SET_CDR_ADDR=0x%lx\n", (unsigned long)(void*)set_cdr);

    /* Vector operations */
    printf("VECTOR_REF_ADDR=0x%lx\n", (unsigned long)(void*)vector_ref);
    printf("VECTOR_SET_ADDR=0x%lx\n", (unsigned long)(void*)vector_set);

    /* String operations */
    printf("STRING_REF_ADDR=0x%lx\n", (unsigned long)(void*)string_ref);
    printf("STRING_LENGTH_RAW_ADDR=0x%lx\n", (unsigned long)(void*)string_length_raw);
    printf("STRING_CONCAT_ADDR=0x%lx\n", (unsigned long)(void*)string_concat);
    printf("STRING_SUBSTRING_ADDR=0x%lx\n", (unsigned long)(void*)string_substring);
    printf("FIXNUM_TO_STRING_ADDR=0x%lx\n", (unsigned long)(void*)fixnum_to_string);
    printf("MAKE_STRING_FROM_VECTOR_ADDR=0x%lx\n", (unsigned long)(void*)make_string_from_vector);

    /* Symbol operations */
    printf("MAKE_SYMBOL_FROM_STRING_ADDR=0x%lx\n", (unsigned long)(void*)make_symbol_from_string);
    printf("SYMBOL_NAME_ADDR=0x%lx\n", (unsigned long)(void*)symbol_name);

    /* Closure operations */
    printf("MAKE_CLOSURE_ADDR=0x%lx\n", (unsigned long)(void*)make_closure);
    printf("CLOSURE_CODE_ADDR=0x%lx\n", (unsigned long)(void*)closure_code);
    printf("CLOSURE_ENV_ADDR=0x%lx\n", (unsigned long)(void*)closure_env);

    /* Type operations */
    printf("LISP_GET_TAG_ADDR=0x%lx\n", (unsigned long)(void*)lisp_get_tag);

    /* I/O operations */
    printf("PRINT_ADDR=0x%lx\n", (unsigned long)(void*)print);
    printf("WRITE_BYTE_ADDR=0x%lx\n", (unsigned long)(void*)write_byte);
    printf("READ_BYTE_ADDR=0x%lx\n", (unsigned long)(void*)read_byte);
    printf("FGETS_LINE_ADDR=0x%lx\n", (unsigned long)(void*)fgets_line);
}
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <unistd.h>

bool gc_enabled = true;

/* I/O functions */

void write_byte(uint8_t byte) {
    putchar(byte);
}

uint8_t read_byte(void) {
    return (uint8_t)getchar();
}

void print(const char *str) {
    fputs(str, stdout);
    fflush(stdout);
}

/* Read a line from stdin (replaces lineedit for minimal REPL) */
char* fgets_line(void) {
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

uint64_t time_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}

/* Accessors (will use GC allocation later) */

habu_value_t car(habu_value_t cons) {
    if (is_nil(cons)) {
        return NIL;
    }
    habu_cons_t *c = value_to_cons(cons);
    return c->car;
}

habu_value_t cdr(habu_value_t cons) {
    if (is_nil(cons)) {
        return NIL;
    }
    habu_cons_t *c = value_to_cons(cons);
    return c->cdr;
}

void set_car(habu_value_t cons, habu_value_t value) {
    if (is_nil(cons)) {
        return;
    }
    habu_cons_t *c = value_to_cons(cons);
    c->car = value;
    write_barrier(c, value);
}

void set_cdr(habu_value_t cons, habu_value_t value) {
    if (is_nil(cons)) {
        return;
    }
    habu_cons_t *c = value_to_cons(cons);
    c->cdr = value;
    write_barrier(c, value);
}

habu_value_t vector_ref(habu_value_t vector, size_t index) {
    if (get_tag(vector) != TAG_VECTOR) {
        return NIL;
    }
    habu_vector_t *v = value_to_vector(vector);
    if (index >= v->length) {
        return NIL;  /* Out of bounds */
    }
    return v->data[index];
}

void vector_set(habu_value_t vector, size_t index, habu_value_t value) {
    if (get_tag(vector) != TAG_VECTOR) {
        return;
    }
    habu_vector_t *v = value_to_vector(vector);
    if (index >= v->length) {
        return;  /* Out of bounds */
    }
    v->data[index] = value;
    write_barrier(v, value);
}

/* Closure support */

habu_value_t make_closure(void *code_ptr, habu_value_t env) {
    size_t size = sizeof(habu_closure_t);
    habu_closure_t *closure = gc_alloc(size, TYPE_CLOSURE);
    if (!closure) {
        fprintf(stderr, "ERROR: Out of memory in habu_make_closure\n");
        abort();
    }
    closure->code = code_ptr;
    closure->env = env;
    return tag_pointer(closure, TAG_CLOSURE);
}

void *closure_code(habu_value_t closure_val) {
    if (get_tag(closure_val) != TAG_CLOSURE) {
        return NULL;
    }
    habu_closure_t *closure = value_to_closure(closure_val);
    return closure->code;
}

habu_value_t closure_env(habu_value_t closure_val) {
    if (get_tag(closure_val) != TAG_CLOSURE) {
        return NIL;
    }
    habu_closure_t *closure = value_to_closure(closure_val);
    return closure->env;
}

/* Tag access - fundamental primitive for implementing type predicates in Lisp */

habu_value_t lisp_get_tag(habu_value_t val) {
    return fixnum_to_value(get_tag(val));
}

/* String operations for implementing reader in Lisp */

habu_value_t string_ref(habu_value_t str_val, size_t index) {
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

size_t string_length_raw(habu_value_t str_val) {
    if (get_tag(str_val) != TAG_STRING) {
        return 0;
    }
    habu_string_t *str = value_to_string(str_val);
    return str->length;
}

const char* string_to_cstr(habu_value_t str_val) {
    if (get_tag(str_val) != TAG_STRING) {
        return "";
    }
    habu_string_t *str = value_to_string(str_val);
    // Note: Habu strings are null-terminated
    return str->data;
}

/* Create string from vector of character codes (for reader) */
habu_value_t make_string_from_vector(habu_value_t vec_val) {
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
    habu_value_t result = make_string(buf, len);
    free(buf);
    return result;
}

/* String concatenation */
habu_value_t string_concat(habu_value_t str1_val, habu_value_t str2_val) {
    if (get_tag(str1_val) != TAG_STRING || get_tag(str2_val) != TAG_STRING) {
        return NIL;
    }

    /* Root inputs - concatenation triggers allocation which can trigger GC */
    gc_add_root(&str1_val);
    gc_add_root(&str2_val);

    habu_string_t *str1 = value_to_string(str1_val);
    habu_string_t *str2 = value_to_string(str2_val);

    size_t len1 = str1->length;
    size_t len2 = str2->length;
    size_t total_len = len1 + len2;

    /* Allocate concatenated string buffer */
    char *buf = (char*)malloc(total_len + 1);
    if (!buf) {
        gc_remove_root(&str2_val);
        gc_remove_root(&str1_val);
        return NIL;
    }

    /* Copy both strings */
    memcpy(buf, str1->data, len1);
    memcpy(buf + len1, str2->data, len2);
    buf[total_len] = '\0';

    /* Create result string */
    habu_value_t result = make_string(buf, total_len);
    free(buf);

    gc_remove_root(&str2_val);
    gc_remove_root(&str1_val);
    return result;
}

/* String substring */
habu_value_t string_substring(habu_value_t str_val, habu_value_t start_val, habu_value_t end_val) {
    if (get_tag(str_val) != TAG_STRING || !is_fixnum(start_val) || !is_fixnum(end_val)) {
        return NIL;
    }

    /* Root input string */
    gc_add_root(&str_val);

    habu_string_t *str = value_to_string(str_val);
    int64_t start = value_to_fixnum(start_val);
    int64_t end = value_to_fixnum(end_val);

    /* Bounds checking */
    if (start < 0) start = 0;
    if (end > (int64_t)str->length) end = str->length;
    if (start >= end) {
        gc_remove_root(&str_val);
        return make_string("", 0);  /* Empty string */
    }

    size_t sub_len = end - start;

    /* Allocate substring buffer */
    char *buf = (char*)malloc(sub_len + 1);
    if (!buf) {
        gc_remove_root(&str_val);
        return NIL;
    }

    /* Copy substring */
    memcpy(buf, str->data + start, sub_len);
    buf[sub_len] = '\0';

    /* Create result string */
    habu_value_t result = make_string(buf, sub_len);
    free(buf);

    gc_remove_root(&str_val);
    return result;
}

/* Compare two strings for equality
 * Returns tagged fixnum: 1 if equal, 0 if not equal
 */
habu_value_t string_equal(habu_value_t str1_val, habu_value_t str2_val) {
    if (get_tag(str1_val) != TAG_STRING || get_tag(str2_val) != TAG_STRING) {
        return fixnum_to_value(0);
    }

    habu_string_t *str1 = value_to_string(str1_val);
    habu_string_t *str2 = value_to_string(str2_val);

    /* Different lengths means not equal */
    if (str1->length != str2->length) {
        return fixnum_to_value(0);
    }

    /* Compare bytes */
    if (memcmp(str1->data, str2->data, str1->length) == 0) {
        return fixnum_to_value(1);
    }
    return fixnum_to_value(0);
}

/* Convert fixnum to string */
habu_value_t fixnum_to_string(habu_value_t num_val) {
    if (!is_fixnum(num_val)) {
        return NIL;
    }

    int64_t num = value_to_fixnum(num_val);
    char buf[32];  /* Enough for 64-bit integer */
    snprintf(buf, sizeof(buf), "%lld", (long long)num);

    return make_string(buf, strlen(buf));
}

/* Symbol operations */

habu_value_t make_symbol_from_string(habu_value_t str_val) {
    if (get_tag(str_val) != TAG_STRING) {
        return NIL;
    }

    /* Root the input string - habu_make_symbol can trigger GC */
    gc_add_root(&str_val);

    habu_string_t *str = value_to_string(str_val);
    habu_value_t result = make_symbol(str->data);

    gc_remove_root(&str_val);
    return result;
}

habu_value_t symbol_name(habu_value_t sym_val) {
    if (get_tag(sym_val) != TAG_SYMBOL) {
        return NIL;
    }
    habu_symbol_t *sym = value_to_symbol(sym_val);
    return sym->name;
}

/* Gensym - generate unique uninterned symbol */
static int64_t gensym_counter = 0;

habu_value_t gensym(habu_value_t prefix_val) {
    char buf[128];
    const char *prefix = "G";

    /* Get prefix from string argument if provided */
    if (get_tag(prefix_val) == TAG_STRING) {
        habu_string_t *str = value_to_string(prefix_val);
        prefix = str->data;
    }

    /* Generate unique name */
    snprintf(buf, sizeof(buf), "%s%lld", prefix, (long long)gensym_counter++);

    /* Create symbol directly without interning (uninterned symbol)
     * For simplicity, we use the intern table but with a unique name */
    return make_symbol(buf);
}

/* Multiple values support */

int64_t values_count = 1;              /* Default: single value */
habu_value_t values_array[4] = {0};    /* Storage for secondary values */

habu_value_t values_set(int64_t count, habu_value_t v0, habu_value_t v1,
                              habu_value_t v2, habu_value_t v3) {
    values_count = count;
    if (count > 1) values_array[0] = v1;
    if (count > 2) values_array[1] = v2;
    if (count > 3) values_array[2] = v3;
    return count > 0 ? v0 : NIL;
}

habu_value_t values_get(int64_t index, habu_value_t primary) {
    if (index >= values_count) return NIL;
    if (index == 0) return primary;
    return values_array[index - 1];
}

int64_t values_count_get(void) {
    return values_count;
}

/* GC functions implemented in gc.c */
