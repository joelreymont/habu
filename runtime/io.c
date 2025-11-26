/* I/O System for Habu Lisp
 *
 * Provides file I/O, string I/O, and formatted output.
 */

#include "habu.h"
#include "object.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

/* Maximum number of open file streams */
#define MAX_OPEN_FILES 256

/* File handle table */
static FILE *file_handles[MAX_OPEN_FILES] = {NULL};
static int next_handle = 0;

/* Standard streams are pre-allocated */
#define STDIN_HANDLE 0
#define STDOUT_HANDLE 1
#define STDERR_HANDLE 2

/* Initialize I/O system */
void habu_io_init(void) {
    file_handles[STDIN_HANDLE] = stdin;
    file_handles[STDOUT_HANDLE] = stdout;
    file_handles[STDERR_HANDLE] = stderr;
    next_handle = 3;
}

/* Shutdown I/O system */
void habu_io_shutdown(void) {
    /* Close all open files (except standard streams) */
    for (int i = 3; i < MAX_OPEN_FILES; i++) {
        if (file_handles[i]) {
            fclose(file_handles[i]);
            file_handles[i] = NULL;
        }
    }
}

/* Open a file and return a handle
 *
 * Args:
 *   path_str - habu_value_t tagged string with file path
 *   mode_str - habu_value_t tagged string with mode ("r", "w", "a", "r+", etc.)
 *
 * Returns:
 *   Fixnum handle (>= 0) on success, NIL on failure
 */
habu_value_t habu_open_file(habu_value_t path_str, habu_value_t mode_str) {
    if (get_tag(path_str) != TAG_STRING || get_tag(mode_str) != TAG_STRING) {
        return NIL;
    }

    habu_string_t *path = value_to_string(path_str);
    habu_string_t *mode = value_to_string(mode_str);

    /* Find free handle slot */
    int handle = -1;
    for (int i = 3; i < MAX_OPEN_FILES; i++) {
        if (!file_handles[i]) {
            handle = i;
            break;
        }
    }

    if (handle == -1) {
        /* No free handles */
        return NIL;
    }

    /* Open file */
    FILE *f = fopen(path->data, mode->data);
    if (!f) {
        return NIL;
    }

    file_handles[handle] = f;
    return fixnum_to_value(handle);
}

/* Close a file handle
 *
 * Args:
 *   handle - Fixnum handle returned by habu_open_file
 *
 * Returns:
 *   Fixnum 0 on success, NIL on failure
 */
habu_value_t habu_close_file(habu_value_t handle) {
    if (!is_fixnum(handle)) {
        return NIL;
    }

    habu_fixnum_t h = value_to_fixnum(handle);
    if (h < 3 || h >= MAX_OPEN_FILES || !file_handles[h]) {
        return NIL;
    }

    fclose(file_handles[h]);
    file_handles[h] = NULL;
    return fixnum_to_value(0);
}

/* Read a line from a file
 *
 * Args:
 *   handle - Fixnum handle to read from
 *
 * Returns:
 *   Tagged string with line content (without newline), or NIL on EOF/error
 */
habu_value_t habu_read_line(habu_value_t handle) {
    if (!is_fixnum(handle)) {
        return NIL;
    }

    habu_fixnum_t h = value_to_fixnum(handle);
    if (h < 0 || h >= MAX_OPEN_FILES || !file_handles[h]) {
        return NIL;
    }

    FILE *f = file_handles[h];

    /* Read line into buffer */
    char buffer[4096];
    if (!fgets(buffer, sizeof(buffer), f)) {
        return NIL;  /* EOF or error */
    }

    /* Strip trailing newline */
    size_t len = strlen(buffer);
    if (len > 0 && buffer[len-1] == '\n') {
        buffer[len-1] = '\0';
        len--;
    }

    /* Create Habu string */
    return habu_make_string(buffer, len);
}

/* Write a string to a file
 *
 * Args:
 *   handle - Fixnum handle to write to
 *   str - Tagged string to write
 *
 * Returns:
 *   Fixnum number of bytes written, or NIL on error
 */
habu_value_t habu_write_string(habu_value_t handle, habu_value_t str) {
    if (!is_fixnum(handle) || get_tag(str) != TAG_STRING) {
        return NIL;
    }

    habu_fixnum_t h = value_to_fixnum(handle);
    if (h < 0 || h >= MAX_OPEN_FILES || !file_handles[h]) {
        return NIL;
    }

    FILE *f = file_handles[h];
    habu_string_t *s = value_to_string(str);

    size_t written = fwrite(s->data, 1, s->length, f);
    return fixnum_to_value(written);
}

/* Read entire file as string
 *
 * Args:
 *   path_str - Tagged string with file path
 *
 * Returns:
 *   Tagged string with file contents, or NIL on error
 */
habu_value_t habu_read_file(habu_value_t path_str) {
    if (get_tag(path_str) != TAG_STRING) {
        return NIL;
    }

    habu_string_t *path = value_to_string(path_str);
    FILE *f = fopen(path->data, "r");
    if (!f) {
        return NIL;
    }

    /* Get file size */
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    /* Read entire file */
    char *buffer = malloc(size + 1);
    if (!buffer) {
        fclose(f);
        return NIL;
    }

    size_t read_size = fread(buffer, 1, size, f);
    buffer[read_size] = '\0';
    fclose(f);

    /* Create Habu string */
    habu_value_t result = habu_make_string(buffer, read_size);
    free(buffer);

    return result;
}

/* Write string to file
 *
 * Args:
 *   path_str - Tagged string with file path
 *   content_str - Tagged string with content to write
 *
 * Returns:
 *   Fixnum 0 on success, NIL on error
 */
habu_value_t habu_write_file(habu_value_t path_str, habu_value_t content_str) {
    if (get_tag(path_str) != TAG_STRING || get_tag(content_str) != TAG_STRING) {
        return NIL;
    }

    habu_string_t *path = value_to_string(path_str);
    habu_string_t *content = value_to_string(content_str);

    FILE *f = fopen(path->data, "w");
    if (!f) {
        return NIL;
    }

    size_t written = fwrite(content->data, 1, content->length, f);
    fclose(f);

    if (written != content->length) {
        return NIL;
    }

    return fixnum_to_value(0);
}

/* Forward declaration for recursive printing */
static void print_value_internal(habu_value_t value, int depth);

/* Print a list (cons cell chain) */
static void print_list(habu_value_t list, int depth) {
    if (depth > 100) {
        printf("...");  /* Prevent infinite recursion */
        return;
    }

    printf("(");
    habu_value_t current = list;
    int first = 1;

    while (!is_nil(current)) {
        if (!first) printf(" ");
        first = 0;

        if (get_tag(current) != TAG_CONS) {
            /* Improper list: (1 2 . 3) */
            printf(". ");
            print_value_internal(current, depth + 1);
            break;
        }

        habu_cons_t *cons = value_to_cons(current);
        print_value_internal(cons->car, depth + 1);
        current = cons->cdr;
    }

    printf(")");
}

static void print_value_internal(habu_value_t value, int depth) {
    if (is_fixnum(value)) {
        printf("%lld", (long long)value_to_fixnum(value));
    } else if (is_nil(value)) {
        printf("nil");
    } else {
        uint64_t tag = get_tag(value);
        switch (tag) {
            case TAG_STRING: {
                habu_string_t *s = value_to_string(value);
                printf("%.*s", (int)s->length, s->data);
                break;
            }
            case TAG_CONS:
                print_list(value, depth);
                break;
            case TAG_VECTOR: {
                habu_vector_t *v = value_to_vector(value);
                printf("#(");
                for (size_t i = 0; i < v->length; i++) {
                    if (i > 0) printf(" ");
                    print_value_internal(v->data[i], depth + 1);
                }
                printf(")");
                break;
            }
            case TAG_SYMBOL: {
                habu_symbol_t *sym = value_to_symbol(value);
                habu_string_t *name = value_to_string(sym->name);
                printf("%.*s", (int)name->length, name->data);
                break;
            }
            case TAG_CLOSURE:
                printf("#<closure>");
                break;
            default:
                printf("<object:%llx>", (unsigned long long)value);
        }
    }
}

/* Print a Habu value to stdout (for debugging)
 *
 * Args:
 *   value - Any Habu value
 *
 * Returns:
 *   NIL
 */
habu_value_t habu_print_value(habu_value_t value) {
    print_value_internal(value, 0);
    return NIL;
}

/* Print with newline */
habu_value_t habu_println_value(habu_value_t value) {
    habu_print_value(value);
    printf("\n");
    return NIL;
}

/* High-resolution time measurement for profiling
 *
 * Returns current time in nanoseconds as a tagged fixnum.
 * Note: For times > ~292 years this would overflow, but that's fine for profiling.
 * The value is suitable for computing elapsed time via subtraction.
 */
habu_value_t habu_get_time_ns(void) {
    uint64_t ns = habu_time_ns();
    /* Return as fixnum - note this may truncate for very large values,
     * but for elapsed time calculations this is fine since we only
     * care about differences, not absolute values */
    return fixnum_to_value((int64_t)ns);
}

/* Execute shell command
 *
 * Args:
 *   cmd_str - Tagged string with command to execute
 *
 * Returns:
 *   Fixnum exit status from system() call
 */
habu_value_t habu_system(habu_value_t cmd_str) {
    if (get_tag(cmd_str) != TAG_STRING) {
        return fixnum_to_value(-1);
    }

    habu_string_t *cmd = value_to_string(cmd_str);
    int status = system(cmd->data);
    return fixnum_to_value((int64_t)status);
}

/* Write a vector of bytes to a file
 *
 * Args:
 *   path_str - Tagged string with file path
 *   byte_vec - Tagged vector of fixnum bytes (0-255)
 *
 * Returns:
 *   Fixnum 0 on success, NIL on error
 */
habu_value_t habu_write_bytes(habu_value_t path_str, habu_value_t byte_vec) {
    if (get_tag(path_str) != TAG_STRING || get_tag(byte_vec) != TAG_VECTOR) {
        return NIL;
    }

    habu_string_t *path = value_to_string(path_str);
    habu_vector_t *vec = value_to_vector(byte_vec);

    FILE *f = fopen(path->data, "wb");
    if (!f) {
        return NIL;
    }

    /* Write each byte from the vector */
    for (size_t i = 0; i < vec->length; i++) {
        habu_value_t val = vec->data[i];
        if (is_fixnum(val)) {
            uint8_t byte = (uint8_t)(value_to_fixnum(val) & 0xFF);
            fwrite(&byte, 1, 1, f);
        }
    }

    fclose(f);
    return fixnum_to_value(0);
}
