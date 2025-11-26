/* habu-main.c - Main Habu executable with REPL
 *
 * Usage:
 *   ./habu              - Start REPL
 *   ./habu <file.bin>   - Run compiled bytecode
 *   ./habu --help       - Show help
 *
 * REPL prompt uses snake emoji with asterisk.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include "runtime/habu.h"

/* Habu tagged value representation */
#define HABU_TAG_FIXNUM(n) ((int64_t)(n) << 4)
#define HABU_UNTAG_FIXNUM(v) ((int64_t)(v) >> 4)
#define HABU_IS_FIXNUM(v) (((v) & 0xF) == 0)
#define HABU_IS_CONS(v) (((v) & 0xF) == 1)
#define HABU_IS_SYMBOL(v) (((v) & 0xF) == 2)
#define HABU_IS_VECTOR(v) (((v) & 0xF) == 3)
#define HABU_IS_STRING(v) (((v) & 0xF) == 4)
#define HABU_IS_CLOSURE(v) (((v) & 0xF) == 5)
#define HABU_IS_FLOAT(v) (((v) & 0xF) == 7)

/* Function pointer type for compiled code */
typedef int64_t (*compiled_fn_t)(void** runtime_table);

/* Runtime function table */
static void* g_runtime_table[64];
static void *g_exec_mem = NULL;
static size_t g_exec_size = 0;

/* Forward declaration */
extern char* lineedit_readline(const char *prompt);

static void print_version(void) {
    printf("Habu Lisp Compiler v0.1.0\n");
    printf("Self-hosting ARM64 Lisp implementation\n");
}

static void print_help(const char *prog) {
    printf("Usage: %s [OPTIONS] [FILE]\n\n", prog);
    printf("Options:\n");
    printf("  --help, -h     Show this help message\n");
    printf("  --version, -v  Show version information\n");
    printf("\n");
    printf("If FILE is provided, execute it as compiled bytecode (.bin).\n");
    printf("Otherwise, start an interactive REPL.\n");
    printf("\n");
    printf("REPL Commands:\n");
    printf("  ,quit          Exit the REPL\n");
    printf("  ,help          Show REPL help\n");
}

static void setup_runtime_table(void *exec_mem) {
    g_runtime_table[0] = (void*)cons;
    g_runtime_table[1] = (void*)car;
    g_runtime_table[2] = (void*)cdr;
    g_runtime_table[3] = (void*)make_closure;
    g_runtime_table[4] = (void*)closure_code;
    g_runtime_table[5] = (void*)closure_env;
    g_runtime_table[6] = exec_mem;  /* Code base pointer for closures */
    g_runtime_table[7] = (void*)make_vector;
    g_runtime_table[8] = (void*)vector_set;
    g_runtime_table[9] = (void*)vector_ref;
    g_runtime_table[10] = (void*)make_string_from_vector;
    g_runtime_table[11] = (void*)make_symbol_from_string;
    g_runtime_table[12] = (void*)string_length_raw;
    g_runtime_table[13] = (void*)symbol_name;
    g_runtime_table[14] = (void*)set_car;
    g_runtime_table[15] = (void*)set_cdr;
    g_runtime_table[16] = (void*)string_ref;
    g_runtime_table[17] = (void*)values_set;
    g_runtime_table[18] = (void*)values_get;
    g_runtime_table[19] = (void*)make_hash_table;
    g_runtime_table[20] = (void*)gethash;
    g_runtime_table[21] = (void*)puthash;
    g_runtime_table[22] = (void*)remhash;
    g_runtime_table[23] = (void*)hash_table_count;
    g_runtime_table[24] = (void*)string_concat;
    g_runtime_table[25] = (void*)string_substring;
    g_runtime_table[26] = (void*)fixnum_to_string;
    g_runtime_table[27] = (void*)values_count_get;
    g_runtime_table[28] = (void*)gensym;
    /* Float operations (IEEE 754) */
    g_runtime_table[29] = (void*)make_float;
    g_runtime_table[30] = (void*)float_add;
    g_runtime_table[31] = (void*)float_sub;
    g_runtime_table[32] = (void*)float_mul;
    g_runtime_table[33] = (void*)float_div;
    g_runtime_table[34] = (void*)float_lt;
    g_runtime_table[35] = (void*)float_gt;
    g_runtime_table[36] = (void*)float_le;
    g_runtime_table[37] = (void*)float_ge;
    g_runtime_table[38] = (void*)float_eq;
    g_runtime_table[39] = (void*)fixnum_to_float;
    g_runtime_table[40] = (void*)float_to_fixnum;
    g_runtime_table[41] = (void*)float_value;
    /* File I/O operations */
    g_runtime_table[42] = (void*)open_file;
    g_runtime_table[43] = (void*)close_file;
    g_runtime_table[44] = (void*)read_line;
    g_runtime_table[45] = (void*)write_string;
    g_runtime_table[46] = (void*)read_file;
    g_runtime_table[47] = (void*)write_file;
    /* Print operations */
    g_runtime_table[48] = (void*)print_value;
    g_runtime_table[49] = (void*)println_value;
    /* Profiling operations */
    g_runtime_table[50] = (void*)get_time_ns;
}

static int run_bytecode_file(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) {
        perror("fopen");
        return 1;
    }

    /* Get file size */
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (size <= 0 || size > 16*1024*1024) {
        fprintf(stderr, "Invalid bytecode size: %ld\n", size);
        fclose(f);
        return 1;
    }

    /* Read bytecode into buffer */
    uint8_t *code = malloc(size);
    if (!code) {
        perror("malloc");
        fclose(f);
        return 1;
    }

    size_t read_bytes = fread(code, 1, size, f);
    fclose(f);

    if (read_bytes != (size_t)size) {
        fprintf(stderr, "Failed to read all bytecode\n");
        free(code);
        return 1;
    }

    /* Allocate executable memory */
    void *exec_mem = mmap(NULL, size,
                          PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS,
                          -1, 0);

    if (exec_mem == MAP_FAILED) {
        perror("mmap");
        free(code);
        return 1;
    }

    /* Copy code to executable memory */
    memcpy(exec_mem, code, size);
    free(code);

    /* Make memory executable */
    if (mprotect(exec_mem, size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(exec_mem, size);
        return 1;
    }

    /* Store for cleanup */
    g_exec_mem = exec_mem;
    g_exec_size = size;

    /* Setup runtime table */
    setup_runtime_table(exec_mem);

    /* Execute code */
    compiled_fn_t fn = (compiled_fn_t)exec_mem;
    int64_t result = fn(g_runtime_table);

    /* Print result */
    if (HABU_IS_FIXNUM(result)) {
        printf("%lld\n", HABU_UNTAG_FIXNUM(result));
    } else if (HABU_IS_FLOAT(result)) {
        printf("%g\n", float_value(result));
    } else if (HABU_IS_STRING(result)) {
        habu_string_t *s = (habu_string_t *)(result & ~0xF);
        printf("\"%.*s\"\n", (int)s->length, s->data);
    } else if (result == 0) {
        printf("NIL\n");
    } else {
        printf("#<object 0x%llx>\n", (unsigned long long)result);
    }

    /* Cleanup */
    munmap(exec_mem, size);
    return 0;
}

static void debug_print_value(int64_t value) {
    if (HABU_IS_FIXNUM(value)) {
        printf("%lld", HABU_UNTAG_FIXNUM(value));
    } else if (HABU_IS_FLOAT(value)) {
        printf("%g", float_value(value));
    } else if (HABU_IS_STRING(value)) {
        habu_string_t *s = (habu_string_t *)(value & ~0xF);
        printf("\"%.*s\"", (int)s->length, s->data);
    } else if (HABU_IS_SYMBOL(value)) {
        habu_symbol_t *sym = (habu_symbol_t *)(value & ~0xF);
        if (sym->name != 0) {
            habu_string_t *name = (habu_string_t *)(sym->name & ~0xF);
            printf("%.*s", (int)name->length, name->data);
        } else {
            printf("#<symbol>");
        }
    } else if (value == 0) {
        printf("NIL");
    } else if (HABU_IS_CONS(value)) {
        printf("(");
        int64_t current = value;
        int first = 1;
        while (HABU_IS_CONS(current)) {
            if (!first) printf(" ");
            first = 0;
            habu_cons_t *cons_ptr = (habu_cons_t *)(current & ~0xF);
            print_value(cons_ptr->car);
            current = cons_ptr->cdr;
        }
        if (current != 0) {
            printf(" . ");
            print_value(current);
        }
        printf(")");
    } else {
        printf("#<object 0x%llx>", (unsigned long long)value);
    }
}

static void repl(void) {
    /* Snake emoji prompt */
    const char *prompt = "\xF0\x9F\x90\x8D* ";
    
    printf("Habu Lisp REPL\n");
    printf("Type ,help for commands, ,quit to exit\n\n");

    /* Initialize runtime with NULL code base (REPL mode) */
    setup_runtime_table(NULL);

    while (1) {
        char *line = lineedit_readline(prompt);
        if (!line) {
            printf("\nBye!\n");
            break;
        }

        /* Skip empty lines */
        if (line[0] == '\0') {
            continue;
        }

        /* Handle REPL commands */
        if (line[0] == ',') {
            if (strcmp(line, ",quit") == 0 || strcmp(line, ",q") == 0) {
                printf("Bye!\n");
                break;
            }
            if (strcmp(line, ",help") == 0 || strcmp(line, ",h") == 0) {
                printf("REPL Commands:\n");
                printf("  ,quit, ,q        Exit the REPL\n");
                printf("  ,help, ,h        Show this help\n");
                printf("  ,load <file>     Load and run a Lisp file\n");
                printf("  ,deliver <in> <out>  Create standalone executable\n");
                printf("  ,compile <file>  Compile to .bin\n");
                continue;
            }
            if (strncmp(line, ",load ", 6) == 0) {
                char cmd[1024];
                snprintf(cmd, sizeof(cmd), "./run-bytecode \"%s\" 2>&1", line + 6);
                /* First compile */
                char compile_cmd[1024];
                snprintf(compile_cmd, sizeof(compile_cmd),
                    "sbcl --noinform --non-interactive "
                    "--load sbcl-habu-shim.lisp "
                    "--load habu-arm64-codegen-sbcl.lisp "
                    "--load run-habu.lisp "
                    "--eval '(habu-sbcl:compile-and-run-forms (habu-sbcl:read-forms-from-file \"%s\"))' "
                    "--eval '(sb-ext:quit)' 2>/dev/null", line + 6);
                system(compile_cmd);
                continue;
            }
            if (strncmp(line, ",deliver ", 9) == 0) {
                char *args = line + 9;
                char source[256], output[256];
                if (sscanf(args, "%255s %255s", source, output) == 2) {
                    char cmd[1024];
                    snprintf(cmd, sizeof(cmd), "./habu-deliver \"%s\" -o \"%s\"", source, output);
                    system(cmd);
                } else {
                    printf("Usage: ,deliver <source.lisp> <output>\n");
                }
                continue;
            }
            if (strncmp(line, ",compile ", 9) == 0) {
                char *file = line + 9;
                char cmd[1024];
                snprintf(cmd, sizeof(cmd), "./habu-compile \"%s\"", file);
                system(cmd);
                continue;
            }
            printf("Unknown command: %s\n", line);
            printf("Type ,help for available commands.\n");
            continue;
        }

        /* For now, compile and run the expression */
        char tmpfile[] = "/tmp/habu-repl-XXXXXX.lisp";
        int fd = mkstemps(tmpfile, 5);
        if (fd >= 0) {
            write(fd, line, strlen(line));
            close(fd);
            char cmd[1024];
            snprintf(cmd, sizeof(cmd),
                "sbcl --noinform --non-interactive "
                "--load sbcl-habu-shim.lisp "
                "--load habu-arm64-codegen-sbcl.lisp "
                "--load run-habu.lisp "
                "--eval '(format t \"~A~%%\" (habu-sbcl:compile-and-run-forms (habu-sbcl:read-forms-from-file \"%s\")))' "
                "--eval '(sb-ext:quit)' 2>/dev/null", tmpfile);
            system(cmd);
            unlink(tmpfile);
        }
    }
}

int main(int argc, char **argv) {
    /* Initialize Habu runtime (GC, etc.) */
    init(4 * 1024 * 1024);  /* 4MB heap */
    io_init();

    /* Parse arguments */
    if (argc == 1) {
        /* No arguments - start REPL */
        repl();
        return 0;
    }

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
            print_help(argv[0]);
            return 0;
        }
        if (strcmp(argv[i], "--version") == 0 || strcmp(argv[i], "-v") == 0) {
            print_version();
            return 0;
        }
        /* Assume it's a bytecode file */
        return run_bytecode_file(argv[i]);
    }

    return 0;
}
