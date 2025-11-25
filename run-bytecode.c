/* run-bytecode.c - Execute ARM64 bytecode via JIT
 *
 * Usage: ./run-bytecode <bytecode-file>
 *
 * Reads ARM64 machine code from file and executes it via JIT.
 * Returns the result as a tagged fixnum.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>
#include "runtime/habu.h"

/* Habu tagged value representation */
#define HABU_TAG_FIXNUM(n) ((int64_t)(n) << 4)
#define HABU_UNTAG_FIXNUM(v) ((int64_t)(v) >> 4)
#define HABU_IS_CONS(v) (((v) & 0xF) == 1)

/* Runtime functions are declared in habu.h with habu_value_t */

/* Function pointer type for compiled code - receives runtime function table */
typedef int64_t (*compiled_fn_t)(void** runtime_table);

/* Runtime function table */
void* g_runtime_table[48];

void print_runtime_addresses(void) {
    printf("Runtime function addresses in this process:\n");
    printf("  habu_cons: %p\n", (void*)habu_cons);
    printf("  habu_car:  %p\n", (void*)habu_car);
    printf("  habu_cdr:  %p\n", (void*)habu_cdr);
    printf("\n");
}

int main(int argc, char **argv) {
    if (argc > 1 && strcmp(argv[1], "--print-addrs") == 0) {
        habu_init(1024 * 1024);
        printf("HABU_CONS_ADDR=0x%llx\n", (unsigned long long)habu_cons);
        printf("HABU_CAR_ADDR=0x%llx\n", (unsigned long long)habu_car);
        printf("HABU_CDR_ADDR=0x%llx\n", (unsigned long long)habu_cdr);
        printf("HABU_MAKE_VECTOR_ADDR=0x%llx\n", (unsigned long long)habu_make_vector);
        printf("HABU_VECTOR_SET_ADDR=0x%llx\n", (unsigned long long)habu_vector_set);
        printf("HABU_VECTOR_REF_ADDR=0x%llx\n", (unsigned long long)habu_vector_ref);
        printf("HABU_MAKE_STRING_FROM_VECTOR_ADDR=0x%llx\n", (unsigned long long)habu_make_string_from_vector);
        printf("HABU_MAKE_SYMBOL_FROM_STRING_ADDR=0x%llx\n", (unsigned long long)habu_make_symbol_from_string);
        printf("HABU_STRING_LENGTH_RAW_ADDR=0x%llx\n", (unsigned long long)habu_string_length_raw);
        printf("HABU_SYMBOL_NAME_ADDR=0x%llx\n", (unsigned long long)habu_symbol_name);
        printf("HABU_MAKE_CLOSURE_ADDR=0x%llx\n", (unsigned long long)habu_make_closure);
        printf("HABU_CLOSURE_CODE_ADDR=0x%llx\n", (unsigned long long)habu_closure_code);
        printf("HABU_CLOSURE_ENV_ADDR=0x%llx\n", (unsigned long long)habu_closure_env);
        printf("HABU_CODE_BASE=0x%llx\n", (unsigned long long)0ULL); /* placeholder */
        return 0;
    }
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <bytecode-file>\n", argv[0]);
        return 1;
    }

    /* Read bytecode from file */
    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("fopen");
        return 1;
    }

    /* Get file size */
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (size <= 0 || size > 1024*1024) {
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

    printf("Read %ld bytes of ARM64 bytecode\n", size);

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

    /* Initialize Habu runtime (GC, etc.) */
    habu_init(1024 * 1024);  /* 1MB heap */

    /* Setup runtime function table */
    g_runtime_table[0] = (void*)habu_cons;
    g_runtime_table[1] = (void*)habu_car;
    g_runtime_table[2] = (void*)habu_cdr;
    g_runtime_table[3] = (void*)habu_make_closure;
    g_runtime_table[4] = (void*)habu_closure_code;
    g_runtime_table[5] = (void*)habu_closure_env;
    g_runtime_table[6] = exec_mem;              /* Code base pointer for closures */
    g_runtime_table[7] = (void*)habu_make_vector;
    g_runtime_table[8] = (void*)habu_vector_set;
    g_runtime_table[9] = (void*)habu_vector_ref;
    g_runtime_table[10] = (void*)habu_make_string_from_vector;
    g_runtime_table[11] = (void*)habu_make_symbol_from_string;
    g_runtime_table[12] = (void*)habu_string_length_raw;
    g_runtime_table[13] = (void*)habu_symbol_name;
    g_runtime_table[14] = (void*)habu_set_car;
    g_runtime_table[15] = (void*)habu_set_cdr;
    g_runtime_table[16] = (void*)habu_string_ref;
    g_runtime_table[17] = (void*)habu_values_set;
    g_runtime_table[18] = (void*)habu_values_get;
    g_runtime_table[19] = (void*)habu_make_hash_table;
    g_runtime_table[20] = (void*)habu_gethash;
    g_runtime_table[21] = (void*)habu_puthash;
    g_runtime_table[22] = (void*)habu_remhash;
    g_runtime_table[23] = (void*)habu_hash_table_count;
    g_runtime_table[24] = (void*)habu_string_concat;
    g_runtime_table[25] = (void*)habu_string_substring;
    g_runtime_table[26] = (void*)habu_fixnum_to_string;
    g_runtime_table[27] = (void*)habu_values_count_get;
    g_runtime_table[28] = (void*)habu_gensym;
    /* Float operations (IEEE 754) */
    g_runtime_table[29] = (void*)habu_make_float;
    g_runtime_table[30] = (void*)habu_float_add;
    g_runtime_table[31] = (void*)habu_float_sub;
    g_runtime_table[32] = (void*)habu_float_mul;
    g_runtime_table[33] = (void*)habu_float_div;
    g_runtime_table[34] = (void*)habu_float_lt;
    g_runtime_table[35] = (void*)habu_float_gt;
    g_runtime_table[36] = (void*)habu_float_le;
    g_runtime_table[37] = (void*)habu_float_ge;
    g_runtime_table[38] = (void*)habu_float_eq;
    g_runtime_table[39] = (void*)habu_fixnum_to_float;
    g_runtime_table[40] = (void*)habu_float_to_fixnum;
    g_runtime_table[41] = (void*)habu_float_value;
    /* File I/O operations */
    g_runtime_table[42] = (void*)habu_open_file;
    g_runtime_table[43] = (void*)habu_close_file;
    g_runtime_table[44] = (void*)habu_read_line;
    g_runtime_table[45] = (void*)habu_write_string;
    g_runtime_table[46] = (void*)habu_read_file;
    g_runtime_table[47] = (void*)habu_write_file;

    if (getenv("HABU_DEBUG_TABLE")) {
        for (int i = 0; i <= 47; i++) {
            fprintf(stderr, "RT[%d]=%p\n", i, g_runtime_table[i]);
        }
    }

    /* Execute code - pass runtime table as argument */
    printf("Executing bytecode...\n");
    compiled_fn_t fn = (compiled_fn_t)exec_mem;
    int64_t result = fn(g_runtime_table);

    /* Print result */
    printf("Raw result: 0x%llx (%lld)\n", result, result);
    if (HABU_IS_CONS(result)) {
        printf("Result is a cons cell\n");
    } else if ((result & 0xF) == 0) {
        printf("Untagged fixnum: %lld\n", HABU_UNTAG_FIXNUM(result));
    } else if ((result & 0xF) == 0x4) {
        printf("Result tag STRING (#x4)\n");
    } else if ((result & 0xF) == 0x2) {
        printf("Result tag SYMBOL (#x2)\n");
    } else if ((result & 0xF) == 0x7) {
        printf("Result tag FLOAT (#x7), value: %g\n", habu_float_value(result));
    }

    /* Cleanup */
    munmap(exec_mem, size);

    return 0;
}
