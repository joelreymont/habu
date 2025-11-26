/* run-fasl.c - Load and execute FASL files or raw bytecode
 *
 * Usage: ./run-fasl <file.fasl>
 *        ./run-fasl <file.bin>   (legacy raw bytecode)
 *
 * FASL format:
 *   Magic:    4 bytes "HFSL" (0x4C534648 LE)
 *   Version:  4 bytes
 *   Flags:    4 bytes (reserved)
 *   Code-len: 4 bytes
 *   Code:     N bytes (ARM64 machine code)
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>
#include "runtime/habu.h"

/* FASL file format constants */
#define FASL_MAGIC 0x4C534648  /* "HFSL" in little-endian */
#define FASL_VERSION 1

/* Habu tagged value representation */
#define HABU_TAG_FIXNUM(n) ((int64_t)(n) << 4)
#define HABU_UNTAG_FIXNUM(v) ((int64_t)(v) >> 4)
#define HABU_IS_CONS(v) (((v) & 0xF) == 1)

/* Function pointer type for compiled code */
typedef int64_t (*compiled_fn_t)(void** runtime_table);

/* Runtime function table */
void* g_runtime_table[64];

static uint32_t read_u32_le(FILE *f) {
    uint8_t buf[4];
    if (fread(buf, 1, 4, f) != 4) {
        return 0;
    }
    return buf[0] | (buf[1] << 8) | (buf[2] << 16) | (buf[3] << 24);
}

/* Returns allocated code buffer and sets *size. Returns NULL on error. */
static uint8_t* read_file_code(const char *path, long *size) {
    FILE *f = fopen(path, "rb");
    if (!f) {
        perror("fopen");
        return NULL;
    }

    /* Check for FASL magic */
    uint32_t magic = read_u32_le(f);

    uint8_t *code = NULL;

    if (magic == FASL_MAGIC) {
        /* FASL file */
        uint32_t version = read_u32_le(f);
        if (version > FASL_VERSION) {
            fprintf(stderr, "Unsupported FASL version: %u\n", version);
            fclose(f);
            return NULL;
        }

        read_u32_le(f);  /* flags - reserved */
        uint32_t code_len = read_u32_le(f);

        if (code_len == 0 || code_len > 16*1024*1024) {
            fprintf(stderr, "Invalid code length: %u\n", code_len);
            fclose(f);
            return NULL;
        }

        code = malloc(code_len);
        if (!code) {
            perror("malloc");
            fclose(f);
            return NULL;
        }

        if (fread(code, 1, code_len, f) != code_len) {
            fprintf(stderr, "Failed to read code section\n");
            free(code);
            fclose(f);
            return NULL;
        }

        *size = code_len;
        printf("FASL: %u bytes of ARM64 code (version %u)\n", code_len, version);
    } else {
        /* Raw bytecode - rewind and read entire file */
        fseek(f, 0, SEEK_END);
        long file_size = ftell(f);
        fseek(f, 0, SEEK_SET);

        if (file_size <= 0 || file_size > 16*1024*1024) {
            fprintf(stderr, "Invalid file size: %ld\n", file_size);
            fclose(f);
            return NULL;
        }

        code = malloc(file_size);
        if (!code) {
            perror("malloc");
            fclose(f);
            return NULL;
        }

        if (fread(code, 1, file_size, f) != (size_t)file_size) {
            fprintf(stderr, "Failed to read bytecode\n");
            free(code);
            fclose(f);
            return NULL;
        }

        *size = file_size;
        printf("Raw: %ld bytes of ARM64 code\n", file_size);
    }

    fclose(f);
    return code;
}

static void setup_runtime_table(void *exec_mem) {
    g_runtime_table[0] = (void*)cons;
    g_runtime_table[1] = (void*)car;
    g_runtime_table[2] = (void*)cdr;
    g_runtime_table[3] = (void*)make_closure;
    g_runtime_table[4] = (void*)closure_code;
    g_runtime_table[5] = (void*)closure_env;
    g_runtime_table[6] = exec_mem;              /* Code base for closures */
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
    /* Float operations */
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
    /* File I/O */
    g_runtime_table[42] = (void*)open_file;
    g_runtime_table[43] = (void*)close_file;
    g_runtime_table[44] = (void*)read_line;
    g_runtime_table[45] = (void*)write_string;
    g_runtime_table[46] = (void*)read_file;
    g_runtime_table[47] = (void*)write_file;
    /* Print */
    g_runtime_table[48] = (void*)print_value;
    g_runtime_table[49] = (void*)println_value;
    /* Profiling */
    g_runtime_table[50] = (void*)get_time_ns;
    /* System */
    g_runtime_table[51] = (void*)system_cmd;
    g_runtime_table[52] = (void*)string_equal;
    g_runtime_table[53] = (void*)write_bytes;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <file.fasl|file.bin>\n", argv[0]);
        fprintf(stderr, "  Loads and executes compiled Habu code\n");
        return 1;
    }

    /* Read code from file */
    long code_size;
    uint8_t *code = read_file_code(argv[1], &code_size);
    if (!code) {
        return 1;
    }

    /* Allocate executable memory */
    void *exec_mem = mmap(NULL, code_size,
                          PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS,
                          -1, 0);

    if (exec_mem == MAP_FAILED) {
        perror("mmap");
        free(code);
        return 1;
    }

    /* Copy code to executable memory */
    memcpy(exec_mem, code, code_size);
    free(code);

    /* Make memory executable */
    if (mprotect(exec_mem, code_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(exec_mem, code_size);
        return 1;
    }

    /* Initialize runtime */
    init(1024 * 1024);
    setup_runtime_table(exec_mem);

    if (getenv("HABU_DEBUG_TABLE")) {
        for (int i = 0; i <= 53; i++) {
            fprintf(stderr, "RT[%d]=%p\n", i, g_runtime_table[i]);
        }
    }

    /* Execute */
    printf("Executing...\n");
    compiled_fn_t fn = (compiled_fn_t)exec_mem;
    int64_t result = fn(g_runtime_table);

    /* Print result */
    printf("Result: 0x%llx (%lld)\n", result, result);
    if (HABU_IS_CONS(result)) {
        printf("  => cons cell\n");
    } else if ((result & 0xF) == 0) {
        printf("  => fixnum %lld\n", HABU_UNTAG_FIXNUM(result));
    } else if ((result & 0xF) == 0x4) {
        printf("  => string\n");
    } else if ((result & 0xF) == 0x2) {
        printf("  => symbol\n");
    } else if ((result & 0xF) == 0x7) {
        printf("  => float %g\n", float_value(result));
    }

    munmap(exec_mem, code_size);
    return 0;
}
