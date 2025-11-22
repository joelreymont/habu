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
void* g_runtime_table[32];

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

    /* Execute code - pass runtime table as argument */
    printf("Executing bytecode...\n");
    compiled_fn_t fn = (compiled_fn_t)exec_mem;
    int64_t result = fn(g_runtime_table);

    /* Print result */
    printf("Raw result: 0x%llx (%lld)\n", result, result);
    if (HABU_IS_CONS(result)) {
        printf("Result is a cons cell\n");
    } else {
        printf("Untagged fixnum: %lld\n", HABU_UNTAG_FIXNUM(result));
    }

    /* Cleanup */
    munmap(exec_mem, size);

    return 0;
}
