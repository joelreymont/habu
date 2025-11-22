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

/* Habu tagged value representation */
#define HABU_TAG_FIXNUM(n) ((int64_t)(n) << 4)
#define HABU_UNTAG_FIXNUM(v) ((int64_t)(v) >> 4)

/* Function pointer type for compiled code */
typedef int64_t (*compiled_fn_t)(void);

int main(int argc, char **argv) {
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

    /* Execute code */
    printf("Executing bytecode...\n");
    compiled_fn_t fn = (compiled_fn_t)exec_mem;
    int64_t result = fn();

    /* Print result */
    printf("Raw result: 0x%llx (%lld)\n", result, result);
    printf("Untagged fixnum: %lld\n", HABU_UNTAG_FIXNUM(result));

    /* Cleanup */
    munmap(exec_mem, size);

    return 0;
}
