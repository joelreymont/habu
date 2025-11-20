/* Habu JIT Executor - C library for executing Habu-generated machine code
 *
 * This provides JIT execution services to the Habu compiler.
 * The Habu compiler generates ARM64 bytes, then calls these functions
 * to allocate JIT memory and execute the code.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

/* Global JIT memory pool (simple allocator for now) */
static void *jit_memory = NULL;
static size_t jit_memory_size = 0;
static size_t jit_memory_used = 0;

/* Initialize JIT memory pool */
int habu_jit_init(size_t pool_size) {
    if (jit_memory != NULL) {
        fprintf(stderr, "JIT already initialized\n");
        return -1;
    }

    /* Allocate a large pool of JIT memory */
    jit_memory = mmap(NULL, pool_size,
                      PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANON | MAP_JIT,
                      -1, 0);

    if (jit_memory == MAP_FAILED) {
        perror("mmap JIT pool");
        jit_memory = NULL;
        return -1;
    }

    jit_memory_size = pool_size;
    jit_memory_used = 0;

    printf("JIT pool initialized: %zu bytes at %p\n", pool_size, jit_memory);
    return 0;
}

/* Allocate space in JIT pool for code */
void *habu_jit_alloc(size_t code_size) {
    if (jit_memory == NULL) {
        fprintf(stderr, "JIT not initialized\n");
        return NULL;
    }

    /* Align to 16-byte boundary */
    size_t aligned_size = (code_size + 15) & ~15;

    if (jit_memory_used + aligned_size > jit_memory_size) {
        fprintf(stderr, "JIT pool exhausted\n");
        return NULL;
    }

    void *ptr = (char *)jit_memory + jit_memory_used;
    jit_memory_used += aligned_size;

    return ptr;
}

/* Execute Habu-generated machine code
 *
 * Takes a byte array and its length, allocates separate JIT memory,
 * makes it executable, and runs it.
 *
 * Returns the int64 result from the function.
 */
int64_t habu_jit_execute(unsigned char *code, size_t code_len) {
    if (code == NULL || code_len == 0) {
        fprintf(stderr, "Invalid code\n");
        return -1;
    }

    /* Allocate separate memory for each execution to avoid mprotect issues */
    size_t page_size = 4096;
    size_t aligned_size = ((code_len + page_size - 1) / page_size) * page_size;

    void *mem = mmap(NULL, aligned_size,
                     PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT,
                     -1, 0);

    if (mem == MAP_FAILED) {
        perror("mmap");
        return -1;
    }

    /* Copy code */
    memcpy(mem, code, code_len);

    /* Make executable */
    if (mprotect(mem, aligned_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, aligned_size);
        return -1;
    }

    /* Execute */
    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();

    /* Free memory */
    munmap(mem, aligned_size);

    return result;
}

/* Execute code from a Habu list of bytes
 *
 * This is called from Habu with a list like: (0 54 128 210 ...)
 * We need to convert it to a C array and execute.
 */
int64_t habu_jit_execute_list(void *list_ptr) {
    /* For now, assume list is already a C array for testing */
    /* In real integration, we'd walk the Habu cons cells */
    fprintf(stderr, "habu_jit_execute_list not yet implemented\n");
    return -1;
}

/* Reset JIT pool (for testing) */
void habu_jit_reset(void) {
    jit_memory_used = 0;
}

/* Shutdown JIT */
void habu_jit_shutdown(void) {
    if (jit_memory != NULL) {
        munmap(jit_memory, jit_memory_size);
        jit_memory = NULL;
        jit_memory_size = 0;
        jit_memory_used = 0;
    }
}

/* Test harness */
#ifdef HABU_JIT_TEST
int main() {
    printf("Habu JIT Executor Test\n");

    /* Initialize JIT pool (1MB) */
    if (habu_jit_init(1024 * 1024) != 0) {
        return 1;
    }

    /* Test 1: Return 42 */
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
            0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */
            0x00, 0x54, 0x80, 0xD2,  /* movz x0, #672 (42 << 4) */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */
            0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
            0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };

        int64_t result = habu_jit_execute(code, sizeof(code));
        printf("Test 1 (return 42): %lld %s\n", result,
               result == 42 ? "✓" : "✗");
    }

    /* Test 2: (+ 3 4) = 7 */
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
            0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 (3 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x08, 0x80, 0xD2,  /* movz x0, #64 (4 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */
            0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
            0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };

        habu_jit_reset();  /* Reset for fresh allocation */
        int64_t result = habu_jit_execute(code, sizeof(code));
        printf("Test 2 ((+ 3 4) = 7): %lld %s\n", result,
               result == 7 ? "✓" : "✗");
    }

    habu_jit_shutdown();
    printf("JIT tests complete\n");
    return 0;
}
#endif
