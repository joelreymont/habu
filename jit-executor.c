/* JIT Executor - Execute Habu-generated machine code directly
 * Uses MAP_JIT like SBCL does on modern macOS
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

int execute_bytes(unsigned char *code, size_t code_len, const char *name) {
    printf("\n=== Executing: %s ===\n", name);
    printf("Code (%zu bytes): ", code_len);
    for (size_t i = 0; i < code_len; i++) {
        printf("%02X ", code[i]);
    }
    printf("\n");

    /* Allocate JIT memory (like SBCL) */
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

    /* Execute! */
    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();

    printf("Result: %lld\n", result);

    munmap(mem, aligned_size);
    return result;
}

int main() {
    printf("========================================\n");
    printf("Habu JIT Executor - Testing ARM64 Code\n");
    printf("========================================\n");

    int pass = 0;
    int fail = 0;

    /* Test 1: Return 42 */
    {
        unsigned char code[] = {
            0x00, 0x54, 0x80, 0xD2,  /* movz x0, #672 (42 << 4) */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };
        int64_t result = execute_bytes(code, sizeof(code), "Return 42");
        if (result == 42) {
            printf("✓ PASS\n");
            pass++;
        } else {
            printf("✗ FAIL (expected 42)\n");
            fail++;
        }
    }

    /* Test 2: Return 0 */
    {
        unsigned char code[] = {
            0x00, 0x00, 0x80, 0xD2,  /* movz x0, #0 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };
        int64_t result = execute_bytes(code, sizeof(code), "Return 0");
        if (result == 0) {
            printf("✓ PASS\n");
            pass++;
        } else {
            printf("✗ FAIL (expected 0)\n");
            fail++;
        }
    }

    /* Test 3: Addition (need to test when we have full code gen) */

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");

    return (fail == 0) ? 0 : 1;
}
