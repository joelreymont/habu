/* Test cond expression - simplest case */

#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int64_t execute_bytes(unsigned char *code, size_t len) {
    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return -1;
    }

    memcpy(mem, code, len);

    /* Print bytes for debugging */
    printf("Code bytes (%zu):\n", len);
    for (size_t i = 0; i < len; i++) {
        printf("%02X ", code[i]);
        if ((i + 1) % 16 == 0) printf("\n");
    }
    printf("\n");

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return -1;
    }

    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();
    munmap(mem, page_size);
    return result;
}

int main() {
    printf("========================================\n");
    printf("Simple Cond Test\n");
    printf("========================================\n");

    /* Test: (cond (1 42)) - literal 1 as test, always true */
    {
        printf("\n=== Test: (cond (1 42)) ===\n");
        printf("Expected: 42\n");

        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
            0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

            /* Test: literal 1 (tagged as 16) */
            0x00, 0x02, 0x80, 0xD2,  /* movz x0, #16 (1 << 4) */

            /* cmp x0, xzr - check if test passed */
            0x1F, 0x00, 0x1F, 0xEB,  /* cmp x0, xzr */

            /* beq default - jump to default if test failed (x0 == 0) */
            /* offset = 2 instructions (movz #672 + b) + beq itself = 3 total  */
            0x60, 0x00, 0x00, 0x54,  /* beq +12 (3 instructions) */

            /* Result: movz x0, #672 (42 << 4) */
            0x00, 0x54, 0x80, 0xD2,  /* movz x0, #672 */

            /* b end - skip default */
            /* offset = 2 instructions (movz #0 + b itself to end) */
            0x02, 0x00, 0x00, 0x14,  /* b +2 (2 instructions) */

            /* Default: return 0 */
            0x00, 0x00, 0x80, 0xD2,  /* movz x0, #0 */

            /* Untag result */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
            0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };

        int64_t result = execute_bytes(code, sizeof(code));
        printf("Result: %lld ", result);
        if (result == 42) {
            printf("✓ PASS\n");
            return 0;
        } else {
            printf("✗ FAIL\n");
            return 1;
        }
    }
}
