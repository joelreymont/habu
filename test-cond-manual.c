/* Test cond expression - manual bytecode */

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
    int pass = 0, fail = 0;

    printf("========================================\n");
    printf("Cond Expression Tests\n");
    printf("========================================\n");

    /* Test 1: (cond ((= 3 3) 42)) → 42
     * Structure:
     * - prologue
     * - test: (= 3 3)
     * - cmp x0, xzr
     * - beq default
     * - result: 42
     * - b end
     * - default: movz x0, #0
     * - untag + epilogue
     */
    {
        printf("\n=== Test 1: (cond ((= 3 3) 42)) ===\n");
        printf("Expected: 42\n");

        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
            0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

            /* Test: (= 3 3) */
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 (3 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */
            0xE0, 0x17, 0x9F, 0x9A,  /* cset x0, EQ */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */

            /* cmp x0, xzr - check if test passed */
            0x1F, 0x00, 0x1F, 0xEB,  /* cmp x0, xzr */

            /* beq default - jump to default if test failed */
            /* offset = 2 instructions (movz #672 + b) */
            0x40, 0x00, 0x00, 0x54,  /* beq +8 (2 instructions) */

            /* Result: movz x0, #672 (42 << 4) */
            0x00, 0x54, 0x82, 0xD2,  /* movz x0, #672 */

            /* b end - skip default */
            /* offset = 1 instruction (movz #0) */
            0x01, 0x00, 0x00, 0x14,  /* b +1 (1 instruction) */

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
            pass++;
        } else {
            printf("✗ FAIL\n");
            fail++;
        }
    }

    /* Test 2: (cond ((< 5 3) 100)) → 0 (no match) */
    {
        printf("\n=== Test 2: (cond ((< 5 3) 100)) ===\n");
        printf("Expected: 0\n");

        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* Test: (< 5 3) - always false */
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 (5 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 (3 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */
            0xE0, 0xB7, 0x9F, 0x9A,  /* cset x0, LT */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */

            /* cmp x0, xzr */
            0x1F, 0x00, 0x1F, 0xEB,

            /* beq default */
            0x40, 0x00, 0x00, 0x54,  /* beq +8 */

            /* Result: movz x0, #1600 (100 << 4) */
            0x00, 0xC8, 0x80, 0xD2,  /* movz x0, #1600 */

            /* b end */
            0x01, 0x00, 0x00, 0x14,  /* b +1 */

            /* Default: return 0 */
            0x00, 0x00, 0x80, 0xD2,

            /* Untag result */
            0x00, 0xFC, 0x44, 0xD3,

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };

        int64_t result = execute_bytes(code, sizeof(code));
        printf("Result: %lld ", result);
        if (result == 0) {
            printf("✓ PASS\n");
            pass++;
        } else {
            printf("✗ FAIL\n");
            fail++;
        }
    }

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");

    if (pass == 2) {
        printf("\n✓ Cond expression working!\n");
    }

    return (fail == 0) ? 0 : 1;
}
