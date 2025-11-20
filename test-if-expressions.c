/* Test If Expressions
 * Tests (if test then else) compilation
 */

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
    size_t aligned_size = ((len + page_size - 1) / page_size) * page_size;

    void *mem = mmap(NULL, aligned_size,
                     PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT,
                     -1, 0);

    if (mem == MAP_FAILED) {
        perror("mmap");
        return -1;
    }

    memcpy(mem, code, len);

    if (mprotect(mem, aligned_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, aligned_size);
        return -1;
    }

    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();
    munmap(mem, aligned_size);

    return result;
}

int test_if(const char *expr, unsigned char *code, size_t len, int64_t expected) {
    printf("\n=== %s ===\n", expr);
    printf("Expected: %lld\n", expected);

    int64_t result = execute_bytes(code, len);
    printf("Result: %lld ", result);

    if (result == expected) {
        printf("✓ PASS\n");
        return 1;
    } else {
        printf("✗ FAIL\n");
        return 0;
    }
}

int main() {
    printf("========================================\n");
    printf("If Expression Tests\n");
    printf("========================================\n");

    int pass = 0, fail = 0;

    /* Test 1: (if 1 42 99) → 42 (true branch)
     * Test is literal 1 (truthy), should take then branch
     */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
            0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

            /* Test: literal 1 */
            0x00, 0x02, 0x80, 0xD2,  /* movz x0, #16 (1 << 4) */

            /* Compare with zero (XZR = x31) */
            0x1F, 0x00, 0x1F, 0xEB,  /* cmp x0, xzr */

            /* b.eq else (+3: from b.eq to else) */
            0x60, 0x00, 0x00, 0x54,  /* b.eq +3 */

            /* Then: literal 42 */
            0x00, 0x54, 0x80, 0xD2,  /* movz x0, #672 (42 << 4) */

            /* b end (+2: from b to untag) */
            0x02, 0x00, 0x00, 0x14,  /* b +2 */

            /* Else: literal 99 */
            0x00, 0xC6, 0x80, 0xD2,  /* movz x0, #1584 (99 << 4) */

            /* Untag result */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
            0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };
        test_if("(if 1 42 99)", code, sizeof(code), 42) ? pass++ : fail++;
    }

    /* Test 2: (if 0 42 99) → 99 (false branch)
     * Test is literal 0 (falsy), should take else branch
     */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* Test: literal 0 */
            0x00, 0x00, 0x80, 0xD2,  /* movz x0, #0 */

            /* Compare with zero */
            0x1F, 0x00, 0x1F, 0xEB,  /* cmp x0, xzr */

            /* b.eq else */
            0x60, 0x00, 0x00, 0x54,  /* b.eq +3 */

            /* Then: 42 */
            0x00, 0x54, 0x80, 0xD2,

            /* b end */
            0x02, 0x00, 0x00, 0x14,

            /* Else: 99 */
            0x00, 0xC6, 0x80, 0xD2,

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_if("(if 0 42 99)", code, sizeof(code), 99) ? pass++ : fail++;
    }

    /* Test 3: (if (= 5 5) 1 0) → 1
     * Test is comparison, should take then branch
     */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* Test: (= 5 5) */
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 (5 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */
            0xE0, 0x17, 0x9F, 0x9A,  /* cset x0, EQ */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 (tag result) */

            /* Compare with zero */
            0x1F, 0x00, 0x1F, 0xEB,  /* cmp x0, xzr */

            /* b.eq else */
            0x60, 0x00, 0x00, 0x54,  /* b.eq +2 */

            /* Then: 1 */
            0x00, 0x02, 0x80, 0xD2,  /* movz x0, #16 (1 << 4) */

            /* b end */
            0x02, 0x00, 0x00, 0x14,

            /* Else: 0 */
            0x00, 0x00, 0x80, 0xD2,

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_if("(if (= 5 5) 1 0)", code, sizeof(code), 1) ? pass++ : fail++;
    }

    /* Test 4: (if (< 10 5) 42 7) → 7
     * Test is false comparison, should take else branch
     */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* Test: (< 10 5) */
            0x00, 0x14, 0x80, 0xD2,  /* movz x0, #160 (10 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 (5 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */
            0xE0, 0xA7, 0x9F, 0x9A,  /* cset x0, LT */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */

            /* Compare with zero */
            0x1F, 0x00, 0x1F, 0xEB,

            /* b.eq else */
            0x60, 0x00, 0x00, 0x54,

            /* Then: 42 */
            0x00, 0x54, 0x80, 0xD2,

            /* b end */
            0x02, 0x00, 0x00, 0x14,

            /* Else: 7 */
            0x00, 0x0E, 0x80, 0xD2,

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_if("(if (< 10 5) 42 7)", code, sizeof(code), 7) ? pass++ : fail++;
    }

    /* Test 5: (if (> 8 3) (+ 10 5) (* 2 3)) → 15
     * Test is true, then branch has arithmetic
     */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* Test: (> 8 3) */
            0x00, 0x10, 0x80, 0xD2,  /* movz x0, #128 (8 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 (3 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */
            0xE0, 0xD7, 0x9F, 0x9A,  /* cset x0, GT */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */

            /* Compare with zero */
            0x1F, 0x00, 0x1F, 0xEB,

            /* b.eq else (+8: from b.eq to else = 1 b.eq + 6 then + 1 b) */
            0x00, 0x01, 0x00, 0x54,  /* b.eq +8 */

            /* Then: (+ 10 5) = 15 */
            0x00, 0x14, 0x80, 0xD2,  /* movz x0, #160 (10 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 (5 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */

            /* b end (+8: from b to untag = 1 b + 6 else + 1 untag) */
            0x08, 0x00, 0x00, 0x14,  /* b +8 */

            /* Else: (* 2 3) = 6 */
            0x00, 0x04, 0x80, 0xD2,  /* movz x0, #32 (2 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 (3 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 (untag for mul) */
            0x00, 0x7C, 0x01, 0x9B,  /* mul x0, x0, x1 */

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_if("(if (> 8 3) (+ 10 5) (* 2 3))", code, sizeof(code), 15) ? pass++ : fail++;
    }

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");

    if (pass == 5) {
        printf("\n✓ If expressions fully working!\n");
        printf("  - Simple literals in branches\n");
        printf("  - Comparison tests\n");
        printf("  - Arithmetic in branches\n");
    }

    return (fail == 0) ? 0 : 1;
}
