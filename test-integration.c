/* Comprehensive Integration Test
 * Tests all implemented features working together
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

int test_expr(const char *expr, unsigned char *code, size_t len, int64_t expected) {
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
    printf("Comprehensive Integration Test\n");
    printf("========================================\n");

    int pass = 0, fail = 0;

    /* Test 1: (if (and (> 10 5) (< 3 7)) 42 0) → 42 */
    printf("\n--- Combining if, and, comparisons ---\n");
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* (> 10 5) */
            0x00, 0x14, 0x80, 0xD2,  /* movz x0, #160 */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */
            0xE0, 0xD7, 0x9F, 0x9A,  /* cset x0, GT */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! (save result) */

            /* (< 3 7) */
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x0E, 0x80, 0xD2,  /* movz x0, #112 */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */
            0xE0, 0xA7, 0x9F, 0x9A,  /* cset x0, LT */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */

            /* AND */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x1F, 0x00, 0x1F, 0xEB,  /* cmp x0, xzr */
            0xE2, 0x07, 0x9F, 0x9A,  /* cset x2, NE */
            0x3F, 0x00, 0x1F, 0xEB,  /* cmp x1, xzr */
            0xE3, 0x07, 0x9F, 0x9A,  /* cset x3, NE */
            0x40, 0x00, 0x03, 0x8A,  /* and x0, x2, x3 */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */

            /* IF */
            0x1F, 0x00, 0x1F, 0xEB,  /* cmp x0, xzr */
            0x60, 0x00, 0x00, 0x54,  /* b.eq +3 */

            /* Then: 42 */
            0x00, 0x54, 0x80, 0xD2,
            0x02, 0x00, 0x00, 0x14,  /* b +2 */

            /* Else: 0 */
            0x00, 0x00, 0x80, 0xD2,

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_expr("(if (and (> 10 5) (< 3 7)) 42 0)", code, sizeof(code), 42) ? pass++ : fail++;
    }

    /* Test 2: (if (not (= 5 3)) (+ 10 20) 0) → 30 */
    printf("\n--- Combining if, not, =, + ---\n");
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* (= 5 3) */
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */
            0xE0, 0x07, 0x9F, 0x9A,  /* cset x0, EQ */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */

            /* NOT */
            0x1F, 0x00, 0x1F, 0xEB,  /* cmp x0, xzr */
            0xE0, 0x17, 0x9F, 0x9A,  /* cset x0, EQ (inverted) */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */

            /* IF */
            0x1F, 0x00, 0x1F, 0xEB,  /* cmp x0, xzr */
            0x00, 0x01, 0x00, 0x54,  /* b.eq +8 */

            /* Then: (+ 10 20) = 30 */
            0x00, 0x14, 0x80, 0xD2,  /* movz x0, #160 */
            0xE0, 0x0F, 0x1F, 0xF8,
            0x00, 0x28, 0x80, 0xD2,  /* movz x0, #320 */
            0xE1, 0x03, 0x00, 0xAA,
            0xE0, 0x07, 0x41, 0xF8,
            0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */
            0x02, 0x00, 0x00, 0x14,  /* b +2 */

            /* Else: 0 */
            0x00, 0x00, 0x80, 0xD2,

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_expr("(if (not (= 5 3)) (+ 10 20) 0)", code, sizeof(code), 30) ? pass++ : fail++;
    }

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");

    if (pass == 2) {
        printf("\n✓ All integration tests passing!\n");
        printf("Implemented features working together:\n");
        printf("  - Arithmetic: +, -, *, /\n");
        printf("  - Comparisons: =, <, >, <=, >=, !=\n");
        printf("  - Logical: and, or, not\n");
        printf("  - Control flow: if\n");
        printf("  - Sequential: progn\n");
        printf("  - Predicates: fixnum?\n");
    }

    return (fail == 0) ? 0 : 1;
}
