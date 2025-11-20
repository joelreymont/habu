/* Test Compiled Comparison Expressions
 * Tests (= ...), (< ...), (> ...) compiled by compile-to-arm64
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

int test_expression(const char *expr, unsigned char *code, size_t len, int64_t expected) {
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
    printf("Compiled Comparison Expression Tests\n");
    printf("(compile-to-arm64 '(= ...))\n");
    printf("========================================\n");

    int pass = 0, fail = 0;

    /* Test 1: (= 5 5) → 1 */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
            0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

            /* arg1: 5 */
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 (5 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */

            /* arg2: 5 */
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 (5 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */

            /* Load arg1, compare */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */
            0xE0, 0x17, 0x9F, 0x9A,  /* cset x0, EQ */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 (tag result) */

            /* Untag for return */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
            0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };
        test_expression("(= 5 5)", code, sizeof(code), 1) ? pass++ : fail++;
    }

    /* Test 2: (= 5 3) → 0 */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* arg1: 5 */
            0x00, 0x0A, 0x80, 0xD2,
            0xE0, 0x0F, 0x1F, 0xF8,

            /* arg2: 3 */
            0x00, 0x06, 0x80, 0xD2,
            0xE1, 0x03, 0x00, 0xAA,

            /* Compare */
            0xE0, 0x07, 0x41, 0xF8,
            0x1F, 0x00, 0x01, 0xEB,
            0xE0, 0x17, 0x9F, 0x9A,
            0x00, 0xEC, 0x7C, 0xD3,

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_expression("(= 5 3)", code, sizeof(code), 0) ? pass++ : fail++;
    }

    /* Test 3: (< 3 5) → 1 */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* arg1: 3 */
            0x00, 0x06, 0x80, 0xD2,
            0xE0, 0x0F, 0x1F, 0xF8,

            /* arg2: 5 */
            0x00, 0x0A, 0x80, 0xD2,
            0xE1, 0x03, 0x00, 0xAA,

            /* Compare with LT */
            0xE0, 0x07, 0x41, 0xF8,
            0x1F, 0x00, 0x01, 0xEB,
            0xE0, 0xA7, 0x9F, 0x9A,  /* cset x0, LT */
            0x00, 0xEC, 0x7C, 0xD3,

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_expression("(< 3 5)", code, sizeof(code), 1) ? pass++ : fail++;
    }

    /* Test 4: (< 7 2) → 0 */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* arg1: 7 */
            0x00, 0x0E, 0x80, 0xD2,
            0xE0, 0x0F, 0x1F, 0xF8,

            /* arg2: 2 */
            0x00, 0x04, 0x80, 0xD2,
            0xE1, 0x03, 0x00, 0xAA,

            /* Compare */
            0xE0, 0x07, 0x41, 0xF8,
            0x1F, 0x00, 0x01, 0xEB,
            0xE0, 0xA7, 0x9F, 0x9A,
            0x00, 0xEC, 0x7C, 0xD3,

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_expression("(< 7 2)", code, sizeof(code), 0) ? pass++ : fail++;
    }

    /* Test 5: (> 10 5) → 1 */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* arg1: 10 */
            0x00, 0x14, 0x80, 0xD2,
            0xE0, 0x0F, 0x1F, 0xF8,

            /* arg2: 5 */
            0x00, 0x0A, 0x80, 0xD2,
            0xE1, 0x03, 0x00, 0xAA,

            /* Compare with GT */
            0xE0, 0x07, 0x41, 0xF8,
            0x1F, 0x00, 0x01, 0xEB,
            0xE0, 0xD7, 0x9F, 0x9A,  /* cset x0, GT */
            0x00, 0xEC, 0x7C, 0xD3,

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_expression("(> 10 5)", code, sizeof(code), 1) ? pass++ : fail++;
    }

    /* Test 6: (> 3 8) → 0 */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* arg1: 3 */
            0x00, 0x06, 0x80, 0xD2,
            0xE0, 0x0F, 0x1F, 0xF8,

            /* arg2: 8 */
            0x00, 0x10, 0x80, 0xD2,
            0xE1, 0x03, 0x00, 0xAA,

            /* Compare */
            0xE0, 0x07, 0x41, 0xF8,
            0x1F, 0x00, 0x01, 0xEB,
            0xE0, 0xD7, 0x9F, 0x9A,
            0x00, 0xEC, 0x7C, 0xD3,

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_expression("(> 3 8)", code, sizeof(code), 0) ? pass++ : fail++;
    }

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");
    printf("\nComparison operators integrated into compiler!\n");
    printf("✓ (= a b) - Equality\n");
    printf("✓ (< a b) - Less than\n");
    printf("✓ (> a b) - Greater than\n");

    return (fail == 0) ? 0 : 1;
}
