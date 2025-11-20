/* Test PROGN - sequential execution */

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

int test_progn(const char *expr, unsigned char *code, size_t len, int64_t expected) {
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
    printf("PROGN Tests\n");
    printf("========================================\n");

    int pass = 0, fail = 0;

    /* Test 1: (progn 10 20 30) → 30 (returns last) */
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* Expr 1: 10 */
            0x00, 0x14, 0x80, 0xD2,  /* movz x0, #160 (10 << 4) */

            /* Expr 2: 20 */
            0x00, 0x28, 0x80, 0xD2,  /* movz x0, #320 (20 << 4) */

            /* Expr 3: 30 */
            0x00, 0x3C, 0x80, 0xD2,  /* movz x0, #480 (30 << 4) */

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_progn("(progn 10 20 30)", code, sizeof(code), 30) ? pass++ : fail++;
    }

    /* Test 2: (progn (+ 1 2) (* 3 4)) → 12 (returns last calc) */
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* Expr 1: (+ 1 2) = 3 */
            0x00, 0x02, 0x80, 0xD2,  /* movz x0, #16 (1 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x04, 0x80, 0xD2,  /* movz x0, #32 (2 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */

            /* Expr 2: (* 3 4) = 12 */
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 (3 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x08, 0x80, 0xD2,  /* movz x0, #64 (4 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */
            0x00, 0x7C, 0x01, 0x9B,  /* mul x0, x0, x1 */

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_progn("(progn (+ 1 2) (* 3 4))", code, sizeof(code), 12) ? pass++ : fail++;
    }

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");

    if (pass == 2) {
        printf("\n✓ PROGN working!\n");
    }

    return (fail == 0) ? 0 : 1;
}
