/* Test Modulo Operator */

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

int test_mod(const char *expr, unsigned char *code, size_t len, int64_t expected) {
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
    printf("Modulo Operator Tests\n");
    printf("========================================\n");

    int pass = 0, fail = 0;

    /* Test 1: (mod 10 3) → 1 */
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            0x00, 0x14, 0x80, 0xD2,  /* movz x0, #160 (10 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 (3 << 4) */
            0xE1, 0x03, 0x00, 0xAA,
            0xE0, 0x07, 0x41, 0xF8,

            /* Modulo */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 (untag a) */
            0x21, 0xFC, 0x44, 0xD3,  /* lsr x1, x1, #4 (untag b) */
            0xE2, 0x03, 0x00, 0xAA,  /* mov x2, x0 (save a) */
            0x00, 0x0C, 0xC1, 0x9A,  /* udiv x0, x0, x1 */
            0x00, 0x88, 0x01, 0x9B,  /* msub x0, x0, x1, x2 */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 (retag) */

            0x00, 0xFC, 0x44, 0xD3,  /* untag final */

            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_mod("(mod 10 3)", code, sizeof(code), 1) ? pass++ : fail++;
    }

    /* Test 2: (mod 20 7) → 6 */
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            0x00, 0x28, 0x80, 0xD2,  /* movz x0, #320 (20 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,
            0x00, 0x0E, 0x80, 0xD2,  /* movz x0, #112 (7 << 4) */
            0xE1, 0x03, 0x00, 0xAA,
            0xE0, 0x07, 0x41, 0xF8,

            0x00, 0xFC, 0x44, 0xD3,
            0x21, 0xFC, 0x44, 0xD3,
            0xE2, 0x03, 0x00, 0xAA,
            0x00, 0x0C, 0xC1, 0x9A,
            0x00, 0x88, 0x01, 0x9B,
            0x00, 0xEC, 0x7C, 0xD3,

            0x00, 0xFC, 0x44, 0xD3,

            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_mod("(mod 20 7)", code, sizeof(code), 6) ? pass++ : fail++;
    }

    /* Test 3: (mod 15 5) → 0 (exact division) */
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            0x00, 0x1E, 0x80, 0xD2,  /* movz x0, #240 (15 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 (5 << 4) */
            0xE1, 0x03, 0x00, 0xAA,
            0xE0, 0x07, 0x41, 0xF8,

            0x00, 0xFC, 0x44, 0xD3,
            0x21, 0xFC, 0x44, 0xD3,
            0xE2, 0x03, 0x00, 0xAA,
            0x00, 0x0C, 0xC1, 0x9A,
            0x00, 0x88, 0x01, 0x9B,
            0x00, 0xEC, 0x7C, 0xD3,

            0x00, 0xFC, 0x44, 0xD3,

            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_mod("(mod 15 5)", code, sizeof(code), 0) ? pass++ : fail++;
    }

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");

    if (pass == 3) {
        printf("\n✓ Modulo operator working!\n");
    }

    return (fail == 0) ? 0 : 1;
}
