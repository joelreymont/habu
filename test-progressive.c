/* Progressive Compilation Tests
 * Tests increasingly complex expressions to verify the full pipeline
 */

#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int test_code(const char *name, unsigned char *code, size_t len, int64_t expected) {
    printf("\n=== %s ===\n", name);
    printf("Expected: %lld\n", expected);
    printf("Code (%zu bytes): ", len);
    for (size_t i = 0; i < len; i++) {
        printf("%02X ", code[i]);
        if ((i + 1) % 16 == 0 && i + 1 < len) printf("\n  ");
    }
    printf("\n");

    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return 0;
    }

    memcpy(mem, code, len);

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return 0;
    }

    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();
    munmap(mem, page_size);

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
    printf("Progressive Compilation Test Suite\n");
    printf("========================================\n");

    int pass = 0, fail = 0;

    /* Level 1: Literal - just return 42 */
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
        test_code("Level 1: Literal 42", code, sizeof(code), 42) ? pass++ : fail++;
    }

    /* Level 2: Simple addition (+ 3 4) */
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
        test_code("Level 2: (+ 3 4) = 7", code, sizeof(code), 7) ? pass++ : fail++;
    }

    /* Level 3: Nested expression (+ (* 3 4) 5) = 17 */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
            0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

            /* Inner: (* 3 4) */
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 (3 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x08, 0x80, 0xD2,  /* movz x0, #64 (4 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 (untag for mul) */
            0x00, 0x7C, 0x01, 0x9B,  /* mul x0, x0, x1 */

            /* Outer: (+ result 5) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 (5 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */

            /* Untag result */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
            0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };
        test_code("Level 3: (+ (* 3 4) 5) = 17", code, sizeof(code), 17) ? pass++ : fail++;
    }

    /* Level 4: Complex nested (- (* 10 3) (+ 2 8)) = 20 */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
            0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

            /* Left: (* 10 3) = 30 */
            0x00, 0x14, 0x80, 0xD2,  /* movz x0, #160 (10 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 (3 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */
            0x00, 0x7C, 0x01, 0x9B,  /* mul x0, x0, x1 */

            /* Save left result */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */

            /* Right: (+ 2 8) = 10 */
            0x00, 0x04, 0x80, 0xD2,  /* movz x0, #32 (2 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x10, 0x80, 0xD2,  /* movz x0, #128 (8 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */

            /* Outer: (- left right) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x00, 0x00, 0x01, 0xCB,  /* sub x0, x0, x1 */

            /* Untag result */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
            0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };
        test_code("Level 4: (- (* 10 3) (+ 2 8)) = 20", code, sizeof(code), 20) ? pass++ : fail++;
    }

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");

    return (fail == 0) ? 0 : 1;
}
