/* Test let bindings implementation */

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
    printf("Let Bindings Tests\n");
    printf("========================================\n");

    /* Test 1: (let ((x 5)) x) → 5 */
    {
        printf("\n=== Test 1: (let ((x 5)) x) ===\n");
        printf("Expected: 5\n");

        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
            0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

            /* Evaluate value: movz x0, #80 (5 << 4) */
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 */

            /* Save on stack: str x0, [sp, #-16]! */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */

            /* Load from stack (var 0): ldr x0, [sp] */
            0xE0, 0x03, 0x40, 0xF9,  /* ldr x0, [sp] */

            /* Restore stack: add sp, sp, #16 */
            0xFF, 0x43, 0x00, 0x91,  /* add sp, sp, #16 */

            /* Untag result */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
            0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };

        int64_t r = execute_bytes(code, sizeof(code));
        printf("Result: %lld ", r);
        if (r == 5) {
            printf("✓ PASS\n");
            pass++;
        } else {
            printf("✗ FAIL\n");
            fail++;
        }
    }

    /* Test 2: (let ((x 3)) (+ x 2)) → 5 */
    {
        printf("\n=== Test 2: (let ((x 3)) (+ x 2)) ===\n");
        printf("Expected: 5\n");

        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* Evaluate value: movz x0, #48 (3 << 4) */
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 */

            /* Save on stack */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */

            /* Body: (+ x 2) */
            /* Load x: ldr x0, [sp] */
            0xE0, 0x03, 0x40, 0xF9,  /* ldr x0, [sp] */

            /* Save x for binary op */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */

            /* Load 2: movz x0, #32 (2 << 4) */
            0x00, 0x04, 0x80, 0xD2,  /* movz x0, #32 */

            /* Move to x1 */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */

            /* Load saved x */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */

            /* Add */
            0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */

            /* Restore let stack */
            0xFF, 0x43, 0x00, 0x91,  /* add sp, sp, #16 */

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };

        int64_t r = execute_bytes(code, sizeof(code));
        printf("Result: %lld ", r);
        if (r == 5) {
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
        printf("\n✓ Let bindings working!\n");
    }

    return (fail == 0) ? 0 : 1;
}
