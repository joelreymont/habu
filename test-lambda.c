/* Test lambda expressions
 * Lambda syntax: ((lambda (param) body) arg)
 * This should work like let bindings
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*fn_t)(void);

int64_t execute_code(unsigned char *code, size_t size) {
    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return -1;
    }

    memcpy(mem, code, size);

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return -1;
    }

    fn_t fn = (fn_t)mem;
    int64_t result = fn();

    munmap(mem, page_size);
    return result;
}

void test_lambda_simple() {
    /* Test: ((lambda (x) x) 42) → 42 */
    printf("Test 1: ((lambda (x) x) 42) → 42\n");

    /* This compiles to:
     * - Evaluate 42, save to stack (parameter x)
     * - Load x from stack
     * - Restore stack
     * - Return result
     */
    unsigned char code[] = {
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

        /* Evaluate 42 (42 << 4 = 672 = 0x2A0) */
        0x00, 0x54, 0x80, 0xD2,  /* movz x0, #0x2A0 */
        0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */

        /* Load x from stack at offset 0 */
        0xE0, 0x03, 0x40, 0xF9,  /* ldr x0, [sp] */

        /* Restore stack (1 binding * 16 = 16) */
        0xFF, 0x43, 0x00, 0x91,  /* add sp, sp, #16 */

        /* Untag result */
        0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

        /* Epilogue */
        0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    int64_t result = execute_code(code, sizeof(code));
    printf("  Result: %lld (expected 42)\n", result);
    if (result == 42) {
        printf("  ✅ PASS\n\n");
    } else {
        printf("  ❌ FAIL\n\n");
        exit(1);
    }
}

void test_lambda_increment() {
    /* Test: ((lambda (x) (+ x 1)) 5) → 6 */
    printf("Test 2: ((lambda (x) (+ x 1)) 5) → 6\n");

    unsigned char code[] = {
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

        /* Evaluate 5 (5 << 4 = 80 = 0x50) */
        0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #0x50 */
        0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */

        /* Body: (+ x 1) */
        /* Load x from stack */
        0xE0, 0x03, 0x40, 0xF9,  /* ldr x0, [sp] */
        0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! (save for binop) */

        /* Load 1 (1 << 4 = 16 = 0x10) */
        0x00, 0x02, 0x80, 0xD2,  /* movz x0, #0x10 */
        0x01, 0x00, 0x00, 0xAA,  /* mov x1, x0 */
        0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */

        /* Add */
        0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */

        /* Restore stack (1 binding * 16 = 16) */
        0xFF, 0x43, 0x00, 0x91,  /* add sp, sp, #16 */

        /* Untag result */
        0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

        /* Epilogue */
        0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    int64_t result = execute_code(code, sizeof(code));
    printf("  Result: %lld (expected 6)\n", result);
    if (result == 6) {
        printf("  ✅ PASS\n\n");
    } else {
        printf("  ❌ FAIL\n\n");
        exit(1);
    }
}

void test_lambda_two_params() {
    /* Test: ((lambda (x y) (+ x y)) 3 4) → 7 */
    printf("Test 3: ((lambda (x y) (+ x y)) 3 4) → 7\n");

    unsigned char code[] = {
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

        /* Evaluate 3 (3 << 4 = 48 = 0x30) */
        0x00, 0x06, 0x80, 0xD2,  /* movz x0, #0x30 */
        0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */

        /* Evaluate 4 (4 << 4 = 64 = 0x40) */
        0x00, 0x08, 0x80, 0xD2,  /* movz x0, #0x40 */
        0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */

        /* Body: (+ x y) */
        /* Load x from stack at offset 1 (second from top = 16 bytes) */
        0xE0, 0x0B, 0x40, 0xF9,  /* ldr x0, [sp, #16] */
        0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! (save for binop) */

        /* After push, stack is: [sp]=x_temp, [sp+16]=y, [sp+32]=x */
        /* Load y which is now at [sp, #16] */
        0xE0, 0x0B, 0x40, 0xF9,  /* ldr x0, [sp, #16] */
        0x01, 0x00, 0x00, 0xAA,  /* mov x1, x0 */
        0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 (pop x_temp) */

        /* Add */
        0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */

        /* Restore stack (2 bindings * 16 = 32) */
        0xFF, 0x83, 0x00, 0x91,  /* add sp, sp, #32 */

        /* Untag result */
        0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

        /* Epilogue */
        0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    int64_t result = execute_code(code, sizeof(code));
    printf("  Result: %lld (expected 7)\n", result);
    if (result == 7) {
        printf("  ✅ PASS\n\n");
    } else {
        printf("  ❌ FAIL\n\n");
        exit(1);
    }
}

int main() {
    printf("=== Lambda Expression Tests ===\n\n");

    test_lambda_simple();
    test_lambda_increment();
    test_lambda_two_params();

    printf("All lambda tests passed! ✅\n");
    return 0;
}
