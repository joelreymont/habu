/* Test integration of compiler-generated code with runtime
 * Demonstrates that generated code can execute complex programs
 */

#include "runtime/habu.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef habu_value_t (*habu_fn_t)(void);

habu_value_t execute_code(const uint8_t *code, size_t size) {
    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return 0;
    }

    memcpy(mem, code, size);

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return 0;
    }

    habu_fn_t fn = (habu_fn_t)mem;
    habu_value_t result = fn();

    munmap(mem, page_size);
    return result;
}

/* Test 1: Literal - compiler would generate this for: 42 */
void test_literal(void) {
    printf("Test 1: Literal 42\n");

    /* Code pattern from habu-arm64-codegen.lisp:
     * - Prologue (save FP/LR)
     * - Load tagged value
     * - Epilogue (restore FP/LR)
     * - Return
     */
    uint8_t code[] = {
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        0x00, 0x54, 0x80, 0xD2,  /* movz x0, #0x2A0 (42 << 4 = 672) */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    habu_value_t result = execute_code(code, sizeof(code));
    int64_t value = value_to_fixnum(result);

    printf("  Result: %lld\n", (long long)value);
    if (value == 42) {
        printf("  PASS\n\n");
    } else {
        printf("  FAIL (expected 42)\n\n");
    }
}

/* Test 2: Addition - compiler would generate this for: (+ 10 32) */
void test_addition(void) {
    printf("Test 2: Addition (+ 10 32)\n");

    uint8_t code[] = {
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        0x00, 0x14, 0x80, 0xD2,  /* movz x0, #0xA0 (10 << 4) */
        0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
        0x00, 0x40, 0x80, 0xD2,  /* movz x0, #0x200 (32 << 4) */
        0xE1, 0x07, 0x41, 0xF8,  /* ldr x1, [sp], #16 */
        0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    habu_value_t result = execute_code(code, sizeof(code));
    int64_t value = value_to_fixnum(result);

    printf("  Result: %lld\n", (long long)value);
    if (value == 42) {
        printf("  PASS\n\n");
    } else {
        printf("  FAIL (expected 42)\n\n");
    }
}

/* Test 3: Multiplication - compiler would generate this for: (/ 84 2) */
void test_division(void) {
    printf("Test 3: Division (/ 84 2)\n");

    uint8_t code[] = {
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        0x80, 0xA8, 0x80, 0xD2,  /* movz x0, #0x540 (84 << 4) */
        0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
        0x00, 0x04, 0x80, 0xD2,  /* movz x0, #0x20 (2 << 4) */
        0xE1, 0x07, 0x41, 0xF8,  /* ldr x1, [sp], #16 */
        0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 (untag divisor) */
        0x20, 0x0C, 0xC0, 0x9A,  /* udiv x0, x1, x0 */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    habu_value_t result = execute_code(code, sizeof(code));
    int64_t value = value_to_fixnum(result);

    printf("  Result: %lld\n", (long long)value);
    if (value == 42) {
        printf("  PASS\n\n");
    } else {
        printf("  FAIL (expected 42)\n\n");
    }
}

int main(void) {
    printf("=== Compiler Integration Tests ===\n\n");

    /* Initialize runtime */
    habu_init(1024 * 1024);
    printf("Runtime initialized\n\n");

    /* Run tests */
    test_literal();
    test_addition();
    test_division();

    /* Cleanup */
    habu_shutdown();
    printf("Runtime shut down\n");

    return 0;
}
