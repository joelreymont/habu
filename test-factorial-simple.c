/* Test simple factorial - simplified version
 * Computes factorial(5) = 120 using untagged arithmetic
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

typedef habu_value_t (*habu_fn_t)(habu_value_t);

habu_value_t execute_code(const uint8_t *code, size_t size, habu_value_t input) {
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
    habu_value_t result = fn(input);

    munmap(mem, page_size);
    return result;
}

/* Simple iterative factorial
 * Works with untagged integers, tags at end
 */
void test_factorial_iterative(void) {
    printf("Test: Iterative factorial(5)\n");

    uint8_t code[] = {
        /* Prologue */
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        
        /* Untag input: lsr x0, x0, #4 */
        0x00, 0xFC, 0x44, 0xD3,  
        
        /* result = 1 in x1 */
        0x01, 0x00, 0x80, 0xD2,  /* movz x1, #0 (will increment to 1) */
        0x21, 0x00, 0x00, 0x91,  /* add x1, x1, #0 (actually just set to 1) */
        /* Actually, let me use: movz x1, #1 */
        0x21, 0x00, 0x80, 0xD2,  /* movz x1, #1 */
        
        /* loop: */
        /* if n == 0 goto done */
        0x1F, 0x00, 0x00, 0xF1,  /* cmp x0, #0 */
        0x60, 0x00, 0x00, 0x54,  /* b.eq done (forward 3 instructions) */
        
        /* result *= n */
        0x21, 0x7C, 0x00, 0x9B,  /* mul x1, x1, x0 */
        
        /* n-- */
        0x00, 0x04, 0x00, 0xD1,  /* sub x0, x0, #1 */
        
        /* goto loop */
        0xFE, 0xFF, 0xFF, 0x17,  /* b loop */
        
        /* done: move result to x0 and tag it */
        0xE0, 0x03, 0x01, 0xAA,  /* mov x0, x1 */
        
        /* Tag result: x0 = x0 << 4 using add x0, xzr, x0, lsl #4 */
        0xE0, 0x13, 0x00, 0x8B,  /* add x0, xzr, x0, lsl #4 */
        
        /* Epilogue */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    habu_value_t input = fixnum_to_value(5);
    habu_value_t result = execute_code(code, sizeof(code), input);
    int64_t value = value_to_fixnum(result);

    printf("  factorial(5) = %lld\n", (long long)value);
    if (value == 120) {
        printf("  PASS\n\n");
    } else {
        printf("  FAIL (expected 120, got %lld)\n\n", (long long)value);
    }
}

int main(void) {
    printf("=== Factorial Test ===\n\n");

    habu_init(1024 * 1024);
    printf("Runtime initialized\n\n");

    test_factorial_iterative();

    habu_shutdown();
    printf("Runtime shut down\n");

    return 0;
}
