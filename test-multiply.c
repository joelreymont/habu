/* Test simple multiplication first */

#include "runtime/habu.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef habu_value_t (*habu_fn_t)(habu_value_t, habu_value_t);

habu_value_t execute_code(const uint8_t *code, size_t size, habu_value_t a, habu_value_t b) {
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
    habu_value_t result = fn(a, b);

    munmap(mem, page_size);
    return result;
}

/* Test: multiply two numbers (untagged) */
void test_multiply(void) {
    printf("Test: Multiply 5 * 24 = 120\n");

    uint8_t code[] = {
        /* Prologue */
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        
        /* Untag inputs: lsr x0, x0, #4 and lsr x1, x1, #4 */
        0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */
        0x21, 0xFC, 0x44, 0xD3,  /* lsr x1, x1, #4 */
        
        /* Multiply: mul x0, x0, x1 */
        0x00, 0x7C, 0x01, 0x9B,  /* mul x0, x0, x1 */
        
        /* Tag result: add x0, xzr, x0, lsl #4 */
        0xE0, 0x13, 0x00, 0x8B,  /* add x0, xzr, x0, lsl #4 */
        
        /* Epilogue */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    habu_value_t a = fixnum_to_value(5);
    habu_value_t b = fixnum_to_value(24);
    habu_value_t result = execute_code(code, sizeof(code), a, b);
    int64_t value = value_to_fixnum(result);

    printf("  Result: %lld\n", (long long)value);
    if (value == 120) {
        printf("  PASS\n\n");
    } else {
        printf("  FAIL (expected 120)\n\n");
    }
}

int main(void) {
    printf("=== Multiply Test ===\n\n");

    habu_init(1024 * 1024);
    printf("Runtime initialized\n\n");

    test_multiply();

    habu_shutdown();
    printf("Runtime shut down\n");

    return 0;
}
