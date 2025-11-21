/* Test simple function that computes 5! = 120 directly
 * No loops, just hardcoded arithmetic
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

/* Test: Simple function returning 120 (factorial 5)
 * Just loads the constant and returns it
 */
void test_return_constant(void) {
    printf("Test: Return constant 120\n");

    uint8_t code[] = {
        /* Prologue */
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        
        /* Load 120 tagged (1920 = 0x780) */
        0x00, 0xF0, 0x80, 0xD2,  /* movz x0, #1920 */
        
        /* Epilogue */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    habu_value_t result = execute_code(code, sizeof(code));
    int64_t value = value_to_fixnum(result);

    printf("  Result: %lld\n", (long long)value);
    if (value == 120) {
        printf("  PASS\n\n");
    } else {
        printf("  FAIL (expected 120)\n\n");
    }
}

int main(void) {
    printf("=== Simple Function Test ===\n\n");

    habu_init(1024 * 1024);
    printf("Runtime initialized\n\n");

    test_return_constant();

    habu_shutdown();
    printf("Runtime shut down\n");

    return 0;
}
