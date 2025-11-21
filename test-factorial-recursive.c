/* Test recursive factorial - simpler version */

#include "runtime/habu.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(int64_t);

int64_t execute_code(const uint8_t *code, size_t size, int64_t input) {
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
    int64_t result = fn(input);

    munmap(mem, page_size);
    return result;
}

void test_factorial_recursive(void) {
    printf("Test: Recursive factorial(5) = 120\n");

    uint8_t code[] = {
        /* factorial: */
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        
        /* if x0 == 0 return 1 */
        0x1F, 0x00, 0x00, 0xF1,  /* cmp x0, #0 */
        0x41, 0x00, 0x00, 0x54,  /* b.ne +2 */
        0x20, 0x00, 0x80, 0xD2,  /* movz x0, #1 */
        0x03, 0x00, 0x00, 0x14,  /* b +3 (to epilogue) */
        
        /* recursive case */
        0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
        0x00, 0x04, 0x00, 0xD1,  /* sub x0, x0, #1 */
        
        /* BL to self: offset from here (at byte 28) to start (at byte 0) = -28 bytes = -7 words */
        /* -7 in 26-bit two's complement = 0x3FFFFF9 */
        0xF9, 0xFF, 0xFF, 0x97,  /* bl -7 */
        
        0xE1, 0x07, 0x41, 0xF8,  /* ldr x1, [sp], #16 */
        0x20, 0x7C, 0x00, 0x9B,  /* mul x0, x1, x0 */
        
        /* epilogue */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    int64_t result = execute_code(code, sizeof(code), 5);

    printf("  factorial(5) = %lld\n", (long long)result);
    if (result == 120) {
        printf("  PASS - Recursive factorial works!\n\n");
    } else {
        printf("  FAIL (expected 120, got %lld)\n\n", (long long)result);
    }
}

int main(void) {
    printf("=== Recursive Factorial Test ===\n\n");

    habu_init(1024 * 1024);
    printf("Runtime initialized\n\n");

    test_factorial_recursive();

    habu_shutdown();
    printf("Runtime shut down\n");

    return 0;
}
