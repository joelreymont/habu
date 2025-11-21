/* Test recursive factorial */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*fn_t)(int64_t);

int64_t execute_code(const uint8_t *code, size_t size, int64_t input) {
    /* Pre-allocate large stack buffer to ensure safety */
    volatile char stack_buffer[16384];  /* 16KB */
    stack_buffer[0] = stack_buffer[16383] = 0;

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

    fn_t fn = (fn_t)mem;
    int64_t result = fn(input);

    munmap(mem, page_size);
    return result;
}

void test_factorial_recursive(void) {
    /* Allocate large stack buffer FIRST to ensure space for all subsequent calls */
    volatile char guard_stack[32768];  /* 32KB guard */
    guard_stack[0] = guard_stack[32767] = 0;

    printf("Test: Recursive factorial(5) = 120\n");

    uint8_t code[] = {
        /* factorial: */
        /* Prologue - allocate stack space first to avoid page boundaries */
        0xFF, 0x83, 0x00, 0xD1,  /* sub sp, sp, #32 (offset 0) */
        0xFD, 0x7B, 0x00, 0xA9,  /* stp x29, x30, [sp, #0] (offset 4) */
        0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp (offset 8) */

        /* if x0 == 0 return 1 */
        0x1F, 0x00, 0x00, 0xF1,  /* cmp x0, #0 (offset 12) */
        0x41, 0x00, 0x00, 0x54,  /* b.ne +2 (offset 16) */
        0x20, 0x00, 0x80, 0xD2,  /* movz x0, #1 (offset 20) */
        0x06, 0x00, 0x00, 0x14,  /* b +6 (to epilogue at offset 48) (offset 24) */

        /* recursive case (offset 28) */
        0xE0, 0x0B, 0x00, 0xF9,  /* str x0, [sp, #16] - save n (offset 28) */
        0x00, 0x04, 0x00, 0xD1,  /* sub x0, x0, #1 (offset 32) */

        /* BL to self: from offset 36 to offset 0 = -36 bytes = -9 words */
        /* -9 in 26-bit two's complement = 0x3FFFFF7 */
        0xF7, 0xFF, 0xFF, 0x97,  /* bl -9 (offset 36) */

        0xE1, 0x0B, 0x40, 0xF9,  /* ldr x1, [sp, #16] - restore n (offset 40) */
        0x20, 0x7C, 0x00, 0x9B,  /* mul x0, x1, x0 - n * factorial(n-1) (offset 44) */

        /* epilogue (offset 48) */
        0xFD, 0x7B, 0x40, 0xA9,  /* ldp x29, x30, [sp, #0] (offset 48) */
        0xFF, 0x83, 0x00, 0x91,  /* add sp, sp, #32 (offset 52) */
        0xC0, 0x03, 0x5F, 0xD6   /* ret (offset 56) */
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
    test_factorial_recursive();
    return 0;
}
