/* Simplest possible recursion test: count down and return */

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
        return -999;
    }

    memcpy(mem, code, size);

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return -999;
    }

    fn_t fn = (fn_t)mem;
    int64_t result = fn(input);

    munmap(mem, page_size);
    return result;
}

void test_simple_recursion(void) {
    /* Allocate large stack buffer FIRST to ensure space for all subsequent calls */
    volatile char guard_stack[32768];  /* 32KB guard */
    guard_stack[0] = guard_stack[32767] = 0;

    printf("Test: Simple recursion - just count down (should return 0)\n");

    uint8_t code[] = {
        /* countdown: */
        /* Prologue - with trampoline ensuring stack space, use standard 64-byte frame */
        0xFF, 0x03, 0x01, 0xD1,  /* sub sp, sp, #64 (offset 0) */
        0xFD, 0x7B, 0x00, 0xA9,  /* stp x29, x30, [sp, #0] (offset 4) */
        0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp (offset 8) */

        /* if (x0 == 0) return 0 */
        0x1F, 0x00, 0x00, 0xF1,  /* cmp x0, #0 (offset 12) */
        0x41, 0x00, 0x00, 0x54,  /* b.ne +2 (skip to recursive) (offset 16) */

        /* x0 is already 0, jump to epilogue */
        0x03, 0x00, 0x00, 0x14,  /* b +3 (jump to epilogue at offset 32) (offset 20) */

        /* recursive: (offset 24) */
        /* x0 = x0 - 1 */
        0x00, 0x04, 0x00, 0xD1,  /* sub x0, x0, #1 (offset 24) */

        /* BL to start: from offset 28 back to offset 0 */
        /* -28 bytes / 4 = -7 words */
        /* -7 in 26-bit two's complement = 0x3FFFFF9 */
        0xF9, 0xFF, 0xFF, 0x97,  /* bl -7 (offset 28) */

        /* epilogue: (offset 32) */
        /* Restore x29, x30 and deallocate */
        0xFD, 0x7B, 0x40, 0xA9,  /* ldp x29, x30, [sp] (offset 32) */
        0xFF, 0x03, 0x01, 0x91,  /* add sp, sp, #64 (offset 36) */

        /* Return */
        0xC0, 0x03, 0x5F, 0xD6   /* ret (offset 40) */
    };

    printf("  Testing countdown(2)...\n");
    int64_t result = execute_code(code, sizeof(code), 2);

    printf("  Result: %lld\n", (long long)result);
    if (result == 0) {
        printf("  PASS\n\n");
    } else {
        printf("  FAIL (expected 0, got %lld)\n\n", (long long)result);
    }
}

int main(void) {
    printf("=== Simple Recursion Test ===\n\n");
    test_simple_recursion();
    return 0;
}
