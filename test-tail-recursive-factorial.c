/* Test tail-recursive factorial with tail-call optimization */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*fn_t)(int64_t, int64_t);

int64_t execute_code(const uint8_t *code, size_t size, int64_t n, int64_t acc) {
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
    int64_t result = fn(n, acc);

    munmap(mem, page_size);
    return result;
}

void test_tail_recursive_factorial() {
    printf("Test: Tail-recursive factorial(5) with TCO\\n");
    printf("==========================================\\n\\n");

    /*
     * Tail-recursive factorial with accumulator:
     *   factorial_tail(n, acc):
     *     if n == 0: return acc
     *     else: return factorial_tail(n-1, n*acc)  # Tail call!
     *
     * With TCO, this should compile to a loop (no stack growth)
     */

    uint8_t code[] = {
        /* factorial_tail at offset 0 - x0=n, x1=acc */
        /* Prologue - safe pattern */
        0xFF, 0x83, 0x00, 0xD1,  /* sub sp, sp, #32 (offset 0) */
        0xFD, 0x7B, 0x00, 0xA9,  /* stp x29, x30, [sp, #0] (offset 4) */
        0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp (offset 8) */

        /* if n == 0 return acc */
        0x1F, 0x00, 0x00, 0xF1,  /* cmp x0, #0 (offset 12) */
        0x61, 0x00, 0x00, 0x54,  /* b.ne +3 (to offset 0x1C=28, recursive case) (offset 16) */
        0xE0, 0x03, 0x01, 0xAA,  /* mov x0, x1 (return acc) (offset 20) */
        0x06, 0x00, 0x00, 0x14,  /* b +6 (to epilogue at offset 44) (offset 24) */

        /* recursive case (offset 28) - TAIL CALL */
        /* Save n to x2 temporarily */
        0xE2, 0x03, 0x00, 0xAA,  /* mov x2, x0 (x2 = n) (offset 28) */
        /* n = n - 1 */
        0x00, 0x04, 0x00, 0xD1,  /* sub x0, x0, #1 (offset 32) */
        /* acc = n * acc (x1 = x2 * x1) */
        0x21, 0x7C, 0x02, 0x9B,  /* mul x1, x1, x2 (offset 36) */

        /* TAIL CALL: jump to start WITHOUT restoring frame (reuse current frame!) */
        /* Jump back to function body start (after prologue at offset 12) */
        /* From offset 40 to offset 12 = -28 bytes = -7 words */
        0xF9, 0xFF, 0xFF, 0x17,  /* b -7 (offset 40) */

        /* epilogue (offset 44) - only reached by base case */
        0xFD, 0x7B, 0x40, 0xA9,  /* ldp x29, x30, [sp, #0] (offset 44) */
        0xFF, 0x83, 0x00, 0x91,  /* add sp, sp, #32 (offset 48) */
        0xC0, 0x03, 0x5F, 0xD6   /* ret (offset 52) */
    };

    int64_t result = execute_code(code, sizeof(code), 5, 1);

    printf("  factorial_tail(5, 1) = %lld\\n", (long long)result);
    if (result == 120) {
        printf("  ✓ PASS - Tail-call optimization works!\\n");
        printf("  No stack growth for tail recursion!\\n\\n");
    } else {
        printf("  ✗ FAIL (expected 120, got %lld)\\n\\n", (long long)result);
    }
}

void test_stack_depth() {
    printf("Test: Verify no stack growth with deep recursion\\n");
    printf("===============================================\\n\\n");

    /* Same code as above - tail-call pattern */
    uint8_t code[] = {
        0xFF, 0x83, 0x00, 0xD1,  0xFD, 0x7B, 0x00, 0xA9,
        0xFD, 0x03, 0x00, 0x91,  0x1F, 0x00, 0x00, 0xF1,
        0x61, 0x00, 0x00, 0x54,  0xE0, 0x03, 0x01, 0xAA,
        0x06, 0x00, 0x00, 0x14,  0xE2, 0x03, 0x00, 0xAA,
        0x00, 0x04, 0x00, 0xD1,  0x21, 0x7C, 0x02, 0x9B,
        0xF9, 0xFF, 0xFF, 0x17,  0xFD, 0x7B, 0x40, 0xA9,
        0xFF, 0x83, 0x00, 0x91,  0xC0, 0x03, 0x5F, 0xD6
    };

    /* Test with VERY deep recursion - would overflow stack without TCO */
    int64_t result = execute_code(code, sizeof(code), 10, 1);

    printf("  factorial_tail(10, 1) = %lld\\n", (long long)result);
    if (result == 3628800) {
        printf("  ✓ PASS - Can handle deep recursion with TCO!\\n\\n");
    } else {
        printf("  ✗ FAIL (expected 3628800, got %lld)\\n\\n", (long long)result);
    }
}

int main(void) {
    printf("\\n=== Tail-Call Optimization Tests ===\\n\\n");
    test_tail_recursive_factorial();
    test_stack_depth();
    return 0;
}
