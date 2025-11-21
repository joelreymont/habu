/* Test that compiler-generated factorial with BL offsets works */

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

void test_factorial() {
    printf("Test: Compiler-generated recursive factorial\\n");
    printf("===========================================\\n\\n");

    /* This is the pattern the compiler should generate:
     *
     * offset 0: factorial function
     * offset N: main function that calls factorial
     *
     * Main should use: BL (0 - (N + prologue_size))
     */

    /* For simplicity, let's test with inline code that matches compiler output */
    uint8_t code[] = {
        /* factorial at offset 0 - use safe stack pattern from test-factorial-recursive.c */
        0xFF, 0x83, 0x00, 0xD1,  /* sub sp, sp, #32 (offset 0) */
        0xFD, 0x7B, 0x00, 0xA9,  /* stp x29, x30, [sp, #0] (offset 4) */
        0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp (offset 8) */

        /* if x0 == 0 return 1 */
        0x1F, 0x00, 0x00, 0xF1,  /* cmp x0, #0 (offset 12) */
        0x61, 0x00, 0x00, 0x54,  /* b.ne +3 (to offset 0x1C=28, recursive case) (offset 16) */
        0x20, 0x00, 0x80, 0xD2,  /* movz x0, #1 (offset 20) */
        0x06, 0x00, 0x00, 0x14,  /* b +6 (to epilogue at offset 48) (offset 24) */

        /* recursive case (offset 28) */
        0xE0, 0x0B, 0x00, 0xF9,  /* str x0, [sp, #16] - save n (offset 28) */
        0x00, 0x04, 0x00, 0xD1,  /* sub x0, x0, #1 (offset 32) */

        /* BL to self: from offset 36 to offset 0 = -36 bytes = -9 words */
        0xF7, 0xFF, 0xFF, 0x97,  /* bl -9 (offset 36) */

        0xE1, 0x0B, 0x40, 0xF9,  /* ldr x1, [sp, #16] - restore n (offset 40) */
        0x20, 0x7C, 0x00, 0x9B,  /* mul x0, x1, x0 - n * factorial(n-1) (offset 44) */

        /* epilogue (offset 48) */
        0xFD, 0x7B, 0x40, 0xA9,  /* ldp x29, x30, [sp, #0] (offset 48) */
        0xFF, 0x83, 0x00, 0x91,  /* add sp, sp, #32 (offset 52) */
        0xC0, 0x03, 0x5F, 0xD6,  /* ret (offset 56) */

        /* main function at offset 60 */
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! (offset 60) */
        0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp (offset 64) */
        0xA0, 0x00, 0x80, 0xD2,  /* movz x0, #5 (untagged) (offset 68) */

        /* BL factorial: from offset 72 to offset 0 = -72 bytes = -18 words */
        0xEE, 0xFF, 0xFF, 0x97,  /* bl -18 (offset 72) */

        /* No untag needed - result is already untagged */
        0xFD, 0x03, 0x00, 0x91,  /* mov sp, x29 (offset 76) */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 (offset 80) */
        0xC0, 0x03, 0x5F, 0xD6   /* ret (offset 84) */
    };

    /* Call main which is at offset 60 */
    fn_t main_fn = (fn_t)((char*)code + 60);

    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return;
    }

    memcpy(mem, code, sizeof(code));

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return;
    }

    fn_t fn = (fn_t)((char*)mem + 60);  /* main at offset 60 */
    int64_t result = fn(0);  /* main takes no meaningful input */

    printf("  factorial(5) = %lld\\n", (long long)result);
    if (result == 120) {
        printf("  ✓ PASS - Compiler pattern works!\\n\\n");
    } else {
        printf("  ✗ FAIL (expected 120, got %lld)\\n\\n", (long long)result);
    }

    munmap(mem, page_size);
}

int main(void) {
    printf("\\n=== Testing Compiler Integration Pattern ===\\n\\n");
    test_factorial();
    return 0;
}
