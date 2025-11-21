/* Inline test - all code in one file */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

/* Addition */
uint8_t add_code[] = {
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
    0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* Subtraction */
uint8_t sub_code[] = {
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
    0x00, 0x00, 0x01, 0xCB,  /* sub x0, x0, x1 */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* Multiplication */
uint8_t mul_code[] = {
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
    0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */
    0x21, 0xFC, 0x44, 0xD3,  /* lsr x1, x1, #4 */
    0x00, 0x7C, 0x01, 0x9B,  /* mul x0, x0, x1 */
    0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* Division */
uint8_t div_code[] = {
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
    0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */
    0x21, 0xFC, 0x44, 0xD3,  /* lsr x1, x1, #4 */
    0x00, 0x08, 0xC1, 0x9A,  /* udiv x0, x0, x1 */
    0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

typedef int64_t (*binary_fn_t)(int64_t, int64_t);

int64_t execute(uint8_t *code, size_t size, int64_t a, int64_t b) {
    void *mem = mmap(NULL, 4096, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    memcpy(mem, code, size);
    mprotect(mem, 4096, PROT_READ | PROT_EXEC);
    binary_fn_t fn = (binary_fn_t)mem;
    int64_t result = fn(a, b);
    munmap(mem, 4096);
    return result;
}

int64_t tag(int64_t n) { return n << 4; }
int64_t untag(int64_t n) { return n >> 4; }

int main(void) {
    int passed = 0;
    int failed = 0;

    printf("\n=== Tier 1 Arithmetic Tests ===\n\n");

    int64_t result = execute(add_code, sizeof(add_code), tag(5), tag(7));
    printf("add(5, 7) = %lld ", untag(result));
    if (untag(result) == 12) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 12)\n");
        failed++;
    }

    result = execute(sub_code, sizeof(sub_code), tag(10), tag(3));
    printf("sub(10, 3) = %lld ", untag(result));
    if (untag(result) == 7) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 7)\n");
        failed++;
    }

    result = execute(mul_code, sizeof(mul_code), tag(6), tag(7));
    printf("mul(6, 7) = %lld ", untag(result));
    if (untag(result) == 42) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 42)\n");
        failed++;
    }

    result = execute(div_code, sizeof(div_code), tag(20), tag(4));
    printf("div(20, 4) = %lld ", untag(result));
    if (untag(result) == 5) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 5)\n");
        failed++;
    }

    printf("\n%d passed, %d failed\n\n", passed, failed);
    return (failed == 0) ? 0 : 1;
}
