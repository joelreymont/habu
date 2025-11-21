/* Test comparison operations: =, <, > */

#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

/* Equality: x == y */
uint8_t eq_code[] = {
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
    0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */
    0xE0, 0x17, 0x9F, 0x9A,  /* cset x0, eq */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* Less than: x < y */
uint8_t lt_code[] = {
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
    0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */
    0xE0, 0xA7, 0x9F, 0x9A,  /* cset x0, lt */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* Greater than: x > y */
uint8_t gt_code[] = {
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
    0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */
    0xE0, 0xD7, 0x9F, 0x9A,  /* cset x0, gt */
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

int main(void) {
    int passed = 0;
    int failed = 0;

    printf("\n=== Tier 1 Comparison Tests ===\n\n");

    /* Test equality */
    printf("Testing eq (=):\n");

    int64_t result = execute(eq_code, sizeof(eq_code), tag(5), tag(5));
    printf("  eq(5, 5) = %lld ", result);
    if (result == 1) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 1)\n");
        failed++;
    }

    result = execute(eq_code, sizeof(eq_code), tag(5), tag(7));
    printf("  eq(5, 7) = %lld ", result);
    if (result == 0) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 0)\n");
        failed++;
    }

    /* Test less than */
    printf("\nTesting lt (<):\n");

    result = execute(lt_code, sizeof(lt_code), tag(5), tag(7));
    printf("  lt(5, 7) = %lld ", result);
    if (result == 1) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 1)\n");
        failed++;
    }

    result = execute(lt_code, sizeof(lt_code), tag(7), tag(5));
    printf("  lt(7, 5) = %lld ", result);
    if (result == 0) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 0)\n");
        failed++;
    }

    /* Test greater than */
    printf("\nTesting gt (>):\n");

    result = execute(gt_code, sizeof(gt_code), tag(7), tag(5));
    printf("  gt(7, 5) = %lld ", result);
    if (result == 1) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 1)\n");
        failed++;
    }

    result = execute(gt_code, sizeof(gt_code), tag(5), tag(7));
    printf("  gt(5, 7) = %lld ", result);
    if (result == 0) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 0)\n");
        failed++;
    }

    printf("\n%d passed, %d failed\n\n", passed, failed);

    if (failed == 0) {
        printf("✓ All comparison operations working!\n\n");
        return 0;
    } else {
        printf("✗ Some tests failed\n\n");
        return 1;
    }
}
