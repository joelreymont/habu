/* Test type predicates: nil?, cons? */

#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

/* nil?: Check if value is nil (0) */
uint8_t nil_p_code[] = {
    /* Prologue */
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */

    /* Compare x0 with 0 (nil is represented as 0) */
    0x1F, 0x00, 0x00, 0xF1,  /* cmp x0, #0 */

    /* Set x0 to 1 if equal, 0 if not */
    0xE0, 0x17, 0x9F, 0x9A,  /* cset x0, eq */

    /* Epilogue */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* cons?: Check if value is cons cell (tag == 1) */
uint8_t cons_p_code[] = {
    /* Prologue */
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */

    /* Extract tag: x0 & 0xF */
    0x00, 0x0C, 0x40, 0x92,  /* and x0, x0, #0xF */

    /* Compare with 1 (cons tag) */
    0x1F, 0x04, 0x00, 0xF1,  /* cmp x0, #1 */

    /* Set x0 to 1 if equal, 0 if not */
    0xE0, 0x17, 0x9F, 0x9A,  /* cset x0, eq */

    /* Epilogue */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

typedef int64_t (*unary_fn_t)(int64_t);

int64_t execute(uint8_t *code, size_t size, int64_t a) {
    void *mem = mmap(NULL, 4096, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    memcpy(mem, code, size);
    mprotect(mem, 4096, PROT_READ | PROT_EXEC);
    unary_fn_t fn = (unary_fn_t)mem;
    int64_t result = fn(a);
    munmap(mem, 4096);
    return result;
}

int64_t tag(int64_t n) { return n << 4; }

int main(void) {
    int passed = 0;
    int failed = 0;

    printf("\n=== Tier 1 Predicate Tests ===\n\n");

    /* Test nil? */
    printf("Testing nil?:\n");

    int64_t result = execute(nil_p_code, sizeof(nil_p_code), 0);
    printf("  nil?(nil) = %lld ", result);
    if (result == 1) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 1)\n");
        failed++;
    }

    result = execute(nil_p_code, sizeof(nil_p_code), tag(5));
    printf("  nil?(5) = %lld ", result);
    if (result == 0) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 0)\n");
        failed++;
    }

    result = execute(nil_p_code, sizeof(nil_p_code), 0x1234);
    printf("  nil?(0x1234) = %lld ", result);
    if (result == 0) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 0)\n");
        failed++;
    }

    /* Test cons? */
    printf("\nTesting cons?:\n");

    result = execute(cons_p_code, sizeof(cons_p_code), 0x11);  /* Tag = 1 */
    printf("  cons?(0x11) = %lld ", result);
    if (result == 1) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 1)\n");
        failed++;
    }

    result = execute(cons_p_code, sizeof(cons_p_code), 0x21);  /* Tag = 1 */
    printf("  cons?(0x21) = %lld ", result);
    if (result == 1) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 1)\n");
        failed++;
    }

    result = execute(cons_p_code, sizeof(cons_p_code), tag(5));  /* Tag = 0 */
    printf("  cons?(5) = %lld ", result);
    if (result == 0) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 0)\n");
        failed++;
    }

    result = execute(cons_p_code, sizeof(cons_p_code), 0);  /* nil, tag = 0 */
    printf("  cons?(nil) = %lld ", result);
    if (result == 0) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 0)\n");
        failed++;
    }

    result = execute(cons_p_code, sizeof(cons_p_code), 0x12);  /* Tag = 2 */
    printf("  cons?(0x12) = %lld ", result);
    if (result == 0) {
        printf("✓\n");
        passed++;
    } else {
        printf("✗ (expected 0)\n");
        failed++;
    }

    printf("\n%d passed, %d failed\n\n", passed, failed);

    if (failed == 0) {
        printf("✓ All predicate operations working!\n\n");
        return 0;
    } else {
        printf("✗ Some tests failed\n\n");
        return 1;
    }
}
