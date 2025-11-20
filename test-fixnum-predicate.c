/* Test FIXNUM? predicate */

#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int64_t execute_bytes(unsigned char *code, size_t len) {
    size_t page_size = 4096;
    size_t aligned_size = ((len + page_size - 1) / page_size) * page_size;

    void *mem = mmap(NULL, aligned_size,
                     PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT,
                     -1, 0);

    if (mem == MAP_FAILED) {
        perror("mmap");
        return -1;
    }

    memcpy(mem, code, len);

    if (mprotect(mem, aligned_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, aligned_size);
        return -1;
    }

    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();
    munmap(mem, aligned_size);

    return result;
}

int test_fixnum(const char *expr, unsigned char *code, size_t len, int64_t expected) {
    printf("\n=== %s ===\n", expr);
    printf("Expected: %lld\n", expected);

    int64_t result = execute_bytes(code, len);
    printf("Result: %lld ", result);

    if (result == expected) {
        printf("✓ PASS\n");
        return 1;
    } else {
        printf("✗ FAIL\n");
        return 0;
    }
}

int main() {
    printf("========================================\n");
    printf("FIXNUM? Predicate Tests\n");
    printf("========================================\n");

    int pass = 0, fail = 0;

    /* Test 1: (fixnum? 42) → 1 (fixnums have lower 4 bits = 0) */
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* Load 42 (tagged) */
            0x00, 0x54, 0x80, 0xD2,  /* movz x0, #672 (42 << 4) */

            /* fixnum? check */
            0xE1, 0x01, 0x80, 0xD2,  /* movz x1, #15 */
            0x01, 0x00, 0x01, 0x8A,  /* and x1, x0, x1 */
            0x3F, 0x00, 0x1F, 0xEB,  /* cmp x1, xzr */
            0xE0, 0x17, 0x9F, 0x9A,  /* cset x0, EQ */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,

            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_fixnum("(fixnum? 42)", code, sizeof(code), 1) ? pass++ : fail++;
    }

    /* Test 2: (fixnum? 0) → 1 (0 is a fixnum) */
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            0x00, 0x00, 0x80, 0xD2,  /* movz x0, #0 */

            0xE1, 0x01, 0x80, 0xD2,
            0x01, 0x00, 0x01, 0x8A,
            0x3F, 0x00, 0x1F, 0xEB,
            0xE0, 0x17, 0x9F, 0x9A,
            0x00, 0xEC, 0x7C, 0xD3,

            0x00, 0xFC, 0x44, 0xD3,

            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_fixnum("(fixnum? 0)", code, sizeof(code), 1) ? pass++ : fail++;
    }

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");

    if (pass == 2) {
        printf("\n✓ FIXNUM? working!\n");
    }

    return (fail == 0) ? 0 : 1;
}
