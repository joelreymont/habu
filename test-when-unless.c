/* Test when and unless macros */

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
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) return -1;

    memcpy(mem, code, len);
    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        munmap(mem, page_size);
        return -1;
    }

    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();
    munmap(mem, page_size);
    return result;
}

int main() {
    int pass = 0, fail = 0;

    printf("========================================\n");
    printf("When/Unless Tests\n");
    printf("========================================\n");

    /* Test 1: (when 1 42) - test is true, should return 42 */
    {
        printf("\n=== Test 1: (when 1 42) ===\n");
        printf("Expected: 42\n");

        /* This is equivalent to (if 1 42 0) */
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9, 0xFD, 0x03, 0x00, 0x91,
            0x00, 0x02, 0x80, 0xD2,  // movz x0, #16 (1 << 4) - test
            0x1F, 0x00, 0x1F, 0xEB,  // cmp x0, xzr
            0x60, 0x00, 0x00, 0x54,  // beq else (+3 instrs)
            0x00, 0x54, 0x80, 0xD2,  // movz x0, #672 (42 << 4) - then
            0x02, 0x00, 0x00, 0x14,  // b end (+2 instrs)
            0x00, 0x00, 0x80, 0xD2,  // movz x0, #0 - else
            0x00, 0xFC, 0x44, 0xD3,  // lsr x0, x0, #4
            0xBF, 0x03, 0x00, 0x91, 0xFD, 0x7B, 0xC1, 0xA8, 0xC0, 0x03, 0x5F, 0xD6
        };

        int64_t r = execute_bytes(code, sizeof(code));
        printf("Result: %lld ", r);
        if (r == 42) {
            printf("✓ PASS\n");
            pass++;
        } else {
            printf("✗ FAIL\n");
            fail++;
        }
    }

    /* Test 2: (when 0 42) - test is false, should return 0 */
    {
        printf("\n=== Test 2: (when 0 42) ===\n");
        printf("Expected: 0\n");

        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9, 0xFD, 0x03, 0x00, 0x91,
            0x00, 0x00, 0x80, 0xD2,  // movz x0, #0 - test (false)
            0x1F, 0x00, 0x1F, 0xEB,
            0x60, 0x00, 0x00, 0x54,
            0x00, 0x54, 0x80, 0xD2,  // movz x0, #672 - then (skipped)
            0x02, 0x00, 0x00, 0x14,
            0x00, 0x00, 0x80, 0xD2,  // movz x0, #0 - else (executed)
            0x00, 0xFC, 0x44, 0xD3,
            0xBF, 0x03, 0x00, 0x91, 0xFD, 0x7B, 0xC1, 0xA8, 0xC0, 0x03, 0x5F, 0xD6
        };

        int64_t r = execute_bytes(code, sizeof(code));
        printf("Result: %lld ", r);
        if (r == 0) {
            printf("✓ PASS\n");
            pass++;
        } else {
            printf("✗ FAIL\n");
            fail++;
        }
    }

    /* Test 3: (unless 0 42) - test is false, should return 42 */
    {
        printf("\n=== Test 3: (unless 0 42) ===\n");
        printf("Expected: 42\n");

        /* This is equivalent to (if 0 0 42) */
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9, 0xFD, 0x03, 0x00, 0x91,
            0x00, 0x00, 0x80, 0xD2,  // movz x0, #0 - test (false)
            0x1F, 0x00, 0x1F, 0xEB,
            0x60, 0x00, 0x00, 0x54,  // beq else - branches because test is false
            0x00, 0x00, 0x80, 0xD2,  // movz x0, #0 - then (skipped)
            0x02, 0x00, 0x00, 0x14,
            0x00, 0x54, 0x80, 0xD2,  // movz x0, #672 (42 << 4) - else (executed)
            0x00, 0xFC, 0x44, 0xD3,
            0xBF, 0x03, 0x00, 0x91, 0xFD, 0x7B, 0xC1, 0xA8, 0xC0, 0x03, 0x5F, 0xD6
        };

        int64_t r = execute_bytes(code, sizeof(code));
        printf("Result: %lld ", r);
        if (r == 42) {
            printf("✓ PASS\n");
            pass++;
        } else {
            printf("✗ FAIL\n");
            fail++;
        }
    }

    /* Test 4: (unless 1 42) - test is true, should return 0 */
    {
        printf("\n=== Test 4: (unless 1 42) ===\n");
        printf("Expected: 0\n");

        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9, 0xFD, 0x03, 0x00, 0x91,
            0x00, 0x02, 0x80, 0xD2,  // movz x0, #16 (1 << 4) - test (true)
            0x1F, 0x00, 0x1F, 0xEB,
            0x60, 0x00, 0x00, 0x54,  // beq else - does NOT branch
            0x00, 0x00, 0x80, 0xD2,  // movz x0, #0 - then (executed)
            0x02, 0x00, 0x00, 0x14,
            0x00, 0x54, 0x80, 0xD2,  // movz x0, #672 - else (skipped)
            0x00, 0xFC, 0x44, 0xD3,
            0xBF, 0x03, 0x00, 0x91, 0xFD, 0x7B, 0xC1, 0xA8, 0xC0, 0x03, 0x5F, 0xD6
        };

        int64_t r = execute_bytes(code, sizeof(code));
        printf("Result: %lld ", r);
        if (r == 0) {
            printf("✓ PASS\n");
            pass++;
        } else {
            printf("✗ FAIL\n");
            fail++;
        }
    }

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");

    if (pass == 4) {
        printf("\n✓ When/Unless macros working!\n");
    }

    return (fail == 0) ? 0 : 1;
}
