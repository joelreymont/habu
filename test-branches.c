/* Test ARM64 Branch Instructions
 * Verifies B and B.cond work correctly
 */

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

int test_branch(const char *name, unsigned char *code, size_t len, int64_t expected) {
    printf("\n=== %s ===\n", name);
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
    printf("ARM64 Branch Instruction Tests\n");
    printf("========================================\n");

    int pass = 0, fail = 0;

    /* Test 1: Unconditional branch - skip setting x0 to 99, return 42
     * movz x0, #42
     * b skip         ; Branch forward 1 instruction
     * movz x0, #99   ; Should be skipped
     * skip: ret
     */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
            0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

            /* movz x0, #42 */
            0x00, 0x54, 0x80, 0xD2,  /* movz x0, #672 (42 << 4) */

            /* b skip (forward 2 instructions = offset +2) */
            0x02, 0x00, 0x00, 0x14,  /* b +2 */

            /* movz x0, #99 (should be skipped) */
            0x00, 0xE6, 0x80, 0xD2,  /* movz x0, #1584 (99 << 4) */

            /* skip: untag and return */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
            0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };
        test_branch("Unconditional branch", code, sizeof(code), 42) ? pass++ : fail++;
    }

    /* Test 2: Conditional branch - if (5 == 5) return 1 else return 0
     * movz x0, #5
     * movz x1, #5
     * cmp x0, x1
     * b.ne else      ; Branch if not equal
     * movz x0, #1    ; then branch
     * b end
     * else:
     * movz x0, #0    ; else branch
     * end: ret
     */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
            0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

            /* Load values and compare */
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 (5 << 4) */
            0x01, 0x0A, 0x80, 0xD2,  /* movz x1, #80 (5 << 4) */
            0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */

            /* b.ne else (forward 3 instructions = offset +3) */
            0x61, 0x00, 0x00, 0x54,  /* b.ne +3 (cond=1, offset=3) */

            /* then: movz x0, #1 */
            0x00, 0x02, 0x80, 0xD2,  /* movz x0, #16 (1 << 4) */

            /* b end (forward 2 instructions = offset +2) */
            0x02, 0x00, 0x00, 0x14,  /* b +2 */

            /* else: movz x0, #0 */
            0x00, 0x00, 0x80, 0xD2,  /* movz x0, #0 */

            /* end: untag */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
            0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };
        test_branch("Conditional (5 == 5) → 1", code, sizeof(code), 1) ? pass++ : fail++;
    }

    /* Test 3: Conditional branch - if (5 == 3) return 1 else return 0
     * Same as above but 5 != 3, so should return 0
     */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,
            0xFD, 0x03, 0x00, 0x91,

            /* Load 5 and 3, compare */
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 (5 << 4) */
            0x01, 0x06, 0x80, 0xD2,  /* movz x1, #48 (3 << 4) */
            0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */

            /* b.ne else */
            0x61, 0x00, 0x00, 0x54,  /* b.ne +3 */

            /* then: return 1 */
            0x00, 0x02, 0x80, 0xD2,  /* movz x0, #16 */

            /* b end */
            0x02, 0x00, 0x00, 0x14,  /* b +2 */

            /* else: return 0 */
            0x00, 0x00, 0x80, 0xD2,  /* movz x0, #0 */

            /* end: untag */
            0x00, 0xFC, 0x44, 0xD3,

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,
            0xFD, 0x7B, 0xC1, 0xA8,
            0xC0, 0x03, 0x5F, 0xD6
        };
        test_branch("Conditional (5 == 3) → 0", code, sizeof(code), 0) ? pass++ : fail++;
    }

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");

    return (fail == 0) ? 0 : 1;
}
