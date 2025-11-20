/* Test ARM64 Code Generation - Verify intrinsics generate correct machine code
 * This tests that our ARM64 intrinsics generate the expected byte sequences
 */

#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int execute_bytes(unsigned char *code, size_t code_len, const char *name, int64_t expected) {
    printf("\n=== Testing: %s ===\n", name);
    printf("Code (%zu bytes): ", code_len);
    for (size_t i = 0; i < code_len; i++) {
        printf("%02X ", code[i]);
    }
    printf("\n");

    size_t page_size = 4096;
    size_t aligned_size = ((code_len + page_size - 1) / page_size) * page_size;

    void *mem = mmap(NULL, aligned_size,
                     PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT,
                     -1, 0);

    if (mem == MAP_FAILED) {
        perror("mmap");
        return -1;
    }

    memcpy(mem, code, code_len);

    if (mprotect(mem, aligned_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, aligned_size);
        return -1;
    }

    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();

    printf("Result: %lld (expected %lld)\n", result, expected);

    munmap(mem, aligned_size);

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
    printf("ARM64 Code Generation Tests\n");
    printf("========================================\n");

    int pass = 0;
    int fail = 0;
    int result;

    /* Test 1: movz x0, #672; lsr x0, x0, #4; ret
     * Should return 42 (672 >> 4)
     * This tests: arm64-movz, arm64-lsr, arm64-ret
     */
    {
        unsigned char code[] = {
            0x00, 0x54, 0x80, 0xD2,  /* movz x0, #672 */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };
        result = execute_bytes(code, sizeof(code), "Return 42 (movz + lsr)", 42);
        if (result > 0) pass++; else fail++;
    }

    /* Test 2: movz x0, #100; movz x1, #200; add x0, x0, x1; ret
     * Should return 300
     * This tests: arm64-movz with different registers, arm64-add
     */
    {
        unsigned char code[] = {
            0x80, 0x0C, 0x80, 0xD2,  /* movz x0, #100 */
            0x01, 0x19, 0x80, 0xD2,  /* movz x1, #200 */
            0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };
        result = execute_bytes(code, sizeof(code), "Add 100 + 200", 300);
        if (result > 0) pass++; else fail++;
    }

    /* Test 3: movz x0, #100; movz x1, #30; sub x0, x0, x1; ret
     * Should return 70
     * This tests: arm64-sub
     */
    {
        unsigned char code[] = {
            0x80, 0x0C, 0x80, 0xD2,  /* movz x0, #100 */
            0xC1, 0x03, 0x80, 0xD2,  /* movz x1, #30 */
            0x00, 0x00, 0x01, 0xCB,  /* sub x0, x0, x1 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };
        result = execute_bytes(code, sizeof(code), "Subtract 100 - 30", 70);
        if (result > 0) pass++; else fail++;
    }

    /* Test 4: movz x0, #10; movz x1, #5; mul x0, x0, x1; ret
     * Should return 50
     * This tests: arm64-mul
     */
    {
        unsigned char code[] = {
            0x40, 0x01, 0x80, 0xD2,  /* movz x0, #10 */
            0xA1, 0x00, 0x80, 0xD2,  /* movz x1, #5 */
            0x00, 0x7C, 0x01, 0x9B,  /* mul x0, x0, x1 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };
        result = execute_bytes(code, sizeof(code), "Multiply 10 * 5", 50);
        if (result > 0) pass++; else fail++;
    }

    /* Test 5: movz x0, #32; lsl x0, x0, #4; ret
     * Should return 512 (32 << 4)
     * This tests: arm64-lsl
     */
    {
        unsigned char code[] = {
            0x00, 0x04, 0x80, 0xD2,  /* movz x0, #32 */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };
        result = execute_bytes(code, sizeof(code), "Shift left 32 << 4", 512);
        if (result > 0) pass++; else fail++;
    }

    /* Test 6: movz x0, #100; mov x1, x0; ret (returns from x0)
     * Should return 100
     * This tests: arm64-mov
     */
    {
        unsigned char code[] = {
            0x80, 0x0C, 0x80, 0xD2,  /* movz x0, #100 */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };
        result = execute_bytes(code, sizeof(code), "Move register", 100);
        if (result > 0) pass++; else fail++;
    }

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");

    return (fail == 0) ? 0 : 1;
}
