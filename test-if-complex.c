/* Debug test 5: (if (> 8 3) (+ 10 5) (* 2 3)) should return 15 */

#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int main() {
    /* Test: (> 8 3) is true, should take then branch and return (+ 10 5) = 15 */
    unsigned char code[] = {
        /* Prologue */
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

        /* Test: (> 8 3) */
        0x00, 0x10, 0x80, 0xD2,  /* movz x0, #128 (8 << 4) */
        0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
        0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 (3 << 4) */
        0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
        0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
        0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */
        0xE0, 0xD7, 0x9F, 0x9A,  /* cset x0, GT */
        0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */

        /* Compare with zero */
        0x1F, 0x00, 0x1F, 0xEB,  /* cmp x0, xzr */

        /* b.eq else */
        0x00, 0x01, 0x00, 0x54,  /* b.eq +8 - SUSPECT! */

        /* Then: (+ 10 5) = 15 */
        0x00, 0x14, 0x80, 0xD2,  /* movz x0, #160 (10 << 4) */
        0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
        0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 (5 << 4) */
        0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
        0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
        0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */

        /* b end */
        0x08, 0x00, 0x00, 0x14,  /* b +8 */

        /* Else: (* 2 3) = 6 */
        0x00, 0x04, 0x80, 0xD2,  /* movz x0, #32 (2 << 4) */
        0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
        0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 (3 << 4) */
        0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
        0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
        0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 (untag for mul) */
        0x00, 0x7C, 0x01, 0x9B,  /* mul x0, x0, x1 */

        /* Untag result */
        0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

        /* Epilogue */
        0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    printf("Test: (if (> 8 3) (+ 10 5) (* 2 3)) - should take then and return 15\n");
    printf("Code bytes:\n");
    for (size_t i = 0; i < sizeof(code); i++) {
        printf("%02X ", code[i]);
        if ((i + 1) % 16 == 0) printf("\n");
    }
    printf("\n\n");

    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size,
                     PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT,
                     -1, 0);

    if (mem == MAP_FAILED) {
        perror("mmap");
        return 1;
    }

    memcpy(mem, code, sizeof(code));

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return 1;
    }

    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();
    munmap(mem, page_size);

    printf("Result: %lld\n", result);
    printf("Expected: 15\n");
    printf("%s\n", result == 15 ? "✓ PASS" : "✗ FAIL");

    return (result == 15) ? 0 : 1;
}
