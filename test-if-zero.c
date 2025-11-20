/* Debug test 2: (if 0 42 99) should return 99 */

#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int main() {
    /* Test is 0 (false), should take else branch and return 99 */
    unsigned char code[] = {
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

        /* Test: 0 */
        0x00, 0x00, 0x80, 0xD2,  /* movz x0, #0 */

        /* cmp x0, xzr */
        0x1F, 0x00, 0x1F, 0xEB,

        /* b.eq +3 (should branch since 0 == 0) */
        0x60, 0x00, 0x00, 0x54,

        /* Then: 42 (should be skipped) */
        0x00, 0x54, 0x80, 0xD2,  /* movz x0, #672 */

        /* b +2 (should be skipped) */
        0x02, 0x00, 0x00, 0x14,

        /* Else: 99 (should execute) */
        0x00, 0xC6, 0x80, 0xD2,  /* movz x0, #1584 (99 << 4) */

        /* Untag */
        0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

        0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    printf("Test: (if 0 42 99) - should branch to else and return 99\n");
    printf("Code bytes: ");
    for (size_t i = 0; i < sizeof(code); i++) {
        printf("%02X ", code[i]);
        if ((i + 1) % 16 == 0) printf("\n            ");
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
    printf("Expected: 99\n");
    printf("%s\n", result == 99 ? "✓ PASS" : "✗ FAIL");

    return (result == 99) ? 0 : 1;
}
