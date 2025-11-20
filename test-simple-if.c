/* Simple If Test - Debug version */

#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int main() {
    /* Test: if 1 then return 42 else return 99
     * Since test is 1 (nonzero), should return 42
     */
    unsigned char code[] = {
        /* Frame */
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

        /* Test: load 1 (nonzero/truthy) */
        0x00, 0x02, 0x80, 0xD2,  /* movz x0, #16 (1 << 4) */

        /* cmp x0, xzr - is it zero? */
        0x1F, 0x00, 0x1F, 0xEB,  /* cmp x0, xzr */

        /* If zero, skip to else (+3 instructions from b.eq itself) */
        0x60, 0x00, 0x00, 0x54,  /* b.eq +3 */

        /* Then branch: return 42 */
        0x00, 0x54, 0x80, 0xD2,  /* movz x0, #672 (42 << 4) */

        /* Skip else branch (+2 instructions from b itself) */
        0x02, 0x00, 0x00, 0x14,  /* b +2 */

        /* Else branch: return 99 */
        0x00, 0xE6, 0x80, 0xD2,  /* movz x0, #1584 (99 << 4) */

        /* Untag result before returning */
        0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

        /* Restore frame and return */
        0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    size_t len = sizeof(code);
    printf("Code size: %zu bytes\n", len);
    printf("Bytes: ");
    for (size_t i = 0; i < len; i++) {
        printf("%02X ", code[i]);
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

    memcpy(mem, code, len);

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return 1;
    }

    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();
    munmap(mem, page_size);

    printf("Result: %lld\n", result);
    printf("Expected: 42\n");
    printf("%s\n", result == 42 ? "✓ PASS" : "✗ FAIL");

    return (result == 42) ? 0 : 1;
}
