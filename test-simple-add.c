/* Test simple addition without full frame */

#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int main() {
    /* Test: movz x0, #48; movz x1, #64; add x0, x0, x1; lsr x0, x0, #4; ret
     * Should compute (3 << 4) + (4 << 4) = 112, then >> 4 = 7
     */
    unsigned char code[] = {
        0x60, 0x00, 0x80, 0xD2,  /* movz x0, #3 */
        0x81, 0x00, 0x80, 0xD2,  /* movz x1, #4 */
        0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    printf("Testing: 3 + 4 (no tagging)\n");
    printf("Code: ");
    for (size_t i = 0; i < sizeof(code); i++) {
        printf("%02X ", code[i]);
    }
    printf("\n");

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

    printf("Result: %lld (expected 7)\n", result);

    munmap(mem, page_size);

    if (result == 7) {
        printf("✓ PASS\n");
        return 0;
    } else {
        printf("✗ FAIL\n");
        return 1;
    }
}
