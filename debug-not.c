#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int main() {
    /* Test: (not 1) should return 0 */
    unsigned char code[] = {
        0xFD, 0x7B, 0xBF, 0xA9,
        0xFD, 0x03, 0x00, 0x91,

        0x00, 0x02, 0x80, 0xD2,  /* movz x0, #16 (1 << 4) */

        0x1F, 0x00, 0x1F, 0xEB,  /* cmp x0, xzr */
        0xE0, 0x17, 0x9F, 0x9A,  /* cset x0, EQ */
        0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */

        0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 (untag) */

        0xBF, 0x03, 0x00, 0x91,
        0xFD, 0x7B, 0xC1, 0xA8,
        0xC0, 0x03, 0x5F, 0xD6
    };

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

    printf("Result: %lld (expected 0)\n", result);
    return 0;
}
