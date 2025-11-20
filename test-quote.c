/* Test QUOTE */
#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int main() {
    /* (quote 42) → 42 */
    unsigned char code[] = {
        0xFD, 0x7B, 0xBF, 0xA9,
        0xFD, 0x03, 0x00, 0x91,
        0x00, 0x54, 0x80, 0xD2,  /* movz x0, #672 (42 << 4) */
        0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */
        0xBF, 0x03, 0x00, 0x91,
        0xFD, 0x7B, 0xC1, 0xA8,
        0xC0, 0x03, 0x5F, 0xD6
    };

    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) { perror("mmap"); return 1; }

    memcpy(mem, code, sizeof(code));
    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect"); munmap(mem, page_size); return 1;
    }

    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();
    munmap(mem, page_size);

    printf("(quote 42) = %lld %s\n", result, result == 42 ? "✓" : "✗");
    return result == 42 ? 0 : 1;
}
