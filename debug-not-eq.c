#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int main() {
    /* (not (= 5 3)) should return 1 (true) */
    unsigned char code[] = {
        0xFD, 0x7B, 0xBF, 0xA9,
        0xFD, 0x03, 0x00, 0x91,

        /* (= 5 3) */
        0x00, 0x0A, 0x80, 0xD2,
        0xE0, 0x0F, 0x1F, 0xF8,
        0x00, 0x06, 0x80, 0xD2,
        0xE1, 0x03, 0x00, 0xAA,
        0xE0, 0x07, 0x41, 0xF8,
        0x1F, 0x00, 0x01, 0xEB,
        0xE0, 0x07, 0x9F, 0x9A,
        0x00, 0xEC, 0x7C, 0xD3,

        /* NOT */
        0x1F, 0x00, 0x1F, 0xEB,
        0xE0, 0x17, 0x9F, 0x9A,
        0x00, 0xEC, 0x7C, 0xD3,

        /* Untag */
        0x00, 0xFC, 0x44, 0xD3,

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

    printf("(not (= 5 3)) = %lld (expected 1) %s\n", result,
           result == 1 ? "✓" : "✗");
    return result == 1 ? 0 : 1;
}
