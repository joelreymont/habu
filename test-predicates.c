/* Test nil? and zero? predicates */

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

    /* (nil? 0) → 1 */
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9, 0xFD, 0x03, 0x00, 0x91,
            0x00, 0x00, 0x80, 0xD2,  /* movz x0, #0 */
            0x1F, 0x00, 0x1F, 0xEB,  /* cmp x0, xzr */
            0xE0, 0x17, 0x9F, 0x9A,  /* cset x0, EQ */
            0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */
            0xBF, 0x03, 0x00, 0x91, 0xFD, 0x7B, 0xC1, 0xA8, 0xC0, 0x03, 0x5F, 0xD6
        };
        int64_t r = execute_bytes(code, sizeof(code));
        printf("(nil? 0) = %lld %s\n", r, r == 1 ? "✓" : "✗");
        (r == 1) ? pass++ : fail++;
    }

    /* (nil? 5) → 0 */
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9, 0xFD, 0x03, 0x00, 0x91,
            0x00, 0x0A, 0x80, 0xD2,  /* movz x0, #80 (5 << 4) */
            0x1F, 0x00, 0x1F, 0xEB,
            0xE0, 0x17, 0x9F, 0x9A,
            0x00, 0xEC, 0x7C, 0xD3,
            0x00, 0xFC, 0x44, 0xD3,
            0xBF, 0x03, 0x00, 0x91, 0xFD, 0x7B, 0xC1, 0xA8, 0xC0, 0x03, 0x5F, 0xD6
        };
        int64_t r = execute_bytes(code, sizeof(code));
        printf("(nil? 5) = %lld %s\n", r, r == 0 ? "✓" : "✗");
        (r == 0) ? pass++ : fail++;
    }

    /* (zero? 0) → 1 */
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9, 0xFD, 0x03, 0x00, 0x91,
            0x00, 0x00, 0x80, 0xD2,
            0x1F, 0x00, 0x1F, 0xEB,
            0xE0, 0x17, 0x9F, 0x9A,
            0x00, 0xEC, 0x7C, 0xD3,
            0x00, 0xFC, 0x44, 0xD3,
            0xBF, 0x03, 0x00, 0x91, 0xFD, 0x7B, 0xC1, 0xA8, 0xC0, 0x03, 0x5F, 0xD6
        };
        int64_t r = execute_bytes(code, sizeof(code));
        printf("(zero? 0) = %lld %s\n", r, r == 1 ? "✓" : "✗");
        (r == 1) ? pass++ : fail++;
    }

    /* (zero? 7) → 0 */
    {
        unsigned char code[] = {
            0xFD, 0x7B, 0xBF, 0xA9, 0xFD, 0x03, 0x00, 0x91,
            0x00, 0x0E, 0x80, 0xD2,  /* movz x0, #112 (7 << 4) */
            0x1F, 0x00, 0x1F, 0xEB,
            0xE0, 0x17, 0x9F, 0x9A,
            0x00, 0xEC, 0x7C, 0xD3,
            0x00, 0xFC, 0x44, 0xD3,
            0xBF, 0x03, 0x00, 0x91, 0xFD, 0x7B, 0xC1, 0xA8, 0xC0, 0x03, 0x5F, 0xD6
        };
        int64_t r = execute_bytes(code, sizeof(code));
        printf("(zero? 7) = %lld %s\n", r, r == 0 ? "✓" : "✗");
        (r == 0) ? pass++ : fail++;
    }

    printf("\n%d passed, %d failed\n", pass, fail);
    return fail == 0 ? 0 : 1;
}
