/* Test direct machine code execution (like SBCL does) */
#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

typedef int64_t (*habu_fn_t)(void);

int main() {
    unsigned char code[] = {
        0x00, 0x54, 0xAA, 0xD2,  /* movz x0, #672 */
        0x00, 0x10, 0x44, 0xD3,  /* lsr x0, x0, #4 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    void *mem = mmap(NULL, 4096, PROT_READ | PROT_WRITE | PROT_EXEC,
                     MAP_PRIVATE | MAP_ANON, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return 1;
    }

    memcpy(mem, code, sizeof(code));
    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();

    printf("Result: %lld (expected 42)\n", result);
    munmap(mem, 4096);
    return (result == 42) ? 0 : 1;
}
