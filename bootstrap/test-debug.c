#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int main() {
    unsigned char code[] = {
        0x00, 0x54, 0xAA, 0xD2,  /* movz x0, #672 */
        0x00, 0x10, 0x44, 0xD3,  /* lsr x0, x0, #4 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    printf("Original code:\n");
    for (int i = 0; i < sizeof(code); i++) {
        printf("%02X ", code[i]);
    }
    printf("\n");

    void *mem = mmap(NULL, 4096, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return 1;
    }

    memcpy(mem, code, sizeof(code));
    
    printf("Code in memory:\n");
    unsigned char *p = (unsigned char *)mem;
    for (int i = 0; i < sizeof(code); i++) {
        printf("%02X ", p[i]);
    }
    printf("\n");
    
    if (mprotect(mem, 4096, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        return 1;
    }
    
    printf("Calling function...\n");
    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();

    printf("Result: %lld (expected 42)\n", result);
    munmap(mem, 4096);
    return (result == 42) ? 0 : 1;
}
