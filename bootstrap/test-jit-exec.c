#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>
#include <pthread.h>

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

    /* Try with MAP_JIT flag for newer macOS */
    void *mem = mmap(NULL, 4096, 
                     PROT_READ | PROT_WRITE | PROT_EXEC,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, 
                     -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap with MAP_JIT");
        return 1;
    }

    printf("Successfully allocated JIT memory!\n");
    
    /* Enable write permissions with pthread_jit_write_protect_np */
    pthread_jit_write_protect_np(0);
    memcpy(mem, code, sizeof(code));
    pthread_jit_write_protect_np(1);
    
    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();

    printf("Result: %lld (expected 42)\n", result);
    munmap(mem, 4096);
    return (result == 42) ? 0 : 1;
}
