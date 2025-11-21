/* Debug stack pointer issues */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*fn_t)(int64_t);

int64_t execute_code(const uint8_t *code, size_t size, int64_t input) {
    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return -999;
    }

    memcpy(mem, code, size);

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return -999;
    }

    fn_t fn = (fn_t)mem;
    
    // Read sp before calling
    register uint64_t sp_before asm("sp");
    printf("  SP before call: %p\n", (void*)sp_before);
    printf("  Code address: %p\n", mem);
    printf("  Would write to: %p (sp-16)\n", (void*)(sp_before - 16));
    
    int64_t result = fn(input);

    munmap(mem, page_size);
    return result;
}

int main(void) {
    printf("Test: Check stack pointer alignment\n");

    uint8_t code[] = {
        /* Just return x0 without modifying stack */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    int64_t result = execute_code(code, sizeof(code), 42);
    printf("  Result: %lld\n", (long long)result);
    
    return 0;
}
