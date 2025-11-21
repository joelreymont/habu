/* Debug stack pointer issues with inline asm */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*fn_t)(int64_t);

uint64_t read_sp(void) {
    uint64_t sp;
    __asm__ volatile("mov %0, sp" : "=r"(sp));
    return sp;
}

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
    
    uint64_t sp_before = read_sp();
    printf("  SP before call: 0x%llx\n", (unsigned long long)sp_before);
    printf("  Code address: %p\n", mem);
    printf("  Would write to: 0x%llx (sp-16)\n", (unsigned long long)(sp_before - 16));
    printf("  SP mod 4096: %llu\n", (unsigned long long)(sp_before % 4096));
    
    int64_t result = fn(input);

    munmap(mem, page_size);
    return result;
}

int main(void) {
    printf("Test: Check stack pointer before stack frame setup\n\n");

    uint8_t code[] = {
        /* Just return x0 without modifying stack */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    int64_t result = execute_code(code, sizeof(code), 42);
    printf("  Result: %lld\n\n", (long long)result);
    
    printf("Now test with stack frame:\n\n");
    
    uint8_t code2[] = {
        /* Try to save x29, x30 */
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };
    
    result = execute_code(code2, sizeof(code2), 42);
    printf("  Result: %lld\n", (long long)result);
    
    return 0;
}
