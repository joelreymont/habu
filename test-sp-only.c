/* Test to see what sp value we get in mmap'd code */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef uint64_t (*sp_reader_t)(void);

uint64_t get_sp_from_code(void) {
    uint8_t code[] = {
        /* mov x0, sp */
        0xE0, 0x03, 0x00, 0x91,  /* add x0, sp, #0 (mov x0, sp) */
        /* ret */
        0xC0, 0x03, 0x5F, 0xD6
    };

    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return 0;
    }

    memcpy(mem, code, sizeof(code));

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return 0;
    }

    sp_reader_t fn = (sp_reader_t)mem;
    
    uint64_t sp_in_c;
    __asm__ volatile("mov %0, sp" : "=r"(sp_in_c));
    
    uint64_t sp_in_code = fn();

    printf("SP in C before call: 0x%llx (mod 4096 = %llu)\n", 
           (unsigned long long)sp_in_c, (unsigned long long)(sp_in_c % 4096));
    printf("SP in mmap'd code:    0x%llx (mod 4096 = %llu)\n", 
           (unsigned long long)sp_in_code, (unsigned long long)(sp_in_code % 4096));
    printf("Difference: %lld bytes\n", (long long)(sp_in_c - sp_in_code));

    munmap(mem, page_size);
    return sp_in_code;
}

int main(void) {
    printf("Testing SP values\n\n");
    get_sp_from_code();
    return 0;
}
