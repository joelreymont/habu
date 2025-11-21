/* Simple test for just addition */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

extern uint8_t bootstrap_add_code[];
extern const size_t bootstrap_add_size;

typedef int64_t (*binary_fn_t)(int64_t, int64_t);

int main(void) {
    printf("Testing bootstrap_add...\n");

    /* Print the bytecode */
    printf("Bytecode (%zu bytes):\n", bootstrap_add_size);
    for (size_t i = 0; i < bootstrap_add_size; i++) {
        printf("%02X ", bootstrap_add_code[i]);
        if ((i + 1) % 16 == 0) printf("\n");
    }
    printf("\n\n");

    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return 1;
    }

    memcpy(mem, bootstrap_add_code, bootstrap_add_size);

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return 1;
    }

    binary_fn_t fn = (binary_fn_t)mem;

    /* Test: 5 + 7 = 12 (tagged: 80 + 112 = 192) */
    printf("Calling add(80, 112)...\n");
    int64_t result = fn(80, 112);
    printf("Result: %lld (expected 192)\n", (long long)result);

    if (result == 192) {
        printf("✓ PASS\n");
    } else {
        printf("✗ FAIL\n");
    }

    munmap(mem, page_size);
    return (result == 192) ? 0 : 1;
}
