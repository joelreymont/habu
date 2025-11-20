/* Test cond expression */

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

    /* (cond ((> 5 3) 100) ((< 5 3) 200)) → 100 */
    printf("\n=== Test 1: (cond ((> 5 3) 100) ((< 5 3) 200)) ===\n");
    printf("Expected: 100\n");
    {
        /* This will be filled in by the compiler */
        printf("Test code not yet generated\n");
    }

    /* (cond ((< 5 3) 100) ((> 5 3) 200)) → 200 */
    printf("\n=== Test 2: (cond ((< 5 3) 100) ((> 5 3) 200)) ===\n");
    printf("Expected: 200\n");
    {
        printf("Test code not yet generated\n");
    }

    /* (cond ((= 5 3) 100) ((= 3 3) 200) ((= 4 4) 300)) → 200 */
    printf("\n=== Test 3: (cond ((= 5 3) 100) ((= 3 3) 200) ((= 4 4) 300)) ===\n");
    printf("Expected: 200\n");
    {
        printf("Test code not yet generated\n");
    }

    /* (cond ((< 5 3) 100) ((< 2 1) 200)) → 0 (no match) */
    printf("\n=== Test 4: (cond ((< 5 3) 100) ((< 2 1) 200)) ===\n");
    printf("Expected: 0\n");
    {
        printf("Test code not yet generated\n");
    }

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");

    return (fail == 0) ? 0 : 1;
}
