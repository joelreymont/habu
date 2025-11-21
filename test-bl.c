/* Test BL (branch and link) instruction */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*main_fn_t)(void);

int64_t execute_code(const uint8_t *code, size_t size) {
    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return -1;
    }

    memcpy(mem, code, size);

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return -1;
    }

    main_fn_t fn = (main_fn_t)mem;
    int64_t result = fn();

    munmap(mem, page_size);
    return result;
}

void test_bl(void) {
    printf("Test: BL (call subroutine that returns 42)\n");

    uint8_t code[] = {
        /* main: */
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        
        /* Call subroutine (BL +3 to skip 3 instructions) */
        0x03, 0x00, 0x00, 0x94,  /* bl +3 */
        
        /* Return */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6,  /* ret */
        
        /* subroutine: */
        0x40, 0x05, 0x80, 0xD2,  /* movz x0, #42 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    int64_t result = execute_code(code, sizeof(code));

    printf("  Result: %lld\n", (long long)result);
    if (result == 42) {
        printf("  PASS - BL works!\n\n");
    } else {
        printf("  FAIL (expected 42, got %lld)\n\n", (long long)result);
    }
}

int main(void) {
    printf("=== BL Instruction Test ===\n\n");
    test_bl();
    return 0;
}
