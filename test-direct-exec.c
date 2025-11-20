/* Test direct machine code execution (like SBCL does) */

#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

typedef int64_t (*habu_fn_t)(void);

int main() {
    /* ARM64 machine code: movz x0, #672; lsr x0, x0, #4; ret */
    unsigned char code[] = {
        0x00, 0x54, 0xAA, 0xD2,  /* movz x0, #672 (42 << 4) */
        0x00, 0x10, 0x44, 0xD3,  /* lsr x0, x0, #4 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    size_t code_size = sizeof(code);
    size_t page_size = 4096;
    size_t aligned_size = ((code_size + page_size - 1) / page_size) * page_size;

    printf("Allocating executable memory...\n");

    /* Allocate executable memory (like SBCL does) */
    void *mem = mmap(NULL, aligned_size,
                     PROT_READ | PROT_WRITE | PROT_EXEC,
                     MAP_PRIVATE | MAP_ANON,
                     -1, 0);

    if (mem == MAP_FAILED) {
        perror("mmap");
        return 1;
    }

    printf("Copying code to executable memory...\n");

    /* Copy code to executable memory */
    memcpy(mem, code, code_size);

    printf("Executing generated code...\n");

    /* Cast to function pointer and call (like SBCL does) */
    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();

    printf("Result: %lld\n", result);
    printf("Expected: 42\n");

    /* Clean up */
    munmap(mem, aligned_size);

    if (result == 42) {
        printf("✓ SUCCESS! Direct machine code execution works!\n");
        return 0;
    } else {
        printf("✗ FAILED\n");
        return 1;
    }
}
