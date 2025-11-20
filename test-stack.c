/* Test stack operations */

#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int main() {
    /* Test stack operations with frame
     * movz x0, #5
     * str x0, [sp, #-16]!    ; Push x0 to stack
     * movz x0, #3             ; Overwrite x0
     * ldr x0, [sp], #16       ; Pop back from stack (should get 5)
     */
    unsigned char code[] = {
        /* Prologue */
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

        /* Body */
        0xA0, 0x00, 0x80, 0xD2,  /* movz x0, #5 */
        0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
        0x60, 0x00, 0x80, 0xD2,  /* movz x0, #3 */
        0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */

        /* Epilogue */
        0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    printf("Testing: Stack push/pop (should return 5)\n");
    printf("Code (%zu bytes): ", sizeof(code));
    for (size_t i = 0; i < sizeof(code); i++) {
        printf("%02X ", code[i]);
    }
    printf("\n");

    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size,
                     PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT,
                     -1, 0);

    if (mem == MAP_FAILED) {
        perror("mmap");
        return 1;
    }

    memcpy(mem, code, sizeof(code));

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return 1;
    }

    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();

    printf("Result: %lld (expected 5)\n", result);

    munmap(mem, page_size);

    if (result == 5) {
        printf("✓ PASS - Stack operations work!\n");
        return 0;
    } else {
        printf("✗ FAIL\n");
        return 1;
    }
}
