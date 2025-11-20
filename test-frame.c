/* Test frame prologue/epilogue */

#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int main() {
    /* Test just prologue, simple operation, epilogue */
    unsigned char code[] = {
        /* Prologue - save frame */
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp (add x29, sp, #0) */

        /* Body - just return 42 */
        0x40, 0x05, 0x80, 0xD2,  /* movz x0, #42 */

        /* Epilogue - restore frame */
        0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 (add sp, x29, #0) */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    printf("Testing: Frame prologue/epilogue with movz x0, #42\n");
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

    printf("Result: %lld (expected 42)\n", result);

    munmap(mem, page_size);

    if (result == 42) {
        printf("✓ PASS - Frame setup/teardown works!\n");
        return 0;
    } else {
        printf("✗ FAIL\n");
        return 1;
    }
}
