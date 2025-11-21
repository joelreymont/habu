/* Test the same code but with n=0 (no recursion) */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*fn_t)(int64_t);

/* Wrapper with guaranteed stack space */
__attribute__((noinline))
static int64_t call_with_stack(fn_t fn, int64_t input) {
    /* Allocate a large stack buffer to ensure sp is not at a page boundary */
    volatile char stack_buffer[1024];
    stack_buffer[0] = 0;  /* Touch it to prevent optimization */

    uint64_t sp;
    __asm__ volatile("mov %0, sp" : "=r"(sp));
    printf("  SP in call_with_stack before fn call: 0x%llx\n", (unsigned long long)sp);
    printf("  SP mod 4096: %llu\n", (unsigned long long)(sp % 4096));

    return fn(input);
}

int64_t execute_code(const uint8_t *code, size_t size, int64_t input) {
    /* Allocate stack buffer FIRST to move sp away from page boundary */
    volatile char guard_buffer[256];
    guard_buffer[0] = 0;

    printf("  execute_code: start\n");
    fflush(stdout);

    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return -999;
    }

    printf("  execute_code: mmap ok, mem=%p\n", mem);
    fflush(stdout);

    memcpy(mem, code, size);

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return -999;
    }

    printf("  execute_code: about to call\n");
    fflush(stdout);

    fn_t fn = (fn_t)mem;
    int64_t result = call_with_stack(fn, input);

    munmap(mem, page_size);
    return result;
}

void test_base_case(void) {
    printf("Test: Base case (n=0, no recursion)\n");

    uint8_t code[] = {
        /* Prologue */
        0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
        
        /* if (x0 == 0) return 0 */
        0x1F, 0x00, 0x00, 0xF1,  /* cmp x0, #0 */
        0x41, 0x00, 0x00, 0x54,  /* b.ne +2 */
        0x02, 0x00, 0x00, 0x14,  /* b +2 (to epilogue) */
        
        /* recursive: (never reached when x0=0) */
        0x00, 0x04, 0x00, 0xD1,  /* sub x0, x0, #1 */
        0xFB, 0xFF, 0xFF, 0x97,  /* bl -5 */
        
        /* epilogue: */
        0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
        0xC0, 0x03, 0x5F, 0xD6   /* ret */
    };

    int64_t result = execute_code(code, sizeof(code), 0);

    printf("  Result: %lld\n", (long long)result);
    if (result == 0) {
        printf("  PASS\n\n");
    } else {
        printf("  FAIL (expected 0, got %lld)\n\n", (long long)result);
    }
}

int main(void) {
    printf("=== Base Case Test ===\n\n");
    test_base_case();
    return 0;
}
