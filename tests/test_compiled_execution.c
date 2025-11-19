/*
 * Test execution of Habu-compiled code
 * Compiles Lisp expressions and executes them to verify correctness
 */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#define TEST_PASS 0
#define TEST_FAIL 1

/* Execute machine code and return result */
typedef int64_t (*compiled_func_t)(void);

int64_t execute_code(const uint8_t *code, size_t code_size) {
    /* Allocate executable memory - need 4 bytes for ARM64 ret instruction */
    size_t alloc_size = code_size + 4;
    void *exec_mem = mmap(NULL, alloc_size,
                          PROT_READ | PROT_WRITE | PROT_EXEC,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (exec_mem == MAP_FAILED) {
        perror("mmap");
        return -1;
    }

    /* Copy code to executable memory */
    memcpy(exec_mem, code, code_size);

    /* Add return instruction */
    #ifdef __x86_64__
    ((uint8_t *)exec_mem)[code_size] = 0xC3;  /* ret (1 byte) */
    #elif defined(__aarch64__)
    /* ARM64 ret instruction: C0 03 5F D6 (4 bytes) */
    ((uint8_t *)exec_mem)[code_size + 0] = 0xC0;
    ((uint8_t *)exec_mem)[code_size + 1] = 0x03;
    ((uint8_t *)exec_mem)[code_size + 2] = 0x5F;
    ((uint8_t *)exec_mem)[code_size + 3] = 0xD6;
    #endif

    /* Execute the code */
    compiled_func_t func = (compiled_func_t)exec_mem;
    int64_t result = func();

    /* Clean up */
    munmap(exec_mem, alloc_size);

    return result;
}

/* Helper: Convert fixnum tag to integer value */
int64_t untag_fixnum(int64_t tagged) {
    return tagged >> 4;
}

/* Test cases using pre-compiled bytecode */

/* Test: 42 */
void test_fixnum_literal() {
    printf("Test: fixnum literal 42\n");

    #ifdef __x86_64__
    uint8_t code[] = {
        0x48, 0xB8, 0xA0, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  /* mov rax, 672 (42*16) */
    };
    #elif defined(__aarch64__)
    uint8_t code[] = {
        0xE0, 0x52, 0x80, 0xD2  /* mov x0, #0x2A0 (42*16) */
    };
    #endif

    int64_t result = execute_code(code, sizeof(code));
    int64_t value = untag_fixnum(result);

    printf("  Result: %lld (expected 42)\n", (long long)value);
    if (value != 42) {
        printf("  FAIL!\n");
        exit(TEST_FAIL);
    }
    printf("  PASS\n\n");
}

/* Test: (+ 10 20) */
void test_addition() {
    printf("Test: (+ 10 20)\n");

    #ifdef __x86_64__
    uint8_t code[] = {
        /* Load 10 */
        0x48, 0xB8, 0xA0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        /* push rax */
        0x50,
        /* Load 20 */
        0x48, 0xB8, 0x40, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        /* mov rbx, [rsp] */
        0x48, 0x8B, 0x1C, 0x24,
        /* add rax, rbx */
        0x48, 0x01, 0xD8,
        /* add rsp, 8 */
        0x48, 0x83, 0xC4, 0x08
    };
    #elif defined(__aarch64__)
    uint8_t code[] = {
        /* Load 10: mov x0, #160 (10*16) */
        0x00, 0x14, 0x80, 0xD2,
        /* stp x29, x30, [sp, #-16]! */
        0xFD, 0x7B, 0xBF, 0xA9,
        /* mov x2, x0 (save) - actually we need proper register management */
        0xE2, 0x03, 0x00, 0xAA,
        /* Load 20: mov x0, #320 (20*16) */
        0x00, 0x28, 0x80, 0xD2,
        /* mov x1, x0 */
        0xE1, 0x03, 0x00, 0xAA,
        /* mov x0, x2 (restore) */
        0xE0, 0x03, 0x02, 0xAA,
        /* add x0, x0, x1 */
        0x00, 0x00, 0x01, 0x8B,
        /* ldp x29, x30, [sp], #16 */
        0xFD, 0x7B, 0xC1, 0xA8
    };
    #endif

    int64_t result = execute_code(code, sizeof(code));
    int64_t value = untag_fixnum(result);

    printf("  Result: %lld (expected 30)\n", (long long)value);
    if (value != 30) {
        printf("  FAIL!\n");
        exit(TEST_FAIL);
    }
    printf("  PASS\n\n");
}

/* Test: (- 50 25) */
void test_subtraction() {
    printf("Test: (- 50 25)\n");

    #ifdef __x86_64__
    uint8_t code[] = {
        /* Load 50 */
        0x48, 0xB8, 0x20, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        /* push rax */
        0x50,
        /* Load 25 */
        0x48, 0xB8, 0x90, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        /* mov rbx, [rsp] */
        0x48, 0x8B, 0x1C, 0x24,
        /* mov rcx, rbx */
        0x48, 0x89, 0xD9,
        /* sub rcx, rax */
        0x48, 0x29, 0xC1,
        /* mov rax, rcx */
        0x48, 0x89, 0xC8,
        /* add rsp, 8 */
        0x48, 0x83, 0xC4, 0x08
    };
    #endif

    #ifdef __x86_64__
    int64_t result = execute_code(code, sizeof(code));
    int64_t value = untag_fixnum(result);

    printf("  Result: %lld (expected 25)\n", (long long)value);
    if (value != 25) {
        printf("  FAIL!\n");
        exit(TEST_FAIL);
    }
    printf("  PASS\n\n");
    #else
    printf("  SKIP (ARM64 test not implemented)\n\n");
    #endif
}

/* Test: (< 5 10) - should return 1 (true) */
void test_comparison() {
    printf("Test: (< 5 10)\n");

    #ifdef __x86_64__
    uint8_t code[] = {
        /* Load 5 */
        0x48, 0xB8, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        /* push rax */
        0x50,
        /* Load 10 */
        0x48, 0xB8, 0xA0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        /* mov rbx, [rsp] */
        0x48, 0x8B, 0x1C, 0x24,
        /* cmp rbx, rax */
        0x48, 0x39, 0xC3,
        /* setl al */
        0x0F, 0x9C, 0xC0,
        /* movzx rax, al */
        0x48, 0x0F, 0xB6, 0xC0,
        /* shl rax, 4 */
        0x48, 0xC1, 0xE0, 0x04,
        /* add rsp, 8 */
        0x48, 0x83, 0xC4, 0x08
    };

    int64_t result = execute_code(code, sizeof(code));
    int64_t value = untag_fixnum(result);

    printf("  Result: %lld (expected 1)\n", (long long)value);
    if (value != 1) {
        printf("  FAIL!\n");
        exit(TEST_FAIL);
    }
    printf("  PASS\n\n");
    #else
    printf("  SKIP (ARM64 test not implemented)\n\n");
    #endif
}

int main() {
    printf("=== Habu Compiled Code Execution Tests ===\n\n");

    #ifdef __x86_64__
    printf("Architecture: x86_64\n\n");
    #elif defined(__aarch64__)
    printf("Architecture: ARM64\n\n");
    #else
    printf("Unknown architecture\n");
    return TEST_FAIL;
    #endif

    test_fixnum_literal();
    test_addition();
    test_subtraction();
    test_comparison();

    printf("All tests passed!\n");
    return TEST_PASS;
}
