/* Minimal Habu runtime executable
 * Executes compiled Habu bytecode
 */

#include "runtime/habu.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

/* Function type for compiled Habu code */
typedef habu_value_t (*habu_compiled_fn_t)(void);

/* Execute bytecode in JIT memory */
habu_value_t execute_bytecode(const uint8_t *code, size_t code_size) {
    size_t page_size = 4096;

    /* Allocate executable memory */
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return 0;
    }

    /* Copy code */
    memcpy(mem, code, code_size);

    /* Make executable */
    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return 0;
    }

    /* Execute */
    habu_compiled_fn_t fn = (habu_compiled_fn_t)mem;
    habu_value_t result = fn();

    /* Cleanup */
    munmap(mem, page_size);

    return result;
}

/* Test program: (+ 21 21) returning tagged value */
void test_simple_arithmetic(void) {
    printf("Test: (+ 21 21)\n");
    printf("Expected: 42\n\n");

    uint8_t code[] = {
        /* Prologue: stp x29, x30, [sp, #-16]! */
        0xFD, 0x7B, 0xBF, 0xA9,

        /* Load 21 tagged (336 = 0x150) into x0: movz x0, #336 */
        0x00, 0x2A, 0x80, 0xD2,

        /* Push x0: str x0, [sp, #-16]! */
        0xE0, 0x0F, 0x1F, 0xF8,

        /* Load 21 tagged (336 = 0x150) into x0: movz x0, #336 */
        0x00, 0x2A, 0x80, 0xD2,

        /* Pop x1: ldr x1, [sp], #16 */
        0xE1, 0x07, 0x41, 0xF8,

        /* Add: add x0, x0, x1 (result is tagged) */
        0x00, 0x00, 0x01, 0x8B,

        /* Epilogue: ldp x29, x30, [sp], #16 */
        0xFD, 0x7B, 0xC1, 0xA8,

        /* Return: ret */
        0xC0, 0x03, 0x5F, 0xD6
    };

    habu_value_t result = execute_bytecode(code, sizeof(code));

    /* Untag the result */
    int64_t value = value_to_fixnum(result);

    printf("Result (tagged): 0x%llx\n", (unsigned long long)result);
    printf("Result (untagged): %lld\n", (long long)value);

    if (value == 42) {
        printf("PASS - Simple arithmetic works!\n\n");
    } else {
        printf("FAIL - Expected 42, got %lld\n\n", (long long)value);
    }
}

/* Test program: (cons 1 2) then (car cons) */
void test_cons_operations(void) {
    printf("Test: (cons 1 2) with runtime\n");
    printf("Expected: cons cell, then car=1\n\n");

    /* First create a cons cell using runtime */
    habu_value_t one = fixnum_to_value(1);
    habu_value_t two = fixnum_to_value(2);

    HABU_ROOT(cell, habu_cons(one, two));

    habu_value_t car_val = habu_car(cell);
    habu_value_t cdr_val = habu_cdr(cell);

    int64_t car_int = value_to_fixnum(car_val);
    int64_t cdr_int = value_to_fixnum(cdr_val);

    printf("cons cell created: %p\n", (void*)cell);
    printf("car: %lld (expected: 1)\n", (long long)car_int);
    printf("cdr: %lld (expected: 2)\n", (long long)cdr_int);

    if (car_int == 1 && cdr_int == 2) {
        printf("PASS - cons/car/cdr work with runtime!\n\n");
    } else {
        printf("FAIL - Expected car=1, cdr=2\n\n");
    }

    HABU_UNROOT(cell);
}

int main(void) {
    printf("=== Habu Runtime Executor ===\n\n");

    /* Initialize runtime with 1MB heap */
    habu_init(1024 * 1024);
    printf("Runtime initialized (1MB heap)\n\n");

    /* Test simple arithmetic via JIT */
    test_simple_arithmetic();

    /* Test cons operations via runtime */
    test_cons_operations();

    /* Cleanup */
    habu_shutdown();
    printf("Runtime shut down\n");

    return 0;
}
