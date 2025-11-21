/* Integration test - arithmetic operations */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>
#include "../habu-minimal.h"

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

/* Forward declarations */
habu_value_t bootstrap_compile(habu_value_t expr);
uint8_t *bootstrap_codegen(habu_value_t ir, size_t *code_size);

/* Execute generated code */
typedef int64_t (*function_t)(void);

int64_t execute_code(uint8_t *code, size_t size) {
    void *mem = mmap(NULL, 4096, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        fprintf(stderr, "mmap failed\n");
        return -1;
    }

    memcpy(mem, code, size);
    mprotect(mem, 4096, PROT_READ | PROT_EXEC);

    function_t fn = (function_t)mem;
    int64_t result = fn();

    munmap(mem, 4096);
    return result;
}

/* Helper: create Lisp expression (op a b) */
habu_value_t make_binop(const char *op_name, int64_t a, int64_t b) {
    habu_value_t op = habu_intern(op_name);
    habu_value_t a_val = HABU_TAG_FIXNUM(a);
    habu_value_t b_val = HABU_TAG_FIXNUM(b);
    /* Create (op a b) */
    return habu_cons(op, habu_cons(a_val, habu_cons(b_val, HABU_NIL)));
}

int test_arithmetic(const char *op_name, int64_t a, int64_t b, int64_t expected) {
    printf("Test: (%s %lld %lld)\n", op_name, a, b);
    fflush(stdout);

    /* Create expression */
    habu_value_t expr = make_binop(op_name, a, b);
    printf("  Expression created: 0x%llX\n", expr);
    fflush(stdout);

    /* Compile to IR */
    printf("  Compiling to IR...\n");
    fflush(stdout);
    habu_value_t ir = bootstrap_compile(expr);
    printf("  IR generated: 0x%llX\n", ir);
    fflush(stdout);

    /* Generate code */
    printf("  Generating code...\n");
    fflush(stdout);
    size_t code_size = 0;
    uint8_t *code = bootstrap_codegen(ir, &code_size);
    printf("  Generated %zu bytes of code\n", code_size);
    fflush(stdout);

    if (code_size == 0) {
        printf("  ✗ No code generated\n");
        return 0;
    }

    /* Print generated code */
    printf("  Code bytes: ");
    for (size_t i = 0; i < (code_size < 40 ? code_size : 40); i++) {
        printf("%02X ", code[i]);
    }
    printf("\n");
    fflush(stdout);

    /* Execute */
    printf("  Executing...\n");
    fflush(stdout);
    int64_t result = execute_code(code, code_size);
    int64_t result_untagged = HABU_UNTAG_FIXNUM(result);
    printf("  Result: %lld (raw: 0x%llX)\n", result_untagged, result);

    /* Check */
    int64_t expected_tagged = HABU_TAG_FIXNUM(expected);
    if (result == expected_tagged) {
        printf("  ✓ PASS\n\n");
        free(code);
        return 1;
    } else {
        printf("  ✗ FAIL: expected %lld (0x%llX)\n\n", expected, expected_tagged);
        free(code);
        return 0;
    }
}

int main(void) {
    int passed = 0;
    int failed = 0;

    printf("\n=== Bootstrap Integration Test: Arithmetic ===\n\n");

    /* Test addition */
    if (test_arithmetic("+", 5, 7, 12)) {
        passed++;
    } else {
        failed++;
    }

    if (test_arithmetic("+", 10, 20, 30)) {
        passed++;
    } else {
        failed++;
    }

    /* Test subtraction */
    if (test_arithmetic("-", 10, 3, 7)) {
        passed++;
    } else {
        failed++;
    }

    if (test_arithmetic("-", 20, 5, 15)) {
        passed++;
    } else {
        failed++;
    }

    /* Test multiplication */
    if (test_arithmetic("*", 5, 6, 30)) {
        passed++;
    } else {
        failed++;
    }

    if (test_arithmetic("*", 7, 8, 56)) {
        passed++;
    } else {
        failed++;
    }

    printf("\n%d passed, %d failed\n\n", passed, failed);

    if (failed == 0) {
        printf("✓ All arithmetic tests passed!\n\n");
        return 0;
    } else {
        printf("✗ Some tests failed\n\n");
        return 1;
    }
}
