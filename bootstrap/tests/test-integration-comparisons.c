/* Integration test - comparison operations */

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

int test_comparison(const char *op_name, int64_t a, int64_t b, int expected) {
    printf("Test: (%s %lld %lld)\n", op_name, a, b);
    fflush(stdout);

    /* Create expression */
    habu_value_t expr = make_binop(op_name, a, b);

    /* Compile to IR */
    habu_value_t ir = bootstrap_compile(expr);

    /* Generate code */
    size_t code_size = 0;
    uint8_t *code = bootstrap_codegen(ir, &code_size);
    printf("  Generated %zu bytes of code\n", code_size);

    if (code_size == 0) {
        printf("  ✗ No code generated\n");
        return 0;
    }

    /* Execute */
    int64_t result = execute_code(code, code_size);
    printf("  Result: %lld (raw: 0x%llX)\n", result, result);

    /* Check - comparisons return 0 or 1 (not tagged) */
    if (result == expected) {
        printf("  ✓ PASS\n\n");
        free(code);
        return 1;
    } else {
        printf("  ✗ FAIL: expected %d\n\n", expected);
        free(code);
        return 0;
    }
}

int main(void) {
    int passed = 0;
    int failed = 0;

    printf("\n=== Bootstrap Integration Test: Comparisons ===\n\n");

    /* Test equality */
    printf("Testing = (equality):\n");
    if (test_comparison("=", 5, 5, 1)) {
        passed++;
    } else {
        failed++;
    }

    if (test_comparison("=", 5, 7, 0)) {
        passed++;
    } else {
        failed++;
    }

    if (test_comparison("=", 10, 10, 1)) {
        passed++;
    } else {
        failed++;
    }

    /* Test less than */
    printf("\nTesting < (less than):\n");
    if (test_comparison("<", 5, 7, 1)) {
        passed++;
    } else {
        failed++;
    }

    if (test_comparison("<", 7, 5, 0)) {
        passed++;
    } else {
        failed++;
    }

    if (test_comparison("<", 10, 10, 0)) {
        passed++;
    } else {
        failed++;
    }

    /* Test greater than */
    printf("\nTesting > (greater than):\n");
    if (test_comparison(">", 7, 5, 1)) {
        passed++;
    } else {
        failed++;
    }

    if (test_comparison(">", 5, 7, 0)) {
        passed++;
    } else {
        failed++;
    }

    if (test_comparison(">", 10, 10, 0)) {
        passed++;
    } else {
        failed++;
    }

    printf("\n%d passed, %d failed\n\n", passed, failed);

    if (failed == 0) {
        printf("✓ All comparison tests passed!\n\n");
        return 0;
    } else {
        printf("✗ Some tests failed\n\n");
        return 1;
    }
}
