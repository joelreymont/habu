/* Integration test - conditional expressions (if) */

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

/* Helper: create if expression (if test then else) */
habu_value_t make_if(habu_value_t test, habu_value_t then_expr, habu_value_t else_expr) {
    habu_value_t if_sym = habu_intern("if");
    return habu_cons(if_sym, habu_cons(test, habu_cons(then_expr, habu_cons(else_expr, HABU_NIL))));
}

/* Helper: create comparison (op a b) */
habu_value_t make_cmp(const char *op_name, int64_t a, int64_t b) {
    habu_value_t op = habu_intern(op_name);
    habu_value_t a_val = HABU_TAG_FIXNUM(a);
    habu_value_t b_val = HABU_TAG_FIXNUM(b);
    return habu_cons(op, habu_cons(a_val, habu_cons(b_val, HABU_NIL)));
}

int test_if(const char *description, habu_value_t expr, int64_t expected) {
    printf("Test: %s\n", description);
    fflush(stdout);

    /* Compile to IR */
    habu_value_t ir = bootstrap_compile(expr);
    printf("  IR generated\n");
    fflush(stdout);

    /* Generate code */
    size_t code_size = 0;
    uint8_t *code = bootstrap_codegen(ir, &code_size);
    printf("  Generated %zu bytes of code\n", code_size);
    fflush(stdout);

    if (code_size == 0) {
        printf("  ✗ No code generated\n");
        return 0;
    }

    /* Print first few bytes */
    printf("  Code bytes: ");
    for (size_t i = 0; i < (code_size < 60 ? code_size : 60); i++) {
        printf("%02X ", code[i]);
        if (i > 0 && (i + 1) % 20 == 0) printf("\n              ");
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

    printf("\n=== Bootstrap Integration Test: If Expressions ===\n\n");

    /* Test 1: (if (= 5 5) 42 99) -> 42 */
    habu_value_t test1_cond = make_cmp("=", 5, 5);
    habu_value_t test1_then = HABU_TAG_FIXNUM(42);
    habu_value_t test1_else = HABU_TAG_FIXNUM(99);
    habu_value_t test1 = make_if(test1_cond, test1_then, test1_else);

    if (test_if("(if (= 5 5) 42 99)", test1, 42)) {
        passed++;
    } else {
        failed++;
    }

    /* Test 2: (if (= 5 7) 42 99) -> 99 */
    habu_value_t test2_cond = make_cmp("=", 5, 7);
    habu_value_t test2_then = HABU_TAG_FIXNUM(42);
    habu_value_t test2_else = HABU_TAG_FIXNUM(99);
    habu_value_t test2 = make_if(test2_cond, test2_then, test2_else);

    if (test_if("(if (= 5 7) 42 99)", test2, 99)) {
        passed++;
    } else {
        failed++;
    }

    /* Test 3: (if (< 5 10) 100 200) -> 100 */
    habu_value_t test3_cond = make_cmp("<", 5, 10);
    habu_value_t test3_then = HABU_TAG_FIXNUM(100);
    habu_value_t test3_else = HABU_TAG_FIXNUM(200);
    habu_value_t test3 = make_if(test3_cond, test3_then, test3_else);

    if (test_if("(if (< 5 10) 100 200)", test3, 100)) {
        passed++;
    } else {
        failed++;
    }

    printf("\n%d passed, %d failed\n\n", passed, failed);

    if (failed == 0) {
        printf("✓ All if tests passed!\n\n");
        return 0;
    } else {
        printf("✗ Some tests failed\n\n");
        return 1;
    }
}
