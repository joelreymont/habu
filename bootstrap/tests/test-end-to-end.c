/* End-to-End Test: Parse → Compile → Execute
 *
 * Tests the complete pipeline from text source to execution:
 * 1. Parse Lisp text with habu_read
 * 2. Compile to IR with bootstrap_compile
 * 3. Generate ARM64 code with bootstrap_codegen
 * 4. Execute and verify results
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/mman.h>
#include "../habu-minimal.h"

/* Compiler functions */
extern habu_value_t bootstrap_compile(habu_value_t expr);
extern uint8_t *bootstrap_codegen(habu_value_t ir, size_t *code_size);

/* Execute generated code */
static int64_t execute_code(uint8_t *code, size_t code_size) {
    void *mem = mmap(NULL, code_size,
                     PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (mem == MAP_FAILED) {
        fprintf(stderr, "mmap failed\n");
        return -1;
    }

    memcpy(mem, code, code_size);

    if (mprotect(mem, code_size, PROT_READ | PROT_EXEC) != 0) {
        fprintf(stderr, "mprotect failed\n");
        munmap(mem, code_size);
        return -1;
    }

    int64_t (*func)(void) = (int64_t (*)(void))mem;
    int64_t result = func();

    munmap(mem, code_size);
    return result;
}

/* Helper to run complete pipeline */
static int64_t compile_and_run(const char *source) {
    // 1. Parse
    habu_value_t expr = habu_read(source);

    // 2. Compile to IR
    habu_value_t ir = bootstrap_compile(expr);

    // 3. Generate code
    size_t code_size;
    uint8_t *code = bootstrap_codegen(ir, &code_size);

    // 4. Execute (returns tagged value)
    int64_t tagged_result = execute_code(code, code_size);

    free(code);

    // 5. Untag fixnum result
    if (HABU_IS_FIXNUM(tagged_result)) {
        return HABU_UNTAG_FIXNUM(tagged_result);
    }

    // For non-fixnum results (comparisons return untagged 0/1)
    return tagged_result;
}

void test_literal() {
    printf("Test: literal from text\n");
    const char *source = "42";
    printf("  Source: \"%s\"\n", source);

    int64_t result = compile_and_run(source);
    printf("  Result: %lld\n", result);

    assert(result == 42);
    printf("  ✓ Correct result\n\n");
}

void test_addition() {
    printf("Test: addition from text\n");
    const char *source = "(+ 5 7)";
    printf("  Source: \"%s\"\n", source);

    int64_t result = compile_and_run(source);
    printf("  Result: %lld\n", result);

    assert(result == 12);
    printf("  ✓ Correct result\n\n");
}

void test_multiplication() {
    printf("Test: multiplication from text\n");
    const char *source = "(* 6 7)";
    printf("  Source: \"%s\"\n", source);

    int64_t result = compile_and_run(source);
    printf("  Result: %lld\n", result);

    assert(result == 42);
    printf("  ✓ Correct result\n\n");
}

void test_nested_expression() {
    printf("Test: nested expression from text\n");
    const char *source = "(* (+ 3 4) 5)";
    printf("  Source: \"%s\"\n", source);

    int64_t result = compile_and_run(source);
    printf("  Result: %lld\n", result);

    assert(result == 35);
    printf("  ✓ Correct result\n\n");
}

void test_comparison_equal() {
    printf("Test: equality comparison from text\n");
    const char *source = "(= 10 10)";
    printf("  Source: \"%s\"\n", source);

    int64_t result = compile_and_run(source);
    printf("  Result: %lld\n", result);

    assert(result == 1);
    printf("  ✓ Correct result (true)\n\n");
}

void test_comparison_less() {
    printf("Test: less than comparison from text\n");
    const char *source = "(< 5 10)";
    printf("  Source: \"%s\"\n", source);

    int64_t result = compile_and_run(source);
    printf("  Result: %lld\n", result);

    assert(result == 1);
    printf("  ✓ Correct result (true)\n\n");
}

void test_if_true() {
    printf("Test: if expression (true branch) from text\n");
    const char *source = "(if (= 5 5) 42 99)";
    printf("  Source: \"%s\"\n", source);

    int64_t result = compile_and_run(source);
    printf("  Result: %lld\n", result);

    assert(result == 42);
    printf("  ✓ Correct result (then branch)\n\n");
}

void test_if_false() {
    printf("Test: if expression (false branch) from text\n");
    const char *source = "(if (< 10 5) 42 99)";
    printf("  Source: \"%s\"\n", source);

    int64_t result = compile_and_run(source);
    printf("  Result: %lld\n", result);

    assert(result == 99);
    printf("  ✓ Correct result (else branch)\n\n");
}

void test_complex_nested() {
    printf("Test: complex nested expression from text\n");
    const char *source = "(if (< 5 10) (* 10 10) (+ 1 1))";
    printf("  Source: \"%s\"\n", source);

    int64_t result = compile_and_run(source);
    printf("  Result: %lld\n", result);

    assert(result == 100);
    printf("  ✓ Correct result\n\n");
}

int main(void) {
    printf("===== End-to-End Pipeline Tests =====\n");
    printf("Text → Parse → Compile → Execute\n\n");

    test_literal();
    test_addition();
    test_multiplication();
    test_nested_expression();
    test_comparison_equal();
    test_comparison_less();
    test_if_true();
    test_if_false();
    test_complex_nested();

    printf("All end-to-end tests passed! ✓\n");
    printf("\nBootstrap compiler can now:\n");
    printf("  - Parse Lisp source text\n");
    printf("  - Compile to intermediate representation\n");
    printf("  - Generate ARM64 machine code\n");
    printf("  - Execute the generated code\n");
    printf("\nReady for self-hosting!\n");

    return 0;
}
