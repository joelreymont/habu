/* Simple integration test - compile and execute a literal */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>
#include "../habu-minimal.h"

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

/* Forward declarations for compiler functions */
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

int main(void) {
    printf("\n=== Bootstrap Integration Test: Simple Literal ===\n\n");

    /* Test 1: Compile literal 42 */
    printf("Test 1: Compile literal 42\n");

    /* Create Habu expression: 42 (just a tagged fixnum) */
    habu_value_t expr = HABU_TAG_FIXNUM(42);
    printf("  Input: 42 (tagged as 0x%llX)\n", expr);

    /* Compile to IR */
    habu_value_t ir = bootstrap_compile(expr);
    printf("  IR generated: 0x%llX\n", ir);

    /* Check that IR is a list starting with 'lit */
    if (!HABU_IS_CONS(ir)) {
        printf("  ✗ IR is not a cons cell\n");
        return 1;
    }

    printf("  IR is a cons cell at 0x%llX (tag: %lld)\n", ir, ir & 0xF);

    /* Extract pointer and look at memory */
    void *ir_ptr = (void *)(ir & ~0xFLL);
    printf("  IR pointer (untagged): %p\n", ir_ptr);

    habu_value_t ir_head = habu_car(ir);
    habu_value_t ir_tail = habu_cdr(ir);
    habu_value_t lit_sym = habu_intern("lit");
    printf("  IR car: 0x%llX, cdr: 0x%llX\n", ir_head, ir_tail);
    printf("  lit symbol: 0x%llX\n", lit_sym);

    /* Check if both are symbols */
    if (!HABU_IS_SYMBOL(ir_head)) {
        printf("  ✗ IR head is not a symbol (tag: %lld)\n", ir_head & 0xF);
        return 1;
    }
    if (!HABU_IS_SYMBOL(lit_sym)) {
        printf("  ✗ lit_sym is not a symbol\n");
        return 1;
    }

    if (ir_head != lit_sym) {
        printf("  ✗ IR head is not 'lit\n");
        return 1;
    }
    printf("  ✓ IR is (lit ...)\n");

    /* Generate code */
    size_t code_size = 0;
    uint8_t *code = bootstrap_codegen(ir, &code_size);
    printf("  Generated %zu bytes of code\n", code_size);

    if (code_size == 0) {
        printf("  ✗ No code generated\n");
        return 1;
    }

    /* Print first few bytes */
    printf("  Code bytes: ");
    for (int i = 0; i < (code_size < 20 ? code_size : 20); i++) {
        printf("%02X ", code[i]);
    }
    printf("\n");

    /* Execute code */
    int64_t result = execute_code(code, code_size);
    printf("  Executed, result: %lld (0x%llX)\n", result, result);

    /* Check result */
    int64_t expected = HABU_TAG_FIXNUM(42);
    if (result == expected) {
        printf("  ✓ Result matches expected value %lld\n\n", expected);
        printf("✓ Integration test passed!\n\n");
        free(code);
        return 0;
    } else {
        printf("  ✗ Result %lld != expected %lld\n\n", result, expected);
        printf("✗ Integration test failed\n\n");
        free(code);
        return 1;
    }
}
