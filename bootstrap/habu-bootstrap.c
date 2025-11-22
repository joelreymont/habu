/* Habu Bootstrap Compiler - Main Driver
 *
 * Minimal bootstrap compiler that can compile and execute
 * simple Habu expressions.
 *
 * Usage: ./habu-bootstrap
 *
 * This demonstrates the full compilation pipeline:
 * 1. Parse Habu expression (manual construction for now)
 * 2. Compile to IR
 * 3. Generate ARM64 machine code
 * 4. Execute and return result
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>
#include "habu-minimal.h"

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

/* Forward declarations from compilation pipeline */
extern habu_value_t bootstrap_compile(habu_value_t expr);
extern uint8_t *bootstrap_codegen(habu_value_t ir, size_t *code_size);

/* Execute generated code */
typedef int64_t (*compiled_fn_t)(void);

int64_t execute_code(uint8_t *code, size_t size) {
    void *mem = mmap(NULL, 4096, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        fprintf(stderr, "Error: mmap failed\n");
        return -1;
    }

    memcpy(mem, code, size);

    if (mprotect(mem, 4096, PROT_READ | PROT_EXEC) != 0) {
        fprintf(stderr, "Error: mprotect failed\n");
        munmap(mem, 4096);
        return -1;
    }

    compiled_fn_t fn = (compiled_fn_t)mem;
    int64_t result = fn();

    munmap(mem, 4096);
    return result;
}

/* Helper: Build expressions */
habu_value_t make_binop(const char *op, habu_value_t a, habu_value_t b) {
    habu_value_t op_sym = habu_intern(op);
    return habu_cons(op_sym, habu_cons(a, habu_cons(b, HABU_NIL)));
}

habu_value_t make_if(habu_value_t test, habu_value_t then_expr, habu_value_t else_expr) {
    habu_value_t if_sym = habu_intern("if");
    return habu_cons(if_sym, habu_cons(test, habu_cons(then_expr, habu_cons(else_expr, HABU_NIL))));
}

/* Main compilation function */
int64_t compile_and_run(habu_value_t expr) {
    /* Step 1: Compile to IR */
    habu_value_t ir = bootstrap_compile(expr);

    /* Step 2: Generate code */
    size_t code_size = 0;
    uint8_t *code = bootstrap_codegen(ir, &code_size);

    if (code_size == 0) {
        fprintf(stderr, "Error: code generation failed\n");
        return -1;
    }

    /* Step 3: Execute */
    int64_t result = execute_code(code, code_size);

    /* Cleanup */
    free(code);

    return result;
}

/* Print result */
void print_result(const char *expr_str, habu_value_t expr, int64_t result) {
    printf("%s => ", expr_str);

    /* Check if result looks like a tagged fixnum */
    if (HABU_IS_FIXNUM(result)) {
        printf("%lld\n", HABU_UNTAG_FIXNUM(result));
    } else {
        /* Comparison or other untagged result */
        printf("%lld (raw)\n", result);
    }
}

/* Example programs */
void run_examples(void) {
    printf("=== Habu Bootstrap Compiler ===\n\n");
    printf("Example Programs:\n\n");

    /* Example 1: Literal */
    printf("1. Literals:\n");
    habu_value_t lit1 = HABU_TAG_FIXNUM(42);
    int64_t res1 = compile_and_run(lit1);
    print_result("42", lit1, res1);

    habu_value_t lit2 = HABU_TAG_FIXNUM(100);
    int64_t res2 = compile_and_run(lit2);
    print_result("100", lit2, res2);

    /* Example 2: Arithmetic */
    printf("\n2. Arithmetic:\n");
    habu_value_t add1 = make_binop("+", HABU_TAG_FIXNUM(5), HABU_TAG_FIXNUM(7));
    int64_t res3 = compile_and_run(add1);
    print_result("(+ 5 7)", add1, res3);

    habu_value_t mul1 = make_binop("*", HABU_TAG_FIXNUM(6), HABU_TAG_FIXNUM(7));
    int64_t res4 = compile_and_run(mul1);
    print_result("(* 6 7)", mul1, res4);

    habu_value_t sub1 = make_binop("-", HABU_TAG_FIXNUM(20), HABU_TAG_FIXNUM(8));
    int64_t res5 = compile_and_run(sub1);
    print_result("(- 20 8)", sub1, res5);

    /* Example 3: Nested arithmetic */
    printf("\n3. Nested Expressions:\n");
    habu_value_t inner = make_binop("+", HABU_TAG_FIXNUM(3), HABU_TAG_FIXNUM(4));
    habu_value_t nested = make_binop("*", inner, HABU_TAG_FIXNUM(5));
    int64_t res6 = compile_and_run(nested);
    print_result("(* (+ 3 4) 5)", nested, res6);

    /* Example 4: Comparisons */
    printf("\n4. Comparisons:\n");
    habu_value_t eq1 = make_binop("=", HABU_TAG_FIXNUM(5), HABU_TAG_FIXNUM(5));
    int64_t res7 = compile_and_run(eq1);
    print_result("(= 5 5)", eq1, res7);

    habu_value_t lt1 = make_binop("<", HABU_TAG_FIXNUM(3), HABU_TAG_FIXNUM(10));
    int64_t res8 = compile_and_run(lt1);
    print_result("(< 3 10)", lt1, res8);

    /* Example 5: Conditionals */
    printf("\n5. Conditionals:\n");
    habu_value_t test1 = make_binop("=", HABU_TAG_FIXNUM(10), HABU_TAG_FIXNUM(10));
    habu_value_t if1 = make_if(test1, HABU_TAG_FIXNUM(42), HABU_TAG_FIXNUM(99));
    int64_t res9 = compile_and_run(if1);
    print_result("(if (= 10 10) 42 99)", if1, res9);

    habu_value_t test2 = make_binop(">", HABU_TAG_FIXNUM(5), HABU_TAG_FIXNUM(10));
    habu_value_t if2 = make_if(test2, HABU_TAG_FIXNUM(100), HABU_TAG_FIXNUM(200));
    int64_t res10 = compile_and_run(if2);
    print_result("(if (> 5 10) 100 200)", if2, res10);

    /* Example 6: Complex nested expression */
    printf("\n6. Complex Expression:\n");
    habu_value_t cmp = make_binop("<", HABU_TAG_FIXNUM(5), HABU_TAG_FIXNUM(10));
    habu_value_t then_expr = make_binop("*", HABU_TAG_FIXNUM(10), HABU_TAG_FIXNUM(10));
    habu_value_t else_expr = make_binop("+", HABU_TAG_FIXNUM(1), HABU_TAG_FIXNUM(1));
    habu_value_t complex = make_if(cmp, then_expr, else_expr);
    int64_t res11 = compile_and_run(complex);
    print_result("(if (< 5 10) (* 10 10) (+ 1 1))", complex, res11);

    printf("\n=== All Examples Complete ===\n\n");
}

int main(int argc, char *argv[]) {
    /* Run example programs */
    run_examples();

    printf("Bootstrap compiler ready for more complex programs!\n\n");

    return 0;
}
