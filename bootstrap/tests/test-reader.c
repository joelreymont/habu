/* Test S-Expression Reader */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "../habu-minimal.h"

/* Reader function */
habu_value_t habu_read(const char *input);

/* Helper to print expressions for debugging */
void print_expr(habu_value_t expr) {
    if (expr == HABU_NIL) {
        printf("nil");
        return;
    }

    if (HABU_IS_FIXNUM(expr)) {
        printf("%lld", HABU_UNTAG_FIXNUM(expr));
        return;
    }

    if (HABU_IS_SYMBOL(expr)) {
        // Can't easily get symbol name, so just print address
        printf("<symbol:%llx>", expr);
        return;
    }

    if (HABU_IS_CONS(expr)) {
        printf("(");
        habu_value_t current = expr;
        int first = 1;

        while (current != HABU_NIL && HABU_IS_CONS(current)) {
            if (!first) printf(" ");
            first = 0;

            habu_value_t car = habu_car(current);
            print_expr(car);

            current = habu_cdr(current);
        }

        if (current != HABU_NIL) {
            printf(" . ");
            print_expr(current);
        }

        printf(")");
        return;
    }

    printf("<unknown:%llx>", expr);
}

void test_number() {
    printf("Test: number\n");
    habu_value_t expr = habu_read("42");
    printf("  Input: \"42\"\n");
    printf("  Parsed: ");
    print_expr(expr);
    printf("\n");

    assert(HABU_IS_FIXNUM(expr));
    assert(HABU_UNTAG_FIXNUM(expr) == 42);
    printf("  ✓ Correct fixnum value\n\n");
}

void test_negative_number() {
    printf("Test: negative number\n");
    habu_value_t expr = habu_read("-10");
    printf("  Input: \"-10\"\n");
    printf("  Parsed: ");
    print_expr(expr);
    printf("\n");

    assert(HABU_IS_FIXNUM(expr));
    assert(HABU_UNTAG_FIXNUM(expr) == -10);
    printf("  ✓ Correct negative fixnum\n\n");
}

void test_symbol() {
    printf("Test: symbol\n");
    habu_value_t expr = habu_read("+");
    printf("  Input: \"+\"\n");
    printf("  Parsed: ");
    print_expr(expr);
    printf("\n");

    assert(HABU_IS_SYMBOL(expr));
    printf("  ✓ Correct symbol\n\n");
}

void test_empty_list() {
    printf("Test: empty list\n");
    habu_value_t expr = habu_read("()");
    printf("  Input: \"()\"\n");
    printf("  Parsed: ");
    print_expr(expr);
    printf("\n");

    assert(expr == HABU_NIL);
    printf("  ✓ Empty list is nil\n\n");
}

void test_simple_list() {
    printf("Test: simple list\n");
    habu_value_t expr = habu_read("(+ 5 7)");
    printf("  Input: \"(+ 5 7)\"\n");
    printf("  Parsed: ");
    print_expr(expr);
    printf("\n");

    assert(HABU_IS_CONS(expr));

    habu_value_t op = habu_car(expr);
    assert(HABU_IS_SYMBOL(op));

    habu_value_t rest = habu_cdr(expr);
    assert(HABU_IS_CONS(rest));

    habu_value_t arg1 = habu_car(rest);
    assert(HABU_IS_FIXNUM(arg1));
    assert(HABU_UNTAG_FIXNUM(arg1) == 5);

    rest = habu_cdr(rest);
    assert(HABU_IS_CONS(rest));

    habu_value_t arg2 = habu_car(rest);
    assert(HABU_IS_FIXNUM(arg2));
    assert(HABU_UNTAG_FIXNUM(arg2) == 7);

    rest = habu_cdr(rest);
    assert(rest == HABU_NIL);

    printf("  ✓ Correct list structure: (op arg1 arg2)\n\n");
}

void test_nested_list() {
    printf("Test: nested list\n");
    habu_value_t expr = habu_read("(* (+ 3 4) 5)");
    printf("  Input: \"(* (+ 3 4) 5)\"\n");
    printf("  Parsed: ");
    print_expr(expr);
    printf("\n");

    assert(HABU_IS_CONS(expr));

    habu_value_t op = habu_car(expr);
    assert(HABU_IS_SYMBOL(op));

    habu_value_t rest = habu_cdr(expr);
    assert(HABU_IS_CONS(rest));

    habu_value_t nested = habu_car(rest);
    assert(HABU_IS_CONS(nested)); // (+ 3 4)

    habu_value_t nested_op = habu_car(nested);
    assert(HABU_IS_SYMBOL(nested_op));

    printf("  ✓ Correct nested structure\n\n");
}

void test_if_expression() {
    printf("Test: if expression\n");
    habu_value_t expr = habu_read("(if (= 5 5) 42 99)");
    printf("  Input: \"(if (= 5 5) 42 99)\"\n");
    printf("  Parsed: ");
    print_expr(expr);
    printf("\n");

    assert(HABU_IS_CONS(expr));

    habu_value_t if_sym = habu_car(expr);
    assert(HABU_IS_SYMBOL(if_sym));

    habu_value_t rest = habu_cdr(expr);
    assert(HABU_IS_CONS(rest));

    habu_value_t test = habu_car(rest);
    assert(HABU_IS_CONS(test)); // (= 5 5)

    rest = habu_cdr(rest);
    assert(HABU_IS_CONS(rest));

    habu_value_t then_expr = habu_car(rest);
    assert(HABU_IS_FIXNUM(then_expr));
    assert(HABU_UNTAG_FIXNUM(then_expr) == 42);

    rest = habu_cdr(rest);
    assert(HABU_IS_CONS(rest));

    habu_value_t else_expr = habu_car(rest);
    assert(HABU_IS_FIXNUM(else_expr));
    assert(HABU_UNTAG_FIXNUM(else_expr) == 99);

    printf("  ✓ Correct if structure\n\n");
}

int main(void) {
    printf("===== S-Expression Reader Tests =====\n\n");

    test_number();
    test_negative_number();
    test_symbol();
    test_empty_list();
    test_simple_list();
    test_nested_list();
    test_if_expression();

    printf("All reader tests passed! ✓\n");
    return 0;
}
