/* Bootstrap Compiler - Tier 3: IR Generation
 *
 * Compile Habu Lisp expressions to intermediate representation (IR).
 * The IR is a simplified tree structure that's easier to generate code from.
 *
 * IR Node Types:
 * - (lit N)          - Literal value N
 * - (var offset)     - Variable reference at stack offset
 * - (binop OP a b)   - Binary operation
 * - (if test then else) - Conditional
 * - (let bindings body) - Let binding
 * - (call fn args)   - Function call
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "habu-minimal.h"

/* ============================================
 * IR Node Creation
 * ============================================ */

/* Create a literal IR node: (lit value) */
habu_value_t ir_lit(int64_t value) {
    habu_value_t lit_sym = habu_intern("lit");
    habu_value_t value_tagged = HABU_TAG_FIXNUM(value);
    return habu_cons(lit_sym, habu_cons(value_tagged, HABU_NIL));
}

/* Create a variable reference IR node: (var offset) */
habu_value_t ir_var(int64_t offset) {
    habu_value_t var_sym = habu_intern("var");
    habu_value_t offset_tagged = HABU_TAG_FIXNUM(offset);
    return habu_cons(var_sym, habu_cons(offset_tagged, HABU_NIL));
}

/* Create a binary operation IR node: (binop op a b) */
habu_value_t ir_binop(habu_value_t op, habu_value_t a, habu_value_t b) {
    habu_value_t binop_sym = habu_intern("binop");
    habu_value_t rest = habu_cons(op, habu_cons(a, habu_cons(b, HABU_NIL)));
    return habu_cons(binop_sym, rest);
}

/* Create an if IR node: (if test then else) */
habu_value_t ir_if(habu_value_t test, habu_value_t then_expr, habu_value_t else_expr) {
    habu_value_t if_sym = habu_intern("if");
    habu_value_t rest = habu_cons(test, habu_cons(then_expr, habu_cons(else_expr, HABU_NIL)));
    return habu_cons(if_sym, rest);
}

/* Create a let IR node: (let bindings body) */
habu_value_t ir_let(habu_value_t bindings, habu_value_t body) {
    habu_value_t let_sym = habu_intern("let");
    return habu_cons(let_sym, habu_cons(bindings, habu_cons(body, HABU_NIL)));
}

/* Create a call IR node: (call fn args) */
habu_value_t ir_call(habu_value_t fn, habu_value_t args) {
    habu_value_t call_sym = habu_intern("call");
    return habu_cons(call_sym, habu_cons(fn, habu_cons(args, HABU_NIL)));
}

/* ============================================
 * Environment Management
 * ============================================ */

/* Environment is a list of (var . offset) pairs
 * offset is distance from current stack pointer in 8-byte slots
 */

/* Lookup variable in environment, returns offset or -1 if not found */
int64_t env_lookup(habu_value_t var, habu_value_t env) {
    while (env != HABU_NIL) {
        habu_value_t entry = habu_car(env);
        habu_value_t entry_var = habu_car(entry);

        /* Compare symbols */
        if (HABU_IS_SYMBOL(var) && HABU_IS_SYMBOL(entry_var)) {
            /* Symbol comparison - check if same symbol */
            if (var == entry_var) {
                habu_value_t offset = habu_cdr(entry);
                return HABU_UNTAG_FIXNUM(offset);
            }
        }

        env = habu_cdr(env);
    }
    return -1;  /* Not found */
}

/* Extend environment with new variable binding */
habu_value_t env_extend(habu_value_t var, int64_t offset, habu_value_t env) {
    habu_value_t offset_tagged = HABU_TAG_FIXNUM(offset);
    habu_value_t entry = habu_cons(var, offset_tagged);
    return habu_cons(entry, env);
}

/* ============================================
 * Expression Compilation
 * ============================================ */

/* Forward declaration */
habu_value_t compile_expr(habu_value_t expr, habu_value_t env);

/* Check if expression is a tagged list with given head */
int is_tagged_list(habu_value_t expr, const char *tag) {
    if (!HABU_IS_CONS(expr)) return 0;
    habu_value_t head = habu_car(expr);
    if (!HABU_IS_SYMBOL(head)) return 0;
    habu_value_t tag_sym = habu_intern(tag);
    return head == tag_sym;
}

/* Compile a literal number */
habu_value_t compile_literal(habu_value_t expr) {
    if (HABU_IS_FIXNUM(expr)) {
        int64_t value = HABU_UNTAG_FIXNUM(expr);
        return ir_lit(value);
    }
    /* TODO: Handle other literal types */
    return ir_lit(0);
}

/* Compile a variable reference */
habu_value_t compile_variable(habu_value_t var, habu_value_t env) {
    int64_t offset = env_lookup(var, env);
    if (offset < 0) {
        /* Variable not found - error */
        fprintf(stderr, "Error: undefined variable\n");
        return ir_lit(0);
    }
    return ir_var(offset);
}

/* Compile a binary operation like (+ a b) */
habu_value_t compile_binop(habu_value_t op, habu_value_t args, habu_value_t env) {
    if (!HABU_IS_CONS(args)) {
        fprintf(stderr, "Error: binary op needs arguments\n");
        return ir_lit(0);
    }

    habu_value_t arg1 = habu_car(args);
    habu_value_t rest = habu_cdr(args);

    if (!HABU_IS_CONS(rest)) {
        fprintf(stderr, "Error: binary op needs two arguments\n");
        return ir_lit(0);
    }

    habu_value_t arg2 = habu_car(rest);

    /* Compile both operands */
    habu_value_t ir_arg1 = compile_expr(arg1, env);
    habu_value_t ir_arg2 = compile_expr(arg2, env);

    return ir_binop(op, ir_arg1, ir_arg2);
}

/* Compile an if expression: (if test then else) */
habu_value_t compile_if(habu_value_t args, habu_value_t env) {
    if (!HABU_IS_CONS(args)) {
        fprintf(stderr, "Error: if needs test expression\n");
        return ir_lit(0);
    }

    habu_value_t test = habu_car(args);
    habu_value_t rest = habu_cdr(args);

    if (!HABU_IS_CONS(rest)) {
        fprintf(stderr, "Error: if needs then expression\n");
        return ir_lit(0);
    }

    habu_value_t then_expr = habu_car(rest);
    rest = habu_cdr(rest);

    habu_value_t else_expr = HABU_NIL;
    if (HABU_IS_CONS(rest)) {
        else_expr = habu_car(rest);
    }

    /* Compile all three parts */
    habu_value_t ir_test = compile_expr(test, env);
    habu_value_t ir_then = compile_expr(then_expr, env);
    habu_value_t ir_else = compile_expr(else_expr, env);

    return ir_if(ir_test, ir_then, ir_else);
}

/* Compile a let expression: (let ((var val) ...) body) */
habu_value_t compile_let(habu_value_t args, habu_value_t env) {
    if (!HABU_IS_CONS(args)) {
        fprintf(stderr, "Error: let needs bindings\n");
        return ir_lit(0);
    }

    habu_value_t bindings = habu_car(args);
    habu_value_t rest = habu_cdr(args);

    if (!HABU_IS_CONS(rest)) {
        fprintf(stderr, "Error: let needs body\n");
        return ir_lit(0);
    }

    habu_value_t body = habu_car(rest);

    /* Process bindings and extend environment */
    habu_value_t new_env = env;
    habu_value_t ir_bindings = HABU_NIL;
    int64_t offset = 0;

    while (HABU_IS_CONS(bindings)) {
        habu_value_t binding = habu_car(bindings);

        if (!HABU_IS_CONS(binding)) {
            fprintf(stderr, "Error: invalid binding\n");
            return ir_lit(0);
        }

        habu_value_t var = habu_car(binding);
        habu_value_t val_list = habu_cdr(binding);

        if (!HABU_IS_CONS(val_list)) {
            fprintf(stderr, "Error: binding needs value\n");
            return ir_lit(0);
        }

        habu_value_t val = habu_car(val_list);

        /* Compile value expression in current environment */
        habu_value_t ir_val = compile_expr(val, env);

        /* Add to IR bindings with offset */
        habu_value_t ir_binding = habu_cons(var,
            habu_cons(ir_val, habu_cons(HABU_TAG_FIXNUM(offset), HABU_NIL)));
        ir_bindings = habu_cons(ir_binding, ir_bindings);

        /* Extend environment for next binding and body */
        new_env = env_extend(var, offset, new_env);
        offset++;

        bindings = habu_cdr(bindings);
    }

    /* Reverse bindings (they were built backwards) */
    /* TODO: Add reverse function or build in correct order */

    /* Compile body in extended environment */
    habu_value_t ir_body = compile_expr(body, new_env);

    return ir_let(ir_bindings, ir_body);
}

/* Compile a function call: (fn arg1 arg2 ...) */
habu_value_t compile_call(habu_value_t fn, habu_value_t args, habu_value_t env) {
    /* Compile all arguments */
    habu_value_t ir_args = HABU_NIL;

    while (HABU_IS_CONS(args)) {
        habu_value_t arg = habu_car(args);
        habu_value_t ir_arg = compile_expr(arg, env);
        ir_args = habu_cons(ir_arg, ir_args);
        args = habu_cdr(args);
    }

    /* TODO: Reverse args */

    return ir_call(fn, ir_args);
}

/* Main compilation function */
habu_value_t compile_expr(habu_value_t expr, habu_value_t env) {
    /* Literal number */
    if (HABU_IS_FIXNUM(expr)) {
        return compile_literal(expr);
    }

    /* Variable reference */
    if (HABU_IS_SYMBOL(expr)) {
        return compile_variable(expr, env);
    }

    /* List expression */
    if (HABU_IS_CONS(expr)) {
        habu_value_t head = habu_car(expr);
        habu_value_t args = habu_cdr(expr);

        /* If expression */
        if (is_tagged_list(expr, "if")) {
            return compile_if(args, env);
        }

        /* Let expression */
        if (is_tagged_list(expr, "let")) {
            return compile_let(args, env);
        }

        /* Binary operations */
        if (HABU_IS_SYMBOL(head)) {
            habu_value_t plus = habu_intern("+");
            habu_value_t minus = habu_intern("-");
            habu_value_t times = habu_intern("*");
            habu_value_t divide = habu_intern("/");

            if (head == plus || head == minus || head == times || head == divide) {
                return compile_binop(head, args, env);
            }

            /* Comparison operations */
            habu_value_t eq = habu_intern("=");
            habu_value_t lt = habu_intern("<");
            habu_value_t gt = habu_intern(">");

            if (head == eq || head == lt || head == gt) {
                return compile_binop(head, args, env);
            }
        }

        /* Function call */
        return compile_call(head, args, env);
    }

    /* Unknown expression type */
    fprintf(stderr, "Error: unknown expression type\n");
    return ir_lit(0);
}

/* Entry point for compilation */
habu_value_t bootstrap_compile(habu_value_t expr) {
    habu_value_t empty_env = HABU_NIL;
    return compile_expr(expr, empty_env);
}
