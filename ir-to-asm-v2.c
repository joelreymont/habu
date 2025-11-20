/* Convert Habu IR to ARM64 assembly - Version 2
 * Now with support for nested expressions!
 *
 * Input: S-expression IR
 *   (lit 42)                           -> Load literal
 *   (call + (lit 3) (lit 4))          -> Binary operation
 *   (call + (lit 1) (call + (lit 2) (lit 3)))  -> Nested!
 *
 * Output: ARM64 assembly to stdout
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>

/* S-expression AST nodes */
typedef enum {
    NODE_LIT,    /* (lit value) */
    NODE_CALL,   /* (call op arg1 arg2) */
    NODE_VAR     /* (var name) - not yet implemented */
} NodeType;

typedef struct Node {
    NodeType type;
    union {
        int64_t lit_value;
        struct {
            char op;           /* '+', '-', '*', '/' */
            struct Node *arg1;
            struct Node *arg2;
        } call;
        char *var_name;
    } data;
} Node;

/* Forward declarations */
void codegen_expr(Node *node);
Node *parse_expr(const char **input);

/* Stack depth tracking for comments */
static int stack_depth = 0;

/* Emit ARM64 assembly header */
void emit_header(void) {
    printf(".section __TEXT,__text,regular,pure_instructions\n");
    printf(".globl _main\n");
    printf(".p2align 2\n");
    printf("\n");
    printf("_main:\n");
    printf("    ; Set up stack frame\n");
    printf("    stp x29, x30, [sp, #-16]!\n");
    printf("    mov x29, sp\n");
}

/* Emit ARM64 assembly footer */
void emit_footer(void) {
    printf("    ; Tear down stack frame\n");
    printf("    mov sp, x29\n");
    printf("    ldp x29, x30, [sp], #16\n");
    printf("    ; Untag fixnum for exit code\n");
    printf("    lsr x0, x0, #4\n");
    printf("    ret\n");
}

/* Generate code for a literal - result in x0 */
void codegen_lit(int64_t value) {
    int64_t tagged = value << 4;
    printf("    ; Literal: %lld\n", value);
    printf("    mov x0, #%lld\n", tagged);
}

/* Generate code for binary operation - result in x0 */
void codegen_binop(char op, Node *arg1, Node *arg2) {
    printf("    ; Binary op: %c\n", op);

    /* Generate code for left operand → x0 */
    codegen_expr(arg1);

    /* Save left result on stack */
    printf("    ; Save left operand\n");
    printf("    str x0, [sp, #-16]!\n");
    stack_depth++;

    /* Generate code for right operand → x0 */
    codegen_expr(arg2);

    /* Move right result to x2 */
    printf("    mov x2, x0\n");

    /* Pop left result from stack → x1 */
    printf("    ; Restore left operand\n");
    printf("    ldr x1, [sp], #16\n");
    stack_depth--;

    /* Perform operation: x1 op x2 → x0 */
    switch(op) {
        case '+':
            printf("    ; Add: x1 + x2\n");
            printf("    add x0, x1, x2\n");
            break;
        case '-':
            printf("    ; Subtract: x1 - x2\n");
            printf("    sub x0, x1, x2\n");
            break;
        case '*':
            printf("    ; Multiply: x1 * x2\n");
            printf("    ; Untag one operand for correct result\n");
            printf("    lsr x1, x1, #4\n");
            printf("    mul x0, x1, x2\n");
            break;
        case '/':
            printf("    ; Divide: x1 / x2\n");
            printf("    ; Untag both operands\n");
            printf("    lsr x1, x1, #4\n");
            printf("    lsr x2, x2, #4\n");
            printf("    udiv x0, x1, x2\n");
            printf("    ; Re-tag result\n");
            printf("    lsl x0, x0, #4\n");
            break;
        default:
            printf("    ; Unknown op, return 0\n");
            printf("    mov x0, #0\n");
    }
}

/* Generate code for expression - result in x0 */
void codegen_expr(Node *node) {
    if (!node) {
        printf("    ; NULL node, return 0\n");
        printf("    mov x0, #0\n");
        return;
    }

    switch(node->type) {
        case NODE_LIT:
            codegen_lit(node->data.lit_value);
            break;

        case NODE_CALL:
            codegen_binop(node->data.call.op,
                         node->data.call.arg1,
                         node->data.call.arg2);
            break;

        case NODE_VAR:
            printf("    ; Variable not yet supported\n");
            printf("    mov x0, #0\n");
            break;
    }
}

/* Simple recursive descent parser */

void skip_whitespace(const char **p) {
    while (**p && isspace(**p)) (*p)++;
}

/* Parse (lit N) */
Node *parse_lit(const char **input) {
    const char *p = *input;
    skip_whitespace(&p);

    /* Expect (lit */
    if (*p != '(' || strncmp(p, "(lit ", 5) != 0) {
        return NULL;
    }
    p += 5;

    /* Parse number */
    int64_t value = atoll(p);

    /* Skip to closing paren */
    while (*p && *p != ')') p++;
    if (*p == ')') p++;

    *input = p;

    Node *node = malloc(sizeof(Node));
    node->type = NODE_LIT;
    node->data.lit_value = value;
    return node;
}

/* Parse (call OP ARG1 ARG2) */
Node *parse_call(const char **input) {
    const char *p = *input;
    skip_whitespace(&p);

    /* Expect (call */
    if (*p != '(' || strncmp(p, "(call ", 6) != 0) {
        return NULL;
    }
    p += 6;

    skip_whitespace(&p);

    /* Parse operator */
    char op = *p++;

    skip_whitespace(&p);

    /* Parse first argument (recursive) */
    Node *arg1 = parse_expr(&p);

    skip_whitespace(&p);

    /* Parse second argument (recursive) */
    Node *arg2 = parse_expr(&p);

    skip_whitespace(&p);

    /* Expect closing paren */
    if (*p == ')') p++;

    *input = p;

    Node *node = malloc(sizeof(Node));
    node->type = NODE_CALL;
    node->data.call.op = op;
    node->data.call.arg1 = arg1;
    node->data.call.arg2 = arg2;
    return node;
}

/* Parse any expression */
Node *parse_expr(const char **input) {
    const char *p = *input;
    skip_whitespace(&p);

    if (*p != '(') return NULL;

    /* Peek ahead to determine type */
    if (strncmp(p, "(lit ", 5) == 0) {
        return parse_lit(input);
    } else if (strncmp(p, "(call ", 6) == 0) {
        return parse_call(input);
    }

    return NULL;
}

/* Free AST */
void free_node(Node *node) {
    if (!node) return;

    if (node->type == NODE_CALL) {
        free_node(node->data.call.arg1);
        free_node(node->data.call.arg2);
    }

    free(node);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s '<IR-expression>'\n", argv[0]);
        fprintf(stderr, "Examples:\n");
        fprintf(stderr, "  %s '(lit 42)'\n", argv[0]);
        fprintf(stderr, "  %s '(call + (lit 3) (lit 4))'\n", argv[0]);
        fprintf(stderr, "  %s '(call + (lit 1) (call + (lit 2) (lit 3)))'\n", argv[0]);
        return 1;
    }

    /* Parse IR */
    const char *input = argv[1];
    Node *ast = parse_expr(&input);

    if (!ast) {
        fprintf(stderr, "Error: Failed to parse IR\n");
        return 1;
    }

    /* Generate assembly */
    emit_header();
    codegen_expr(ast);
    emit_footer();

    /* Cleanup */
    free_node(ast);

    return 0;
}
