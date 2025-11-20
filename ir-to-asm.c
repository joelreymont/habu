/* Convert Habu IR to ARM64 assembly
 *
 * Input: S-expression IR from stdin
 *   (lit 42)           -> Load literal
 *   (call + (lit 3) (lit 4))  -> Binary operation
 *
 * Output: ARM64 assembly to stdout
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Simple S-expression parser and code generator */

typedef enum {
    NODE_LIT,    /* (lit value) */
    NODE_CALL,   /* (call op arg1 arg2) */
    NODE_VAR     /* (var name) */
} NodeType;

typedef struct Node {
    NodeType type;
    union {
        int64_t lit_value;
        struct {
            char op;           /* '+', '-', '*' */
            struct Node *arg1;
            struct Node *arg2;
        } call;
        char *var_name;
    } data;
} Node;

/* Emit ARM64 assembly header */
void emit_header(void) {
    printf(".section __TEXT,__text,regular,pure_instructions\n");
    printf(".globl _main\n");
    printf(".p2align 2\n");
    printf("\n");
    printf("_main:\n");
}

/* Emit ARM64 assembly footer (untag and return) */
void emit_footer(void) {
    printf("    ; Untag fixnum for exit code\n");
    printf("    lsr x0, x0, #4\n");
    printf("    ret\n");
}

/* Generate code for a literal */
void codegen_lit(int64_t value) {
    int64_t tagged = value << 4;  /* Tag as fixnum */
    printf("    ; Load literal %lld (tagged: %lld)\n", value, tagged);
    printf("    mov x0, #%lld\n", tagged);
}

/* Generate code for a binary operation */
void codegen_call(char op, int64_t arg1, int64_t arg2) {
    int64_t tagged1 = arg1 << 4;
    int64_t tagged2 = arg2 << 4;

    printf("    ; Binary operation: %lld %c %lld\n", arg1, op, arg2);
    printf("    mov x1, #%lld\n", tagged1);
    printf("    mov x2, #%lld\n", tagged2);

    switch(op) {
        case '+':
            printf("    add x0, x1, x2\n");
            break;
        case '-':
            printf("    sub x0, x1, x2\n");
            break;
        case '*':
            /* For multiply, we need to untag one operand to get correct result */
            printf("    lsr x1, x1, #4  ; Untag first arg\n");
            printf("    mul x0, x1, x2\n");
            break;
        default:
            printf("    mov x0, #0\n");
    }
}

/* Parse and generate code from simple IR
 * For now, just handle:
 *   (lit N)
 *   (call OP (lit N1) (lit N2))
 */
void process_ir(const char *ir) {
    /* Super simple parser for demonstration */
    if (strstr(ir, "(lit ") == ir) {
        /* Parse (lit N) */
        int64_t value;
        if (sscanf(ir, "(lit %lld)", &value) == 1) {
            codegen_lit(value);
        }
    }
    else if (strstr(ir, "(call ") == ir) {
        /* Parse (call OP (lit N1) (lit N2)) */
        char op_str[32];
        int64_t arg1, arg2;

        /* Extract operator */
        const char *p = ir + 6;  /* Skip "(call " */
        while (*p && isspace(*p)) p++;

        char op = *p++;  /* Get operator character */

        /* Skip to first (lit ...) */
        p = strstr(p, "(lit ");
        if (p && sscanf(p, "(lit %lld)", &arg1) == 1) {
            /* Skip to second (lit ...) */
            p = strstr(p + 1, "(lit ");
            if (p && sscanf(p, "(lit %lld)", &arg2) == 1) {
                codegen_call(op, arg1, arg2);
            }
        }
    }
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s '<IR-expression>'\n", argv[0]);
        fprintf(stderr, "Example: %s '(lit 42)'\n", argv[0]);
        fprintf(stderr, "Example: %s '(call + (lit 3) (lit 4))'\n", argv[0]);
        return 1;
    }

    emit_header();
    process_ir(argv[1]);
    emit_footer();

    return 0;
}
