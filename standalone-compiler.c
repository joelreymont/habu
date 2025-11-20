/* Standalone ARM64 Machine Code Compiler and Runtime
 *
 * This is a minimal C implementation that:
 * 1. Parses simple Lisp expressions
 * 2. Generates ARM64 machine code directly
 * 3. Executes the code using mmap
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>
#include <ctype.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

/* ============================================
 * ARM64 Instruction Encoders
 * ============================================ */

typedef struct {
    uint8_t *data;
    size_t size;
    size_t capacity;
} CodeBuffer;

CodeBuffer* code_buffer_new() {
    CodeBuffer *buf = malloc(sizeof(CodeBuffer));
    buf->capacity = 4096;
    buf->data = malloc(buf->capacity);
    buf->size = 0;
    return buf;
}

void code_buffer_append(CodeBuffer *buf, uint32_t word) {
    if (buf->size + 4 > buf->capacity) {
        buf->capacity *= 2;
        buf->data = realloc(buf->data, buf->capacity);
    }
    buf->data[buf->size++] = (word >> 0) & 0xFF;
    buf->data[buf->size++] = (word >> 8) & 0xFF;
    buf->data[buf->size++] = (word >> 16) & 0xFF;
    buf->data[buf->size++] = (word >> 24) & 0xFF;
}

/* MOVZ Xd, #imm */
void emit_movz(CodeBuffer *buf, int rd, int imm) {
    uint32_t base = 0xD2800000;
    uint32_t instr = base | (imm << 5) | rd;
    code_buffer_append(buf, instr);
}

/* ADD Xd, Xn, Xm */
void emit_add(CodeBuffer *buf, int rd, int rn, int rm) {
    uint32_t base = 0x8B000000;
    uint32_t instr = base | (rm << 16) | (rn << 5) | rd;
    code_buffer_append(buf, instr);
}

/* SUB Xd, Xn, Xm */
void emit_sub(CodeBuffer *buf, int rd, int rn, int rm) {
    uint32_t base = 0xCB000000;
    uint32_t instr = base | (rm << 16) | (rn << 5) | rd;
    code_buffer_append(buf, instr);
}

/* MUL Xd, Xn, Xm */
void emit_mul(CodeBuffer *buf, int rd, int rn, int rm) {
    uint32_t base = 0x9B007C00;
    uint32_t instr = base | (rm << 16) | (rn << 5) | rd;
    code_buffer_append(buf, instr);
}

/* LSR Xd, Xn, #imm */
void emit_lsr(CodeBuffer *buf, int rd, int rn, int imm) {
    uint32_t base = 0xD3400000;
    uint32_t instr = base | (imm << 16) | (63 << 10) | (rn << 5) | rd;
    code_buffer_append(buf, instr);
}

/* LSL Xd, Xn, #imm */
void emit_lsl(CodeBuffer *buf, int rd, int rn, int imm) {
    uint32_t base = 0xD3400000;
    uint32_t shift = 64 - imm;
    uint32_t instr = base | (shift << 16) | ((63 - imm) << 10) | (rn << 5) | rd;
    code_buffer_append(buf, instr);
}

/* MOV Xd, Xn (implemented as ORR Xd, XZR, Xn) */
void emit_mov(CodeBuffer *buf, int rd, int rn) {
    uint32_t base = 0xAA0003E0;
    uint32_t instr = base | (rn << 16) | rd;
    code_buffer_append(buf, instr);
}

/* STR Xt, [Xn, #imm]! - pre-index */
void emit_str_pre(CodeBuffer *buf, int rt, int rn, int imm) {
    /* For sp (#-16): 0xF81F0FE0 for x0 */
    if (rt == 0 && rn == 31 && imm == -16) {
        code_buffer_append(buf, 0xF81F0FE0);
    }
}

/* LDR Xt, [Xn], #imm - post-index */
void emit_ldr_post(CodeBuffer *buf, int rt, int rn, int imm) {
    /* For sp (#16): 0xF84107E0 for x0 */
    if (rt == 0 && rn == 31 && imm == 16) {
        code_buffer_append(buf, 0xF84107E0);
    }
}

/* STP X29, X30, [SP, #-16]! */
void emit_stp_pre(CodeBuffer *buf) {
    code_buffer_append(buf, 0xA9BF7BFD);
}

/* LDP X29, X30, [SP], #16 */
void emit_ldp_post(CodeBuffer *buf) {
    code_buffer_append(buf, 0xA8C17BFD);
}

/* MOV X29, SP */
void emit_mov_fp_sp(CodeBuffer *buf) {
    code_buffer_append(buf, 0x910003FD);
}

/* MOV SP, X29 */
void emit_mov_sp_fp(CodeBuffer *buf) {
    code_buffer_append(buf, 0x910003BF);
}

/* RET */
void emit_ret(CodeBuffer *buf) {
    code_buffer_append(buf, 0xD65F03C0);
}

/* ============================================
 * Expression Parser
 * ============================================ */

typedef enum {
    EXPR_NUMBER,
    EXPR_SYMBOL,
    EXPR_LIST
} ExprType;

typedef struct Expr {
    ExprType type;
    union {
        int64_t number;
        char *symbol;
        struct {
            struct Expr **items;
            int count;
        } list;
    };
} Expr;

Expr* expr_new_number(int64_t n) {
    Expr *e = malloc(sizeof(Expr));
    e->type = EXPR_NUMBER;
    e->number = n;
    return e;
}

Expr* expr_new_symbol(const char *s) {
    Expr *e = malloc(sizeof(Expr));
    e->type = EXPR_SYMBOL;
    e->symbol = strdup(s);
    return e;
}

Expr* expr_new_list(Expr **items, int count) {
    Expr *e = malloc(sizeof(Expr));
    e->type = EXPR_LIST;
    e->list.items = items;
    e->list.count = count;
    return e;
}

/* Simple parser for (+ 1 2) style expressions */
Expr* parse(const char **input) {
    while (isspace(**input)) (*input)++;

    if (**input == '(') {
        (*input)++;
        Expr **items = malloc(sizeof(Expr*) * 100);
        int count = 0;

        while (**input && **input != ')') {
            items[count++] = parse(input);
            while (isspace(**input)) (*input)++;
        }

        if (**input == ')') (*input)++;
        return expr_new_list(items, count);
    }

    if (isdigit(**input) || **input == '-') {
        int64_t num = atoll(*input);
        while (isdigit(**input) || **input == '-') (*input)++;
        return expr_new_number(num);
    }

    /* Symbol */
    const char *start = *input;
    while (**input && !isspace(**input) && **input != '(' && **input != ')') {
        (*input)++;
    }
    size_t len = *input - start;
    char *sym = malloc(len + 1);
    memcpy(sym, start, len);
    sym[len] = '\0';
    return expr_new_symbol(sym);
}

/* ============================================
 * Code Generator
 * ============================================ */

void compile_expr(Expr *expr, CodeBuffer *buf);

void compile_binary_op(Expr *expr, CodeBuffer *buf) {
    if (expr->list.count != 3) {
        fprintf(stderr, "Error: binary op needs 2 arguments\n");
        return;
    }

    const char *op = expr->list.items[0]->symbol;
    Expr *left = expr->list.items[1];
    Expr *right = expr->list.items[2];

    /* Compile left operand */
    compile_expr(left, buf);

    /* Save x0 on stack */
    emit_str_pre(buf, 0, 31, -16);

    /* Compile right operand */
    compile_expr(right, buf);

    /* Move result to x1 */
    emit_mov(buf, 1, 0);

    /* Load left operand back to x0 */
    emit_ldr_post(buf, 0, 31, 16);

    /* Perform operation */
    if (strcmp(op, "+") == 0) {
        emit_add(buf, 0, 0, 1);
    } else if (strcmp(op, "-") == 0) {
        emit_sub(buf, 0, 0, 1);
    } else if (strcmp(op, "*") == 0) {
        /* For multiply, untag one operand */
        emit_lsr(buf, 0, 0, 4);
        emit_mul(buf, 0, 0, 1);
    }
}

void compile_expr(Expr *expr, CodeBuffer *buf) {
    if (expr->type == EXPR_NUMBER) {
        /* Load tagged fixnum: value << 4 */
        int64_t tagged = expr->number << 4;
        if (tagged >= 0 && tagged < 65536) {
            emit_movz(buf, 0, (int)tagged);
        } else {
            fprintf(stderr, "Error: number too large: %lld\n", expr->number);
        }
    } else if (expr->type == EXPR_LIST && expr->list.count > 0) {
        if (expr->list.items[0]->type == EXPR_SYMBOL) {
            const char *op = expr->list.items[0]->symbol;
            if (strcmp(op, "+") == 0 || strcmp(op, "-") == 0 || strcmp(op, "*") == 0) {
                compile_binary_op(expr, buf);
            } else {
                fprintf(stderr, "Error: unknown operator: %s\n", op);
            }
        }
    }
}

CodeBuffer* compile_program(Expr *expr) {
    CodeBuffer *buf = code_buffer_new();

    /* Prologue */
    emit_stp_pre(buf);
    emit_mov_fp_sp(buf);

    /* Body */
    compile_expr(expr, buf);

    /* Untag result */
    emit_lsr(buf, 0, 0, 4);

    /* Epilogue */
    emit_mov_sp_fp(buf);
    emit_ldp_post(buf);
    emit_ret(buf);

    return buf;
}

/* ============================================
 * Executor
 * ============================================ */

typedef int64_t (*fn_t)(void);

int64_t execute(CodeBuffer *buf) {
    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return -1;
    }

    memcpy(mem, buf->data, buf->size);

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return -1;
    }

    fn_t fn = (fn_t)mem;
    int64_t result = fn();

    munmap(mem, page_size);
    return result;
}

/* ============================================
 * Main
 * ============================================ */

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <expression>\n", argv[0]);
        fprintf(stderr, "Example: %s '(+ 2 3)'\n", argv[0]);
        return 1;
    }

    const char *input = argv[1];

    printf("Compiling: %s\n", input);

    Expr *expr = parse(&input);
    CodeBuffer *code = compile_program(expr);

    printf("Generated %zu bytes of machine code\n", code->size);

    /* Print bytes */
    printf("Bytes: ");
    for (size_t i = 0; i < code->size && i < 100; i++) {
        printf("%02X ", code->data[i]);
        if ((i + 1) % 16 == 0) printf("\n       ");
    }
    printf("\n");

    int64_t result = execute(code);
    printf("Result: %lld\n", result);

    return 0;
}
