/* Bootstrap Compiler - Tier 4: Code Generation
 *
 * Generate ARM64 machine code from IR.
 * Uses the encoder functions from Tier 2 to emit instructions.
 *
 * Code generation strategy:
 * - Result always in x0
 * - Stack grows downward
 * - Variables stored on stack at known offsets
 * - Temporary values pushed/popped as needed
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "habu-minimal.h"

/* Encoder function declarations from Tier 2 */
extern void arm64_encode_movz(uint8_t *dest, uint8_t rd, uint16_t imm);
extern void arm64_encode_add(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t rm);
extern void arm64_encode_sub(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t rm);
extern void arm64_encode_mul(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t rm);
extern void arm64_encode_lsr(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t shift);
extern void arm64_encode_lsl(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t shift);
extern void arm64_encode_ldr(uint8_t *dest, uint8_t rt, uint8_t rn, uint16_t offset);
extern void arm64_encode_str(uint8_t *dest, uint8_t rt, uint8_t rn, uint16_t offset);
extern void arm64_encode_stp(uint8_t *dest, uint8_t rt1, uint8_t rt2, uint8_t rn, int16_t imm);
extern void arm64_encode_ldp(uint8_t *dest, uint8_t rt1, uint8_t rt2, uint8_t rn, int16_t imm);
extern void arm64_encode_b(uint8_t *dest, int32_t offset);
extern void arm64_encode_b_cond(uint8_t *dest, uint8_t cond, int32_t offset);
extern void arm64_encode_bl(uint8_t *dest, int32_t offset);
extern void arm64_encode_ret(uint8_t *dest);
extern void arm64_encode_mov(uint8_t *dest, uint8_t rd, uint8_t rm);
extern void arm64_encode_cmp(uint8_t *dest, uint8_t rn, uint8_t rm);
extern void arm64_encode_cset(uint8_t *dest, uint8_t rd, uint8_t cond);
extern void arm64_encode_and_imm_0xF(uint8_t *dest, uint8_t rd, uint8_t rn);
extern void arm64_encode_cmp_imm(uint8_t *dest, uint8_t rn, uint16_t imm);

/* ============================================
 * Code Buffer Management
 * ============================================ */

#define MAX_CODE_SIZE 4096

typedef struct {
    uint8_t *buffer;      /* Code buffer */
    size_t size;          /* Current size in bytes */
    size_t capacity;      /* Maximum capacity */
} code_buffer_t;

/* Initialize code buffer */
code_buffer_t *code_buffer_init(void) {
    code_buffer_t *cb = malloc(sizeof(code_buffer_t));
    cb->buffer = malloc(MAX_CODE_SIZE);
    cb->size = 0;
    cb->capacity = MAX_CODE_SIZE;
    return cb;
}

/* Free code buffer */
void code_buffer_free(code_buffer_t *cb) {
    free(cb->buffer);
    free(cb);
}

/* Emit 4 bytes (one instruction) */
void emit(code_buffer_t *cb, uint8_t *bytes) {
    if (cb->size + 4 > cb->capacity) {
        fprintf(stderr, "Error: code buffer overflow\n");
        return;
    }
    memcpy(cb->buffer + cb->size, bytes, 4);
    cb->size += 4;
}

/* Get current instruction offset (in instruction units, not bytes) */
int get_instr_offset(code_buffer_t *cb) {
    return cb->size / 4;
}

/* ============================================
 * Helper Functions
 * ============================================ */

/* Check if IR node has given tag */
int ir_has_tag(habu_value_t ir, const char *tag) {
    if (!HABU_IS_CONS(ir)) return 0;
    habu_value_t head = habu_car(ir);
    if (!HABU_IS_SYMBOL(head)) return 0;
    habu_value_t tag_sym = habu_intern(tag);
    return head == tag_sym;
}

/* Get nth element of list (0-indexed) */
habu_value_t list_ref(habu_value_t list, int n) {
    for (int i = 0; i < n; i++) {
        if (!HABU_IS_CONS(list)) return HABU_NIL;
        list = habu_cdr(list);
    }
    if (!HABU_IS_CONS(list)) return HABU_NIL;
    return habu_car(list);
}

/* ============================================
 * Code Generation Functions
 * ============================================ */

/* Forward declaration */
void codegen_expr(code_buffer_t *cb, habu_value_t ir);

/* Generate code for literal: (lit N) */
void codegen_lit(code_buffer_t *cb, habu_value_t ir) {
    habu_value_t value_tagged = list_ref(ir, 1);
    int64_t value = HABU_UNTAG_FIXNUM(value_tagged);

    /* Tag the value (value << 4) */
    int64_t tagged = value << 4;

    /* Emit: movz x0, #tagged */
    /* For large values, we'd need multiple instructions (movz + movk) */
    /* For now, assume values fit in 16 bits */
    uint8_t instr[4];
    arm64_encode_movz(instr, 0, tagged & 0xFFFF);
    emit(cb, instr);
}

/* Generate code for variable: (var offset) */
void codegen_var(code_buffer_t *cb, habu_value_t ir) {
    habu_value_t offset_tagged = list_ref(ir, 1);
    int64_t offset = HABU_UNTAG_FIXNUM(offset_tagged);

    /* Emit: ldr x0, [sp, #(offset * 8)] */
    uint8_t instr[4];
    arm64_encode_ldr(instr, 0, 31, offset * 8);  /* sp = x31 */
    emit(cb, instr);
}

/* Generate code for binary operation: (binop op a b) */
void codegen_binop(code_buffer_t *cb, habu_value_t ir) {
    habu_value_t op = list_ref(ir, 1);
    habu_value_t a_ir = list_ref(ir, 2);
    habu_value_t b_ir = list_ref(ir, 3);

    /* Generate code for first operand (result in x0) */
    codegen_expr(cb, a_ir);

    /* Save first operand to stack - use sub sp to allocate space */
    uint8_t instr[4];
    /* For now, use x2 as a temporary register to avoid stack issues */
    /* mov x2, x0 - save first operand in x2 */
    arm64_encode_mov(instr, 2, 0);
    emit(cb, instr);

    /* Generate code for second operand (result in x0) */
    codegen_expr(cb, b_ir);

    /* Move second operand from x0 to x1 */
    uint8_t mov_instr[4];
    arm64_encode_mov(mov_instr, 1, 0);  /* x1 = x0 */
    emit(cb, mov_instr);

    /* Move first operand from x2 to x0 */
    arm64_encode_mov(instr, 0, 2);  /* x0 = x2 */
    emit(cb, instr);

    /* Determine operation and emit appropriate instruction */
    habu_value_t plus_sym = habu_intern("+");
    habu_value_t minus_sym = habu_intern("-");
    habu_value_t times_sym = habu_intern("*");
    habu_value_t divide_sym = habu_intern("/");
    habu_value_t eq_sym = habu_intern("=");
    habu_value_t lt_sym = habu_intern("<");
    habu_value_t gt_sym = habu_intern(">");

    if (op == plus_sym) {
        /* add x0, x0, x1 */
        arm64_encode_add(instr, 0, 0, 1);
        emit(cb, instr);
    } else if (op == minus_sym) {
        /* sub x0, x0, x1 */
        arm64_encode_sub(instr, 0, 0, 1);
        emit(cb, instr);
    } else if (op == times_sym) {
        /* For multiplication: untag, multiply, re-tag */
        /* lsr x0, x0, #4 */
        arm64_encode_lsr(instr, 0, 0, 4);
        emit(cb, instr);
        /* lsr x1, x1, #4 */
        arm64_encode_lsr(instr, 1, 1, 4);
        emit(cb, instr);
        /* mul x0, x0, x1 */
        arm64_encode_mul(instr, 0, 0, 1);
        emit(cb, instr);
        /* lsl x0, x0, #4 */
        arm64_encode_lsl(instr, 0, 0, 4);
        emit(cb, instr);
    } else if (op == divide_sym) {
        /* Similar to multiply but with division */
        /* TODO: Implement division */
    } else if (op == eq_sym) {
        /* cmp x0, x1 */
        arm64_encode_cmp(instr, 0, 1);
        emit(cb, instr);
        /* cset x0, eq */
        arm64_encode_cset(instr, 0, 0);  /* EQ = 0 */
        emit(cb, instr);
    } else if (op == lt_sym) {
        /* cmp x0, x1 */
        arm64_encode_cmp(instr, 0, 1);
        emit(cb, instr);
        /* cset x0, lt */
        arm64_encode_cset(instr, 0, 11);  /* LT = 11 */
        emit(cb, instr);
    } else if (op == gt_sym) {
        /* cmp x0, x1 */
        arm64_encode_cmp(instr, 0, 1);
        emit(cb, instr);
        /* cset x0, gt */
        arm64_encode_cset(instr, 0, 12);  /* GT = 12 */
        emit(cb, instr);
    }
}

/* Generate code for if expression: (if test then else) */
void codegen_if(code_buffer_t *cb, habu_value_t ir) {
    habu_value_t test_ir = list_ref(ir, 1);
    habu_value_t then_ir = list_ref(ir, 2);
    habu_value_t else_ir = list_ref(ir, 3);

    /* Generate code for test */
    codegen_expr(cb, test_ir);

    /* Compare result with 0 */
    uint8_t instr[4];
    arm64_encode_cmp_imm(instr, 0, 0);  /* cmp x0, #0 */
    emit(cb, instr);

    /* We need to generate then and else first, then calculate offsets */
    /* Use a temporary buffer approach */

    /* Save current position */
    int beq_pos = get_instr_offset(cb);

    /* Emit placeholder conditional branch (b.eq else_label) */
    /* When test is 0 (false), branch to else */
    /* When test is non-zero (true), continue to then */
    arm64_encode_b_cond(instr, 0, 0);  /* EQ=0, offset=0 placeholder */
    int beq_byte_offset = cb->size;
    emit(cb, instr);

    /* Generate then code (executed when test != 0) */
    codegen_expr(cb, then_ir);

    /* Emit placeholder unconditional branch (b end_label) */
    int b_pos = get_instr_offset(cb);
    arm64_encode_b(instr, 0);  /* offset=0 placeholder */
    int b_byte_offset = cb->size;
    emit(cb, instr);

    /* else_label: Generate else code (executed when test == 0) */
    int else_pos = get_instr_offset(cb);
    codegen_expr(cb, else_ir);

    /* end_label: */
    int end_pos = get_instr_offset(cb);

    /* Calculate offsets (in instruction units) */
    /* ARM branches are PC-relative where PC = current instruction */
    /* b.eq offset: from beq instruction to else_label */
    int beq_offset = else_pos - beq_pos;

    /* b offset: from b instruction to end_label */
    int b_offset = end_pos - b_pos;

    /* Patch the branch instructions */
    arm64_encode_b_cond(instr, 0, beq_offset);  /* EQ=0: branch to else when x0 == 0 */
    memcpy(cb->buffer + beq_byte_offset, instr, 4);

    arm64_encode_b(instr, b_offset);
    memcpy(cb->buffer + b_byte_offset, instr, 4);
}

/* Generate code for let expression: (let bindings body) */
void codegen_let(code_buffer_t *cb, habu_value_t ir) {
    habu_value_t bindings = list_ref(ir, 1);
    habu_value_t body = list_ref(ir, 2);

    /* Allocate stack space for bindings */
    /* Count bindings */
    int num_bindings = 0;
    habu_value_t b = bindings;
    while (HABU_IS_CONS(b)) {
        num_bindings++;
        b = habu_cdr(b);
    }

    /* Generate code for each binding and save to stack */
    b = bindings;
    int offset = 0;
    while (HABU_IS_CONS(b)) {
        habu_value_t binding = habu_car(b);
        /* binding is (var val-ir offset) */
        habu_value_t val_ir = list_ref(binding, 1);

        /* Generate code for value */
        codegen_expr(cb, val_ir);

        /* Save to stack at offset */
        uint8_t instr[4];
        arm64_encode_str(instr, 0, 31, offset * 8);
        emit(cb, instr);

        offset++;
        b = habu_cdr(b);
    }

    /* Generate code for body */
    codegen_expr(cb, body);

    /* Deallocate stack space */
    /* TODO: Restore stack pointer */
}

/* Generate code for function call: (call fn args) */
void codegen_call(code_buffer_t *cb, habu_value_t ir) {
    habu_value_t fn = list_ref(ir, 1);
    habu_value_t args = list_ref(ir, 2);

    /* TODO: Implement function calls */
    /* This requires:
     * 1. Evaluate arguments
     * 2. Place arguments in registers (x0, x1, x2) or stack
     * 3. Call function (bl offset or blr register)
     * 4. Result in x0
     */
}

/* Main code generation function */
void codegen_expr(code_buffer_t *cb, habu_value_t ir) {
    if (ir_has_tag(ir, "lit")) {
        codegen_lit(cb, ir);
    } else if (ir_has_tag(ir, "var")) {
        codegen_var(cb, ir);
    } else if (ir_has_tag(ir, "binop")) {
        codegen_binop(cb, ir);
    } else if (ir_has_tag(ir, "if")) {
        codegen_if(cb, ir);
    } else if (ir_has_tag(ir, "let")) {
        codegen_let(cb, ir);
    } else if (ir_has_tag(ir, "call")) {
        codegen_call(cb, ir);
    } else {
        fprintf(stderr, "Error: unknown IR node type\n");
    }
}

/* Entry point: compile IR to machine code */
uint8_t *bootstrap_codegen(habu_value_t ir, size_t *code_size) {
    code_buffer_t *cb = code_buffer_init();

    /* Generate function prologue */
    uint8_t instr[4];
    arm64_encode_stp(instr, 29, 30, 31, -16);
    emit(cb, instr);

    /* Generate code for expression */
    codegen_expr(cb, ir);

    /* Generate function epilogue */
    arm64_encode_ldp(instr, 29, 30, 31, 16);
    emit(cb, instr);
    arm64_encode_ret(instr);
    emit(cb, instr);

    /* Return code buffer */
    *code_size = cb->size;
    uint8_t *code = cb->buffer;
    free(cb);  /* Don't free buffer, just the wrapper */
    return code;
}
