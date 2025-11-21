/* Bootstrap Compiler - Tier 1: Primitives
 *
 * Hand-written ARM64 bytecode for fundamental operations:
 * - List operations: car, cdr, cons
 * - Arithmetic: +, -, *, /
 * - Comparison: =, <, >
 *
 * These functions are the foundation of the minimal compiler.
 * They will be used to build the compiler that compiles itself.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>
/* Note: We don't need runtime/habu.h for these bytecode arrays */

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

/* ============================================
 * List Operations
 * ============================================ */

/* car: Get first element of cons cell
 * Args: x0 = cons cell (tagged pointer)
 * Returns: x0 = first element
 * Calls: habu_car runtime function
 */
uint8_t bootstrap_car_code[] = {
    /* Simple wrapper around runtime habu_car */
    /* No prologue needed for simple runtime call */

    /* Load address of habu_car into x1 */
    /* This will be patched with actual runtime address */
    0x01, 0x00, 0x00, 0x58,  /* ldr x1, #0 (PC-relative, will be patched) */

    /* Call habu_car via register */
    0x20, 0x00, 0x3F, 0xD6,  /* blr x1 */

    /* Return */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */

    /* Runtime address placeholder (8 bytes) */
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00
};

/* cdr: Get rest of cons cell
 * Args: x0 = cons cell (tagged pointer)
 * Returns: x0 = rest
 * Calls: habu_cdr runtime function
 */
uint8_t bootstrap_cdr_code[] = {
    0x01, 0x00, 0x00, 0x58,  /* ldr x1, #0 (PC-relative) */
    0x20, 0x00, 0x3F, 0xD6,  /* blr x1 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
    0x00, 0x00, 0x00, 0x00,  /* Runtime address */
    0x00, 0x00, 0x00, 0x00
};

/* cons: Create cons cell
 * Args: x0 = first element, x1 = rest
 * Returns: x0 = new cons cell (tagged pointer)
 * Calls: habu_cons runtime function
 */
uint8_t bootstrap_cons_code[] = {
    0x02, 0x00, 0x00, 0x58,  /* ldr x2, #0 (PC-relative) */
    0x40, 0x00, 0x3F, 0xD6,  /* blr x2 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
    0x00, 0x00, 0x00, 0x00,  /* Runtime address */
    0x00, 0x00, 0x00, 0x00
};

/* nil?: Check if value is nil
 * Args: x0 = value
 * Returns: x0 = 1 (true) if nil, 0 (false) otherwise
 */
uint8_t bootstrap_nil_p_code[] = {
    /* Prologue */
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */

    /* Compare x0 with 0 (nil is represented as 0) */
    0x1F, 0x00, 0x00, 0xF1,  /* cmp x0, #0 */

    /* Set x0 to 1 if equal, 0 if not */
    0xE0, 0x17, 0x9F, 0x1A,  /* cset x0, eq */

    /* Epilogue */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* cons?: Check if value is cons cell
 * Args: x0 = value
 * Returns: x0 = 1 (true) if cons, 0 (false) otherwise
 * Checks if tag == 1 (cons tag)
 */
uint8_t bootstrap_cons_p_code[] = {
    /* Prologue */
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */

    /* Extract tag: x0 & 0xF */
    0x00, 0x3C, 0x00, 0x92,  /* and x0, x0, #0xF */

    /* Compare with 1 (cons tag) */
    0x1F, 0x04, 0x00, 0xF1,  /* cmp x0, #1 */

    /* Set x0 to 1 if equal, 0 if not */
    0xE0, 0x17, 0x9F, 0x1A,  /* cset x0, eq */

    /* Epilogue */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* ============================================
 * Arithmetic Operations
 * ============================================ */

/* add: Addition (for tagged fixnums)
 * Args: x0 = first operand (tagged), x1 = second operand (tagged)
 * Returns: x0 = sum (tagged)
 * Note: Fixnums are tagged with LSB=0, value in upper 60 bits
 *       Since tag is 0, we can just add directly
 */
uint8_t bootstrap_add_code[] = {
    /* Prologue */
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */

    /* Add x0 and x1 */
    0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */

    /* Epilogue */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* sub: Subtraction (for tagged fixnums)
 * Args: x0 = first operand (tagged), x1 = second operand (tagged)
 * Returns: x0 = difference (tagged)
 */
uint8_t bootstrap_sub_code[] = {
    /* Prologue */
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */

    /* Subtract x1 from x0 */
    0x00, 0x00, 0x01, 0xCB,  /* sub x0, x0, x1 */

    /* Epilogue */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* mul: Multiplication (for tagged fixnums)
 * Args: x0 = first operand (tagged), x1 = second operand (tagged)
 * Returns: x0 = product (tagged)
 * Note: Must untag operands, multiply, then re-tag result
 *       Fixnum value = tagged >> 4
 */
uint8_t bootstrap_mul_code[] = {
    /* Prologue */
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */

    /* Untag x0: x0 = x0 >> 4 */
    0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

    /* Untag x1: x1 = x1 >> 4 */
    0x21, 0xFC, 0x44, 0xD3,  /* lsr x1, x1, #4 */

    /* Multiply: x0 = x0 * x1 */
    0x00, 0x7C, 0x01, 0x9B,  /* mul x0, x0, x1 */

    /* Re-tag: x0 = x0 << 4 */
    0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */

    /* Epilogue */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* div: Division (for tagged fixnums)
 * Args: x0 = dividend (tagged), x1 = divisor (tagged)
 * Returns: x0 = quotient (tagged)
 * Note: Must untag operands, divide, then re-tag result
 */
uint8_t bootstrap_div_code[] = {
    /* Prologue */
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */

    /* Untag x0: x0 = x0 >> 4 */
    0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

    /* Untag x1: x1 = x1 >> 4 */
    0x21, 0xFC, 0x44, 0xD3,  /* lsr x1, x1, #4 */

    /* Divide: x0 = x0 / x1 (unsigned) */
    0x00, 0x08, 0xC1, 0x9A,  /* udiv x0, x0, x1 */

    /* Re-tag: x0 = x0 << 4 */
    0x00, 0xEC, 0x7C, 0xD3,  /* lsl x0, x0, #4 */

    /* Epilogue */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* ============================================
 * Comparison Operations
 * ============================================ */

/* eq: Equality comparison
 * Args: x0 = first operand, x1 = second operand
 * Returns: x0 = 1 if equal, 0 otherwise
 */
uint8_t bootstrap_eq_code[] = {
    /* Prologue */
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */

    /* Compare x0 with x1 */
    0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */

    /* Set x0 to 1 if equal, 0 if not */
    0xE0, 0x17, 0x9F, 0x1A,  /* cset x0, eq */

    /* Epilogue */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* lt: Less than comparison
 * Args: x0 = first operand, x1 = second operand
 * Returns: x0 = 1 if x0 < x1, 0 otherwise
 */
uint8_t bootstrap_lt_code[] = {
    /* Prologue */
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */

    /* Compare x0 with x1 */
    0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */

    /* Set x0 to 1 if less than, 0 otherwise */
    0xE0, 0xB7, 0x9F, 0x1A,  /* cset x0, lt */

    /* Epilogue */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* gt: Greater than comparison
 * Args: x0 = first operand, x1 = second operand
 * Returns: x0 = 1 if x0 > x1, 0 otherwise
 */
uint8_t bootstrap_gt_code[] = {
    /* Prologue */
    0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */

    /* Compare x0 with x1 */
    0x1F, 0x00, 0x01, 0xEB,  /* cmp x0, x1 */

    /* Set x0 to 1 if greater than, 0 otherwise */
    0xE0, 0xC7, 0x9F, 0x1A,  /* cset x0, gt */

    /* Epilogue */
    0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};

/* ============================================
 * Size Information
 * ============================================ */

const size_t bootstrap_car_size = sizeof(bootstrap_car_code);
const size_t bootstrap_cdr_size = sizeof(bootstrap_cdr_code);
const size_t bootstrap_cons_size = sizeof(bootstrap_cons_code);
const size_t bootstrap_nil_p_size = sizeof(bootstrap_nil_p_code);
const size_t bootstrap_cons_p_size = sizeof(bootstrap_cons_p_code);
const size_t bootstrap_add_size = sizeof(bootstrap_add_code);
const size_t bootstrap_sub_size = sizeof(bootstrap_sub_code);
const size_t bootstrap_mul_size = sizeof(bootstrap_mul_code);
const size_t bootstrap_div_size = sizeof(bootstrap_div_code);
const size_t bootstrap_eq_size = sizeof(bootstrap_eq_code);
const size_t bootstrap_lt_size = sizeof(bootstrap_lt_code);
const size_t bootstrap_gt_size = sizeof(bootstrap_gt_code);
