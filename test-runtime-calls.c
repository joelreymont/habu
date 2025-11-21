/* Test compiled code calling runtime functions
 * Demonstrates proper calling convention for runtime integration
 */

#include "runtime/habu.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef habu_value_t (*habu_fn_t)(void);

/* Helper to encode 32-bit word */
void encode_word(uint8_t *buf, uint32_t word) {
    buf[0] = word & 0xFF;
    buf[1] = (word >> 8) & 0xFF;
    buf[2] = (word >> 16) & 0xFF;
    buf[3] = (word >> 24) & 0xFF;
}

/* Generate movz instruction */
void gen_movz(uint8_t *buf, int rd, uint16_t imm) {
    uint32_t instr = 0xD2800000 | (imm << 5) | rd;
    encode_word(buf, instr);
}

/* Generate movk instruction */
void gen_movk(uint8_t *buf, int rd, uint16_t imm, int shift) {
    uint32_t shift_sel = shift / 16;
    uint32_t instr = 0xF2800000 | (shift_sel << 21) | (imm << 5) | rd;
    encode_word(buf, instr);
}

/* Generate blr instruction */
void gen_blr(uint8_t *buf, int rn) {
    uint32_t instr = 0xD63F0000 | (rn << 5);
    encode_word(buf, instr);
}

/* Generate mov instruction (as orr with xzr) */
void gen_mov(uint8_t *buf, int rd, int rn) {
    uint32_t instr = 0xAA0003E0 | (rn << 16) | rd;
    encode_word(buf, instr);
}

/* Load 64-bit address into register */
int gen_load_addr(uint8_t *buf, int rd, uint64_t addr) {
    int pos = 0;
    gen_movz(&buf[pos], rd, addr & 0xFFFF);
    pos += 4;
    gen_movk(&buf[pos], rd, (addr >> 16) & 0xFFFF, 16);
    pos += 4;
    gen_movk(&buf[pos], rd, (addr >> 32) & 0xFFFF, 32);
    pos += 4;
    gen_movk(&buf[pos], rd, (addr >> 48) & 0xFFFF, 48);
    pos += 4;
    return pos;
}

habu_value_t execute_code(const uint8_t *code, size_t size) {
    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return 0;
    }

    memcpy(mem, code, size);

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return 0;
    }

    habu_fn_t fn = (habu_fn_t)mem;
    habu_value_t result = fn();

    munmap(mem, page_size);
    return result;
}

/* Test 1: Call habu_cons from compiled code */
void test_cons_call(void) {
    printf("Test 1: Call habu_cons(5, 10) from JIT code\n");

    uint8_t code[256];
    int pos = 0;

    /* Prologue */
    encode_word(&code[pos], 0xA9BF7BFD);  /* stp x29, x30, [sp, #-16]! */
    pos += 4;

    /* Load 5 tagged (80) into x0 */
    gen_movz(&code[pos], 0, 80);
    pos += 4;

    /* Load 10 tagged (160) into x1 */
    gen_movz(&code[pos], 1, 160);
    pos += 4;

    /* Load habu_cons address into x2 */
    pos += gen_load_addr(&code[pos], 2, (uint64_t)habu_cons);

    /* Call habu_cons: blr x2 */
    gen_blr(&code[pos], 2);
    pos += 4;

    /* Result in x0, epilogue */
    encode_word(&code[pos], 0xA8C17BFD);  /* ldp x29, x30, [sp], #16 */
    pos += 4;

    /* Return */
    encode_word(&code[pos], 0xD65F03C0);  /* ret */
    pos += 4;

    printf("  Generated %d bytes\n", pos);
    printf("  habu_cons address: %p\n", (void*)habu_cons);

    habu_value_t result = execute_code(code, pos);

    /* Verify it's a cons cell */
    if (is_pointer(result) && get_tag(result) == TAG_CONS) {
        habu_value_t car_val = habu_car(result);
        habu_value_t cdr_val = habu_cdr(result);
        int64_t car = value_to_fixnum(car_val);
        int64_t cdr = value_to_fixnum(cdr_val);

        printf("  Result: cons cell at %p\n", (void*)result);
        printf("  car: %lld (expected 5)\n", (long long)car);
        printf("  cdr: %lld (expected 10)\n", (long long)cdr);

        if (car == 5 && cdr == 10) {
            printf("  PASS - Compiled code successfully called habu_cons!\n\n");
        } else {
            printf("  FAIL - Wrong values\n\n");
        }
    } else {
        printf("  FAIL - Result is not a cons cell\n\n");
    }
}

/* Test 2: Call habu_car on cons cell */
void test_car_call(void) {
    printf("Test 2: Create cons, then call habu_car from JIT\n");

    /* First create a cons cell */
    HABU_ROOT(cell, habu_cons(fixnum_to_value(42), fixnum_to_value(99)));

    uint8_t code[256];
    int pos = 0;

    /* Prologue */
    encode_word(&code[pos], 0xA9BF7BFD);  /* stp x29, x30, [sp, #-16]! */
    pos += 4;

    /* Load cons cell address into x0 */
    pos += gen_load_addr(&code[pos], 0, (uint64_t)cell);

    /* Load habu_car address into x2 */
    pos += gen_load_addr(&code[pos], 2, (uint64_t)habu_car);

    /* Call habu_car: blr x2 */
    gen_blr(&code[pos], 2);
    pos += 4;

    /* Epilogue */
    encode_word(&code[pos], 0xA8C17BFD);  /* ldp x29, x30, [sp], #16 */
    pos += 4;

    /* Return */
    encode_word(&code[pos], 0xD65F03C0);  /* ret */
    pos += 4;

    printf("  Generated %d bytes\n", pos);
    printf("  Cons cell: %p\n", (void*)cell);
    printf("  habu_car address: %p\n", (void*)habu_car);

    habu_value_t result = execute_code(code, pos);
    int64_t value = value_to_fixnum(result);

    printf("  Result: %lld (expected 42)\n", (long long)value);

    if (value == 42) {
        printf("  PASS - JIT code called habu_car correctly!\n\n");
    } else {
        printf("  FAIL\n\n");
    }

    HABU_UNROOT(cell);
}

int main(void) {
    printf("=== Runtime Function Call Tests ===\n\n");

    habu_init(1024 * 1024);
    printf("Runtime initialized\n\n");

    test_cons_call();
    test_car_call();

    habu_shutdown();
    printf("Runtime shut down\n");

    return 0;
}
