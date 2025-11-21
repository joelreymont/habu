/* Test conditional execution (if expressions)
 * Demonstrates control flow in compiled code
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

void encode_word(uint8_t *buf, uint32_t word) {
    buf[0] = word & 0xFF;
    buf[1] = (word >> 8) & 0xFF;
    buf[2] = (word >> 16) & 0xFF;
    buf[3] = (word >> 24) & 0xFF;
}

void gen_movz(uint8_t *buf, int rd, uint16_t imm) {
    uint32_t instr = 0xD2800000 | (imm << 5) | rd;
    encode_word(buf, instr);
}

void gen_cmp(uint8_t *buf, int rn, int rm) {
    /* CMP Xn, Xm is SUBS XZR, Xn, Xm */
    uint32_t instr = 0xEB00001F | (rm << 16) | (rn << 5);
    encode_word(buf, instr);
}

void gen_beq(uint8_t *buf, int offset) {
    /* B.EQ - branch if equal */
    uint32_t instr = 0x54000000 | ((offset / 4) << 5);
    encode_word(buf, instr);
}

void gen_b(uint8_t *buf, int offset) {
    /* B - unconditional branch */
    int imm26 = (offset / 4) & 0x3FFFFFF;
    uint32_t instr = 0x14000000 | imm26;
    encode_word(buf, instr);
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

/* Test 1: Simple if - (if (= 5 5) 42 99) */
void test_if_true(void) {
    printf("Test 1: (if (= 5 5) 42 99)\n");
    printf("Expected: 42\n");

    uint8_t code[256];
    int pos = 0;

    /* Prologue */
    encode_word(&code[pos], 0xA9BF7BFD);  /* stp x29, x30, [sp, #-16]! */
    pos += 4;

    /* Load 5 tagged into x0 and x1 */
    gen_movz(&code[pos], 0, 80);  /* 5 << 4 = 80 */
    pos += 4;
    gen_movz(&code[pos], 1, 80);
    pos += 4;

    /* Compare: cmp x0, x1 */
    gen_cmp(&code[pos], 0, 1);
    pos += 4;

    /* Branch if equal to then branch (skip 8 bytes to then) */
    encode_word(&code[pos], 0x54000040);  /* b.eq #8 */
    pos += 4;

    /* Else branch (if not equal): load 99 */
    gen_movz(&code[pos], 0, 1584);  /* 99 << 4 */
    pos += 4;

    /* Skip then branch (4 bytes - 1 instruction) */
    gen_b(&code[pos], 4);
    pos += 4;

    /* Then branch (if equal): load 42 */
    gen_movz(&code[pos], 0, 672);  /* 42 << 4 */
    pos += 4;

    /* Epilogue */
    encode_word(&code[pos], 0xA8C17BFD);  /* ldp x29, x30, [sp], #16 */
    pos += 4;

    /* Return */
    encode_word(&code[pos], 0xD65F03C0);  /* ret */
    pos += 4;

    habu_value_t result = execute_code(code, pos);
    int64_t value = value_to_fixnum(result);

    printf("  Result: %lld\n", (long long)value);
    if (value == 42) {
        printf("  PASS\n\n");
    } else {
        printf("  FAIL\n\n");
    }
}

/* Test 2: if false - (if (= 5 10) 42 99) */
void test_if_false(void) {
    printf("Test 2: (if (= 5 10) 42 99)\n");
    printf("Expected: 99\n");

    uint8_t code[256];
    int pos = 0;

    /* Prologue */
    encode_word(&code[pos], 0xA9BF7BFD);
    pos += 4;

    /* Load 5 and 10 tagged */
    gen_movz(&code[pos], 0, 80);
    pos += 4;
    gen_movz(&code[pos], 1, 160);  /* 10 << 4 */
    pos += 4;

    /* Compare */
    gen_cmp(&code[pos], 0, 1);
    pos += 4;

    /* Branch if equal to then */
    encode_word(&code[pos], 0x54000040);  /* b.eq #8 */
    pos += 4;

    /* Else: 99 (if not equal) */
    gen_movz(&code[pos], 0, 1584);
    pos += 4;

    /* Skip then */
    gen_b(&code[pos], 4);
    pos += 4;

    /* Then: 42 (if equal) */
    gen_movz(&code[pos], 0, 672);
    pos += 4;

    /* Epilogue */
    encode_word(&code[pos], 0xA8C17BFD);
    pos += 4;

    encode_word(&code[pos], 0xD65F03C0);
    pos += 4;

    habu_value_t result = execute_code(code, pos);
    int64_t value = value_to_fixnum(result);

    printf("  Result: %lld\n", (long long)value);
    if (value == 99) {
        printf("  PASS\n\n");
    } else {
        printf("  FAIL\n\n");
    }
}

/* Test 3: Nested if - (if (= 1 1) (if (= 2 2) 42 10) 99) */
void test_nested_if(void) {
    printf("Test 3: (if (= 1 1) (if (= 2 2) 42 10) 99)\n");
    printf("Expected: 42\n");

    uint8_t code[256];
    int pos = 0;

    /* Prologue */
    encode_word(&code[pos], 0xA9BF7BFD);
    pos += 4;

    /* Outer test: 1 = 1 */
    gen_movz(&code[pos], 0, 16);  /* 1 << 4 */
    pos += 4;
    gen_movz(&code[pos], 1, 16);
    pos += 4;
    gen_cmp(&code[pos], 0, 1);
    pos += 4;

    /* If outer equal, jump to then (inner test), else jump to outer else */
    int outer_else_patch = pos;
    encode_word(&code[pos], 0x54000001);  /* b.ne to outer else - will patch */
    pos += 4;

    /* Outer then = Inner test: 2 = 2 */
    gen_movz(&code[pos], 0, 32);  /* 2 << 4 */
    pos += 4;
    gen_movz(&code[pos], 1, 32);
    pos += 4;
    gen_cmp(&code[pos], 0, 1);
    pos += 4;

    /* If inner equal, jump to inner then */
    encode_word(&code[pos], 0x54000040);  /* b.eq #8 */
    pos += 4;

    /* Inner else: 10 */
    gen_movz(&code[pos], 0, 160);
    pos += 4;
    gen_b(&code[pos], 4);  /* Skip inner then */
    pos += 4;

    /* Inner then: 42 */
    gen_movz(&code[pos], 0, 672);
    pos += 4;

    /* Skip outer else */
    int skip_outer_else = pos;
    gen_b(&code[pos], 4);  /* Will patch */
    pos += 4;

    /* Outer else: 99 */
    int outer_else_start = pos;
    gen_movz(&code[pos], 0, 1584);
    pos += 4;

    /* Patch outer else branch */
    int outer_offset = outer_else_start - outer_else_patch;
    encode_word(&code[outer_else_patch], 0x54000001 | ((outer_offset / 4) << 5));

    /* Patch skip outer else */
    int skip_offset = pos - skip_outer_else;
    gen_b(&code[skip_outer_else], skip_offset);

    /* Epilogue */
    encode_word(&code[pos], 0xA8C17BFD);
    pos += 4;
    encode_word(&code[pos], 0xD65F03C0);
    pos += 4;

    habu_value_t result = execute_code(code, pos);
    int64_t value = value_to_fixnum(result);

    printf("  Result: %lld\n", (long long)value);
    if (value == 42) {
        printf("  PASS\n\n");
    } else {
        printf("  FAIL\n\n");
    }
}

int main(void) {
    printf("=== Conditional Execution Tests ===\n\n");

    habu_init(1024 * 1024);
    printf("Runtime initialized\n\n");

    test_if_true();
    test_if_false();
    test_nested_if();

    habu_shutdown();
    printf("Runtime shut down\n");

    return 0;
}
