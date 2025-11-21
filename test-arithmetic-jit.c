/* Test JIT compilation of arithmetic expressions
 * Tests: (+ (* 3 4) (- 10 5))
 * Expected result: 17
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*test_fn_t)(void);

void encode_word(unsigned char *buf, uint32_t word) {
    buf[0] = word & 0xFF;
    buf[1] = (word >> 8) & 0xFF;
    buf[2] = (word >> 16) & 0xFF;
    buf[3] = (word >> 24) & 0xFF;
}

void gen_movz(unsigned char *buf, int rd, uint16_t imm) {
    uint32_t instr = 0xD2800000 | (imm << 5) | rd;
    encode_word(buf, instr);
}

void gen_add(unsigned char *buf, int rd, int rn, int rm) {
    uint32_t instr = 0x8B000000 | (rm << 16) | (rn << 5) | rd;
    encode_word(buf, instr);
}

void gen_sub(unsigned char *buf, int rd, int rn, int rm) {
    uint32_t instr = 0xCB000000 | (rm << 16) | (rn << 5) | rd;
    encode_word(buf, instr);
}

void gen_mul(unsigned char *buf, int rd, int rn, int rm) {
    uint32_t instr = 0x9B007C00 | (rm << 16) | (rn << 5) | rd;
    encode_word(buf, instr);
}

void gen_lsr(unsigned char *buf, int rd, int rn, int shift) {
    /* LSR Xd, Xn, #shift is UBFM Xd, Xn, #shift, #63
     * Encoding: 1101 0011 01.. .... ...... nnnn nddd
     * imms = 63, immr = shift */
    uint32_t instr = 0xD340FC00 | (shift << 16) | (rn << 5) | rd;
    encode_word(buf, instr);
}

void gen_ret(unsigned char *buf) {
    encode_word(buf, 0xD65F03C0);
}

int64_t execute_code(unsigned char *code, size_t size) {
    size_t page_size = 4096;
    void *mem = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap");
        return -1;
    }

    memcpy(mem, code, size);

    if (mprotect(mem, page_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, page_size);
        return -1;
    }

    test_fn_t fn = (test_fn_t)mem;
    int64_t result = fn();

    munmap(mem, page_size);
    return result;
}

int main(void) {
    printf("=== Arithmetic JIT Test ===\n\n");

    /* Test: (+ (* 3 4) (- 10 5))
     * Tagged arithmetic:
     * - 3 tagged = 48 (0x30)
     * - 4 tagged = 64 (0x40)
     * - 10 tagged = 160 (0xA0)
     * - 5 tagged = 80 (0x50)
     *
     * (* 3 4):
     *   - Load 3 tagged -> x0
     *   - Load 4 tagged -> x1
     *   - LSR x0, x0, #4 (untag)
     *   - MUL x0, x0, x1 (result is tagged because x1 is tagged)
     *
     * (- 10 5):
     *   - Load 10 tagged -> x1
     *   - Load 5 tagged -> x2
     *   - SUB x1, x1, x2
     *
     * (+):
     *   - ADD x0, x0, x1
     *   - LSR x0, x0, #4 (untag for return)
     *   - RET
     */

    unsigned char code[256];
    int pos = 0;

    /* Load 3 tagged (48) into x0 */
    gen_movz(&code[pos], 0, 48);
    pos += 4;

    /* Load 4 tagged (64) into x1 */
    gen_movz(&code[pos], 1, 64);
    pos += 4;

    /* Untag x0: lsr x0, x0, #4 */
    gen_lsr(&code[pos], 0, 0, 4);
    pos += 4;

    /* Multiply: mul x0, x0, x1 */
    gen_mul(&code[pos], 0, 0, 1);
    pos += 4;

    /* Load 10 tagged (160) into x1 */
    gen_movz(&code[pos], 1, 160);
    pos += 4;

    /* Load 5 tagged (80) into x2 */
    gen_movz(&code[pos], 2, 80);
    pos += 4;

    /* Subtract: sub x1, x1, x2 */
    gen_sub(&code[pos], 1, 1, 2);
    pos += 4;

    /* Add: add x0, x0, x1 */
    gen_add(&code[pos], 0, 0, 1);
    pos += 4;

    /* Untag result: lsr x0, x0, #4 */
    gen_lsr(&code[pos], 0, 0, 4);
    pos += 4;

    /* Return */
    gen_ret(&code[pos]);
    pos += 4;

    printf("Generated %d bytes of code\n", pos);
    printf("Testing: (+ (* 3 4) (- 10 5))\n");
    printf("Expected: 17\n");

    int64_t result = execute_code(code, pos);
    printf("Result: %lld\n", result);

    if (result == 17) {
        printf("PASS - Arithmetic JIT works correctly!\n");
        return 0;
    } else {
        printf("FAIL - Expected 17, got %lld\n", result);
        return 1;
    }
}
