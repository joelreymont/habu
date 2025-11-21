/* Bootstrap Compiler - Tier 2: ARM64 Instruction Encoders
 *
 * Functions that generate ARM64 machine code instructions.
 * Each function takes operands and writes the encoded instruction
 * to a buffer.
 *
 * These encoders are building blocks for the code generator (Tier 4).
 */

#include <stdint.h>
#include <string.h>

/* ============================================
 * Data Movement Encoders
 * ============================================ */

/* encode_movz: MOVZ Xd, #imm
 * Move 16-bit immediate with zero extension
 * Encoding: 1101 0010 1... .... ...i iiii iiid dddd
 * Base: 0xD2800000 | (imm << 5) | rd
 */
void arm64_encode_movz(uint8_t *dest, uint8_t rd, uint16_t imm) {
    uint32_t base = 0xD2800000;
    uint32_t encoded = base | ((uint32_t)imm << 5) | rd;

    /* Write little-endian */
    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* encode_add: ADD Xd, Xn, Xm
 * Add two registers
 * Encoding: 1000 1011 000m mmmm 0000 00nn nnn d dddd
 * Base: 0x8B000000 | (rm << 16) | (rn << 5) | rd
 */
void arm64_encode_add(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t rm) {
    uint32_t base = 0x8B000000;
    uint32_t encoded = base | ((uint32_t)rm << 16) | ((uint32_t)rn << 5) | rd;

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* encode_sub: SUB Xd, Xn, Xm
 * Subtract two registers
 * Encoding: 1100 1011 000m mmmm 0000 00nn nnnd dddd
 * Base: 0xCB000000 | (rm << 16) | (rn << 5) | rd
 */
void arm64_encode_sub(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t rm) {
    uint32_t base = 0xCB000000;
    uint32_t encoded = base | ((uint32_t)rm << 16) | ((uint32_t)rn << 5) | rd;

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* encode_mul: MUL Xd, Xn, Xm
 * Multiply two registers
 * Encoding: 1001 1011 000m mmmm 0111 11nn nnnd dddd
 * Base: 0x9B007C00 | (rm << 16) | (rn << 5) | rd
 */
void arm64_encode_mul(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t rm) {
    uint32_t base = 0x9B007C00;
    uint32_t encoded = base | ((uint32_t)rm << 16) | ((uint32_t)rn << 5) | rd;

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* encode_lsr: LSR Xd, Xn, #shift
 * Logical shift right
 * Encoding: 1101 0011 0100 0000 1111 11nn nnnd dddd (for shift=4)
 * Base: 0xD3400000 | (63 << 10) | (shift << 16) | (rn << 5) | rd
 */
void arm64_encode_lsr(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t shift) {
    uint32_t base = 0xD3400000;
    uint32_t encoded = base | (63 << 10) | ((uint32_t)shift << 16) | ((uint32_t)rn << 5) | rd;

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* encode_lsl: LSL Xd, Xn, #shift
 * Logical shift left (using UBFM encoding)
 * immr = (-shift) mod 64, imms = 63 - shift
 * Base: 0xD3400000 | (immr << 16) | (imms << 10) | (rn << 5) | rd
 */
void arm64_encode_lsl(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t shift) {
    uint32_t base = 0xD3400000;
    uint32_t immr = (64 - shift) % 64;  /* -shift mod 64 */
    uint32_t imms = 63 - shift;
    uint32_t encoded = base | (immr << 16) | (imms << 10) | ((uint32_t)rn << 5) | rd;

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* ============================================
 * Memory Encoders
 * ============================================ */

/* encode_ldr: LDR Xt, [Xn, #offset]
 * Load register from memory
 * Encoding: 1111 1001 01ii iiii iiii iinn nnnt tttt
 * Base: 0xF9400000 | (imm12 << 10) | (rn << 5) | rt
 * offset must be multiple of 8, imm12 = offset/8
 */
void arm64_encode_ldr(uint8_t *dest, uint8_t rt, uint8_t rn, uint16_t offset) {
    uint32_t base = 0xF9400000;
    uint32_t imm12 = offset / 8;  /* offset in 8-byte units */
    uint32_t encoded = base | (imm12 << 10) | ((uint32_t)rn << 5) | rt;

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* encode_str: STR Xt, [Xn, #offset]
 * Store register to memory
 * Encoding: 1111 1001 00ii iiii iiii iinn nnnt tttt
 * Base: 0xF9000000 | (imm12 << 10) | (rn << 5) | rt
 * offset must be multiple of 8, imm12 = offset/8
 */
void arm64_encode_str(uint8_t *dest, uint8_t rt, uint8_t rn, uint16_t offset) {
    uint32_t base = 0xF9000000;
    uint32_t imm12 = offset / 8;  /* offset in 8-byte units */
    uint32_t encoded = base | (imm12 << 10) | ((uint32_t)rn << 5) | rt;

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* encode_stp: STP Xt1, Xt2, [Xn, #imm]!
 * Store pair with pre-increment
 * Encoding: 1010 1001 1iii iiii iitt ttt2 222n nnnn
 * Base: 0xA9800000 | (imm7 << 15) | (rt2 << 10) | (rn << 5) | rt1
 * imm is in 8-byte units (signed 7-bit)
 */
void arm64_encode_stp(uint8_t *dest, uint8_t rt1, uint8_t rt2, uint8_t rn, int16_t imm) {
    uint32_t base = 0xA9800000;
    uint32_t imm7 = (imm / 8) & 0x7F;  /* offset in 8-byte units, 7 bits */
    uint32_t encoded = base | (imm7 << 15) | ((uint32_t)rt2 << 10) | ((uint32_t)rn << 5) | rt1;

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* encode_ldp: LDP Xt1, Xt2, [Xn], #imm
 * Load pair with post-increment
 * Encoding: 1010 1000 11ii iiii iitt tttn nnnn 1111
 * Base: 0xA8C00000 | (imm7 << 15) | (rt2 << 10) | (rn << 5) | rt1
 * imm is in 8-byte units (signed 7-bit)
 */
void arm64_encode_ldp(uint8_t *dest, uint8_t rt1, uint8_t rt2, uint8_t rn, int16_t imm) {
    uint32_t base = 0xA8C00000;
    uint32_t imm7 = (imm / 8) & 0x7F;  /* offset in 8-byte units, 7 bits */
    uint32_t encoded = base | (imm7 << 15) | ((uint32_t)rt2 << 10) | ((uint32_t)rn << 5) | rt1;

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* ============================================
 * Control Flow Encoders
 * ============================================ */

/* encode_b: B <label>
 * Unconditional branch
 * Encoding: 0001 01ii iiii iiii iiii iiii iiii iiii
 * Base: 0x14000000 | (offset & 0x03FFFFFF)
 * offset is signed 26-bit in instruction units
 */
void arm64_encode_b(uint8_t *dest, int32_t offset) {
    uint32_t base = 0x14000000;
    uint32_t encoded = base | (offset & 0x03FFFFFF);

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* encode_bl: BL <label>
 * Branch with link (function call)
 * Encoding: 1001 01ii iiii iiii iiii iiii iiii iiii
 * Base: 0x94000000 | (offset & 0x03FFFFFF)
 * offset is signed 26-bit in instruction units
 */
void arm64_encode_bl(uint8_t *dest, int32_t offset) {
    uint32_t base = 0x94000000;
    uint32_t encoded = base | (offset & 0x03FFFFFF);

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* encode_ret: RET
 * Return from subroutine
 * Encoding: 1101 0110 0101 1111 0000 0011 1100 0000
 * Fixed encoding: 0xD65F03C0
 */
void arm64_encode_ret(uint8_t *dest) {
    dest[0] = 0xC0;
    dest[1] = 0x03;
    dest[2] = 0x5F;
    dest[3] = 0xD6;
}

/* encode_mov: MOV Xd, Xm
 * Move register to register
 * Implemented as ORR Xd, XZR, Xm
 * Encoding: 1010 1010 000m mmmm 0000 0011 111d dddd
 * Base: 0xAA0003E0 | (rm << 16) | rd
 */
void arm64_encode_mov(uint8_t *dest, uint8_t rd, uint8_t rm) {
    uint32_t base = 0xAA0003E0;
    uint32_t encoded = base | ((uint32_t)rm << 16) | rd;

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* encode_cmp: CMP Xn, Xm
 * Compare registers (sets flags)
 * Implemented as SUBS XZR, Xn, Xm
 * Encoding: 1110 1011 000m mmmm 0000 00nn nnn1 1111
 * Base: 0xEB00001F | (rm << 16) | (rn << 5)
 */
void arm64_encode_cmp(uint8_t *dest, uint8_t rn, uint8_t rm) {
    uint32_t base = 0xEB00001F;
    uint32_t encoded = base | ((uint32_t)rm << 16) | ((uint32_t)rn << 5);

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* encode_cset: CSET Xd, cond
 * Conditional set (1 if condition true, else 0)
 * Implemented as CSINC Xd, XZR, XZR, invert(cond)
 * Encoding: 1001 1010 1001 1111 cccc 0111 111d dddd
 * Base: 0x9A9F07E0 | (inverted_cond << 12) | rd
 * Condition codes: EQ=0, NE=1, LT=11, LE=13, GT=12, GE=10
 */
void arm64_encode_cset(uint8_t *dest, uint8_t rd, uint8_t cond) {
    uint32_t base = 0x9A9F07E0;
    uint8_t inverted_cond = cond ^ 1;  /* XOR with 1 to invert */
    uint32_t encoded = base | ((uint32_t)inverted_cond << 12) | rd;

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* encode_and: AND Xd, Xn, #imm
 * Bitwise AND with immediate (for tag extraction)
 * For imm=0xF (common case for tag extraction):
 * Encoding: 1001 0010 0100 0000 0011 11nn nnnd dddd
 * Base: 0x92400C00 | (rn << 5) | rd
 *
 * NOTE: This is specifically for AND with #0xF
 * General immediate encoding for AND is complex
 */
void arm64_encode_and_imm_0xF(uint8_t *dest, uint8_t rd, uint8_t rn) {
    uint32_t base = 0x92400C00;  /* Verified for AND x, x, #0xF */
    uint32_t encoded = base | ((uint32_t)rn << 5) | rd;

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* encode_cmp_imm: CMP Xn, #imm
 * Compare register with immediate
 * Implemented as SUBS XZR, Xn, #imm
 * Encoding: 1111 0001 00ii iiii iiii iinn nnn1 1111
 * Base: 0xF100001F | (imm << 10) | (rn << 5)
 * imm is 12-bit unsigned
 */
void arm64_encode_cmp_imm(uint8_t *dest, uint8_t rn, uint16_t imm) {
    uint32_t base = 0xF100001F;
    uint32_t encoded = base | ((uint32_t)imm << 10) | ((uint32_t)rn << 5);

    dest[0] = (encoded >> 0) & 0xFF;
    dest[1] = (encoded >> 8) & 0xFF;
    dest[2] = (encoded >> 16) & 0xFF;
    dest[3] = (encoded >> 24) & 0xFF;
}

/* Encoder function sizes (all are 4 bytes per instruction) */
const size_t ARM64_INSTR_SIZE = 4;
