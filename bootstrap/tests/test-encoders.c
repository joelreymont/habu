/* Test ARM64 instruction encoders */

#include <stdio.h>
#include <stdint.h>
#include <string.h>

/* Include encoder declarations */
void arm64_encode_movz(uint8_t *dest, uint8_t rd, uint16_t imm);
void arm64_encode_add(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t rm);
void arm64_encode_sub(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t rm);
void arm64_encode_mul(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t rm);
void arm64_encode_lsr(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t shift);
void arm64_encode_lsl(uint8_t *dest, uint8_t rd, uint8_t rn, uint8_t shift);
void arm64_encode_ldr(uint8_t *dest, uint8_t rt, uint8_t rn, uint16_t offset);
void arm64_encode_str(uint8_t *dest, uint8_t rt, uint8_t rn, uint16_t offset);
void arm64_encode_stp(uint8_t *dest, uint8_t rt1, uint8_t rt2, uint8_t rn, int16_t imm);
void arm64_encode_ldp(uint8_t *dest, uint8_t rt1, uint8_t rt2, uint8_t rn, int16_t imm);
void arm64_encode_b(uint8_t *dest, int32_t offset);
void arm64_encode_bl(uint8_t *dest, int32_t offset);
void arm64_encode_ret(uint8_t *dest);
void arm64_encode_cmp(uint8_t *dest, uint8_t rn, uint8_t rm);
void arm64_encode_cset(uint8_t *dest, uint8_t rd, uint8_t cond);
void arm64_encode_and_imm_0xF(uint8_t *dest, uint8_t rd, uint8_t rn);
void arm64_encode_cmp_imm(uint8_t *dest, uint8_t rn, uint16_t imm);

/* Helper to compare byte arrays */
int compare_bytes(const char *name, uint8_t *actual, uint8_t *expected, int len) {
    for (int i = 0; i < len; i++) {
        if (actual[i] != expected[i]) {
            printf("  %s: ✗ mismatch at byte %d: got 0x%02X, expected 0x%02X\n",
                   name, i, actual[i], expected[i]);
            return 0;
        }
    }
    printf("  %s: ✓\n", name);
    return 1;
}

int main(void) {
    int passed = 0;
    int failed = 0;
    uint8_t buf[4];

    printf("\n=== Tier 2 Encoder Tests ===\n\n");

    /* Test MOVZ */
    printf("Testing MOVZ:\n");
    arm64_encode_movz(buf, 0, 0x50);  /* movz x0, #0x50 */
    uint8_t expected_movz[] = {0x00, 0x0A, 0x80, 0xD2};
    if (compare_bytes("movz x0, #0x50", buf, expected_movz, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test ADD */
    printf("\nTesting ADD:\n");
    arm64_encode_add(buf, 0, 0, 1);  /* add x0, x0, x1 */
    uint8_t expected_add[] = {0x00, 0x00, 0x01, 0x8B};
    if (compare_bytes("add x0, x0, x1", buf, expected_add, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test SUB */
    printf("\nTesting SUB:\n");
    arm64_encode_sub(buf, 0, 0, 1);  /* sub x0, x0, x1 */
    uint8_t expected_sub[] = {0x00, 0x00, 0x01, 0xCB};
    if (compare_bytes("sub x0, x0, x1", buf, expected_sub, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test MUL */
    printf("\nTesting MUL:\n");
    arm64_encode_mul(buf, 0, 0, 1);  /* mul x0, x0, x1 */
    uint8_t expected_mul[] = {0x00, 0x7C, 0x01, 0x9B};
    if (compare_bytes("mul x0, x0, x1", buf, expected_mul, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test LSR */
    printf("\nTesting LSR:\n");
    arm64_encode_lsr(buf, 0, 0, 4);  /* lsr x0, x0, #4 */
    uint8_t expected_lsr[] = {0x00, 0xFC, 0x44, 0xD3};
    if (compare_bytes("lsr x0, x0, #4", buf, expected_lsr, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test LSL */
    printf("\nTesting LSL:\n");
    arm64_encode_lsl(buf, 0, 0, 4);  /* lsl x0, x0, #4 */
    uint8_t expected_lsl[] = {0x00, 0xEC, 0x7C, 0xD3};
    if (compare_bytes("lsl x0, x0, #4", buf, expected_lsl, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test LDR */
    printf("\nTesting LDR:\n");
    arm64_encode_ldr(buf, 0, 31, 0);  /* ldr x0, [sp, #0] */
    uint8_t expected_ldr[] = {0xE0, 0x03, 0x40, 0xF9};
    if (compare_bytes("ldr x0, [sp, #0]", buf, expected_ldr, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test STR */
    printf("\nTesting STR:\n");
    arm64_encode_str(buf, 0, 31, 0);  /* str x0, [sp, #0] */
    uint8_t expected_str[] = {0xE0, 0x03, 0x00, 0xF9};
    if (compare_bytes("str x0, [sp, #0]", buf, expected_str, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test STP */
    printf("\nTesting STP:\n");
    arm64_encode_stp(buf, 29, 30, 31, -16);  /* stp x29, x30, [sp, #-16]! */
    uint8_t expected_stp[] = {0xFD, 0x7B, 0xBF, 0xA9};
    if (compare_bytes("stp x29, x30, [sp, #-16]!", buf, expected_stp, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test LDP */
    printf("\nTesting LDP:\n");
    arm64_encode_ldp(buf, 29, 30, 31, 16);  /* ldp x29, x30, [sp], #16 */
    uint8_t expected_ldp[] = {0xFD, 0x7B, 0xC1, 0xA8};
    if (compare_bytes("ldp x29, x30, [sp], #16", buf, expected_ldp, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test B */
    printf("\nTesting B:\n");
    arm64_encode_b(buf, 2);  /* b #8 (2 instructions forward) */
    uint8_t expected_b[] = {0x02, 0x00, 0x00, 0x14};
    if (compare_bytes("b #8", buf, expected_b, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test BL */
    printf("\nTesting BL:\n");
    arm64_encode_bl(buf, 2);  /* bl #8 (2 instructions forward) */
    uint8_t expected_bl[] = {0x02, 0x00, 0x00, 0x94};
    if (compare_bytes("bl #8", buf, expected_bl, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test RET */
    printf("\nTesting RET:\n");
    arm64_encode_ret(buf);  /* ret */
    uint8_t expected_ret[] = {0xC0, 0x03, 0x5F, 0xD6};
    if (compare_bytes("ret", buf, expected_ret, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test CMP */
    printf("\nTesting CMP:\n");
    arm64_encode_cmp(buf, 0, 1);  /* cmp x0, x1 */
    uint8_t expected_cmp[] = {0x1F, 0x00, 0x01, 0xEB};
    if (compare_bytes("cmp x0, x1", buf, expected_cmp, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test CSET */
    printf("\nTesting CSET:\n");
    arm64_encode_cset(buf, 0, 0);  /* cset x0, eq (cond=0) */
    uint8_t expected_cset[] = {0xE0, 0x17, 0x9F, 0x9A};
    if (compare_bytes("cset x0, eq", buf, expected_cset, 4)) {
        passed++;
    } else {
        failed++;
    }

    arm64_encode_cset(buf, 0, 11);  /* cset x0, lt (cond=11) */
    uint8_t expected_cset_lt[] = {0xE0, 0xA7, 0x9F, 0x9A};
    if (compare_bytes("cset x0, lt", buf, expected_cset_lt, 4)) {
        passed++;
    } else {
        failed++;
    }

    arm64_encode_cset(buf, 0, 12);  /* cset x0, gt (cond=12) */
    uint8_t expected_cset_gt[] = {0xE0, 0xD7, 0x9F, 0x9A};
    if (compare_bytes("cset x0, gt", buf, expected_cset_gt, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test AND with #0xF */
    printf("\nTesting AND:\n");
    arm64_encode_and_imm_0xF(buf, 0, 0);  /* and x0, x0, #0xF */
    uint8_t expected_and[] = {0x00, 0x0C, 0x40, 0x92};
    if (compare_bytes("and x0, x0, #0xF", buf, expected_and, 4)) {
        passed++;
    } else {
        failed++;
    }

    /* Test CMP with immediate */
    printf("\nTesting CMP with immediate:\n");
    arm64_encode_cmp_imm(buf, 0, 0);  /* cmp x0, #0 */
    uint8_t expected_cmp_imm[] = {0x1F, 0x00, 0x00, 0xF1};
    if (compare_bytes("cmp x0, #0", buf, expected_cmp_imm, 4)) {
        passed++;
    } else {
        failed++;
    }

    arm64_encode_cmp_imm(buf, 0, 1);  /* cmp x0, #1 */
    uint8_t expected_cmp_imm1[] = {0x1F, 0x04, 0x00, 0xF1};
    if (compare_bytes("cmp x0, #1", buf, expected_cmp_imm1, 4)) {
        passed++;
    } else {
        failed++;
    }

    printf("\n%d passed, %d failed\n\n", passed, failed);

    if (failed == 0) {
        printf("✓ All encoder tests passing!\n\n");
        return 0;
    } else {
        printf("✗ Some tests failed\n\n");
        return 1;
    }
}
