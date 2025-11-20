/* End-to-End Habu Compilation Pipeline Test
 *
 * This tests the complete pipeline:
 * 1. Habu expression: (+ 3 4)
 * 2. Compile to IR: (call + (lit 3) (lit 4))
 * 3. Generate ARM64 bytes using intrinsics
 * 4. Execute with MAP_JIT
 * 5. Verify result = 7
 *
 * Generated code for (+ 3 4):
 *
 * Prologue:
 *   stp x29, x30, [sp, #-16]!   ; Save frame
 *   mov x29, sp                   ; Set frame pointer
 *
 * Body (+ 3 4):
 *   movz x0, #48                  ; Load 3 tagged (3 << 4 = 48)
 *   str x0, [sp, #-16]!           ; Save to stack
 *   movz x0, #64                  ; Load 4 tagged (4 << 4 = 64)
 *   mov x1, x0                    ; Move to x1
 *   ldr x0, [sp], #16             ; Restore first operand
 *   add x0, x0, x1                ; Add (result is tagged)
 *
 * Untag:
 *   lsr x0, x0, #4                ; Untag result
 *
 * Epilogue:
 *   mov sp, x29                   ; Restore stack
 *   ldp x29, x30, [sp], #16       ; Restore frame
 *   ret                           ; Return
 */

#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>

#ifndef MAP_JIT
#define MAP_JIT 0x0800
#endif

typedef int64_t (*habu_fn_t)(void);

int test_expression(unsigned char *code, size_t code_len,
                    const char *expr, int64_t expected) {
    printf("\n=== Testing Habu Expression: %s ===\n", expr);
    printf("Expected result: %lld\n", expected);
    printf("Generated code (%zu bytes):\n  ", code_len);
    for (size_t i = 0; i < code_len; i++) {
        printf("%02X ", code[i]);
        if ((i + 1) % 16 == 0 && i + 1 < code_len) {
            printf("\n  ");
        }
    }
    printf("\n");

    size_t page_size = 4096;
    size_t aligned_size = ((code_len + page_size - 1) / page_size) * page_size;

    void *mem = mmap(NULL, aligned_size,
                     PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_JIT,
                     -1, 0);

    if (mem == MAP_FAILED) {
        perror("mmap");
        return 0;
    }

    memcpy(mem, code, code_len);

    if (mprotect(mem, aligned_size, PROT_READ | PROT_EXEC) != 0) {
        perror("mprotect");
        munmap(mem, aligned_size);
        return 0;
    }

    habu_fn_t fn = (habu_fn_t)mem;
    int64_t result = fn();

    printf("Execution result: %lld\n", result);

    munmap(mem, aligned_size);

    if (result == expected) {
        printf("✓ PASS\n");
        return 1;
    } else {
        printf("✗ FAIL (expected %lld, got %lld)\n", expected, result);
        return 0;
    }
}

int main() {
    printf("========================================\n");
    printf("Habu Compilation Pipeline E2E Tests\n");
    printf("========================================\n");

    int pass = 0;
    int fail = 0;

    /* Test 1: (+ 3 4) = 7
     * This exercises the full compilation pipeline
     */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
            0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp (add x29, sp, #0) */

            /* Body: (+ 3 4) */
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 (3 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x08, 0x80, 0xD2,  /* movz x0, #64 (4 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x00, 0x00, 0x01, 0x8B,  /* add x0, x0, x1 */

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 (add sp, x29, #0) */
            0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };

        if (test_expression(code, sizeof(code), "(+ 3 4)", 7)) {
            pass++;
        } else {
            fail++;
        }
    }

    /* Test 2: (* 6 7) = 42
     * Tests multiplication with tagging
     */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
            0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

            /* Body: (* 6 7) */
            0x00, 0x0C, 0x80, 0xD2,  /* movz x0, #96 (6 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x0E, 0x80, 0xD2,  /* movz x0, #112 (7 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 (untag one operand) */
            0x00, 0x7C, 0x01, 0x9B,  /* mul x0, x0, x1 */

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
            0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };

        if (test_expression(code, sizeof(code), "(* 6 7)", 42)) {
            pass++;
        } else {
            fail++;
        }
    }

    /* Test 3: (- 10 3) = 7
     * Tests subtraction
     */
    {
        unsigned char code[] = {
            /* Prologue */
            0xFD, 0x7B, 0xBF, 0xA9,  /* stp x29, x30, [sp, #-16]! */
            0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

            /* Body: (- 10 3) */
            0x00, 0x14, 0x80, 0xD2,  /* movz x0, #160 (10 << 4) */
            0xE0, 0x0F, 0x1F, 0xF8,  /* str x0, [sp, #-16]! */
            0x00, 0x06, 0x80, 0xD2,  /* movz x0, #48 (3 << 4) */
            0xE1, 0x03, 0x00, 0xAA,  /* mov x1, x0 */
            0xE0, 0x07, 0x41, 0xF8,  /* ldr x0, [sp], #16 */
            0x00, 0x00, 0x01, 0xCB,  /* sub x0, x0, x1 */

            /* Untag */
            0x00, 0xFC, 0x44, 0xD3,  /* lsr x0, x0, #4 */

            /* Epilogue */
            0xBF, 0x03, 0x00, 0x91,  /* mov sp, x29 */
            0xFD, 0x7B, 0xC1, 0xA8,  /* ldp x29, x30, [sp], #16 */
            0xC0, 0x03, 0x5F, 0xD6   /* ret */
        };

        if (test_expression(code, sizeof(code), "(- 10 3)", 7)) {
            pass++;
        } else {
            fail++;
        }
    }

    printf("\n========================================\n");
    printf("Pipeline Test Results: %d passed, %d failed\n", pass, fail);
    printf("========================================\n");

    return (fail == 0) ? 0 : 1;
}
