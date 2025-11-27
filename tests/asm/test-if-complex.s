.global _main
.align 4

_main:
    /* Prologue */
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    /* Test: (> 8 3) */
    movz x0, #128          /* 8 << 4 */
    str x0, [sp, #-16]!
    movz x0, #48           /* 3 << 4 */
    mov x1, x0
    ldr x0, [sp], #16
    cmp x0, x1
    cset x0, GT
    lsl x0, x0, #4         /* tag result */

    /* Compare with zero */
    cmp x0, xzr

    /* b.eq else */
    b.eq else

    /* Then: (+ 10 5) = 15 */
then:
    movz x0, #160          /* 10 << 4 */
    str x0, [sp, #-16]!
    movz x0, #80           /* 5 << 4 */
    mov x1, x0
    ldr x0, [sp], #16
    add x0, x0, x1

    /* b end */
    b end

    /* Else: (* 2 3) = 6 */
else:
    movz x0, #32           /* 2 << 4 */
    str x0, [sp, #-16]!
    movz x0, #48           /* 3 << 4 */
    mov x1, x0
    ldr x0, [sp], #16
    lsr x0, x0, #4         /* untag for mul */
    mul x0, x0, x1

    /* Untag result */
end:
    lsr x0, x0, #4

    /* Epilogue */
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret
