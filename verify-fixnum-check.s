.global _main
.align 4

_main:
    movz x0, #672      /* 42 << 4 */
    movz x1, #15
    and x1, x0, x1
    cmp x1, xzr
    cset x0, EQ
    lsl x0, x0, #4
    lsr x0, x0, #4
    ret
