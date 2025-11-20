.global _main
.align 4

_main:
    movz x0, #0
    cmp x0, xzr
    cset x0, EQ
    ret
