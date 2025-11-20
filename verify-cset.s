.global _main
.align 4

_main:
    /* Test NE (not equal) */
    movz x0, #5
    movz x1, #3
    cmp x0, x1
    cset x0, NE    /* Should set x0 to 1 if NE */
    ret
