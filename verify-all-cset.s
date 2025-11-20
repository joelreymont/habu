.global _main
.align 4

_main:
    /* NE */
    movz x0, #5
    movz x1, #3
    cmp x0, x1
    cset x0, NE
    
    /* LE */
    movz x0, #5
    movz x1, #10
    cmp x0, x1
    cset x0, LE
    
    /* GE */
    movz x0, #10
    movz x1, #5
    cmp x0, x1
    cset x0, GE
    
    ret
