.global _main
.align 4

_main:
    movz x0, #5
    movz x1, #3
    cmp x0, x1
    cset x0, EQ    /* Should set x0=0 since 5 != 3 */
    
    movz x2, #5
    movz x3, #5
    cmp x2, x3
    cset x2, EQ    /* Should set x2=1 since 5 == 5 */
    
    ret
