.section __TEXT,__text
.globl _main
.p2align 2

_main:
    movz x0, #672
    lsr x0, x0, #4
    ret
