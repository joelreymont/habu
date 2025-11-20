.global _main
.align 4

_main:
    movz x0, #10
    movz x1, #3
    mov x2, x0
    udiv x0, x0, x1
    msub x0, x0, x1, x2
    ret
