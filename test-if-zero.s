.globl _main
.align 4

_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    movz x0, #0
    cmp x0, xzr
    b.eq else

    movz x0, #672
    b end

else:
    movz x0, #1584

end:
    lsr x0, x0, #4

    mov sp, x29
    ldp x29, x30, [sp], #16
    ret
