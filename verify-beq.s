.text
.global _main
.align 2

_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    
    movz x0, #16
    cmp x0, xzr
    b.eq skip
    
    movz x0, #672
    b end
    
skip:
    movz x0, #0
    
end:
    lsr x0, x0, #4
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret
