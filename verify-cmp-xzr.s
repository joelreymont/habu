.globl _main
.align 4

_main:
    movz x0, #42
    cmp x0, xzr
    b.eq else
    movz x0, #1
    b end
else:
    movz x0, #0
end:
    ret
