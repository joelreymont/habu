.globl _main
.align 4

_main:
    movz x0, #42
    b skip          /* Branch forward 1 instruction */
    movz x0, #99    /* Should be skipped */
skip:
    ret

test2:
    movz x0, #5
    movz x1, #5
    cmp x0, x1
    b.ne else       /* Branch if not equal */
    movz x0, #1
    b end
else:
    movz x0, #0
end:
    ret
