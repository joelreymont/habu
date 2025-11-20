.section __TEXT,__text,regular,pure_instructions
.globl _main
.p2align 2

_main:
    ; Binary operation: 10 + 15
    mov x1, #160
    mov x2, #240
    add x0, x1, x2
    ; Untag fixnum for exit code
    lsr x0, x0, #4
    ret
