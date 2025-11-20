.section __TEXT,__text,regular,pure_instructions
.globl _main
.p2align 2

_main:
    ; Binary operation: 6 * 7
    mov x1, #96
    mov x2, #112
    lsr x1, x1, #4  ; Untag first arg
    mul x0, x1, x2
    ; Untag fixnum for exit code
    lsr x0, x0, #4
    ret
