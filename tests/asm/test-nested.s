.section __TEXT,__text,regular,pure_instructions
.globl _main
.p2align 2

_main:
    ; Set up stack frame
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    ; Binary op: +
    ; Literal: 1
    mov x0, #16
    ; Save left operand
    str x0, [sp, #-16]!
    ; Binary op: +
    ; Literal: 2
    mov x0, #32
    ; Save left operand
    str x0, [sp, #-16]!
    ; Literal: 3
    mov x0, #48
    mov x2, x0
    ; Restore left operand
    ldr x1, [sp], #16
    ; Add: x1 + x2
    add x0, x1, x2
    mov x2, x0
    ; Restore left operand
    ldr x1, [sp], #16
    ; Add: x1 + x2
    add x0, x1, x2
    ; Tear down stack frame
    mov sp, x29
    ldp x29, x30, [sp], #16
    ; Untag fixnum for exit code
    lsr x0, x0, #4
    ret
