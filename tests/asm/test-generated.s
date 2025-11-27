.section __TEXT,__text,regular,pure_instructions
.globl _main
.p2align 2

_main:
    ; Load literal 42 (tagged: 672)
    mov x0, #672
    ; Untag fixnum for exit code
    lsr x0, x0, #4
    ret
