; ARM64 test - return 42 as tagged fixnum
; Tagged fixnum format: value << 4
; 42 << 4 = 672 = 0x2A0

.section __TEXT,__text,regular,pure_instructions
.globl _main
.p2align 2

_main:
    ; Load tagged fixnum 42 into x0 (return register)
    mov x0, #672        ; 42 << 4

    ; Untag for exit code: shift right by 4
    lsr x0, x0, #4

    ; Return
    ret
