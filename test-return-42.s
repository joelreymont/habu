; Test executable - return 42
; Generated machine code: mov eax, 672 (42 << 4); ret
;
; In x86_64 assembly:

.section __TEXT,__text,regular,pure_instructions
.globl _main
.p2align 4

_main:
    ; mov eax, 672 (0x2A0) - this is 42 as a tagged fixnum (42 << 4)
    ; Opcode: B8 A0 02 00 00
    .byte 0xB8, 0xA0, 0x02, 0x00, 0x00

    ; Shift right by 4 to untag the fixnum for exit code
    shr rax, 4

    ; ret
    .byte 0xC3

