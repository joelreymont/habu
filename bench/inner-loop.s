// inner-loop.s — Phase 0.3 native baseline (the ceiling a caf backend targets).
// Hand-written ARM64: the EXACT instructions caf's codegen will emit for the
// serial LCG step  x = x*A + C  (wrapping 64-bit), 1e9 iterations. No C, no
// optimizer — this is the real native instruction sequence, not clang's view.
// Assembled/linked by clang (assembler+linker role only; the source is pure
// asm). Exits with the low byte of the result for a correctness check.
.global _main
.align 2
_main:
    mov   x9,  #1                  // x = seed 1
    movz  x10, #0x7F2D             // A = 0x5851F42D4C957F2D
    movk  x10, #0x4C95, lsl #16
    movk  x10, #0xF42D, lsl #32
    movk  x10, #0x5851, lsl #48
    movz  x11, #0x814F             // C = 0x14057B7EF767814F
    movk  x11, #0xF767, lsl #16
    movk  x11, #0x7B7E, lsl #32
    movk  x11, #0x1405, lsl #48
    movz  x13, #0xCA00             // ITERS = 1e9 = 0x3B9ACA00
    movk  x13, #0x3B9A, lsl #16
    mov   x12, xzr                 // i = 0
1:
    mul   x14, x9, x10             // x*A
    add   x9,  x14, x11            // + C   (serial dependency = the hot path)
    add   x12, x12, #1
    cmp   x12, x13
    b.lo  1b
    and   x0,  x9, #0xff           // exit(low byte of result) — correctness probe
    mov   x16, #1                  // SYS_exit
    svc   #0x80
