// dispatch.s — native baseline for the dispatch-bound bench. Hand-written ARM64:
// the instructions caf's backend will emit for the xorshift byte-mix. The 3
// `dup<<n xor` Forth ops each become ONE shifted-EOR. Seed 1, buf[i]=i&0xff.
// Exits with low byte of the hash for a cross-check.
.global _main
.align 2
.section __DATA,__bss
.align 4
buf: .space 65536
.section __TEXT,__text
.align 2
_main:
    adrp x9, buf@PAGE
    add  x9, x9, buf@PAGEOFF      // x9 = buf
    mov  x13, #65536              // BUFLEN
    // fill buf[i] = i & 0xff
    mov  x11, #0
Lfill:
    and  w15, w11, #0xff
    strb w15, [x9, x11]
    add  x11, x11, #1
    cmp  x11, x13
    b.lo Lfill
    // hash
    mov  x10, #1                  // h = 1
    movz x14, #0x3A98             // PASSES = 15000
    mov  x12, #0                  // pass
Lpass:
    mov  x11, #0                  // i
Lbyte:
    ldrb w15, [x9, x11]           // b = buf[i]
    add  x10, x10, x15            // h += b
    eor  x10, x10, x10, lsl #13   // h ^= h<<13
    eor  x10, x10, x10, lsr #7    // h ^= h>>7
    eor  x10, x10, x10, lsl #17   // h ^= h<<17
    add  x11, x11, #1
    cmp  x11, x13
    b.lo Lbyte
    add  x12, x12, #1
    cmp  x12, x14
    b.lo Lpass
    and  x0, x10, #0xff
    mov  x16, #1
    svc  #0x80
