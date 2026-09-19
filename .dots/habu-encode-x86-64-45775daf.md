---
title: Encode x86-64 immediate memory stores
status: active
priority: 2
issue-type: task
created-at: "2026-09-19T18:30:02.114409+03:00"
---

Reconciliation c191257a: the experimental STORE-I32 encodes C7 /0 imm32 stores, including RIP-relative destinations; current X64ASM has register stores only. Supply this form in the existing BUF encoder for direct constant stores, with llvm-mc differential vectors and the displacement measured from the full instruction end. Ownership: alder, src/arch/x86-64/asm.f and test/compiler/x86-64-asm.f. Claim: .jj-ws/alder-x64-encoders.

Measured with `llvm-mc -triple=x86_64 -show-encoding` (AT&T syntax):

| Vector | Bytes |
| --- | --- |
| `movl $0x12345678, 16(%rip)` | `c7051000000078563412` |
| `movl $-1, -129(%r13,%r12,8)` | `43c784e57fffffffffffffff` |

Acceptance: write exactly 32 bits; check the immediate with the existing
imm32 contract; cover zero/disp8/disp32 and extended base/index operands.
The RIP-relative displacement precedes four immediate bytes, so relocation
must use the complete instruction length, not the displacement field's end.
Run compiler-x86-64-asm; any relocation integration also pins its target.
