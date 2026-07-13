---
title: Grow ICODE fixup capacity
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T03:48:25.990835+02:00\""
---

Full context: src/arch/arm64/icode.f defines ICODE-TAB-CELLS=0x1000 and NFX appends one entry per forward fixup. The owner AOT validation emitter crosses 4096 and the exact full debug compile fails closed after compiler fixpoint with 'icode: out of fixups' / wrapper rc67. Implement architecturally synchronized native/bootstrap capacity growth, add a boundary regression proving the retired edge and new bound, measure the new watermark, and preserve table bounds/ABI parity. Dependency: blocks habu-owner-seal-persist-1f23e205.
