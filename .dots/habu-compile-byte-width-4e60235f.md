---
title: Compile byte-width memory access in the native chain
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-01T11:58:25.955829+02:00\""
---

Phase 1 built the cell-width load and store (hir.load, hir.store, a64.aldr, a64.astr). BYTE-SUM and BYTE-FIND use c@, which is a byte load with zero extension - asm.f already has ENC-LDRB and ENC-STRB with their own unscaled twelve-bit field. Wanted: a width on the HIR memory ops and on the two machine forms, the c@ and c! word-model rows, the emitter arms, and the mutation that a byte access encoded at cell width dies. Depends on the cell-width memory leaf (habu-compile-mem-access-64ae47d3).

Claim: agent=bytelane workspace=.jj-ws/habu-compile-byte-width-4e60235f
