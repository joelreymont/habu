---
title: Add x86-64 runtime moves and per-target DSTACK
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.526025+03:00"
blocks:
  - habu-add-the-x86-aad02c7e
---

Problem: `src/habu/layout.f:34` `ENGINE-GPR:DSTACK` is the shared `19` (consumed by `src/arch/arm64/machine.f:84,150` and `src/habu/rt.f:23`; the x86 side already has `X64IR:R-DSP 12` and `X64M:DSTACK-GPR`, `src/compiler/native/x64ir.f:180,346`), and the x86 data-stack moves have no home: the seam's `G-POP`/`G-PUSH` take x86 register numbers and `test/x86-64-emit.f` carries recording stubs (cross-build obligation (5)).
Acceptance: `ENGINE-GPR` selects `DSTACK` (`12`) and the reserved mask (`rbx rbp r12-r15`) on `HB-TARGET-LINUX-X86-64?`, fail-closed otherwise; `src/arch/x86-64/rt.f`: `G-PUSH`/`G-POP` with x86 register numbers (7 rdi, 6 rsi, 2 rdx, 0 rax), the byte-appending stencil consumer (`C-EMIT-STENCIL`'s x86 twin), an `RT:DSTACK-AGREE` twin against `X64IR:R-DSP`; `test/x86-64-emit.f` drops its recording stubs; `layout.f` also declares `ENGINE-MAIN:XT-CELL`, a DATA cell declared like `NCOMP-DISPATCH:XT-CELL` (`src/compiler/native/compiler.f:646`) and unused until a boot reads it (I10c on ARM64, X4d on x86); the ARM64 engine byte-identical (chain). Discharges cross-build obligation (5).
Files: `src/habu/layout.f`, new `src/arch/x86-64/rt.f`, `test/x86-64-emit.f`.
Verify: spark `bin/hb --load test/x86-64-emit.f`; rebuild; chain gen2==gen3; gate.
Depends: habu-add-the-x86-aad02c7e (K1). Serialise with P2 on `layout.f`.
Route: Alder (shared: src/habu/layout.f).
Ownership: krait (Intel lane).
Claim: unassigned.
