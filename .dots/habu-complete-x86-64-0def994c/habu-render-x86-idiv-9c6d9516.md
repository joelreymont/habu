---
title: Render x86 idiv with Habu semantics
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.451004+03:00"
blocks:
  - habu-render-x86-trap-33eba82b
---

Problem: `src/compiler/native/emit-x64.f:40-51` refuses `idiv`; Habu requires `MIN-N -1 /` to give `MIN-N` and a zero divisor to throw `E-DIV-ZERO` (`src/habu/arith-abi.f`; `docs/forth-card.md:184-186`).
Acceptance: quotient and remainder over every sign combination; a zero divisor reaches `E-DIV-ZERO` through `x64.throw-entry`; `MIN-N -1` gives `(MIN-N, 0)`; the RDX early-clobber hazard is tested through allocation plus render, not bytes alone; executed natively on the ThinkPad.
Files: `src/compiler/native/emit-x64.f`, `src/compiler/native/select-x64.f` if the copy shape changes, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f` and `bin/hb --load test/compiler/x64-chain.f`; ThinkPad routine images (C8).
Depends: habu-run-emitted-x86-b704f918 (C8), habu-render-x86-trap-33eba82b (C5).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
