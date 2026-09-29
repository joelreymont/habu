---
title: Render x86 neg, shl and shr
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.441939+03:00"
blocks:
  - habu-run-emitted-x86-b704f918
---

Problem: `src/compiler/native/emit-x64.f:40-51` refuses `neg`, `shl` and `shr`; the variable shifts take their count in the fixed register `rcx` (`x64ir.f` schema).
Acceptance: the count is copied into `rcx` and the source and count stay live after; counts 0, 1, 63 and 64 (masked to 6 bits per `INTEL.md:221-223`) and `neg` execute natively on the ThinkPad.
Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f` and `bin/hb --load test/compiler/x64-chain.f`; ThinkPad routine images (C8).
Depends: habu-run-emitted-x86-b704f918 (C8).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
