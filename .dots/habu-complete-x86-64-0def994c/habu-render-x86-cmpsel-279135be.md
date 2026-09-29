---
title: Render x86 cmpsel and selz
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.459377+03:00"
blocks:
  - habu-run-emitted-x86-b704f918
---

Problem: `src/compiler/native/emit-x64.f:40-51` refuses `cmpsel` and `selz`; RFLAGS is never an SSA value.
Acceptance: both selects render, and the result/source aliasing cases execute natively on the ThinkPad.
Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f` and `bin/hb --load test/compiler/x64-chain.f`; ThinkPad routine images (C8).
Depends: habu-run-emitted-x86-b704f918 (C8).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
