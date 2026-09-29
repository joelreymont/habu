---
title: Render x86 trap and codeaddr
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.467577+03:00"
blocks:
  - habu-run-emitted-x86-b704f918
---

Problem: `src/compiler/native/emit-x64.f:40-51` refuses `trap` and `codeaddr`.
Acceptance: `trap` reaches the runtime `throw` entry; `codeaddr` is a recorded CODE site (a `MOVABS` site row); quotations execute natively on the ThinkPad.
Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f` and `bin/hb --load test/compiler/x64-chain.f`; ThinkPad routine images (C8).
Depends: habu-run-emitted-x86-b704f918 (C8).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
