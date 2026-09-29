---
title: "Emit x86 frame forms: reserve, release, store, load"
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.432839+03:00"
blocks:
  - habu-run-emitted-x86-b704f918
---

Problem: `src/compiler/native/emit-x64.f:40-51` refuses `reserve`, `release`, `store` and `load` with `E-X64EMIT-FORM`; spills lower but cannot execute.
Acceptance: pressure fixtures (spills across branches, loops, calls, several slots) execute natively on the ThinkPad with exact `rsp` restoration (compare `rsp` before and after as the peer fixture does with `RSP RBP`); bytes pinned against `llvm-mc`.
Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f` and `bin/hb --load test/compiler/x64-chain.f`; ThinkPad routine images (C8).
Depends: habu-run-emitted-x86-b704f918 (C8).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
