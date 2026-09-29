---
title: Emit x86 control bodies
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.568965+03:00"
blocks:
  - habu-boot-and-exit-367c46f5
---

Problem: the kernel has no control rows.
Acceptance: bodies for `execute run-in-stack catch throw finally die 2>r 2r> 2r@` and an `evaluate` stub until I9a; catch/throw restore machine stack, data stack and handler frame (`src/habu/habu1.f:2673-2830` semantics); native tests through routine images; the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: routine images (C8).
Depends: habu-boot-and-exit-367c46f5 (K3).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
- From I4a: the body list includes `execute-floor ( n -- bool )` (call the xt, then clamp and report a stack below S0).
