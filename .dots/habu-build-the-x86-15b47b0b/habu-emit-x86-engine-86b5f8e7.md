---
title: Emit x86 engine-state bodies
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.586158+03:00"
blocks:
  - habu-boot-and-exit-367c46f5
---

Problem: the kernel has no engine-state rows.
Acceptance: bodies for `cp@ cp! dbase@ rbase ndict@ ndict! seed-ndict! ndict-append data-base prot-wid-add prot-wid-room SEAL-* DRAIN-PRETRUST set-check check@ set-preflight set-top-check top-check@ set-tier tier@ code-origin executable-build-enter/leave wordlist get-current set-current wide-mark xt! ptr-cell-mark addr-cells-abi here allot align , c, . u. .s depth emit cr space type`; `set-tier 0` refuses on x86 (precedent `src/habu/prof.f:47-49`); the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: routine images (C8).
Depends: habu-boot-and-exit-367c46f5 (K3).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
