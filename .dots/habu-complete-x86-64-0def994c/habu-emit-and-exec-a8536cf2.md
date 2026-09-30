---
title: Emit and execute x86 scalar floats
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.492952+03:00"
---

Problem: after C7a the float forms select and allocate but `src/compiler/native/emit-x64.f` renders none of them. Second half of the float work.
Acceptance: every C7a float form renders; spills across calls; NaN compare semantics equal to ARM64's; the float fixtures execute natively on the ThinkPad.
Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f` and `bin/hb --load test/compiler/x64-chain.f`; ThinkPad routine images (C8).
Depends: habu-declare-and-select-bfbb301b (C7a), habu-run-emitted-x86-b704f918 (C8).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
C7a landing note: (a) `realint` must meet the `f>s` contract (saturate, NaN to 0): settle it in `select-x64.f`'s `realint` arm (`asm.f:576-577` leaves range and NaN to lowering) and prove it with NaN, +2^63 and -2^63 rows; (b) the `fcmpset` render maps `gt` to `seta` (C-A; after `ucomisd` SF=OF=0, so `setg` is wrong), `equal` to `sete` plus `setnp` into the scratch, and refuses any other condition or a result count other than 1 (gt) or 1 plus 1 scratch (equal): nothing enforces the one-scratch rule today (hand-built modules with the wrong count pass freeze, allocation and A64RAV).
