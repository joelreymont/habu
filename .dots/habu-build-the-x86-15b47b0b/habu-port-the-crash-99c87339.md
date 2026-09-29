---
title: Port the crash handler and guard classification
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.871049+03:00"
blocks:
  - habu-port-signals-crash-2c7768ca
---

Problem: the crash handler's register dump and guard-page classification exist for aarch64 only (`src/habu/crash.f`, whose constants at 30-36 are aarch64).
Acceptance: an x86 register dump on fd 2, exit 134, and a guard-page fault distinguished from ordinary faults per the existing contract (`src/habu/crash.f` semantics); the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/boot-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: routine images faulting on a guard page and on an ordinary address; the dump, the classification and status 134 checked.
Depends: habu-port-signals-crash-2c7768ca (K10a).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
