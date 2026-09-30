---
title: Link records and wids into the x86 image
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.991875+03:00"
blocks:
  - habu-carry-the-shadow-dcb84138
---

Problem: no writer lays the captured dictionary out for x86.
Acceptance: `src/habu/link-x64.f`: 48-byte records at their final addresses, wids rebased (`WID-REL-BASE`), the protected-wid bitmap, and the name index either built by the writer or rebuilt by the kernel's `seed-ndict!` at boot (decided by measuring whether the writer's index is deterministic).
Files: new `src/habu/link-x64.f`, a host-side test under `test/x86-64-*`.
Verify: spark: the host-side test over a captured window with a shadow; the index-determinism measurement recorded in the commit.
Depends: habu-write-and-link-f6e6017f (X4a), habu-carry-the-shadow-dcb84138 (X2b).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
- From I4a: primitive flag cells come from `ENGINE-PRIMS:DNAME`, which carries each primitive's min-in.
