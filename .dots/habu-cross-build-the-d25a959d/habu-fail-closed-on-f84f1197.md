---
title: Fail closed on x86 in habu1.f two-arm forms
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.757499+03:00"
---

Problem: 25 `HB-TARGET-LINUX?` two-arm forms in `src/habu/habu1.f` (cross-build obligation (4)).
Acceptance: each gets an explicit refusal for linux-x86-64 (the x86 engine never runs these emitters); the `docs/porting.md` fail-closed rule holds; the Gforth chain stays ARM64.
Files: `src/habu/habu1.f`.
Verify: spark: rebuild; chain gen2==gen3 (the ARM64 engine is unchanged); gate.
Depends: none. Serialise on `habu1.f`/`habu2.f` with K4, I6 and I10c.
Route: Alder (shared: src/habu/habu1.f).
Ownership: krait (Intel lane).
Claim: unassigned.
