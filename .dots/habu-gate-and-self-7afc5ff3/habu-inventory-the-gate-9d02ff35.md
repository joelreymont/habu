---
title: Inventory the gate for a tier-1-only engine
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.826339+03:00"
blocks:
  - habu-run-bin-hb-6378f297
---

Problem: the x86 engine is tier-1-only; some gate rows need tier 0 or an x86 whitebox build, and which ones is unmeasured (user `immediate` words: 80 files, 313 uses).
Acceptance: a measured list of rows that cannot run tier-1-only (the 21 `0 set-tier` files, `engine-stack-jit`, `tier`, the JIT-snapshot whitebox rows) and of rows needing whitebox builds, each excluded behind a target predicate with its reason in `test/gate-stdlib-cases.f`; rows that merely default to tier 0 stay; `docs/gate.md` lists the x86 exclusions.
Files: `test/gate-stdlib-cases.f`, `docs/gate.md`.
Verify: spark gate unchanged; ThinkPad: the gate's row inventory with the cross-built engine.
Depends: habu-run-bin-hb-6378f297 (X6).
Route: Alder (shared: test/gate-stdlib-cases.f, docs/gate.md).
Ownership: krait (Intel lane).
Claim: unassigned.
