---
title: Parse numbers in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.638255+03:00"
blocks:
  - habu-mark-blob-provided-21605a53
---

Problem: number parsing is the assembly `LNUM`; `bootstrap/cg/forth.fs:2290` documents its contract.
Acceptance: the `LNUM` contract (value, ok, range-refused), `$hex` and negatives in `src/habu/outer.f`; a differential test against `num-parse`.
Files: `src/habu/outer.f`, a differential test beside `test/outer-find.f`.
Verify: spark: the differential test against `num-parse`.
Depends: habu-mark-blob-provided-21605a53 (I1).
Route: Alder (shared: src/habu/outer.f and the new test).
Ownership: krait (Intel lane).
Claim: unassigned.
