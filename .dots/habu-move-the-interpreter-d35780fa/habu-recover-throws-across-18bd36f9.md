---
title: Recover throws across evaluate in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.957323+03:00"
blocks:
  - habu-roll-back-failed-64bf2ba5
---

Problem: throw recovery across `evaluate` is assembly (`LEVCORRUPT` validates the handler frame).
Acceptance: rc propagation across `evaluate` and handler-frame validation, fail-closed as `LEVCORRUPT`.
Files: `src/habu/outer.f`, cases beside `test/outer-interpret.f`.
Verify: spark: throw-across-`evaluate` cases through the Habu loop under the feature cell, including a corrupted frame; gate.
Depends: habu-roll-back-failed-64bf2ba5 (I9b).
Route: Alder (shared: src/habu/outer.f and the test).
Ownership: krait (Intel lane).
Claim: unassigned.
