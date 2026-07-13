---
title: snap-rebase straddling-range gap past endpoint guards
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:46:16.026032+02:00"
---

Current-state correction on master 8f3ce0e9: the implementation is already present. `BSNAPREBASE` derives byte length as `end - base` and calls `GUARD-SPAN` before `LSNAPRBD`/`LSNAPRBC`; the shared guard rejects overlap with every registered protected band and address+length wrap. The remaining bug is proof coverage: `test/seal.f` still calls snap-rebase hand-review-only and has no child-process straddling fixture, so a future point-guard regression can pass the gate. Fix: add a child forge whose `[base,end)` starts below and ends above a protected band and assert rc83 before relocation, add exact-below/exact-above adjacency and invalid/wrap boundary cases, keep a legitimate high scratch snapshot rebase positive, and update the prove-absence/census text to classify snap-rebase as exercised. Acceptance: replacing `GUARD-SPAN` with endpoint point guards makes the new negative fail red; the real guard rejects every intersecting or wrapped interval before bytes change; adjacent and legitimate snapshot ranges succeed; the regression uses the exact production load path and does not perform relocation until the guard has proven the range safe. Files: test/seal.f and test/seal-absence.f only unless the red-first mutation exposes a production defect. Verify: seal and seal-absence suites, snapshot/fixpoint build, typed-local diff lint, host/filemap/dot lints, full native gate. Serialize with active span-guard test ownership; do not add the obsolete `PROT-GUARD-RANGE` design or edit engine code without new evidence.
