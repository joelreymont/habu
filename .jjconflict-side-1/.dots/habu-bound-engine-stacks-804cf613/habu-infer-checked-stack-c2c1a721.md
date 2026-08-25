---
title: Infer checked stack peak certificates
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:20:06.542690+02:00"
blocks:
  - habu-infer-linear-kinds-1f77b4c4
---

Static invariant: a checked callable has a compositional finite relative peak for data, user-return, and loop stacks, or it cannot be used where bounded capacity is required. Problem: input/output effects prove balanced exits but not maximum live depth; recursion under a retained >r value or loop frame can grow without bound while preserving the declared final row. Fix: extend checked effect metadata with per-stack relative peaks, compose call peaks, take path maxima at joins, persist/replay certificates transactionally, and reject recursive SCCs with positive live growth unless an explicit independently checked finite bound exists. Acceptance negatives: balanced >r-across-recurse, recurse under a live loop frame, branch-only excess peak, stale serialized certificate, and forged too-small peak reject; finite >r/r>, bounded control flow, zero-growth recursion, quotation composition, and exact replay pass; diagnostics name stack and required peak. Files: src/core/checker.f, checker effect metadata serialization owners, new test/stack-peak-check.f, docs/effects.md. Verify: red-first checker matrix, rollback/snapshot/AOT/bootstrap parity, property/checker suites, typed-local diff, host/dot lints, native fixpoint/full gate. Serialized after linear-kind metadata so both changes share one canonical effect representation.
