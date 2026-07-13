---
title: Add lexical mutable scratch borrows
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T11:44:22.215821+02:00"
blocks:
  - habu-add-bounded-host-b40b048f
---

Context: compiler, checker, emitter, gate, and report helpers use package globals as hidden scratch; nested or recursive calls can overwrite caller-live indexes and shared buffers while ordinary stack effects remain unchanged. Existing DEFLINEAR proves owner conservation but does not express a lexical unique mutable borrow or hidden state preservation. Fix: add a package-scoped scratch-frame surface that mints a linear owner, permits one lexical mutable borrow or multiple read borrows, prevents escape and re-entry, restores state across throw and checker rollback, and composes with habu-checker-path-sensitive-226626ae and habu-linear-once-resource-4c58a7a1. Infer or declare package-qualified state read and write effects so callers can require PRESERVES. Acceptance: reject caller-index clobber, double mutable borrow, write during read borrow, escaped shared buffer, recursion with a live borrow, branch-only restoration, and helper calls violating PRESERVES; prove legal nested disjoint frames, task isolation, and exception cleanup; migrate representative checker, emitter, and report scratch stores.
