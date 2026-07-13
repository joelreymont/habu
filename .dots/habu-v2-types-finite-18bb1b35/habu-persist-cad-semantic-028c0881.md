---
title: Persist CAD semantic effects in checker
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T13:16:45.394355+02:00"
blocks:
  - habu-define-finite-cad-0bdf52ad
  - habu-checker-seal-owner-f7de26ff
---

Full context: src/core/checker.f stored words, primitives, and quotations retain stack, return-stack, linear, and control facts but no CAD semantic mask; a balanced pure declaration can therefore call IO or device authority invisibly. Fix: persist finite semantic masks and canonical bindings in primitive, stored-word, and quotation metadata; union only successful calls; preserve latent quotation effects, rollback, snapshot, replay, and fixpoint identity; expose a checked lookup surface. Acceptance: higher-order propagation, failed-overload rollback, missing boundary declarations, and pure-calls-device mutations reject; current checked sources remain certified. Files: src/core/checker.f plus focused checker suite and bootstrap mirrors required by the native model. Verify: red-first probes, checker suites, bootstrap, fixpoint, full native gate. Ownership: checker metadata only; depends on finite row vocabulary and sealed owner lane.
