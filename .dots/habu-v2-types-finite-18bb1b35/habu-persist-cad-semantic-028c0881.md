---
title: Persist CAD semantic effects in checker
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T13:16:45.394355+02:00"
blocks:
  - habu-seal-cad-effect-49cac404
---

Full context: src/core/checker.f stored words, primitives, and quotations retain stack, return-stack, linear, and control facts but no CAD semantic row; a balanced pure declaration can therefore call IO or device authority invisibly. Fix: persist sealed static CAD-EFFECT rows in primitive, stored-word, and quotation metadata; validate every primitive declaration, stored-word ingress, quotation construction/open, snapshot/replay record, and checked lookup. Callee bindings are relative to declared inputs, attributes, capabilities, and captures; at each word call or quotation construction/open, substitute them through CAD-EFFECT:REMAP into a stable caller/call-site namespace before canonical UNION. Union only successful calls and preserve latent quotation effects, rollback, snapshot, replay, and fixpoint identity. Acceptance: malformed or unsealed rows reject before metadata mutation; two callees that both use local slot zero remain distinct after composition, while reapplying the same remapped binding is idempotent; capture, nested quotation, higher-order propagation, failed-overload rollback, missing boundary declarations, and pure-calls-device mutations reject or compose exactly; current checked sources remain certified only after every primitive boundary is explicitly classified. Files: src/core/checker.f plus focused checker suite and bootstrap mirrors required by the native model. Verify: red-first ingress/substitution mutations, checker suites, bootstrap, fixpoint, full native gate. Ownership: checker metadata and call-site substitution only; no runtime artifact/capability resolution.
