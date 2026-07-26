---
title: Optimize straight-line SIR
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:57:03.044781+02:00"
blocks:
  - habu-verify-straight-line-6fe4bfed
---

Full context: Wave 2 permits only basic integer constant folding and dead pure operation elimination. Implement stable deterministic passes plus independent validation; obey explicit overflow/numeric policy and never apply generic floating algebra. Acceptance: pass-specific mutation/golden fixtures, witness bindings, and semantics differential pass; stack renames remain instruction-free. Dependency: SIR verifier.
