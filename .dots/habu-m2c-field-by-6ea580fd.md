---
title: "M2c: field-by-field unify for parametric terms"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:03:30.798732+02:00"
blocks:
  - habu-m2b-t-param-02ccd510
---

Part of PTX M2. Unify two parametric terms head-then-field-by-field (ptx-sketch.md M2). Same atom token = proven agreement; mismatch rejects; field type vars unify via the existing HM union-find. Extent tokens NOMINAL - never invent agreement between fresh tokens (inference.md rule 2).
- Files: src/core/checker.f unify.
- Verify negatives: space-shared vs space-global rejects; extent-r vs extent-c rejects; same token unifies.
- Dep: M2b.
