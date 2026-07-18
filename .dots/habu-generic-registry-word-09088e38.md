---
title: Generic registry word-set for checker core
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T14:15:04.472762+02:00"
blocks:
  - habu-split-checker-f-837bc1a4
---

Depth review 2026-07-18: checker.f hand-rolls ensure/grow/rebase/snapshot-persist for ~15 registries (CT, SYM, MF, RBF, NORET, MSEEN, LOC-HW, TOKBUF, ...; type-family.f adds more) — SYM-STR-GROW is line-for-line CT-STR-GROW with renamed vars; est. 1.5-2K duplicated lines. Collapse into a parameterized registry layer (row width, string-pool y/n, rebase walker as xt). CAUTION: self-hosted core; baked boot buffers may demand concreteness — prove fixpoint+gate. Fold into the checker split window (habu-split-checker-f-837bc1a4) — same file, one churn window.
