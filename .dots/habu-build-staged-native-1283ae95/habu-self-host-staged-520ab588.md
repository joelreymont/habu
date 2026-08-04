---
title: Self-host staged compiler
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:56:29.091440+02:00"
blocks:
  - habu-cut-over-staged-070d68c8
---

Full context: `docs/compiler-ir-design.md` Wave 8 compiles the new compiler with itself, proves a byte-identical fixpoint, makes it default after release-quality green evidence, deletes old direct paths, and updates the current size baseline atomically. Acceptance: the complete candidate suite and Wave 8 native exits pass on the exact cutover tree.
