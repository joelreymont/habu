---
title: "SPEC: word generating extent-typed goldens"
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T13:19:18.330716+02:00"
---

docs/golden-syntax.md candidate C. Parsing word SPEC: GGEMM O[m n] = A[ix[m] k] B[n k] * +Sk ; deriving (1) candidate-B golden code (2) planner dataflow (idxctx + contraction extents) (3) PROMOTE shape obligations. Small parser over existing sig-grammar machinery. Strictly after 'Extent-typed tensor accessors' — without extent roles the generated code is plausible-not-proven.
