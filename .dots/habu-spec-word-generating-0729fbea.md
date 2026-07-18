---
title: "SPEC: word — THE default golden-authoring surface"
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T13:19:18.330716+02:00"
---

docs/golden-syntax.md candidate C — the DEFAULT way goldens are written once landed (per Joel), not an optional layer: new dataflow definitions are SPEC: lines; hand-written accessor bodies become the exception for what SPEC: cannot express. Parsing word SPEC: GGEMM O[m n] = A[ix[m] k] B[n k] * +Sk ; deriving (1) candidate-B golden code (2) planner dataflow (idxctx + contraction extents) (3) PROMOTE shape obligations. Small parser over existing sig-grammar machinery. Strictly after 'Extent-typed tensor accessors' (habu-extent-typed-tensor-bde435dc), which needs 'Foundation A' (habu-foundation-a-declarable-0390600f) — that chain is the critical path.
