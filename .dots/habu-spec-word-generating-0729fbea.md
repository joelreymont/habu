---
title: "SPEC: word — THE default golden-authoring surface"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-18T13:19:18.330716+02:00\""
---

docs/golden-syntax.md candidate C — the DEFAULT way goldens are written once landed (per Joel), not an optional layer: new dataflow definitions are SPEC: lines; hand-written accessor bodies become the exception for what SPEC: cannot express. Parsing word SPEC: GGEMM O[m n] = A[ix[m] k] B[n k] * +Sk ; deriving (1) candidate-B golden code (2) planner dataflow (idxctx + contraction extents) (3) PROMOTE shape obligations. Small parser over existing sig-grammar machinery. Strictly after 'Extent-typed tensor accessors' (habu-extent-typed-tensor-bde435dc), which needs 'Foundation A' (habu-foundation-a-declarable-0390600f) — that chain is the critical path.

UNBLOCKED 2026-07-18: the prerequisite chain is complete - Foundation A substrate and the extent-typed accessors (habu-extent-typed-tensor-bde435dc) are merged on master (b90d1c14): EXTENT:/TENSOR:/ITENSOR:, the ix<extent> family, range-guarded injectors, and the XG codegen boundary in maki/extent.f are the machinery SPEC: generates against. Known gap to honor: the loop counter is not extent-typed yet (habu-extent-bound-loop-a70a49b3), so generated loop bodies use the explicit `i >#EXT` crossing exactly as maki/extent-tensor-test.f's GGEMM does. Claim: agent=tensor-opus workspace=.jj-ws/habu-spec-word-generating-0729fbea
