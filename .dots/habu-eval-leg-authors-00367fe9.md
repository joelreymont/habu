---
title: "Eval leg authors a SPEC: equation line"
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T17:16:53.320674+02:00"
---

Follow-up from the eval authoring-surface landing (bbeb380f): the leg's authored model uses the registered op vocabulary but no SPEC: einsum line, because TR-CAP was saturated at its base. TR-CAP 64->256 has since landed (f63446a5). Add an authored SPEC: equation (canonical prefix-Σ infix-· spelling) with its TENSOR: declarations to the eval model - the equation should enter the TRAINED graph (equation-composition training, not just forward), exercising the derived-adjoint path the examples use. Keep the deterministic lock discipline (new lock, run-twice bit-identical) and the slice budget. Territory: maki/eval/train.f + docs/maki/eval.md.

2026-07-20 SERIALIZED behind habu-decouple-schedule-clip-3adcb400 (spark): same file (maki/eval/train.f); author the SPEC: line against the post-decouple leg.

2026-07-20 serialization released (train-core extraction landed 4a406088; TR-CAP landed f63446a5).
Claim: agent=evalspec workspace=.jj-ws/fable-evalspec machine=spark (owns maki/eval/train.f + docs/maki/eval.md; derive3 lane owns cad/model-ir/tensor-value/backward/executor - READ-ONLY for this lane)
