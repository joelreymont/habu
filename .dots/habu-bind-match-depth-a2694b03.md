---
title: Bind MATCH depth and LIN-CHECK vectors
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:36:18.199163+02:00"
---

Full context: two checker guards remain unbound after the vacuity audit closed three others. (1) MATCH's own depth guard: mutating its #CFC bound from 30 to 10 in src/core/checker.f leaves the checker-model gate GREEN — needs a vector nesting 31 begins then a MATCH over a real SUMTYPE fixture in test/compiler/checker-model-cases.f, model side opens 31 ++ TMatch/TFamTok = VReject. (2) LIN-CHECK: making it a no-op leaves all three linear vectors answering the same verdicts (verified directly — the rejections come from the deferred-taint rule, not the per-step count). A vector for the case where the linear value sits on NEITHER row when LIN-CHECK runs — an ordinary word carrying a to-r effect — would bind it. Both are the audit's falsification residue: theorems exist, gates cannot see the guards.
