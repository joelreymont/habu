---
title: Finish remaining control-flow model gaps
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T13:04:18.535211+02:00"
---

Full context: formal/Common/Control.v covers loops, case, match, throw and die, branch-scoped locals and linear conservation, all measured against bin/hb. Four items were left undone rather than being out of reach, and are recorded here so they are work rather than caveats. (1) The return-stack transfers 2>r, 2r>, 2r@ and r@ (src/core/checker.f:1954-1983). Identical in shape to the >r and r> rules already modelled as TToR and TFromR; the author stopped at the two needed for the linear-conservation example. Small. (2) Typed local annotations (LOC-ANN at checker.f:7386, LOC-ANN-BIND-CHECK at :7424). The model binds untyped fresh variables. The annotation is a strictly additional constraint so the model is conservative and every measured fixture agrees, but a fixture where the annotation is what rejects is not covered. Small. (3) LMODE as lingering state: TLocals is atomic in the model, so the checker's reject for a definition that ended mid '{:' (CHECK's LMODE @ 0 <> test) is unreachable. Small. (4) MDIAG prose rendering: the model asserts reason codes but does not model render.f's mapping to E-* names, which is why four MATCH fixtures claim verdicts rather than message text. Acceptance: each of the four is modelled with a measured fixture pair, the build stays green, no Admitted. Bundle-valued locals (LOC-BUNDLE-BIND at checker.f:7434) are NOT in this leaf - they are blocked on arity-n families and belong to that dot.
