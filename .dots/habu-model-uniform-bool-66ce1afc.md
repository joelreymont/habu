---
title: Model uniform bool and barrier control
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T15:42:48.627604+02:00"
---

Full context: the arity-n widening made uniform<bool> representable - uniform is a TK-CELL family of arity 1 (src/core/type-family.f:2098), width 1, not a layout, and COND-UNIFORM? (src/core/checker.f:7779) needs only facts the model now has: top of DCUR is a T-PARAM, family = PTX-UNIFORM-FAM, argc 1, argument 0 resolves to bool. What remains is ordinary Control.v frame-and-flag work, named exactly by the modelling worker: the CF.UNI frame slot (one more field on the frame record), ALL-CF-UNIFORM? as a fold over st_cfs (checker.f:7681), STEP-UNIFORM-BOOL-IN, capture of the PTX-UNIFORM-FAM and PTX-TILE-FAM ids, and the CTL-BARRIER control flag with PTX-BARRIER-ROWS? and BARRIER-CUR?. Acceptance: definitional examples cover a uniform<bool> branch certifying, a divergent-condition barrier rejection (MD-DIVBAR), and a non-uniform condition falling back to the ordinary branch rule; each measured against bin/hb with exit codes; build green; no Admitted. Depends on the arity-n commit (merged on the proofs bookmark).
