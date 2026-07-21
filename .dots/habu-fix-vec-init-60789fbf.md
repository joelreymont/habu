---
title: Fix VEC-INIT re-init leak
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-21T07:41:44.408758+02:00\""
---

Loose end from the vector-release landing (stack cb1e4cae): VEC-INIT on an already-live header overwrites VEC.DATA without releasing the owned mapping - the one remaining leak path after resize/dispose were fixed. Decide the contract (reject live re-init with E-VEC-STATE, or dispose-then-init) against how callers actually use it, then red-first prove. lib/vector.f + tests.

Claim: agent=vecinit workspace=.jj-ws/fable-vecinit machine=spark (owns the VEC-INIT live-header re-init contract: lib/vector.f + tests)
