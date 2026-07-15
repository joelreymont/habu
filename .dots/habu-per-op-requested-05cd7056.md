---
title: Per-op requested numeric policy for transcendental EW
status: open
priority: 3
issue-type: task
created-at: "2026-07-15T09:13:13.364202+02:00"
---

Found by the npolgate lane 2026-07-15: CLASS-DEFAULT-POL keys the requested numeric policy per op CLASS, and CLASS-EW defaults to exact (mixed class: relu/cast exact, add/mul ulp, gelu/silu relative; sched-key-test freezes an elementwise-region exact key). A PURE-transcendental elementwise region (only gelu/silu, no matmul/reduce) would request exact yet achieve relative - over-refusing at promotion. No such region is promoted today, so nothing regresses, but the correct fix is a per-OP requested-policy axis: REGION-POL folds the region's ops through NUM>DOM/OP-DOM (the achieved side already does this) instead of the per-class table, with the sched-key fixtures updated to the honest per-op keys. Acceptance: a gelu-only region requests relative (promotes with a relative golden); pure-relu region still exact; existing keys/fixtures updated honestly; executed promote/refuse pair. Files: maki/numpolicy.f (REGION-POL/CLASS-DEFAULT-POL), maki/sched-key-test.f + affected key pins, policy-e2e/cad tests. Verify: maki/test.f. Ownership: maki numeric policy.
