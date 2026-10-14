---
title: Cache dynamic buffer element bounds
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T22:00:09.496497+03:00"
---

Otto profile /Users/joel/.cache/tmp/dots-profile/report.md For Habu3: DYNAMIC-STORAGE:OFFSET divides exact byte capacity by element width on every access,71.5 instructions/access vs14.8 plain. Existing density patch95829178 shares guard code but leaves this measured hot path. Establish the element bound when byte capacity or element width changes, preserve exact byte capacity for copy/release and current fractional-capacity semantics (width24/cap64 permits2 elements but retains64 mapping bytes), and use the bound directly at access. No dropped range guard or capacity rounding workaround. Design control representation and reserve/lifecycle/capture/Wasm invariants before implementation; existing real dynamic buffer/task/capture suites first, same-source db/plain probe measurement, independent Astra review and combined matching-engine/fullnative/convergence gate for baked ABI/codegen impact. Coordinate density95829178; no Dots edits.
