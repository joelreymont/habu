---
title: Cache dynamic buffer element bounds
status: active
priority: 1
issue-type: task
created-at: "2026-10-14T22:00:09.496497+03:00"
---

Otto profile /Users/joel/.cache/tmp/dots-profile/report.md For Habu3: DYNAMIC-STORAGE:OFFSET divides exact byte capacity by element width on every access,71.5 instructions/access vs14.8 plain. Existing density patch95829178 shares guard code but leaves this measured hot path. Establish the element bound when byte capacity or element width changes, preserve exact byte capacity for copy/release and current fractional-capacity semantics (width24/cap64 permits2 elements but retains64 mapping bytes), and use the bound directly at access. No dropped range guard or capacity rounding workaround. Design control representation and reserve/lifecycle/capture/Wasm invariants before implementation; existing real dynamic buffer/task/capture suites first, same-source db/plain probe measurement, independent Astra review and combined matching-engine/fullnative/convergence gate for baked ABI/codegen impact. Coordinate density95829178; no Dots edits.

The focused Wasm storage suite passes all seven emitted modules on the rebuilt5b93a9ee engine. Validation and execution cover growth, release/reuse, typed pointer elements, two complete24-byte elements in64bytes, preservation of byte63 across growth, and width/bounds/release refusals. The former fractional-case linker failure is resolved on this matching host.

The rebuilt 5b93a9ee engine (7cc7f9be, matching map95f25745) reduces the same bounded 100000-access native DB probe from 70.11 to 35.85 retired instructions per read; the plain-array control stays near14. Generated DB access is20bytes instead of64. This verifies the measured access cost, not Dots-wide throughput; the combined native/convergence gates still require qualification.
