---
title: Compile guarded stores across calls
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T11:51:57.526750+03:00"
closed-at: "2026-10-22T12:36:34.903942+03:00"
close-reason: "Triage 2026-10-22, DONE (STALE-CLAIM): The combined source landed (3bdffeeafa17), and bin/hb.map from 10-20 holds process-argv.f's PROC-CLOSE-CELL, so the engine compiled."
---

Combined e8d5 source loaded through the matching selected-event recovery host cannot compile PROC-CLOSE-CELL: E-A64RA-FIXED -8460. This blocks the next native product before target emission. Reduce whether guarded ! lowering, immutable local SSA, or another responsible layer introduces incompatible fixed registers; preserve allocator checks and real protection behavior. Existing close-cell function is the red real-load path. Require a focused meaningful regression before the fix, actual native build, independent Astra review and final native/convergence checks.
