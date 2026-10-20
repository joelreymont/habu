---
title: Compile guarded stores across calls
status: active
priority: 2
issue-type: task
created-at: "2026-10-19T11:51:57.526750+03:00"
---

Combined e8d5 source loaded through the matching selected-event recovery host cannot compile PROC-CLOSE-CELL: E-A64RA-FIXED -8460. This blocks the next native product before target emission. Reduce whether guarded ! lowering, immutable local SSA, or another responsible layer introduces incompatible fixed registers; preserve allocator checks and real protection behavior. Existing close-cell function is the red real-load path. Require a focused meaningful regression before the fix, actual native build, independent Astra review and final native/convergence checks.
