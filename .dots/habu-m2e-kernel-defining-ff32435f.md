---
title: "M2e: KERNEL: defining word plus header markers"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:03:30.829902+02:00"
blocks:
  - habu-m2d-render-record-6b4a33c6
---

Part of PTX M2. KERNEL: is a checker alias for : so CHECK! verifies the body against the declared parametric ( in -- out ); GRID:/WHERE are compile-time header markers consumed by lib/ptx.f (already stubbed as immediate skips).
- Files: lib/ptx.f, KERNEL: alias hook in the checker.
- Verify: a KERNEL: parametric signature is checked; a body violating the declared effect is REJECTED.
- Dep: M2a-M2d (also needs M2c unify).
