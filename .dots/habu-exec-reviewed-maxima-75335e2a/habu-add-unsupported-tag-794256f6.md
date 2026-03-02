---
title: Add unsupported-tag histogram for Maxima JIT triage
status: open
priority: 2
issue-type: task
created-at: "2026-03-08T17:08:50.338612+01:00"
---

Files: src/interp/repl.zig:doHoistCompile/firstUnsupportedTag path, src/interp/vm.zig:JitAdmStats or adjacent stats payload, tools/maxima-hotspots. What: record firstUnsupportedTag counts per workload so JIT roadmap order is data-driven, not anecdotal. Why: PLAN 7.5. Verification: workload output includes histogram and clearly separates signature/admission failures from true unsupported-IR failures.
