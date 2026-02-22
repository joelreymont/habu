---
title: Post-hoist JIT uplift execution
status: open
priority: 1
issue-type: task
created-at: "2026-02-22T19:46:28.545749+01:00"
---

PLAN.md:A. Refresh Habu-side JIT integration after ../hoist improvements. Scope: src/interp/repl.zig:2664-2921, src/jit/backend.zig workaround gates, tools/maxima-hotspots, tools/perf-loop. Goal: real JIT speedup + no stale workaround assumptions.
