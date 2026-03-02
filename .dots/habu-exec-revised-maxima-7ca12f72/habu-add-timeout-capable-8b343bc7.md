---
title: Add timeout-capable external Maxima runner
status: open
priority: 2
issue-type: task
created-at: "2026-03-07T19:32:55.783902+01:00"
blocks:
  - habu-adopt-canonical-test-a8a0cbe4
---

Direct ./zig-out/bin/habu runner path from PLAN 3.5 plus a shell-level timeout wrapper/tool. Root cause: hangs must be triaged without wedging the whole session, but the workflow must stay on direct Habu probes rather than zig build test. Fix: distinguish timeout vs error vs fail and keep one full-suite attempt in closure evidence. Why: needed once the named hang families are reduced.
