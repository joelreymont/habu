---
title: Add invoke-restart primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:54.968791+02:00"
---

src/runtime/primitives/primitives.zig: Implement restart invocation
- invoke-restart: invoke named restart with args
- invoke-restart-interactively: prompt for args
- find-restart: lookup restart by name
- compute-restarts: return list of active restarts
- restart-name: get restart name
- Add tests for restart introspection
- Est: 30 min
