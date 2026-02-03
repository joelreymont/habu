---
title: Add RootSet types
status: active
priority: 1
issue-type: task
created-at: "\"2026-02-03T22:28:55.552844+01:00\""
---

Context: docs/stack-maps.md:34 + src/runtime/heap.zig:1477; cause: GC root enumeration copies Values into ArrayList and writes back; fix: introduce RootRange/RootSet types (ranges + slots) in new runtime module for upcoming GC API; deps: habu-jit-ir-3ffafaa9; verification: zig build test.
