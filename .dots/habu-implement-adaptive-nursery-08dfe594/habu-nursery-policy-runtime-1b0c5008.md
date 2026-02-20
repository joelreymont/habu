---
title: "Nursery policy: runtime resizing"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-20T08:58:56.281925+01:00\\\"\""
closed-at: "2026-02-20T10:14:03.485846+01:00"
close-reason: Implement runtime nursery resize with live-floor safety
blocks:
  - habu-nursery-policy-derive-d65d5879
---

src/runtime/gc.zig, src/runtime/heap.zig: implement dynamic nursery resize with safety bounds and hysteresis.
