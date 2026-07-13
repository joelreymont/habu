---
title: Grow checked input capacity
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T03:01:25.023945+02:00\""
---

Full context: src/habu/layout.f and bootstrap mirror define IBUFSZ=0x180000. Owner persistence exact stdin maker generates 1,048,333-byte stage source and final driver append fails closed with rc67/-2802: hb: source prefix buffer full. Implement synchronized capacity growth with a boundary regression proving the previous edge and the new capacity; preserve native/bootstrap ABI parity. Dependency: blocks habu-owner-seal-persist-1f23e205.
