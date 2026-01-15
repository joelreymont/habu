---
title: Update GC for Stream objects
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:42:52.743844+02:00"
---

src/runtime/gc.zig: Add Stream case to marking/copying. Follow Stream buffer pointers. Dependencies: habu-design-stream-obj-270e828e. Verify: streams survive GC.
