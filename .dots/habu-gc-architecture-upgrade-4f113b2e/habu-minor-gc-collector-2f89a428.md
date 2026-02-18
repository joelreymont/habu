---
title: Minor GC collector
status: open
priority: 1
issue-type: task
created-at: "2026-02-18T21:50:53.706636+01:00"
blocks:
  - habu-remembered-set-c9541b7e
---

src/runtime/gc.zig. Cause: current collector always traces full heap. Fix: implement nursery copy collector scanning roots+remembered cards; promote survivors by age/size. Why: reduce average pause and copied bytes.
