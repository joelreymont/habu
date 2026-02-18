---
title: Tenured collector
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-18T21:50:53.711212+01:00\\\"\""
closed-at: "2026-02-18T23:16:42.244564+01:00"
close-reason: implemented non-moving tenured mark-sweep + free-span reuse
blocks:
  - habu-minor-gc-collector-2f89a428
---

src/runtime/gc.zig and src/runtime/objects.zig. Cause: tenured space has no reclamation strategy yet. Fix: add non-moving mark-sweep for tenured objects with free-list bins. Why: stable addresses and controlled memory growth.
