---
title: Tenured collector
status: open
priority: 1
issue-type: task
created-at: "2026-02-18T21:50:53.711212+01:00"
blocks:
  - habu-minor-gc-collector-2f89a428
---

src/runtime/gc.zig and src/runtime/objects.zig. Cause: tenured space has no reclamation strategy yet. Fix: add non-moving mark-sweep for tenured objects with free-list bins. Why: stable addresses and controlled memory growth.
