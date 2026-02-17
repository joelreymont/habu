---
title: Lock hoist api drift
status: open
priority: 1
issue-type: task
created-at: "2026-02-17T22:23:04.198667+01:00"
---

src/jit/backend.zig and ../hoist interface points. Cause: upstream hoist rebuilds can break Habu silently. Fix: add compile-time and runtime contract probes in Habu (no hoist edits).
