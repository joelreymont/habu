---
title: Lock hoist api drift
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-17T22:23:04.198667+01:00\""
closed-at: "2026-02-18T23:31:02.166003+01:00"
close-reason: closed hoist API drift lock with contract probes
---

src/jit/backend.zig and ../hoist interface points. Cause: upstream hoist rebuilds can break Habu silently. Fix: add compile-time and runtime contract probes in Habu (no hoist edits).
