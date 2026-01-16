---
title: Fix error masking in gc.zig
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:15:09.887250+02:00"
---

Files: src/runtime/gc.zig
From triage list, fix error masking in GC:
- Change void functions to !void where needed
- Replace catch unreachable with try
- Propagate errors through call chain
Dependencies: habu-triage-err-masking-b2d8c1c3
Verification: zig build test passes, gc.zig clean
