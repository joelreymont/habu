---
title: Fix error masking in compiler
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:15:14.278328+02:00"
---

Files: src/compiler/compile.zig
From triage list, fix error masking in compiler:
- Update function signatures to return !T
- Replace masking with try
- Update call sites
Dependencies: habu-triage-err-masking-b2d8c1c3
Verification: zig build test passes, compiler clean
