---
title: Fix error masking in remaining files
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:15:18.938619+02:00"
---

Files: src/runtime/primitives/*.zig, src/bytecode/*.zig, src/types/*.zig, etc.
From triage list, fix all remaining error masking.
Work file by file from triage plan.
Dependencies: habu-fix-err-masking-c9e5dff2, habu-fix-err-masking-79350905
Verification: rg audit shows only allowed patterns
