---
title: Wire package primitives to compiler
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:42:45.413131+02:00"
---

src/compiler/compile.zig: Add builtin symbols for all 20 package functions around line ~300-350. Add dispatch cases. Map to primitive calls. Dependencies: habu-update-reader-for-7c26ce2a. Verify: all package ops compile.
