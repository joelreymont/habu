---
title: Design Package object layout
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:40:28.328705+02:00"
---

src/runtime/objects.zig: Add Package struct after Stream. Fields: name, nicknames, use-list, export-list, shadow-list, symbol-table (hash). Add Package type tag. Dependencies: none. Verify: struct compiles.
