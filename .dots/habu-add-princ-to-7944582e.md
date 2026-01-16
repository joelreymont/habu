---
title: Add princ-to-string primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:27.360977+02:00"
---

src/runtime/primitives/io.zig: Implement princ-to-string
- princ-to-string: write-to-string with *print-escape* = nil
- Return human-readable string (no quotes/escapes)
- Use write-to-string with escape disabled
- Add tests for strings without quotes
- Est: 10 min
