---
title: Add prin1-to-string primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:27.058976+02:00"
---

src/runtime/primitives/io.zig: Implement prin1-to-string
- prin1-to-string: write-to-string with *print-escape* = t
- Return readable string representation
- Use write-to-string with escape forced
- Add tests for strings/symbols/special chars
- Est: 10 min
