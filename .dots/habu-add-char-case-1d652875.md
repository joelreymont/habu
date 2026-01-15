---
title: Add char case primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T16:58:51.230679+02:00"
---

File: src/runtime/primitives/char.zig
Add char-upcase, char-downcase primitives.
Use Zig std.ascii.toUpper/toLower for ASCII range.
For Unicode, use simple case mapping tables or stub with ASCII-only.
Est: 20 min
