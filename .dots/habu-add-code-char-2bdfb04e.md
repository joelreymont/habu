---
title: Add code-char primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T16:58:51.225499+02:00"
---

File: src/runtime/primitives/char.zig
Add code-char function - inverse of char-code.
Takes fixnum, returns character (also fixnum).
Validate code is in valid Unicode range (0-0x10FFFF).
Est: 10 min
Depends: char-code primitive
