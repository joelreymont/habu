---
title: Add char-int and int-char
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T16:59:01.715075+02:00"
---

File: src/runtime/primitives/char.zig
Add char-int, int-char primitives.
char-int: character to non-negative integer (same as char-code).
int-char: non-negative integer to character (same as code-char).
CL compatibility - may be same as code-char/char-code.
Est: 10 min
