---
title: Add char-name and name-char
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T16:59:12.393564+02:00"
---

File: src/runtime/primitives/char.zig
Add char-name, name-char primitives.
char-name: return string name for character (Space, Newline, Tab, etc).
name-char: parse string name to character.
Support standard CL names: Space, Newline, Tab, Return, Linefeed, Page, Rubout.
Est: 25 min
