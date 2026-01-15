---
title: Add char type predicates
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T16:59:01.709931+02:00"
---

File: src/runtime/primitives/char.zig
Add digit-char-p, alphanumericp primitives.
digit-char-p: check if char is 0-9, optional radix parameter.
alphanumericp: check if letter or digit.
Use std.ascii.isDigit, isAlphanumeric.
Est: 20 min
