---
title: Add digit-char primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T16:59:01.720710+02:00"
---

File: src/runtime/primitives/char.zig
Add digit-char primitive.
Takes weight (0-35) and optional radix, returns digit character.
For weight < radix, return '0'-'9' or 'A'-'Z', else nil.
Est: 15 min
