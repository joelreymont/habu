---
title: Add char case predicate primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T16:58:51.235683+02:00"
---

File: src/runtime/primitives/char.zig
Add upper-case-p, lower-case-p, both-case-p primitives.
Use Zig std.ascii.isUpper/isLower for ASCII.
both-case-p checks if char has upper and lower forms.
Est: 15 min
