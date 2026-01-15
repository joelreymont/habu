---
title: Add char case-insensitive comparison
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T16:59:01.704419+02:00"
---

File: src/runtime/primitives/char.zig
Add char-equal, char-lessp, char-greaterp, char-not-lessp, char-not-greaterp.
Case-insensitive versions of char comparison.
Convert both chars to uppercase before comparing.
Est: 20 min
