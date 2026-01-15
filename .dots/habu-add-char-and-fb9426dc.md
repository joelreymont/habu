---
title: Add char and schar functions
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T16:59:12.398640+02:00"
---

File: lib/stdlib.habu
Add char, schar functions (wrappers for string-ref).
char: generic string character access.
schar: simple-string character access (same as char in Habu).
Both call existing string-ref primitive.
Est: 10 min
Depends: char primitive module exists
