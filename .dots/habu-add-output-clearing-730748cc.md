---
title: Add output clearing primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:27.968742+02:00"
---

src/runtime/primitives/io.zig: Implement output control
- clear-input: discard buffered input
- write-char: output single character
- write-string: output string with :start/:end
- write-line: write-string + newline
- terpri: output newline
- fresh-line: newline only if not at line start
- Add tests for output sequences
- Est: 25 min
