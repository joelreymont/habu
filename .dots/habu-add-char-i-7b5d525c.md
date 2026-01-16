---
title: Add character I/O primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:27.666971+02:00"
---

src/runtime/primitives/io.zig: Implement char-level I/O
- read-char: read single character from stream
- unread-char: push character back to stream
- peek-char: look at next char without consuming
- listen: test if char available (non-blocking)
- read-char-no-hang: read char if available, else nil
- Add unread buffer to Stream object
- Add tests for character sequences
- Est: 30 min
