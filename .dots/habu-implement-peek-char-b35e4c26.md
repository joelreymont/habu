---
title: Implement peek-char for streams
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:39:29.001456+02:00"
---

src/runtime/primitives/io.zig: Add peek_char(peek_type, stream). Read without advancing position. Dependencies: habu-implement-unread-char-7b7e317d. Verify: (peek-char stream) doesn't advance.
