---
title: Implement unread-char for streams
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:39:21.969406+02:00"
---

src/runtime/primitives/io.zig: Add unread_char(char, stream). Decrement position, push char back. Dependencies: habu-implement-read-char-af19429a. Verify: read then unread char.
