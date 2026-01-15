---
title: Implement write-char for streams
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:39:34.801152+02:00"
---

src/runtime/primitives/io.zig: Add write_char(char, stream). Append char to output stream buffer. Dependencies: habu-implement-peek-char-b35e4c26. Verify: (write-char #\a stream) writes.
