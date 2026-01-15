---
title: Implement write-string for streams
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:39:44.329417+02:00"
---

src/runtime/primitives/io.zig: Add write_string(string, stream, start, end). Write substring to stream. Dependencies: habu-implement-read-line-3a90843d. Verify: (write-string "hi" stream).
