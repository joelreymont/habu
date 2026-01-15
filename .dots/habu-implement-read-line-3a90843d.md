---
title: Implement read-line for streams
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:39:39.692761+02:00"
---

src/runtime/primitives/io.zig: Add read_line(stream). Read until newline. Return line and t/nil for eof. Dependencies: habu-implement-write-char-9e5fa7d5. Verify: (read-line stream) reads line.
