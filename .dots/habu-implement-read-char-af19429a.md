---
title: Implement read-char for streams
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:39:17.262140+02:00"
---

src/runtime/primitives/io.zig: Add read_char(stream, eof_error_p, eof_value). Read one character, advance position. Dependencies: habu-implement-get-output-937d5e58. Verify: (read-char stream) reads char.
