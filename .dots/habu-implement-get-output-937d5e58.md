---
title: Implement get-output-stream-string
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:39:12.232688+02:00"
---

src/runtime/primitives/io.zig: Add get_output_stream_string(stream). Extract accumulated string from output stream. Dependencies: habu-implement-make-str-a8d1783e. Verify: write to stream, get string back.
