---
title: Implement make-string-input-stream
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:39:02.708884+02:00"
---

src/runtime/primitives/io.zig: Add make_string_input_stream(str, start, end). Create Stream object with string buffer. Dependencies: habu-add-streamp-primitive-4054eaad. Verify: (make-string-input-stream "hello") creates stream.
