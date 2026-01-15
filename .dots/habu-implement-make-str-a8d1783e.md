---
title: Implement make-string-output-stream
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:39:07.250767+02:00"
---

src/runtime/primitives/io.zig: Add make_string_output_stream(). Create Stream with growable string buffer. Dependencies: habu-implement-make-str-91039da5. Verify: (make-string-output-stream) creates stream.
