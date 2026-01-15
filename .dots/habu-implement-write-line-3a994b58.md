---
title: Implement write-line for streams
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:39:48.996437+02:00"
---

src/runtime/primitives/io.zig: Add write_line(string, stream). Write string + newline. Dependencies: habu-implement-write-str-033c84c7. Verify: (write-line "hi" stream) adds newline.
