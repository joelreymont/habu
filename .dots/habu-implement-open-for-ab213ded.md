---
title: Implement open for file streams
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:39:54.368570+02:00"
---

src/runtime/primitives/io.zig: Add open_file(filename, direction, if_exists, if_does_not_exist). Create file stream backed by OS file handle. Dependencies: habu-design-stream-obj-270e828e. Verify: (open "file.txt" :direction :input).
