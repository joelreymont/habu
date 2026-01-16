---
title: Add stream query primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:28.873907+02:00"
---

src/runtime/primitives/io.zig: Implement stream predicates
- file-length: return file size in elements
- stream-element-type: return element type (character/byte)
- streamp: test if stream
- input-stream-p: test if input stream
- output-stream-p: test if output stream
- interactive-stream-p: test if interactive (tty)
- open-stream-p: test if stream is open
- Add tests for all predicates
- Est: 20 min
