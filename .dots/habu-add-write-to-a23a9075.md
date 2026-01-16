---
title: Add write-to-string primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:26.756754+02:00"
---

src/runtime/primitives/io.zig: Implement write-to-string
- write-to-string: convert object to string
- Respect *print-escape*, *print-readably*
- Return string representation
- Use existing printer logic with string output
- Add tests for various object types
- Est: 20 min
