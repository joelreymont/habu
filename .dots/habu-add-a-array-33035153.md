---
title: "Add #A array reader macro"
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:52.237998+02:00"
---

src/reader/parser.zig: Implement array reader
- #A(dims element-type contents): read multi-dimensional array
- #2A((row1) (row2)): convenient 2D array syntax
- Expand to make-array with :initial-contents
- Add tests for array reading
- Est: 25 min
