---
title: "Add #S struct reader macro"
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-16T13:41:51.938751+02:00\""
---

src/reader/parser.zig: Implement structure reader
- #S(struct-name :slot1 val1 :slot2 val2): read structure
- Expand to (make-struct 'struct-name :slot1 val1 ...)
- Support defstruct-defined types
- Add tests for struct reading
- Est: 25 min
