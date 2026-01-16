---
title: Add output flushing primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:28.268681+02:00"
---

src/runtime/primitives/io.zig: Implement output flushing
- finish-output: flush and wait
- force-output: flush without waiting
- clear-output: discard buffered output
- Use underlying stream flush operations
- Add tests for buffer behavior
- Est: 15 min
