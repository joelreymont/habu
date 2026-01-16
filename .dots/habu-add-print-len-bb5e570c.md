---
title: Add *print-length*/*print-level* support
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:35.245531+02:00"
---

src/interp/vm.zig + io.zig: Implement print depth limits
- Add global *print-length* (max list elements) and *print-level* (max nesting)
- Print ... when limits exceeded
- Track nesting depth during printing
- Add tests for truncation behavior
- Est: 25 min
