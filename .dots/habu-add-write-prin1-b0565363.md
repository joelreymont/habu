---
title: Add write/prin1/princ/print primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:36.458286+02:00"
---

src/runtime/primitives/io.zig: Implement standard output functions
- write: full control with all :escape/:base/:case/etc keywords
- prin1: write with *print-escape* = t
- princ: write with *print-escape* = nil
- print: prin1 + newline + space
- Add tests for all output functions
- Est: 25 min
