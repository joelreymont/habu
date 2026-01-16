---
title: Add multiple-value-call primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:32.506655+02:00"
---

src/compiler/compile.zig: Implement multiple-value-call
- multiple-value-call: call function with all values from forms
- Collect all values from each form
- Apply function to collected values as separate args
- Add IR node and bytecode support
- Add tests for value gathering
- Est: 30 min
