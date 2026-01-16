---
title: Add *print-circle* support
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:34.330320+02:00"
---

src/interp/vm.zig + io.zig: Implement *print-circle* variable
- Add global *print-circle* variable (boolean)
- Track already-printed objects with identity table
- Print #n# for back-references, #n= for definitions
- Handle circular structures without infinite loop
- Add tests for circular lists/structures
- Est: 30 min
