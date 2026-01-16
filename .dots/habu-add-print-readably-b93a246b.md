---
title: Add *print-readably* support
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:34.939099+02:00"
---

src/interp/vm.zig + io.zig: Implement *print-readably* variable
- Add global *print-readably* variable (boolean)
- When t, print so read produces equivalent object
- Override *print-escape*, *print-length*, etc
- Error if object not readable
- Add tests for readable output
- Est: 20 min
