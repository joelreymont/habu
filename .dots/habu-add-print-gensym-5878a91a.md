---
title: Add *print-gensym* support
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:35.851279+02:00"
---

src/interp/vm.zig + io.zig: Implement *print-gensym* variable
- Add global *print-gensym* variable (boolean)
- When t, print #: prefix for uninterned symbols
- When nil, omit prefix
- Add tests for gensym printing
- Est: 15 min
