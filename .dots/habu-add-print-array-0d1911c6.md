---
title: Add *print-array* support
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:36.154850+02:00"
---

src/interp/vm.zig + io.zig: Implement *print-array* variable
- Add global *print-array* variable (boolean)
- When t, print array contents with #(...) or #2A(...)
- When nil, print #<ARRAY ...> unreadably
- Add tests for array printing modes
- Est: 20 min
