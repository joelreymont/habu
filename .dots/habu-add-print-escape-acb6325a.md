---
title: Add *print-escape* support
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:34.634496+02:00"
---

src/interp/vm.zig + io.zig: Implement *print-escape* variable
- Add global *print-escape* variable (boolean)
- When nil, print strings without quotes, symbols without escapes
- When t, print readably (default)
- Update printer to check setting
- Add tests for escaped vs unescaped output
- Est: 15 min
