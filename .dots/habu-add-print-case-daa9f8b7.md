---
title: Add *print-case* support
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:34.028609+02:00"
---

src/interp/vm.zig + io.zig: Implement *print-case* variable
- Add global *print-case* variable (:upcase/:downcase/:capitalize)
- Update symbol/string printing to respect setting
- Default to :upcase (CL standard)
- Add tests for all case modes
- Est: 20 min
