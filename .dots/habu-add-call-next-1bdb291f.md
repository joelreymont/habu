---
title: Add call-next-method for combinations
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:47.998217+02:00"
---

src/interp/vm.zig: Extend call-next-method for qualifiers
- Depends on: dot (qualified method dispatch)
- call-next-method in around calls next around or primary
- call-next-method in primary calls next less-specific primary
- Error if no next method available
- Add tests for next-method chains
- Est: 25 min
