---
title: Add LOOP with clause
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:50.417600+02:00"
---

lib/stdlib.habu: Implement local variable binding
- Depends on: dot (LOOP design)
- with var = expr: bind local variable before loop
- Support multiple with clauses (parallel binding)
- with var = expr and var2 = expr2: sequential binding
- Add tests for local variables
- Est: 20 min
