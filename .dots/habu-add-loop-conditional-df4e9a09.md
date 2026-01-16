---
title: Add LOOP conditional execution
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:51.328436+02:00"
---

lib/stdlib.habu: Implement if/when/unless in LOOP
- Depends on: dot (LOOP design)
- if condition do/collect/sum/etc: conditional accumulation
- when/unless: aliases for if/(if (not ...))
- else: alternative branch
- Support nested conditionals
- Add tests for conditional clauses
- Est: 25 min
