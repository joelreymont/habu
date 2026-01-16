---
title: Add LOOP append/nconc/minimize/maximize clauses
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:51.025728+02:00"
---

lib/stdlib.habu: Implement additional accumulation clauses
- Depends on: dot (LOOP design)
- append: collect into accumulated list (append lists)
- nconc: destructive append
- minimize: track minimum value
- maximize: track maximum value
- Add tests for all accumulation modes
- Est: 25 min
