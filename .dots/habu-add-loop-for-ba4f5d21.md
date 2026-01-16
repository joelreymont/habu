---
title: Add LOOP for/being/symbols clauses
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:49.811997+02:00"
---

lib/stdlib.habu: Implement symbol iteration
- Depends on: dot (LOOP design, do-symbols macros)
- for sym being symbols of package: iterate package symbols
- for sym being present-symbols/external-symbols: filter iteration
- Use do-symbols/do-external-symbols internally
- Add tests for symbol iteration
- Est: 25 min
