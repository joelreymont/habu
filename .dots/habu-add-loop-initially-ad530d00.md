---
title: Add LOOP initially/finally clauses
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:50.113777+02:00"
---

lib/stdlib.habu: Implement loop prologue/epilogue
- Depends on: dot (LOOP design)
- initially: execute forms before loop starts
- finally: execute forms after loop ends (before return)
- Support multiple initially/finally clauses
- Add tests for initialization/cleanup
- Est: 20 min
