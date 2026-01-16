---
title: Add multiple-value-prog1 macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:32.811078+02:00"
---

lib/stdlib.habu: Implement multiple-value-prog1
- multiple-value-prog1: like prog1 but preserves all values
- Evaluate first form, save all values
- Evaluate remaining forms (for side effects)
- Return saved values
- Add tests for multiple value preservation
- Est: 15 min
