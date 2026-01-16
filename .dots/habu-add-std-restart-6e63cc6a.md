---
title: Add standard restart functions
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:55.271306+02:00"
---

lib/stdlib.habu: Implement restart convenience functions
- Depends on: dot (invoke-restart primitives)
- abort: invoke abort restart
- continue: invoke continue restart
- muffle-warning: invoke muffle-warning restart
- store-value: invoke store-value restart with value
- use-value: invoke use-value restart with value
- Add tests for standard restarts
- Est: 20 min
