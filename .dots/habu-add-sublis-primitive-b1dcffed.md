---
title: Add sublis primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:23.728510+02:00"
---

src/runtime/primitives/list.zig: Implement sublis function
- sublis: substitute using association list
- Traverse tree, replace keys with values from alist
- Support :test/:test-not/:key parameters
- Non-destructive (copy tree structure)
- Add tests for nested substitution
- Est: 25 min
