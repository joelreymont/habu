---
title: Add pushnew macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:23.120208+02:00"
---

lib/stdlib.habu: Implement pushnew macro
- Depends on: dot (adjoin primitive)
- pushnew: setf place to adjoin element
- Expand to (setf place (adjoin element place ...))
- Support :test/:test-not/:key parameters
- Add tests for various place forms
- Est: 10 min
