---
title: Add make-condition primitive
status: closed
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:53.751078+02:00"
---

src/runtime/primitives/primitives.zig: Implement condition creation
- Depends on: dot (condition object types)
- make-condition: create condition instance
- Support condition type + initargs
- Use CLOS make-instance if condition is CLOS class
- Add tests for various condition types
- Est: 20 min
Resolution: Added Heap.allocCondition, condition.zig primitive, stdlib wrapper using make-instance.
