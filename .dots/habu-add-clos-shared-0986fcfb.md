---
title: Add CLOS shared slot allocation
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-16T13:41:46.478067+02:00\""
---

src/runtime/objects.zig: Add :allocation :class support
- Add allocation type to SlotDefinition
- Store class-allocated slots separately (in Class object)
- Update slot-value to check allocation type
- Share slot storage across all instances
- Add tests for shared slots
- Est: 30 min
