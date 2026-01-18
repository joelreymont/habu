---
title: Implement CLOS method combinations
status: open
priority: 2
issue-type: task
created-at: "2026-01-18T06:26:51.885885+02:00"
---

Files: src/runtime/primitives/clos.zig, stdlib.habu
Add :before/:after/:around method qualifiers.
Implement call-next-method primitive.
Wire into defmethod compilation.
Add tests for method combination dispatch.
Verify: zig build test passes with method combination tests.
Est: 90min
