---
title: Change logtest/logbitp signatures
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T10:24:00.333556+02:00"
---

src/runtime/primitives/arith.zig:217,226 - Change return type to Error!bool:
1. logtest: if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch
2. logbitp: same check plus range check for pos
Return actual bool result if valid
Verification: Type errors propagate, not false
