---
title: Implement missing array functions
status: closed
priority: 2
issue-type: task
created-at: "2026-01-18T06:26:57.260958+02:00"
closed-at: "2026-01-18T08:26:00+02:00"
close-reason: "All vector/array functions implemented: fill-pointer, vector-push/pop/push-extend fully working. Added %adjust-array primitive (opcode 0xB7) wired to adjustArray in vector.zig. array-displacement returns (values nil 0) per spec."
---

Files: stdlib.habu, src/runtime/primitives/vector.zig
fill-pointer, vector-push, vector-push-extend, vector-pop.
adjust-array, array-displacement.
Currently marked ⚠ (stub).
Make functional implementations.
Verify: zig build test with array operation tests.
Est: 60min
