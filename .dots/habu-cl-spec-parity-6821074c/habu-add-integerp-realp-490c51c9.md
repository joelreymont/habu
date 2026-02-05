---
title: Add integerp/realp
status: open
priority: 1
issue-type: task
created-at: "2026-02-05T12:11:38.644118+01:00"
---

docs/cl-symbols.md:1091/1104 mark integerp/realp missing. Root cause: only typep/subtypep + numberp/rationalp/complexp exist. Fix: implement integerp/realp primitives (Value kind checks) in src/runtime/primitives/arith.zig (or new predicates.zig), wire into compiler builtins+IR (like numberp), add unit tests.
