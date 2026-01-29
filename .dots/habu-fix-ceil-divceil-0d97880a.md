---
title: Fix ceil divCeil error mapping
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:05:35.919030+01:00"
---

Context: src/runtime/primitives/arith.zig:873-880; cause: divCeil error mapped to TypeMismatch; fix: propagate Overflow/DivisionByZero correctly; deps: none; verification: add ceil overflow test, run zig build test --filter arith
