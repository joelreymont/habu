---
title: Fix floor/ceiling two-arg behavior
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T07:17:55.456502+02:00"
---

src/runtime/primitives/arith.zig

(floor 17 5) returns 17 instead of 3
(ceiling 17 5) returns 17 instead of 4

These should divide first, then floor/ceil the result.
Currently they just floor/ceil the first arg ignoring second.

Test:
(floor 17 5) => 3
(ceiling 17 5) => 4
(floor 2.5) => 2
(ceiling 2.5) => 3
