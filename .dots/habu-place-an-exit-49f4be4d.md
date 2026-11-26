---
title: "Place an exit's return-stack meet rejection at the closer"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T13:15:47.957632+01:00"
---

Problem: src/core/check-walk.f CHECK-FOLD-EXITS joins the exits' rows with the body's end at the closer, but a rejection from that join takes the place of the token walked last: `: W ( n -- ) 0 >r 0= if r> drop true >r exit then r> drop ;` is rejected at the final `drop` (1:54) with 'the paths that meet here', where the paths meet at `;`; an if-arm meet is placed at its `then`. Acceptance: that program is rejected at `;` (1:59), and the same with `does>` and `;]` as the closer; test/diag-position-test.f's rmeet-exit case asserts it. Depends: none.
