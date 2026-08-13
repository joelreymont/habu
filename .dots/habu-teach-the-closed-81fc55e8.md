---
title: Teach the closed-forming pass the do-while trip count
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T05:51:41.964153+02:00"
---

Found by the do-loop landing (1bfc2749): loop.f declines plain-do loops because its precondition is a pre-header entered from a brz over limit-start, which plain do lacks - sound (the guard's presence implies limit != start, exactly where the openers agree; DOQ-CASE measures a ?do nested in a plain do still folding). The unclaimed win: teach the pass the do-while trip count (limit-start, floor 1) so plain-do loops fold too; native-do.f records where the loop-count rows move. Files: src/compiler/native/loop.f. Depends: none.
