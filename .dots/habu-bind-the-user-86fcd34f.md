---
title: Bind the user-slot positions in the task suite
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T09:54:42.246445+02:00"
---

Coverage gap found by the does-conv lane's mutation: shifting every +USER slot by 8 keeps lib/task-test.f green (a uniform translation is self-consistent and the region has slack); only breaking slot DISTINCTNESS reds it. The suite binds distinctness but not position. Add one assertion that two adjacent slots differ by their declared size (and one that the first slot sits at its declared offset). Mutate a layout generator by ALIASING, not translating - recorded lesson. Files: lib/task-test.f. Depends: none.
