---
title: Remove hardcoded x9 scratch - use virtual registers throughout
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-08T11:19:45.997828+02:00"
closed-at: "2025-12-09T09:29:52.736513+02:00"
close-reason: ""
---

Fix register allocation design: remove x9 as hardcoded scratch, make it allocatable, use virtual registers for all temporaries in TAC generation. Fix tac-call to only save registers that are actually allocated.
