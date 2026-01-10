---
title: Add prog macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:25:02.189858+02:00"
---

Missing from stdlib. Add to lib/stdlib.habu.
Pattern: (prog (vars...) body...) -> let + block for return
Expand to: (block nil (let (vars...) body...))
Test: (prog (x) (setq x 10) (return x)) => 10
