---
title: Grow eval frame arena
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T15:47:04.151739+02:00"
---

Full Maki gate hits native BRK at src/habu/habu1.f C-EVAL depth guard: EVALD=8 equals EVAL-MAX-DEPTH while LAYOUT-BUFFER performs one nested generated-accessor evaluate inside a legal include chain. Relocate 16 eval frames above task-user cells, bump DATA-START, mirror bootstrap, make task-user stop at EVAL-FRAME, derive snapshot zero span, add capacity invariant regression, and rerun native/bootstrap/fixpoint gates.
