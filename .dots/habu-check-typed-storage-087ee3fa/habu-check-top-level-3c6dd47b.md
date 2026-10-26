---
title: Check top-level literals in constant time
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T18:18:29.265835+02:00"
---

Rule defect 2: per event the interpret row loads and saves the whole row (E-COPY per cell): 2000 literals 1.21 s vs 0.06 s, 8000 die rc 102. Requirement O(1) amortized per token, refusals unchanged. Design: Plan brief /Users/joel/.cache/tmp/carl-file/brief-litcost-design.md (running); then a worker lane on carl-trdiag or carl-trtype.
