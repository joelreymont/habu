---
title: Census and measure fork-and-continue callers
status: closed
priority: 1
issue-type: task
created-at: "2026-10-28T19:04:35.626601+02:00"
closed-at: "2026-10-28T19:54:55.017412+02:00"
close-reason: "Census done (carl-fork): fork costs 4.0 ms CPU per run; a fresh hb floor is 26.7 ms; the heaviest SUBJECT file takes 0.5 s and about 1.6 s with spawned children loading their libraries."
---

Worker lane, read and measure only. For each caller listed in the parent: what parent state the child uses (loaded definitions, open fds, cells), what it would need loaded in a fresh hb, and the cost per case: fresh hb start plus loading that setup vs today's fork, measured on the heaviest SUBJECT users and on a gate-pool worker. Output: a table feeding the design. Claim: unassigned.
