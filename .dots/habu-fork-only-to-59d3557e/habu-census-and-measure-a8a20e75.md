---
title: Census and measure fork-and-continue callers
status: open
priority: 1
issue-type: task
created-at: "2026-10-28T19:04:35.626601+02:00"
---

Worker lane, read and measure only. For each caller listed in the parent: what parent state the child uses (loaded definitions, open fds, cells), what it would need loaded in a fresh hb, and the cost per case: fresh hb start plus loading that setup vs today's fork, measured on the heaviest SUBJECT users and on a gate-pool worker. Output: a table feeding the design. Claim: unassigned.
