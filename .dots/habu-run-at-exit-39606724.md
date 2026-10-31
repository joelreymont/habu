---
title: "Run AT-EXIT cleanups registered during a task's exit walk"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T19:38:56.133452+02:00"
---

TASK-RUN-EXIT (src/lib/task.f ~904-913) reads the task's exit-chain head once and walks the links from it, so a cleanup that TASK:AT-EXIT registers while the walk runs (pushed at the head) never runs. crew.f's report runs inside an exit cleanup; a LOG line or GENIO line opened there registers its release on the same chain and leaks its BUF row. Found by the LOG lane review 2026-10-31. Fix at task.f: the walk pops from the head until the chain is empty, so a registration made during the walk runs too; a test registers a cleanup from inside a cleanup and sees it run. Workspace: none yet.
