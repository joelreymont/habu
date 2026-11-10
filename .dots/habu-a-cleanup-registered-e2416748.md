---
title: "A cleanup registered during a task's exit never runs"
status: active
priority: 3
issue-type: task
created-at: "2026-11-10T10:28:37.038544+02:00"
---

Problem: src/lib/log.f:13-15 states that a line logged from a TASK:AT-EXIT cleanup leaks its row: LINE registers the row's release with TASK:AT-EXIT (log.f:69-78), and the release then joins a chain already running. TASK-RUN-EXIT (src/lib/task.f:901-908) reads the chain's head once, so a cleanup registered while it runs is not visited, although its comment says every one of them runs. The row is an OS mapping, leaked on each such run. Found by the code review of package LOG, 2026-11-10 (~/Work/reviews/habu-10x-2026-11-10/log-review.md); no caller reaches it today and nothing tests it. Lines are as of 92c8b01b. Acceptance: first a reproducer and a reported cause, with no code change: a task whose AT-EXIT cleanup logs a line, showing the row's mapping is not released, and what in the exit chain misses it. Then a cleanup registered during the exit chain runs before the task ends, LOG's header drops the leak sentence, and a test shows the row released. Files: src/lib/task.f, src/lib/log.f, their tests. Verify: the reproducer and src/lib/task-test.f. Depends: none. Ownership: src/lib/task.f's exit chain. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-cleanup-registered-e2416748
