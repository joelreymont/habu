---
title: log.f says a line logged from a cleanup leaks its row
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T13:02:01.369092+02:00"
---

Problem: src/lib/log.f:17-19 says a line logged from a TASK:AT-EXIT cleanup leaks its row because the release LINE registers joins a chain already running; habu-a-cleanup-registered-e2416748 (landed) makes a registration during the ending run before the task ends, so the sentence is likely false, and no log-test case logs from a cleanup. The scratch reproducer /Users/joel/Work/reviews/habu-10x-2026-11-10/exit-chain/cleanup-log.f (run2.txt is its output before the fix) no longer loads on 9756ff01: it reads LOG's private LIVE, which is gone. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a case in src/lib/log-test.f logs its task's first line from a cleanup and shows the row released after the join; the header sentence states what the case shows. Files: src/lib/log.f, src/lib/log-test.f. Verify: the case fails on a tree before e2416748's fix or the dot says why that cannot be shown. Depends: habu-a-log-test-8756554e landed (same test file). Ownership: LOG. Claim: agent=bob workspace=.jj-ws.noindex/bob-withcwd

Narrowed 2026-11-10 (triage; Joel: "Close as triaged"): correct only the header sentence at src/lib/log.f:17-19 to what habu-a-cleanup-registered-e2416748 makes true; no new test case.

Shown 2026-11-10 (bob): src/lib/task.f TASK-RUN-EXIT runs the newest owed row until none is owed, so a cleanup registered while the cleanups run runs in the same ending (its comment, and the case at src/lib/task-test.f:1089); LOG's REGISTER uses TASK:AT-EXIT, so the row is released. The header now says so.
