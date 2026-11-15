---
title: Test a warm build at a full dictionary
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T17:19:45.392658+01:00"
closed-at: "2026-11-16T04:07:33.875849+01:00"
---

Problem: no test covers src/habu/linker.f:4175, which keeps a warm build from overflowing the dictionary where a cold build rejects. Acceptance: a test shows the warm build rejects a full dictionary as the cold build does. Depends: habu-reject-every-full-f8d2fc3b. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (commit mnkunwml, stacks after the third merge, closes at landing).
