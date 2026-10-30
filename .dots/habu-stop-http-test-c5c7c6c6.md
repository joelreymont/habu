---
title: "Stop http-test's FLOOD-CASE flaking under load"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T23:21:07.014475+02:00"
---

src/lib/net/http-test.f:2096 asserts FLOOD-ENDED @ raised > TTRUE and fails about 3 runs in 40 on master and on H1 alike (FLOOD-only harness, /tmp/carl-h1-rev/runs/fo-summary.txt). FLOOD's requests end silent or stopped with a plain close and never reach LINGER or REPLY. Next: reproduce under load, find whether the assert or the server's flood handling is wrong, fix that layer. Not claimed.
