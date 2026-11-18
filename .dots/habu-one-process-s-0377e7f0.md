---
title: "One process's io_uring ring fits many processes' locked memory"
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T15:11:11.942065+01:00"
---

Problem: src/lib/aio.f sizes every process's ring for a server (AIO-CQ-ENTRIES = 2 x AIO-RECS-MAX = 8192 CQEs, 256 SQEs, 37 pages). Kernel 6.17 charges each ring to the user's locked memory user-wide and refuses one past RLIMIT_MEMLOCK: under spark's 8 MiB the 56th ring across joel's processes fails E-AIO-SETUP, so one-shot hb runs fail while servers and tests hold rings (mark's /tmp/mark-uring/REPORT.md). Acceptance: a one-shot hb process charges a few pages; a server still holds thousands of waits without dropping a completion; repro.sh at LIMIT_KB 1024 starts many more hold.f processes than today. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-uring
