---
title: "Cut the smoke test's drain polls from a second to 100 ms"
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-17T13:24:25.053161+03:00\""
---

Problem: test/process-pty-tty-smoke.f spends nearly its whole 15 s in DRAIN: five drains times three quiet polls of STEP-MS ($3E8 = 1000 ms) each; nothing in the case bodies needs a full second of silence to know the child is quiet (pty lane, 2026-09-17: the longest real wait measured was 26 ms). Acceptance: the quiet poll is 100 ms with the barrier semantics unchanged (QUIET-POLLS consecutive quiet polls), the suite runs in about 2 s, and ten consecutive runs are green at the load stated. Files: test/process-pty-tty-smoke.f. Verify: ten runs; wall clock before and after. Depends: none. Ownership: process/pty tests. Claim: agent=hazel-pty-barriers workspace=.jj-ws/hazel-pty-barriers.
