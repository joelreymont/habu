---
title: "Chain the cleanups a task registers with TASK:AT-EXIT"
status: closed
priority: 1
issue-type: task
created-at: "2026-09-22T04:22:40.164893+03:00"
closed-at: "2026-09-22T04:50:41.952337+03:00"
close-reason: "Landed as 00f50ae8 (TASK:AT-EXIT chains: TASK-EXIT-LINK row, head release-published with atomic!, same-quotation dedupe through the locked scratch cell, first throwing cleanup is the task's error, TASK-EXIT-MAX $80) and 7721ead7 (AIO:LOOP-RUNNING?). Pinned: task-test order 21 / dedupe 1 / throw chain; aio-test CASE-OWN-EXIT (own cleanup + one poll: mark 1, ring idle); aspen's reproducer passes on the line tree."
---

Problem: TASK:AT-EXIT holds one quotation per task (TASK-EXIT-QT row, TCB.EXIT-SLOT) and registering again replaces it; lib/aio.f ENSURE-SCRUB registers SCRUB at a task's first submission, so a program's cleanup registered before the first wait is dropped (Tender's HTTP workers lose RUN-EXIT-HOOKS: 4 Postgres backends where 2 are expected after HTTP:STOP) and one registered after drops AIO's. docs/aio.md documented it as a limitation. Design: registration is additive - a per-task chain through a TASK-EXIT-LINK row, head in TCB.EXIT-SLOT published last, newest runs first, the same quotation registered again is one registration (identity read through BYTE-VIEW CELL-VIEW on the typed cell; = on quotations is E-MISMATCH), registration locked, TASK-RUN-EXIT lock-free; first throwing cleanup is the task's error, every cleanup runs; TASK-EXIT-MAX $80. Acceptance: task-test cases for order, dedupe and the throw rule; aio-test reproducer (own cleanup + one wait: hook runs, scrub runs); docs/threads.md and docs/aio.md state the chain rule. Blocks: the EO/EP root flip (aspen gates on it). Ownership: hazel; lane .jj-ws/hazel-exit-chain.
