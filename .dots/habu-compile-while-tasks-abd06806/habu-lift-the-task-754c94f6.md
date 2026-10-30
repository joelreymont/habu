---
title: Lift the task refusal on x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T17:24:59.364026+02:00"
---

Problem: W^X landed for arm64 (679c7755 two views, efc74616 compile while tasks run); the x86-64 engine still refuses compilation while any task is live and has no two views. src/habu/kernel-x64.f:244 TASK-LIVE-GUARD, (callers :1966, :2029, :3136) tests TASKS-LIVE-CELL, not the thread's TASK-TCB-CELL as habu1.f B-TASK-GUARD does; src/habu/prof-x64.f:408-409 skips only DICT-WL:RETIRED (-2/-1) and must also skip DICT-WL:RETIRED-NAMESPACE (-3); kernel-x64.f comments name deleted habu1.f words (carl-wx/.out/wx-facts.md lists them). Until this lands, test/task-test.f, test/outer-interpret.f and test/native-eval.f are red on the x86-64 host (pulsar). Acceptance: the x86-64 engine maps the region through two views as arm64 does (carl-wx/.out/wx-design.md), the guard reads the thread's TASK-TCB-CELL, the span reader skips -3, the stale comments go; those three rows, src/lib/process-fork-test.f and test/code-window.f rc 0 on pulsar with an engine built from the lane. Files: src/habu/kernel-x64.f, src/habu/prof-x64.f, src/habu/boot-x64.f. Verify: the rows above on joel@pulsar-ua-106. Depends: none (after arm64 end to end, one arch first). Claim: unassigned.
