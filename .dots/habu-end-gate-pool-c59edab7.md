---
title: "End gate-pool rows through PROCESS's capture end"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T05:49:19.377208+02:00"
---

Problem: test/gate-pool.f keeps a private copy of the end PROCESS:KILL-CAPTURE now takes (GT-POOL-CATCHES-TERM? :557, GT-POOL-ASK-END :567, GT-POOL-AWAIT-END :576, GT-POOL-GRACE-MS :58), mirroring src/lib/process.f CATCHES-TERM? :510, AWAIT-END :500, END-TREE :510 and END-GRACE-MS :487. The copy stays because the pool asks every row first and then awaits them, so the graces run side by side; KILL-CAPTURE asks and awaits one capture. Acceptance: gate-pool ends its rows through PROCESS words (one ask step and one await step a caller can run over many children), its private copy and GT-POOL-GRACE-MS go, graces still run side by side, and the pg, gate-signal and stdlib-process-fixtures suites pass. Adding the public ask/await words is a new public API: it goes to Joel as a design first. Files: src/lib/process.f, test/gate-pool.f. Verify: bin/hb --load test/run.f -- pg gate-signal stdlib-process-fixtures (pg needs initdb and postgres on PATH). Depends: none. Ownership: carl. Claim: unassigned.
