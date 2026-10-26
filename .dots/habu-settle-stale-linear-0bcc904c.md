---
title: Settle stale linear tokens after catch
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T15:05:57.070564+02:00"
---

Problem: docs/forth.md (:1274-1280, :2232-2238) says a checked catch leaves a possibly overwritten input cell stale<t>, and that a stale value may be dropped unless linear. The checker instead admits a linear token through a caught quotation with its type intact: PCMDT-SPAWN-FAILS (src/lib/process-command-test.f) and PTT-CANCEL-PRE-KEPT (src/lib/process-task-test.f) each catch a throwing CMD:RUN-OUTCOME inside a CMD:WITH-COMMAND body, then call CMD:OUTCOME@ on the same CMD:command token, and the checker admits both. Either the rule or the checker is wrong, and CMD's reachable FAILED state depends on the answer. Acceptance: one rule for a linear token in a caught quotation's input window, stated in docs/forth.md and enforced by the checker, with a check-test row that fails on the other behavior; CMD (process-command.f) and ZED still check under it. Verify: tools/check-test.f, tools/check.f on process-command.f, and those two rows: bin/hb --load src/lib/process-command-test.f and bin/hb --load src/lib/process-task-test.f.
