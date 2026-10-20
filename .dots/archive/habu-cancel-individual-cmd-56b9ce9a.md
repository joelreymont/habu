---
title: Cancel individual command captures
status: closed
priority: 1
issue-type: task
created-at: "2026-10-20T16:15:40.106763+03:00"
closed-at: "2026-10-20T17:33:13.470387+03:00"
close-reason: Implemented per-call borrowed-FD cancellation, isolated child-tree cleanup and captured-prefix access. Independent adversarial review passed; macOS source/native consumers, byte-identical generations and all 677 native suites passed. Documented source-only recovery produced a native Linux AArch64 engine; focused process, command, task, capture-tree and signal consumers plus a stripped native cancellation application passed. Reconciled current master and reran affected test utilities and command/task consumers. Public product integration remains separate; no installed engine was replaced.
---

Public 09fea843 has no composable per-call cancellation boundary for CMD captures. Bob retained /Users/joel/.cache/tmp/bob-task-halt-ajc_j6ms/task-halt-run.f and task-halt.f: a worker captures /bin/sleep 1, main requests TASK:HALT after 50 ms, JOIN waits another 954 ms and capture returns normally. This is a capability gap, not a violation of the cooperative TASK:HALT contract. Existing PROC-STOP-FD and PROC-STOP are process-wide signal policy and cannot isolate simultaneous calls. Provide explicit per-call cancellation for command capture, including prompt wakeup, child-tree termination and reap, capture-prefix/cleanup semantics, and isolation from other running captures. Retain the supplied real child-process repro; inspect existing process/capture E2Es before extending them, with concurrent unaffected-call control. Independent review and proportionate source/native consumer qualification required. Joel assigned implementation to Bob; Kaizen consumes the qualified public release afterward.

Acceptance: CMD:CANCEL-FD! borrows one pipe read end for one run, and cancellation throws E-PROC-CANCELLED after capture cleanup without changing the outcome ABI or another command. CMD:CAPTURE-LENGTHS preserves valid prefix lengths for fixed and growing captures. Pre-signalled requests never spawn, getters retain cancellation, and reset or synchronized reuse disconnects old requests. Verify with src/lib/process-task-test.f through source and a native application, existing process/command/tree/task/signal consumers, the native suite, and independent review. Keep this dot active until qualification and review finish.
