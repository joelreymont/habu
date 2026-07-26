---
title: Synchronize proc-watch dead-path assert
status: active
priority: 1
issue-type: task
created-at: "2026-07-26T23:25:28.994601+02:00"
---

Gate blocker (deterministic red under the standard-library pool; 20 isolated runs green): proc-watch-primitive-smoke assertion 9 infers child exit from alive-pipe EOF, but EOF proves only that the pipe write end closed before die - Linux can then open a still-live pidfd and the zero-time poll correctly reports not-ready. The synchronization predicate is wrong; the box is not slow.

THE FIX, one design, frozen: the non-consuming exit barrier is a FIRST watch. Hold the to-be-dead child on a go pipe; the parent opens watch A while the child is provably live, releases the child, and waits for watch A readiness - kernel-observed process death, no reap. Then open watch B on the surviving zombie and assert the already-dead contract (Linux immediate readiness; macOS fail-closed), close both, and only then wait/reap. Delete the racy alive-pipe EOF inference entirely. Deterministic, exercises the production primitive as its own barrier, needs no new syscall. Forbidden: sleeps, timeout increases, poll retries, reap-before-open (reaping destroys the zombie contract the fixture exists to test), and any alternative synchronization mechanism - this contract has exactly one design. Preserve the live-child case and macOS semantics.

Owner: the proc-watch smoke suite. Dependencies: none. Acceptance: the focused smoke green under aggregate contention (run beside a competing load, the condition that reds it today) plus the full standard-library gate; no timing constants changed.

Claim: agent=codex workspace=.jj-ws/codex-proc-watch-dot
