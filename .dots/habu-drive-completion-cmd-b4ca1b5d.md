---
title: Drive completion command
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:31:05.723904+02:00"
blocks:
  - habu-start-completion-srv-16b95afe
  - habu-infer-serve-concurrent-a5e7de35
  - habu-finish-completion-srv-a3944f96
---

Why: a published server needs one command lifetime separate from startup and final composition. Result: SERVE-CMD:DRIVE consumes running(opts,server,bound-address). Without --once it calls SERVE:RUN-ONCE indefinitely until a named iteration failure; with --once it stops only after one complete Content-Length response is written. Every exit calls SERVE:STOP. A STOP refusal returns the exact stop-only owner and storage without attempting later cleanup. After STOP succeeds, DRIVE uses the shared engine and model cleanup chain; only after that succeeds does it release the returned server memory through MEM:RELEASE-BYTES. Once success releases every owner; any earlier refusal preserves every not-yet-consumed owner and makes no second close or release. A release failure propagates out of the command path and never produces a success result. Owner: command loop, once condition, and ordered product shutdown only. Production red: a published server has no command lifetime or complete release path. Acceptance: once success, run failure, server-stop refusal and explicit retry, engine or model close failure before memory release, successful memory release after device cleanup, and immediate restart preserve exact owners; a real release failure emits no success marker. Forbidden: startup, normal-mode success return, caught release, hidden cleanup retry, signal handler, daemonization, thread, worker, second loop, retry of consumed resources, version, compatibility, metric, or lint. Smallest owning check: focused tools/serve-run-test.f through real SERVE:RUN-ONCE, STOP, runtime cleanup, and MEM release. Claim: unassigned.
