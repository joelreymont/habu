---
title: Poll server sockets
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:12:14.601922+02:00"
blocks:
  - habu-own-completion-srv-dfc8812e
  - habu-drive-completion-req-39137097
---

Why: listener and connection readiness must advance without blocking scheduler work. Interface: package-private SERVE:POLL-IO builds interests from the listener and fixed connection rows, chooses timeout zero whenever SCHED reports runnable work and otherwise the bounded nearest idle deadline, polls once, accepts into free rows until would-block, drives each reported READABLE or WRITABLE connection once, and expires idle rows. It returns the server with no scheduler tick or result routing. Owner: one bounded socket-poll iteration only. Production red: a runnable scheduler can stall behind an unrelated poll timeout. Acceptance: runnable work progresses with no file-descriptor readiness, nearest deadline bounds idle wait, full capacity suppresses accept, and three fragmented or slow clients remain isolated through poll, accept, read, write, timeout, and errno failures. Forbidden: scheduler execution, result routing, general event loop, busy loop, thread, dynamic rows, metrics, retry, version, or compatibility path. Smallest owning check: focused multi-client poll traces with real SOCK-OS descriptors.
