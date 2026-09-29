---
title: Pass task, signal and guard suites on x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.791095+03:00"
blocks:
  - habu-port-signals-crash-2c7768ca
  - habu-port-the-crash-99c87339
  - habu-port-the-profiler-97103e6e
  - habu-add-the-x86-efc81b26
  - habu-marshal-ffi-calls-46dfd999
  - habu-add-linux-family-56c75040
---

Problem: the task, signal-stub, stack-guard and io_uring suites have never run on x86; their x86 bodies arrive in K10a-c, K11c, R2 and R3. Split from habu-port-the-ffi-676f745d (task entry and guard-page model).
Acceptance: the `tasking-threads`, `task-entry`, `signal-stub`, `stack-guard` and `aio-uring` suites green on the ThinkPad.
Files: `lib/task.f`, `src/os/linux-x86-64/sign.f` and the suite files as the reds require.
Verify: ThinkPad: each suite through the cross-built `hb-x64`.
Depends: habu-port-signals-crash-2c7768ca (K10a), habu-port-the-crash-99c87339 (K10b), habu-port-the-profiler-97103e6e (K10c), habu-add-the-x86-efc81b26 (K11c), habu-marshal-ffi-calls-46dfd999 (R2), habu-add-linux-family-56c75040 (R3).
Route: Alder (shared: lib/task.f and any shared suite file it changes).
Ownership: krait (Intel lane).
Claim: unassigned.
