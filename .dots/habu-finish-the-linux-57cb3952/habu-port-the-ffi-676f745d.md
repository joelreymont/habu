---
title: Bring the x86-64 runtime to parity
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T17:32:42.546263+03:00"
blocks:
  - habu-marshal-ffi-calls-46dfd999
  - habu-add-linux-family-56c75040
  - habu-pass-task-signal-bdb4c4f4
  - habu-pass-crash-and-c451eb43
  - habu-build-stripped-images-b27cfad7
  - habu-write-snapshots-and-25a9e6b7
---

Lane R: runtime parity on x86-64. The original scope of this dot (SysV AMD64 marshalling selected by target, the VM preserved across a foreign call by the callee-saved rule, task entry and the guard-page model, signal frames read through RIP and RSP, the FFI/task/guard suites on the ThinkPad) is split into habu-add-sysv-ffi-17a130a1 (K11a), habu-add-sysv-abi-75f86980 (K11b) and habu-add-the-x86-efc81b26 (K11c) (trampolines and task entry), habu-port-signals-crash-2c7768ca (K10a), habu-port-the-crash-99c87339 (K10b) and habu-port-the-profiler-97103e6e (K10c) (signal frames, crash, profiler), habu-sign-extend-c-c7f55f0e (R1: C-int result width, both arches), habu-marshal-ffi-calls-46dfd999 (R2: SysV marshalling) and habu-pass-task-signal-bdb4c4f4 (R4: task, signal and guard suites). The variadic-`al` case this dot named (SysV makes libc `syscall` a true variadic call and a variadic callee reads `al` as the vector-register count; `lib/aio.f`'s two `syscall` rows are the first consumers and `docs/aio.md` names the seam) is answered by K11a/K11b setting `al` on every call and proved by R2's dirty-`al` case. The lane also holds the kernel-facts library predicates (R3), crash and profiler suites (R5), stripped images and tools (R6) and snapshots (R7). R1 is a baseline defect independent of everything else and lands first.
Leaves: habu-sign-extend-c-c7f55f0e (R1), habu-marshal-ffi-calls-46dfd999 (R2), habu-add-linux-family-56c75040 (R3), habu-pass-task-signal-bdb4c4f4 (R4), habu-pass-crash-and-c451eb43 (R5), habu-build-stripped-images-b27cfad7 (R6), habu-write-snapshots-and-25a9e6b7 (R7).
Campaign lane only; do not dispatch. It lists its leaves under blocks: so it stays off dot ready until they close.
Ownership: krait (Intel lane).
Claim: unassigned.
