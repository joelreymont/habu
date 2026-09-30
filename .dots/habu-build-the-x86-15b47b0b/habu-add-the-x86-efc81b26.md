---
title: Add the x86-64 task entry
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.897036+03:00"
---

Problem: `BTASK-ENTRY` (`src/habu/habu1.f:2099`) is ARM64 code. Split from habu-port-the-ffi-676f745d (task entry).
Acceptance: `task-entry` synthesises the pthread entry stack and VM registers; the return path and state restoration follow `habu1.f:2099` semantics; the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/boot-x64.f`, `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: a routine image that starts a pthread through `task-entry` and returns.
Depends: habu-add-sysv-ffi-17a130a1 (K11a).
Route: direct.
Ownership: krait (Intel lane).
Claim: krait.

Preflight corrections (2026-09-30; override the lines above where they differ):
- `BTASK-ENTRY` is at `src/habu/habu1.f:2025-2051` (not 2099). The peer entry cannot run a kernel body (`test/x86-64-boot-harness.f:17-20`); kernel rows are proved through the booted harness, as K11a (`habu-add-sysv-ffi-17a130a1.md:23-27`). `boot-x64.f` holds only `_start` and the signal emitters; the FFI twins are `kernel-x64.f` `FFI,`.
- Acceptance: `X64KERNEL` gains a `TASK,` section appended to `KERNEL,` after `FFI,`, holding the `task-entry` PRIM `( -- n )` (`prims.f:632`). The body pushes the entry label's address (`MOVABS,`) and jumps over the entry, so the entry sits inside the row's record as on ARM. The entry is a SysV function with rdi = the TASK-ABI descriptor: it pushes rbx, rbp, r12-r15 (the SysV callee-saved set is exactly the VM set, `docs/x86-64.md:63-66`; no d8-d15 twin), loads rbp←REGION-OFF, r12←STACK-OFF, r13←DBASE-OFF, r14←NDICT-OFF, r15←CP-OFF, rbx←0, stores rdi at [rbp+TASK-TCB-CELL], calls [rdi+XT-OFF], reloads the TCB, publishes DONE at STATUS-OFF with `xchg` (ARM publishes with the STLR behind `atomic!`, `habu1.f:2012-2016`; x86 `atomic!` is `xchg`, `kernel-x64.f:1581-1585`), zeroes eax, pops in reverse and returns. It never writes RUNNING.
- Files: `src/habu/kernel-x64.f`; new `test/x86-64-kernel-task.f` (package `X64K-TASK`, `require test/x86-64-boot-harness.f`); `test/gate-stdlib-cases.f` (`SUITE x86-64-kernel-task` after `x86-64-kernel-ffi`); `docs/x86-64.md` ("### Task entry" after "### FFI rows").
- Verify (ThinkPad, native): `hb-x64-kernel-task` exits 0 and `-negative` 21. (1) A direct `ffi-call` of the entry on a scratch descriptor answers 0; the caller's rbp, depth and balance survive. (2) `pthread_create`/`pthread_join` via `DLSYM,` (GETPID pattern, `test/x86-64-kernel-ffi.f:124-134`): both answer 0, retval 0; the descriptor's status is DONE (3); the region's TASK-TCB-CELL holds the descriptor; the body is a `ROUTINE,` that records r13, r14, r15 and pushes one cell through r12, matching the descriptor's sentinels with the cell at the STACK base. Every image ends with `EXPECT-DEPTH,` and `EXPECT-BALANCED,`, at most 10 checks per image.
- Pre-change failing check: the build dies 76 at `s" task-entry" CALL-ROW,` (`ENTRY-LABEL`, `kernel-x64.f:160-171`).
- R4 owns the task suite reds (`lib/task-test.f:1418-1470`'s A64 pin).
