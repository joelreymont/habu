---
title: Emit the x86 signal stub and publish it
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T12:11:53.130107+03:00"
blocks:
  - habu-port-signals-crash-2c7768ca
---

Problem: the ARM64 engine bakes a signal stub (`EMIT-SIGNAL-HANDLER`, `src/habu/crash.f:300-330`, label LSIGH) and publishes it on every boot (`habu2.f:7538-7540`: `SIGNAL-ABI:STUB-CELL` and `SIGNAL-ABI:FD-PTR-CELL`); `lib/signal.f:143-169` refuses `E-SIGNAL-ABI` when the stub cell is zero. The x86 kernel has neither, so R4's `signal-stub` suite cannot go green.
Acceptance: an x86 stub with the same contract (`crash.f:300-318`): it writes the signal number as one four-byte write to the fd held at the absolute address `DATA-VA + SIGNAL-ABI:FD-CELL` (never through the thread's DATA register), ignores the write's result, absorbs the signal when that fd word is zero, touches no Forth state, and returns to the kernel's restorer; the x86 boot stores the stub's address in `SIGNAL-ABI:STUB-CELL` and `DATA-VA + FD-CELL` in `SIGNAL-ABI:FD-PTR-CELL` on every boot. `lib/signal.f` installs through libc `sigaction` (glibc supplies `SA_RESTORER`), so no restorer is added here.
Files: `src/habu/boot-x64.f` or `src/habu/kernel-x64.f` beside K10a's handler emitters, a case in the K10a signal test, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: an image that installs the stub for SIGUSR1 with a pipe's write end in the fd word, raises the signal, and reads the four-byte number back; a second with the fd word zero survives the signal and writes nothing.
Depends: habu-port-signals-crash-2c7768ca (K10a).
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: unassigned.
