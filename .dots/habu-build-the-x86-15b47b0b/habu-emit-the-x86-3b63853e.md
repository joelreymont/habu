---
title: Emit the x86 signal stub and publish it
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T12:11:53.130107+03:00"
---

Problem: the ARM64 engine bakes a signal stub (`EMIT-SIGNAL-HANDLER`, `src/habu/crash.f:300-330`, label LSIGH) and publishes it on every boot (`habu2.f:7538-7540`: `SIGNAL-ABI:STUB-CELL` and `SIGNAL-ABI:FD-PTR-CELL`); `lib/signal.f:143-169` refuses `E-SIGNAL-ABI` when the stub cell is zero. The x86 kernel has neither, so R4's `signal-stub` suite cannot go green.
Acceptance: an x86 stub with the same contract (`crash.f:300-318`): it writes the signal number as one four-byte write to the fd held at the absolute address `DATA-VA + SIGNAL-ABI:FD-CELL` (never through the thread's DATA register), ignores the write's result, absorbs the signal when that fd word is zero, touches no Forth state, and returns to the kernel's restorer; the x86 boot stores the stub's address in `SIGNAL-ABI:STUB-CELL` and `DATA-VA + FD-CELL` in `SIGNAL-ABI:FD-PTR-CELL` on every boot. `lib/signal.f` installs through libc `sigaction` (glibc supplies `SA_RESTORER`), so no restorer is added here.
Files: `src/habu/boot-x64.f` or `src/habu/kernel-x64.f` beside K10a's handler emitters, a case in the K10a signal test, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: an image that installs the stub for SIGUSR1 with a pipe's write end in the fd word, raises the signal, and reads the four-byte number back; a second with the fd word zero survives the signal and writes nothing.
Depends: habu-port-signals-crash-2c7768ca (K10a).
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: krait.

Preflight corrections (2026-09-30; override the lines above where they differ):
- The ARM64 publication is at `habu2.f:7553-7555`; the stub's contract and body at `crash.f:286-330` (`EMIT-SIGNAL-HANDLER` at 319).
- Files: `src/habu/boot-x64.f` (package `X64BOOT`) only among sources; `kernel-x64.f` is untouched (`test/x86-64-skel-image.f:42` runs `START,` without `KERNEL,`, and the linker refuses an unbound label, `icode.f:122`). `START,` emits the stub in its out-of-line block after `booted JMP,`, beside the `FAIL,` blocks (158-162). After `DATA-INIT,` it stores the stub's address in `SIGNAL-ABI:STUB-CELL` and `DATA-VA + FD-CELL` in `FD-PTR-CELL`. Add a public `SIGACTION-AT, ( n n r64 label -- )` that takes the handler's address in a register; `SIGACTION,`'s stack effect stays as is (K10b and K10c use it). New `test/x86-64-boot-signal.f` (package `X64K-SIGNAL`) on `test/x86-64-boot-harness.f` (the peer harness never runs `START,`, `test/x86-64-peer-harness.f:150-158`), with its `SUITE` row beside `x86-64-kernel-ffi` in `test/gate-stdlib-cases.f`. Add two rows to the DATA-cell table in `docs/x86-64.md`.
- Verify: ThinkPad, booted images. (a) Read the handler from `[rbp+STUB-CELL]`, check `FD-PTR-CELL` = `DATA-VA + FD-CELL`, install it for SIGUSR1 through `SIGACTION-AT,`, store a non-blocking pipe's write end through `[rbp+FD-PTR-CELL]`, raise the signal, read back the four-byte 10. (b) With the fd word zero the image survives the signal and the pipe read finds nothing. (c) One negative image exits 21. `hb-x64-skel` still links and exits 3.
- Pre-change failing check: in the booted harness `STUB-CELL` reads 0.
- The x86 `FD-CELL` is clear by construction (`START,` maps DATA fresh, `boot-x64.f:117-119`); ARM64 clears it at boot (`habu2.f:7544`). `habu-write-snapshots-and-25a9e6b7` owns the clear once restore carries DATA bytes.
