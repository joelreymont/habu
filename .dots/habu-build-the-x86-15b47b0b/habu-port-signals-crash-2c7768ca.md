---
title: Install x86 signal handlers and decode frames
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.594682+03:00"
---

Problem: `src/habu/crash.f:30-36` and `src/habu/prof.f:47-50` model aarch64 signal frames only; the x86 kernel installs no handler. Split from habu-port-the-ffi-676f745d (signal frame decoding); K10b and K10c build on it.
Acceptance: x86 `sigaction` install with `SA_SIGINFO|SA_RESTORER`, an `rt_sigreturn` restorer, `ucontext` offsets for `RIP`/`RSP`/gregs pinned by a test that raises a real signal on the ThinkPad (glibc layout suggests `uc_mcontext` at 0x28, `RIP` at 0xA8, `RSP` at 0xA0; the test, not the constants, is the proof); the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/boot-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: a routine image that raises a real signal and checks the decoded registers.
Depends: habu-boot-and-exit-367c46f5 (K3), habu-emit-x86-control-9a35e3b3 (K7).
Route: direct.
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-port-signals-crash-2c7768ca.
