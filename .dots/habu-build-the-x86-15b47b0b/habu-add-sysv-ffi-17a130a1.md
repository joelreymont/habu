---
title: Add integer SysV FFI trampolines
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.603500+03:00"
blocks:
  - habu-boot-and-exit-367c46f5
  - habu-emit-x86-syscall-a0d501db
---

Problem: the FFI trampolines are ARM64 code. Split from habu-port-the-ffi-676f745d (SysV calls); K11b and K11c build on it.
Acceptance: bodies for `ffi-call`, `ffi-call-n`, `ffi-call-bounded`: rdi rsi rdx rcx r8 r9, stack args, 16-byte alignment before `call`, exact incoming `rsp` restored, `al` zeroed before every call (safe: `rax` is caller-saved and not an argument register), the VM set preserved by the callee-saved rule; the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/boot-x64.f`, `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: routine images calling libc `getpid` and a 9-argument symbol.
Depends: habu-boot-and-exit-367c46f5 (K3), habu-emit-x86-syscall-a0d501db (K6).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
