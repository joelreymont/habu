---
title: Add SysV ABI trampolines with float arguments
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.888710+03:00"
blocks:
  - habu-emit-and-exec-a8536cf2
---

Problem: the ABI-planned trampolines pass float arguments and results, which on SysV use XMM registers and the vector-register count in `al`.
Acceptance: bodies for `ffi-call-abi(-r)(-bounded)`: xmm0-7, `al` = vector-register count, float return in xmm0; a case dirties `al` before `snprintf`; the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/boot-x64.f`, `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: routine images with float arguments and results, and `snprintf` after a dirty `al`.
Depends: habu-add-sysv-ffi-17a130a1 (K11a), habu-emit-and-exec-a8536cf2 (C7b).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.

K11a landing note (2026-09-30): K11a's `X64KERNEL:SYSV-CALL,` takes the function in r11, the register arguments loaded, and r10 = the count of stack cells at rax; it assumes r10 >= 0. K11b extends it with the xmm loads and `al` = the vector-register count, and clamps any count derived from user data as ARM64 `BFFI-COPY-ABI-STACK` does: a negative count corrupts the machine stack.
