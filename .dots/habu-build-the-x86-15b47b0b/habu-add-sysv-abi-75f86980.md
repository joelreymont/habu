---
title: Add SysV ABI trampolines with float arguments
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.888710+03:00"
closed-at: "2026-09-30T18:46:33.000000+03:00"
close-reason: ThinkPad x86 proof plain and Mac-shim green; hb-x64-kernel-ffi-abi and -snprintf 0, four ABI -armed 83; spark ffi suite ok on eff4
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

Preflight corrections (2026-09-30; override the lines above where they differ):
- Twin: `habu1.f` `BFFI-CALL-ABI-CORE` 1912-1938, `BFFI-COPY-ABI-STACK` 1895-1910, `BFFI-CALL-ABI-BOUNDED-CORE` 2054-2084. Rows `prims.f:707-720`: `ffi-call-abi(-r) ( argbuf fpbuf stackbuf nstack nint sret fn -- n|r )`, `ffi-call-abi(-r)-bounded ( argbuf fpbuf stackbuf regext stkext nstack fn -- n|r )`.
- Descriptor: no per-argument tag; floats sit in fpbuf (`lib/ffi-abi.f` `FFI-FBUF`), integers in argbuf, overflow prepacked in stackbuf. rdi..r9 = argbuf[0..6); xmm0..7 = fpbuf[0..8) always; the stack gets max(nstack,0) stackbuf cells and nothing else (SysV planning, `habu-marshal-ffi-calls-46dfd999`, prepacks integer overflow). argbuf[6..9) pass nowhere but keep the twin's guards: unbounded, one byte at argbuf[i] for i < nint and at argbuf[8] when sret is nonzero; bounded, regext over slots 0..8 (`habu1.f:2065`), then stkext over nstack cells. `al` = 8: no input carries a float count and the psABI takes an upper bound. `-r` pushes xmm0's bits.
- `SYSV-CALL,` (`kernel-x64.f:855`) takes `al` as an argument, 0 from `C-CALL,` (878) and `BUF-CALL,` (2712); `GUARD-ARGS,` (2677) takes its count and extents as arguments.
- Files: `src/habu/kernel-x64.f` (FFI section 2659-2742), `test/x86-64-kernel-ffi.f` (new images and header lines, same `SUITE`), `docs/x86-64.md` "### FFI rows" (1409-1456). Not `boot-x64.f` (`_start` only), not `test/x86-64-peer-routines.f` (`test/x86-64-boot-harness.f:17-20`). Nothing baked.
- Pre-change: a booted case calling `ffi-call-abi` dies 76 at build: `x64kernel: no registered body named ffi-call-abi`.
- Tests, beside `hb-x64-kernel-ffi`, `-negative` 21, `-armed` and `-call-armed` 83; no second negative:
  - `hb-x64-kernel-ffi-abi` 0: a stub folding xmm0..7 and `al`, answering the fold in xmm0 and its complement in rax, through all four rows; K11a's nine-place stub through `ffi-call-abi` and `-bounded` (`SHIFTED-ROW`), nstack 3, junk in argbuf[6..9); nstack -1 on a stub without stack places.
  - `hb-x64-kernel-ffi-snprintf` 0, the dot's dirty-`al` case: `snprintf(buf, 8, "%.1f %.1f")` with 1.5 and 2.5. Control first: push 64 zero cells and drop them, load xmm0/xmm1, call through unguarded `ffi-call-n` (`al` 0): `0.0 0.0`. Then `ffi-call-abi-bounded`: 7 and `1.5 2.5`. (Measured on this host's glibc: after zeroing 512 bytes below rsp, al=0 prints `0.0 0.0` and al=8 prints `1.5 2.5`; without the zeroing al=0 printed `0.0 2.5` from stale slots.)
  - `-abi-armed` 83 (a stack extent names a band cell), `-sret-armed` 83 (`ffi-call-abi`, sret 1, argbuf[8] a band cell).
- Verify: `bin/hb --load test/x86-64-kernel-ffi.f`, then every image natively on the ThinkPad; rerun the syscalls and task suites (the other `SYSV-CALL,` callers).
