---
title: Add integer SysV FFI trampolines
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.603500+03:00"
---

Problem: the FFI trampolines are ARM64 code. Split from habu-port-the-ffi-676f745d (SysV calls); K11b and K11c build on it.
Acceptance: bodies for `ffi-call`, `ffi-call-n`, `ffi-call-bounded`: rdi rsi rdx rcx r8 r9, stack args, 16-byte alignment before `call`, exact incoming `rsp` restored, `al` zeroed before every call (safe: `rax` is caller-saved and not an argument register), the VM set preserved by the callee-saved rule; the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/boot-x64.f`, `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: routine images calling libc `getpid` and a 9-argument symbol.
Depends: habu-boot-and-exit-367c46f5 (K3), habu-emit-x86-syscall-a0d501db (K6).
Route: direct.
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-add-sysv-ffi-17a130a1.

K-lane correction (design 2026-09-30): Acceptance add "reuses K6a's `X64KERNEL:DLSYM,` and `C-CALL,`"; no second loader-slot reader.

Preflight corrections (2026-09-30; override the lines above, including the K-lane corrections):
- Reuses `X64KERNEL:DLSYM,` (no second loader-slot reader). `C-CALL,` is not reusable: it calls `rax` (`kernel-x64.f:835`) where `al` must be 0, and restores from `[rsp+8]` (836), which holds the entry rsp only with nothing pushed after the alignment. K11a owns one new public `X64KERNEL` emitter for a SysV call with n stack cells (`C-CALL,` may become its zero-cell case): the entry rsp is kept across the call on the data stack (`G-PUSH`/`G-POP`, r12 is callee-saved; the twin of x20 carrying the frame sp, `habu1.f:1963-1965,1990-1991`; every x86 callee-saved register is a VM register, `docs/x86-64.md:632-639`); reserve roundup16(n*8), `and rsp,-16`, copy, `xor eax,eax`, `call r11`. K11b extends it with the xmm loads and `al` = count.
- Cells: rdi..r9 = argbuf[0..5]; the stack takes argbuf[6..max(nargs,8)) so the 8-cell contract (`habu1.f:1840-1843`) holds on x86 as ARM64's unconditional 8-register load does.
- Guards: `PROT-SPAN-CALL,` clobbers every scratch register (`kernel-x64.f:162-164`), so the per-argument loop of `BFFI-GUARD-ARGS`/`-BOUNDS` (`habu1.f:1862-1873,1948-1957`, which keep x5/x14/x15/x17 live) cannot hold its state in registers: guard before the pops through `PEEK,` (`kernel-x64.f:697-704`) with the index on the machine stack.
- Files: `src/habu/kernel-x64.f` (`FFI,` appended to `KERNEL,`), new `test/x86-64-kernel-ffi.f` (package `X64K-FFI`, `require test/x86-64-boot-harness.f`), `test/gate-stdlib-cases.f` (`SUITE x86-64-kernel-ffi` beside the other kernel suites), `docs/x86-64.md` ("### FFI rows" after "Engine-state rows"). Not `boot-x64.f` (`_start` only) nor `test/x86-64-peer-routines.f` (its harness cannot run a kernel body, `test/x86-64-boot-harness.f:17-20`).
- Verify (ThinkPad, native): `hb-x64-kernel-ffi` exits 0, `-negative` 21, `-armed` 83. Cases: `ffi-call` on `getpid` from `DLSYM,` (name through the syscalls suite's `PATH,` pattern, `test/x86-64-kernel-syscalls.f:136-142`) equals the `getpid` row; an image-carried SysV stub via `X64HARNESS:ROUTINE,`/`PUSH-XT,` (`test/x86-64-boot-harness.f:211-218`, `test/x86-64-kernel-control.f:73`) that sums rdi rsi rdx rcx r8 r9 [rsp+8..24] and folds `rsp & 15` and `al` into its answer, called by `ffi-call` (8 cells) and `ffi-call-n`/`ffi-call-bounded` (9), each also through `SHIFTED-ROW` (`test/x86-64-kernel-syscalls.f:149-150`) so both rsp parities cross the alignment; `ffi-call-bounded` with an extent over a band in the armed image; every image `EXPECT-BALANCED,` and `EXPECT-DEPTH,`. Pre-change failure: `CALL-ROW,` dies 76 for an unregistered name (`kernel-x64.f:143-150`). No libc symbol takes 9 integer arguments; the stub stands in, as `lib/ffi-test.f:266-276` does on ARM64.
