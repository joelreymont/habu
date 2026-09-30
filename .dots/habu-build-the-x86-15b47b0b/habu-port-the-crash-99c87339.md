---
title: Port the crash handler and guard classification
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.871049+03:00"
---

Problem: the crash handler's register dump and guard-page classification exist for aarch64 only (`src/habu/crash.f`, whose constants at 30-36 are aarch64).
Acceptance: an x86 register dump on fd 2, exit 134, and a guard-page fault distinguished from ordinary faults per the existing contract (`src/habu/crash.f` semantics); the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/boot-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: routine images faulting on a guard page and on an ordinary address; the dump, the classification and status 134 checked.
Depends: habu-port-signals-crash-2c7768ca (K10a).
Route: direct.
Ownership: krait (Intel lane).
Claim: krait.

Preflight corrections (2026-09-30; override the lines above where they differ):
- Owner: X64BOOT (`src/habu/boot-x64.f`), new words private. `START,` moves below the signal words it now calls; `TEXT-BASE,` becomes `( r64 -- )`. Add `require src/core/engine-error.f` (as `kernel-x64.f:28`).
- Install: in `START,` after `FRAME-STACKS,` (`aot-lib.f:579`), `SIGACTION,` with `SA-SIGINFO` for 4 5 7 8 11 (`crash.f:280`), unchecked, as `crash.f`. Handler, printer, texts and a `RESTORER,` (required: `SIGACTION,` always sets SA_RESTORER) go behind `booted JMP,`.
- Classification as `crash.f:156-227` (si_addr at siginfo+$10, DATA `RBP UC-GREG`, zero or unaligned bases skipped; a hit writes its `hb: stack bounds exceeded (...)` line in one write and exits 102), for signals 11 and 7, only when the saved rip lies in [text base, text base - CODE-OFF + REGION-OFF + REGION). Only there is rbp DATA; foreign SysV code keeps a frame pointer in it (host libc is built `-fno-omit-frame-pointer`).
- Otherwise dump, exit 134. Header `habu-crash regs [sig rax rcx rdx rbx rsp rbp rsi rdi r8..r15 rip] code [rip-8 rip rip+8], hex one-per-line:` (the `habu-crash regs` prefix is what `tools/build-fixpoint-sandbox-test.f:40` matches), then 22 lines of 16 lowercase hex digits + LF, one write each: sig, the registers by number (`UC-GREG`), rip (`UC-RIP`) as values; then the 24 bytes from rip-8 as three lines in memory order, each 0 unless its 8 bytes lie in the code region (text base - CODE-OFF + REGION-OFF, REGION bytes; rip-relative, never the saved r13). The handler never reads an address it cannot prove mapped (`crash.f:162-166`).
- Test: `test/x86-64-kernel-crash.f`, package X64K-CRASH, SUITE `x86-64-kernel-crash` after `x86-64-kernel-ffi`. Each image forks; the child dup2s a pipe onto fd 2 and faults; the parent checks the wait status and every captured byte but rsp's digits, then exits 0 (fork pattern: `test/x86-64-kernel-syscalls.f:299-309`). Images: `hb-x64-kernel-crash` (registers but rsp distinct, rbp non-canonical, jump to $1000: all three code lines 0), its `-negative` (21), `-region` (rbp DATA, 24 known bytes around CP, jump to CP), `-ill` (ud2, rbp non-canonical), `-data`/`-return`/`-loop` (data overflow; return base + RETURN-BYTES; below the loop base; each 102 and its line). Pre-change: every positive crash image exits 21.
- Files: also the skel-image header, `test/gate-stdlib-cases.f:1345-1346`, `test/x86-64-kernel-atomics.f:19` (now 134), `docs/x86-64.md:542, 709-718, 743-755, 1100-1102`, and a "Crash handler" subsection after "Signal install and frames". Drop `test/x86-64-peer-routines.f`.
- Verify (ThinkPad): rebuild every booted suite, run natively; `hb-x64-skel-negative` goes 139 to 102 (pre-change failure); other statuses hold; `bin/hb --load test/gate-entry-guard-test.f`. No `bin/hb` rebuild: only x86 tests load `boot-x64.f`.
- K10d also edits `START,` (textual conflict only); K10b does not touch the signal stub, STUB-CELL or FD-PTR-CELL.
