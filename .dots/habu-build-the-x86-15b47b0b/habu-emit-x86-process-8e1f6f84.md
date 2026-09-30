---
title: Emit x86 process rows
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T09:22:47.220353+03:00"
---

Problem: the process rows need one `LINUX-SPAWN` twin and the existing x86 emitters registered; K6a covers only the table rows. Split from K6 by the K-lane design (2026-09-30).
Acceptance: `spawn-io spawn-argv-io spawn-argv-env-io spawn-argv-env-cwd-io` over one `LINUX-SPAWN` twin (`habu1.f:672-830`); `fork wait-status pipe dup2 fcntl poll kill setpgid`; `run-rc` (`BRUNRC` `habu1.f:824`, spawn+wait; UNROWED at `prims.f:661`); registrations of the existing emitters `BPROCWATCHOPEN`, `BKILLERRNO`, `BEXECVE` (`src/os/linux-x86-64/proc-watch.f:20-26`, `proc-control.f:15-27`; `kernel-x64.f` requires both files). `fork` is raw `clone(SIGCHLD)`, as `NR-FORK` states (`habu1.f:2320-2325`). Every row runs through the booted harness (`test/x86-64-kernel-syscalls.f`); its table joins `docs/x86-64.md` `### Syscall rows`.
Files: `src/habu/kernel-x64.f`, `test/x86-64-kernel-syscalls.f`, `docs/x86-64.md`.
Verify: host K3's product engine: `--load test/x86-64-kernel-syscalls.f`; ThinkPad: the images natively, each with its negative twin; `strace -f` spot checks of one spawn and one `run-rc`.
Depends: habu-emit-x86-syscall-a0d501db (K6a), habu-scaffold-the-x86-9af80979 (scaffold). Hand-written through `X64ASM`; no allocator dependency.
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-emit-x86-process-8e1f6f84.

Note (K6a landing, 2026-09-30): `X64RT:SYS-PUSH` now exists (`src/arch/x86-64/rt.f`). `src/os/linux-x86-64/proc-watch.f` still inlines its own copy (`mov rcx,-1 / cmovb rax,rcx / push`) under a stale "Loaded before habu1.f, so … inlined" comment; this leaf, which owns `BPROCWATCHOPEN`, switches it to `X64RT:SYS-PUSH` (same bytes, pinned by `test/x86-64-emit.f`).

Preflight corrections (2026-09-30; override the lines above where they differ):
1. `fcntl`: cmd 73 (`F-SETNOSIGPIPE`, `lib/process.f:62`; `FD-NOSIGPIPE!` at `lib/process.f:220-221` throws unless rc = 0, used at `:431`) is not a syscall: the Linux arm runs `LINUX-IGNORE-SIGPIPE` = `rt_sigaction(SIGPIPE=13, {SIG_IGN,0,0,0}, 0, 8)` (`habu1.f:836-849`, dispatch `:856-859`). The x86-64 kernel `struct sigaction` has the same four 8-byte cells (handler, flags, restorer, mask); `NR-SIGACTION` 13 is at `sys.f:49`. K10a owns handlers with `SA_SIGINFO`, not this ignore.
2. `poll`: guards `fds` for `nfds*8` bytes, with `nfds>>61 <> 0` becoming an all-address span (`habu1.f:901-905`), the rule of the section these rows join (`docs/x86-64.md:761-763`); converts ms to a timespec, negative = NULL (`:908-919`); publishes rax raw as `-errno`, not `SYS-PUSH` (`:921`, `prims.f:466`). One `-armed` image exits 83, as `hb-x64-kernel-sys-mmap-armed` does.
3. `kernel-x64.f` does not require `proc-watch.f`/`proc-control.f` (`kernel-x64.f:24-35`; only `test/x86-64-emit.f:39-41` and `tools/native-emit.f:34-35` load them). Add both `require`s after `rt.f` (`:34`), since they open `using X64RT`.
4. Refs: `BRUNRC` is `habu1.f:723`; `UNROWED: run-rc` is `prims.f:716`; `NR-FORK` is `sys.f:61` and the ARM64 `fork` is `LIBC-OS:FORK` `habu1.f:2214-2217` (`clone(17,0,0,0,0)`). ARM64 twins: `LINUX-SPAWN` and helpers `habu1.f:571-721`, `BRUNRC :723-779`, `BPIPE :781-805`, `BDUP2 :807-830`, `BFCNTL :851-878`, `BPOLL :899-934`, `BKILL/BSETPGID/BWAITSTATUS :936-972`, `BSPAWN* :1185-1291`. Every NR needed is in `sys.f:34-73`.
5. Tests: single positive images; the suite's one `-negative` image (exit 21) proves the harness; no per-image twins. A `fork` case's child leaves through the `die` row with a distinct rc before the harness's trailing checks, so `wait-status` proves the child ran. Pre-change failure: `s" fork" X64HARNESS:CALL-ROW,` dies 76 `x64kernel: no registered body named fork`; same for `run-rc`.
