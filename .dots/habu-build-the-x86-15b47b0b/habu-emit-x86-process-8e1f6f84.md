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
Claim: unassigned.

Note (K6a landing, 2026-09-30): `X64RT:SYS-PUSH` now exists (`src/arch/x86-64/rt.f`). `src/os/linux-x86-64/proc-watch.f` still inlines its own copy (`mov rcx,-1 / cmovb rax,rcx / push`) under a stale "Loaded before habu1.f, so … inlined" comment; this leaf, which owns `BPROCWATCHOPEN`, switches it to `X64RT:SYS-PUSH` (same bytes, pinned by `test/x86-64-emit.f`).
