---
title: Emit x86 syscall table rows
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.560625+03:00"
blocks:
  - habu-size-the-x86-fb635c93
---

Problem: the kernel has no syscall rows; the seam provides `SYS,`, `SYS-PUSH` polarity and the Linux flag translators (`src/os/linux-x86-64/sys.f`).
Acceptance: a table-driven emitter over `SYS,` for the ~40 syscall rows (open/read/write/close/close-rc/ioctl/ map-anon/mmap/munmap/open-rd/access/unlink/rename/chmod/symlink/readlink/realpath (lexical)/mkdir/rmdir/ stat64/lstat64/getdirentries64 (getdents64 translation)/pipe/dup2/fcntl/poll/kill/setpgid/getpid/ epoch-seconds/mono-ns/spawn-* (fork+dup2+execve)/fork/wait-status/execve/kill-errno/proc-watch-open), `setc` polarity (`SYS-PUSH`), flag translation via `OS-OPEN-FLAGS`/`OS-MMAP-FLAGS` (they clobber rax, rcx and r11); the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: routine images calling each row through the kernel; `strace` spot checks.
Depends: habu-boot-and-exit-367c46f5 (K3), habu-share-the-primitive-58c235e5 (K4).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
Owner note (from K2's preflight): the x86-64 `SYS-PUSH` (a `setc` after `SYS,`, `docs/x86-64.md:417`) belongs here, in `X64RT` beside K2's moves, as its first consumer; correct the docs line that assigns it to K2.

K-lane corrections (design 2026-09-30; these override the lines above where they differ):
- Depends add habu-scaffold-the-x86-9af80979 (the kernel scaffold). Files: replace `test/x86-64-peer-routines.f` with this leaf's `test/x86-64-kernel-<name>.f` from the scaffold. Bodies are hand-written through `X64ASM` (no allocator dependency). Every body reads DATA through rbp, so cases run in the booted harness (`test/x86-64-boot-harness.f`). The row table goes in this leaf's `docs/x86-64.md` subsection. Verify: host engine K3's product `264c829e…`; the ThinkPad runs the images natively, each with its negative twin. Base: the scaffold on master.
- Split: this dot is K6a, the table rows; K6b (habu-emit-x86-process-8e1f6f84) takes `spawn-*`, `fork`, `wait-status`, `pipe`, `dup2`, `fcntl`, `poll`, `kill`, `setpgid`, `run-rc` and the registrations of `BPROCWATCHOPEN`, `BKILLERRNO`, `BEXECVE`.
- `SYS-PUSH`: `X64RT:SYS-PUSH ( -- )` in `src/arch/x86-64/rt.f`: `mov rcx, -1 / cmovb rax, rcx / 0 G-PUSH` (ARM64 pushes x0 or -1, `habu1.f:1876-1883`; x86 twin inline at `proc-watch.f:24-26`). It is not a `setc`: correct K3 `docs/x86-64.md:433-435`. The owner note above is superseded.
- `mono-ns`: `clock_gettime(CLOCK_MONOTONIC=1, [rbp+GTOD-SCRATCH])`, NR 228 added to `src/os/linux-x86-64/sys.f:34-72`; the result is `sec*1000000000+nsec` (ARM64 reads `CNTVCT_EL0`, `habu1.f:1498-1508`). `epoch-seconds`: `gettimeofday` into `GTOD-SCRATCH` (`habu1.f:1487-1492`; NR 96 exists).
- `realpath`: libc through the loader slot, not lexical (`test/realpath-test.f:57-62` needs symlink resolution; the x86 ELF already binds `dlsym`, `docs/x86-64.md` "ELF", `src/os/linux-x86-64/layout.f:20,45`). Public in `X64KERNEL` for K11a: `DLSYM, ( -- )`, twin of `habu1.f:2251-2258`, reading `[rbp+RBASE-CELL] - CODE-OFF + [text size] + LINUX-DLSYM-SLOT-OFF` ($B8, `layout.f:20`); `C-CALL, ( -- )` (`push rsp; push [rsp]; and rsp, -16; call rax; mov rsp, [rsp+8]`; VM registers survive by the callee-saved rule). The body twins `REALPATH` (`habu1.f:2263-2315`: the -1/-2 contract, free after copy).
- Rows: `open read write close close-rc ioctl map-anon mmap munmap open-rd access unlink rename chmod symlink readlink mkdir rmdir stat64 lstat64 getdirentries64 epoch-seconds mono-ns getpid realpath`. `read ioctl mmap munmap stat64 lstat64 readlink getdirentries64 realpath` call `X64KERNEL:PROT-SPAN-CALL,` (twins `habu1.f:412-427`). Flag translation through `OS-OPEN-FLAGS`/`OS-MMAP-FLAGS`, which clobber rax, rcx and r11: stage them first.
- Files: `src/habu/kernel-x64.f`, `src/arch/x86-64/rt.f`, `src/os/linux-x86-64/sys.f`, `test/x86-64-kernel-syscalls.f`, `docs/x86-64.md`.
- ARM64 twins: `habu1.f:1885-1937, 2187-2404, 2709-2716, 1286-1394`.

Preflight corrections (Fable, 2026-09-30; these override the lines above where they differ):
- `stat64`/`lstat64`: the x86-64 fix reads `st_mode` at 24 (aarch64: 16; x86-64 `struct stat` has three u64 `st_dev st_ino st_nlink` before it, `/usr/include/asm/stat.h`) and the other five fields at the offsets `LINUX-STAT-FIX` (`habu1.f:2257-2263`) reads; it writes the layout `lib/fs.f:47-52` reads (mode 4, mtime 48/56, ctime 64/72, size 96). A case checks that mode at 4 of a stat of `/` has `S_IFDIR`.
- `C-CALL,` is `mov rcx, rsp; push rcx; push rcx; and rsp, -16; call rax; mov rsp, [rsp+8]` (`X64ASM` has no memory `push`, `asm.f:719-727`; both copies are the saved rsp, so `[rsp+8]` holds it whether the alignment removed 0 or 8 bytes).
- Tests follow master's convention (`505b7f52`): single positive images, no per-image negative twin.
- Verify: install `strace` first (`pkexec pacman -S --needed --noconfirm strace`); the ThinkPad has none. Host engine: master's product `5f4d3321…`.
- `docs/x86-64.md:445-447` still calls `SYS-PUSH` "a `setc`"; this leaf corrects it.
- ARM64 twin lines on master: `SYS-PUSH` 1775, `BOPEN..BGETPID` 1784-1837, `BEPOCHSECONDS` 1386, `BMONONS` 1397, `BOPENRD..BREADLINK` 2086-2148, `DLSYM` 2150, `REALPATH` 2172, `BMKDIR..BGETDIRENTRIES64` 2237-2306, `BCLOSE`/`-RC` 2608-2615.
