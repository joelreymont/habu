---
title: Emit x86 syscall bodies
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.560625+03:00"
blocks:
  - habu-boot-and-exit-367c46f5
  - habu-share-the-primitive-58c235e5
---

Problem: the kernel has no syscall rows; the seam provides `SYS,`, `SYS-PUSH` polarity and the Linux flag translators (`src/os/linux-x86-64/sys.f`).
Acceptance: a table-driven emitter over `SYS,` for the ~40 syscall rows (open/read/write/close/close-rc/ioctl/ map-anon/mmap/munmap/open-rd/access/unlink/rename/chmod/symlink/readlink/realpath (lexical)/mkdir/rmdir/ stat64/lstat64/getdirentries64 (getdents64 translation)/pipe/dup2/fcntl/poll/kill/setpgid/getpid/ epoch-seconds/mono-ns/spawn-* (fork+dup2+execve)/fork/wait-status/execve/kill-errno/proc-watch-open), `setc` polarity (`SYS-PUSH`), flag translation via `OS-OPEN-FLAGS`/`OS-MMAP-FLAGS` (they clobber rax, rcx and r11); the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: routine images calling each row through the kernel; `strace` spot checks.
Depends: habu-boot-and-exit-367c46f5 (K3), habu-share-the-primitive-58c235e5 (K4).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
