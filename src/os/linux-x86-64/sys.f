\ sys.f -- linux-x86-64 OS seam: syscall numbers and kernel argument constants.
\
\ The numbers are the x86_64 table, chosen against the argument shapes the
\ engine's primitives already build (src/habu/habu1.f BACCESS, BUNLINK, BRENAME,
\ BCHMOD, BSTAT64, BLSTAT64 and src/os/<target>/sys.f OS-OPEN-RD): each of those
\ loads AT_FDCWD and a flags register, so this seam names the *at* family -
\ faccessat, unlinkat, renameat, fchmodat, newfstatat, openat, mkdirat - exactly
\ as the aarch64 seam does, and never the legacy open/stat/access numbers that
\ take a different argument list. unlinkat serves both unlink and rmdir (with
\ AT_REMOVEDIR) and newfstatat serves both stat and lstat (with
\ AT_SYMLINK_NOFOLLOW), which is why one number appears twice in each pair.
\
\ THE TRAP EMITTER IS NOT HERE YET. SYS,, the OS-OPEN-RD/OS-OPEN-FLAGS/
\ OS-MMAP-FLAGS translators and the runtime-emit stencils are instruction
\ emitters: they belong to package X64ASM (src/arch/x86-64/asm.f), which this
\ seam is written ahead of. They arrive with that package rather than as
\ hand-encoded bytes, so this file publishes the kernel's numbers and the flag
\ values its callers pass, and nothing that claims to emit an instruction.

$22 constant MAP-ANON-PRIVATE
$32 constant MAP-ANON-PRIVATE-FIXED

60  constant NR-EXIT
0   constant NR-READ
1   constant NR-WRITE
269 constant NR-ACCESS            \ faccessat(dirfd, path, mode, flags)
263 constant NR-UNLINK            \ unlinkat(dirfd, path, 0)
268 constant NR-CHMOD             \ fchmodat(dirfd, path, mode, flags)
16  constant NR-IOCTL
15  constant NR-SIGRETURN         \ rt_sigreturn
257 constant NR-OPEN              \ openat(dirfd, path, flags, mode)
3   constant NR-CLOSE
62  constant NR-KILL
109 constant NR-SETPGID
39  constant NR-GETPID
434 constant NR-PIDFD-OPEN
293 constant NR-PIPE              \ pipe2(fds, flags)
13  constant NR-SIGACTION         \ rt_sigaction
10  constant NR-MPROTECT
11  constant NR-MUNMAP
292 constant NR-DUP2              \ dup3(old, new, flags)
72  constant NR-FCNTL
38  constant NR-SETITIMER
131 constant NR-SIGALTSTACK
96  constant NR-GETTIMEOFDAY
9   constant NR-MMAP
271 constant NR-POLL              \ ppoll(fds, n, timespec, sigmask, sigsetsize)
56  constant NR-SPAWN
56  constant NR-FORK              \ clone(SIGCHLD, 0, 0, 0, 0)
264 constant NR-RENAME            \ renameat(olddirfd, old, newdirfd, new)
258 constant NR-MKDIR             \ mkdirat(dirfd, path, mode)
263 constant NR-RMDIR             \ unlinkat(dirfd, path, AT_REMOVEDIR)
262 constant NR-STAT64            \ newfstatat(dirfd, path, statbuf, flags)
262 constant NR-LSTAT64           \ the same, with AT_SYMLINK_NOFOLLOW
217 constant NR-GETDIRENTRIES64   \ getdents64
267 constant NR-READLINKAT
266 constant NR-SYMLINKAT
61  constant NR-WAIT4
59  constant NR-EXECVE
80  constant NR-CHDIR
231 constant NR-EXIT-GROUP

-100 constant AT-FDCWD
$100 constant AT-SYMLINK-NOFOLLOW
