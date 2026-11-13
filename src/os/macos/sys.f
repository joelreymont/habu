\ sys.f — the darwin-arm64 OS seam: syscall numbers + the SVC emitter.
\ Engine emitters say
\ `NR-WRITE SYS,`; porting to another OS/arch swaps this file.

require src/arch/arm64/mnem.f
using A64ICODE
package SYSCALL
public
$1002 constant MAP-ANON-PRIVATE
$1012 constant MAP-ANON-PRIVATE-FIXED

\ MAP-DATA-RESERVE maps the DATA window: Darwin's MAP_ANON|MAP_PRIVATE|MAP_FIXED
\ is already lazy, charged only for touched pages.
$1012 constant MAP-DATA-RESERVE
$01  constant MAP-SHARED
$11  constant MAP-SHARED-FIXED

private
$1   constant NR-EXIT
public
$2   constant NR-FORK
$3   constant NR-READ
$4   constant NR-WRITE
$21  constant NR-ACCESS
$A   constant NR-UNLINK
$F   constant NR-CHMOD
$36  constant NR-IOCTL
$B8  constant NR-SIGRETURN
$5   constant NR-OPEN
$6   constant NR-CLOSE
$25  constant NR-KILL
$52  constant NR-SETPGID
$14  constant NR-GETPID
private
$148 constant NR-PTHREAD-KILL  \ __pthread_kill(thread port, sig) — BSD syscall 328
$149 constant NR-PTHREAD-SIGMASK  \ __pthread_sigmask(how, set, oset) — BSD syscall 329
26   constant MACH-THREAD-SELF  \ thread_self_trap is Mach trap -27, so x16 = ~26
public
$16A constant NR-KQUEUE
$171 constant NR-KEVENT64
$FFFB constant EVFILT-PROC-U16     \ EVFILT_PROC as an unsigned 16-bit filter (-5)
$15  constant EV-PROC-FLAGS        \ EV_ADD | EV_ONESHOT
$80000000 constant NOTE-EXIT       \ fflags: signal on process exit
$2A  constant NR-PIPE
$2E  constant NR-SIGACTION
$4A  constant NR-MPROTECT
$49  constant NR-MUNMAP
$5A  constant NR-DUP2
$5C  constant NR-FCNTL
$53  constant NR-SETITIMER
$C2  constant NR-GETRLIMIT
3    constant RLIMIT-STACK
$7FFFFFFFFFFFFFFF constant RLIM-INFINITY
$35  constant NR-SIGALTSTACK
$74  constant NR-GETTIMEOFDAY
$C5  constant NR-MMAP
$C9  constant NR-FTRUNCATE
$E6  constant NR-POLL
$F4  constant NR-SPAWN     \ posix_spawn(&pid, path, 0, 0, argv, envp)
$80  constant NR-RENAME
$88  constant NR-MKDIR
$89  constant NR-RMDIR
$152 constant NR-STAT64
$154 constant NR-LSTAT64
$158 constant NR-GETDIRENTRIES64
$1D9 constant NR-READLINKAT
$1DA constant NR-SYMLINKAT
$7   constant NR-WAIT4     \ wait4(pid, &status, 0, 0)
$3B  constant NR-EXECVE    \ execve(path, argv, envp) — BSD syscall 59
$C   constant NR-CHDIR     \ chdir(path) — BSD syscall 12; only Linux emitters use it today
$1   constant NR-EXIT-GROUP

\ A Linux-only call has no Darwin number: its constant is -1 and SYS, refuses
\ every negative number, so a macOS arm that reaches one dies at engine build
\ instead of reaching the kernel. 0 cannot mark an absent call: Darwin syscall
\ 0 is the indirect `syscall` entry, which dispatches on x0.
-1   constant NR-CLOSE-RANGE
-1   constant NR-MEMFD-CREATE

private
-2 constant AT-FDCWD
public
$20 constant AT-SYMLINK-NOFOLLOW   \ sys/fcntl.h AT_SYMLINK_NOFOLLOW; only Linux emitters use it today

: SYS, ( n -- )
   dup 0 < if s" hb: sys: a Linux-only syscall number" 76 die then
   16 swap MOVZ,  $80 SVC, ;

: OS-OPEN-RD ( n -- )
   0 swap 0 ADDI,  1 0 MOVZ,  2 0 MOVZ,  NR-OPEN SYS, ;

\ Clobbers x0, x1 and x16.
: OS-RAISE-SELF ( n -- )
   {: sigreg :}
   16 MACH-THREAD-SELF MOVN,  $80 SVC,
   1 sigreg 0 ADDI,  NR-PTHREAD-KILL SYS, ;

\ __pthread_sigmask(SIG_BLOCK, set, NULL) on this thread, the 4-byte set in a
\ frame on sp that it gives back. Clobbers x0, x1, x2, x10 and x16.
: OS-BLOCK-SELF ( n -- )
   {: sigreg :}
   10 1 MOVZ,  1 sigreg 1 SUBI,  10 10 1 LSLV,     \ bit sig - 1
   SP SP $10 SUBI,  10 SP 0 STRW,
   0 1 MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,  NR-PTHREAD-SIGMASK SYS,
   SP SP $10 ADDI, ;

: OS-OPEN-FLAGS ( -- ) ;

: OS-MMAP-FLAGS ( -- ) ;
;package
;using
