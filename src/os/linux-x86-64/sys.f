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
\ THE INSTRUCTION EMITTERS BELOW ASSUME TWO THINGS THIS SEAM DOES NOT OWN.
\
\ ASM-SINK ( -- ptr u8 ) is the byte buffer the code stream currently being
\ emitted appends into. An x86_64 instruction has no value representation, so
\ every X64ASM encoder takes that sink as its last operand instead of returning a
\ word the way the ARM64 encoders do. Supplying ASM-SINK is the x86-64 code
\ layer's obligation (habu-cross-build-the-d25a959d); this file names it
\ unrequired exactly as src/os/linux/sys.f names MOVZ, and SVC, from
\ src/arch/arm64/mnem.f, and test/x86-64-emit.f defines it over a test-owned
\ buffer so every byte emitted here is pinned on an aarch64 host.
\
\ THE SYSCALL ARGUMENT REGISTERS are rdi, rsi, rdx, r10, r8 and r9, and the
\ number and the result are rax; the engine's x0..x5 order maps onto them in that
\ sequence. G-POP and G-PUSH therefore name x86_64 register numbers here.

using X64ASM

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

\ ---- the trap ----------------------------------------------------------------
\ THE CARRY POLARITY IS CHOSEN, NOT TRANSLITERATED. Linux returns -errno in the
\ result register on both architectures, and the engine reconciles that into the
\ carry flag so SYS-PUSH and the process primitives can read one bit. The aarch64
\ seam spells the reconciliation `cmp x0, #-4095`, and ARM sets C on NO borrow,
\ so there C means "x0 is at or above -4095 unsigned", which is the error range.
\ x86 sets CF on BORROW, so `cmp rax, -4095` would set CF exactly when the call
\ SUCCEEDED and every C-CS consumer would read the flag upside down. Reversing
\ the operands restores the aarch64 predicate: with rcx = -4096,
\ `cmp rcx, rax` sets CF when rax is above -4096 unsigned, i.e. when rax is in
\ [-4095, -1], which is the error range again. CF set means error.
\
\ rcx is free for the comparison constant: `syscall` architecturally clobbers rcx
\ (it holds the return rip) and r11 (the saved rflags), so nothing live can be
\ sitting in either one after the trap.
: SYS, ( n -- )
   0 >R32 swap >IMM32 ASM-SINK ENC-MOV32-RI32   \ mov eax, #NR (zero-extends into rax)
   ASM-SINK ENC-SYSCALL                          \ syscall
   RCX -4096 >IMM32 ASM-SINK ENC-MOV-RI32        \ mov rcx, -4096
   RCX RAX ASM-SINK ENC-CMP-RR ;                 \ cmp rcx, rax -> CF = error

\ Zero a register with the two-byte 32-bit xor, which clears all 64 bits because
\ a 32-bit result zero-extends.
: ZERO-REG, ( r64 -- ) {: r:r64 :}
   r R64>N {: n:n :}
   n >R32 n >R32 ASM-SINK ENC-XOR32-RR ;

\ ---- runtime-emit syscall stencils -------------------------------------------
\ The MATCH bad-tag die is EMITTED into the user word, so its write+exit syscall
\ steps are baked here at engine-build time rather than assembled by SYS, at
\ build time. The die never inspects the write return, so the -errno
\ reconciliation SYS, adds is not needed.
\
\ A STENCIL IS A BYTE STRING because an x86_64 instruction is one to fifteen
\ bytes: `mov eax, imm32` is five and `syscall` is two, where every ARM64 stencil
\ is a single four-byte word. C-EMIT-STENCIL (src/habu/jit.f) compiles the
\ runtime emission from the span. The bytes are written out here, as the aarch64
\ seam writes its instruction words out, and test/x86-64-emit.f pins each one
\ against both X64ASM's encoders and llvm-mc.
5 constant SYS-NR-STENCIL-BYTES          \ mov eax, imm32
2 constant SYS-SVC-STENCIL-BYTES         \ syscall

create SYS-WRITE-STENCIL SYS-NR-STENCIL-BYTES allot
create SYS-EXIT-STENCIL SYS-NR-STENCIL-BYTES allot
create SYS-SVC-STENCIL SYS-SVC-STENCIL-BYTES allot

: SYS-NR-STENCIL! ( n ptr u8 -- )        \ B8 id, the immediate little-endian
   $B8 over c!
   over $FF and over 1 + c!
   over 8 rshift $FF and over 2 + c!
   over 16 rshift $FF and over 3 + c!
   swap 24 rshift $FF and swap 4 + c! ;

NR-WRITE SYS-WRITE-STENCIL SYS-NR-STENCIL!        \ llvm-mc: movl $1, %eax
NR-EXIT-GROUP SYS-EXIT-STENCIL SYS-NR-STENCIL!    \ llvm-mc: movl $231, %eax
$0F SYS-SVC-STENCIL c!                             \ llvm-mc: syscall
$05 SYS-SVC-STENCIL 1 + c!

: SYS-EMIT-WRITE ( -- ptr u8 n )  SYS-WRITE-STENCIL SYS-NR-STENCIL-BYTES ;
: SYS-EMIT-EXIT ( -- ptr u8 n )   SYS-EXIT-STENCIL SYS-NR-STENCIL-BYTES ;
: SYS-EMIT-SVC ( -- ptr u8 n )    SYS-SVC-STENCIL SYS-SVC-STENCIL-BYTES ;

\ ---- the kernel-argument translators -----------------------------------------
\ openat(AT_FDCWD, path, 0, 0). The path register is set before AT_FDCWD so a
\ caller may name rdi itself.
: OS-OPEN-RD ( n -- ) {: pathreg:n :}
   RSI pathreg >R64 ASM-SINK ENC-MOV-RR         \ mov rsi, path
   RDI AT-FDCWD >IMM32 ASM-SINK ENC-MOV-RI32    \ mov rdi, AT_FDCWD
   RDX ZERO-REG,                                 \ flags 0
   R10 ZERO-REG,                                 \ mode 0
   NR-OPEN SYS, ;

\ One flag bit of the caller's BSD-shaped word becomes one flag bit of the Linux
\ word, branchlessly: the source bit selects between zero and the Linux bit, and
\ the accumulator takes it. Both masks are single bits, so the selection is the
\ whole translation. rax accumulates and rcx and r11 hold the candidate - none of
\ the three is a VM register or a syscall argument, and all three are dead across
\ the trap that follows.
: OS-FLAG-BIT, ( r64 n n -- ) {: src:r64 srcbit:n dstbit:n :}
   RCX ZERO-REG,
   R11 dstbit >IMM32 ASM-SINK ENC-MOV-RI32      \ mov r11, the Linux bit
   src srcbit >IMM32 ASM-SINK ENC-TEST-RI32     \ test src, the BSD bit
   C-NE RCX R11 ASM-SINK ENC-CMOVCC             \ cmovne rcx, r11
   RAX RCX ASM-SINK ENC-OR-RR ;                 \ or rax, rcx

\ rsi carries the caller's open flags and rdx takes the kernel's: the access mode
\ in the low two bits travels unchanged, and four flags move between the BSD and
\ the Linux bit positions. The values are the same ones the aarch64 seam maps.
\ Both translators clobber rax, rcx and r11 and are called before the trap, so a
\ caller stages any other argument register first - the ordering constraint the
\ aarch64 seam's caller already has around its own x6/x7 clobber.
: OS-OPEN-FLAGS ( -- )
   RAX RSI ASM-SINK ENC-MOV-RR
   RAX 3 >IMM8 ASM-SINK ENC-AND-RI8
   RSI $8 $400 OS-FLAG-BIT,                     \ O_APPEND
   RSI $200 $40 OS-FLAG-BIT,                    \ O_CREAT
   RSI $400 $200 OS-FLAG-BIT,                   \ O_TRUNC
   RSI $20000 $100 OS-FLAG-BIT,                 \ O_NOCTTY
   RDX RAX ASM-SINK ENC-MOV-RR ;

\ r10 carries mmap's flags both ways: MAP_SHARED, MAP_PRIVATE and MAP_FIXED have
\ the same values on both kernels, and MAP_ANON moves from the BSD bit to $20.
: OS-MMAP-FLAGS ( -- )
   RAX R10 ASM-SINK ENC-MOV-RR
   RAX $13 >IMM8 ASM-SINK ENC-AND-RI8
   R10 $1000 $20 OS-FLAG-BIT,                   \ MAP_ANONYMOUS
   R10 RAX ASM-SINK ENC-MOV-RR ;

;using
