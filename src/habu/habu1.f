\ habu1.f, engine builder part 1: emits the native engine's primitive bodies,
\ helper routines and seed dictionary for arm64 (Linux and macOS).
\ Requests run before any package or `using` opens, so each file links
\ (linker.f TOP-SCOPE?). A64ASM's encoders live in src/arch/arm64/asm.f.
\
\ An indirect call through an xt fences before it branches: an xt at or above
\ the thread's CP register gets an ISB, the reader's half of publication. An xt
\ held as data may name code main compiled after this thread started, and no
\ instruction fetched before the xt arrived may run in its place. An xt below
\ CP needs none. On main every xt is below the live CP and main's flushes end
\ in an ISB. A task's CP is the snapshot taken as the task went live, after
\ every earlier flush and before the thread existed, so an xt below it names
\ code published before the thread. With a definition pending the snapshot is
\ that definition's body start, not CP: its body is flushed only at `;`, after
\ the thread exists, and CP may move back within it. Everything below a live
\ task's CP stays unwritten, since a rollback with a task live lowers nothing,
\ except a `does>` or patch32 rewrite, which is outside the architecture's
\ concurrent-modification guarantee (only B, BL, BRK, HVC, ISB, NOP, SMC and
\ SVC may be rewritten under a running thread). The compare is the control
\ dependency that orders the fence after the xt's load; the comparison is
\ unsigned. A direct BL needs no fence: its target was published before the
\ caller was compiled.
\
\ spawn-fds's Linux child blocks BLOCKED's signals, then setpgid(0, PGROUP)
\ puts it in the request's group. Every descriptor is then marked close-on-exec,
\ so a -1 slot and every fd at or above NFDS close at exec. Each source is then
\ lifted to a fresh fd at or above the floor in the cell past the request,
\ kept in a scratch array, before any slot is placed, so a source that is
\ another slot's target (map [0 2 1]) survives until it is read, and no lifted
\ copy takes the number of a later source the parent has closed; dup3 with no
\ flags places each one and clears its mark.
\
\ On macOS, spawn-fds's POSIX_SPAWN_CLOEXEC_DEFAULT closes every fd no record
\ targets: a -1 slot and every fd at or above NFDS. Each source is first
\ duplicated to T+i, T above NFDS and every source, so a source that is another
\ slot's target (map [0 2 1]) survives until it is read; then T+i goes to i and
\ is closed. POSIX_SPAWN_SETPGROUP puts the child in the request's PGROUP, and
\ a BLOCKED set takes POSIX_SPAWN_SETSIGMASK with this thread's mask and that
\ set.
require src/core/exit-status.f
require src/arch/arm64/icode.f
require src/habu/primitive-registry.f
require src/habu/data-claims.f
require src/habu/task-abi.f
require src/habu/spawn-abi.f
require src/habu/code-span.f
require src/habu/prof-abi.f
require src/os/macos/macho.f
require src/os/linux/elf.f
require src/habu/fnv.f
require src/arch/arm64/asm.f
require src/arch/arm64/mnem.f
require src/habu/crash.f
require src/habu/fault-record.f
require src/habu/rt.f
HB-TARGET-LINUX-ARM64? [if]
   require src/os/linux/proc-control.f
   require src/os/linux/proc-watch.f
   require src/os/linux/sys.f
[then]
HB-TARGET-MACOS? [if]
   require src/os/macos/proc-control.f
   require src/os/macos/proc-watch.f
   require src/os/macos/sys.f
[then]

package ENGINE-EMIT
public
$D65F03C0 constant W-RET
;package
package ENGINE-EMIT
TYPED-VARIABLE FP-NAME str SCRATCH-ONLY

defer FP-EMIT ( -- )

variable FPL  variable FPE

: FP-ARGS ( str [ -- ] -- )
   is FP-EMIT  FP-NAME !
   FP-NAME @ ENGINE-PRIMS:SPEC-CHECK ;

: FP-REG ( -- )
   FP-NAME @ FPL A64ICODE:LABEL@ FPE A64ICODE:LABEL@ ENGINE-PRIMS:ADD drop ;

public
: FPRIM ( str [ -- ] -- )
   FP-ARGS
   A64ICODE:LBL FPL !  A64ICODE:LBL FPE !
   FP-REG
   FPL A64ICODE:LABEL@ A64ICODE:LBL,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,
   FP-EMIT  30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI,  A64ICODE:RET,  FPE A64ICODE:LABEL@ A64ICODE:LBL, ;

\ FPRIM-L only for a body that emits no BL or BLR of its own; any other body
\ uses FPRIM, whose frame preserves the caller's return address in x30.
: FPRIM-L ( str [ -- ] -- )      \ body preserves LR; stack guards frame their own BL
   FP-ARGS
   A64ICODE:LBL FPL !  A64ICODE:LBL FPE !
   FP-REG
   FPL A64ICODE:LABEL@ A64ICODE:LBL,  FP-EMIT  A64ICODE:RET,  FPE A64ICODE:LABEL@ A64ICODE:LBL, ;

\ label ids (forward refs)
variable LFIND  variable LNUM  variable LDICT
variable LTOK  variable LFLUSH variable LFLUSHTO variable LNCOUNT
\ The process-exit trampoline, declared here because BDIE below calls it.
variable LEXITHOOK
private
\ A compiler-owned callback stub branches to this disjoint shared C entry.
\ Reserve its label before callback-entry is emitted, then register its own
\ span afterward.
variable LCALLBACKTHUNK
variable LCALLBACKADDR
;package
package ENGINE-EMIT
public
variable LDPBAD   \ DP-CHECK's out-of-range target: throws E-DATA-SPACE (habu2.f EM-COMPILE-DIE)
variable LBGROWFAIL   \ the body text could not grow: throws E-BODY-TEXT (habu2.f EM-BODY-GROW-DIE)
;package
\ Hash-index label ids, bound in EMIT-HIDX: LREBUILD for the in-place compaction
\ LHIDXADD and LHIDXBUILD call, LFULL for the cannot-maintain exit, LREKEY for
\ the re-key after a write to a record's wid cell.
package HIDX-EMIT
public
variable LREBUILD
variable LFULL
variable LREKEY
;package
package ENGINE-EMIT
public
variable LHIDXADD
;package

package EM-DEF-OCC
public
variable LCOUNT
variable LAPPEND
variable LRESET
variable LISSUE
variable LINIT
variable LBEFORE
variable LAFTER
variable LMATCH
variable LRECLAIM
;package
\ The one-wordlist search shared by the `bare-search-wl` primitive and the AOT
\ seed's call-site pass.
package WLFIND
public
variable LENTRY  variable LPAIR
;package
package ENGINE-EMIT
public
variable LBCS  variable LDOESPATCH
variable LFINDUSED-CORE variable LSCOPEREC
;package
package ENGINE-EMIT
public
9 constant A   10 constant B   11 constant C
private
12 constant DREG  13 constant EREG
;package

\ Every target selector below names the Linux-aarch64 and macOS arms and calls
\ this otherwise: this file holds primitive bodies for those two targets only,
\ so emitting one for any other target fails closed.
package ENGINE-EMIT
public
: TARGET-UNKNOWN ( -- )
   s" hb: habu1: no primitive body for this target" EXIT-STATUS:INTERNAL die ;
;package

package ENGINE-EMIT
\ Primitive bodies operate on the x19 data stack.
: B+ ( -- )
   B RT:G-POP  A RT:G-POP  A A B A64ICODE:ADD,  A RT:G-PUSH ;

: B- ( -- )
   B RT:G-POP  A RT:G-POP  A A B A64ICODE:SUB,  A RT:G-PUSH ;

: B* ( -- )
   B RT:G-POP  A RT:G-POP  A A B A64ICODE:MUL,  A RT:G-PUSH ;

: BDUP ( -- )
   A RT:G-POP  A RT:G-PUSH  A RT:G-PUSH ;

: BDROP ( -- )
   A64ICODE:XDS A64ICODE:XDS 8 A64ICODE:SUBI, ;

: BSWAP ( -- )
   A RT:G-POP  B RT:G-POP  A RT:G-PUSH  B RT:G-PUSH ;

: BDOT ( -- )
   A RT:G-POP  RT:G-PRINT9 ;

: BU. ( -- )
   A RT:G-POP  RT:G-PRINTU9 ;

\ LINUX-SPAWN's seven arguments or spawn-fds's request and its lift floor at
\ SPAWN-ABI:BYTES, then the error pipe's u32 fds, errno, pid and wait status.
SPAWN-ABI:BYTES 8 + constant LINUX-SPAWN-PIPE-R-OFF
LINUX-SPAWN-PIPE-R-OFF 4 + constant LINUX-SPAWN-PIPE-W-OFF
LINUX-SPAWN-PIPE-R-OFF 8 + constant LINUX-SPAWN-ERR-OFF
LINUX-SPAWN-ERR-OFF 8 + constant LINUX-SPAWN-PID-OFF
LINUX-SPAWN-PID-OFF 8 + constant LINUX-SPAWN-STATUS-OFF
LINUX-SPAWN-STATUS-OFF 8 + 15 + -16 and constant LINUX-SPAWN-FRAME
3 constant LINUX-SPAWN-MIN-ERRFD
1024 constant LINUX-F-LINUX-SPECIFIC-BASE
6 constant LINUX-F-DUPFD-CLOEXEC-OFF
2 constant LINUX-F-SETFD
$80000 constant LINUX-O-CLOEXEC
LINUX-F-LINUX-SPECIFIC-BASE LINUX-F-DUPFD-CLOEXEC-OFF + constant LINUX-F-DUPFD-CLOEXEC

variable LNX-ERR
variable LNX-FD
variable LNX-NEW
variable LNX-CWD
variable LNX-OK
variable LNX-DONE
variable LNX-REAL
variable LNX-PNEG
variable LNX-PCALL
variable LNX-PATH
variable LNX-ARGV
variable LNX-ENV
variable LNX-IN
variable LNX-OUT
variable TIME-OK
variable PS-LOOP
variable PS-DONE
variable CMP-COND
variable PRIM-DONE
variable RSTK-REG
variable DP-REG
variable DP-LOW
variable DP-HIGH
variable SYS-OK
variable SYS-DONE
variable FFI-DREG
variable FFI-OFF
variable FFI-SKIP
variable FFI-LOOP
variable FFI-DONE
public
variable STAT-BUF
private
variable STAT-OK
variable STAT-DONE
variable CATCH-RES
variable CATCH-PUSH
variable FINALLY-DONE
variable C2-INVOKE-DONE
variable C2-INVOKE-CLEAN
variable THROW-NOH
variable THROW-NOREC-FB
variable THROW-NOREC-FB2
public
variable SWL-LOOP
variable SWL-END
variable SWL-NEXT
variable SWL-MATCH
private
variable PARSE-NONE
variable PARSE-DONE
variable FP-COND
variable FD-FRAC
variable FD-INT
variable FD-SIGN
variable BCAP-OK
variable BCAP-CP
variable BCAP-CD
variable TOK-SKIP
variable TOK-HAS
variable TOK-SCAN
variable TOK-GOT
variable TOK-NONE
variable FL-DL
variable FL-DD
variable FL-IL
variable FL-ID
variable FIND-QSCAN
variable FIND-QNONE
variable FIND-QHAS
variable FIND-QBAD
variable FIND-QTAIL
variable FIND-QTAILOK
variable FIND-NEND
variable FIND-START
variable FIND-LOOP
variable FIND-DONE
variable FIND-NEXT
variable FIND-CMP
variable FIND-MATCH
variable FIND-INL
variable FIND-MISS
variable FIND-FOUND
variable NUM-DONE
variable NUM-NDOLL
variable NUM-NOHEX
variable NUM-LOOP
variable NUM-OK
variable NUM-GOTD
variable NUM-ND
variable NUM-NUC
variable NUM-NDOT
variable NUM-ISFRAC
variable NUM-FRAC-NEXT
variable NUM-LINT

: LINUX-SPAWN-FAIL-N ( n -- )
   0 swap 0 A64ICODE:ADDI,
   15 1 A64ICODE:MOVZ,  15 A64ICODE:SP LINUX-SPAWN-ERR-OFF A64ICODE:STRB,
   1 A64ICODE:SP LINUX-SPAWN-ERR-OFF A64ICODE:ADDI,  2 1 A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   0 EXIT-STATUS:EXEC A64ICODE:MOVZ,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS, ;

: LINUX-SPAWN-FAIL ( reg -- )
   REG>N LINUX-SPAWN-FAIL-N ;

: LINUX-DUP2-ARGS ( reg fd reg -- )
   REG>N LNX-ERR !
   FD>N LNX-NEW !
   REG>N LNX-FD ! ;

\ Put the child's descriptor in the first reg on fd n: dup3 it there, or, when
\ it already sits on n, clear the close-on-exec flag a dup3 would have cleared.
\ A negative descriptor leaves n alone.
: LINUX-DUP2-FD ( reg fd reg -- )
   LINUX-DUP2-ARGS
   A64ICODE:LBL {: skip:label :}
   A64ICODE:LBL {: keep:label :}
   A64ICODE:LBL {: done:label :}
   LNX-FD @ 0 A64ICODE:CMPI,  A64ASM:C-LT skip A64ICODE:BCOND,
   0 LNX-FD @ 0 A64ICODE:ADDI,
   LNX-FD @ LNX-NEW @ A64ICODE:CMPI,  A64ASM:C-EQ keep A64ICODE:BCOND,
   1 LNX-NEW @ A64ICODE:MOVZ,  2 0 A64ICODE:MOVZ,  SYSCALL:NR-DUP2 SYSCALL:SYS,  done A64ICODE:B,
   keep A64ICODE:LBL,
   1 LINUX-F-SETFD A64ICODE:MOVZ,  2 0 A64ICODE:MOVZ,  SYSCALL:NR-FCNTL SYSCALL:SYS,
   done A64ICODE:LBL,
   9 A64ASM:C-CS A64ICODE:CSET,  9 skip A64ICODE:CBZ,
      LNX-ERR @ LINUX-SPAWN-FAIL-N
   skip A64ICODE:LBL, ;

: LINUX-CHDIR-ARGS ( reg reg -- )
   REG>N LNX-ERR !
   REG>N LNX-CWD ! ;

: LINUX-CHDIR-FD ( reg reg -- )
   LINUX-CHDIR-ARGS
   A64ICODE:LBL {: skip:label :}
   A64ICODE:LBL {: ok:label :}
   LNX-CWD @ 0 A64ICODE:CMPI,  A64ASM:C-LT skip A64ICODE:BCOND,
   0 LNX-CWD @ 0 A64ICODE:ADDI,  SYSCALL:NR-CHDIR SYSCALL:SYS,
   9 A64ASM:C-CS A64ICODE:CSET,  9 ok A64ICODE:CBZ,
      LNX-ERR @ LINUX-SPAWN-FAIL-N
   ok A64ICODE:LBL,
   skip A64ICODE:LBL, ;

: LINUX-SETPGID-SELF ( reg -- )
   REG>N LNX-ERR !
   A64ICODE:LBL {: ok:label :}
   0 0 A64ICODE:MOVZ,  1 0 A64ICODE:MOVZ,  SYSCALL:NR-SETPGID SYSCALL:SYS,
   9 A64ASM:C-CS A64ICODE:CSET,  9 ok A64ICODE:CBZ,
      LNX-ERR @ LINUX-SPAWN-FAIL-N
   ok A64ICODE:LBL, ;

: LINUX-SPAWN-CLOSE-R ( -- )
   0 A64ICODE:SP LINUX-SPAWN-PIPE-R-OFF A64ICODE:LDRW,  SYSCALL:NR-CLOSE SYSCALL:SYS, ;

: LINUX-SPAWN-CLOSE-W ( -- )
   0 A64ICODE:SP LINUX-SPAWN-PIPE-W-OFF A64ICODE:LDRW,  SYSCALL:NR-CLOSE SYSCALL:SYS, ;

: LINUX-SPAWN-CLOSE-PIPE ( -- )
   LINUX-SPAWN-CLOSE-R
   LINUX-SPAWN-CLOSE-W ;

\ Move the pipe's write end in the frame to the fd in x12 or above, so no
\ descriptor the child places can replace it. x9 becomes 0, or the fcntl's
\ -errno.
: LINUX-SPAWN-LIFT-W ( -- )
   A64ICODE:LBL {: fail:label :}
   A64ICODE:LBL {: done:label :}
   9 0 A64ICODE:MOVZ,
   0 A64ICODE:SP LINUX-SPAWN-PIPE-W-OFF A64ICODE:LDRW,
   0 12 A64ICODE:CMP,  A64ASM:C-GE done A64ICODE:BCOND,
      1 LINUX-F-DUPFD-CLOEXEC A64ICODE:MOVZ,  2 12 0 A64ICODE:ADDI,
      SYSCALL:NR-FCNTL SYSCALL:SYS,
      A64ASM:C-CS fail A64ICODE:BCOND,
      14 0 0 A64ICODE:ADDI,
      0 A64ICODE:SP LINUX-SPAWN-PIPE-W-OFF A64ICODE:LDRW,  SYSCALL:NR-CLOSE SYSCALL:SYS,
      14 A64ICODE:SP LINUX-SPAWN-PIPE-W-OFF A64ICODE:STRW,
      done A64ICODE:B,
   fail A64ICODE:LBL,
      9 0 0 A64ICODE:ADDI,
   done A64ICODE:LBL, ;

: LINUX-SPAWN-PREP-W ( -- )
   12 LINUX-SPAWN-MIN-ERRFD A64ICODE:MOVZ,
   LINUX-SPAWN-LIFT-W ;

: LINUX-SPAWN-WAIT-STORED ( -- )
   0 A64ICODE:SP LINUX-SPAWN-PID-OFF A64ICODE:LDR,
   1 A64ICODE:SP LINUX-SPAWN-STATUS-OFF A64ICODE:ADDI,  2 0 A64ICODE:MOVZ,  3 0 A64ICODE:MOVZ,
   SYSCALL:NR-WAIT4 SYSCALL:SYS, ;

: LINUX-SPAWN-PARENT ( -- )
   A64ICODE:LBL {: ok:label :}
   A64ICODE:LBL {: fail:label :}
   A64ICODE:LBL {: done:label :}
   0 A64ICODE:SP LINUX-SPAWN-PID-OFF A64ICODE:STR,
   LINUX-SPAWN-CLOSE-W
   0 A64ICODE:SP LINUX-SPAWN-PIPE-R-OFF A64ICODE:LDRW,
   1 A64ICODE:SP LINUX-SPAWN-ERR-OFF A64ICODE:ADDI,  2 1 A64ICODE:MOVZ,  SYSCALL:NR-READ SYSCALL:SYS,
   9 A64ASM:C-CS A64ICODE:CSET,  9 fail A64ICODE:CBNZ,
   0 0 A64ICODE:CMPI,  A64ASM:C-EQ ok A64ICODE:BCOND,
   fail A64ICODE:LBL,
      LINUX-SPAWN-CLOSE-R
      LINUX-SPAWN-WAIT-STORED
      9 0 A64ICODE:MOVN,  done A64ICODE:B,
   ok A64ICODE:LBL,
      LINUX-SPAWN-CLOSE-R
      9 A64ICODE:SP LINUX-SPAWN-PID-OFF A64ICODE:LDR,
   done A64ICODE:LBL, ;

: LINUX-SKIP-SAME ( n label -- )
   {: off:n skip:label :}
   10 A64ICODE:SP off A64ICODE:LDR,  9 10 A64ICODE:CMP,  A64ASM:C-EQ skip A64ICODE:BCOND, ;

\ Close the stdio source in the frame cell at n (32, 40, 48: stdin, stdout,
\ stderr; the three roles sit on 0, 1 and 2). Nothing is closed for a
\ source at or below 2 (-1 or already a stdio number), for one an earlier cell
\ also holds (that cell's step closes it), or for the exec-failure pipe's write
\ end.
: LINUX-CLOSE-SOURCE ( n -- )
   {: off:n :}
   A64ICODE:LBL {: skip:label :}
   9 A64ICODE:SP off A64ICODE:LDR,
   9 2 A64ICODE:CMPI,  A64ASM:C-LE skip A64ICODE:BCOND,
   32 BEGIN dup off < WHILE
      dup skip LINUX-SKIP-SAME
      8 +
   REPEAT drop
   14 A64ICODE:SP LINUX-SPAWN-PIPE-W-OFF A64ICODE:LDRW,  9 14 A64ICODE:CMP,  A64ASM:C-EQ skip A64ICODE:BCOND,
   0 9 0 A64ICODE:ADDI,  SYSCALL:NR-CLOSE SYSCALL:SYS,
   9 A64ASM:C-CS A64ICODE:CSET,  9 skip A64ICODE:CBZ,
      14 LINUX-SPAWN-FAIL-N
   skip A64ICODE:LBL, ;

: LINUX-SPAWN-CHILD ( -- )
   LINUX-SPAWN-CLOSE-R
   14 A64ICODE:SP LINUX-SPAWN-PIPE-W-OFF A64ICODE:LDRW,
   14 >REG LINUX-SETPGID-SELF
   14 A64ICODE:SP LINUX-SPAWN-PIPE-W-OFF A64ICODE:LDRW,
   9 A64ICODE:SP 24 A64ICODE:LDR,  9 >REG 14 >REG LINUX-CHDIR-FD
   14 A64ICODE:SP LINUX-SPAWN-PIPE-W-OFF A64ICODE:LDRW,
   9 A64ICODE:SP 32 A64ICODE:LDR,  9 >REG 0 >FD 14 >REG LINUX-DUP2-FD
   14 A64ICODE:SP LINUX-SPAWN-PIPE-W-OFF A64ICODE:LDRW,
   9 A64ICODE:SP 40 A64ICODE:LDR,  9 >REG 1 >FD 14 >REG LINUX-DUP2-FD
   14 A64ICODE:SP LINUX-SPAWN-PIPE-W-OFF A64ICODE:LDRW,
   9 A64ICODE:SP 48 A64ICODE:LDR,  9 >REG 2 >FD 14 >REG LINUX-DUP2-FD
   32 LINUX-CLOSE-SOURCE
   40 LINUX-CLOSE-SOURCE
   48 LINUX-CLOSE-SOURCE
   0 A64ICODE:SP 0 A64ICODE:LDR,  1 A64ICODE:SP 8 A64ICODE:LDR,  2 A64ICODE:SP 16 A64ICODE:LDR,
   SYSCALL:NR-EXECVE SYSCALL:SYS,
   14 A64ICODE:SP LINUX-SPAWN-PIPE-W-OFF A64ICODE:LDRW,
   14 >REG LINUX-SPAWN-FAIL ;

: LINUX-SPAWN ( reg reg reg reg reg reg reg -- )
   REG>N LNX-ERR !
   REG>N LNX-OUT !
   REG>N LNX-IN !
   REG>N LNX-CWD !
   REG>N LNX-ENV !
   REG>N LNX-ARGV !
   REG>N LNX-PATH !
   A64ICODE:LBL {: child:label :}
   A64ICODE:LBL {: closefail:label :}
   A64ICODE:LBL {: fail:label :}
   A64ICODE:LBL {: done:label :}
   A64ICODE:SP A64ICODE:SP LINUX-SPAWN-FRAME A64ICODE:SUBI,
   LNX-PATH @ A64ICODE:SP 0 A64ICODE:STR,  LNX-ARGV @ A64ICODE:SP 8 A64ICODE:STR,  LNX-ENV @ A64ICODE:SP 16 A64ICODE:STR,  LNX-CWD @ A64ICODE:SP 24 A64ICODE:STR,
   LNX-IN @ A64ICODE:SP 32 A64ICODE:STR,  LNX-OUT @ A64ICODE:SP 40 A64ICODE:STR,  LNX-ERR @ A64ICODE:SP 48 A64ICODE:STR,
   0 A64ICODE:SP LINUX-SPAWN-PIPE-R-OFF A64ICODE:ADDI,  1 LINUX-O-CLOEXEC A64ICODE:LIT64,  SYSCALL:NR-PIPE SYSCALL:SYS,
   9 A64ASM:C-CS A64ICODE:CSET,  9 fail A64ICODE:CBNZ,
   LINUX-SPAWN-PREP-W
   9 closefail A64ICODE:CBNZ,
   0 17 A64ICODE:MOVZ,  1 0 A64ICODE:MOVZ,  2 0 A64ICODE:MOVZ,  3 0 A64ICODE:MOVZ,  4 0 A64ICODE:MOVZ,
   SYSCALL:NR-SPAWN SYSCALL:SYS,
   9 A64ASM:C-CS A64ICODE:CSET,  9 closefail A64ICODE:CBNZ,
   0 child A64ICODE:CBZ,
      LINUX-SPAWN-PARENT
      done A64ICODE:B,
   child A64ICODE:LBL,
      LINUX-SPAWN-CHILD
   closefail A64ICODE:LBL,
      LINUX-SPAWN-CLOSE-PIPE
   fail A64ICODE:LBL,
      9 0 A64ICODE:MOVN,
   done A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP LINUX-SPAWN-FRAME A64ICODE:ADDI,
   9 RT:G-PUSH ;

\ spawn-fds on Linux, over LINUX-SPAWN's frame, the request's cells copied to
\ SPAWN-ABI's offsets.
4 constant LINUX-CLOSE-RANGE-CLOEXEC
3 constant SPAWN-PROT-RW                \ PROT_READ | PROT_WRITE
22 constant SPAWN-EINVAL
4 constant SPAWN-EINTR
4 constant SPAWN-ERRNO-BYTES
0 constant LINUX-SIG-BLOCK
public
8 constant LINUX-SIGSET-SIZE
private

\ The child, frame at SP, through the header's steps. A failing step writes its
\ errno to the pipe and exits 127.
: LINUX-SPAWN-FDS-CHILD ( -- )
   A64ICODE:LBL {: fail:label :}
   A64ICODE:LBL {: nocwd:label :}
   A64ICODE:LBL {: lift:label :}
   A64ICODE:LBL {: keep:label :}
   A64ICODE:LBL {: lifted:label :}
   A64ICODE:LBL {: place:label :}
   A64ICODE:LBL {: skip:label :}
   A64ICODE:LBL {: placed:label :}
   A64ICODE:LBL {: unmasked:label :}
   LINUX-SPAWN-CLOSE-R
   0 A64ICODE:SP SPAWN-ABI:BLOCKED A64ICODE:LDR,  0 unmasked A64ICODE:CBZ,
      0 LINUX-SIG-BLOCK A64ICODE:MOVZ,  1 A64ICODE:SP SPAWN-ABI:BLOCKED A64ICODE:ADDI,  2 0 A64ICODE:MOVZ,
      3 LINUX-SIGSET-SIZE A64ICODE:MOVZ,  SYSCALL:NR-RT-SIGPROCMASK SYSCALL:SYS,  A64ASM:C-CS fail A64ICODE:BCOND,
   unmasked A64ICODE:LBL,
   0 0 A64ICODE:MOVZ,  1 A64ICODE:SP SPAWN-ABI:PGROUP A64ICODE:LDR,  SYSCALL:NR-SETPGID SYSCALL:SYS,  A64ASM:C-CS fail A64ICODE:BCOND,
   0 A64ICODE:SP SPAWN-ABI:CWD A64ICODE:LDR,  0 nocwd A64ICODE:CBZ,
      SYSCALL:NR-CHDIR SYSCALL:SYS,  A64ASM:C-CS fail A64ICODE:BCOND,
   nocwd A64ICODE:LBL,
   0 0 A64ICODE:MOVZ,  1 0 A64ICODE:MOVN,  2 LINUX-CLOSE-RANGE-CLOEXEC A64ICODE:MOVZ,
   SYSCALL:NR-CLOSE-RANGE SYSCALL:SYS,  A64ASM:C-CS fail A64ICODE:BCOND,
   0 0 A64ICODE:MOVZ,  1 A64ICODE:SP SPAWN-ABI:NFDS A64ICODE:LDR,  1 1 3 A64ICODE:LSLI,  1 1 8 A64ICODE:ADDI,
   2 SPAWN-PROT-RW A64ICODE:MOVZ,  3 SYSCALL:MAP-ANON-PRIVATE A64ICODE:MOVZ,  4 0 A64ICODE:MOVN,  5 0 A64ICODE:MOVZ,
   SYSCALL:NR-MMAP SYSCALL:SYS,  A64ASM:C-CS fail A64ICODE:BCOND,
   13 0 0 A64ICODE:ADDI,
   10 A64ICODE:SP SPAWN-ABI:NFDS A64ICODE:LDR,  11 A64ICODE:SP SPAWN-ABI:FDS A64ICODE:LDR,  15 13 0 A64ICODE:ADDI,
   lift A64ICODE:LBL,
      10 lifted A64ICODE:CBZ,
      9 11 0 A64ICODE:LDR,  9 0 A64ICODE:CMPI,  A64ASM:C-LT keep A64ICODE:BCOND,
      0 9 0 A64ICODE:ADDI,  1 LINUX-F-DUPFD-CLOEXEC A64ICODE:MOVZ,  2 A64ICODE:SP SPAWN-ABI:BYTES A64ICODE:LDR,
      SYSCALL:NR-FCNTL SYSCALL:SYS,  A64ASM:C-CS fail A64ICODE:BCOND,
      9 0 0 A64ICODE:ADDI,
   keep A64ICODE:LBL,
      9 15 0 A64ICODE:STR,
      11 11 8 A64ICODE:ADDI,  15 15 8 A64ICODE:ADDI,  10 10 1 A64ICODE:SUBI,
      lift A64ICODE:B,
   lifted A64ICODE:LBL,
   10 0 A64ICODE:MOVZ,  12 A64ICODE:SP SPAWN-ABI:NFDS A64ICODE:LDR,  15 13 0 A64ICODE:ADDI,
   place A64ICODE:LBL,
      10 12 A64ICODE:CMP,  A64ASM:C-GE placed A64ICODE:BCOND,
      0 15 0 A64ICODE:LDR,  0 0 A64ICODE:CMPI,  A64ASM:C-LT skip A64ICODE:BCOND,
      1 10 0 A64ICODE:ADDI,  2 0 A64ICODE:MOVZ,  SYSCALL:NR-DUP2 SYSCALL:SYS,  A64ASM:C-CS fail A64ICODE:BCOND,
   skip A64ICODE:LBL,
      15 15 8 A64ICODE:ADDI,  10 10 1 A64ICODE:ADDI,
      place A64ICODE:B,
   placed A64ICODE:LBL,
   0 A64ICODE:SP SPAWN-ABI:PATH A64ICODE:LDR,  1 A64ICODE:SP SPAWN-ABI:ARGV A64ICODE:LDR,  2 A64ICODE:SP SPAWN-ABI:ENVP A64ICODE:LDR,
   SYSCALL:NR-EXECVE SYSCALL:SYS,
   fail A64ICODE:LBL,
   9 31 0 A64ICODE:SUB,  9 A64ICODE:SP LINUX-SPAWN-ERR-OFF A64ICODE:STRW,
   0 A64ICODE:SP LINUX-SPAWN-PIPE-W-OFF A64ICODE:LDRW,  1 A64ICODE:SP LINUX-SPAWN-ERR-OFF A64ICODE:ADDI,
   2 SPAWN-ERRNO-BYTES A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   0 EXIT-STATUS:EXEC A64ICODE:MOVZ,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS, ;

\ The parent, with the child's pid in x0: read the pipe until end of file (an
\ execve that worked: x9 becomes the pid) or the child's errno (x9 becomes
\ -errno and the child is reaped). An interrupted read is retried; any other
\ failed read is its -errno, and the child is reaped.
: LINUX-SPAWN-FDS-PARENT ( -- )
   A64ICODE:LBL {: retry:label :}
   A64ICODE:LBL {: got:label :}
   A64ICODE:LBL {: reap:label :}
   A64ICODE:LBL {: done:label :}
   0 A64ICODE:SP LINUX-SPAWN-PID-OFF A64ICODE:STR,
   LINUX-SPAWN-CLOSE-W
   31 A64ICODE:SP LINUX-SPAWN-ERR-OFF A64ICODE:STRW,
   retry A64ICODE:LBL,
   0 A64ICODE:SP LINUX-SPAWN-PIPE-R-OFF A64ICODE:LDRW,  1 A64ICODE:SP LINUX-SPAWN-ERR-OFF A64ICODE:ADDI,
   2 SPAWN-ERRNO-BYTES A64ICODE:MOVZ,  SYSCALL:NR-READ SYSCALL:SYS,
   A64ASM:C-CC got A64ICODE:BCOND,
      14 0 SPAWN-EINTR A64ICODE:ADDI,  14 retry A64ICODE:CBZ,
      9 0 0 A64ICODE:ADDI,  reap A64ICODE:B,
   got A64ICODE:LBL,
   9 A64ICODE:SP LINUX-SPAWN-PID-OFF A64ICODE:LDR,  0 done A64ICODE:CBZ,
   9 A64ICODE:SP LINUX-SPAWN-ERR-OFF A64ICODE:LDRW,  9 31 9 A64ICODE:SUB,
   reap A64ICODE:LBL,
   LINUX-SPAWN-WAIT-STORED
   done A64ICODE:LBL,
   LINUX-SPAWN-CLOSE-R ;

\ The request's address is in x10. x9 becomes the child's pid or -errno. The
\ floor, max(NFDS, highest source + 1), goes in the cell past the request, and
\ the error pipe's write end is lifted to it or above: no slot targets it and no
\ source names it, so a closed source fd is EBADF in the child rather than the
\ pipe placed onto a slot.
: LINUX-SPAWN-FDS ( -- )
   A64ICODE:LBL {: child:label :}
   A64ICODE:LBL {: closefail:label :}
   A64ICODE:LBL {: leave:label :}
   A64ICODE:LBL {: top:label :}
   A64ICODE:LBL {: below:label :}
   A64ICODE:LBL {: topped:label :}
   A64ICODE:SP A64ICODE:SP LINUX-SPAWN-FRAME A64ICODE:SUBI,
   SPAWN-ABI:BYTES 0 ?do
      11 10 i A64ICODE:LDR,  11 A64ICODE:SP i A64ICODE:STR,
   8 +loop
   0 A64ICODE:SP LINUX-SPAWN-PIPE-R-OFF A64ICODE:ADDI,  1 LINUX-O-CLOEXEC A64ICODE:LIT64,  SYSCALL:NR-PIPE SYSCALL:SYS,
   9 0 0 A64ICODE:ADDI,  A64ASM:C-CS leave A64ICODE:BCOND,
   12 A64ICODE:SP SPAWN-ABI:NFDS A64ICODE:LDR,  13 12 0 A64ICODE:ADDI,  11 A64ICODE:SP SPAWN-ABI:FDS A64ICODE:LDR,
   top A64ICODE:LBL,
      13 topped A64ICODE:CBZ,
      9 11 0 A64ICODE:LDR,  9 12 A64ICODE:CMP,  A64ASM:C-LT below A64ICODE:BCOND,
      12 9 1 A64ICODE:ADDI,
   below A64ICODE:LBL,
      11 11 8 A64ICODE:ADDI,  13 13 1 A64ICODE:SUBI,
      top A64ICODE:B,
   topped A64ICODE:LBL,
   12 A64ICODE:SP SPAWN-ABI:BYTES A64ICODE:STR,  LINUX-SPAWN-LIFT-W
   9 closefail A64ICODE:CBNZ,
   0 17 A64ICODE:MOVZ,  1 0 A64ICODE:MOVZ,  2 0 A64ICODE:MOVZ,  3 0 A64ICODE:MOVZ,  4 0 A64ICODE:MOVZ,
   SYSCALL:NR-SPAWN SYSCALL:SYS,
   9 0 0 A64ICODE:ADDI,  A64ASM:C-CS closefail A64ICODE:BCOND,
   0 child A64ICODE:CBZ,
      LINUX-SPAWN-FDS-PARENT
      leave A64ICODE:B,
   child A64ICODE:LBL,
      LINUX-SPAWN-FDS-CHILD
   closefail A64ICODE:LBL,
      LINUX-SPAWN-CLOSE-PIPE
   leave A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP LINUX-SPAWN-FRAME A64ICODE:ADDI, ;

: BPIPE ( -- )                     \ ( -- rfd wfd rc ) rc=0, or -errno thrice
   A64ICODE:LBL LNX-OK !
   A64ICODE:LBL LNX-DONE !
   HB-TARGET-LINUX-ARM64? IF
      A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,
      0 A64ICODE:SP 0 A64ICODE:ADDI,  1 0 A64ICODE:MOVZ,  SYSCALL:NR-PIPE SYSCALL:SYS,
      9 A64ASM:C-CS A64ICODE:CSET,  9 LNX-OK A64ICODE:LABEL@ A64ICODE:CBZ,
         0 RT:G-PUSH  0 RT:G-PUSH  0 RT:G-PUSH  LNX-DONE A64ICODE:LABEL@ A64ICODE:B,
      LNX-OK A64ICODE:LABEL@ A64ICODE:LBL,
      0 A64ICODE:SP 0 A64ICODE:LDRW,  1 A64ICODE:SP 4 A64ICODE:LDRW,
      0 RT:G-PUSH  1 RT:G-PUSH  9 0 A64ICODE:MOVZ,  9 RT:G-PUSH
      LNX-DONE A64ICODE:LABEL@ A64ICODE:LBL,
      A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI,
      exit
   THEN
   HB-TARGET-MACOS? IF
      SYSCALL:NR-PIPE SYSCALL:SYS,
      9 A64ASM:C-CS A64ICODE:CSET,  9 LNX-OK A64ICODE:LABEL@ A64ICODE:CBZ,
         0 31 0 A64ICODE:SUB,  0 RT:G-PUSH  0 RT:G-PUSH  0 RT:G-PUSH  LNX-DONE A64ICODE:LABEL@ A64ICODE:B,
      LNX-OK A64ICODE:LABEL@ A64ICODE:LBL,
      0 RT:G-PUSH  1 RT:G-PUSH  9 0 A64ICODE:MOVZ,  9 RT:G-PUSH
      LNX-DONE A64ICODE:LABEL@ A64ICODE:LBL,
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BDUP2 ( -- )                     \ ( oldfd newfd -- rc ) rc=newfd or -1
   1 RT:G-POP  0 RT:G-POP
   A64ICODE:LBL LNX-OK !
   A64ICODE:LBL LNX-DONE !
   HB-TARGET-LINUX-ARM64? IF
      2 0 A64ICODE:MOVZ,
      SYSCALL:NR-DUP2 SYSCALL:SYS,
      9 A64ASM:C-CS A64ICODE:CSET,  9 LNX-OK A64ICODE:LABEL@ A64ICODE:CBZ,
         0 0 A64ICODE:MOVN,  LNX-DONE A64ICODE:LABEL@ A64ICODE:B,
      LNX-OK A64ICODE:LABEL@ A64ICODE:LBL,
      LNX-DONE A64ICODE:LABEL@ A64ICODE:LBL,
      0 RT:G-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF
      SYSCALL:NR-DUP2 SYSCALL:SYS,
      9 A64ASM:C-CS A64ICODE:CSET,  9 LNX-OK A64ICODE:LABEL@ A64ICODE:CBZ,
         0 0 A64ICODE:MOVN,  LNX-DONE A64ICODE:LABEL@ A64ICODE:B,
      LNX-OK A64ICODE:LABEL@ A64ICODE:LBL,
      LNX-DONE A64ICODE:LABEL@ A64ICODE:LBL,
      0 RT:G-PUSH
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

13 constant LINUX-SIGPIPE
1 constant LINUX-SIG-IGN

\ x0 is rt_sigaction's answer: 0, or -errno.
: LINUX-IGNORE-SIGPIPE ( -- )
   A64ICODE:SP A64ICODE:SP 64 A64ICODE:SUBI,
   9 LINUX-SIG-IGN A64ICODE:MOVZ,  9 A64ICODE:SP 0 A64ICODE:STR,
   9 0 A64ICODE:MOVZ,  9 A64ICODE:SP 8 A64ICODE:STR,  9 A64ICODE:SP 16 A64ICODE:STR,  9 A64ICODE:SP 24 A64ICODE:STR,
   0 LINUX-SIGPIPE A64ICODE:MOVZ,  1 A64ICODE:SP 0 A64ICODE:ADDI,  2 0 A64ICODE:MOVZ,  3 LINUX-SIGSET-SIZE A64ICODE:MOVZ,
   SYSCALL:NR-SIGACTION SYSCALL:SYS,
   A64ICODE:SP A64ICODE:SP 64 A64ICODE:ADDI, ;

: BFCNTL ( -- )                    \ ( fd cmd arg -- rc ) rc=sysret or -errno
   2 RT:G-POP  1 RT:G-POP  0 RT:G-POP
   A64ICODE:LBL {: ok:label :}
   A64ICODE:LBL {: done:label :}
   A64ICODE:LBL LNX-REAL !
   HB-TARGET-LINUX-ARM64? IF
      1 73 A64ICODE:CMPI,  A64ASM:C-NE LNX-REAL A64ICODE:LABEL@ A64ICODE:BCOND,
         LINUX-IGNORE-SIGPIPE
         done A64ICODE:B,
      LNX-REAL A64ICODE:LABEL@ A64ICODE:LBL,
      SYSCALL:NR-FCNTL SYSCALL:SYS,
      done A64ICODE:LBL,
      0 RT:G-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF
      SYSCALL:NR-FCNTL SYSCALL:SYS,
      9 A64ASM:C-CS A64ICODE:CSET,  9 ok A64ICODE:CBZ,
         0 31 0 A64ICODE:SUB,  done A64ICODE:B,
      ok A64ICODE:LBL,
      done A64ICODE:LBL,
      0 RT:G-PUSH
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

\ BPOLL publishes -errno: poll(2) is the one blocking call the kernel never
\ restarts. read, write and wait4 restart under SA_RESTART, which the engine's
\ only handler sets; a handler without it would need -errno on those too.
\ Linux leaves 0/-errno in x0; macOS sets carry with a positive errno, negated
\ here.
: BPOLL ( -- )                     \ ( fds nfds timeout -- rc ) rc=nready/0 or -errno
   2 RT:G-POP  1 RT:G-POP  0 RT:G-POP
   A64ICODE:LBL LNX-PNEG !
   A64ICODE:LBL LNX-PCALL !
   HB-TARGET-LINUX-ARM64? IF
      A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,
      2 0 A64ICODE:CMPI,  A64ASM:C-LT LNX-PNEG A64ICODE:LABEL@ A64ICODE:BCOND,
         5 1000 A64ICODE:MOVZ,  6 2 5 A64ICODE:UDIV,
         7 6 5 A64ICODE:MUL,  7 2 7 A64ICODE:SUB,
         5 1000 A64ICODE:MOVZ,  7 7 5 A64ICODE:MUL,  5 1000 A64ICODE:MOVZ,  7 7 5 A64ICODE:MUL,
         6 A64ICODE:SP 0 A64ICODE:STR,  7 A64ICODE:SP 8 A64ICODE:STR,
         2 A64ICODE:SP 0 A64ICODE:ADDI,  LNX-PCALL A64ICODE:LABEL@ A64ICODE:B,
      LNX-PNEG A64ICODE:LABEL@ A64ICODE:LBL,
         2 0 A64ICODE:MOVZ,
      LNX-PCALL A64ICODE:LABEL@ A64ICODE:LBL,
      3 0 A64ICODE:MOVZ,  4 0 A64ICODE:MOVZ,
      SYSCALL:NR-POLL SYSCALL:SYS,
      0 RT:G-PUSH                                  \ x0 already holds nready/0 or -errno
      A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI,
      exit
   THEN
   HB-TARGET-MACOS? IF
      A64ICODE:LBL {: ok:label :}
      SYSCALL:NR-POLL SYSCALL:SYS,
      9 A64ASM:C-CS A64ICODE:CSET,  9 ok A64ICODE:CBZ,          \ carry clear -> x0 already holds nready or 0
         10 0 A64ICODE:MOVZ,  0 10 0 A64ICODE:SUB,        \ carry set -> x0 = 0 - errno = -errno
      ok A64ICODE:LBL,
      0 RT:G-PUSH
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BKILL ( -- )                     \ ( pid sig -- rc ) rc=0 or -1
   1 RT:G-POP  0 RT:G-POP
   A64ICODE:LBL LNX-OK !
   A64ICODE:LBL LNX-DONE !
   SYSCALL:NR-KILL SYSCALL:SYS,
   9 A64ASM:C-CS A64ICODE:CSET,  9 LNX-OK A64ICODE:LABEL@ A64ICODE:CBZ,
      0 0 A64ICODE:MOVN,  LNX-DONE A64ICODE:LABEL@ A64ICODE:B,
   LNX-OK A64ICODE:LABEL@ A64ICODE:LBL,
   LNX-DONE A64ICODE:LABEL@ A64ICODE:LBL,
   0 RT:G-PUSH ;

: BSETPGID ( -- )                  \ ( pid pgid -- rc ) rc=0 or -1
   1 RT:G-POP  0 RT:G-POP
   A64ICODE:LBL LNX-OK !
   A64ICODE:LBL LNX-DONE !
   SYSCALL:NR-SETPGID SYSCALL:SYS,
   9 A64ASM:C-CS A64ICODE:CSET,  9 LNX-OK A64ICODE:LABEL@ A64ICODE:CBZ,
      0 0 A64ICODE:MOVN,  LNX-DONE A64ICODE:LABEL@ A64ICODE:B,
   LNX-OK A64ICODE:LABEL@ A64ICODE:LBL,
   LNX-DONE A64ICODE:LABEL@ A64ICODE:LBL,
   0 RT:G-PUSH ;

: BWAITSTATUS ( -- )               \ ( pid -- status ) wait4 raw status; -1 = wait failed
   A RT:G-POP
   A64ICODE:LBL LNX-OK !
   A64ICODE:LBL LNX-DONE !
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,
   0 9 0 A64ICODE:ADDI,
   1 A64ICODE:SP 0 A64ICODE:ADDI,  2 0 A64ICODE:MOVZ,  3 0 A64ICODE:MOVZ,
   SYSCALL:NR-WAIT4 SYSCALL:SYS,
   9 A64ASM:C-CS A64ICODE:CSET,  9 LNX-OK A64ICODE:LABEL@ A64ICODE:CBZ,
      9 0 A64ICODE:MOVN,  LNX-DONE A64ICODE:LABEL@ A64ICODE:B,
   LNX-OK A64ICODE:LABEL@ A64ICODE:LBL,
   9 A64ICODE:SP 0 A64ICODE:LDRW,
   LNX-DONE A64ICODE:LABEL@ A64ICODE:LBL,
   9 RT:G-PUSH
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI, ;

1040 constant SPAWN-ACTION-SIZE
7 constant SPAWN-ACTION-CAP            \ a chdir, three stdio records, three closes
0 constant SPAWN-PID-OFF
16 constant SPAWN-ARGV-OFF
24 constant SPAWN-ARGV-END-OFF
32 constant SPAWN-ENVP-OFF
48 constant SPAWN-ADESC-OFF
48 constant SPAWN-ADESC-ATTR-SIZE-OFF
56 constant SPAWN-ADESC-ATTR-PTR-OFF
64 constant SPAWN-ADESC-FA-SIZE-OFF
72 constant SPAWN-ADESC-FA-PTR-OFF
128 constant SPAWN-ADESC-SIZE
176 constant SPAWN-ATTR-OFF
184 constant SPAWN-ATTR-SIZE
0 constant SPAWN-ATTR-FLAGS-OFF
8 constant SPAWN-ATTR-SIGMASK-OFF          \ psa_sigmask, a u32 sigset_t
12 constant SPAWN-ATTR-PGROUP-OFF          \ psa_pgroup, a pid_t
56 constant SPAWN-ATTR-RESERVED-OFF
68 constant SPAWN-ATTR-PRIORITY-OFF
72 constant SPAWN-ATTR-MEMLIMIT-ACTIVE-OFF
76 constant SPAWN-ATTR-MEMLIMIT-INACTIVE-OFF
112 constant SPAWN-ATTR-SUBCPU-OFF
2 constant POSIX-SPAWN-SETPGROUP
8 constant POSIX-SPAWN-SETSIGMASK
1 constant DARWIN-SIG-BLOCK
368 constant SPAWN-ACTIONS-OFF
0 constant SPAWN-FA-CAP-OFF
4 constant SPAWN-FA-COUNT-OFF
8 constant SPAWN-FA-ACTS-OFF
1 constant PSFA-CLOSE
2 constant PSFA-DUP2
3 constant PSFA-INHERIT
5 constant PSFA-CHDIR
8 constant SPAWN-CHDIR-PATH-OFF
SPAWN-ACTION-SIZE SPAWN-CHDIR-PATH-OFF - constant SPAWN-CHDIR-PATH-CAP

\ The frame ends with room for SPAWN-ACTION-CAP records, rounded to 16 bytes.
\ SUBI takes at most 4095, so it is entered in two steps.
SPAWN-ACTIONS-OFF SPAWN-FA-ACTS-OFF + SPAWN-ACTION-CAP SPAWN-ACTION-SIZE * +
   15 + -16 and constant SPAWN-FRAME
4080 constant SPAWN-FRAME-STEP

variable SDA-FD  variable SDA-NEW  variable SDA-SKIP
variable SCA-CWD  variable SCA-FAIL
variable SCA-COPY  variable SCA-OVER  variable SCA-DONE
variable SPD-PATH  variable SAE-ARGV  variable SAE-ENVP
variable SDEF-PATH  variable SADV-ARGV
variable SFIN-OK  variable SFIN-FAIL
variable BSP-OK  variable BSP-DN
variable SZA-I

\ Point x14 at the next record of the runtime file-actions blob at x13.
: SPAWN-NEXT-ACTION ( -- )
   14 13 SPAWN-FA-COUNT-OFF A64ICODE:LDRW,  15 SPAWN-ACTION-SIZE A64ICODE:MOVZ,  14 14 15 A64ICODE:MUL,
   14 14 SPAWN-FA-ACTS-OFF A64ICODE:ADDI,  14 14 13 A64ICODE:ADD, ;

: SPAWN-COUNT-ACTION ( -- )
   14 13 SPAWN-FA-COUNT-OFF A64ICODE:LDRW,  14 14 1 A64ICODE:ADDI,  14 13 SPAWN-FA-COUNT-OFF A64ICODE:STRW, ;

: SPAWN-DUP2-ARGS ( reg fd -- )
   SDA-NEW !  SDA-FD ! ;

\ Emit the record that puts the descriptor in reg on fd n: PSFA_DUP2, or
\ PSFA_INHERIT when it already sits on n, since xnu's dup2 of a descriptor onto
\ itself keeps close-on-exec. A negative descriptor emits nothing.
: SPAWN-DUP2-ACTION ( reg fd -- )
   SPAWN-DUP2-ARGS
   A64ICODE:LBL SDA-SKIP !
   SDA-FD @ 0 A64ICODE:CMPI,  A64ASM:C-LT SDA-SKIP A64ICODE:LABEL@ A64ICODE:BCOND,
   SPAWN-NEXT-ACTION
   15 PSFA-DUP2 A64ICODE:MOVZ,  16 PSFA-INHERIT A64ICODE:MOVZ,
   SDA-FD @ SDA-NEW @ A64ICODE:CMPI,  15 16 15 A64ASM:C-EQ A64ICODE:CSEL,
   15 14 0 A64ICODE:STRW,
   SDA-FD @ 14 4 A64ICODE:STRW,
   15 SDA-NEW @ A64ICODE:MOVZ,  15 14 8 A64ICODE:STRW,
   SPAWN-COUNT-ACTION
   SDA-SKIP A64ICODE:LABEL@ A64ICODE:LBL, ;

\ Emit a PSFA_CLOSE record for the stdio source in reg (x10, x11 or x12), after
\ the three stdio records. No record for a source at or below 2 (-1 or already
\ a stdio number) or for one an earlier stdio register also holds, whose record
\ closes it: xnu fails the whole spawn when a close action fails, so each
\ source is closed exactly once.
: SPAWN-CLOSE-ACTION ( reg -- )
   REG>N {: r:n :}
   A64ICODE:LBL {: skip:label :}
   r 2 A64ICODE:CMPI,  A64ASM:C-LE skip A64ICODE:BCOND,
   10 BEGIN dup r < WHILE
      r over A64ICODE:CMP,  A64ASM:C-EQ skip A64ICODE:BCOND,
      1+
   REPEAT drop
   SPAWN-NEXT-ACTION
   15 PSFA-CLOSE A64ICODE:MOVZ,  15 14 0 A64ICODE:STRW,
   r 14 4 A64ICODE:STRW,
   SPAWN-COUNT-ACTION
   skip A64ICODE:LBL, ;

: SPAWN-CHDIR-ARGS ( reg label -- )
   SCA-FAIL !  SCA-CWD ! ;

: SPAWN-CHDIR-LABELS ( -- )
   A64ICODE:LBL SCA-COPY !  A64ICODE:LBL SCA-OVER !  A64ICODE:LBL SCA-DONE ! ;

: SPAWN-CHDIR-ACTION ( reg label -- )
   SPAWN-CHDIR-ARGS
   SPAWN-CHDIR-LABELS
   SPAWN-NEXT-ACTION
   15 PSFA-CHDIR A64ICODE:MOVZ,  15 14 0 A64ICODE:STRW,
   16 SCA-CWD @ 0 A64ICODE:ADDI,
   17 14 SPAWN-CHDIR-PATH-OFF A64ICODE:ADDI,
   5 SPAWN-CHDIR-PATH-CAP A64ICODE:MOVZ,
   SCA-COPY A64ICODE:LABEL@ A64ICODE:LBL,
      5 0 A64ICODE:CMPI,  A64ASM:C-EQ SCA-OVER A64ICODE:LABEL@ A64ICODE:BCOND,
      15 16 0 A64ICODE:LDRB,
      15 17 0 A64ICODE:STRB,
      16 16 1 A64ICODE:ADDI,
      17 17 1 A64ICODE:ADDI,
      5 5 1 A64ICODE:SUBI,
      15 SCA-COPY A64ICODE:LABEL@ A64ICODE:CBNZ,
   SPAWN-COUNT-ACTION
   SCA-DONE A64ICODE:LABEL@ A64ICODE:B,
   SCA-OVER A64ICODE:LABEL@ A64ICODE:LBL,
   9 0 A64ICODE:MOVN,  SCA-FAIL A64ICODE:LABEL@ A64ICODE:B,
   SCA-DONE A64ICODE:LABEL@ A64ICODE:LBL, ;

: SPAWN-DARWIN-FRAME-ENTER ( -- )
   A64ICODE:SP A64ICODE:SP SPAWN-FRAME-STEP A64ICODE:SUBI,
   A64ICODE:SP A64ICODE:SP SPAWN-FRAME SPAWN-FRAME-STEP - A64ICODE:SUBI, ;

: SPAWN-DARWIN-FRAME-LEAVE ( -- )
   A64ICODE:SP A64ICODE:SP SPAWN-FRAME SPAWN-FRAME-STEP - A64ICODE:ADDI,
   A64ICODE:SP A64ICODE:SP SPAWN-FRAME-STEP A64ICODE:ADDI, ;

: SPAWN-DARWIN-ACTIONS-RESET ( -- )
   13 A64ICODE:SP SPAWN-ACTIONS-OFF A64ICODE:ADDI,
   14 SPAWN-ACTION-CAP A64ICODE:MOVZ,  14 13 SPAWN-FA-CAP-OFF A64ICODE:STRW,
   14 0 A64ICODE:MOVZ,  14 13 SPAWN-FA-COUNT-OFF A64ICODE:STRW, ;

\ The stdio records, then the closes, so a source two roles share is closed
\ after both records read it.
: SPAWN-DARWIN-STDIO-ACTIONS ( -- )
   10 >REG 0 >FD SPAWN-DUP2-ACTION
   11 >REG 1 >FD SPAWN-DUP2-ACTION
   12 >REG 2 >FD SPAWN-DUP2-ACTION
   10 >REG SPAWN-CLOSE-ACTION
   11 >REG SPAWN-CLOSE-ACTION
   12 >REG SPAWN-CLOSE-ACTION ;

: SPAWN-DARWIN-ZERO-ADESC ( -- )
   14 0 A64ICODE:MOVZ,
   0 SZA-I !
   BEGIN SZA-I @ SPAWN-ADESC-SIZE < WHILE
      14 A64ICODE:SP SPAWN-ADESC-OFF SZA-I @ + A64ICODE:STR,
      SZA-I @ 8 + SZA-I !
   REPEAT ;

: SPAWN-DARWIN-ZERO-ATTR ( -- )
   14 0 A64ICODE:MOVZ,
   0 SZA-I !
   BEGIN SZA-I @ SPAWN-ATTR-SIZE < WHILE
      14 A64ICODE:SP SPAWN-ATTR-OFF SZA-I @ + A64ICODE:STR,
      SZA-I @ 8 + SZA-I !
   REPEAT ;

: SPAWN-DARWIN-ATTR-DEFAULTS ( -- )
   14 POSIX-SPAWN-SETPGROUP A64ICODE:MOVZ,
   14 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-FLAGS-OFF + A64ICODE:STRW,
   14 1 A64ICODE:MOVZ,
   14 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-RESERVED-OFF + A64ICODE:STR,
   14 0 A64ICODE:MOVN,
   14 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-PRIORITY-OFF + A64ICODE:STRW,
   14 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-MEMLIMIT-ACTIVE-OFF + A64ICODE:STRW,
   14 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-MEMLIMIT-INACTIVE-OFF + A64ICODE:STRW,
   14 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-SUBCPU-OFF + A64ICODE:STRW,
   14 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-SUBCPU-OFF 4 + + A64ICODE:STRW,
   14 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-SUBCPU-OFF 8 + + A64ICODE:STRW,
   14 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-SUBCPU-OFF 12 + + A64ICODE:STRW, ;

: SPAWN-DARWIN-FILL-ADESC ( -- )
   A64ICODE:LBL {: done:label :}
   SPAWN-DARWIN-ATTR-DEFAULTS
   14 SPAWN-ATTR-SIZE A64ICODE:MOVZ,
   14 A64ICODE:SP SPAWN-ADESC-ATTR-SIZE-OFF A64ICODE:STR,
   14 A64ICODE:SP SPAWN-ATTR-OFF A64ICODE:ADDI,
   14 A64ICODE:SP SPAWN-ADESC-ATTR-PTR-OFF A64ICODE:STR,
   14 13 SPAWN-FA-COUNT-OFF A64ICODE:LDRW,
   14 done A64ICODE:CBZ,
   15 SPAWN-ACTION-SIZE A64ICODE:MOVZ,
   14 14 15 A64ICODE:MUL,
   14 14 SPAWN-FA-ACTS-OFF A64ICODE:ADDI,
   14 A64ICODE:SP SPAWN-ADESC-FA-SIZE-OFF A64ICODE:STR,
   13 A64ICODE:SP SPAWN-ADESC-FA-PTR-OFF A64ICODE:STR,
   done A64ICODE:LBL, ;

: SPAWN-DARWIN-USE-ADESC ( -- )
   2 A64ICODE:SP SPAWN-ADESC-OFF A64ICODE:ADDI, ;

: SPAWN-DARWIN-PID-PATH ( reg -- )
   SPD-PATH !
   0 A64ICODE:SP SPAWN-PID-OFF A64ICODE:ADDI,
   1 SPD-PATH @ 0 A64ICODE:ADDI, ;

: SPAWN-DARWIN-ARGV-ENVP ( reg reg -- )
   SAE-ENVP !  SAE-ARGV !
   3 SAE-ARGV @ 0 A64ICODE:ADDI,
   4 SAE-ENVP @ 0 A64ICODE:ADDI, ;

: SPAWN-DARWIN-DEFAULT-ARGV-ENVP ( reg -- )
   SDEF-PATH !
   SDEF-PATH @ A64ICODE:SP SPAWN-ARGV-OFF A64ICODE:STR,
   14 0 A64ICODE:MOVZ,
   14 A64ICODE:SP SPAWN-ARGV-END-OFF A64ICODE:STR,
   14 A64ICODE:SP SPAWN-ENVP-OFF A64ICODE:STR, ;

: SPAWN-DARWIN-DEFAULT-ENVP ( -- )
   14 0 A64ICODE:MOVZ,
   14 A64ICODE:SP SPAWN-ARGV-OFF A64ICODE:STR, ;

: SPAWN-DARWIN-USE-DEFAULT-ARGV-ENVP ( -- )
   3 A64ICODE:SP SPAWN-ARGV-OFF A64ICODE:ADDI,
   4 A64ICODE:SP SPAWN-ENVP-OFF A64ICODE:ADDI, ;

: SPAWN-DARWIN-ARGV-DEFAULT-ENVP ( reg -- )
   SADV-ARGV !
   3 SADV-ARGV @ 0 A64ICODE:ADDI,
   4 A64ICODE:SP SPAWN-ARGV-OFF A64ICODE:ADDI, ;

: SPAWN-DARWIN-FINISH ( label label -- )
   SFIN-FAIL !  SFIN-OK !
   SYSCALL:NR-SPAWN SYSCALL:SYS,
   9 A64ASM:C-CS A64ICODE:CSET,  9 SFIN-OK A64ICODE:LABEL@ A64ICODE:CBZ,
      9 0 A64ICODE:MOVZ,  9 9 0 A64ICODE:SUB,  SFIN-FAIL A64ICODE:LABEL@ A64ICODE:B,
   SFIN-OK A64ICODE:LABEL@ A64ICODE:LBL,
   9 A64ICODE:SP SPAWN-PID-OFF A64ICODE:LDRW,
   SFIN-FAIL A64ICODE:LABEL@ A64ICODE:LBL,
   9 RT:G-PUSH ;

: BSP-LABELS3 ( -- )
   A64ICODE:LBL BSP-OK !  A64ICODE:LBL BSP-DN ! ;

: BSP-LABELS2 ( -- )
   A64ICODE:LBL BSP-OK !  A64ICODE:LBL BSP-DN ! ;

: BSPAWNIO ( -- )                  \ ( pathz stdinfd stdoutfd stderrfd -- pid|-errno )
   BSP-LABELS3
   12 RT:G-POP  11 RT:G-POP  10 RT:G-POP  9 RT:G-POP
   HB-TARGET-LINUX-ARM64? IF
      A64ICODE:SP A64ICODE:SP 64 A64ICODE:SUBI,
      9 A64ICODE:SP 16 A64ICODE:STR,
      13 0 A64ICODE:MOVZ,  13 A64ICODE:SP 24 A64ICODE:STR,
      13 A64ICODE:SP 32 A64ICODE:STR,
      13 0 A64ICODE:MOVN,
      14 A64ICODE:SP 16 A64ICODE:ADDI,  15 A64ICODE:SP 32 A64ICODE:ADDI,
      9 >REG 14 >REG 15 >REG 13 >REG 10 >REG 11 >REG 12 >REG LINUX-SPAWN
      A64ICODE:SP A64ICODE:SP 64 A64ICODE:ADDI,
      exit
   THEN
   HB-TARGET-MACOS? IF
      SPAWN-DARWIN-FRAME-ENTER
      9 >REG SPAWN-DARWIN-DEFAULT-ARGV-ENVP
      SPAWN-DARWIN-ACTIONS-RESET
      SPAWN-DARWIN-STDIO-ACTIONS
      SPAWN-DARWIN-ZERO-ADESC
      SPAWN-DARWIN-ZERO-ATTR
      SPAWN-DARWIN-FILL-ADESC
      9 >REG SPAWN-DARWIN-PID-PATH
      SPAWN-DARWIN-USE-ADESC
      SPAWN-DARWIN-USE-DEFAULT-ARGV-ENVP
      BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
      SPAWN-DARWIN-FRAME-LEAVE
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BSPAWNARGVIO ( -- )              \ ( pathz argvp stdinfd stdoutfd stderrfd -- pid|-errno )
   BSP-LABELS3
   12 RT:G-POP  11 RT:G-POP  10 RT:G-POP  9 RT:G-POP  8 RT:G-POP
   HB-TARGET-LINUX-ARM64? IF
      A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,
      13 0 A64ICODE:MOVZ,  13 A64ICODE:SP 0 A64ICODE:STR,
      13 0 A64ICODE:MOVN,
      15 A64ICODE:SP 0 A64ICODE:ADDI,
      8 >REG 9 >REG 15 >REG 13 >REG 10 >REG 11 >REG 12 >REG LINUX-SPAWN
      A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI,
      exit
   THEN
   HB-TARGET-MACOS? IF
      SPAWN-DARWIN-FRAME-ENTER
      SPAWN-DARWIN-DEFAULT-ENVP
      SPAWN-DARWIN-ACTIONS-RESET
      SPAWN-DARWIN-STDIO-ACTIONS
      SPAWN-DARWIN-ZERO-ADESC
      SPAWN-DARWIN-ZERO-ATTR
      SPAWN-DARWIN-FILL-ADESC
      8 >REG SPAWN-DARWIN-PID-PATH
      SPAWN-DARWIN-USE-ADESC
      9 >REG SPAWN-DARWIN-ARGV-DEFAULT-ENVP
      BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
      SPAWN-DARWIN-FRAME-LEAVE
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BSPAWNARGVENVIO ( -- )           \ ( pathz argvp envp stdinfd stdoutfd stderrfd -- pid|-errno )
   BSP-LABELS3
   12 RT:G-POP  11 RT:G-POP  10 RT:G-POP  7 RT:G-POP  9 RT:G-POP  8 RT:G-POP
   HB-TARGET-LINUX-ARM64? IF
      13 0 A64ICODE:MOVN,
      8 >REG 9 >REG 7 >REG 13 >REG 10 >REG 11 >REG 12 >REG LINUX-SPAWN
      exit
   THEN
   HB-TARGET-MACOS? IF
      SPAWN-DARWIN-FRAME-ENTER
      SPAWN-DARWIN-ACTIONS-RESET
      SPAWN-DARWIN-STDIO-ACTIONS
      SPAWN-DARWIN-ZERO-ADESC
      SPAWN-DARWIN-ZERO-ATTR
      SPAWN-DARWIN-FILL-ADESC
      8 >REG SPAWN-DARWIN-PID-PATH
      SPAWN-DARWIN-USE-ADESC
      9 >REG 7 >REG SPAWN-DARWIN-ARGV-ENVP
      BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
      SPAWN-DARWIN-FRAME-LEAVE
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BSPAWNARGVENVCWDIO ( -- )        \ ( pathz argvp envp cwdz stdinfd stdoutfd stderrfd -- pid|-errno )
   BSP-LABELS2
   12 RT:G-POP  11 RT:G-POP  10 RT:G-POP  6 RT:G-POP  7 RT:G-POP  9 RT:G-POP  8 RT:G-POP
   HB-TARGET-LINUX-ARM64? IF
      8 >REG 9 >REG 7 >REG 6 >REG 10 >REG 11 >REG 12 >REG LINUX-SPAWN
      exit
   THEN
   HB-TARGET-MACOS? IF
      SPAWN-DARWIN-FRAME-ENTER
      SPAWN-DARWIN-ACTIONS-RESET
      6 >REG BSP-DN @ >LABEL SPAWN-CHDIR-ACTION
      SPAWN-DARWIN-STDIO-ACTIONS
      SPAWN-DARWIN-ZERO-ADESC
      SPAWN-DARWIN-ZERO-ATTR
      SPAWN-DARWIN-FILL-ADESC
      8 >REG SPAWN-DARWIN-PID-PATH
      SPAWN-DARWIN-USE-ADESC
      9 >REG 7 >REG SPAWN-DARWIN-ARGV-ENVP
      BSP-OK @ >LABEL BSP-DN @ >LABEL SPAWN-DARWIN-FINISH
      SPAWN-DARWIN-FRAME-LEAVE
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

\ spawn-fds on macOS. The frame keeps SPAWN-FRAME's descriptor, attributes and
\ pid offsets; the file-actions blob, three records per slot plus a chdir, is
\ mapped for the call, and its address and size sit where SPAWN-FRAME's own
\ blob starts, followed by the request, which the mmap's SYS, may clobber.
SPAWN-ACTIONS-OFF constant SPAWN-FDS-BLOB-OFF
SPAWN-ACTIONS-OFF 8 + constant SPAWN-FDS-BLOB-U-OFF
SPAWN-ACTIONS-OFF 16 + constant SPAWN-FDS-REQ-OFF
SPAWN-ACTIONS-OFF 32 + constant SPAWN-FDS-FRAME
$4000 constant POSIX-SPAWN-CLOEXEC-DEFAULT
63 constant SPAWN-ENAMETOOLONG

\ spawn-fds on macOS, through the header's steps. The request's address is in
\ x8; x9 becomes the child's pid or -errno.
: DARWIN-SPAWN-FDS ( -- )
   A64ICODE:LBL {: mapped:label :}
   A64ICODE:LBL {: nocwd:label :}
   A64ICODE:LBL {: toolong:label :}
   A64ICODE:LBL {: top:label :}
   A64ICODE:LBL {: below:label :}
   A64ICODE:LBL {: topped:label :}
   A64ICODE:LBL {: lift:label :}
   A64ICODE:LBL {: lskip:label :}
   A64ICODE:LBL {: lifted:label :}
   A64ICODE:LBL {: place:label :}
   A64ICODE:LBL {: pskip:label :}
   A64ICODE:LBL {: placed:label :}
   A64ICODE:LBL {: spawned:label :}
   A64ICODE:LBL {: unmap:label :}
   A64ICODE:LBL {: leave:label :}
   A64ICODE:LBL {: masked:label :}
   A64ICODE:LBL {: unmasked:label :}
   A64ICODE:SP A64ICODE:SP SPAWN-FDS-FRAME A64ICODE:SUBI,
   8 A64ICODE:SP SPAWN-FDS-REQ-OFF A64ICODE:STR,
   6 8 SPAWN-ABI:NFDS A64ICODE:LDR,
   7 6 1 A64ICODE:LSLI,  7 7 6 A64ICODE:ADD,  7 7 1 A64ICODE:ADDI,
   1 SPAWN-ACTION-SIZE A64ICODE:MOVZ,  1 7 1 A64ICODE:MUL,  1 1 SPAWN-FA-ACTS-OFF A64ICODE:ADDI,
   1 A64ICODE:SP SPAWN-FDS-BLOB-U-OFF A64ICODE:STR,
   0 0 A64ICODE:MOVZ,  2 SPAWN-PROT-RW A64ICODE:MOVZ,  3 SYSCALL:MAP-ANON-PRIVATE A64ICODE:MOVZ,  4 0 A64ICODE:MOVN,  5 0 A64ICODE:MOVZ,
   SYSCALL:NR-MMAP SYSCALL:SYS,
   A64ASM:C-CC mapped A64ICODE:BCOND,
      9 31 0 A64ICODE:SUB,  leave A64ICODE:B,
   mapped A64ICODE:LBL,
   8 A64ICODE:SP SPAWN-FDS-REQ-OFF A64ICODE:LDR,
   0 A64ICODE:SP SPAWN-FDS-BLOB-OFF A64ICODE:STR,  13 0 0 A64ICODE:ADDI,
   7 13 SPAWN-FA-CAP-OFF A64ICODE:STRW,
   5 8 SPAWN-ABI:CWD A64ICODE:LDR,  5 nocwd A64ICODE:CBZ,
      5 >REG toolong SPAWN-CHDIR-ACTION
   nocwd A64ICODE:LBL,
   12 6 0 A64ICODE:ADDI,  11 8 SPAWN-ABI:FDS A64ICODE:LDR,  10 6 0 A64ICODE:ADDI,
   top A64ICODE:LBL,
      10 topped A64ICODE:CBZ,
      9 11 0 A64ICODE:LDR,  9 12 A64ICODE:CMP,  A64ASM:C-LT below A64ICODE:BCOND,
      12 9 1 A64ICODE:ADDI,
   below A64ICODE:LBL,
      11 11 8 A64ICODE:ADDI,  10 10 1 A64ICODE:SUBI,
      top A64ICODE:B,
   topped A64ICODE:LBL,
   10 0 A64ICODE:MOVZ,  11 8 SPAWN-ABI:FDS A64ICODE:LDR,
   lift A64ICODE:LBL,
      10 6 A64ICODE:CMP,  A64ASM:C-GE lifted A64ICODE:BCOND,
      9 11 0 A64ICODE:LDR,  9 0 A64ICODE:CMPI,  A64ASM:C-LT lskip A64ICODE:BCOND,
      SPAWN-NEXT-ACTION
      15 PSFA-DUP2 A64ICODE:MOVZ,  15 14 0 A64ICODE:STRW,  9 14 4 A64ICODE:STRW,
      15 12 10 A64ICODE:ADD,  15 14 8 A64ICODE:STRW,
      SPAWN-COUNT-ACTION
   lskip A64ICODE:LBL,
      11 11 8 A64ICODE:ADDI,  10 10 1 A64ICODE:ADDI,
      lift A64ICODE:B,
   lifted A64ICODE:LBL,
   10 0 A64ICODE:MOVZ,  11 8 SPAWN-ABI:FDS A64ICODE:LDR,
   place A64ICODE:LBL,
      10 6 A64ICODE:CMP,  A64ASM:C-GE placed A64ICODE:BCOND,
      9 11 0 A64ICODE:LDR,  9 0 A64ICODE:CMPI,  A64ASM:C-LT pskip A64ICODE:BCOND,
      SPAWN-NEXT-ACTION
      15 PSFA-DUP2 A64ICODE:MOVZ,  15 14 0 A64ICODE:STRW,
      15 12 10 A64ICODE:ADD,  15 14 4 A64ICODE:STRW,  10 14 8 A64ICODE:STRW,
      SPAWN-COUNT-ACTION
      SPAWN-NEXT-ACTION
      15 PSFA-CLOSE A64ICODE:MOVZ,  15 14 0 A64ICODE:STRW,
      15 12 10 A64ICODE:ADD,  15 14 4 A64ICODE:STRW,
      SPAWN-COUNT-ACTION
   pskip A64ICODE:LBL,
      11 11 8 A64ICODE:ADDI,  10 10 1 A64ICODE:ADDI,
      place A64ICODE:B,
   placed A64ICODE:LBL,
   SPAWN-DARWIN-ZERO-ADESC
   SPAWN-DARWIN-ZERO-ATTR
   SPAWN-DARWIN-FILL-ADESC
   14 POSIX-SPAWN-SETPGROUP POSIX-SPAWN-CLOEXEC-DEFAULT or A64ICODE:MOVZ,
   14 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-FLAGS-OFF + A64ICODE:STRW,
   14 8 SPAWN-ABI:PGROUP A64ICODE:LDR,
   14 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-PGROUP-OFF + A64ICODE:STRW,
   14 8 SPAWN-ABI:BLOCKED A64ICODE:LDR,  14 unmasked A64ICODE:CBZ,
      0 DARWIN-SIG-BLOCK A64ICODE:MOVZ,  1 0 A64ICODE:MOVZ,
      2 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-SIGMASK-OFF + A64ICODE:ADDI,
      SYSCALL:NR-RT-SIGPROCMASK SYSCALL:SYS,
      A64ASM:C-CC masked A64ICODE:BCOND,
         9 31 0 A64ICODE:SUB,  unmap A64ICODE:B,
      masked A64ICODE:LBL,
      8 A64ICODE:SP SPAWN-FDS-REQ-OFF A64ICODE:LDR,
      14 8 SPAWN-ABI:BLOCKED A64ICODE:LDR,
      15 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-SIGMASK-OFF + A64ICODE:LDRW,
      15 15 14 A64ICODE:ORR,
      15 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-SIGMASK-OFF + A64ICODE:STRW,
      14 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-FLAGS-OFF + A64ICODE:LDRW,
      15 POSIX-SPAWN-SETSIGMASK A64ICODE:MOVZ,  14 14 15 A64ICODE:ORR,
      14 A64ICODE:SP SPAWN-ATTR-OFF SPAWN-ATTR-FLAGS-OFF + A64ICODE:STRW,
   unmasked A64ICODE:LBL,
   0 A64ICODE:SP SPAWN-PID-OFF A64ICODE:ADDI,  1 8 SPAWN-ABI:PATH A64ICODE:LDR,  2 A64ICODE:SP SPAWN-ADESC-OFF A64ICODE:ADDI,
   3 8 SPAWN-ABI:ARGV A64ICODE:LDR,  4 8 SPAWN-ABI:ENVP A64ICODE:LDR,
   SYSCALL:NR-SPAWN SYSCALL:SYS,
   A64ASM:C-CC spawned A64ICODE:BCOND,
      9 31 0 A64ICODE:SUB,  unmap A64ICODE:B,
   spawned A64ICODE:LBL,
   9 A64ICODE:SP SPAWN-PID-OFF A64ICODE:LDRW,  unmap A64ICODE:B,
   toolong A64ICODE:LBL,
   9 SPAWN-ENAMETOOLONG 1- A64ICODE:MOVN,
   unmap A64ICODE:LBL,
   0 A64ICODE:SP SPAWN-FDS-BLOB-OFF A64ICODE:LDR,  1 A64ICODE:SP SPAWN-FDS-BLOB-U-OFF A64ICODE:LDR,  SYSCALL:NR-MUNMAP SYSCALL:SYS,
   leave A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP SPAWN-FDS-FRAME A64ICODE:ADDI, ;

\ spawn-fds ( request -- pid|-errno ). A negative NFDS is -EINVAL.
: BSPAWNFDS ( -- )
   A64ICODE:LBL {: valid:label :}
   A64ICODE:LBL {: push:label :}
   HB-TARGET-LINUX-ARM64? HB-TARGET-MACOS? or 0= IF ENGINE-EMIT:TARGET-UNKNOWN THEN
   10 RT:G-POP
   11 10 SPAWN-ABI:NFDS A64ICODE:LDR,  11 0 A64ICODE:CMPI,  A64ASM:C-GE valid A64ICODE:BCOND,
      9 SPAWN-EINVAL 1- A64ICODE:MOVN,  push A64ICODE:B,
   valid A64ICODE:LBL,
   HB-TARGET-LINUX-ARM64? IF LINUX-SPAWN-FDS THEN
   HB-TARGET-MACOS? IF 8 10 0 A64ICODE:ADDI,  DARWIN-SPAWN-FDS THEN
   push A64ICODE:LBL,
   9 RT:G-PUSH ;

: BCPFETCH ( -- ) 9 CP 0 A64ICODE:ADDI,  A RT:G-PUSH ;     \ ( -- addr ) live CP (snapshot writer)
: BNDICTFETCH ( -- ) 9 NDICT 0 A64ICODE:ADDI,  A RT:G-PUSH ;  \ ( -- n ) live dict count
: BDBASEFETCH ( -- ) 9 DBASE 0 A64ICODE:ADDI,  A RT:G-PUSH ;  \ ( -- addr ) region base
: BDATAFETCH ( -- ) 9 XREG-RBASE 0 A64ICODE:ADDI,  A RT:G-PUSH ;   \ ( -- addr ) live DATA base
: BNULLPTR ( -- ) 31 RT:G-PUSH ;   \ ( -- 0 ) the null: register 31 as a store source is xzr
: BCHECKFETCH ( -- ) 9 XREG-RBASE HOOK-CELL A64ICODE:LDR,  A RT:G-PUSH ;  \ ( -- xt ) live checker hook — getter for set-check ([x20/XREG-RBASE + HOOK-CELL])

private
\ x3 = the lr, copied before FAULT-OPEN,'s calls replace x30.
: GUARD-RECORD, ( str str str str -- )
   {: c:str p:str r:str h:str :}
   3 30 0 A64ICODE:ADDI,
   c  p r FAULT-RECORD:GUARD-MSG$  CRASH:FAULT-OPEN,
   4 DBASE 0 A64ICODE:ADDI,  6 NDICT 0 A64ICODE:ADDI,  CRASH:FAULT-IN,
   h EXIT-STATUS:TASK-COMPILE CRASH:FAULT-STOP, ;

public
\ The main thread is the compiler: only the thread whose DATA is the main
\ region, TASK-TCB-CELL 0, defines, allots, evaluates or changes package scope.
\ A task and a callback thread hold a TCB there, and B-TASK-GUARD exits on
\ one. It takes the code and the primitive's name and prints a record naming
\ the primitive, its note `in` the word whose code holds the lr, so it runs
\ before the body's first BL. Clobbers x9.
: B-TASK-GUARD ( str str -- )
   {: c:str p:str :}
   A64ICODE:LBL {: ok:label :}
   9 XREG-RBASE TASK-TCB-CELL A64ICODE:LDR,  9 ok A64ICODE:CBZ,
      c p  FAULT-RECORD:TASK-GUARD$  FAULT-RECORD:TASK-GUARD-HELP$
      GUARD-RECORD,
   ok A64ICODE:LBL, ;
;package

\ An open checker overlay restores NDICT, CP and WIDN at its close or rollback,
\ so every writer but its own runs GUARD, first and is rejected. Clobbers x9.
package OVERLAY-EMIT
public
variable LDIE
: GUARD, ( -- )
   A64ICODE:LBL {: ok:label :}
   9 REPLAY-SCOPE:LATCH A64ICODE:LIT64,  9 XREG-RBASE 9 A64ICODE:ADD,  9 9 0 A64ICODE:LDR,  9 ok A64ICODE:CBZ,
      LDIE A64ICODE:LABEL@ A64ICODE:B,
   ok A64ICODE:LBL, ;
;package

package ENGINE-EMIT
\ cp! and ndict! lower CP and NDICT. A caller lowers only with no task live and
\ retires instead while one is, except a token's CP rewind in a pending body.
: BCPSET ( -- )
   A RT:G-POP
   EM-DEF-OCC:LRECLAIM A64ICODE:LABEL@ A64ICODE:BL,  CP A 0 A64ICODE:ADDI, ;   \ ( addr -- ) set CP
\ ndict! calls this after lowering NDICT: a LASTC-CELL record (does>'s target) at or
\ above the count is cleared, so does> rejects by name. Clobbers both scratch registers.
private
: LASTC-TRIM, ( n n -- ) {: t:n u:n :}
   A64ICODE:LBL {: live:label :}
   t DREC A64ICODE:MOVZ,  t NDICT t A64ICODE:MUL,  t DBASE t A64ICODE:ADD,        \ the first record past the count
   u XREG-RBASE LASTC-CELL A64ICODE:LDR,  u t A64ICODE:CMP,  A64ASM:C-CC live A64ICODE:BCOND,
   u 0 A64ICODE:MOVZ,  u XREG-RBASE LASTC-CELL A64ICODE:STR,
   live A64ICODE:LBL, ;

: BNDSET ( -- )                                     \ ( n -- ) set NDICT — forget dict entries past a mark
   A RT:G-POP
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,
   EM-DEF-OCC:LCOUNT A64ICODE:LABEL@ A64ICODE:BL,
   A B LASTC-TRIM,
   30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI, ;

\ Append the completed native pending record at the caller's expected index.
\ NPUB proves the emission and record shape; this sink owns the live count and
\ index and checks pending ownership before changing either. DOES> publishes
\ its parent, then its one prepared companion, which CLAUSE-CELL admits; no
\ other row is eligible.
: BNDAPPEND ( -- )                                  \ ( expected-index -- )
   s" E-TASK-COMPILE" s" ndict-append" B-TASK-GUARD  A RT:G-POP
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: owner:label bad:label done:label :}
   A NDICT A64ICODE:CMP,  A64ASM:C-NE bad A64ICODE:BCOND,
   14 DICT-CAP A64ICODE:LIT64,  A 14 A64ICODE:CMP,  A64ASM:C-CS bad A64ICODE:BCOND,
   14 XREG-RBASE NCOMP-DISPATCH:COMP-LATCH-CELL A64ICODE:LDR,  14 bad A64ICODE:CBZ,
   C DREC A64ICODE:MOVZ,  B A C A64ICODE:MUL,  B DBASE B A64ICODE:ADD,
   14 XREG-RBASE PEND-CELL A64ICODE:LDR,  14 bad A64ICODE:CBZ,
   14 B A64ICODE:CMP,  A64ASM:C-EQ owner A64ICODE:BCOND,
   14 14 DREC A64ICODE:ADDI,  14 B A64ICODE:CMP,  A64ASM:C-NE bad A64ICODE:BCOND,
   14 XREG-RBASE CLAUSE-CELL A64ICODE:LDR,  14 bad A64ICODE:CBZ,
   owner A64ICODE:LBL,
   EM-DEF-OCC:LAPPEND A64ICODE:LABEL@ A64ICODE:BL,
   LHIDXADD A64ICODE:LABEL@ A64ICODE:BL,
   done A64ICODE:B,
   bad A64ICODE:LBL,  0 CRASH:CALLER-LR,
   s" E-SEAL-VIOLATION habu: ndict-append index not NDICT and the pending native record, or at or past DICT-CAP:"
   EXIT-STATUS:SEAL-VIOLATION A CRASH:DIE-LINE-VALUE,
   done A64ICODE:LBL, ;

: BEPOCHSECONDS ( -- )
   A64ICODE:LBL TIME-OK !
   0 XREG-RBASE GTOD-SCRATCH A64ICODE:ADDI,  1 0 A64ICODE:MOVZ,  2 0 A64ICODE:MOVZ,  SYSCALL:NR-GETTIMEOFDAY SYSCALL:SYS,
   9 A64ASM:C-CS A64ICODE:CSET,  9 9 0 A64ICODE:ORR,  9 0 A64ICODE:CMPI,  A64ASM:C-EQ TIME-OK A64ICODE:LABEL@ A64ICODE:BCOND,  A64ICODE:BRK,
   TIME-OK A64ICODE:LABEL@ A64ICODE:LBL,
   9 XREG-RBASE GTOD-SCRATCH A64ICODE:LDR,  9 RT:G-PUSH ;

\ Monotonic nanoseconds on the clock the OS's timers expire on; quotient and
\ remainder keep the ticks * 10^9 multiply from overflowing.
: BMONONS ( -- )
   A64ICODE:LBL TIME-OK !
   9 SYSCALL:OS-TIMER-NOW  10 SYSCALL:OS-TIMER-HZ
   10 TIME-OK A64ICODE:LABEL@ A64ICODE:CBNZ,  A64ICODE:BRK,  TIME-OK A64ICODE:LABEL@ A64ICODE:LBL,
   11 9 10 A64ICODE:UDIV,                            \ q = ticks / freq
   12 11 10 A64ICODE:MUL,  9 9 12 A64ICODE:SUB,               \ r = ticks % freq
   13 $3B9ACA00 A64ICODE:LIT64,                      \ 1_000_000_000 ns/s
   11 11 13 A64ICODE:MUL,
   9 9 13 A64ICODE:MUL,  9 9 10 A64ICODE:UDIV,
   9 11 9 A64ICODE:ADD,  9 RT:G-PUSH ;
;package

\ The source checker's operations and its record mark.
package DECL-OWNER
public

\ x11 = the operation at off in the source owner's record, or 0 with no record.
: FIND ( n -- ) {: off:n :}
   A64ICODE:LBL {: done:label :}
   11 XREG-RBASE NCOMP-DISPATCH:DECL-CELL A64ICODE:LDR,  11 done A64ICODE:CBZ,
   11 11 off A64ICODE:LDR,
   done A64ICODE:LBL, ;

\ x11 = the operation at off in the target owner's record; no record, or a 0
\ operation, branches to absent. The active native compiler retains its
\ checker even while a build replaces the source dictionary: a distinct target
\ checker owns this second record during source replacement.
: TARGET ( n label -- ) {: off:n absent:label :}
   11 XREG-RBASE NCOMP-DISPATCH:TARGET-DECL-CELL A64ICODE:LDR,  11 absent A64ICODE:CBZ,
   11 11 off A64ICODE:LDR,  11 absent A64ICODE:CBZ, ;
;package

package ENGINE-EMIT
public

private
\ B-EVAL and the bodies after it jump, their arguments on the data stack,
\ through their PROVIDED-XT cell.
: B-EVAL ( -- )
   s" E-TASK-COMPILE" s" evaluate" B-TASK-GUARD
   9 XREG-RBASE PROVIDED-XT:EVALUATE-CELL A64ICODE:LDR,  9 A64ICODE:BR, ;

: B-EVAL-RANGE ( -- )
   s" E-TASK-COMPILE" s" evaluate-range" B-TASK-GUARD
   9 XREG-RBASE PROVIDED-XT:EVALUATE-RANGE-CELL A64ICODE:LDR,  9 A64ICODE:BR, ;

: B-LITERAL ( -- )
   s" E-TASK-COMPILE" s" literal" B-TASK-GUARD
   9 XREG-RBASE PROVIDED-XT:LITERAL-CELL A64ICODE:LDR,  9 A64ICODE:BR, ;

: B-COMPILE ( -- )
   s" E-TASK-COMPILE" s" compile," B-TASK-GUARD
   9 XREG-RBASE PROVIDED-XT:COMPILE-CELL A64ICODE:LDR,  9 A64ICODE:BR, ;

: B-END ( -- )
   s" E-TASK-COMPILE" s" ;" B-TASK-GUARD
   9 XREG-RBASE PROVIDED-XT:END-CELL A64ICODE:LDR,  9 A64ICODE:BR, ;

: BCREATE ( -- )
   s" E-TASK-COMPILE" s" create" B-TASK-GUARD
   15 0 A64ICODE:MOVZ,  16 20 CREATEP-CELL A64ICODE:LDR,  16 A64ICODE:BLR, ;   \ ( "name" -- ) runtime CREATE via the
                                     \ startup-stored cell: subsets emit prims w/o labels;
                                     \ LCREATE publishes the word's `-- ^a` in both modes

: BEMIT ( -- )
   A RT:G-POP  13 9 0 A64ICODE:ADDI,  RT:G-EMITC ;

public
: BCR ( -- )
   13 10 A64ICODE:MOVZ,  RT:G-EMITC ;

private
: BSPACE ( -- )
   13 32 A64ICODE:MOVZ,  RT:G-EMITC ;

: B.S ( -- )
   A64ICODE:LBL PS-LOOP !
   A64ICODE:LBL PS-DONE !
   9 XREG-RBASE S0-CELL A64ICODE:LDR,  9 XREG-RBASE SSCR-CELL A64ICODE:STR,
   PS-LOOP A64ICODE:LABEL@ A64ICODE:LBL,
      9 XREG-RBASE SSCR-CELL A64ICODE:LDR,  9 A64ICODE:XDS A64ICODE:CMP,  A64ASM:C-CS PS-DONE A64ICODE:LABEL@ A64ICODE:BCOND,
      9 9 0 A64ICODE:LDR,  RT:G-PRINT9
      9 XREG-RBASE SSCR-CELL A64ICODE:LDR,  9 9 8 A64ICODE:ADDI,  9 XREG-RBASE SSCR-CELL A64ICODE:STR,
      PS-LOOP A64ICODE:LABEL@ A64ICODE:B,
   PS-DONE A64ICODE:LABEL@ A64ICODE:LBL, ;

: BDEPTH ( -- )
   A XREG-RBASE S0-CELL A64ICODE:LDR,
   A A64ICODE:XDS A A64ICODE:SUB,
   A A 3 A64ICODE:LSRI,
   A RT:G-PUSH ;

: (CMP) ( n -- )
   CMP-COND !
   B RT:G-POP  A RT:G-POP  A B A64ICODE:CMP,  A CMP-COND @ A64ICODE:CSET,  A A64ICODE:SP A A64ICODE:SUB,  A RT:G-PUSH ;

: B= ( -- )
   A64ASM:C-EQ (CMP) ;

: B<> ( -- )
   A64ASM:C-NE (CMP) ;

: B< ( -- )
   A64ASM:C-LT (CMP) ;

: B> ( -- )
   A64ASM:C-GT (CMP) ;

: B<= ( -- )
   A64ASM:C-LE (CMP) ;

: B>= ( -- )
   A64ASM:C-GE (CMP) ;

: B0= ( -- )
   A RT:G-POP  A 0 A64ICODE:CMPI,  A A64ASM:C-EQ A64ICODE:CSET,  A A64ICODE:SP A A64ICODE:SUB,  A RT:G-PUSH ;

: B0< ( -- )
   A RT:G-POP  A 0 A64ICODE:CMPI,  A A64ASM:C-LT A64ICODE:CSET,  A A64ICODE:SP A A64ICODE:SUB,  A RT:G-PUSH ;

: B1+ ( -- )
   A RT:G-POP  A A 1 A64ICODE:ADDI,  A RT:G-PUSH ;

: B1- ( -- )
   A RT:G-POP  A A 1 A64ICODE:SUBI,  A RT:G-PUSH ;

: BAND ( -- )
   B RT:G-POP A RT:G-POP  A A B A64ICODE:AND, A RT:G-PUSH ;

: BOR ( -- )
   B RT:G-POP A RT:G-POP  A A B A64ICODE:ORR, A RT:G-PUSH ;

: BXOR ( -- )
   B RT:G-POP A RT:G-POP  A A B A64ICODE:EOR, A RT:G-PUSH ;

: BINV ( -- )
   A RT:G-POP  B 0 A64ICODE:MOVN,  A A B A64ICODE:EOR,  A RT:G-PUSH ;

: BNEG ( -- )
   A RT:G-POP  A A64ICODE:SP A A64ICODE:SUB,  A RT:G-PUSH ;

: BLSH ( -- )
   B RT:G-POP A RT:G-POP  A A B A64ICODE:LSLV, A RT:G-PUSH ;

: BRSH ( -- )
   B RT:G-POP A RT:G-POP  A A B A64ICODE:LSRV, A RT:G-PUSH ;

\ arm64 SDIV by zero answers 0.
\
\ MIN-N -1 / is MIN-N, SDIV's modular answer, like the wrap of `+`, `-` and
\ `*`; a guard for it would cost every division a compare no caller in this
\ tree can fail.
: BDIV ( -- )
   B RT:G-POP A RT:G-POP  A A B A64ICODE:SDIV, A RT:G-PUSH ;

: BMOD ( -- )
   B RT:G-POP A RT:G-POP  C A B A64ICODE:SDIV,  C C B A64ICODE:MUL,  A A C A64ICODE:SUB,  A RT:G-PUSH ;

: BDIVMOD ( -- )
   B RT:G-POP A RT:G-POP  C A B A64ICODE:SDIV,  DREG C B A64ICODE:MUL,  A A DREG A64ICODE:SUB,  A RT:G-PUSH C RT:G-PUSH ;

: BABS ( -- )
   A64ICODE:LBL PRIM-DONE !
   A RT:G-POP  A 0 A64ICODE:CMPI,  A64ASM:C-GE PRIM-DONE A64ICODE:LABEL@ A64ICODE:BCOND,  A A64ICODE:SP A A64ICODE:SUB,  PRIM-DONE A64ICODE:LABEL@ A64ICODE:LBL,  A RT:G-PUSH ;

: BMIN ( -- )
   A64ICODE:LBL PRIM-DONE !
   B RT:G-POP A RT:G-POP  A B A64ICODE:CMP,  A64ASM:C-LE PRIM-DONE A64ICODE:LABEL@ A64ICODE:BCOND,  A B 0 A64ICODE:ADDI,  PRIM-DONE A64ICODE:LABEL@ A64ICODE:LBL,  A RT:G-PUSH ;

: BMAX ( -- )
   A64ICODE:LBL PRIM-DONE !
   B RT:G-POP A RT:G-POP  A B A64ICODE:CMP,  A64ASM:C-GE PRIM-DONE A64ICODE:LABEL@ A64ICODE:BCOND,  A B 0 A64ICODE:ADDI,  PRIM-DONE A64ICODE:LABEL@ A64ICODE:LBL,  A RT:G-PUSH ;

: BNIP ( -- )
   A RT:G-POP  A64ICODE:XDS A64ICODE:XDS 8 A64ICODE:SUBI,  A RT:G-PUSH ;

: BOVER ( -- )
   B RT:G-POP A RT:G-POP  A RT:G-PUSH B RT:G-PUSH A RT:G-PUSH ;

: BTUCK ( -- )
   B RT:G-POP A RT:G-POP  B RT:G-PUSH A RT:G-PUSH B RT:G-PUSH ;

: BROT ( -- )
   C RT:G-POP B RT:G-POP A RT:G-POP  B RT:G-PUSH C RT:G-PUSH A RT:G-PUSH ;

: BMROT ( -- )
   C RT:G-POP B RT:G-POP A RT:G-POP  C RT:G-PUSH A RT:G-PUSH B RT:G-PUSH ;

: B2DUP ( -- )
   B RT:G-POP A RT:G-POP  A RT:G-PUSH B RT:G-PUSH A RT:G-PUSH B RT:G-PUSH ;

: B2DROP ( -- )
   A64ICODE:XDS A64ICODE:XDS 16 A64ICODE:SUBI, ;

: B2SWAP ( -- )
   EREG RT:G-POP DREG RT:G-POP C RT:G-POP A RT:G-POP  DREG RT:G-PUSH EREG RT:G-PUSH A RT:G-PUSH C RT:G-PUSH ;

: B2OVER ( -- )
   EREG RT:G-POP DREG RT:G-POP C RT:G-POP A RT:G-POP  A RT:G-PUSH C RT:G-PUSH DREG RT:G-PUSH EREG RT:G-PUSH A RT:G-PUSH C RT:G-PUSH ;

: BFETCH ( -- )
   A RT:G-POP  A A 0 A64ICODE:LDR,  A RT:G-PUSH ;

public
: BSTORE ( -- )
   B RT:G-POP A RT:G-POP  A B 0 A64ICODE:STR, ;

private
: BPTRFIELD ( -- )
   B RT:G-POP  A RT:G-POP  B B 3 A64ICODE:LSLI,  A A B A64ICODE:ADD,  A RT:G-PUSH ;

: BPLUSSTORE ( -- )
   B RT:G-POP A RT:G-POP  C B 0 A64ICODE:LDR,  C C A A64ICODE:ADD,  C B 0 A64ICODE:STR, ;

: BCFETCH ( -- )
   A RT:G-POP  A A 0 A64ICODE:LDRB, A RT:G-PUSH ;

: BCSTORE ( -- )
   B RT:G-POP A RT:G-POP  A B 0 A64ICODE:STRB, ;

: BHFETCH ( -- )
   A RT:G-POP  A A 0 A64ICODE:LDRH, A RT:G-PUSH ;

: BHSTORE ( -- )
   B RT:G-POP A RT:G-POP  A B 0 A64ICODE:STRH, ;

: BWFETCH ( -- )
   A RT:G-POP  A A 0 A64ICODE:LDRW, A RT:G-PUSH ;

: BWSTORE ( -- )
   B RT:G-POP A RT:G-POP  A B 0 A64ICODE:STRW, ;

\ Atomic primitives (ARMv8.1 LSE). A=x9 B=x10 C=x11.
: BATFETCH ( -- )   \ atomic@ ( ^a -- a ) : LDAR x9,[x9]
   A RT:G-POP  A A A64ICODE:LDAR,  A RT:G-PUSH ;
: BATSTORE ( -- )   \ atomic! ( a ^a -- ) : STLR x9,[x10]
   B RT:G-POP A RT:G-POP  A B A64ICODE:STLR, ;
: BATFETCH32 ( -- )   \ atomic-u32@ ( ^u8 -- n ) : LDAR w9,[x9], zero-extended
   A RT:G-POP  A A A64ICODE:LDARW,  A RT:G-PUSH ;
: BATSTORE32 ( -- )   \ atomic-u32! ( n ^u8 -- ) : STLR w9,[x10]
   B RT:G-POP A RT:G-POP  A B A64ICODE:STLRW, ;
: BATADD ( -- )     \ atomic-add ( delta addr -- old ) : LDADDAL x9,x9,[x10]
   B RT:G-POP A RT:G-POP  A A B A64ICODE:LDADDAL,  A RT:G-PUSH ;
: BATCAS ( -- )     \ atomic-cas ( expected new addr -- actual ) : CASAL x9,x10,[x11]
   C RT:G-POP B RT:G-POP A RT:G-POP
   A B C A64ICODE:CASAL,  A RT:G-PUSH ;
: BFENCE ( -- )     \ fence ( -- ) : DMB ISH
   A64ICODE:DMB-ISH, ;

: BCELLS ( -- )
   A RT:G-POP  A A 3 A64ICODE:LSLI, A RT:G-PUSH ;

: BCELLPLUS ( -- )
   A RT:G-POP  A A 8 A64ICODE:ADDI, A RT:G-PUSH ;

: BCHARS ( -- ) ;

\ Memory views, PTR>N, >str and str> change only the checked type; the machine
\ cells are unchanged.
: BADDRESSVIEW ( -- ) ;

: BCHARPLUS ( -- )
   A RT:G-POP  A A 1 A64ICODE:ADDI, A RT:G-PUSH ;

: BCOUNT ( -- )
   A RT:G-POP  B A 0 A64ICODE:LDRB,  A A 1 A64ICODE:ADDI,  A RT:G-PUSH  B RT:G-PUSH ;

\ The return stack is a guarded mapping. The depth is stored before x14 is
\ reloaded with the base, so two scratch registers suffice. A push past
\ RETURN-CELLS faults on the mapping's guard page; there is no capacity compare.
: RSTK-PUSH ( n -- )
   RSTK-REG !
   14 XREG-RBASE RSP-CELL A64ICODE:LDR,
   15 14 3 A64ICODE:LSLI,
   14 14 1 A64ICODE:ADDI,  14 XREG-RBASE RSP-CELL A64ICODE:STR,
   14 XREG-RBASE STACK-ABI:RETURN-BASE-CELL A64ICODE:LDR,  15 14 15 A64ICODE:ADD,
   RSTK-REG @ 15 0 A64ICODE:STR, ;

: RSTK-POP ( n -- )
   RSTK-REG !
   14 XREG-RBASE RSP-CELL A64ICODE:LDR,
   14 14 1 A64ICODE:SUBI,
   15 14 3 A64ICODE:LSLI,
   14 XREG-RBASE RSP-CELL A64ICODE:STR,
   14 XREG-RBASE STACK-ABI:RETURN-BASE-CELL A64ICODE:LDR,  15 14 15 A64ICODE:ADD,
   RSTK-REG @ 15 0 A64ICODE:LDR, ;

: B2TOR ( -- )
   B RT:G-POP A RT:G-POP  A RSTK-PUSH  B RSTK-PUSH ;

: B2RFROM ( -- )
   B RSTK-POP  A RSTK-POP  A RT:G-PUSH  B RT:G-PUSH ;

: B2RFETCH ( -- )
   B RSTK-POP  A RSTK-POP  A RSTK-PUSH  B RSTK-PUSH  A RT:G-PUSH  B RT:G-PUSH ;

: BHERE ( -- )
   7 XREG-RBASE 0 A64ICODE:LDR,  7 RT:G-PUSH ;

public
\ Bound the DP heap before any allot , c, or definer stores a new DP: outside
\ it, LDPBAD throws E-DATA-SPACE.
: DP-CHECK ( n -- )
   DP-REG !
   A64ICODE:LBL DP-LOW !
   A64ICODE:LBL DP-HIGH !
   5 XREG-RBASE BOOT:DP-FLOOR-CELL A64ICODE:LDR,
   DP-REG @ 5 A64ICODE:CMP,  A64ASM:C-GE DP-LOW A64ICODE:LABEL@ A64ICODE:BCOND,
      LDPBAD A64ICODE:LABEL@ A64ICODE:B,
   DP-LOW A64ICODE:LABEL@ A64ICODE:LBL,
   5 XREG-RBASE BOOT:DP-CEILING-CELL A64ICODE:LDR,
   DP-REG @ 5 A64ICODE:CMP,  A64ASM:C-LE DP-HIGH A64ICODE:LABEL@ A64ICODE:BCOND,
      LDPBAD A64ICODE:LABEL@ A64ICODE:B,
   DP-HIGH A64ICODE:LABEL@ A64ICODE:LBL, ;

private
\ Before DP becomes register new: ends the `,` run and raises the release
\ high-water to the DP when that is above new. Clobbers both scratch registers.
: DP-LOWER, ( n n n -- )
   {: new:n t:n u:n :}
   A64ICODE:LBL {: done:label :}
   31 XREG-RBASE COMMA-RUN-CELL A64ICODE:STR,
   t XREG-RBASE DP-CELL A64ICODE:LDR,  t new A64ICODE:CMP,  A64ASM:C-LS done A64ICODE:BCOND,
   t t XREG-RBASE A64ICODE:SUB,
   u XREG-RBASE DPHW-CELL A64ICODE:LDR,  t u A64ICODE:CMP,  A64ASM:C-LS done A64ICODE:BCOND,
   t XREG-RBASE DPHW-CELL A64ICODE:STR,
   done A64ICODE:LBL, ;

: BALIGN ( -- )
   s" E-TASK-COMPILE" s" align" B-TASK-GUARD
   7 XREG-RBASE 0 A64ICODE:LDR,  7 7 7 A64ICODE:ADDI,  7 7 3 A64ICODE:LSRI,  7 7 3 A64ICODE:LSLI,  7 DP-CHECK  7 XREG-RBASE 0 A64ICODE:STR, ;

\ `,` keeps the latest run of its cells that starts at a created body: a cell
\ written at the body start starts a run, even where the last run ended; one
\ written at the run's end extends it.
: BCOMMA ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: more:label grow:label done:label :}
   s" E-TASK-COMPILE" s" ," B-TASK-GUARD
   A RT:G-POP  7 XREG-RBASE 0 A64ICODE:LDR,  C 7 8 A64ICODE:ADDI,  C DP-CHECK  A 7 0 A64ICODE:STR,  C XREG-RBASE 0 A64ICODE:STR,
   A XREG-RBASE COMMA-BASE-CELL A64ICODE:LDR,  A 7 A64ICODE:CMP,  A64ASM:C-NE more A64ICODE:BCOND,
   7 XREG-RBASE COMMA-RUN-CELL A64ICODE:STR,  grow A64ICODE:B,
   more A64ICODE:LBL,  A XREG-RBASE COMMA-END-CELL A64ICODE:LDR,  A 7 A64ICODE:CMP,  A64ASM:C-NE done A64ICODE:BCOND,
   grow A64ICODE:LBL,  C XREG-RBASE COMMA-END-CELL A64ICODE:STR,
   done A64ICODE:LBL, ;

: BCCOMMA ( -- )
   s" E-TASK-COMPILE" s" c," B-TASK-GUARD
   A RT:G-POP  7 XREG-RBASE 0 A64ICODE:LDR,  C 7 1 A64ICODE:ADDI,  C DP-CHECK  A 7 0 A64ICODE:STRB, C XREG-RBASE 0 A64ICODE:STR, ;

public
: BTYPE ( -- )
   2 RT:G-POP  1 RT:G-POP  RT:G-OUT ;

private
10 constant DIE-NL

\ An empty span writes nothing, so a site whose report already ends in a
\ newline passes `s" "`. Leaves the rc in x0 and x7.
: DIE-TEXT, ( -- )
   A64ICODE:LBL {: quiet:label :}
   7 RT:G-POP  2 RT:G-POP  1 RT:G-POP
   2 quiet A64ICODE:CBZ,
      0 2 A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
      A64ICODE:SP A64ICODE:SP $10 A64ICODE:SUBI,  A DIE-NL A64ICODE:MOVZ,  A A64ICODE:SP 0 A64ICODE:STRB,
      0 2 A64ICODE:MOVZ,  1 A64ICODE:SP 0 A64ICODE:ADDI,  2 1 A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
      A64ICODE:SP A64ICODE:SP $10 A64ICODE:ADDI,
   quiet A64ICODE:LBL,
   0 7 0 A64ICODE:ADDI, ;

\ exit_group with the rc in x0 and x7. An rc outside [0,255] maps to
\ EXIT-STATUS:UNCAUGHT: the kernel masks rc & 0xFF, and a negative throw code
\ reused as rc could exit 0.
: DIE-EXIT, ( -- )
   A64ICODE:LBL {: lfixed:label :}
   7 0 A64ICODE:CMPI,    A64ASM:C-LT lfixed A64ICODE:BCOND,
   7 255 A64ICODE:CMPI,  A64ASM:C-GT lfixed A64ICODE:BCOND,
   SYSCALL:NR-EXIT-GROUP SYSCALL:SYS,
   lfixed A64ICODE:LBL,  0 EXIT-STATUS:UNCAUGHT A64ICODE:MOVZ,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS, ;

: BDIE ( -- )
   DIE-TEXT,
   \ The trampoline preserves x0; x7 is re-derived because the hook is compiled
   \ code owning every caller-saved register. BDIE never returns, so LR is free.
   LEXITHOOK A64ICODE:LABEL@ A64ICODE:BL,  7 0 0 A64ICODE:ADDI,
   DIE-EXIT, ;

\ die-closed ends the process at once: no exit action runs, and no other
\ thread's exit holds it.
: BDIECLOSED ( -- )
   DIE-TEXT,  DIE-EXIT, ;

: SYS-PUSH ( -- )
   A64ICODE:LBL SYS-OK !
   A64ICODE:LBL SYS-DONE !
   9 A64ASM:C-CS A64ICODE:CSET,  9 SYS-OK A64ICODE:LABEL@ A64ICODE:CBZ,
      0 0 A64ICODE:MOVN,  SYS-DONE A64ICODE:LABEL@ A64ICODE:B,
   SYS-OK A64ICODE:LABEL@ A64ICODE:LBL,
   SYS-DONE A64ICODE:LABEL@ A64ICODE:LBL,
   0 RT:G-PUSH ;

\ x9 holds the carry sampled immediately after a filesystem syscall. Linux
\ leaves -errno in x0; Darwin leaves positive errno there.
: FS-SYS-PUSH-FLAG ( -- )
   A64ICODE:LBL A64ICODE:LBL {: ok:label done:label :}
   9 ok A64ICODE:CBZ,
      HB-TARGET-LINUX-ARM64? IF 10 31 0 A64ICODE:SUB, ELSE
         HB-TARGET-MACOS? IF 10 0 0 A64ICODE:ADDI, ELSE ENGINE-EMIT:TARGET-UNKNOWN THEN
      THEN
      10 XREG-RBASE FS-ABI:ERRNO-OFF A64ICODE:STR,
      0 0 A64ICODE:MOVN, done A64ICODE:B,
   ok A64ICODE:LBL,
   10 0 A64ICODE:MOVZ,  10 XREG-RBASE FS-ABI:ERRNO-OFF A64ICODE:STR,
   done A64ICODE:LBL,
   0 RT:G-PUSH ;

: FS-SYS-PUSH ( -- )
   9 A64ASM:C-CS A64ICODE:CSET,  FS-SYS-PUSH-FLAG ;

: BOPEN ( -- )
   2 RT:G-POP  1 RT:G-POP  0 RT:G-POP
   HB-TARGET-LINUX-ARM64? IF
      3 2 0 A64ICODE:ADDI,
      SYSCALL:OS-OPEN-FLAGS
      1 0 0 A64ICODE:ADDI,
      0 99 A64ICODE:MOVN,
      SYSCALL:NR-OPEN SYSCALL:SYS,  FS-SYS-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF SYSCALL:NR-OPEN SYSCALL:SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BWRITE ( -- )
   2 RT:G-POP  1 RT:G-POP  0 RT:G-POP  SYSCALL:NR-WRITE SYSCALL:SYS,  FS-SYS-PUSH ;

: BREAD ( -- )
   2 RT:G-POP  1 RT:G-POP  0 RT:G-POP  SYSCALL:NR-READ SYSCALL:SYS,  FS-SYS-PUSH ;

: BIOCTL ( -- )
   2 RT:G-POP  1 RT:G-POP  0 RT:G-POP  SYSCALL:NR-IOCTL SYSCALL:SYS,  SYS-PUSH ;

: BMMAP ( -- )                     \ ( addr len prot flags fd off -- addr|-1 )
   5 RT:G-POP  4 RT:G-POP  3 RT:G-POP  2 RT:G-POP  1 RT:G-POP  0 RT:G-POP
   HB-TARGET-LINUX-ARM64? IF SYSCALL:OS-MMAP-FLAGS  SYSCALL:NR-MMAP SYSCALL:SYS,  SYS-PUSH  exit THEN
   HB-TARGET-MACOS? IF SYSCALL:NR-MMAP SYSCALL:SYS,  SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

\ Fresh anonymous storage has no prior element type. The caller chooses the
\ pointee type; the OS boundary returns a null pointer and -1 on failure.
\ Darwin accepts a zero-length mmap; an empty extent owns no storage here.
: BMAPANON ( -- )                      \ ( bytes -- ^a ior )
   A64ICODE:LBL {: failed:label :}
   A64ICODE:LBL {: done:label :}
   1 RT:G-POP
   1 0 A64ICODE:CMPI,  A64ASM:C-LE failed A64ICODE:BCOND,
   0 0 A64ICODE:MOVZ,  2 3 A64ICODE:MOVZ,  3 SYSCALL:MAP-ANON-PRIVATE A64ICODE:LIT64,
   4 0 A64ICODE:MOVN,  5 0 A64ICODE:MOVZ,
   SYSCALL:NR-MMAP SYSCALL:SYS,  A64ASM:C-CS failed A64ICODE:BCOND,
   1 0 A64ICODE:MOVZ,  done A64ICODE:B,
   failed A64ICODE:LBL,  0 0 A64ICODE:MOVZ,  1 0 A64ICODE:MOVN,
   done A64ICODE:LBL,  0 RT:G-PUSH  1 RT:G-PUSH ;

: BMUNMAP ( -- )                   \ ( addr len -- 0|-1 )
   1 RT:G-POP  0 RT:G-POP
   SYSCALL:NR-MUNMAP SYSCALL:SYS,  SYS-PUSH ;

: BGETPID ( -- )                   \ ( -- pid )
   SYSCALL:NR-GETPID SYSCALL:SYS,  SYS-PUSH ;

\ XDS (x19) is callee-saved, so the data stack survives the call.

: BFFI-LOAD-X0-X7 ( -- )
   0 15 0  A64ICODE:LDR,   1 15 8  A64ICODE:LDR,   2 15 16 A64ICODE:LDR,   3 15 24 A64ICODE:LDR,
   4 15 32 A64ICODE:LDR,   5 15 40 A64ICODE:LDR,   6 15 48 A64ICODE:LDR,   7 15 56 A64ICODE:LDR, ;

public
\ This thread's pointer, compared with CB-OWNER and a slot row: one value for a
\ thread's life, distinct across live threads. Linux: TPIDR_EL0, the TLS base
\ clone gives each thread. macOS: TPIDRRO_EL0, the kernel's current cthread
\ pointer. The low three bits are cleared anyway, for kernels that kept the
\ CPU number there.
: CB-SELF, ( n -- ) {: r:n :}
   HB-TARGET-LINUX-ARM64? IF
      $D53BD040 r or A64ICODE:EMITW
   ELSE
      $D53BD060 r or A64ICODE:EMITW
      r r 3 A64ICODE:LSRI,  r r 3 A64ICODE:LSLI,
   THEN ;
private

\ Every outbound core runs this after its pops and guards, before its BLR. The
\ owner comes first: LDAR CB-OWNER; this thread already is nested and claims
\ nothing; 0 is CASAL'd 0 -> self; any other owner, or a lost CAS, dies 106
\ with the region untouched. Only then the $30 frame - the claim flag at [sp],
\ [sp+8] for the core's own x20 park, the four cells it replaces at
\ [sp+$10..$28] - and the live cursors go out. Scratch is x9..x12, which no
\ core holds across this point.
: BCB-ENTER ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: held:label busy:label go:label :}
   12 CB-SELF,
   11 CB-OWNER A64ICODE:MOVZ,  11 XREG-RBASE 11 A64ICODE:ADD,
   9 11 A64ICODE:LDAR,
   10 0 A64ICODE:MOVZ,
   9 12 A64ICODE:CMP,  A64ASM:C-EQ held A64ICODE:BCOND,
   9 busy A64ICODE:CBNZ,
   9 12 11 A64ICODE:CASAL,
   9 busy A64ICODE:CBNZ,
   10 1 A64ICODE:MOVZ,
   held A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP $30 A64ICODE:SUBI,
   10 A64ICODE:SP 0 A64ICODE:STR,
   9 XREG-RBASE CB-XDS A64ICODE:LDR,    9 A64ICODE:SP $10 A64ICODE:STR,
   9 XREG-RBASE CB-DBASE A64ICODE:LDR,  9 A64ICODE:SP $18 A64ICODE:STR,
   9 XREG-RBASE CB-NDICT A64ICODE:LDR,  9 A64ICODE:SP $20 A64ICODE:STR,
   9 XREG-RBASE CB-CP A64ICODE:LDR,     9 A64ICODE:SP $28 A64ICODE:STR,
   A64ICODE:XDS XREG-RBASE CB-XDS A64ICODE:STR,
   DBASE XREG-RBASE CB-DBASE A64ICODE:STR,
   NDICT XREG-RBASE CB-NDICT A64ICODE:STR,
   CP XREG-RBASE CB-CP A64ICODE:STR,
   go A64ICODE:B,
   busy A64ICODE:LBL,
   3 A64ICODE:SP 0 A64ICODE:LDR,                             \ the lr, saved by the primitive's frame
   s" E-CALLBACK" FAULT-RECORD:CB-CONTEXT-BUSY$ CRASH:FAULT-OPEN,
   4 DBASE 0 A64ICODE:ADDI,  6 NDICT 0 A64ICODE:ADDI,  CRASH:FAULT-IN,
   FAULT-RECORD:CB-CONTEXT-BUSY-HELP$ EXIT-STATUS:CALLBACK CRASH:FAULT-STOP,
   go A64ICODE:LBL, ;

\ After the BLR, with sp back at the $30 frame and x20 = DATA: the four cells
\ are restored first and a claimed owner is released last, by STLR, so a
\ thread that acquires the owner sees a complete frame or CB-XDS 0. x0 and d0
\ carry the result, so the scratch is x9..x11.
: BCB-LEAVE ( -- )
   A64ICODE:LBL {: kept:label :}
   9 A64ICODE:SP $10 A64ICODE:LDR,  9 XREG-RBASE CB-XDS A64ICODE:STR,
   9 A64ICODE:SP $18 A64ICODE:LDR,  9 XREG-RBASE CB-DBASE A64ICODE:STR,
   9 A64ICODE:SP $20 A64ICODE:LDR,  9 XREG-RBASE CB-NDICT A64ICODE:STR,
   9 A64ICODE:SP $28 A64ICODE:LDR,  9 XREG-RBASE CB-CP A64ICODE:STR,
   9 A64ICODE:SP 0 A64ICODE:LDR,
   A64ICODE:SP A64ICODE:SP $30 A64ICODE:ADDI,
   9 kept A64ICODE:CBZ,
      11 CB-OWNER A64ICODE:MOVZ,  11 XREG-RBASE 11 A64ICODE:ADD,  10 0 A64ICODE:MOVZ,  10 11 A64ICODE:STLR,
   kept A64ICODE:LBL, ;

: BFFI-CALL ( -- )
   16 RT:G-POP                                            \ x16 = fn
   14 RT:G-POP                                            \ x14 = nargs
   15 RT:G-POP                                            \ x15 = argbuf
   BCB-ENTER
   BFFI-LOAD-X0-X7
   16 A64ICODE:BLR,
   BCB-LEAVE
   0 RT:G-PUSH ;

\ The body of ffi-call-k ( x1 .. xk fn -- n ), k from 0 to 8: x1, the deepest
\ cell, goes to x0 and xk to x(k-1). The pops precede BCB-ENTER, which keeps
\ x0..x7 and x16, so the frame it publishes holds the stack without them.
: BFFI-CALL-K ( n -- ) {: k:n :}
   16 RT:G-POP
   k 0 ?do  k 1- i - RT:G-POP  loop
   BCB-ENTER
   16 A64ICODE:BLR,
   BCB-LEAVE
   0 RT:G-PUSH ;

: BFFI-LOAD-DREG ( n n -- )
   FFI-OFF !
   FFI-DREG !
   9 17 FFI-OFF @ A64ICODE:LDR,  FFI-DREG @ 9 A64ICODE:FMOVXD, ;

: BFFI-LOAD-D0-D7 ( -- )
   0 0 BFFI-LOAD-DREG    1 $8 BFFI-LOAD-DREG
   2 $10 BFFI-LOAD-DREG  3 $18 BFFI-LOAD-DREG
   4 $20 BFFI-LOAD-DREG  5 $28 BFFI-LOAD-DREG
   6 $30 BFFI-LOAD-DREG  7 $38 BFFI-LOAD-DREG ;

: BFFI-COPY-ABI-STACK ( -- )
   A64ICODE:LBL FFI-SKIP !
   A64ICODE:LBL FFI-LOOP !
   A64ICODE:LBL FFI-DONE !
   14 0 A64ICODE:CMPI,  A64ASM:C-LE FFI-SKIP A64ICODE:LABEL@ A64ICODE:BCOND,
   10 14 0 A64ICODE:ADDI,                                      \ x10 = cells left
   11 10 3 A64ICODE:LSLI,  11 11 $F A64ICODE:ADDI,  11 11 4 A64ICODE:LSRI,  11 11 4 A64ICODE:LSLI,
   12 A64ICODE:SP 0 A64ICODE:ADDI,  12 12 11 A64ICODE:SUB,  A64ICODE:SP 12 0 A64ICODE:ADDI,        \ sp -= align(cells*8,16)
   12 13 0 A64ICODE:ADDI,  13 A64ICODE:SP 0 A64ICODE:ADDI,                       \ x12=src, x13=dst
   FFI-LOOP A64ICODE:LABEL@ A64ICODE:LBL,
      10 FFI-DONE A64ICODE:LABEL@ A64ICODE:CBZ,
      9 12 0 A64ICODE:LDR,  9 13 0 A64ICODE:STR,
      12 12 $8 A64ICODE:ADDI,  13 13 $8 A64ICODE:ADDI,
      10 10 1 A64ICODE:SUBI,  FFI-LOOP A64ICODE:LABEL@ A64ICODE:B,
   FFI-DONE A64ICODE:LABEL@ A64ICODE:LBL,
   FFI-SKIP A64ICODE:LABEL@ A64ICODE:LBL, ;

\ A relocated cell holds a prepacked-buffer address; it is moved by the
\ distance x13 from that buffer to the copy on the native stack. The mask in
\ x12 names the cells: bits 0-7 are x0-x7, bits 8-23 the stack cells 0-15.
: BFFI-RELOC-REG ( n -- ) A64ICODE:LBL {: r:n skip:label :}
   11 12 63 r - A64ICODE:LSLI,  11 11 63 A64ICODE:LSRI,  11 skip A64ICODE:CBZ,
   r r 13 A64ICODE:ADD,
   skip A64ICODE:LBL, ;

: BFFI-RELOC-CELL ( n -- ) A64ICODE:LBL {: j:n skip:label :}
   11 12 55 j - A64ICODE:LSLI,  11 11 63 A64ICODE:LSRI,  11 skip A64ICODE:CBZ,
   9 A64ICODE:SP j 8 * A64ICODE:LDR,  9 9 13 A64ICODE:ADD,  9 A64ICODE:SP j 8 * A64ICODE:STR,
   skip A64ICODE:LBL, ;

\ x20 holds DATA through every load that can fault; d0-d7 load first, so x17
\ carries the frame sp to the BLR and x20 (callee-saved) only across the BLR.
\ A relocating call pops the mask under fn into x2 and keeps the prepacked
\ buffer's address in x3 until the copy is made.
: BFFI-CALL-ABI-CORE ( bool -- ) {: reloc:bool :}
   16 RT:G-POP                                            \ x16 = fn
   reloc if 2 RT:G-POP then                               \ x2  = relocation mask
   14 RT:G-POP                                            \ x14 = stack cell count
   13 RT:G-POP                                            \ x13 = prepacked stack cells
   17 RT:G-POP                                            \ x17 = FP argbuf
   15 RT:G-POP                                            \ x15 = integer argbuf
   reloc if 3 13 0 A64ICODE:ADDI, then
   BCB-ENTER
   20 A64ICODE:SP $8 A64ICODE:STR,
   BFFI-LOAD-D0-D7
   17 A64ICODE:SP 0 A64ICODE:ADDI,
   BFFI-COPY-ABI-STACK
   reloc if
      13 A64ICODE:SP 0 A64ICODE:ADDI,  13 13 3 A64ICODE:SUB,
      12 2 0 A64ICODE:ADDI,                                     \ x12 = mask; x0-x7 load over x2/x3
      16 0 do i BFFI-RELOC-CELL loop
   then
   BFFI-LOAD-X0-X7
   8 15 $40 A64ICODE:LDR,                                       \ x8 = indirect-result address
   reloc if 8 0 do i BFFI-RELOC-REG loop then
   20 17 0 A64ICODE:ADDI,
   16 A64ICODE:BLR,
   A64ICODE:SP 20 0 A64ICODE:ADDI,
   20 A64ICODE:SP $8 A64ICODE:LDR,
   BCB-LEAVE ;

: BFFI-CALL-ABI ( -- )
   false BFFI-CALL-ABI-CORE
   0 RT:G-PUSH ;

: BFFI-CALL-ABI-R ( -- )
   false BFFI-CALL-ABI-CORE
   9 0 A64ICODE:FMOVDX,  9 RT:G-PUSH ;

\ Returns an immutable pthread entry, not a Habu xt; its C argument is the
\ TASK-ABI descriptor and the body enters that task's VM state. It writes two
\ task cells: the TCB address into the task's data region, and DONE into the
\ status after the body. Not RUNNING: ACTIVATE stores it before pthread_create,
\ and a store here could land after a TASK:HALT took RUNNING -> HALT-REQ in the
\ create window. DONE is release-published by STLR: it is the task's last write
\ another thread reads, so an owner polling TASK:DONE? (an acquire load) sees
\ the body's writes without joining. STLR takes no offset, so the status address
\ is formed first, in x11.
: BTASK-ENTRY ( -- )
   A64ICODE:LBL A64ICODE:LBL {: entry:label done:label :}
   A entry A64ICODE:ADR, A RT:G-PUSH done A64ICODE:B,
   entry A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP $A0 A64ICODE:SUBI,
   29 19 do i A64ICODE:SP i 19 - cells A64ICODE:STR, loop
   XREG-RBASE 0 A64ICODE:MOVZ,
   29 A64ICODE:SP $50 A64ICODE:STR, 30 A64ICODE:SP $58 A64ICODE:STR,
   16 8 do i A64ICODE:SP i 8 - cells $60 + A64ASM:ENC-STRD A64ICODE:EMITW loop
   29 A64ICODE:SP $50 A64ICODE:ADDI,                         \ previous FP/LR form the C frame
   19 0 0 A64ICODE:ADDI,                            \ the descriptor across the registration
   CRASH:C-SIGNAL-STACK                          \ this thread's own: below the frame
   0 19 0 A64ICODE:ADDI,
   9 0 TASK-ABI:XT-OFF A64ICODE:LDR,
   A64ICODE:XDS 0 TASK-ABI:STACK-OFF A64ICODE:LDR,
   XREG-RBASE 0 TASK-ABI:REGION-OFF A64ICODE:LDR,
   DBASE 0 TASK-ABI:DBASE-OFF A64ICODE:LDR,
   NDICT 0 TASK-ABI:NDICT-OFF A64ICODE:LDR,
   CP 0 TASK-ABI:CP-OFF A64ICODE:LDR,
   0 XREG-RBASE TASK-TCB-CELL A64ICODE:STR,
   A64ICODE:ISB,  9 A64ICODE:BLR,
   CRASH:C-SIGNAL-STACK-OFF
   A64ICODE:SP A64ICODE:SP STACK-ABI:SIGNAL-BYTES CRASH:ADDI-4K,   \ the band back: sp is the frame's again
   9 XREG-RBASE TASK-TCB-CELL A64ICODE:LDR,
   10 TASK-ABI:DONE A64ICODE:MOVZ, 11 9 TASK-ABI:STATUS-OFF A64ICODE:ADDI,  10 11 A64ICODE:STLR,
   0 0 A64ICODE:MOVZ,
   16 8 do i A64ICODE:SP i 8 - cells $60 + A64ASM:ENC-LDRD A64ICODE:EMITW loop
   29 19 do i A64ICODE:SP i 19 - cells A64ICODE:LDR, loop
   29 A64ICODE:SP $50 A64ICODE:LDR, 30 A64ICODE:SP $58 A64ICODE:LDR,
   A64ICODE:SP A64ICODE:SP $A0 A64ICODE:ADDI, A64ICODE:RET,
   done A64ICODE:LBL, ;

\ The thunk's frame on C's stack: C's callee-saved set at [sp..$A0), the
\ marshal frame CB-FRAME names (x0..x7, then d0..d7 from CB-FRAME-FLOATS),
\ and the thunk's own state. Checked code may hold state in x21..x25 and
\ x29, so everything the thunk needs after its BLR is on this frame,
\ never in a register.
$A0 constant CBT-MARSHAL
CBT-MARSHAL CB-FRAME-FLOATS + constant CBT-FLOATS
$120 constant CBT-ROW                   \ the slot row's address
$128 constant CBT-REGION                \ the region the row named
$130 constant CBT-ROW-CLAIM             \ 0 none, 1 the row, 2 a caller count
$138 constant CBT-OWNER-CLAIM           \ 1 when this entry claimed CB-OWNER
$140 constant CBT-OUTER                 \ the CB-FRAME this entry replaced
$148 constant CBT-SLOT
$150 constant CBT-BYTES
$50 constant CBT-FP                     \ C's x29
$58 constant CBT-LR                     \ C's x30

\ A callback rejection at the label, sp at the thunk's frame: its record with
\ the slot and C's lr, which no word owns, then exit EXIT-STATUS:CALLBACK.
\ FAULT-OPEN, moves sp and the record writes no x3 or x4, so both are read first.
: CBT-DIE, ( str str label -- ) {: msg:str help:str at:label :}
   at A64ICODE:LBL,
   3 A64ICODE:SP CBT-SLOT A64ICODE:LDR,  4 A64ICODE:SP CBT-LR A64ICODE:LDR,
   s" E-CALLBACK" msg CRASH:FAULT-OPEN,
   s" slot" 3 CRASH:FAULT-NUMBER,
   s" caller" 4 CRASH:FAULT-ADDRESS,
   help EXIT-STATUS:CALLBACK CRASH:FAULT-STOP, ;

\ Entered from a stub with the slot in x9. The row is claimed before the region
\ it names is read: LDAR the row owner; a thread already nested through this
\ slot claims nothing; 0 is CASAL'd 0 -> self; another thread dies. CB-ROW-BUSY
\ (read, or answered by a lost CAS) is a mover holding the row for a few steps
\ that call and wait on nothing, so the thunk spins on LDAR with YIELD until the
\ row is handed back and claims it as left; a row dropped meanwhile reads
\ unbound and dies unless a bind took it. A caller row names no region: the
\ region is this thread's own, mapped while the thread runs. A region stays
\ mapped while a row names it, so the region is read only then: its CB-OWNER
\ claimed as BCB-ENTER claims it, a live frame (CB-XDS <> 0) and the slot's xt,
\ a ( -- ) dispatch that reads its arguments through CB-FRAME and leaves its
\ result in the x0 or d0 slot.
\ A body whose NDICT or CP differs from the frame's dies: the way out restores
\ C's callee-saved registers, the caller's VM among them, and would drop what a
\ definition added. Otherwise CB-FRAME, then the owner if claimed, then the
\ row are restored, each by STLR.
\ A C-started thread has no alternate signal stack. Only an owner-claiming
\ entry can be on one (an owner already its own means a registered thread), so
\ it queries before the branch and carves one below the frame when none is
\ registered; a 16-byte cell at the branch's sp records that, so the way out
\ gives the band back before reading the frame.
: BCALLBACK-THUNK ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
   {: row-read:label row-wait:label row-held:label owner-held:label
      owner-kept:label row-kept:label no-table:label row-busy:label
      unbound:label owner-busy:label no-frame:label no-xt:label
      defined:label signal-had:label signal-kept:label caller-row:label
      region-found:label caller-leave:label no-region:label :}
   A64ICODE:SP A64ICODE:SP CBT-BYTES A64ICODE:SUBI,
   \ C's callee-saved set is saved and restored in this body.
   29 19 do i A64ICODE:SP i 19 - cells A64ICODE:STR, loop
   XREG-RBASE 0 A64ICODE:MOVZ,
   29 A64ICODE:SP CBT-FP A64ICODE:STR, 30 A64ICODE:SP CBT-LR A64ICODE:STR,
   16 8 do i A64ICODE:SP i 8 - cells $60 + A64ASM:ENC-STRD A64ICODE:EMITW loop
   29 A64ICODE:SP CBT-FP A64ICODE:ADDI,                                  \ previous FP/LR form the C frame
   8 0 do i A64ICODE:SP CBT-MARSHAL i cells + A64ICODE:STR, loop
   8 0 do i A64ICODE:SP CBT-FLOATS i cells + A64ASM:ENC-STRD A64ICODE:EMITW loop
   9 A64ICODE:SP CBT-SLOT A64ICODE:STR,
   10 DATA-VA VA>N A64ICODE:LIT64,                              \ x10 = the main region
   11 CB-ROWS A64ICODE:MOVZ,  11 10 11 A64ICODE:ADD,  11 11 A64ICODE:LDAR,
   11 no-table A64ICODE:CBZ,
   12 CB-ROW-BYTES A64ICODE:MOVZ,  12 9 12 A64ICODE:MUL,  11 11 12 A64ICODE:ADD,  \ x11 = the row
   11 A64ICODE:SP CBT-ROW A64ICODE:STR,
   12 CB-SELF,
   13 11 CB-ROW-OWNER A64ICODE:ADDI,
   15 0 A64ICODE:MOVZ,
   row-read A64ICODE:LBL,
   14 13 A64ICODE:LDAR,
   16 14 3 A64ICODE:ANDI,  16 CB-ROW-CALLER A64ICODE:CMPI,  A64ASM:C-EQ caller-row A64ICODE:BCOND,
   14 12 A64ICODE:CMP,  A64ASM:C-EQ row-held A64ICODE:BCOND,
   14 CB-ROW-BUSY A64ICODE:CMPI,  A64ASM:C-EQ row-wait A64ICODE:BCOND,
   14 row-busy A64ICODE:CBNZ,
   14 12 13 A64ICODE:CASAL,
   16 14 3 A64ICODE:ANDI,  16 CB-ROW-CALLER A64ICODE:CMPI,  A64ASM:C-EQ row-read A64ICODE:BCOND,
   14 CB-ROW-BUSY A64ICODE:CMPI,  A64ASM:C-EQ row-wait A64ICODE:BCOND,
   14 row-busy A64ICODE:CBNZ,
   15 1 A64ICODE:MOVZ,
   row-held A64ICODE:LBL,
   15 A64ICODE:SP CBT-ROW-CLAIM A64ICODE:STR,
   14 11 A64ICODE:LDAR,                                         \ x14 = CB-ROW-REGION, offset 0
   14 unbound A64ICODE:CBZ,
   region-found A64ICODE:LBL,
   14 A64ICODE:SP CBT-REGION A64ICODE:STR,
   13 CB-OWNER A64ICODE:MOVZ,  13 14 13 A64ICODE:ADD,
   15 13 A64ICODE:LDAR,
   16 0 A64ICODE:MOVZ,
   15 12 A64ICODE:CMP,  A64ASM:C-EQ owner-held A64ICODE:BCOND,
   15 owner-busy A64ICODE:CBNZ,
   15 12 13 A64ICODE:CASAL,
   15 owner-busy A64ICODE:CBNZ,
   16 1 A64ICODE:MOVZ,
   owner-held A64ICODE:LBL,
   16 A64ICODE:SP CBT-OWNER-CLAIM A64ICODE:STR,
   15 14 CB-XDS A64ICODE:LDR,
   15 no-frame A64ICODE:CBZ,
   13 CB-XTS A64ICODE:MOVZ,  13 10 13 A64ICODE:ADD,  13 13 A64ICODE:LDAR,
   13 no-xt A64ICODE:CBZ,
   12 9 2 A64ICODE:LSLI,  12 12 9 A64ICODE:ADD,  12 12 3 A64ICODE:LSLI,          \ five cells per descriptor
   13 13 12 A64ICODE:ADD,  13 13 0 A64ICODE:LDR,
   13 no-xt A64ICODE:CBZ,
   12 14 CB-FRAME A64ICODE:LDR,  12 A64ICODE:SP CBT-OUTER A64ICODE:STR,
   12 A64ICODE:SP CBT-MARSHAL A64ICODE:ADDI,  12 14 CB-FRAME A64ICODE:STR,
   A64ICODE:XDS 15 0 A64ICODE:ADDI,
   XREG-RBASE 14 0 A64ICODE:ADDI,
   DBASE 14 CB-DBASE A64ICODE:LDR,
   NDICT 14 CB-NDICT A64ICODE:LDR,
   CP 14 CB-CP A64ICODE:LDR,
   12 0 A64ICODE:MOVZ,
   16 signal-had A64ICODE:CBZ,                                  \ x16 = the owner claim, still live
      CRASH:C-SIGNAL-STACK-ABSENT>R12
      12 signal-had A64ICODE:CBZ,
      CRASH:C-SIGNAL-STACK                                   \ sp drops below the band
      12 1 A64ICODE:MOVZ,
   signal-had A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  12 A64ICODE:SP 0 A64ICODE:STR,                        \ 1 when the band below the frame is this entry's
   A64ICODE:ISB,  13 A64ICODE:BLR,
   12 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI,
   12 signal-kept A64ICODE:CBZ,
      CRASH:C-SIGNAL-STACK-OFF
      A64ICODE:SP A64ICODE:SP STACK-ABI:SIGNAL-BYTES CRASH:ADDI-4K,            \ the band back: sp is the frame's again
   signal-kept A64ICODE:LBL,
   14 A64ICODE:SP CBT-REGION A64ICODE:LDR,
   12 14 CB-NDICT A64ICODE:LDR,  12 NDICT A64ICODE:CMP,  A64ASM:C-NE defined A64ICODE:BCOND,
   12 14 CB-CP A64ICODE:LDR,  12 CP A64ICODE:CMP,  A64ASM:C-NE defined A64ICODE:BCOND,
   12 A64ICODE:SP CBT-OUTER A64ICODE:LDR,  12 14 CB-FRAME A64ICODE:STR,
   10 0 A64ICODE:MOVZ,
   12 A64ICODE:SP CBT-OWNER-CLAIM A64ICODE:LDR,  12 owner-kept A64ICODE:CBZ,
      13 CB-OWNER A64ICODE:MOVZ,  13 14 13 A64ICODE:ADD,  10 13 A64ICODE:STLR,
   owner-kept A64ICODE:LBL,
   12 A64ICODE:SP CBT-ROW-CLAIM A64ICODE:LDR,  12 row-kept A64ICODE:CBZ,
      13 A64ICODE:SP CBT-ROW A64ICODE:LDR,  13 13 CB-ROW-OWNER A64ICODE:ADDI,
      12 1 A64ICODE:CMPI,  A64ASM:C-NE caller-leave A64ICODE:BCOND,
      10 13 A64ICODE:STLR,
   row-kept A64ICODE:LBL,
   0 A64ICODE:SP CBT-MARSHAL A64ICODE:LDR,
   0 A64ICODE:SP CBT-FLOATS A64ASM:ENC-LDRD A64ICODE:EMITW
   16 8 do i A64ICODE:SP i 8 - cells $60 + A64ASM:ENC-LDRD A64ICODE:EMITW loop
   29 19 do i A64ICODE:SP i 19 - cells A64ICODE:LDR, loop
   29 A64ICODE:SP CBT-FP A64ICODE:LDR, 30 A64ICODE:SP CBT-LR A64ICODE:LDR,
   A64ICODE:SP A64ICODE:SP CBT-BYTES A64ICODE:ADDI,
   A64ICODE:RET,
   \ x14 is the owner read, x13 its cell.
   caller-row A64ICODE:LBL,
      16 14 4 A64ICODE:ADDI,  17 14 0 A64ICODE:ADDI,  17 16 13 A64ICODE:CASAL,
      17 14 A64ICODE:CMP,  A64ASM:C-NE row-read A64ICODE:BCOND,
      16 2 A64ICODE:MOVZ,  16 A64ICODE:SP CBT-ROW-CLAIM A64ICODE:STR,
      11 CB-TLS-GET A64ICODE:MOVZ,  11 10 11 A64ICODE:ADD,  11 11 A64ICODE:LDAR,
      11 unbound A64ICODE:CBZ,
      0 10 CB-TLS-KEY A64ICODE:LDR,
      11 A64ICODE:BLR,                                          \ C: x0..x18 are lost
      9 A64ICODE:SP CBT-SLOT A64ICODE:LDR,
      10 DATA-VA VA>N A64ICODE:LIT64,
      12 CB-SELF,
      0 no-region A64ICODE:CBZ,
      14 0 0 A64ICODE:ADDI,
      region-found A64ICODE:B,
   caller-leave A64ICODE:LBL,
      14 13 A64ICODE:LDAR,
      15 14 4 A64ICODE:SUBI,  16 14 0 A64ICODE:ADDI,  16 15 13 A64ICODE:CASAL,
      16 14 A64ICODE:CMP,  A64ASM:C-NE caller-leave A64ICODE:BCOND,
      row-kept A64ICODE:B,
   row-wait A64ICODE:LBL,  A64ICODE:YIELD,  row-read A64ICODE:B,
   FAULT-RECORD:CB-NO-TABLE$ FAULT-RECORD:CB-NO-TABLE-HELP$ no-table CBT-DIE,
   FAULT-RECORD:CB-ROW-BUSY$ FAULT-RECORD:CB-ROW-BUSY-HELP$ row-busy CBT-DIE,
   FAULT-RECORD:CB-UNBOUND$ FAULT-RECORD:CB-UNBOUND-HELP$ unbound CBT-DIE,
   FAULT-RECORD:CB-NO-REGION$ FAULT-RECORD:CB-NO-REGION-HELP$ no-region CBT-DIE,
   FAULT-RECORD:CB-OWNER-BUSY$ FAULT-RECORD:CB-OWNER-BUSY-HELP$
      owner-busy CBT-DIE,
   FAULT-RECORD:CB-NO-FRAME$ FAULT-RECORD:CB-NO-FRAME-HELP$ no-frame CBT-DIE,
   FAULT-RECORD:CB-NO-XT$ FAULT-RECORD:CB-NO-XT-HELP$ no-xt CBT-DIE,
   FAULT-RECORD:CB-DEFINED$ FAULT-RECORD:CB-DEFINED-HELP$ defined CBT-DIE, ;

\ Answers the shared C thunk every compiler-owned callback stub branches to.
\ This primitive ends before the separately owned thunk begins.
: BCALLBACK-ENTRY ( -- )
   LCALLBACKADDR A64ICODE:LABEL@ A64ICODE:BL,  A RT:G-PUSH ;

\ A native callback stub carries this address across FILE and image builds.
\ The thunk follows callback-entry's completed primitive record, so its sealed
\ helper record is disjoint even when an image also uses the public primitive.
: EMIT-CALLBACK-THUNK ( -- )
   A64ICODE:LBL A64ICODE:LBL {: body:label done:label :}
   s" (CALLBACK-THUNK)" LCALLBACKTHUNK A64ICODE:LABEL@ LABEL>N
      done LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   LCALLBACKTHUNK A64ICODE:LABEL@ A64ICODE:LBL,
   body A64ICODE:B,
   \ The public primitive calls this tiny accessor. Its ADR points to the
   \ same member's C entry, so the image relocates it within this record.
   LCALLBACKADDR A64ICODE:LABEL@ A64ICODE:LBL,
   A LCALLBACKTHUNK A64ICODE:LABEL@ A64ICODE:ADR,  A64ICODE:RET,
   body A64ICODE:LBL,
   BCALLBACK-THUNK
   done A64ICODE:LBL, ;

\ A C struct returned by value: x0 then x1, both intact through BCB-LEAVE.
\ FFI stores the record they hold; an x8-indirect record is already written.
\ The relocation mask names the cells holding private struct copies'
\ prepacked addresses, which then point at the copies on the native stack.
: BFFI-CALL-STRUCT ( -- )
   true BFFI-CALL-ABI-CORE
   0 RT:G-PUSH  1 RT:G-PUSH ;

: BOPENRD ( -- )
   A RT:G-POP  A SYSCALL:OS-OPEN-RD  FS-SYS-PUSH ;

: BACCESS ( -- )
   1 RT:G-POP  0 RT:G-POP
   HB-TARGET-LINUX-ARM64? IF
      2 1 0 A64ICODE:ADDI,  1 0 0 A64ICODE:ADDI,  0 99 A64ICODE:MOVN,  3 0 A64ICODE:MOVZ,
      SYSCALL:NR-ACCESS SYSCALL:SYS,  FS-SYS-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF SYSCALL:NR-ACCESS SYSCALL:SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BUNLINK ( -- )
   0 RT:G-POP
   HB-TARGET-LINUX-ARM64? IF
      1 0 0 A64ICODE:ADDI,  0 99 A64ICODE:MOVN,  2 0 A64ICODE:MOVZ,
      SYSCALL:NR-UNLINK SYSCALL:SYS,  FS-SYS-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF SYSCALL:NR-UNLINK SYSCALL:SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BRENAME ( -- )
   1 RT:G-POP  0 RT:G-POP
   HB-TARGET-LINUX-ARM64? IF
      3 1 0 A64ICODE:ADDI,  1 0 0 A64ICODE:ADDI,  0 99 A64ICODE:MOVN,  2 99 A64ICODE:MOVN,
      SYSCALL:NR-RENAME SYSCALL:SYS,  FS-SYS-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF SYSCALL:NR-RENAME SYSCALL:SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BCHMOD ( -- )
   1 RT:G-POP  0 RT:G-POP
   HB-TARGET-LINUX-ARM64? IF
      2 1 0 A64ICODE:ADDI,  1 0 0 A64ICODE:ADDI,  0 99 A64ICODE:MOVN,  3 0 A64ICODE:MOVZ,
      SYSCALL:NR-CHMOD SYSCALL:SYS,  FS-SYS-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF SYSCALL:NR-CHMOD SYSCALL:SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BSYMLINK ( -- )
   2 RT:G-POP  0 RT:G-POP
   HB-TARGET-LINUX-ARM64? IF 1 99 A64ICODE:MOVN, ELSE
      HB-TARGET-MACOS? IF 1 1 A64ICODE:MOVN, ELSE ENGINE-EMIT:TARGET-UNKNOWN THEN
   THEN
   3 0 A64ICODE:MOVZ,  4 0 A64ICODE:MOVZ,  5 0 A64ICODE:MOVZ,
   SYSCALL:NR-SYMLINKAT SYSCALL:SYS,  FS-SYS-PUSH ;

: BREADLINK ( -- )
   3 RT:G-POP  2 RT:G-POP  1 RT:G-POP
   HB-TARGET-LINUX-ARM64? IF 0 99 A64ICODE:MOVN, ELSE
      HB-TARGET-MACOS? IF 0 1 A64ICODE:MOVN, ELSE ENGINE-EMIT:TARGET-UNKNOWN THEN
   THEN
   4 0 A64ICODE:MOVZ,  5 0 A64ICODE:MOVZ,
   SYSCALL:NR-READLINKAT SYSCALL:SYS,  FS-SYS-PUSH ;
;package

package LIBC-OS

: SLOT, ( str -- )   16 XREG-RBASE RBASE-CELL A64ICODE:LDR,  SYSCALL:SLOT-AT, ;

\ Copy [x1, x2) of the region to the same offsets x12 bytes away, a cell at a
\ time; x1 and x2 are 8-byte aligned. Clobbers x1, x3, x4.
: ALIAS-COPY, ( -- )
   A64ICODE:LBL A64ICODE:LBL {: top:label done:label :}
   top A64ICODE:LBL,  1 2 A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      3 1 0 A64ICODE:LDR,  4 1 12 A64ICODE:ADD,  3 4 0 A64ICODE:STR,
      1 1 8 A64ICODE:ADDI,  top A64ICODE:B,
   done A64ICODE:LBL, ;

\ The text's eight bytes from off as one little-endian cell, zero past its end.
: TEXT-CELL ( str n -- n )
   {: s:str off:n :}
   0
   8 0 ?do
      8 lshift
      off 7 + i -  dup s STR:LEN < if
         s STR:DATA swap + c@ or
      else drop then
   loop ;

\ Clobbers x0-x2, x9 and the system call's registers.
: ERR-TEXT, ( str -- )
   {: s:str :}
   s STR:LEN 15 + -16 and {: frame:n :}
   A64ICODE:SP A64ICODE:SP frame A64ICODE:SUBI,
   frame 8 / 0 ?do
      9 s i 8 * TEXT-CELL A64ICODE:LIT64,  9 A64ICODE:SP i 8 * A64ICODE:STR,
   loop
   0 2 A64ICODE:MOVZ,  1 A64ICODE:SP 0 A64ICODE:ADDI,  2 s STR:LEN A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   A64ICODE:SP A64ICODE:SP frame A64ICODE:ADDI, ;

public

\ Linux: x0 = a new memfd "hb-region" of REGION bytes, close-on-exec and
\ executable. A kernel before 6.3 rejects MFD_EXEC with EINVAL, and that answer
\ alone retries without it; any other failure jumps to fail with no descriptor
\ left open. Clobbers x0, x1, x5, x8, x16.
: REGION-MEMFD ( label -- ) {: fail:label :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: made:label retry:label sized:label :}
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,
   1 $6F696765722D6268 A64ICODE:LIT64,  1 A64ICODE:SP 0 A64ICODE:STR,          \ "hb-region" + NUL
   1 $6E A64ICODE:MOVZ,  1 A64ICODE:SP 8 A64ICODE:STR,
   0 A64ICODE:SP 0 A64ICODE:ADDI,  1 $11 A64ICODE:MOVZ,  SYSCALL:NR-MEMFD-CREATE SYSCALL:SYS,   \ MFD_CLOEXEC | MFD_EXEC
   A64ASM:C-CC made A64ICODE:BCOND,
   1 21 A64ICODE:MOVN,  0 1 A64ICODE:CMP,  A64ASM:C-EQ retry A64ICODE:BCOND,           \ -EINVAL
      A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI,  fail A64ICODE:B,
   retry A64ICODE:LBL,
   0 A64ICODE:SP 0 A64ICODE:ADDI,  1 1 A64ICODE:MOVZ,  SYSCALL:NR-MEMFD-CREATE SYSCALL:SYS,     \ MFD_CLOEXEC
   made A64ICODE:LBL,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI,  A64ASM:C-CS fail A64ICODE:BCOND,
   5 0 0 A64ICODE:ADDI,  1 REGION A64ICODE:LIT64,  SYSCALL:NR-FTRUNCATE SYSCALL:SYS,  A64ASM:C-CC sized A64ICODE:BCOND,
      0 5 0 A64ICODE:ADDI,  SYSCALL:NR-CLOSE SYSCALL:SYS,  fail A64ICODE:B,
   sized A64ICODE:LBL,  0 5 0 A64ICODE:ADDI, ;

\ macOS: map the region's RW alias over [x0, x0+REGION) with mach_vm_remap
\ (copy FALSE) and leave x0 = alias - x0. x16 = __text base on entry. fixed
\ maps the alias at exactly x1, replacing what is mapped there, else anywhere.
\ A forked child inherits the alias as a private copy, like the RX view: libc's
\ fork handlers allocate in the child before fork() returns, so the alias range
\ must stay mapped until the child replaces it. Clobbers x0-x17; jumps to
\ fail with SP restored if the remap fails. Frame: [0] &cur [8] &max [16]
\ inheritance (the call's stack arguments), [24] task port, [32] alias, [40]
\ cur [44] max, [48] RX base, [56] __text base.
: REGION-REMAP ( bool label -- ) {: fixed:bool fail:label :}
   A64ICODE:LBL A64ICODE:LBL {: bad:label ok:label :}
   A64ICODE:SP A64ICODE:SP 64 A64ICODE:SUBI,
   0 A64ICODE:SP 48 A64ICODE:STR,  16 A64ICODE:SP 56 A64ICODE:STR,
   9 A64ICODE:SP 40 A64ICODE:ADDI,  9 A64ICODE:SP 0 A64ICODE:STR,  9 A64ICODE:SP 44 A64ICODE:ADDI,  9 A64ICODE:SP 8 A64ICODE:STR,
   9 1 A64ICODE:MOVZ,  9 A64ICODE:SP 16 A64ICODE:STR,                          \ VM_INHERIT_COPY
   fixed if 1 A64ICODE:SP 32 A64ICODE:STR, else 9 0 A64ICODE:MOVZ,  9 A64ICODE:SP 32 A64ICODE:STR, then
   s" mach_task_self_" SYSCALL:SLOT-AT,  0 16 0 A64ICODE:LDRW,  0 A64ICODE:SP 24 A64ICODE:STR,
   16 A64ICODE:SP 56 A64ICODE:LDR,  s" mach_vm_remap" SYSCALL:SLOT-AT,
   0 A64ICODE:SP 24 A64ICODE:LDR,  1 A64ICODE:SP 32 A64ICODE:ADDI,  2 REGION A64ICODE:LIT64,  3 0 A64ICODE:MOVZ,
   fixed if 4 $4000 A64ICODE:MOVZ, else 4 1 A64ICODE:MOVZ, then       \ VM_FLAGS_FIXED | _OVERWRITE, or _ANYWHERE
   5 0 0 A64ICODE:ADDI,  6 A64ICODE:SP 48 A64ICODE:LDR,  7 0 A64ICODE:MOVZ,             \ copy FALSE
   16 A64ICODE:BLR,
   0 0 $FFFFFFFF A64ICODE:ANDI,  0 bad A64ICODE:CBNZ,                 \ kern_return_t
   0 A64ICODE:SP 32 A64ICODE:LDR,  1 A64ICODE:SP 48 A64ICODE:LDR,  0 0 1 A64ICODE:SUB,
   A64ICODE:SP A64ICODE:SP 64 A64ICODE:ADDI,  ok A64ICODE:B,
   bad A64ICODE:LBL,  A64ICODE:SP A64ICODE:SP 64 A64ICODE:ADDI,  fail A64ICODE:B,
   ok A64ICODE:LBL, ;

\ dlopen-entry ( -- n ): the address the loader bound to dlopen.
: DLOPEN-ENTRY ( -- )   s" dlopen" SLOT,  16 RT:G-PUSH ;

\ dlsym-entry ( -- n ): the address the loader bound to dlsym.
: DLSYM-ENTRY ( -- )   s" dlsym" SLOT,  16 RT:G-PUSH ;

\ ( pathz dst capacity -- length | -1 | -2 )
\ libc allocates its realpath result. Only a fitting result, including its NUL,
\ reaches dst; free runs after both success and insufficient capacity.
\ -1 = resolution failure, -2 = no room for the complete C string.
: REALPATH ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: badcap:label failed:label short:label done:label :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: count:label counted:label copy:label release:label :}
   2 RT:G-POP  1 RT:G-POP  0 RT:G-POP
   A64ICODE:SP A64ICODE:SP 48 A64ICODE:SUBI,                        \ [24] free, [32] result, [40] length
   0 A64ICODE:SP 0 A64ICODE:STR,  1 A64ICODE:SP 8 A64ICODE:STR,  2 A64ICODE:SP 16 A64ICODE:STR,
   2 0 A64ICODE:CMPI,  A64ASM:C-LE badcap A64ICODE:BCOND,
   s" free" SLOT,  16 A64ICODE:SP 24 A64ICODE:STR,
   s" realpath" SLOT,
   0 A64ICODE:SP 0 A64ICODE:LDR,  1 0 A64ICODE:MOVZ,  16 A64ICODE:BLR,
   0 failed A64ICODE:CBZ,  0 A64ICODE:SP 32 A64ICODE:STR,
   9 0 0 A64ICODE:ADDI,  10 0 A64ICODE:MOVZ,
   count A64ICODE:LBL,
      11 9 0 A64ICODE:LDRB,  11 counted A64ICODE:CBZ,
      9 9 1 A64ICODE:ADDI,  10 10 1 A64ICODE:ADDI,  count A64ICODE:B,
   counted A64ICODE:LBL,
   10 A64ICODE:SP 40 A64ICODE:STR,  12 A64ICODE:SP 16 A64ICODE:LDR,
   10 12 A64ICODE:CMP,  A64ASM:C-CS short A64ICODE:BCOND,
   9 A64ICODE:SP 32 A64ICODE:LDR,  11 10 1 A64ICODE:ADDI,  10 A64ICODE:SP 8 A64ICODE:LDR,
   copy A64ICODE:LBL,
      12 9 0 A64ICODE:LDRB,  12 10 0 A64ICODE:STRB,
      9 9 1 A64ICODE:ADDI,  10 10 1 A64ICODE:ADDI,  11 11 1 A64ICODE:SUBI,
      11 copy A64ICODE:CBNZ,
   release A64ICODE:LBL,
   0 A64ICODE:SP 32 A64ICODE:LDR,  16 A64ICODE:SP 24 A64ICODE:LDR,  16 A64ICODE:BLR,
   0 A64ICODE:SP 40 A64ICODE:LDR,  done A64ICODE:B,
   short A64ICODE:LBL,
   9 1 A64ICODE:MOVN,  9 A64ICODE:SP 40 A64ICODE:STR,  release A64ICODE:B,
   badcap A64ICODE:LBL,  0 1 A64ICODE:MOVN,  done A64ICODE:B,
   failed A64ICODE:LBL,  0 0 A64ICODE:MOVN,
   done A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP 48 A64ICODE:ADDI,  0 RT:G-PUSH ;

\ Darwin's libc fork reinitializes libSystem's child state, including Mach
\ reply ports. A raw syscall child can execute Habu but subsequent Mach VM
\ queries fail; the same query succeeds after libc fork. Linux keeps clone.
\ Each child owns its region before any Habu code runs, so its compiles stay
\ out of the parent's code and the parent's patches out of the child's. On
\ Linux both views are MAP_SHARED, so a child would alias the parent's live
\ memfd, which the parent keeps writing once `fork` has returned to it: the
\ parent fills a fresh memfd with its region's used extent BEFORE the clone and
\ the child maps that memfd over both views, so the child's region is the
\ parent's as it stood when the parent made the copy, and no byte the parent
\ writes afterwards reaches it. A failure before the clone answers -1, as a
\ failed clone does; a child that cannot map its views dies named with exit 71.
\ The child starts with no exit owner, so its own exit runs the actions.
: FORK ( -- )                         \ ( -- pid|-1 )
   HB-TARGET-LINUX-ARM64? IF
      A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: unfd:label failed:label parent:label done:label dead:label :}
      A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: scan:label next:label scanned:label plain:label :}
      A64ICODE:LBL A64ICODE:LBL {: span:label out:label :}
      A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,                    \ [0] the child's memfd, [8] its filling view, [16] code end
      \ Delta 0 = no alias (a stripped image): the startup maps one anywhere but DBASE.
      9 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  9 plain A64ICODE:CBZ,
      failed REGION-MEMFD  0 A64ICODE:SP 0 A64ICODE:STR,
      4 0 0 A64ICODE:ADDI,  0 0 A64ICODE:MOVZ,  1 REGION A64ICODE:LIT64,  2 3 A64ICODE:MOVZ,  3 SYSCALL:MAP-SHARED A64ICODE:LIT64,  5 0 A64ICODE:MOVZ,
      SYSCALL:NR-MMAP SYSCALL:SYS,  A64ASM:C-CS unfd A64ICODE:BCOND,
      0 A64ICODE:SP 8 A64ICODE:STR,  12 0 DBASE A64ICODE:SUB,                          \ x12 = view - DBASE
      \ The used extent, every region byte a Habu word in the child can reach:
      \ the records below the larger of NDICT + 1 (DEF-OPEN keeps the open
      \ definition's record at NDICT) and the high-water (the records a
      \ lowering retired), within DICT-CAP; then the control-flow stack
      \ and the code up to the larger of CP and the end of every record's span
      \ below the high-water (after a rollback a retired record's code sits above
      \ CP). CP and NDICT are this thread's registers; a task's are the snapshot
      \ ACTIVATE took as it went live, and main may have published records and
      \ code past that snapshot since. The high-water is main's
      \ cell, read at DATA-VA: a task region does not hold it.
      2 DATA-VA VA>N A64ICODE:LIT64,  2 2 NDICT-HW-CELL A64ICODE:LDR,  3 NDICT 1 A64ICODE:ADDI,  2 3 A64ICODE:CMP,  2 2 3 A64ASM:C-HI A64ICODE:CSEL,
      3 DICT-CAP A64ICODE:LIT64,  2 3 A64ICODE:CMP,  2 2 3 A64ASM:C-LS A64ICODE:CSEL,
      3 DREC A64ICODE:MOVZ,  2 2 3 A64ICODE:MUL,  2 DBASE 2 A64ICODE:ADD,  1 DBASE 0 A64ICODE:ADDI,
      ALIAS-COPY,
      9 CP 0 A64ICODE:ADDI,
      2 DATA-VA VA>N A64ICODE:LIT64,  2 2 NDICT-HW-CELL A64ICODE:LDR,  2 NDICT A64ICODE:CMP,  2 2 NDICT A64ASM:C-HI A64ICODE:CSEL,
      10 0 A64ICODE:MOVZ,
      scan A64ICODE:LBL,
         10 2 A64ICODE:CMP,  A64ASM:C-CS scanned A64ICODE:BCOND,
         11 DREC A64ICODE:MOVZ,  11 10 11 A64ICODE:MUL,  11 DBASE 11 A64ICODE:ADD,
         13 11 40 A64ICODE:LDR,  14 WL:RETIRED invert A64ICODE:MOVN,  13 14 A64ICODE:CMP,  A64ASM:C-EQ span A64ICODE:BCOND,
         13 0 A64ICODE:CMPI,  A64ASM:C-LT next A64ICODE:BCOND,                      \ a package row, live or retired
      span A64ICODE:LBL,
         13 11 0 A64ICODE:LDR,  14 11 8 A64ICODE:LDR,
         15 13 14 A64ICODE:ADD,  15 9 A64ICODE:CMP,  A64ASM:C-LS next A64ICODE:BCOND,  9 15 0 A64ICODE:ADDI,
      next A64ICODE:LBL,
         10 10 1 A64ICODE:ADDI,  scan A64ICODE:B,
      scanned A64ICODE:LBL,
      9 9 7 A64ICODE:ADDI,  9 9 3 A64ICODE:LSRI,  9 9 3 A64ICODE:LSLI,  9 A64ICODE:SP 16 A64ICODE:STR,
      1 CFSTK-OFF A64ICODE:LIT64,  1 DBASE 1 A64ICODE:ADD,  2 9 0 A64ICODE:ADDI,
      ALIAS-COPY,
      0 A64ICODE:SP 8 A64ICODE:LDR,  1 REGION A64ICODE:LIT64,  SYSCALL:NR-MUNMAP SYSCALL:SYS,  A64ASM:C-CS unfd A64ICODE:BCOND,
      0 17 A64ICODE:MOVZ,  1 0 A64ICODE:MOVZ,  2 0 A64ICODE:MOVZ,  3 0 A64ICODE:MOVZ,  4 0 A64ICODE:MOVZ,
      SYSCALL:NR-FORK SYSCALL:SYS,
      A64ASM:C-CS unfd A64ICODE:BCOND,  0 parent A64ICODE:CBNZ,
      \ The child: the filled memfd mapped RW at the parent's alias address, DBASE
      \ + delta, so every region's inherited delta cell stays true and none is
      \ rewritten, then RX over the inherited RX view, whose bytes the copy
      \ repeats under the code running here; the copied code is then flushed.
      10 A64ICODE:SP 0 A64ICODE:LDR,  12 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,
      0 DBASE 12 A64ICODE:ADD,  1 REGION A64ICODE:LIT64,  2 3 A64ICODE:MOVZ,  3 SYSCALL:MAP-SHARED-FIXED A64ICODE:LIT64,
      4 10 0 A64ICODE:ADDI,  5 0 A64ICODE:MOVZ,  SYSCALL:NR-MMAP SYSCALL:SYS,  A64ASM:C-CS dead A64ICODE:BCOND,
      0 DBASE 0 A64ICODE:ADDI,  1 REGION A64ICODE:LIT64,  2 5 A64ICODE:MOVZ,  3 SYSCALL:MAP-SHARED-FIXED A64ICODE:LIT64,
      4 10 0 A64ICODE:ADDI,  5 0 A64ICODE:MOVZ,  SYSCALL:NR-MMAP SYSCALL:SYS,  A64ASM:C-CS dead A64ICODE:BCOND,
      0 10 0 A64ICODE:ADDI,  SYSCALL:NR-CLOSE SYSCALL:SYS,  A64ASM:C-CS dead A64ICODE:BCOND,
      9 DICT-SIZE A64ICODE:LIT64,  9 DBASE 9 A64ICODE:ADD,  11 A64ICODE:SP 16 A64ICODE:LDR,  ENGINE-EMIT:LFLUSHTO A64ICODE:LABEL@ A64ICODE:BL,
      0 0 A64ICODE:MOVZ,  done A64ICODE:B,
      dead A64ICODE:LBL,
         S\" hb: fork: cannot map the child's own region\n" ERR-TEXT,
         0 EXIT-STATUS:SYSTEM A64ICODE:MOVZ,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS,
      parent A64ICODE:LBL,                       \ the descriptor is released whatever close answers
         9 0 0 A64ICODE:ADDI,  0 A64ICODE:SP 0 A64ICODE:LDR,  SYSCALL:NR-CLOSE SYSCALL:SYS,  0 9 0 A64ICODE:ADDI,  done A64ICODE:B,
      plain A64ICODE:LBL,
         0 17 A64ICODE:MOVZ,  1 0 A64ICODE:MOVZ,  2 0 A64ICODE:MOVZ,  3 0 A64ICODE:MOVZ,  4 0 A64ICODE:MOVZ,
         SYSCALL:NR-FORK SYSCALL:SYS,  A64ASM:C-CC done A64ICODE:BCOND,  failed A64ICODE:B,
      unfd A64ICODE:LBL,  0 A64ICODE:SP 0 A64ICODE:LDR,  SYSCALL:NR-CLOSE SYSCALL:SYS,
      failed A64ICODE:LBL,  0 0 A64ICODE:MOVN,
      done A64ICODE:LBL,
      0 out A64ICODE:CBNZ,  9 DATA-VA VA>N A64ICODE:LIT64,  0 9 EXIT-OWNER-CELL A64ICODE:STR,
      out A64ICODE:LBL,
      A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI,  0 RT:G-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF
      A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: done:label dead:label out:label :}
      s" fork" SLOT,  16 A64ICODE:BLR,
      0 0 32 A64ICODE:LSLI,  0 0 32 A64ICODE:ASRI,        \ signed pid_t -> Habu cell
      0 done A64ICODE:CBNZ,
      \ The child: its RX view and alias are private copies, so it replaces the
      \ alias, at the parent's address DBASE + delta, by an alias of its RX copy
      \ and makes it RW. The delta is the parent's, so every region's inherited
      \ delta cell stays true and none is rewritten.
      1 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  1 done A64ICODE:CBZ,   \ delta 0: no alias to re-establish (a stripped image)
      1 1 DBASE A64ICODE:ADD,
      0 DBASE 0 A64ICODE:ADDI,  16 XREG-RBASE RBASE-CELL A64ICODE:LDR,  true dead REGION-REMAP
      0 0 DBASE A64ICODE:ADD,  1 REGION A64ICODE:LIT64,  2 3 A64ICODE:MOVZ,  SYSCALL:NR-MPROTECT SYSCALL:SYS,
      A64ASM:C-CS dead A64ICODE:BCOND,
      0 0 A64ICODE:MOVZ,  done A64ICODE:B,
      dead A64ICODE:LBL,
         S\" hb: fork: cannot map the child's region alias\n" ERR-TEXT,
         0 EXIT-STATUS:SYSTEM A64ICODE:MOVZ,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS,
      done A64ICODE:LBL,
      0 out A64ICODE:CBNZ,  9 DATA-VA VA>N A64ICODE:LIT64,  0 9 EXIT-OWNER-CELL A64ICODE:STR,
      out A64ICODE:LBL,
      0 RT:G-PUSH
      exit
   THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

;package

package ENGINE-EMIT
: BMKDIR ( -- )
   1 RT:G-POP  0 RT:G-POP
   HB-TARGET-LINUX-ARM64? IF
      2 1 0 A64ICODE:ADDI,  1 0 0 A64ICODE:ADDI,  0 99 A64ICODE:MOVN,
      SYSCALL:NR-MKDIR SYSCALL:SYS,  FS-SYS-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF SYSCALL:NR-MKDIR SYSCALL:SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BRMDIR ( -- )
   0 RT:G-POP
   HB-TARGET-LINUX-ARM64? IF
      1 0 0 A64ICODE:ADDI,  0 99 A64ICODE:MOVN,  2 $200 A64ICODE:MOVZ,
      SYSCALL:NR-RMDIR SYSCALL:SYS,  FS-SYS-PUSH
      exit
   THEN
   HB-TARGET-MACOS? IF SYSCALL:NR-RMDIR SYSCALL:SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: LINUX-STAT-FIX ( n -- )
   STAT-BUF !
   5 STAT-BUF @ 16 A64ICODE:LDRW,  5 STAT-BUF @ 4 A64ICODE:STRW,
   5 STAT-BUF @ 48 A64ICODE:LDR,   6 STAT-BUF @ 88 A64ICODE:LDR,   7 STAT-BUF @ 96 A64ICODE:LDR,
   8 STAT-BUF @ 104 A64ICODE:LDR,  9 STAT-BUF @ 112 A64ICODE:LDR,
   5 STAT-BUF @ 96 A64ICODE:STR,   6 STAT-BUF @ 48 A64ICODE:STR,   7 STAT-BUF @ 56 A64ICODE:STR,
   8 STAT-BUF @ 64 A64ICODE:STR,   9 STAT-BUF @ 72 A64ICODE:STR, ;

: BSTAT64 ( -- )
   1 RT:G-POP  0 RT:G-POP
   A64ICODE:LBL STAT-OK !
   A64ICODE:LBL STAT-DONE !
   HB-TARGET-LINUX-ARM64? IF
      2 1 0 A64ICODE:ADDI,  1 0 0 A64ICODE:ADDI,  0 99 A64ICODE:MOVN,  3 0 A64ICODE:MOVZ,
      SYSCALL:NR-STAT64 SYSCALL:SYS,
      9 A64ASM:C-CS A64ICODE:CSET,  9 STAT-OK A64ICODE:LABEL@ A64ICODE:CBZ,
         STAT-DONE A64ICODE:LABEL@ A64ICODE:B,
      STAT-OK A64ICODE:LABEL@ A64ICODE:LBL,
      2 LINUX-STAT-FIX  9 0 A64ICODE:MOVZ,
      STAT-DONE A64ICODE:LABEL@ A64ICODE:LBL,
      FS-SYS-PUSH-FLAG
      exit
   THEN
   HB-TARGET-MACOS? IF SYSCALL:NR-STAT64 SYSCALL:SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BLSTAT64 ( -- )
   1 RT:G-POP  0 RT:G-POP  2 0 A64ICODE:MOVZ,  3 0 A64ICODE:MOVZ,  4 0 A64ICODE:MOVZ,  5 0 A64ICODE:MOVZ,
   A64ICODE:LBL STAT-OK !
   A64ICODE:LBL STAT-DONE !
   HB-TARGET-LINUX-ARM64? IF
      2 1 0 A64ICODE:ADDI,  1 0 0 A64ICODE:ADDI,  0 99 A64ICODE:MOVN,  3 SYSCALL:AT-SYMLINK-NOFOLLOW A64ICODE:MOVZ,
      SYSCALL:NR-LSTAT64 SYSCALL:SYS,
      9 A64ASM:C-CS A64ICODE:CSET,  9 STAT-OK A64ICODE:LABEL@ A64ICODE:CBZ,
         STAT-DONE A64ICODE:LABEL@ A64ICODE:B,
      STAT-OK A64ICODE:LABEL@ A64ICODE:LBL,
      2 LINUX-STAT-FIX  9 0 A64ICODE:MOVZ,
      STAT-DONE A64ICODE:LABEL@ A64ICODE:LBL,
      FS-SYS-PUSH-FLAG
      exit
   THEN
   HB-TARGET-MACOS? IF SYSCALL:NR-LSTAT64 SYSCALL:SYS,  FS-SYS-PUSH  exit THEN
   ENGINE-EMIT:TARGET-UNKNOWN ;

: BGETDIRENTRIES64 ( -- )
   3 RT:G-POP  2 RT:G-POP  1 RT:G-POP  0 RT:G-POP
   SYSCALL:NR-GETDIRENTRIES64 SYSCALL:SYS,  FS-SYS-PUSH ;

: C-FLUSH-X9-LINE ( -- )
   9 A64ICODE:DCCVAU,  A64ICODE:DSB-ISH,  9 A64ICODE:ICIVAU,  A64ICODE:DSB-ISH,  A64ICODE:ISB, ;

: BPATCH32 ( -- )                \ ( w addr -- ): store through the alias, cache-sync
   A RT:G-POP  B RT:G-POP
   11 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  11 9 11 A64ICODE:ADD,  10 11 0 A64ICODE:STRW,
   C-FLUSH-X9-LINE ;
;package

package NPUBWIN

public

\ code-publish ( src dst len -- ): write a whole emission into the code arena in
\ one copy. A publication is an append: dst must equal CP or it fails closed,
\ and CP advances in the same primitive, so a throw never sees CP disagree with
\ the arena. LFLUSH walks [x9, CP) once CP has advanced.
: BCODEPUBLISH ( -- )
   ENGINE-EMIT:A RT:G-POP  ENGINE-EMIT:B RT:G-POP  ENGINE-EMIT:C RT:G-POP     \ x9 = len, x10 = dst, x11 = src
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: loop:label done:label atcp:label :}
   A64ICODE:SP A64ICODE:SP 48 A64ICODE:SUBI,
   ENGINE-EMIT:A A64ICODE:SP 8 A64ICODE:STR,  ENGINE-EMIT:B A64ICODE:SP 16 A64ICODE:STR,  ENGINE-EMIT:C A64ICODE:SP 24 A64ICODE:STR,
   9 A64ICODE:SP 8 A64ICODE:LDR,  10 A64ICODE:SP 16 A64ICODE:LDR,
   10 CP A64ICODE:CMP,  A64ASM:C-EQ atcp A64ICODE:BCOND,
      s" E-SEAL-VIOLATION habu: code-publish destination is not CP:"
      EXIT-STATUS:SEAL-VIOLATION 10 CRASH:DIE-LINE-VALUE,
   atcp A64ICODE:LBL,
   9 A64ICODE:SP 8 A64ICODE:LDR,  10 A64ICODE:SP 16 A64ICODE:LDR,  11 A64ICODE:SP 24 A64ICODE:LDR,
   12 0 A64ICODE:MOVZ,                                    \ x12 = byte offset into the copy
   15 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  15 10 15 A64ICODE:ADD,   \ x15 = dst through the alias
   loop A64ICODE:LBL,
   12 9 A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
   13 11 12 A64ICODE:ADD,  14 13 0 A64ICODE:LDRW,
   13 15 12 A64ICODE:ADD,  14 13 0 A64ICODE:STRW,
   12 12 4 A64ICODE:ADDI,
   loop A64ICODE:B,
   done A64ICODE:LBL,
   9 A64ICODE:SP 8 A64ICODE:LDR,  10 A64ICODE:SP 16 A64ICODE:LDR,
   CP 10 9 A64ICODE:ADD,
   9 10 0 A64ICODE:ADDI,  ENGINE-EMIT:LFLUSH A64ICODE:LABEL@ A64ICODE:BL,
   A64ICODE:SP A64ICODE:SP 48 A64ICODE:ADDI, ;

\ xref-retarget ( start len recidx -- ): point one record at one routine. A
\ record's first two cells (entry address, CODE-SPAN length) mean something
\ only together; patch32 pokes would tear each cell across two stores, so one
\ primitive writes both whole. Length first, then the address by STLR: a
\ reader that LDARs START sees the length that belongs to it and the code
\ code-publish flushed.
: BXREFRETARGET ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: checked:label pending:label done:label bad:label badlen:label :}
   ENGINE-EMIT:A RT:G-POP  ENGINE-EMIT:B RT:G-POP  ENGINE-EMIT:C RT:G-POP     \ x9 = record index, x10 = len, x11 = start
   \ The index names a live row or the one pending row, record[NDICT]; one
   \ unsigned compare also rejects a negative index.
   9 NDICT A64ICODE:CMP,  A64ASM:C-HI bad A64ICODE:BCOND,
   14 CODE-SPAN:MAX A64ICODE:LIT64,  10 14 A64ICODE:CMP,  A64ASM:C-HI badlen A64ICODE:BCOND,  10 badlen A64ICODE:CBZ,
   checked A64ICODE:B,
   bad A64ICODE:LBL,
   s" E-SEAL-VIOLATION habu: xref-retarget record index above NDICT:"
   EXIT-STATUS:SEAL-VIOLATION ENGINE-EMIT:A CRASH:DIE-LINE-VALUE,
   badlen A64ICODE:LBL,
   s" E-SEAL-VIOLATION habu: xref-retarget length 0 or above CODE-SPAN:MAX:"
   EXIT-STATUS:SEAL-VIOLATION ENGINE-EMIT:B CRASH:DIE-LINE-VALUE,
   checked A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,  9 A64ICODE:SP 8 A64ICODE:STR,  10 A64ICODE:SP 16 A64ICODE:STR,  11 A64ICODE:SP 24 A64ICODE:STR,
   9 NDICT A64ICODE:CMP,  A64ASM:C-EQ pending A64ICODE:BCOND,
   14 XREG-RBASE DEF-OCC:PTR-CELL A64ICODE:LDR,
   15 9 3 A64ICODE:LSLI,  14 14 15 A64ICODE:ADD,
   15 0 A64ICODE:MOVZ,  15 14 8 A64ICODE:STR,
   pending A64ICODE:LBL,
   14 DREC A64ICODE:MOVZ,  9 9 14 A64ICODE:MUL,  9 DBASE 9 A64ICODE:ADD,   \ x9 = &record[idx]
   15 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  15 9 15 A64ICODE:ADD,   \ x15 = the record through the alias
   10 15 8 A64ICODE:STR,
   11 15 A64ICODE:STLR,
   9 A64ICODE:SP 8 A64ICODE:LDR,  9 NDICT A64ICODE:CMP,  A64ASM:C-EQ done A64ICODE:BCOND,
   EM-DEF-OCC:LISSUE A64ICODE:LABEL@ A64ICODE:BL,
   done A64ICODE:LBL,
   30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI, ;

;package

package ENGINE-EMIT
: BCLOSE ( -- )
   0 RT:G-POP  SYSCALL:NR-CLOSE SYSCALL:SYS, ;

: BCLOSE-RC ( -- )
   0 RT:G-POP  SYSCALL:NR-CLOSE SYSCALL:SYS,  SYS-PUSH ;

: BRBASE ( -- )
   9 XREG-RBASE RBASE-CELL A64ICODE:LDR,  9 RT:G-PUSH ;

: BCALLABLE ( -- )
   A64ICODE:LBL {: old:label :}
   A CP A64ICODE:CMP,  A64ASM:C-CC old A64ICODE:BCOND,  A64ICODE:ISB,
   old A64ICODE:LBL, ;

\ A reading quotation is its record's address with bit 63 set: x4 := the
\ record, x9 := its code (cell 0); a plain one passes untouched. No code from
\ these pops to their BLR writes x4.
: BRECORD ( -- )
   A64ICODE:LBL {: plain:label :}
   A 0 A64ICODE:CMPI,  A64ASM:C-PL plain A64ICODE:BCOND,
   4 A $7FFFFFFFFFFFFFFF A64ICODE:ANDI,  A 4 0 A64ICODE:LDR,
   plain A64ICODE:LBL, ;

: BEXEC ( -- )
   A RT:G-POP  BRECORD  BCALLABLE
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,  A A64ICODE:BLR,  30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI, ;

\ execute-floor ( xt -- flag ): run the xt, then flag whether it left XDS below
\ the base in S0-CELL, the comparison the interpreter's post-token depth floor
\ makes. Below the base, XDS is reset to the base before the flag is pushed, so
\ the push lands at the base and not in the low guard page. The FPRIM frame
\ holds the caller's x30 across the BLR.
: BEXECFLOOR ( -- )
   A64ICODE:LBL A64ICODE:LBL {: above:label old:label :}
   A RT:G-POP  A CP A64ICODE:CMP,  A64ASM:C-CC old A64ICODE:BCOND,  A64ICODE:ISB,
   old A64ICODE:LBL,  A A64ICODE:BLR,
   B 0 A64ICODE:MOVZ,
   A XREG-RBASE S0-CELL A64ICODE:LDR,  A64ICODE:XDS A A64ICODE:CMP,  A64ASM:C-CS above A64ICODE:BCOND,
      A64ICODE:XDS A 0 A64ICODE:ADDI,  B 0 A64ICODE:MOVN,
   above A64ICODE:LBL,
   B RT:G-PUSH ;

public
\ Wherever a definition ends, published or abandoned, its locals, block depth
\ and raw state clear with their token marks: no later one or image holds them.
: C-CLEAR-LOCALS ( -- )
   9 0 A64ICODE:MOVZ,
   9 XREG-RBASE LOCN-CELL A64ICODE:STR,  9 XREG-RBASE CAPDEPTH-CELL A64ICODE:STR,  9 XREG-RBASE RAWNEXT-CELL A64ICODE:STR,
   9 XREG-RBASE TOKLOCN-CELL A64ICODE:STR,  9 XREG-RBASE TOKDEPTH-CELL A64ICODE:STR,  9 XREG-RBASE TOKRAW-CELL A64ICODE:STR, ;

\ Definition state only: a Habu catch that survived the engine's BTHROW still owns
\ the handler, return and loop stacks that receive the error.
: EM-ABORT-COMPILE-STATE ( -- )
   9 0 A64ICODE:MOVZ,
   9 XREG-RBASE NCOMP-DISPATCH:COMP-LATCH-CELL A64ICODE:STR,
   9 XREG-RBASE VSP-CELL A64ICODE:STR,
   9 XREG-RBASE BODYLEN-CELL A64ICODE:STR,  9 XREG-RBASE EXITH-CELL A64ICODE:STR,
   9 XREG-RBASE PEND-CELL A64ICODE:STR,  9 XREG-RBASE CMM-CELL A64ICODE:STR,
   9 0 A64ICODE:MOVZ,
   9 XREG-RBASE TSIG-A-CELL A64ICODE:STR,   9 XREG-RBASE TSIG-U-CELL A64ICODE:STR,
   9 XREG-RBASE TCSIG-A-CELL A64ICODE:STR,  9 XREG-RBASE TCSIG-U-CELL A64ICODE:STR,
   9 XREG-RBASE CRSIG-A-CELL A64ICODE:STR,  9 XREG-RBASE CRSIG-U-CELL A64ICODE:STR,
   9 XREG-RBASE DOESB-CELL A64ICODE:STR,  9 XREG-RBASE CLAUSE-CELL A64ICODE:STR,
   9 XREG-RBASE TRUSTED-CELL A64ICODE:STR,
   C-CLEAR-LOCALS ;
private

\ catch ( xt -- exc ): a STACK-ABI:CATCH-BYTES handler frame chained through
\ HND-CELL saves the caller's execution frame: 0 prev-HND | 8 data-sp(x19) |
\ 16 machine-sp | 24 resume-pc | 32 link | 40 saved-RSP | 48 saved-LOOPSP, then
\ the STACK-ABI:CATCH-PEND cells. A return branches to CATCH-PUSH with x9 = 0; a
\ throw resumes at CATCH-RES with the code in x9, the saved PEND-CELL in x14 and
\ the frame unlinked below SP. CATCH-RES clears FAULT-MARK-CELL, as a delivered
\ memory fault ends at its catch, and abandons a definition opened since the
\ catch began, keeping one open before it. Its caller places CATCH-PUSH.
: BCATCH-RUN ( -- )
   A64ICODE:LBL {: kept:label :}
   A64ICODE:LBL CATCH-RES !
   A64ICODE:LBL CATCH-PUSH !
   A RT:G-POP  BRECORD  BCALLABLE
   A64ICODE:SP A64ICODE:SP STACK-ABI:CATCH-BYTES A64ICODE:SUBI,
   30 A64ICODE:SP 32 A64ICODE:STR,
   11 XREG-RBASE 8 A64ICODE:LDR,  11 A64ICODE:SP 0 A64ICODE:STR,
   19 A64ICODE:SP 8 A64ICODE:STR,
   13 A64ICODE:SP STACK-ABI:CATCH-BYTES A64ICODE:ADDI,  13 A64ICODE:SP 16 A64ICODE:STR,
   12 CATCH-RES A64ICODE:LABEL@ A64ICODE:ADR,  12 A64ICODE:SP 24 A64ICODE:STR,
   11 XREG-RBASE RSP-CELL A64ICODE:LDR,     11 A64ICODE:SP 40 A64ICODE:STR,
   11 XREG-RBASE LOOPSP-CELL A64ICODE:LDR,  11 A64ICODE:SP 48 A64ICODE:STR,
   11 XREG-RBASE PEND-CELL A64ICODE:LDR,  11 A64ICODE:SP STACK-ABI:CATCH-PEND A64ICODE:STR,
   11 XREG-RBASE STACK-ABI:BASE-CELL A64ICODE:LDR,  11 A64ICODE:SP STACK-ABI:CATCH-BASE A64ICODE:STR,
   11 XREG-RBASE STACK-ABI:CAP-CELL A64ICODE:LDR,  11 A64ICODE:SP STACK-ABI:CATCH-CAP A64ICODE:STR,
   11 XREG-RBASE GENIO-ABI:BUSY-CELL A64ICODE:LDR,  11 A64ICODE:SP STACK-ABI:CATCH-BUSY A64ICODE:STR,
   11 XREG-RBASE GENIO-ABI:ACTIVE-CELL A64ICODE:LDR,  11 A64ICODE:SP STACK-ABI:CATCH-ACTIVE A64ICODE:STR,
   14 A64ICODE:SP 0 A64ICODE:ADDI,  14 XREG-RBASE 8 A64ICODE:STR,
   9 A64ICODE:BLR,
   11 A64ICODE:SP 0 A64ICODE:LDR,  11 XREG-RBASE 8 A64ICODE:STR,
   30 A64ICODE:SP 32 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP STACK-ABI:CATCH-BYTES A64ICODE:ADDI,
   9 0 A64ICODE:MOVZ,  CATCH-PUSH A64ICODE:LABEL@ A64ICODE:B,
   CATCH-RES A64ICODE:LABEL@ A64ICODE:LBL,
   31 XREG-RBASE FAULT-MARK-CELL A64ICODE:STR,
   10 XREG-RBASE PEND-CELL A64ICODE:LDR,  10 kept A64ICODE:CBZ,
   10 14 A64ICODE:CMP,  A64ASM:C-EQ kept A64ICODE:BCOND,
   10 9 0 A64ICODE:ADDI,  EM-ABORT-COMPILE-STATE  9 10 0 A64ICODE:ADDI,   \ it clobbers x9, the code
   kept A64ICODE:LBL, ;

private
\ The unwind of a throw whose code is in x9: to the nearest catch handler.
: BTHROW-UNWIND ( -- )
   A64ICODE:LBL THROW-NOH !
   11 XREG-RBASE 8 A64ICODE:LDR,                                      \ x11 = nearest handler (HND-CELL)
   11 THROW-NOH A64ICODE:LABEL@ A64ICODE:CBZ,
   14 11 STACK-ABI:CATCH-BASE A64ICODE:LDR,
   19 11 8 A64ICODE:LDR,
   14 XREG-RBASE STACK-ABI:BASE-CELL A64ICODE:STR,
   14 11 STACK-ABI:CATCH-PEND A64ICODE:LDR,
   10 11 STACK-ABI:CATCH-CAP A64ICODE:LDR,  10 XREG-RBASE STACK-ABI:CAP-CELL A64ICODE:STR,
   10 11 STACK-ABI:CATCH-BUSY A64ICODE:LDR,  10 XREG-RBASE GENIO-ABI:BUSY-CELL A64ICODE:STR,
   10 11 STACK-ABI:CATCH-ACTIVE A64ICODE:LDR,  10 XREG-RBASE GENIO-ABI:ACTIVE-CELL A64ICODE:STR,
   10 11 40 A64ICODE:LDR,  10 XREG-RBASE RSP-CELL A64ICODE:STR,
   10 11 48 A64ICODE:LDR,  10 XREG-RBASE LOOPSP-CELL A64ICODE:STR,
   10 11 0 A64ICODE:LDR,  10 XREG-RBASE 8 A64ICODE:STR,
   30 11 32 A64ICODE:LDR,  12 11 24 A64ICODE:LDR,  13 11 16 A64ICODE:LDR,
   A64ICODE:SP 13 0 A64ICODE:ADDI,  12 A64ICODE:BR,
   THROW-NOH A64ICODE:LABEL@ A64ICODE:LBL,
     A64ICODE:LBL THROW-NOREC-FB !  A64ICODE:LBL THROW-NOREC-FB2 !
   10 XREG-RBASE UNCGH-CELL A64ICODE:LDR,  10 THROW-NOREC-FB A64ICODE:LABEL@ A64ICODE:CBZ, \ reporter installed? branch with x9 = code
   10 A64ICODE:BR,                                                 \ LUNCAUGHT (habu2.f): rc-map + report out-of-range codes
   THROW-NOREC-FB A64ICODE:LABEL@ A64ICODE:LBL,                             \ pre-install boot fallback: silent but never masked
   0 9 0 A64ICODE:ADDI,                                            \ code in [1,255] is kernel-representable: exit(code)
   9 1 A64ICODE:CMPI,    A64ASM:C-LT THROW-NOREC-FB2 A64ICODE:LABEL@ A64ICODE:BCOND,
   9 255 A64ICODE:CMPI,  A64ASM:C-GT THROW-NOREC-FB2 A64ICODE:LABEL@ A64ICODE:BCOND,
   SYSCALL:NR-EXIT-GROUP SYSCALL:SYS,
   THROW-NOREC-FB2 A64ICODE:LABEL@ A64ICODE:LBL,  0 EXIT-STATUS:UNCAUGHT A64ICODE:MOVZ,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS, ;

\ Adds 1 to THROWS-CELL, at each entry that begins a throw. It writes x10.
: BTHROW-COUNT ( -- )
   10 XREG-RBASE THROWS-CELL A64ICODE:LDR,  10 10 1 A64ICODE:ADDI,  10 XREG-RBASE THROWS-CELL A64ICODE:STR, ;

public
\ BTHROW ( code -- ): unwind to the nearest catch handler with the code as is.
: BTHROW ( -- )  A RT:G-POP  BTHROW-COUNT  BTHROW-UNWIND ;

\ The fault entry: the crash handler resumes a faulting thread here with the
\ code in x9, reaching it with ADR, so it sits next to the handler.
: EMIT-FAULT-REC ( -- )  CRASH:LFAULTREC A64ICODE:LABEL@ A64ICODE:LBL,  BTHROW-COUNT  BTHROW-UNWIND ;

private
\ The `throw` primitive: a code of 0 throws -9854, E-THROW-ZERO, so a catch
\ answers 0 only for a body that returned.
: BTHROW-NONZERO ( -- )
   A64ICODE:LBL {: nonzero:label :}
   A RT:G-POP  A nonzero A64ICODE:CBNZ,  A -9854 A64ICODE:LIT64,
   nonzero A64ICODE:LBL,  A RT:G-PUSH  BTHROW ;

\ Throws on the code in x9 that a catch frame took, for a primitive that has
\ popped that frame: it keeps the throw count, so a record the code carried
\ stays with it.
: BRETHROW ( -- )  BTHROW-UNWIND ;

\ BCATCH-FORWARD sits at CATCH-RES with the code in x9. With HALT-UNWIND-CELL
\ clear the code is delivered as always. Set, a TASK:HALT is unwinding the task:
\ while a frame remains below this one (HND nonzero) this is an inner catch,
\ which rethrows the code at once and never delivers it, so no code after a
\ `catch` observes a halt; the task's root catch (HND 0) clears the cell and
\ delivers the code, so the runner ends the task.
: BCATCH-FORWARD ( -- )
   A64ICODE:LBL A64ICODE:LBL {: inner:label keep:label :}
   10 XREG-RBASE HALT-UNWIND-CELL A64ICODE:LDR,  10 keep A64ICODE:CBZ,
   10 XREG-RBASE HND-CELL A64ICODE:LDR,  10 inner A64ICODE:CBNZ,
   10 XREG-RBASE HALT-UNWIND-CELL A64ICODE:STR,                 \ the root: x10 = HND = 0 clears it
   keep A64ICODE:B,
   inner A64ICODE:LBL,  BTHROW-UNWIND
   keep A64ICODE:LBL, ;

\ A catch that leaves its code in x9 and does not forward a halt, for the body
\ and cleanup of finally and c2-invoke, which take the halt into their own frame
\ and resume it after their cleanup.
: BCATCH-HELD ( -- )
   BCATCH-RUN  CATCH-PUSH A64ICODE:LABEL@ A64ICODE:LBL, ;

public
: BCATCH ( -- )
   BCATCH-RUN  BCATCH-FORWARD  CATCH-PUSH A64ICODE:LABEL@ A64ICODE:LBL,  9 RT:G-PUSH ;

private
\ `allot` hands out zeroed space. Fresh DATA pages are zero, so a positive
\ count zeroes only the part of [old DP, new DP) below the release high-water,
\ the space a lowered DP gave back; above it nothing has been handed out, and
\ the pages a large allot reserves stay untouched. A negative count raises the
\ high-water instead.
: BALLOT ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: grow:label zero:label store:label no-owner:label :}
   s" E-TASK-COMPILE" s" allot" B-TASK-GUARD
   A RT:G-POP  7 XREG-RBASE 0 A64ICODE:LDR,  7 7 A A64ICODE:ADD,  7 DP-CHECK
   A 0 A64ICODE:CMPI,  A64ASM:C-GE grow A64ICODE:BCOND,
   A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,  7 A64ICODE:SP 8 A64ICODE:STR,
   CHECKER-OWNER-ABI:DP-CUT-OFF no-owner DECL-OWNER:TARGET
   9 7 XREG-RBASE A64ICODE:SUB,  9 RT:G-PUSH  11 A64ICODE:BLR,
   no-owner A64ICODE:LBL,
   7 A64ICODE:SP 8 A64ICODE:LDR,  30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI,
   7 10 11 DP-LOWER,  store A64ICODE:B,
   grow A64ICODE:LBL,
   10 7 A A64ICODE:SUB,                                     \ x10 = old DP
   11 XREG-RBASE DPHW-CELL A64ICODE:LDR,  11 11 XREG-RBASE A64ICODE:ADD,  11 7 A64ICODE:CMP,  11 11 7 A64ASM:C-LS A64ICODE:CSEL,  \ x11 = min(high-water, new DP)
   zero A64ICODE:LBL,  10 11 A64ICODE:CMP,  A64ASM:C-CS store A64ICODE:BCOND,
      31 10 0 A64ICODE:STRB,  10 10 1 A64ICODE:ADDI,  zero A64ICODE:B,
   store A64ICODE:LBL,
   7 XREG-RBASE 0 A64ICODE:STR,
   A64ICODE:RET, ;

\ Selection takes the exact record a resolver has already borrowed. Validate
\ its region, stride and published bound before consulting the owner mapping.
: BDEFSELECT ( -- )
   A64ICODE:LBL A64ICODE:LBL {: bad:label done:label :}
   9 RT:G-POP
   9 DBASE A64ICODE:CMP,  A64ASM:C-CC bad A64ICODE:BCOND,
   10 9 DBASE A64ICODE:SUB,  11 DREC A64ICODE:MOVZ,
   12 10 11 A64ICODE:UDIV,  13 12 11 A64ICODE:MUL,
   10 13 A64ICODE:CMP,  A64ASM:C-NE bad A64ICODE:BCOND,
   12 NDICT A64ICODE:CMP,  A64ASM:C-CS bad A64ICODE:BCOND,
   14 XREG-RBASE DEF-OCC:PTR-CELL A64ICODE:LDR,
   13 12 3 A64ICODE:LSLI,  13 14 13 A64ICODE:ADD,  13 13 8 A64ICODE:LDR,
   13 bad A64ICODE:CBZ,
   12 RT:G-PUSH  13 RT:G-PUSH  done A64ICODE:B,
   bad A64ICODE:LBL,
   9 DEF-OCC:E-SELECT A64ICODE:LIT64,  9 RT:G-PUSH  BTHROW
   done A64ICODE:LBL, ;

\ A slot is a locator only. Equality with a nonzero issued occurrence is
\ checked before any byte of its record is read.
: BDEFRESOLVE ( -- )
   A64ICODE:LBL A64ICODE:LBL {: stale:label done:label :}
   10 RT:G-POP  9 RT:G-POP
   10 stale A64ICODE:CBZ,
   9 NDICT A64ICODE:CMP,  A64ASM:C-CS stale A64ICODE:BCOND,
   11 XREG-RBASE DEF-OCC:PTR-CELL A64ICODE:LDR,
   12 9 3 A64ICODE:LSLI,  12 11 12 A64ICODE:ADD,  12 12 8 A64ICODE:LDR,
   12 10 A64ICODE:CMP,  A64ASM:C-NE stale A64ICODE:BCOND,
   11 DREC A64ICODE:MOVZ,  9 9 11 A64ICODE:MUL,  9 DBASE 9 A64ICODE:ADD,
   9 RT:G-PUSH  done A64ICODE:B,
   stale A64ICODE:LBL,
   9 DEF-OCC:E-STALE A64ICODE:LIT64,  9 RT:G-PUSH  BTHROW
   done A64ICODE:LBL, ;

\ A permitted CP rewind can release the tail of a still published code span.
\ Retire each affected slot before those bytes are reusable. Namespace rows
\ contain WIDs in their first two cells and have no code span.
: EMIT-DEF-OCC-RECLAIM ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: loop:label next:label span:label done:label :}
   EM-DEF-OCC:LRECLAIM A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,
   PROF-ABI:LPROFRECLAIM A64ICODE:LABEL@ A64ICODE:BL,
   30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI,
   10 0 A64ICODE:MOVZ,
   loop A64ICODE:LBL,
      10 NDICT A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      11 DREC A64ICODE:MOVZ,  11 10 11 A64ICODE:MUL,  11 DBASE 11 A64ICODE:ADD,
      12 11 40 A64ICODE:LDR,  13 WL:RETIRED invert A64ICODE:MOVN,
      12 13 A64ICODE:CMP,  A64ASM:C-EQ span A64ICODE:BCOND,
      12 0 A64ICODE:CMPI,  A64ASM:C-LT next A64ICODE:BCOND,                          \ a package row, live or retired
   span A64ICODE:LBL,
      13 11 0 A64ICODE:LDR,  13 CP A64ICODE:CMP,  A64ASM:C-CS next A64ICODE:BCOND,
      14 11 8 A64ICODE:LDR,
      15 13 14 A64ICODE:ADD,
      15 9 A64ICODE:CMP,  A64ASM:C-LS next A64ICODE:BCOND,
      12 XREG-RBASE DEF-OCC:PTR-CELL A64ICODE:LDR,
      14 10 3 A64ICODE:LSLI,  12 12 14 A64ICODE:ADD,
      14 0 A64ICODE:MOVZ,  14 12 8 A64ICODE:STR,
   next A64ICODE:LBL,
      10 10 1 A64ICODE:ADDI,  loop A64ICODE:B,
   done A64ICODE:LBL,  A64ICODE:RET, ;

\ Pop B-EVAL-CLOSED's frame: the text's stack (x11) back to the pool, the
\ caller's extent and cursor back in force.
: CLOSED-LEAVE ( -- )
   11 A64ICODE:SP 24 A64ICODE:LDR,
   12 XREG-RBASE CLOSED-FREE-CELL A64ICODE:LDR,  12 11 0 A64ICODE:STR,  11 XREG-RBASE CLOSED-FREE-CELL A64ICODE:STR,
   12 A64ICODE:SP 0 A64ICODE:LDR,  12 XREG-RBASE STACK-ABI:BASE-CELL A64ICODE:STR,
   12 A64ICODE:SP 8 A64ICODE:LDR,  12 XREG-RBASE STACK-ABI:CAP-CELL A64ICODE:STR,
   A64ICODE:XDS A64ICODE:SP 16 A64ICODE:LDR,
   A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI, ;

\ evaluate-closed ( str -- ): the text runs on a guarded stack of its own
\ from the CLOSED-FREE-CELL pool; this frame holds the caller's base, capacity
\ and cursor at [0] [8] [16] and the text's stack at [24], which CLOSED-LEAVE
\ reads. PROVIDED-XT:EVALUATE-CLOSED-CELL's word runs the text under this
\ frame's catch and rejects a text that leaves cells; the frame gives the
\ stack back, then throws what the word threw again through BRETHROW.
: B-EVAL-CLOSED ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: have:label take:label done:label :}
   s" E-TASK-COMPILE" s" evaluate-closed" B-TASK-GUARD
   B RT:G-POP  A RT:G-POP                                  \ x10 = u, x9 = a
   A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,
   11 XREG-RBASE STACK-ABI:BASE-CELL A64ICODE:LDR,  11 A64ICODE:SP 0 A64ICODE:STR,
   12 XREG-RBASE STACK-ABI:CAP-CELL A64ICODE:LDR,  12 A64ICODE:SP 8 A64ICODE:STR,
   A64ICODE:XDS A64ICODE:SP 16 A64ICODE:STR,
   13 XREG-RBASE CLOSED-FREE-CELL A64ICODE:LDR,  13 have A64ICODE:CBNZ,
   11 9 0 A64ICODE:ADDI,                                      \ a waits in x11: the map never writes x10 or x11
   STACK-ABI:PAGE-BYTES 13 STACK-GUARD:EMIT-MAP
   9 11 0 A64ICODE:ADDI,
   take A64ICODE:B,
   have A64ICODE:LBL,
   12 13 0 A64ICODE:LDR,  12 XREG-RBASE CLOSED-FREE-CELL A64ICODE:STR,       \ an idle stack's first cell links the next
   take A64ICODE:LBL,
   13 A64ICODE:SP 24 A64ICODE:STR,
   13 XREG-RBASE STACK-ABI:BASE-CELL A64ICODE:STR,
   12 STACK-ABI:PAGE-BYTES A64ICODE:LIT64,  12 XREG-RBASE STACK-ABI:CAP-CELL A64ICODE:STR,
   A64ICODE:XDS 13 0 A64ICODE:ADDI,
   12 XREG-RBASE PROVIDED-XT:EVALUATE-CLOSED-CELL A64ICODE:LDR,
   9 RT:G-PUSH  10 RT:G-PUSH  12 RT:G-PUSH                    \ a u for the cell's word, under its xt
   BCATCH-RUN
   CATCH-PUSH A64ICODE:LABEL@ A64ICODE:LBL,                            \ x9 = the code, 0 on return
   CLOSED-LEAVE  9 done A64ICODE:CBZ,
   BRETHROW
   done A64ICODE:LBL, ;

\ source-file-run ( xt -- ): run a file's unit on a pool stack and rethrow
\ what it throws; the unit checks the cells the file leaves.
: B-SOURCE-FILE-RUN ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
      {: have:label take:label caught:label done:label :}
   s" E-TASK-COMPILE" s" source-file-run" B-TASK-GUARD
   A RT:G-POP  BCALLABLE
   \ The child uses a fresh guarded stack; retain the parent action's stack
   \ interval after the loader consumed its arguments and before that swap.
   A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,
   11 XREG-RBASE STACK-ABI:BASE-CELL A64ICODE:LDR,  11 A64ICODE:SP 0 A64ICODE:STR,
   12 XREG-RBASE STACK-ABI:CAP-CELL A64ICODE:LDR,   12 A64ICODE:SP 8 A64ICODE:STR,
   A64ICODE:XDS A64ICODE:SP 16 A64ICODE:STR,
   9 A64ICODE:SP 24 A64ICODE:STR,
   13 XREG-RBASE CLOSED-FREE-CELL A64ICODE:LDR,  13 have A64ICODE:CBNZ,
   STACK-ABI:PAGE-BYTES 13 STACK-GUARD:EMIT-MAP
   take A64ICODE:B,
   have A64ICODE:LBL,
   12 13 0 A64ICODE:LDR,  12 XREG-RBASE CLOSED-FREE-CELL A64ICODE:STR,
   take A64ICODE:LBL,
   9 A64ICODE:SP 24 A64ICODE:LDR,  13 A64ICODE:SP 24 A64ICODE:STR,
   13 XREG-RBASE STACK-ABI:BASE-CELL A64ICODE:STR,
   12 STACK-ABI:PAGE-BYTES A64ICODE:LIT64,  12 XREG-RBASE STACK-ABI:CAP-CELL A64ICODE:STR,
   A64ICODE:XDS 13 0 A64ICODE:ADDI,
   \ Same handler shape as BCATCH, `caught` clearing FAULT-MARK-CELL as
   \ CATCH-RES does, but do not push its status on the callback's stack: the
   \ clean callback may have filled that stack exactly. It reads no
   \ HALT-UNWIND-CELL: a task never reaches it, B-TASK-GUARD above exits on a
   \ task's thread.
   A64ICODE:SP A64ICODE:SP STACK-ABI:CATCH-BYTES A64ICODE:SUBI,
   30 A64ICODE:SP 32 A64ICODE:STR,
   11 XREG-RBASE 8 A64ICODE:LDR,  11 A64ICODE:SP 0 A64ICODE:STR,
   19 A64ICODE:SP 8 A64ICODE:STR,
   13 A64ICODE:SP STACK-ABI:CATCH-BYTES A64ICODE:ADDI,  13 A64ICODE:SP 16 A64ICODE:STR,
   12 caught A64ICODE:ADR,  12 A64ICODE:SP 24 A64ICODE:STR,
   11 XREG-RBASE RSP-CELL A64ICODE:LDR,     11 A64ICODE:SP 40 A64ICODE:STR,
   11 XREG-RBASE LOOPSP-CELL A64ICODE:LDR,  11 A64ICODE:SP 48 A64ICODE:STR,
   11 XREG-RBASE STACK-ABI:BASE-CELL A64ICODE:LDR,  11 A64ICODE:SP STACK-ABI:CATCH-BASE A64ICODE:STR,
   11 XREG-RBASE STACK-ABI:CAP-CELL A64ICODE:LDR,  11 A64ICODE:SP STACK-ABI:CATCH-CAP A64ICODE:STR,
   11 XREG-RBASE GENIO-ABI:BUSY-CELL A64ICODE:LDR,  11 A64ICODE:SP STACK-ABI:CATCH-BUSY A64ICODE:STR,
   11 XREG-RBASE GENIO-ABI:ACTIVE-CELL A64ICODE:LDR,  11 A64ICODE:SP STACK-ABI:CATCH-ACTIVE A64ICODE:STR,
   14 A64ICODE:SP 0 A64ICODE:ADDI,  14 XREG-RBASE 8 A64ICODE:STR,
   9 A64ICODE:BLR,
   11 A64ICODE:SP 0 A64ICODE:LDR,  11 XREG-RBASE 8 A64ICODE:STR,
   30 A64ICODE:SP 32 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP STACK-ABI:CATCH-BYTES A64ICODE:ADDI,
   9 0 A64ICODE:MOVZ,
   caught A64ICODE:LBL,
   31 XREG-RBASE FAULT-MARK-CELL A64ICODE:STR,
   11 A64ICODE:SP 24 A64ICODE:LDR,
   12 XREG-RBASE CLOSED-FREE-CELL A64ICODE:LDR,  12 11 0 A64ICODE:STR,  11 XREG-RBASE CLOSED-FREE-CELL A64ICODE:STR,
   12 A64ICODE:SP 0 A64ICODE:LDR,  12 XREG-RBASE STACK-ABI:BASE-CELL A64ICODE:STR,
   12 A64ICODE:SP 8 A64ICODE:LDR,  12 XREG-RBASE STACK-ABI:CAP-CELL A64ICODE:STR,
   A64ICODE:XDS A64ICODE:SP 16 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI,
   9 done A64ICODE:CBZ,
   BRETHROW
   done A64ICODE:LBL, ;

\ Counts a new throw for the code to go on, and moves the kept record's stamp to
\ it when the stamp is x11, the count when that code landed. It writes x10, x12.
: BFINALLY-COUNT ( -- )
   12 XREG-RBASE THROWS-CELL A64ICODE:LDR,  12 12 1 A64ICODE:ADDI,  12 XREG-RBASE THROWS-CELL A64ICODE:STR,
   10 XREG-RBASE STAMP-CELL A64ICODE:LDR,  10 11 A64ICODE:CMP,  10 12 10 A64ASM:C-EQ A64ICODE:CSEL,  10 XREG-RBASE STAMP-CELL A64ICODE:STR, ;

\ finally ( body cleanup -- ). Frame: [0] cleanup | [8] the body's code | [16] the halt its throw carried, set again
\ once the cleanup ends or halts | [24] the throw count when [8] was stored. A cleanup's nonzero code goes to [8]
\ only when [8] is 0: a return or a PAUSE halt. The code goes on as a new throw, which carries the kept record
\ when the throw counted [24] did.
: BFINALLY ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: keep:label calm:label throw:label :}
   A64ICODE:LBL FINALLY-DONE !
   A RT:G-POP  BCALLABLE
   A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,  9 A64ICODE:SP 0 A64ICODE:STR,
   BCATCH-HELD
   9 A64ICODE:SP 8 A64ICODE:STR,  10 XREG-RBASE THROWS-CELL A64ICODE:LDR,  10 A64ICODE:SP 24 A64ICODE:STR,
   10 XREG-RBASE HALT-UNWIND-CELL A64ICODE:LDR,  10 A64ICODE:SP 16 A64ICODE:STR,
   10 0 A64ICODE:MOVZ,  10 XREG-RBASE HALT-UNWIND-CELL A64ICODE:STR,
   9 A64ICODE:SP 0 A64ICODE:LDR,  9 RT:G-PUSH  BCATCH-HELD
   9 keep A64ICODE:CBZ,  10 A64ICODE:SP 8 A64ICODE:LDR,  10 keep A64ICODE:CBNZ,
   9 A64ICODE:SP 8 A64ICODE:STR,  10 XREG-RBASE THROWS-CELL A64ICODE:LDR,  10 A64ICODE:SP 24 A64ICODE:STR,
   keep A64ICODE:LBL,
   10 XREG-RBASE HALT-UNWIND-CELL A64ICODE:LDR,  10 calm A64ICODE:CBZ,
   10 A64ICODE:SP 16 A64ICODE:STR,
   calm A64ICODE:LBL,
   9 A64ICODE:SP 8 A64ICODE:LDR,  10 A64ICODE:SP 16 A64ICODE:LDR,  11 A64ICODE:SP 24 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI,
   10 XREG-RBASE HALT-UNWIND-CELL A64ICODE:STR,                 \ the halt taken, 0 when none
   10 throw A64ICODE:CBNZ,
   9 FINALLY-DONE A64ICODE:LABEL@ A64ICODE:CBZ,
   throw A64ICODE:LBL,
   BFINALLY-COUNT  BRETHROW
   FINALLY-DONE A64ICODE:LABEL@ A64ICODE:LBL, ;

\ c2-invoke ( body cleanup finish -- ): both calls have independent handlers.
\ The finisher sees the selected code while the body's result row is still live;
\ a nonzero code is rethrown before compiled code reloads that row.
\ Frame: [0] cleanup | [8] finisher | [16] code | [24] halt | [32] the throw count
\ when [16] was stored | [40] unused. The cleanup's nonzero code goes to [16] only
\ when [16] is 0. A halt is taken and resumed as BFINALLY does, one begun inside
\ the cleanup included, and the finisher runs with the cell clear. The code goes
\ on as a new throw, which carries the kept record when the throw counted [32]
\ did.
: BC2-INVOKE ( -- )
   A64ICODE:LBL A64ICODE:LBL {: calm:label throw:label :}
   A64ICODE:LBL C2-INVOKE-DONE !
   A64ICODE:LBL C2-INVOKE-CLEAN !
   A RT:G-POP  10 A 0 A64ICODE:ADDI,
   A RT:G-POP
   A64ICODE:SP A64ICODE:SP 48 A64ICODE:SUBI,  A A64ICODE:SP 0 A64ICODE:STR,  10 A64ICODE:SP 8 A64ICODE:STR,
   BCATCH-HELD
   9 A64ICODE:SP 16 A64ICODE:STR,  10 XREG-RBASE THROWS-CELL A64ICODE:LDR,  10 A64ICODE:SP 32 A64ICODE:STR,
   10 XREG-RBASE HALT-UNWIND-CELL A64ICODE:LDR,  10 A64ICODE:SP 24 A64ICODE:STR,
   10 0 A64ICODE:MOVZ,  10 XREG-RBASE HALT-UNWIND-CELL A64ICODE:STR,
   9 A64ICODE:SP 0 A64ICODE:LDR,  9 RT:G-PUSH  BCATCH-HELD
   9 C2-INVOKE-CLEAN A64ICODE:LABEL@ A64ICODE:CBZ,  10 A64ICODE:SP 16 A64ICODE:LDR,  10 C2-INVOKE-CLEAN A64ICODE:LABEL@ A64ICODE:CBNZ,
   9 A64ICODE:SP 16 A64ICODE:STR,  10 XREG-RBASE THROWS-CELL A64ICODE:LDR,  10 A64ICODE:SP 32 A64ICODE:STR,
   C2-INVOKE-CLEAN A64ICODE:LABEL@ A64ICODE:LBL,
   10 XREG-RBASE HALT-UNWIND-CELL A64ICODE:LDR,  10 calm A64ICODE:CBZ,
   10 A64ICODE:SP 24 A64ICODE:STR,
   10 0 A64ICODE:MOVZ,  10 XREG-RBASE HALT-UNWIND-CELL A64ICODE:STR,
   calm A64ICODE:LBL,
   9 A64ICODE:SP 16 A64ICODE:LDR,  9 RT:G-PUSH
   9 A64ICODE:SP 8 A64ICODE:LDR,  BRECORD  BCALLABLE  9 A64ICODE:BLR,
   9 A64ICODE:SP 16 A64ICODE:LDR,  10 A64ICODE:SP 24 A64ICODE:LDR,  11 A64ICODE:SP 32 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 48 A64ICODE:ADDI,
   10 XREG-RBASE HALT-UNWIND-CELL A64ICODE:STR,                 \ the halt taken, 0 when none
   10 throw A64ICODE:CBNZ,
   9 C2-INVOKE-DONE A64ICODE:LABEL@ A64ICODE:CBZ,
   throw A64ICODE:LBL,
   BFINALLY-COUNT  BRETHROW
   C2-INVOKE-DONE A64ICODE:LABEL@ A64ICODE:LBL, ;

\ (R base bound value[W] callback W bytes align frame) -> (R base bytes
\ callback) on INIT-OPEN's PENDING frame: validate all, arm LIVE, then store.
: BC2-INIT-STOW ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: bad:label copy:label done:label :}
   9 RT:G-POP   10 RT:G-POP   11 RT:G-POP   12 RT:G-POP   13 RT:G-POP
   12 0 A64ICODE:CMPI, A64ASM:C-LE bad A64ICODE:BCOND,
   11 0 A64ICODE:CMPI, A64ASM:C-LE bad A64ICODE:BCOND,
   10 8 A64ICODE:CMPI, A64ASM:C-NE bad A64ICODE:BCOND,
   15 12 3 A64ICODE:LSLI,  11 15 A64ICODE:CMP, A64ASM:C-NE bad A64ICODE:BCOND,
   14 A64ICODE:XDS 15 A64ICODE:SUB,  14 14 16 A64ICODE:SUBI,
   16 14 0 A64ICODE:LDR,  17 14 8 A64ICODE:LDR,
   16 0 A64ICODE:CMPI, A64ASM:C-EQ bad A64ICODE:BCOND,
   15 16 7 A64ICODE:ANDI,  15 bad A64ICODE:CBNZ,
   17 11 A64ICODE:CMP, A64ASM:C-LT bad A64ICODE:BCOND,
   15 9 0 A64ICODE:LDR,  15 1 A64ICODE:CMPI, A64ASM:C-NE bad A64ICODE:BCOND,
   16 9 16 A64ICODE:STR,  11 9 24 A64ICODE:STR,  17 9 40 A64ICODE:STR,
   15 2 A64ICODE:MOVZ,  15 9 0 A64ICODE:STR,
   copy A64ICODE:LBL,
      14 RT:G-POP
      15 12 1 A64ICODE:SUBI,  15 15 3 A64ICODE:LSLI,  15 16 15 A64ICODE:ADD,
      14 15 0 A64ICODE:STR,
      12 12 1 A64ICODE:SUBI,  12 copy A64ICODE:CBNZ,
   14 RT:G-POP  11 RT:G-PUSH  13 RT:G-PUSH  done A64ICODE:B,
   bad A64ICODE:LBL,
   9 -6101 A64ICODE:LIT64,  9 RT:G-PUSH  BTHROW
   done A64ICODE:LBL, ;

\ (R base bound count seed[W] callback W stride align frame) ->
\ (R base count callback). One frame owns and clears count * stride bytes.
: BC2-RECORDS-STOW ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
   {: bad:label seed:label first:label copy:label cell-copy:label done:label exit-code:label :}
   9 RT:G-POP  10 RT:G-POP  11 RT:G-POP  12 RT:G-POP  13 RT:G-POP
   12 0 A64ICODE:CMPI, A64ASM:C-LE bad A64ICODE:BCOND,
   11 0 A64ICODE:CMPI, A64ASM:C-LE bad A64ICODE:BCOND,
   10 8 A64ICODE:CMPI, A64ASM:C-NE bad A64ICODE:BCOND,
   16 12 3 A64ICODE:LSLI,  11 16 A64ICODE:CMP, A64ASM:C-NE bad A64ICODE:BCOND,
   14 A64ICODE:XDS 11 A64ICODE:SUB,  14 14 24 A64ICODE:SUBI,
   15 14 0 A64ICODE:LDR,  16 14 8 A64ICODE:LDR,  17 14 16 A64ICODE:LDR,
   15 0 A64ICODE:CMPI, A64ASM:C-EQ bad A64ICODE:BCOND,
   14 15 7 A64ICODE:ANDI,  14 bad A64ICODE:CBNZ,
   17 0 A64ICODE:CMPI, A64ASM:C-LT bad A64ICODE:BCOND,
   16 0 A64ICODE:CMPI, A64ASM:C-LT bad A64ICODE:BCOND,
   14 16 11 A64ICODE:UDIV,  17 14 A64ICODE:CMP, A64ASM:C-HI bad A64ICODE:BCOND,
   14 9 0 A64ICODE:LDR,  14 1 A64ICODE:CMPI, A64ASM:C-NE bad A64ICODE:BCOND,
   15 9 16 A64ICODE:STR,
   14 17 11 A64ICODE:MUL,  14 9 24 A64ICODE:STR,
   16 9 40 A64ICODE:STR,
   14 2 A64ICODE:MOVZ,  14 9 0 A64ICODE:STR,
   17 first A64ICODE:CBNZ,
   seed A64ICODE:LBL,
      14 RT:G-POP
      12 12 1 A64ICODE:SUBI,  12 seed A64ICODE:CBNZ,
      done A64ICODE:B,
   first A64ICODE:LBL,
      14 RT:G-POP
      16 12 1 A64ICODE:SUBI,  16 16 3 A64ICODE:LSLI,  16 15 16 A64ICODE:ADD,
      14 16 0 A64ICODE:STR,
      12 12 1 A64ICODE:SUBI,  12 first A64ICODE:CBNZ,
   17 17 1 A64ICODE:SUBI,  17 done A64ICODE:CBZ,
   14 15 11 A64ICODE:ADD,
   copy A64ICODE:LBL,
      16 15 0 A64ICODE:ADDI,
      12 11 3 A64ICODE:LSRI,
   cell-copy A64ICODE:LBL,
      10 16 0 A64ICODE:LDR,  10 14 0 A64ICODE:STR,
      16 16 8 A64ICODE:ADDI,  14 14 8 A64ICODE:ADDI,
      12 12 1 A64ICODE:SUBI,  12 cell-copy A64ICODE:CBNZ,
      17 17 1 A64ICODE:SUBI,  17 copy A64ICODE:CBNZ,
   done A64ICODE:LBL,
   17 RT:G-POP  16 RT:G-POP
   17 9 24 A64ICODE:LDR,  17 17 11 A64ICODE:UDIV,
   17 RT:G-PUSH  13 RT:G-PUSH  exit-code A64ICODE:B,
   bad A64ICODE:LBL,
   9 -6101 A64ICODE:LIT64,  9 RT:G-PUSH  BTHROW
   exit-code A64ICODE:LBL, ;

: BWORDLIST ( -- )
   OVERLAY-EMIT:GUARD,
   9 XREG-RBASE WIDN-CELL A64ICODE:LDR,  9 RT:G-PUSH  9 9 1 A64ICODE:ADDI,  9 XREG-RBASE WIDN-CELL A64ICODE:STR, ;

: BGETCUR ( -- )
   9 XREG-RBASE CUR-CELL A64ICODE:LDR,  9 RT:G-PUSH ;

\ set-check ( xt -- ): install the checker hook. 0 disables checking and parks
\ an armed hook with its preflight in the SUSP-HOOK pair; arming the parked
\ hook while the preflight cell is empty brings that preflight back, and every
\ arm clears the pair. A non-zero xt must
\ lie in [DBASE, CP) (x26/x28): every real hook is a source-loaded word compiled
\ there, and a wild install (1, a DATA address, a baked-primitive xt) dies here
\ with a named rc-70 diagnostic instead of at the next publish's BLR. The
\ window cannot tell a word entry from another in-range address.
: BSETCHECK ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: bad:label disarm:label kept:label armed:label done:label msg:label :}
   S\" set-check: invalid checker xt\n" {: text:str :}
   A RT:G-POP                               \ x9 = candidate xt
   9 disarm A64ICODE:CBZ,
      9 DBASE A64ICODE:CMP,  A64ASM:C-CC bad A64ICODE:BCOND,
      9 CP A64ICODE:CMP,     A64ASM:C-CS bad A64ICODE:BCOND,
      A XREG-RBASE HOOK-CELL A64ICODE:STR,
      11 XREG-RBASE COMPILE-PREFLIGHT-CELL A64ICODE:LDR,  11 armed A64ICODE:CBNZ,
      11 XREG-RBASE SUSP-HOOK-CELL A64ICODE:LDR,  9 11 A64ICODE:CMP,  A64ASM:C-NE armed A64ICODE:BCOND,
      11 XREG-RBASE SUSP-PREFLIGHT-CELL A64ICODE:LDR,  11 XREG-RBASE COMPILE-PREFLIGHT-CELL A64ICODE:STR,
   armed A64ICODE:LBL,
      10 0 A64ICODE:MOVZ,  10 XREG-RBASE SUSP-HOOK-CELL A64ICODE:STR,  10 XREG-RBASE SUSP-PREFLIGHT-CELL A64ICODE:STR,
      done A64ICODE:B,
   disarm A64ICODE:LBL,
      10 XREG-RBASE HOOK-CELL A64ICODE:LDR,  10 kept A64ICODE:CBZ,   \ already off: the parked pair stands
      10 XREG-RBASE SUSP-HOOK-CELL A64ICODE:STR,
      11 XREG-RBASE COMPILE-PREFLIGHT-CELL A64ICODE:LDR,  11 XREG-RBASE SUSP-PREFLIGHT-CELL A64ICODE:STR,
   kept A64ICODE:LBL,
      A XREG-RBASE HOOK-CELL A64ICODE:STR,  A XREG-RBASE COMPILE-PREFLIGHT-CELL A64ICODE:STR,
      done A64ICODE:B,
   bad A64ICODE:LBL,
      0 2 A64ICODE:MOVZ,  1 msg A64ICODE:ADR,  2 text STR:LEN A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
      0 EXIT-STATUS:REJECTED A64ICODE:MOVZ,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS,
   msg A64ICODE:LBL,  text A64ICODE:BYTES,
   done A64ICODE:LBL, ;

\ The same readiness supplies the live tick gate and the verifier's entry
\ observation. x11 is the installed query, x12 its owner, both zero if absent.
: C-TICK-QUERY ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: noquery:label query:label done:label :}
   11 0 A64ICODE:MOVZ,  12 0 A64ICODE:MOVZ,
   \ The cold prefix installs the bridge before its seal watermark exists.
   9 XREG-RBASE SEAL-NDICT-CELL A64ICODE:LDR,  9 noquery A64ICODE:CBZ,
   \ A replacement target bypasses the source query.
   13 XREG-RBASE NCOMP-DISPATCH:TARGET-DECL-CELL A64ICODE:LDR,  13 query A64ICODE:CBZ,
   12 XREG-RBASE NCOMP-DISPATCH:DECL-CELL A64ICODE:LDR,
   13 12 A64ICODE:CMP,  A64ASM:C-NE noquery A64ICODE:BCOND,
   query A64ICODE:LBL,
   \ This is the installed code identity, independent of later name lookup.
   12 XREG-RBASE NCOMP-DISPATCH:DECL-CELL A64ICODE:LDR,  12 noquery A64ICODE:CBZ,
   11 12 NCOMP-DISPATCH:DECL-TRUSTED-TICK-OFF A64ICODE:LDR,
   11 noquery A64ICODE:CBZ,
   done A64ICODE:B,
   noquery A64ICODE:LBL,  11 0 A64ICODE:MOVZ,  12 0 A64ICODE:MOVZ,
   done A64ICODE:LBL, ;

public
\ Register n gets the latch the next `:` stores in COMP-LATCH-CELL.
: COMP-LATCH ( n -- ) {: r:n :}
   A64ICODE:LBL {: done:label :}
   r XREG-RBASE NCOMP-DISPATCH:COMP-CELL A64ICODE:LDR,  r done A64ICODE:CBZ,
   r 1 A64ICODE:MOVZ,
   done A64ICODE:LBL, ;

private
\ A prospective answer at this boundary: 0 means checking first, 1 means the
\ trusted gate would run first. The verifier captures it before loading tools.
: BTICKORDER ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: next:label checking:label latched:label done:label :}
   9 XREG-RBASE PEND-CELL A64ICODE:LDR,  9 next A64ICODE:CBZ,
      9 XREG-RBASE NCOMP-DISPATCH:COMP-LATCH-CELL A64ICODE:LDR,
      latched A64ICODE:B,
   next A64ICODE:LBL,
      9 COMP-LATCH
   latched A64ICODE:LBL,
   9 checking A64ICODE:CBNZ,
   C-TICK-QUERY
   11 checking A64ICODE:CBZ,
   9 1 A64ICODE:MOVZ,  9 RT:G-PUSH  12 RT:G-PUSH
   done A64ICODE:B,
   checking A64ICODE:LBL,
   9 0 A64ICODE:MOVZ,  9 RT:G-PUSH  9 RT:G-PUSH
   done A64ICODE:LBL, ;

\ set-preflight ( xt -- ): single-assignment installer for the checker-owned
\ immediate preflight. While it is empty the checker may install one live code
\ entry, and an identical reinstall is inert. While checking stays armed, even
\ a TRUSTED: caller cannot replace the hook.
: BSETPREFLIGHT ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
   {: bad:label invalid:label empty:label emit:label done:label msg:label invalid-msg:label :}
   S\" set-preflight: invalid or replaced hook\n" {: text:str :}
   S\" set-preflight: invalid hook\n" {: invalid-text:str :}
   A RT:G-POP
   10 XREG-RBASE COMPILE-PREFLIGHT-CELL A64ICODE:LDR,
   10 empty A64ICODE:CBZ,
      10 A A64ICODE:CMP,  A64ASM:C-EQ done A64ICODE:BCOND,
      bad A64ICODE:B,
   empty A64ICODE:LBL,
      A invalid A64ICODE:CBZ,
      A DBASE A64ICODE:CMP,  A64ASM:C-CC invalid A64ICODE:BCOND,
      A CP A64ICODE:CMP,     A64ASM:C-CS invalid A64ICODE:BCOND,
   A XREG-RBASE COMPILE-PREFLIGHT-CELL A64ICODE:STR,
   done A64ICODE:B,
   bad A64ICODE:LBL,
      1 msg A64ICODE:ADR,  2 text STR:LEN A64ICODE:MOVZ,  emit A64ICODE:B,
   invalid A64ICODE:LBL,
      1 invalid-msg A64ICODE:ADR,  2 invalid-text STR:LEN A64ICODE:MOVZ,
   emit A64ICODE:LBL,
      0 2 A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
      0 EXIT-STATUS:REJECTED A64ICODE:MOVZ,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS,
   msg A64ICODE:LBL,  text A64ICODE:BYTES,
   invalid-msg A64ICODE:LBL,  invalid-text A64ICODE:BYTES,
   done A64ICODE:LBL, ;
;package

package ENGINE-EMIT
\ wide-mark ( -- ): set DNAME-WIDE on the newest record: its effect carries a
\ wider-than-cell layout value, so interpret-mode execute and tick reject it.
\ It stores through the region's RW alias: the record's own address is
\ read-only. The checker calls it at signature-record time.
: BWIDEMARK ( -- )
   9 NDICT 0 A64ICODE:ADDI,  9 9 1 A64ICODE:SUBI,  10 DREC A64ICODE:MOVZ,  9 9 10 A64ICODE:MUL,  9 DBASE 9 A64ICODE:ADD,   \ x9 = &record[NDICT-1]
   12 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  12 9 12 A64ICODE:ADD,   \ x12 = the record through the alias
   10 12 16 A64ICODE:LDR,  10 10 DNAME-WIDE A64ICODE:ORRI,  10 12 16 A64ICODE:STR, ;

\ rep-mark ( n -- ): set DNAME-REP on dictionary record n, the CAST: record the
\ checker certified as a representation cast. It checks no index and is
\ monotonic: no clear prim exists.
: BREPMARK ( -- )
   A RT:G-POP
   C DREC A64ICODE:MOVZ,  A A C A64ICODE:MUL,  A DBASE A A64ICODE:ADD,   \ x9 = &record[n]
   12 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  12 A 12 A64ICODE:ADD,   \ x12 = the record through the alias
   C 12 16 A64ICODE:LDR,  C C DNAME-REP A64ICODE:ORRI,  C 12 16 A64ICODE:STR, ;

\ record-facts! ( n n -- ) offset, index: stores the facts offset in bytes 32-39
\ of a record. Its callers run on the main thread and pass an index from the
\ seal floor to NDICT.
: BRECFACTS ( -- )
   A RT:G-POP  B RT:G-POP                              \ x9 = record index, x10 = offset
   C DREC A64ICODE:MOVZ,  A A C A64ICODE:MUL,  A DBASE A A64ICODE:ADD,      \ x9 = &record[n]
   12 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  12 A 12 A64ICODE:ADD,   \ x12 = the record through the alias
   B 12 32 A64ICODE:STR, ;

\ SHA-256's 64 round constants (FIPS 180-4, 4.2.2).
create SHA256-K
$428a2f98 , $71374491 , $b5c0fbcf , $e9b5dba5 , $3956c25b , $59f111f1 , $923f82a4 , $ab1c5ed5 ,
$d807aa98 , $12835b01 , $243185be , $550c7dc3 , $72be5d74 , $80deb1fe , $9bdc06a7 , $c19bf174 ,
$e49b69c1 , $efbe4786 , $0fc19dc6 , $240ca1cc , $2de92c6f , $4a7484aa , $5cb0a9dc , $76f988da ,
$983e5152 , $a831c66d , $b00327c8 , $bf597fc7 , $c6e00bf3 , $d5a79147 , $06ca6351 , $14292967 ,
$27b70a85 , $2e1b2138 , $4d2c6dfc , $53380d13 , $650a7354 , $766a0abb , $81c2c92e , $92722c85 ,
$a2bfe8a1 , $a81a664b , $c24b8b70 , $c76c51a3 , $d192e819 , $d6990624 , $f40e3585 , $106aa070 ,
$19a4c116 , $1e376c08 , $2748774c , $34b0bcb5 , $391c0cb3 , $4ed8aa4a , $5b9cca4f , $682e6ff3 ,
$748f82ee , $78a5636f , $84c87814 , $8cc70208 , $90befffa , $a4506ceb , $bef9a3f7 , $c67178f2 ,

\ Lays the constants in a 256-byte frame on sp by immediates, two to a cell, not
\ in the code: the AOT linker decodes a copied body's every word (ERR-TEXT,).
: SHA256-K, ( -- )
   A64ICODE:SP A64ICODE:SP 256 A64ICODE:SUBI,
   32 0 DO
      14  SHA256-K i 2 * cells + @  SHA256-K i 2 * 1+ cells + @ 32 lshift or  A64ICODE:LIT64,
      14 A64ICODE:SP i 8 * A64ICODE:STR,
   LOOP ;

\ Linux states the SHA-256 instructions in AT_HWCAP (16) bit 6, HWCAP_SHA2: the
\ auxiliary vector's type and value pairs follow envp's NULL.
: SHA2-PROBE, ( label -- )
   {: none:label :}
   A64ICODE:LBL A64ICODE:LBL {: env:label aux:label :}
   12 XREG-RBASE ENVP-CELL A64ICODE:LDR,  12 none A64ICODE:CBZ,
   env A64ICODE:LBL,  13 12 0 A64ICODE:LDR,  12 12 8 A64ICODE:ADDI,  13 env A64ICODE:CBNZ,
   aux A64ICODE:LBL,  13 12 0 A64ICODE:LDR,  14 12 8 A64ICODE:LDR,  12 12 16 A64ICODE:ADDI,
   13 none A64ICODE:CBZ,  13 16 A64ICODE:CMPI,  A64ASM:C-NE aux A64ICODE:BCOND,
   14 14 $40 A64ICODE:ANDI,  14 none A64ICODE:CBZ, ;

\ Four rounds: v4-v7 hold the next 16 schedule words, x12 walks the constants.
: SHA256-QUAD, ( n -- )
   {: k:n :}
   k 4 mod 4 + {: w:n :}
   2 12 A64ICODE:LD1V-POST,  2 2 w A64ICODE:ADD4S,
   k 12 < IF
      w  k 1+ 4 mod 4 +  A64ICODE:SHA256SU0,
      w  k 2 + 4 mod 4 +  k 3 + 4 mod 4 +  A64ICODE:SHA256SU1,
   THEN
   3 0 A64ICODE:MOVV,  0 1 2 A64ICODE:SHA256H,  1 3 2 A64ICODE:SHA256H2, ;

\ One block from x10 into the state at x9 and x13 = x9 + 32; sp holds the frame.
: SHA256-BLOCK, ( -- )
   0 A A64ICODE:LD2V,  2 13 A64ICODE:LD2V,  1 2 A64ICODE:MOVV,                     \ v0 = a b c d, v1 = e f g h
   4 B A64ICODE:LD1V4-POST,
   4 4 A64ICODE:REV32V,  5 5 A64ICODE:REV32V,  6 6 A64ICODE:REV32V,  7 7 A64ICODE:REV32V,   \ big-endian words
   12 A64ICODE:SP 0 A64ICODE:ADDI,
   16 0 DO  i SHA256-QUAD,  LOOP
   2 A A64ICODE:LD2V,  2 2 0 A64ICODE:ADD4S,  2 A A64ICODE:ST2V,
   4 13 A64ICODE:LD2V,  4 4 1 A64ICODE:ADD4S,  4 13 A64ICODE:ST2V, ;

\ sha256-blocks ( ^n ^u8 n -- n ) state cells, bytes, block count: runs the
\ blocks through the SHA-256 instructions and answers the count, 0 where the CPU
\ lacks them (every Apple arm64 CPU has them). A state cell holds its word in the
\ low half, so LD2 splits four cells into the words and their zero halves.
: BSHA256BLOCKS ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: none:label next:label done:label :}
   C RT:G-POP  B RT:G-POP  A RT:G-POP                    \ x11 = blocks, x10 = bytes, x9 = state
   C 0 A64ICODE:CMPI,  A64ASM:C-LE none A64ICODE:BCOND,
   HB-TARGET-LINUX-ARM64? IF none SHA2-PROBE, THEN
   15 C 0 A64ICODE:ADDI,  13 A 32 A64ICODE:ADDI,
   SHA256-K,
   next A64ICODE:LBL,  SHA256-BLOCK,  C C 1 A64ICODE:SUBI,  C next A64ICODE:CBNZ,
   A64ICODE:SP A64ICODE:SP 256 A64ICODE:ADDI,  done A64ICODE:B,
   none A64ICODE:LBL,  15 0 A64ICODE:MOVZ,
   done A64ICODE:LBL,  15 RT:G-PUSH ;

;package

package ENGINE-EMIT
public

\ POLICY-BITS-AT, ( bits w a m scratch -- ): xbits points at bit 0 of the policy
\ bitmap and xw holds the WID; xa comes back holding the u64 word that holds the
\ bit and xm that bit's mask. The caller proves w < POLICY-WID-MAX first: that
\ bound keeps xa inside the bitmap. xscratch is clobbered; xw is preserved.
: POLICY-BITS-AT, ( n n n n n -- ) {: bits:n w:n a:n m:n scratch:n :}
   scratch w 6 A64ICODE:LSRI,  scratch scratch 3 A64ICODE:LSLI,
   a bits scratch A64ICODE:ADD,
   m w 63 A64ICODE:ANDI,
   scratch 1 A64ICODE:MOVZ,  m scratch m A64ICODE:LSLV, ;

private

\ The design seal's two writers. The harness admits packages and seals before
\ it loads the design; once POLICY-NDICT-CELL is set either prim writes the
\ sealed record and exits EXIT-STATUS:POLICY, so nothing the design can reach
\ widens its own vocabulary.
: POLICY-BOUND$ ( -- str ) s" E-POLICY habu: policy: package wid above the bound" ;

\ x3 = the caller's lr, read from FPRIM's frame.
: POLICY-LR, ( -- )  3 A64ICODE:SP 0 A64ICODE:LDR, ;

\ Note `in` and help, the dictionary read through x26/x27; never returns.
: POLICY-STOP, ( FAULT-RECORD:record str -- ) {: h:str :}
   4 DBASE 0 A64ICODE:ADDI,  6 NDICT 0 A64ICODE:ADDI,  CRASH:FAULT-IN,
   h EXIT-STATUS:POLICY CRASH:FAULT-STOP, ;

\ Unsealed it falls through, having written x9 only.
: POLICY-OPEN, ( -- )
   A64ICODE:LBL {: open:label :}
   9 XREG-RBASE POLICY-NDICT-CELL A64ICODE:LDR,  9 open A64ICODE:CBZ,
      POLICY-LR,
      s" E-POLICY" s" the vocabulary is sealed" CRASH:FAULT-OPEN,
      s" admit every package before `VOCABULARY:RESTRICT`" POLICY-STOP,
   open A64ICODE:LBL, ;

\ policy-admit ( ^u8 n -- ): admit the public wordlist of the package the
\ span names, found as a top-level `using` finds it.
: BPOLICYADMIT ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: found:label bounded:label walked:label :}
   POLICY-OPEN,
   1 RT:G-POP  0 RT:G-POP                                  \ x1 = name length, x0 = name
   13 0 0 A64ICODE:ADDI,                                      \ the name, for the records: the lookup leaves x13 and x1 alone
   2 XREG-RBASE BOOT:NAMESPACE-WL-CELL A64ICODE:LDR,
   WLFIND:LENTRY A64ICODE:LABEL@ A64ICODE:BL,                          \ x11 = public wid, x12 = the row or 0
   14 1 0 A64ICODE:ADDI,                                      \ the kernel's writes keep x13/x14
   12 found A64ICODE:CBNZ,
      4 13 0 A64ICODE:ADDI,  5 14 0 A64ICODE:ADDI,  POLICY-LR,
      s" E-POLICY" s" no package has the name the program admits" CRASH:FAULT-OPEN,
      s" package" 4 5 CRASH:FAULT-TEXT,
      s" admit a package the program defines" POLICY-STOP,
   found A64ICODE:LBL,
   15 POLICY-WID-MAX A64ICODE:MOVZ,  11 15 A64ICODE:CMP,  A64ASM:C-CC bounded A64ICODE:BCOND,
      0 CRASH:CALLER-LR,                                   \ WLFIND's BL replaced x30
      POLICY-BOUND$ EXIT-STATUS:POLICY CRASH:DIE-LINE,
   bounded A64ICODE:LBL,
   \ A public word spelled like a keyword would mean the keyword in one place and
   \ the word in another.
   A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,  11 A64ICODE:SP 0 A64ICODE:STR,  13 A64ICODE:SP 8 A64ICODE:STR,  14 A64ICODE:SP 16 A64ICODE:STR,
   11 RT:G-PUSH  12 XREG-RBASE POLICY-ABI:KEYWORD-CELL A64ICODE:LDR,  12 A64ICODE:BLR,
   15 RT:G-POP  10 RT:G-POP  9 RT:G-POP
   11 A64ICODE:SP 0 A64ICODE:LDR,  13 A64ICODE:SP 8 A64ICODE:LDR,  14 A64ICODE:SP 16 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI,
   15 walked A64ICODE:CBZ,
      4 13 0 A64ICODE:ADDI,  5 14 0 A64ICODE:ADDI,  6 9 0 A64ICODE:ADDI,  7 10 0 A64ICODE:ADDI,  POLICY-LR,
      s" E-POLICY" s" a package publishes a word spelled like a keyword" CRASH:FAULT-OPEN,
      s" package" 4 5 CRASH:FAULT-TEXT,
      s" keyword" 6 7 CRASH:FAULT-TEXT,
      s" rename the word in that package" POLICY-STOP,
   walked A64ICODE:LBL,
   14 POLICY-BITS-OFF A64ICODE:MOVZ,  14 XREG-RBASE 14 A64ICODE:ADD,
   14 11 15 14 16 POLICY-BITS-AT,                    \ x15 = &word, x14 = mask
   16 15 A64ICODE:LDAR,
   16 16 14 A64ICODE:ORR,
   16 15 A64ICODE:STLR, ;

\ policy-seal ( -- ): seal the dictionary as it stands. Every record below the
\ stored NDICT is admitted only through its wordlist's bit; the design's own
\ records land at or above it. NDICT is never 0 in a running engine, so the
\ stored value cannot read as unsealed.
: BPOLICYSEAL ( -- )
   POLICY-OPEN,
   NDICT XREG-RBASE POLICY-NDICT-CELL A64ICODE:STR, ;

;package

package ENGINE-EMIT
\ The hash index's key derivation is the FNV-1a hash of the folded name's
\ bytes. A two-part name hashes as one stream, so it lands in the bucket of
\ the same name spelled as one span.
: C-HIDX-STEP, ( n n n -- ) {: hr:n c3:n c4:n :}
   c3 c4 $41 A64ICODE:SUBI,  c3 $1A A64ICODE:CMPI,  c3 A64ASM:C-CC A64ICODE:CSET,  c3 c3 5 A64ICODE:LSLI,  c4 c4 c3 A64ICODE:ORR,
   hr hr c4 A64ICODE:EOR,
   c3 FNV:PRIME A64ICODE:LIT64,
   hr hr c3 A64ICODE:MUL, ;
: C-HIDX-SPAN, ( n n n n n n -- ) {: nr:n lr:n hr:n c3:n c4:n c7:n :}
   A64ICODE:LBL A64ICODE:LBL {: hl:label hd:label :}
   c7 0 A64ICODE:MOVZ,
   hl A64ICODE:LBL,  c7 lr A64ICODE:CMP,  A64ASM:C-GE hd A64ICODE:BCOND,
      c4 nr c7 A64ICODE:ADD,  c4 c4 0 A64ICODE:LDRB,  hr c3 c4 C-HIDX-STEP,
      c7 c7 1 A64ICODE:ADDI,  hl A64ICODE:B,
   hd A64ICODE:LBL, ;
public
\ Emit the hash of the name at reg nr (ptr), length lr, into hr; clobbers c3
\ c4 (byte/fold scratch) and c7 (cursor).
: C-HIDX-HASH ( n n n n n n -- ) {: nr:n lr:n hr:n c3:n c4:n c7:n :}
   hr FNV:BASIS A64ICODE:LIT64,
   nr lr hr c3 c4 c7 C-HIDX-SPAN, ;
\ C-HIDX-HASH of the name pr's first pl-1 bytes, `:`, then nr/lr; when pl is
\ 0, of nr/lr alone.
: C-HIDX-PAIR ( n n n n n n n n -- ) {: pr:n pl:n nr:n lr:n hr:n c3:n c4:n c7:n :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: hl:label hc:label hd:label :}
   hr FNV:BASIS A64ICODE:LIT64,
   pl hd A64ICODE:CBZ,
   c7 0 A64ICODE:MOVZ,
   hl A64ICODE:LBL,  c3 c7 1 A64ICODE:ADDI,  c3 pl A64ICODE:CMP,  A64ASM:C-GE hc A64ICODE:BCOND,
      c4 pr c7 A64ICODE:ADD,  c4 c4 0 A64ICODE:LDRB,  hr c3 c4 C-HIDX-STEP,
      c7 c7 1 A64ICODE:ADDI,  hl A64ICODE:B,
   hc A64ICODE:LBL,  c4 $3A A64ICODE:MOVZ,  hr c3 c4 C-HIDX-STEP,
   hd A64ICODE:LBL,  nr lr hr c3 c4 c7 C-HIDX-SPAN, ;
\ Emit dr = the first slot of hash hr's chain in wid wr; clobbers c.
: C-HIDX-SLOT, ( n n n n -- ) {: dr:n hr:n wr:n c:n :}
   dr hr wr A64ICODE:EOR,
   c HIDX-MIX A64ICODE:LIT64,
   dr dr c A64ICODE:MUL,
   dr dr 64 HIDX-BITS - A64ICODE:LSRI, ;
;package

\ WLFIND:LENTRY is the name's row in one wordlist and the whole of
\ `bare-search-wl`'s body: in x0 = name, x1 = length, x2 = wid; out x11 = the row's
\ code cell or 0, x12 = the row or 0. The AOT seed's call-site pass shares it
\ and needs the row for the sealed-WID gate. WLFIND:LPAIR looks up the
\ two-part name `first:second` without composing it: x9 = first, x10 = its
\ length (not 0), x0/x1 = second. Both keep x0, x1, x2 and x13.
\ The scan answers the last matching row; the hash probe, insert-once with one
\ slot per (name, wid), answers the first validated row. They agree only where
\ a wid holds one live row per folded name:
\   - a real wordlist (the global wid, a package's private or public wid, a
\     package-row marker) is guarded by duplicate walls at its definers;
\   - WL:RETIRED is not a wordlist: xref.f XREF:RETIRE stamps it onto rows
\     still keyed under their published wid, and retiring one name twice puts
\     two rows under the key, so that wid always takes the scan.
\ The scan also answers for no table and for a chain with no empty slot. An
\ empty slot means absent: every record below NDICT is in the table and slots
\ only go empty -> occupied.
\ A miss in the global wid asks WL:PRIMITIVES by the same entry's rule and
\ answers that, with x2 restored.
package WLFIND
\ One entry's end. x30 and x2 ride the machine stack across the second search,
\ in 16 bytes to keep sp aligned.
: RETRY, ( label -- ) {: entry:label :}
   A64ICODE:LBL {: done:label :}
   12 done A64ICODE:CBNZ,
   4 XREG-RBASE BOOT:GLOBAL-WL-CELL A64ICODE:LDR,  2 4 A64ICODE:CMP,  A64ASM:C-NE done A64ICODE:BCOND,
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,  2 A64ICODE:SP 8 A64ICODE:STR,
   2 WL:PRIMITIVES A64ICODE:MOVZ,  entry A64ICODE:BL,
   30 A64ICODE:SP 0 A64ICODE:LDR,  2 A64ICODE:SP 8 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI,
   done A64ICODE:LBL,  A64ICODE:RET, ;

: FOLD, ( n -- ) {: r:n :}
   3 r $41 A64ICODE:SUBI,  3 $1A A64ICODE:CMPI,  3 A64ASM:C-CC A64ICODE:CSET,  3 3 5 A64ICODE:LSLI,  r r 3 A64ICODE:ORR, ;

\ The record name at x16, of the name's whole length, against the name: x0/x1
\ when x10 is 0, else x9's first x10-1 bytes, `:`, then x0/x1 (the entries
\ set x10). Clobbers x3, x4, x7, x15 and x16.
: SAME, ( label label -- ) {: miss:label hit:label :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: first:label colon:label second:label :}
   7 0 A64ICODE:MOVZ,
   10 second A64ICODE:CBZ,
   first A64ICODE:LBL,
      15 7 1 A64ICODE:ADDI,  15 10 A64ICODE:CMP,  A64ASM:C-GE colon A64ICODE:BCOND,
      15 16 7 A64ICODE:ADD,  15 15 0 A64ICODE:LDRB,  15 FOLD,
      4 9 7 A64ICODE:ADD,  4 4 0 A64ICODE:LDRB,  4 FOLD,
      15 4 A64ICODE:CMP,  A64ASM:C-NE miss A64ICODE:BCOND,
      7 7 1 A64ICODE:ADDI,  first A64ICODE:B,
   colon A64ICODE:LBL,
      15 16 7 A64ICODE:ADD,  15 15 0 A64ICODE:LDRB,  15 $3A A64ICODE:CMPI,  A64ASM:C-NE miss A64ICODE:BCOND,
      16 16 10 A64ICODE:ADD,  7 0 A64ICODE:MOVZ,
   second A64ICODE:LBL,
      7 1 A64ICODE:CMP,  A64ASM:C-GE hit A64ICODE:BCOND,
      15 16 7 A64ICODE:ADD,  15 15 0 A64ICODE:LDRB,  15 FOLD,
      4 0 7 A64ICODE:ADD,  4 4 0 A64ICODE:LDRB,  4 FOLD,
      15 4 A64ICODE:CMP,  A64ASM:C-NE miss A64ICODE:BCOND,
      7 7 1 A64ICODE:ADDI,  second A64ICODE:B, ;

public
: EMIT ( -- )
   A64ICODE:LBL ENGINE-EMIT:SWL-LOOP !
   A64ICODE:LBL ENGINE-EMIT:SWL-END !
   A64ICODE:LBL ENGINE-EMIT:SWL-NEXT !
   A64ICODE:LBL ENGINE-EMIT:SWL-MATCH !
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
   {: body:label plin:label ploop:label pnext:label pinl:label pmatch:label pend:label sinl:label :}
   LPAIR A64ICODE:LABEL@ A64ICODE:LBL,
   10 10 1 A64ICODE:ADDI,  body A64ICODE:B,
   LENTRY A64ICODE:LABEL@ A64ICODE:LBL,
   10 0 A64ICODE:MOVZ,
   body A64ICODE:LBL,
   11 0 A64ICODE:MOVZ,
   12 0 A64ICODE:MOVZ,
   \ Hash probe. x0 (name), x1 (len), x2 (wid), x9/x10 (first part) and x11
   \ (result) are preserved across it for the scan the two fallbacks reach.
   4 WL:RETIRED A64ICODE:LIT64,  2 4 A64ICODE:CMP,  A64ASM:C-EQ plin A64ICODE:BCOND,
   14 XREG-RBASE HIDXP-CELL A64ICODE:LDR,  14 plin A64ICODE:CBZ,
   9 10 0 1 15 4 16 7 ENGINE-EMIT:C-HIDX-PAIR
   6 15 2 5 ENGINE-EMIT:C-HIDX-SLOT,
   8 HIDX-SLOTS A64ICODE:LIT64,
   ploop A64ICODE:LBL,
      17 6 2 A64ICODE:LSLI,  17 14 17 A64ICODE:ADD,  3 17 0 A64ICODE:LDRW,            \ x3 = slot value (index+1)
      3 pend A64ICODE:CBZ,
      4 3 1 A64ICODE:SUBI,  4 NDICT A64ICODE:CMP,  A64ASM:C-GE pnext A64ICODE:BCOND,         \ stale (truncated) index
      5 DREC A64ICODE:MOVZ,  5 4 5 A64ICODE:MUL,  5 DBASE 5 A64ICODE:ADD,             \ x5 = record ptr
      16 5 40 A64ICODE:LDR,  16 2 A64ICODE:CMP,  A64ASM:C-NE pnext A64ICODE:BCOND,           \ wid mismatch (retired / other wordlist)
      4 1 10 A64ICODE:ADD,                                          \ x4 = the name's whole length
      16 5 16 A64ICODE:LDR,  16 16 DNAME-FLAG-BITS A64ICODE:LSLI,  16 16 DNAME-FLAG-BITS A64ICODE:LSRI,  16 4 A64ICODE:CMP,  A64ASM:C-NE pnext A64ICODE:BCOND,  \ name-len mismatch
      16 5 24 A64ICODE:ADDI,
      3 5 16 A64ICODE:LDR,  3 3 DNAME-EXT A64ICODE:ANDI,  3 pinl A64ICODE:CBZ,
         16 5 24 A64ICODE:LDR,
      pinl A64ICODE:LBL,
      pnext pmatch SAME,
      pmatch A64ICODE:LBL,
         11 5 0 A64ICODE:LDR,  12 5 0 A64ICODE:ADDI,  A64ICODE:RET,
      pnext A64ICODE:LBL,
         8 8 1 A64ICODE:SUBI,  8 plin A64ICODE:CBZ,
         6 6 1 A64ICODE:ADDI,  5 HIDX-SLOTS 1 - A64ICODE:LIT64,  6 6 5 A64ICODE:AND,  ploop A64ICODE:B,
   plin A64ICODE:LBL,
   5 DBASE 0 A64ICODE:ADDI,  6 NDICT 0 A64ICODE:ADDI,
   ENGINE-EMIT:SWL-LOOP A64ICODE:LABEL@ A64ICODE:LBL,  6 ENGINE-EMIT:SWL-END A64ICODE:LABEL@ A64ICODE:CBZ,
      15 5 40 A64ICODE:LDR,  15 2 A64ICODE:CMP,  A64ASM:C-NE ENGINE-EMIT:SWL-NEXT A64ICODE:LABEL@ A64ICODE:BCOND,
      4 1 10 A64ICODE:ADD,
      15 5 16 A64ICODE:LDR,  15 15 DNAME-FLAG-BITS A64ICODE:LSLI,  15 15 DNAME-FLAG-BITS A64ICODE:LSRI,  15 4 A64ICODE:CMP,  A64ASM:C-NE ENGINE-EMIT:SWL-NEXT A64ICODE:LABEL@ A64ICODE:BCOND,
      16 5 24 A64ICODE:ADDI,
      15 5 16 A64ICODE:LDR,  15 15 DNAME-EXT A64ICODE:ANDI,  15 sinl A64ICODE:CBZ,
         16 5 24 A64ICODE:LDR,
      sinl A64ICODE:LBL,
      ENGINE-EMIT:SWL-NEXT A64ICODE:LABEL@ ENGINE-EMIT:SWL-MATCH A64ICODE:LABEL@ SAME,
      ENGINE-EMIT:SWL-MATCH A64ICODE:LABEL@ A64ICODE:LBL,  11 5 0 A64ICODE:LDR,  12 5 0 A64ICODE:ADDI,  ENGINE-EMIT:SWL-NEXT A64ICODE:LABEL@ A64ICODE:B,
      ENGINE-EMIT:SWL-NEXT A64ICODE:LABEL@ A64ICODE:LBL,  5 5 DREC A64ICODE:ADDI,  6 6 1 A64ICODE:SUBI,  ENGINE-EMIT:SWL-LOOP A64ICODE:LABEL@ A64ICODE:B,
   ENGINE-EMIT:SWL-END A64ICODE:LABEL@ A64ICODE:LBL,
   pend A64ICODE:LBL,  body RETRY, ;
;package

package ENGINE-EMIT
\ `bare-search-wl ( ^u8 n n -- n )`: the routine above, minus one wordlist.
\ Engine-helper WID records and DNAME-INT records are never raw-searchable: the
\ first are private implementation entries, and the second would otherwise let
\ `search-wl execute` bypass the interpreter/tick internal-word gate. The rejection
\ belongs to the primitive, not WLFIND; the AOT relocator uses WLFIND directly.
: BSWL ( -- )
   A64ICODE:LBL {: done:label :}
   2 RT:G-POP  1 RT:G-POP  0 RT:G-POP
   11 0 A64ICODE:MOVZ,
   2 OWNER-API-PRI-WID A64ICODE:CMPI,  A64ASM:C-EQ done A64ICODE:BCOND,
   WLFIND:LENTRY A64ICODE:LABEL@ A64ICODE:BL,
   12 done A64ICODE:CBZ,
   9 12 16 A64ICODE:LDR,  9 9 DNAME-INT A64ICODE:ANDI,  9 done A64ICODE:CBZ,
   11 0 A64ICODE:MOVZ,
   done A64ICODE:LBL,  11 RT:G-PUSH ;

\ xref-search-wl ( ^u8 n n -- ^n ): the same lookup's record result.
\ DNAME-INT and its checker rows restrict its callers;
\ NDICT checks live callee visibility after resolving namespace records.
: BCOMPILERSWL ( -- )
   2 RT:G-POP  1 RT:G-POP  0 RT:G-POP
   WLFIND:LENTRY A64ICODE:LABEL@ A64ICODE:BL,
   12 RT:G-PUSH ;

: BPARSE-NAME ( -- )
   A64ICODE:LBL PARSE-NONE !
   A64ICODE:LBL PARSE-DONE !
   LTOK A64ICODE:LABEL@ A64ICODE:BL,
   0 PARSE-NONE A64ICODE:LABEL@ A64ICODE:CBZ,
      9 XREG-RBASE TKA-CELL A64ICODE:LDR,  9 RT:G-PUSH
      9 XREG-RBASE TKL-CELL A64ICODE:LDR,  9 RT:G-PUSH
      PARSE-DONE A64ICODE:LABEL@ A64ICODE:B,
   PARSE-NONE A64ICODE:LABEL@ A64ICODE:LBL,
      9 XREG-RBASE INP-CELL A64ICODE:LDR,  9 RT:G-PUSH
      9 0 A64ICODE:MOVZ,  9 RT:G-PUSH
   PARSE-DONE A64ICODE:LABEL@ A64ICODE:LBL, ;
;package

\ num-parse ( ^u8 n -- n bool bool ): the engine's own number reader, the
\ LNUM routine. x11 is the value (a double's bits for a float), x2 whether it
\ read a float, x12 whether it read a number. A rejected spelling answers zero
\ (the two ANDs): a syntax miss stops at the first unreadable byte (`12a`
\ leaves 12) and an over-bound literal ends with x12 false.
package ENGINE-EMIT
public

: BNUMPARSE ( -- )
   B RT:G-POP  A RT:G-POP
   2 0 A64ICODE:MOVZ,
   LNUM A64ICODE:LABEL@ A64ICODE:BL,
   12 0 A64ICODE:CMPI,  12 A64ASM:C-NE A64ICODE:CSET,  12 A64ICODE:SP 12 A64ICODE:SUB,
   2 0 A64ICODE:CMPI,   2 A64ASM:C-NE A64ICODE:CSET,   2 A64ICODE:SP 2 A64ICODE:SUB,
   2 2 12 A64ICODE:AND,
   C C 12 A64ICODE:AND,
   C RT:G-PUSH  2 RT:G-PUSH  12 RT:G-PUSH ;

;package

package ENGINE-EMIT
: EMIT-ARITH-PRIMS ( -- )
   s" +"    ['] B+    FPRIM-L   s" -"    ['] B-    FPRIM-L   s" *"    ['] B*    FPRIM-L
   s" /"    ['] BDIV  FPRIM-L   s" mod"  ['] BMOD  FPRIM-L   s" /mod" ['] BDIVMOD FPRIM-L
   s" abs"  ['] BABS  FPRIM-L   s" min"  ['] BMIN  FPRIM-L   s" max"  ['] BMAX FPRIM-L ;

: EMIT-COMPARE-PRIMS ( -- )
   s" ="    ['] B=    FPRIM-L   s" <>"   ['] B<>   FPRIM-L   s" <"    ['] B<    FPRIM-L
   s" >"    ['] B>    FPRIM-L   s" <="   ['] B<=   FPRIM-L   s" >="   ['] B>=   FPRIM-L
   s" 0="   ['] B0=   FPRIM-L   s" 0<"   ['] B0<   FPRIM-L
   s" 1+"   ['] B1+   FPRIM-L   s" 1-"   ['] B1-   FPRIM-L
   s" and"  ['] BAND  FPRIM-L   s" or"   ['] BOR   FPRIM-L   s" xor"  ['] BXOR  FPRIM-L
   s" invert" ['] BINV FPRIM-L  s" negate" ['] BNEG FPRIM-L
   s" lshift" ['] BLSH FPRIM-L  s" rshift" ['] BRSH FPRIM-L ;

: EMIT-STACK-PRIMS ( -- )
   s" dup"  ['] BDUP  FPRIM-L   s" drop" ['] BDROP FPRIM-L   s" swap" ['] BSWAP FPRIM-L
   s" nip"  ['] BNIP  FPRIM-L   s" over" ['] BOVER FPRIM-L   s" tuck" ['] BTUCK FPRIM-L
   s" rot"  ['] BROT  FPRIM-L   s" -rot" ['] BMROT FPRIM-L
   s" 2dup" ['] B2DUP FPRIM-L   s" 2drop" ['] B2DROP FPRIM-L
   s" 2swap" ['] B2SWAP FPRIM-L  s" 2over" ['] B2OVER FPRIM-L
   s" 2>r" ['] B2TOR FPRIM-L  s" 2r>" ['] B2RFROM FPRIM-L  s" 2r@" ['] B2RFETCH FPRIM-L ;

: EMIT-MEMORY-PRIMS ( -- )
   s" @"    ['] BFETCH FPRIM-L   s" !"    ['] BSTORE FPRIM   s" ptr-field" ['] BPTRFIELD FPRIM-L
   s" byte-view" ['] BADDRESSVIEW FPRIM-L
   s" cell-view" ['] BADDRESSVIEW FPRIM-L
   s" PTR>N" ['] BADDRESSVIEW FPRIM-L
   s" >str" ['] BADDRESSVIEW FPRIM-L
   s" str>" ['] BADDRESSVIEW FPRIM-L
   s" >span" ['] BADDRESSVIEW FPRIM-L
   s" span>" ['] BADDRESSVIEW FPRIM-L
   s" data" ['] BDROP FPRIM-L  s" len" ['] BNIP FPRIM-L
   s" +!" ['] BPLUSSTORE FPRIM
   s" c@"   ['] BCFETCH FPRIM-L  s" c!"   ['] BCSTORE FPRIM
   s" u16@" ['] BHFETCH FPRIM-L  s" u16!" ['] BHSTORE FPRIM
   s" u32@" ['] BWFETCH FPRIM-L  s" u32!" ['] BWSTORE FPRIM
   s" atomic@" ['] BATFETCH FPRIM-L  s" atomic!" ['] BATSTORE FPRIM
   s" atomic-u32@" ['] BATFETCH32 FPRIM-L  s" atomic-u32!" ['] BATSTORE32 FPRIM
   s" atomic-add" ['] BATADD FPRIM  s" atomic-cas" ['] BATCAS FPRIM  s" fence" ['] BFENCE FPRIM-L
   s" cells" ['] BCELLS FPRIM-L  s" cell+" ['] BCELLPLUS FPRIM-L
   s" chars" ['] BCHARS FPRIM-L  s" char+" ['] BCHARPLUS FPRIM-L  s" count" ['] BCOUNT FPRIM-L ;

: EMIT-OUTPUT-PRIMS ( -- )
   s" ."    ['] BDOT  FPRIM-L   s" .s"   ['] B.S   FPRIM-L   s" depth" ['] BDEPTH FPRIM-L
   s" u."   ['] BU.   FPRIM-L   s" emit" ['] BEMIT FPRIM-L
   s" cr"   ['] BCR   FPRIM-L   s" space" ['] BSPACE FPRIM-L
   s" type" ['] BTYPE  FPRIM-L ;

: EMIT-DICT-PRIMS ( -- )
   s" here" ['] BHERE  FPRIM-L   s" allot" ['] BALLOT FPRIM-L
   s" align" ['] BALIGN FPRIM-L
   s" ,"    ['] BCOMMA FPRIM-L   s" c,"   ['] BCCOMMA FPRIM-L
   s" execute" ['] BEXEC FPRIM
   s" execute-floor" ['] BEXECFLOOR FPRIM
   s" create" ['] BCREATE FPRIM
   s" parse-name" ['] BPARSE-NAME FPRIM
   s" num-parse" ['] ENGINE-EMIT:BNUMPARSE FPRIM
   s" evaluate" ['] B-EVAL FPRIM-L
   s" evaluate-range" ['] B-EVAL-RANGE FPRIM-L
   s" literal" ['] B-LITERAL FPRIM-L
   s" compile," ['] B-COMPILE FPRIM-L
   s" ;" ['] B-END FPRIM-L
   s" evaluate-closed" ['] B-EVAL-CLOSED FPRIM
   s" source-file-run" ['] B-SOURCE-FILE-RUN FPRIM ;

: EMIT-PROCESS-PRIMS ( -- )
   s" pipe" ['] BPIPE FPRIM-L   s" dup2" ['] BDUP2 FPRIM-L
   s" fcntl" ['] BFCNTL FPRIM-L   s" poll" ['] BPOLL FPRIM
   s" kill" ['] BKILL FPRIM-L
   s" setpgid" ['] BSETPGID FPRIM-L
   s" getpid" ['] BGETPID FPRIM-L
   s" proc-watch-open" ['] BPROCWATCHOPEN FPRIM-L
   s" kill-errno" ['] BKILLERRNO FPRIM-L
   s" execve" ['] BEXECVE FPRIM-L
   s" spawn-io" ['] BSPAWNIO FPRIM-L
   s" spawn-argv-io" ['] BSPAWNARGVIO FPRIM-L
   s" spawn-argv-env-io" ['] BSPAWNARGVENVIO FPRIM-L
   s" spawn-argv-env-cwd-io" ['] BSPAWNARGVENVCWDIO FPRIM-L
   s" spawn-fds" ['] BSPAWNFDS FPRIM-L
   s" fork" ['] LIBC-OS:FORK FPRIM
   s" wait-status" ['] BWAITSTATUS FPRIM-L ;
;package

package ENGINE-EMIT

: EMIT-ENGINE-PRIMS ( -- )
   s" cp@" ['] BCPFETCH FPRIM-L   s" dbase@" ['] BDBASEFETCH FPRIM-L
   s" data-base" ['] BDATAFETCH FPRIM-L
   s" NULL-PTR" ['] BNULLPTR FPRIM-L
   s" ndict@" ['] BNDICTFETCH FPRIM-L
   s" cp!" ['] BCPSET FPRIM   s" ndict!" ['] BNDSET FPRIM
   s" ndict-append" ['] BNDAPPEND FPRIM
   s" def-occ-select" ['] BDEFSELECT FPRIM
   s" def-occ-resolve" ['] BDEFRESOLVE FPRIM
   s" wide-mark" ['] BWIDEMARK FPRIM
   s" record-facts!" ['] BRECFACTS FPRIM
   s" sha256-blocks" ['] BSHA256BLOCKS FPRIM
   s" rep-mark" ['] BREPMARK FPRIM
   s" policy-admit" ['] BPOLICYADMIT FPRIM
   s" policy-seal" ['] BPOLICYSEAL FPRIM
   s" epoch-seconds" ['] BEPOCHSECONDS FPRIM-L
   s" mono-ns" ['] BMONONS FPRIM-L
   s" die"  ['] BDIE   FPRIM-L
   s" die-closed" ['] BDIECLOSED FPRIM ;

;package

package ENGINE-EMIT
: EMIT-FS-PRIMS ( -- )
   s" open" ['] BOPEN FPRIM-L   s" write" ['] BWRITE FPRIM-L   s" read" ['] BREAD FPRIM   s" ioctl" ['] BIOCTL FPRIM
   s" map-anon" ['] BMAPANON FPRIM
   s" mmap" ['] BMMAP FPRIM
   s" munmap" ['] BMUNMAP FPRIM
   s" ffi-call" ['] BFFI-CALL FPRIM
   s" ffi-call-n" ['] BFFI-CALL FPRIM
   s" ffi-call-0" [: 0 BFFI-CALL-K ;] FPRIM
   s" ffi-call-1" [: 1 BFFI-CALL-K ;] FPRIM
   s" ffi-call-2" [: 2 BFFI-CALL-K ;] FPRIM
   s" ffi-call-3" [: 3 BFFI-CALL-K ;] FPRIM
   s" ffi-call-4" [: 4 BFFI-CALL-K ;] FPRIM
   s" ffi-call-5" [: 5 BFFI-CALL-K ;] FPRIM
   s" ffi-call-6" [: 6 BFFI-CALL-K ;] FPRIM
   s" ffi-call-7" [: 7 BFFI-CALL-K ;] FPRIM
   s" ffi-call-8" [: 8 BFFI-CALL-K ;] FPRIM
   s" task-entry" ['] BTASK-ENTRY FPRIM
   s" dlopen-entry" ['] LIBC-OS:DLOPEN-ENTRY FPRIM
   s" dlsym-entry" ['] LIBC-OS:DLSYM-ENTRY FPRIM
   A64ICODE:LBL LCALLBACKTHUNK !
   A64ICODE:LBL LCALLBACKADDR !
   s" callback-entry" ['] BCALLBACK-ENTRY FPRIM
   EMIT-CALLBACK-THUNK
   s" ffi-call-struct" ['] BFFI-CALL-STRUCT FPRIM
   s" ffi-call-abi" ['] BFFI-CALL-ABI FPRIM
   s" ffi-call-abi-r" ['] BFFI-CALL-ABI-R FPRIM
   s" open-rd" ['] BOPENRD FPRIM-L
   s" access" ['] BACCESS FPRIM-L
   s" unlink" ['] BUNLINK FPRIM-L   s" rename" ['] BRENAME FPRIM-L   s" chmod" ['] BCHMOD FPRIM-L
   s" symlink" ['] BSYMLINK FPRIM-L   s" readlink" ['] BREADLINK FPRIM
   s" realpath" ['] LIBC-OS:REALPATH FPRIM
   s" mkdir" ['] BMKDIR FPRIM-L     s" rmdir" ['] BRMDIR FPRIM-L
   s" stat64" ['] BSTAT64 FPRIM   s" lstat64" ['] BLSTAT64 FPRIM
   s" getdirentries64" ['] BGETDIRENTRIES64 FPRIM
   s" patch32" ['] BPATCH32 FPRIM
   s" code-publish" ['] NPUBWIN:BCODEPUBLISH FPRIM
   s" xref-retarget" ['] NPUBWIN:BXREFRETARGET FPRIM
   s" close" ['] BCLOSE FPRIM-L
   s" close-rc" ['] BCLOSE-RC FPRIM-L
   s" rbase" ['] BRBASE FPRIM-L ;

: EMIT-CHECKER-PRIMS ( -- )
   s" catch" ['] BCATCH FPRIM   s" throw" ['] BTHROW-NONZERO FPRIM-L
   s" halt-throw" ['] BTHROW FPRIM
   s" finally" ['] BFINALLY FPRIM
   s" c2-invoke" ['] BC2-INVOKE FPRIM
   s" c2-init-stow" ['] BC2-INIT-STOW FPRIM
   s" c2-records-stow" ['] BC2-RECORDS-STOW FPRIM
   s" wordlist" ['] BWORDLIST FPRIM-L   s" get-current" ['] BGETCUR FPRIM-L
   s" bare-search-wl" ['] BSWL FPRIM
   s" xref-search-wl" ['] BCOMPILERSWL FPRIM
   s" set-check" ['] BSETCHECK FPRIM
   s" check@" ['] BCHECKFETCH FPRIM-L
   s" tick-order@" ['] BTICKORDER FPRIM-L
   s" set-preflight" ['] BSETPREFLIGHT FPRIM ;
;package

package ENGINE-EMIT

: EMIT-PRIMS ( -- )
   EMIT-ARITH-PRIMS  EMIT-COMPARE-PRIMS  EMIT-STACK-PRIMS
   EMIT-MEMORY-PRIMS  EMIT-OUTPUT-PRIMS  EMIT-DICT-PRIMS
   EMIT-PROCESS-PRIMS  EMIT-ENGINE-PRIMS  EMIT-FS-PRIMS
   EMIT-CHECKER-PRIMS ;

;package

package ENGINE-EMIT
\ FP: doubles as raw IEEE754 bit-cells on the data stack. Each compare's
\ condition is false on fcmp's unordered flags, NZCV 0011.
: BF+ ( -- )
   B RT:G-POP  A RT:G-POP  0 A A64ICODE:FMOVXD,  1 B A64ICODE:FMOVXD,  0 0 1 A64ICODE:FADD,  A 0 A64ICODE:FMOVDX,  A RT:G-PUSH ;

: BF- ( -- )
   B RT:G-POP  A RT:G-POP  0 A A64ICODE:FMOVXD,  1 B A64ICODE:FMOVXD,  0 0 1 A64ICODE:FSUB,  A 0 A64ICODE:FMOVDX,  A RT:G-PUSH ;

: BF* ( -- )
   B RT:G-POP  A RT:G-POP  0 A A64ICODE:FMOVXD,  1 B A64ICODE:FMOVXD,  0 0 1 A64ICODE:FMUL,  A 0 A64ICODE:FMOVDX,  A RT:G-PUSH ;

: BF/ ( -- )
   B RT:G-POP  A RT:G-POP  0 A A64ICODE:FMOVXD,  1 B A64ICODE:FMOVXD,  0 0 1 A64ICODE:FDIV,  A 0 A64ICODE:FMOVDX,  A RT:G-PUSH ;

: BFNEG ( -- )
   A RT:G-POP  0 A A64ICODE:FMOVXD,  0 0 A64ICODE:FNEG,   A 0 A64ICODE:FMOVDX,  A RT:G-PUSH ;

: BFABS ( -- )
   A RT:G-POP  0 A A64ICODE:FMOVXD,  0 0 A64ICODE:FABS,   A 0 A64ICODE:FMOVDX,  A RT:G-PUSH ;

: BFSQRT ( -- )
   A RT:G-POP  0 A A64ICODE:FMOVXD,  0 0 A64ICODE:FSQRT,  A 0 A64ICODE:FMOVDX,  A RT:G-PUSH ;

: (FCMP) ( n -- )
   FP-COND !
   B RT:G-POP  A RT:G-POP  0 A A64ICODE:FMOVXD,  1 B A64ICODE:FMOVXD,  0 1 A64ICODE:FCMP,
   A FP-COND @ A64ICODE:CSET,  A A64ICODE:SP A A64ICODE:SUB,  A RT:G-PUSH ;

: BF< ( -- )
   A64ASM:C-MI (FCMP) ;

: BF> ( -- )
   A64ASM:C-GT (FCMP) ;

: BF= ( -- )
   A64ASM:C-EQ (FCMP) ;

: BF<= ( -- )
   A64ASM:C-LS (FCMP) ;

: BF>= ( -- )
   A64ASM:C-GE (FCMP) ;

: (FCMP0) ( n -- )
   FP-COND !  A RT:G-POP  0 A A64ICODE:FMOVXD,  0 A64ICODE:FCMP0,
   A FP-COND @ A64ICODE:CSET,  A A64ICODE:SP A A64ICODE:SUB,  A RT:G-PUSH ;

: BF0< ( -- )
   A64ASM:C-MI (FCMP0) ;

: BF0= ( -- )
   A64ASM:C-EQ (FCMP0) ;

: BS>F ( -- )
   A RT:G-POP  0 A A64ICODE:SCVTF,   A 0 A64ICODE:FMOVDX,  A RT:G-PUSH ;

: BF>S ( -- )
   A RT:G-POP  0 A A64ICODE:FMOVXD,  A 0 A64ICODE:FCVTZS,  A RT:G-PUSH ;

: BFDOT ( -- )
   A64ICODE:LBL FD-FRAC !
   A64ICODE:LBL FD-INT !
   A64ICODE:LBL FD-SIGN !
   A RT:G-POP  15 A 0 A64ICODE:ADDI,
   A64ICODE:SP A64ICODE:SP 48 A64ICODE:SUBI,
   12 A64ICODE:SP 48 A64ICODE:ADDI,
   13 10 A64ICODE:MOVZ,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,           \ newline
   0 15 A64ICODE:FMOVXD,  1 0 A64ICODE:FABS,
   9 1 A64ICODE:FCVTZS,
   2 9 A64ICODE:SCVTF,  3 1 2 A64ICODE:FSUB,
   14 $F4240 A64ICODE:LIT64,  2 14 A64ICODE:SCVTF,  3 3 2 A64ICODE:FMUL,
   14 3 A64ICODE:FCVTZS,                                        \ x14 = frac * 1e6
   10 10 A64ICODE:MOVZ,  5 6 A64ICODE:MOVZ,
   FD-FRAC A64ICODE:LABEL@ A64ICODE:LBL,
     11 14 10 A64ICODE:SDIV,  13 11 10 A64ICODE:MUL,  13 14 13 A64ICODE:SUB,
     13 13 48 A64ICODE:ADDI,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,
     14 11 0 A64ICODE:ADDI,  5 5 1 A64ICODE:SUBI,  5 FD-FRAC A64ICODE:LABEL@ A64ICODE:CBNZ,
   13 46 A64ICODE:MOVZ,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,           \ '.'
   FD-INT A64ICODE:LABEL@ A64ICODE:LBL,
     11 9 10 A64ICODE:SDIV,  13 11 10 A64ICODE:MUL,  13 9 13 A64ICODE:SUB,
     13 13 48 A64ICODE:ADDI,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,
     9 11 0 A64ICODE:ADDI,  9 FD-INT A64ICODE:LABEL@ A64ICODE:CBNZ,
   15 15 63 A64ICODE:LSRI,  15 FD-SIGN A64ICODE:LABEL@ A64ICODE:CBZ,
     13 45 A64ICODE:MOVZ,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,         \ '-'
   FD-SIGN A64ICODE:LABEL@ A64ICODE:LBL,
   1 12 0 A64ICODE:ADDI,  2 A64ICODE:SP 48 A64ICODE:ADDI,  2 2 12 A64ICODE:SUB,
   RT:G-OUT
   A64ICODE:SP A64ICODE:SP 48 A64ICODE:ADDI, ;

: EMIT-FP-PRIMS ( -- )
   s" f+" ['] BF+ FPRIM-L   s" f-" ['] BF- FPRIM-L   s" f*" ['] BF* FPRIM-L
   s" f/" ['] BF/ FPRIM-L   s" fnegate" ['] BFNEG FPRIM-L
   s" fabs" ['] BFABS FPRIM-L  s" fsqrt" ['] BFSQRT FPRIM-L
   s" f<" ['] BF< FPRIM-L   s" f>" ['] BF> FPRIM-L   s" f=" ['] BF= FPRIM-L
   s" f<=" ['] BF<= FPRIM-L  s" f>=" ['] BF>= FPRIM-L
   s" f0<" ['] BF0< FPRIM-L  s" f0=" ['] BF0= FPRIM-L
   s" s>f" ['] BS>F FPRIM-L  s" f>s" ['] BF>S FPRIM-L
   s" f." ['] BFDOT FPRIM-L ;

\ EMIT-BCAP's grow routine at gsub ( x16 = the text length needed -- x16 = 0, or
\ the capacity a failed map tried ). A capacity at 2^59, whose block's size
\ would pass 2^63, is rejected as a map is. It keeps every register but x16, the
\ syscall's included: LBCS's callers hold x6-x10 live, and the capture then
\ reads x11, x12, x14 and x17. A failed map leaves the handle as it was.
: BCAP-GROW ( label -- ) {: gsub:label :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: dbl:label fits:label fail:label done:label fresh:label :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: tloop:label tdone:label rloop:label rdone:label kept:label :}
   A64ICODE:LBL {: back:label :}
   gsub A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP 144 A64ICODE:SUBI,
   0 A64ICODE:SP 0 A64ICODE:STR,    1 A64ICODE:SP 8 A64ICODE:STR,    2 A64ICODE:SP 16 A64ICODE:STR,   3 A64ICODE:SP 24 A64ICODE:STR,   4 A64ICODE:SP 32 A64ICODE:STR,
   5 A64ICODE:SP 40 A64ICODE:STR,   6 A64ICODE:SP 48 A64ICODE:STR,   7 A64ICODE:SP 56 A64ICODE:STR,   8 A64ICODE:SP 64 A64ICODE:STR,   9 A64ICODE:SP 72 A64ICODE:STR,
   10 A64ICODE:SP 80 A64ICODE:STR,  11 A64ICODE:SP 88 A64ICODE:STR,  12 A64ICODE:SP 96 A64ICODE:STR,  13 A64ICODE:SP 104 A64ICODE:STR, 14 A64ICODE:SP 112 A64ICODE:STR,
   15 A64ICODE:SP 120 A64ICODE:STR, 17 A64ICODE:SP 128 A64ICODE:STR,
   9 16 0 A64ICODE:ADDI,
   10 BODY-TEXT:FIRST-CAP A64ICODE:LIT64,
   11 1 A64ICODE:MOVZ,  11 11 59 A64ICODE:LSLI,
   dbl A64ICODE:LBL,
      10 9 A64ICODE:CMP,  A64ASM:C-CS fits A64ICODE:BCOND,
      10 10 1 A64ICODE:LSLI,
      10 11 A64ICODE:CMP,  A64ASM:C-CC dbl A64ICODE:BCOND,
   fail A64ICODE:B,
   fits A64ICODE:LBL,
   0 0 A64ICODE:MOVZ,  1 10 4 A64ICODE:LSLI,  1 1 10 A64ICODE:ADD,  2 3 A64ICODE:MOVZ,
   3 SYSCALL:MAP-ANON-PRIVATE A64ICODE:LIT64,  4 0 A64ICODE:MOVN,  5 0 A64ICODE:MOVZ,  SYSCALL:NR-MMAP SYSCALL:SYS,
   A64ASM:C-CS fail A64ICODE:BCOND,
   11 0 0 A64ICODE:ADDI,
   12 XREG-RBASE BODY-TEXT:BASE-CELL A64ICODE:LDR,
   13 XREG-RBASE BODY-TEXT:CAP-CELL A64ICODE:LDR,
   12 fresh A64ICODE:CBZ,
   14 XREG-RBASE BODYLEN-CELL A64ICODE:LDR,  14 13 A64ICODE:CMP,  A64ASM:C-LS kept A64ICODE:BCOND,  14 13 0 A64ICODE:ADDI,
   kept A64ICODE:LBL,
   15 0 A64ICODE:MOVZ,
   tloop A64ICODE:LBL,
      15 14 A64ICODE:CMP,  A64ASM:C-CS tdone A64ICODE:BCOND,
      5 12 15 A64ICODE:ADD,  5 5 0 A64ICODE:LDRB,  6 11 15 A64ICODE:ADD,  5 6 0 A64ICODE:STRB,
      15 15 1 A64ICODE:ADDI,  tloop A64ICODE:B,
   tdone A64ICODE:LBL,
   7 XREG-RBASE BODY-TEXT:ROWS-CELL A64ICODE:LDR,  7 7 4 A64ICODE:LSLI,
   15 0 A64ICODE:MOVZ,
   rloop A64ICODE:LBL,
      15 7 A64ICODE:CMP,  A64ASM:C-CS rdone A64ICODE:BCOND,
      5 12 13 A64ICODE:ADD,  5 5 15 A64ICODE:ADD,  5 5 0 A64ICODE:LDR,
      6 11 10 A64ICODE:ADD,  6 6 15 A64ICODE:ADD,  5 6 0 A64ICODE:STR,
      15 15 8 A64ICODE:ADDI,  rloop A64ICODE:B,
   rdone A64ICODE:LBL,
   \ TCSIG-A into the old text moves with it: unsigned, x6 < old cap covers both ends
   5 XREG-RBASE TCSIG-A-CELL A64ICODE:LDR,  6 5 12 A64ICODE:SUB,  6 13 A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      5 5 11 A64ICODE:ADD,  5 5 12 A64ICODE:SUB,  5 XREG-RBASE TCSIG-A-CELL A64ICODE:STR,
   done A64ICODE:LBL,
   0 12 0 A64ICODE:ADDI,  1 XREG-RBASE BODY-TEXT:SIZE-CELL A64ICODE:LDR,  SYSCALL:NR-MUNMAP SYSCALL:SYS,
   fresh A64ICODE:LBL,
   11 XREG-RBASE BODY-TEXT:BASE-CELL A64ICODE:STR,  10 XREG-RBASE BODY-TEXT:CAP-CELL A64ICODE:STR,
   1 10 4 A64ICODE:LSLI,  1 1 10 A64ICODE:ADD,  1 XREG-RBASE BODY-TEXT:SIZE-CELL A64ICODE:STR,
   16 0 A64ICODE:MOVZ,
   back A64ICODE:B,
   fail A64ICODE:LBL,
   16 10 0 A64ICODE:ADDI,
   back A64ICODE:LBL,
   0 A64ICODE:SP 0 A64ICODE:LDR,    1 A64ICODE:SP 8 A64ICODE:LDR,    2 A64ICODE:SP 16 A64ICODE:LDR,   3 A64ICODE:SP 24 A64ICODE:LDR,   4 A64ICODE:SP 32 A64ICODE:LDR,
   5 A64ICODE:SP 40 A64ICODE:LDR,   6 A64ICODE:SP 48 A64ICODE:LDR,   7 A64ICODE:SP 56 A64ICODE:LDR,   8 A64ICODE:SP 64 A64ICODE:LDR,   9 A64ICODE:SP 72 A64ICODE:LDR,
   10 A64ICODE:SP 80 A64ICODE:LDR,  11 A64ICODE:SP 88 A64ICODE:LDR,  12 A64ICODE:SP 96 A64ICODE:LDR,  13 A64ICODE:SP 104 A64ICODE:LDR, 14 A64ICODE:SP 112 A64ICODE:LDR,
   15 A64ICODE:SP 120 A64ICODE:LDR, 17 A64ICODE:SP 128 A64ICODE:LDR,
   A64ICODE:SP A64ICODE:SP 144 A64ICODE:ADDI,  A64ICODE:RET, ;

\ LBCS ( x11=a x12=u ) appends a run and a space to the body text, with a
\ BODY-SOURCE row at base + cap. A map that fails throws E-BODY-TEXT at
\ LBGROWFAIL before anything is stored.
: EMIT-BCAP ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: pop put seed none src rowed :}
   A64ICODE:LBL A64ICODE:LBL {: grow:label gsub:label :}
   LBCS A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL BCAP-OK !
   A64ICODE:LBL BCAP-CP !
   A64ICODE:LBL BCAP-CD !
   17 12 0 A64ICODE:ADDI,                  \ len in x17: callers keep x6-x8
   14 XREG-RBASE BODYLEN-CELL A64ICODE:LDR,
   16 14 17 A64ICODE:ADD,  16 16 1 A64ICODE:ADDI,
   5 XREG-RBASE BODY-TEXT:BASE-CELL A64ICODE:LDR,  5 grow A64ICODE:CBZ,
   5 XREG-RBASE BODY-TEXT:CAP-CELL A64ICODE:LDR,  16 5 A64ICODE:CMP,  A64ASM:C-LS BCAP-OK A64ICODE:LABEL@ A64ICODE:BCOND,
   grow A64ICODE:LBL,
      A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,
      gsub A64ICODE:BL,
      30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI,
      16 BCAP-OK A64ICODE:LABEL@ A64ICODE:CBZ,
      LBGROWFAIL A64ICODE:LABEL@ A64ICODE:B,
   BCAP-OK A64ICODE:LABEL@ A64ICODE:LBL,
   17 rowed A64ICODE:CBZ,
   15 XREG-RBASE BODY-TEXT:BASE-CELL A64ICODE:LDR,  13 XREG-RBASE BODY-TEXT:CAP-CELL A64ICODE:LDR,  15 15 13 A64ICODE:ADD,
   16 XREG-RBASE BODY-TEXT:ROWS-CELL A64ICODE:LDR,
   pop A64ICODE:LBL,  16 put A64ICODE:CBZ,
      13 16 1 A64ICODE:SUBI,  13 13 4 A64ICODE:LSLI,  13 15 13 A64ICODE:ADD,  5 13 0 A64ICODE:LDR,
      5 14 A64ICODE:CMP,  A64ASM:C-CC put A64ICODE:BCOND,
      16 16 1 A64ICODE:SUBI,  pop A64ICODE:B,
   put A64ICODE:LBL,
   13 16 4 A64ICODE:LSLI,  13 15 13 A64ICODE:ADD,  14 13 0 A64ICODE:STR,
   16 16 1 A64ICODE:ADDI,  16 XREG-RBASE BODY-TEXT:ROWS-CELL A64ICODE:STR,
   15 XREG-RBASE SRCLOC:INB-CELL A64ICODE:LDR,  5 XREG-RBASE INE-CELL A64ICODE:LDR,
   \ The name and the run lie in [INB, INE); the first run is the name, seeded
   \ before PENDTKA-CELL is set.
   14 seed A64ICODE:CBZ,
   16 XREG-RBASE PENDTKA-CELL A64ICODE:LDR,
   16 15 A64ICODE:CMP,  A64ASM:C-CC none A64ICODE:BCOND,  16 5 A64ICODE:CMP,  A64ASM:C-CS none A64ICODE:BCOND,
   seed A64ICODE:LBL,
   11 15 A64ICODE:CMP,  A64ASM:C-CC none A64ICODE:BCOND,  11 5 A64ICODE:CMP,  A64ASM:C-CS none A64ICODE:BCOND,
   16 11 15 A64ICODE:SUB,  src A64ICODE:B,
   none A64ICODE:LBL,  16 BODY-SOURCE:NONE A64ICODE:LIT64,
   src A64ICODE:LBL,  16 13 8 A64ICODE:STR,
   rowed A64ICODE:LBL,
   15 XREG-RBASE BODY-TEXT:BASE-CELL A64ICODE:LDR,  15 15 14 A64ICODE:ADD,
   BCAP-CP A64ICODE:LABEL@ A64ICODE:LBL,  12 BCAP-CD A64ICODE:LABEL@ A64ICODE:CBZ,  13 11 0 A64ICODE:LDRB,  13 15 0 A64ICODE:STRB,
      15 15 1 A64ICODE:ADDI,  11 11 1 A64ICODE:ADDI,  12 12 1 A64ICODE:SUBI,  BCAP-CP A64ICODE:LABEL@ A64ICODE:B,
   BCAP-CD A64ICODE:LABEL@ A64ICODE:LBL,  13 32 A64ICODE:MOVZ,  13 15 0 A64ICODE:STRB,
   14 14 17 A64ICODE:ADD,  14 14 1 A64ICODE:ADDI,  14 XREG-RBASE BODYLEN-CELL A64ICODE:STR,
   A64ICODE:RET,
   gsub BCAP-GROW ;

: EMIT-TOK ( -- )
   LTOK A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL TOK-SKIP !
   A64ICODE:LBL TOK-HAS !
   A64ICODE:LBL TOK-SCAN !
   A64ICODE:LBL TOK-GOT !
   A64ICODE:LBL TOK-NONE !
   11 XREG-RBASE INP-CELL A64ICODE:LDR,  12 XREG-RBASE INE-CELL A64ICODE:LDR,
   TOK-SKIP A64ICODE:LABEL@ A64ICODE:LBL,
      11 12 A64ICODE:CMP,  A64ASM:C-GE TOK-NONE A64ICODE:LABEL@ A64ICODE:BCOND,
      9 11 0 A64ICODE:LDRB,  9 32 A64ICODE:CMPI,  A64ASM:C-HI TOK-HAS A64ICODE:LABEL@ A64ICODE:BCOND,
      11 11 1 A64ICODE:ADDI,  TOK-SKIP A64ICODE:LABEL@ A64ICODE:B,
   TOK-HAS A64ICODE:LABEL@ A64ICODE:LBL,  11 XREG-RBASE TKA-CELL A64ICODE:STR,
   TOK-SCAN A64ICODE:LABEL@ A64ICODE:LBL,
      11 12 A64ICODE:CMP,  A64ASM:C-GE TOK-GOT A64ICODE:LABEL@ A64ICODE:BCOND,
      9 11 0 A64ICODE:LDRB,  9 32 A64ICODE:CMPI,  A64ASM:C-LS TOK-GOT A64ICODE:LABEL@ A64ICODE:BCOND,
      11 11 1 A64ICODE:ADDI,  TOK-SCAN A64ICODE:LABEL@ A64ICODE:B,
   TOK-GOT A64ICODE:LABEL@ A64ICODE:LBL,  9 XREG-RBASE TKA-CELL A64ICODE:LDR,  9 11 9 A64ICODE:SUB,  9 XREG-RBASE TKL-CELL A64ICODE:STR,
      9 XREG-RBASE TOKENS-CELL A64ICODE:LDR,  9 9 1 A64ICODE:ADDI,  9 XREG-RBASE TOKENS-CELL A64ICODE:STR,
      11 XREG-RBASE INP-CELL A64ICODE:STR,  0 1 A64ICODE:MOVZ,  A64ICODE:RET,
   TOK-NONE A64ICODE:LABEL@ A64ICODE:LBL,  11 XREG-RBASE INP-CELL A64ICODE:STR,  0 0 A64ICODE:MOVZ,  A64ICODE:RET, ;
;package

package ENGINE-EMIT
\ LFLUSH makes the code in [x9, CP) visible to instruction fetch; LFLUSHTO takes
\ the end in x11 instead, for code that stands above CP. Clobbers x10..x13.
\ macOS steps by 64 bytes: XNU leaves SCTLR_EL1.UCT clear, so a user-mode
\ CTR_EL0 read traps.
\ LFLUSHTO is registered as the sealed (FLUSHTO) engine helper: the fork
\ primitive's Linux child branches to it directly.
: EMIT-FLUSH ( -- )
   A64ICODE:LBL {: end:label :}
   s" (FLUSHTO)" LFLUSHTO A64ICODE:LABEL@ LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   LFLUSH A64ICODE:LABEL@ A64ICODE:LBL,
   11 CP 0 A64ICODE:ADDI,
   LFLUSHTO A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL FL-DL !
   A64ICODE:LBL FL-DD !
   A64ICODE:LBL FL-IL !
   A64ICODE:LBL FL-ID !
   HB-TARGET-MACOS? IF  13 64 A64ICODE:MOVZ,
   ELSE
      $D53B002D A64ICODE:EMITW                                       \ mrs x13, CTR_EL0
      12 13 15 A64ICODE:ANDI,
      13 13 16 A64ICODE:LSRI,  13 13 15 A64ICODE:ANDI,  10 4 A64ICODE:MOVZ,  13 10 13 A64ICODE:LSLV,   \ x13 = 4 << DminLine
   THEN
   10 31 13 A64ICODE:SUB,  10 9 10 A64ICODE:AND,        \ x9 down to its line, or the last line is skipped
   FL-DL A64ICODE:LABEL@ A64ICODE:LBL,  10 11 A64ICODE:CMP,  A64ASM:C-GE FL-DD A64ICODE:LABEL@ A64ICODE:BCOND,  10 A64ICODE:DCCVAU,  10 10 13 A64ICODE:ADD,  FL-DL A64ICODE:LABEL@ A64ICODE:B,
   FL-DD A64ICODE:LABEL@ A64ICODE:LBL,  A64ICODE:DSB-ISH,
   HB-TARGET-MACOS? 0= IF
      10 4 A64ICODE:MOVZ,  13 10 12 A64ICODE:LSLV,                                   \ x13 = 4 << IminLine
   THEN
   10 31 13 A64ICODE:SUB,  10 9 10 A64ICODE:AND,
   FL-IL A64ICODE:LABEL@ A64ICODE:LBL,  10 11 A64ICODE:CMP,  A64ASM:C-GE FL-ID A64ICODE:LABEL@ A64ICODE:BCOND,  10 A64ICODE:ICIVAU,  10 10 13 A64ICODE:ADD,  FL-IL A64ICODE:LABEL@ A64ICODE:B,
   FL-ID A64ICODE:LABEL@ A64ICODE:LBL,  A64ICODE:DSB-ISH,  A64ICODE:ISB,  A64ICODE:RET,
   end A64ICODE:LBL, ;

public
variable LHIDXBUILD

private
\ Emit: x6 = the first slot of record x3's key, its folded name and wid cell;
\ x8 = HIDX-SLOTS, a walk's most slots. Clobbers x2 x4 x5 x7 x15 x16.
: C-HIDX-CHAIN ( -- )
   A64ICODE:LBL {: rinl:label :}
   5 DREC A64ICODE:MOVZ,  5 3 5 A64ICODE:MUL,  5 DBASE 5 A64ICODE:ADD,
   2 5 40 A64ICODE:LDR,
   16 5 24 A64ICODE:ADDI,
   15 5 16 A64ICODE:LDR,  15 15 DNAME-FLAG-BITS A64ICODE:LSLI,  15 15 DNAME-FLAG-BITS A64ICODE:LSRI,
   4 5 16 A64ICODE:LDR,  4 4 DNAME-EXT A64ICODE:ANDI,  4 rinl A64ICODE:CBZ,
      16 5 24 A64ICODE:LDR,
   rinl A64ICODE:LBL,
   16 15 6 4 5 7 C-HIDX-HASH
   6 6 2 5 C-HIDX-SLOT,
   8 HIDX-SLOTS A64ICODE:LIT64, ;

\ Emit: index record x3 in table x14 at its chain's first empty or stale slot;
\ meeting x3 before either writes nothing, and a walk of every slot dies.
\ Clobbers x2 x4 x5 x6 x7 x8 x15 x16 x17.
: C-HIDX-INS ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: iloop:label iempty:label iput:label idone:label :}
   C-HIDX-CHAIN
   iloop A64ICODE:LBL,
      17 6 2 A64ICODE:LSLI,  17 14 17 A64ICODE:ADD,  4 17 0 A64ICODE:LDRW,
      4 iempty A64ICODE:CBZ,
      4 4 1 A64ICODE:SUBI,  4 3 A64ICODE:CMP,  A64ASM:C-EQ idone A64ICODE:BCOND,
      4 NDICT A64ICODE:CMP,  A64ASM:C-GE iput A64ICODE:BCOND,
      8 8 1 A64ICODE:SUBI,  8 HIDX-EMIT:LFULL A64ICODE:LABEL@ A64ICODE:CBZ,
      6 6 1 A64ICODE:ADDI,  5 HIDX-SLOTS 1 - A64ICODE:LIT64,  6 6 5 A64ICODE:AND,  iloop A64ICODE:B,
   iempty A64ICODE:LBL,
      4 XREG-RBASE HIDX:CLAIMS A64ICODE:LDR,  4 4 1 A64ICODE:ADDI,  4 XREG-RBASE HIDX:CLAIMS A64ICODE:STR,
   iput A64ICODE:LBL,
      4 3 1 A64ICODE:ADDI,  4 17 0 A64ICODE:STRW,
   idone A64ICODE:LBL, ;

\ Emit: branch to `held` when record x3's chain in table x14 holds x3 before
\ its first empty slot, the stretch every probe for the record's key walks;
\ else fall through. Clobbers x2 x4 x5 x6 x7 x8 x15 x16 x17.
: C-HIDX-HELD? ( label -- ) {: held:label :}
   A64ICODE:LBL A64ICODE:LBL {: hloop:label hnone:label :}
   C-HIDX-CHAIN
   hloop A64ICODE:LBL,
      17 6 2 A64ICODE:LSLI,  17 14 17 A64ICODE:ADD,  4 17 0 A64ICODE:LDRW,
      4 hnone A64ICODE:CBZ,
      4 4 1 A64ICODE:SUBI,  4 3 A64ICODE:CMP,  A64ASM:C-EQ held A64ICODE:BCOND,
      8 8 1 A64ICODE:SUBI,  8 hnone A64ICODE:CBZ,
      6 6 1 A64ICODE:ADDI,  5 HIDX-SLOTS 1 - A64ICODE:LIT64,  6 6 5 A64ICODE:AND,  hloop A64ICODE:B,
   hnone A64ICODE:LBL, ;

\ The frame LHIDXADD and HIDX-EMIT:LREKEY open: x30 and every register their
\ walks, insert and load bound clobber. EMIT-HIDX's aret closes it.
: HIDX-SAVE, ( -- )
   A64ICODE:SP A64ICODE:SP 96 A64ICODE:SUBI,
   30 A64ICODE:SP 0 A64ICODE:STR,  2 A64ICODE:SP 8 A64ICODE:STR,  3 A64ICODE:SP 16 A64ICODE:STR,  4 A64ICODE:SP 24 A64ICODE:STR,  5 A64ICODE:SP 32 A64ICODE:STR,
   6 A64ICODE:SP 40 A64ICODE:STR,  7 A64ICODE:SP 48 A64ICODE:STR,  14 A64ICODE:SP 56 A64ICODE:STR,  15 A64ICODE:SP 64 A64ICODE:STR,  16 A64ICODE:SP 72 A64ICODE:STR,  17 A64ICODE:SP 80 A64ICODE:STR,
   8 A64ICODE:SP 88 A64ICODE:STR, ;

\ LHIDXADD inserts record NDICT-1 mid-publish, saving its whole clobber set, and
\ compacts the table when HIDX:CLAIMS crosses HIDX:LOAD-MAX, sweeping the stale
\ slots rollback leaves. HIDX-EMIT:LREBUILD zeroes the table and HIDX:CLAIMS and
\ re-inserts [0,NDICT): the only operation that lowers HIDX:CLAIMS.
\ HIDX-EMIT:LFULL is the loud exit for the unreachable full wrap.
\ HIDX-EMIT:LREKEY (x3 = record index) re-indexes a record under its current wid
\ cell unless its chain already holds it, register-transparent.
\ A writer of a published record's wid cell calls it after the store: a rebuild
\ keys a record under the cell it holds then.
: EMIT-HIDX ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
   {: aret:label bfail:label floop:label fdone:label rret:label zloop:label
      zdone:label :}
   A64ICODE:LBL A64ICODE:LBL {: bhave:label ains:label :}
   LHIDXADD A64ICODE:LABEL@ A64ICODE:LBL,
      HIDX-SAVE,
      14 XREG-RBASE HIDXP-CELL A64ICODE:LDR,  14 aret A64ICODE:CBZ,
      3 NDICT 0 A64ICODE:ADDI,  3 3 1 A64ICODE:SUBI,
      ains A64ICODE:LBL,
      C-HIDX-INS
      \ the load bound: compact before chains can approach a full wrap
      4 XREG-RBASE HIDX:CLAIMS A64ICODE:LDR,  5 HIDX:LOAD-MAX A64ICODE:LIT64,  4 5 A64ICODE:CMP,  A64ASM:C-LT aret A64ICODE:BCOND,
      HIDX-EMIT:LREBUILD A64ICODE:LABEL@ A64ICODE:BL,
      aret A64ICODE:LBL,
      30 A64ICODE:SP 0 A64ICODE:LDR,  2 A64ICODE:SP 8 A64ICODE:LDR,  3 A64ICODE:SP 16 A64ICODE:LDR,  4 A64ICODE:SP 24 A64ICODE:LDR,  5 A64ICODE:SP 32 A64ICODE:LDR,
      6 A64ICODE:SP 40 A64ICODE:LDR,  7 A64ICODE:SP 48 A64ICODE:LDR,  14 A64ICODE:SP 56 A64ICODE:LDR,  15 A64ICODE:SP 64 A64ICODE:LDR,  16 A64ICODE:SP 72 A64ICODE:LDR,  17 A64ICODE:SP 80 A64ICODE:LDR,
      8 A64ICODE:SP 88 A64ICODE:LDR,
      A64ICODE:SP A64ICODE:SP 96 A64ICODE:ADDI,  A64ICODE:RET,
   HIDX-EMIT:LREKEY A64ICODE:LABEL@ A64ICODE:LBL,
      HIDX-SAVE,
      14 XREG-RBASE HIDXP-CELL A64ICODE:LDR,  14 aret A64ICODE:CBZ,
      aret C-HIDX-HELD?
      ains A64ICODE:B,
   LHIDXBUILD A64ICODE:LABEL@ A64ICODE:LBL,
      \ Startup BLs here between source setup and the interpret loop: save all
      \ it and the mmap syscall touch. A seeded start builds before the seed
      \ registers and the runtime-state pass reaches here again: an existing
      \ table is refilled in place, never mapped twice.
      A64ICODE:SP A64ICODE:SP 160 A64ICODE:SUBI,
      30 A64ICODE:SP 0 A64ICODE:STR,   0 A64ICODE:SP 8 A64ICODE:STR,   1 A64ICODE:SP 16 A64ICODE:STR,  2 A64ICODE:SP 24 A64ICODE:STR,  3 A64ICODE:SP 32 A64ICODE:STR,
      4 A64ICODE:SP 40 A64ICODE:STR,   5 A64ICODE:SP 48 A64ICODE:STR,  6 A64ICODE:SP 56 A64ICODE:STR,  7 A64ICODE:SP 64 A64ICODE:STR,  8 A64ICODE:SP 72 A64ICODE:STR,
      13 A64ICODE:SP 80 A64ICODE:STR,  14 A64ICODE:SP 88 A64ICODE:STR, 15 A64ICODE:SP 96 A64ICODE:STR, 16 A64ICODE:SP 104 A64ICODE:STR, 17 A64ICODE:SP 112 A64ICODE:STR,
      14 XREG-RBASE HIDXP-CELL A64ICODE:LDR,  14 bhave A64ICODE:CBNZ,
      0 0 A64ICODE:MOVZ,  1 HIDX-BYTES A64ICODE:LIT64,  2 3 A64ICODE:MOVZ,
      3 SYSCALL:MAP-ANON-PRIVATE A64ICODE:LIT64,  4 0 A64ICODE:MOVN,  5 0 A64ICODE:MOVZ,  SYSCALL:NR-MMAP SYSCALL:SYS,
      4 A64ASM:C-CS A64ICODE:CSET,  4 bfail A64ICODE:CBNZ,
      14 0 0 A64ICODE:ADDI,  14 XREG-RBASE HIDXP-CELL A64ICODE:STR,
      4 0 A64ICODE:MOVZ,  4 XREG-RBASE HIDX:CLAIMS A64ICODE:STR,             \ fresh pages hold no claims
      bhave A64ICODE:LBL,
      HIDX-EMIT:LREBUILD A64ICODE:LABEL@ A64ICODE:BL,
      30 A64ICODE:SP 0 A64ICODE:LDR,   0 A64ICODE:SP 8 A64ICODE:LDR,   1 A64ICODE:SP 16 A64ICODE:LDR,  2 A64ICODE:SP 24 A64ICODE:LDR,  3 A64ICODE:SP 32 A64ICODE:LDR,
      4 A64ICODE:SP 40 A64ICODE:LDR,   5 A64ICODE:SP 48 A64ICODE:LDR,  6 A64ICODE:SP 56 A64ICODE:LDR,  7 A64ICODE:SP 64 A64ICODE:LDR,  8 A64ICODE:SP 72 A64ICODE:LDR,
      13 A64ICODE:SP 80 A64ICODE:LDR,  14 A64ICODE:SP 88 A64ICODE:LDR, 15 A64ICODE:SP 96 A64ICODE:LDR, 16 A64ICODE:SP 104 A64ICODE:LDR, 17 A64ICODE:SP 112 A64ICODE:LDR,
      A64ICODE:SP A64ICODE:SP 160 A64ICODE:ADDI,  A64ICODE:RET,
      bfail A64ICODE:LBL,
         s" E-DICT-INDEX habu: dictionary index alloc failed" EXIT-STATUS:SYSTEM CRASH:DIE-LINE,
   HIDX-EMIT:LREBUILD A64ICODE:LABEL@ A64ICODE:LBL,
      \ register-transparent: LHIDXADD and LHIDXBUILD call it mid-primitive
      \ with caller state live.
      A64ICODE:SP A64ICODE:SP 160 A64ICODE:SUBI,
      30 A64ICODE:SP 0 A64ICODE:STR,   2 A64ICODE:SP 8 A64ICODE:STR,   3 A64ICODE:SP 16 A64ICODE:STR,  4 A64ICODE:SP 24 A64ICODE:STR,  5 A64ICODE:SP 32 A64ICODE:STR,
      6 A64ICODE:SP 40 A64ICODE:STR,   7 A64ICODE:SP 48 A64ICODE:STR,  8 A64ICODE:SP 56 A64ICODE:STR,  13 A64ICODE:SP 64 A64ICODE:STR, 14 A64ICODE:SP 72 A64ICODE:STR,
      15 A64ICODE:SP 80 A64ICODE:STR,  16 A64ICODE:SP 88 A64ICODE:STR, 17 A64ICODE:SP 96 A64ICODE:STR,
      14 XREG-RBASE HIDXP-CELL A64ICODE:LDR,  14 rret A64ICODE:CBZ,          \ pre-build window: nothing to compact
      \ NDICT <= DICT-CAP < HIDX:LOAD-MAX: the rebuilt table stays under the bound
      13 0 A64ICODE:MOVZ,  4 0 A64ICODE:MOVZ,  15 HIDX-BYTES A64ICODE:LIT64,
      zloop A64ICODE:LBL,
         13 15 A64ICODE:CMP,  A64ASM:C-GE zdone A64ICODE:BCOND,
         17 14 13 A64ICODE:ADD,  4 17 0 A64ICODE:STR,
         13 13 8 A64ICODE:ADDI,  zloop A64ICODE:B,
      zdone A64ICODE:LBL,
      4 XREG-RBASE HIDX:CLAIMS A64ICODE:STR,
      13 0 A64ICODE:MOVZ,
      floop A64ICODE:LBL,  13 NDICT A64ICODE:CMP,  A64ASM:C-GE fdone A64ICODE:BCOND,
         3 13 0 A64ICODE:ADDI,  C-HIDX-INS
         13 13 1 A64ICODE:ADDI,  floop A64ICODE:B,
      fdone A64ICODE:LBL,
      rret A64ICODE:LBL,
      30 A64ICODE:SP 0 A64ICODE:LDR,   2 A64ICODE:SP 8 A64ICODE:LDR,   3 A64ICODE:SP 16 A64ICODE:LDR,  4 A64ICODE:SP 24 A64ICODE:LDR,  5 A64ICODE:SP 32 A64ICODE:LDR,
      6 A64ICODE:SP 40 A64ICODE:LDR,   7 A64ICODE:SP 48 A64ICODE:LDR,  8 A64ICODE:SP 56 A64ICODE:LDR,  13 A64ICODE:SP 64 A64ICODE:LDR, 14 A64ICODE:SP 72 A64ICODE:LDR,
      15 A64ICODE:SP 80 A64ICODE:LDR,  16 A64ICODE:SP 88 A64ICODE:LDR, 17 A64ICODE:SP 96 A64ICODE:LDR,
      A64ICODE:SP A64ICODE:SP 160 A64ICODE:ADDI,  A64ICODE:RET,
   HIDX-EMIT:LFULL A64ICODE:LABEL@ A64ICODE:LBL,
      s" E-DICT-INDEX habu: dictionary index exhausted" EXIT-STATUS:INTERNAL CRASH:DIE-LINE, ;

\ A dictionary slot's occurrence is owned by one anonymous mapping. The first
\ cell is the last issued number; the remaining DICT-CAP cells are indexed by
\ slot. None of these cells are in the rewindable DATA heap or captured image.
\ The last issued number rises by one per issue: 64 bits never run out.
: EMIT-DEF-OCC ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
   {: lower:label raise:label loop:label raised:label
      done:label clear-loop:label reset:label :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
   {: reset-loop:label init-bad:label append-done:label :}
   EM-DEF-OCC:LISSUE A64ICODE:LABEL@ A64ICODE:LBL,
      A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,
      9 A64ICODE:SP 0 A64ICODE:STR,  10 A64ICODE:SP 8 A64ICODE:STR,  11 A64ICODE:SP 16 A64ICODE:STR,  12 A64ICODE:SP 24 A64ICODE:STR,
      10 XREG-RBASE DEF-OCC:PTR-CELL A64ICODE:LDR,  11 10 0 A64ICODE:LDR,
      11 11 1 A64ICODE:ADDI,  11 10 0 A64ICODE:STR,
      12 9 3 A64ICODE:LSLI,  12 10 12 A64ICODE:ADD,  11 12 8 A64ICODE:STR,
      9 A64ICODE:SP 0 A64ICODE:LDR,  10 A64ICODE:SP 8 A64ICODE:LDR,  11 A64ICODE:SP 16 A64ICODE:LDR,  12 A64ICODE:SP 24 A64ICODE:LDR,
      A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI,  A64ICODE:RET,
   EM-DEF-OCC:LCOUNT A64ICODE:LABEL@ A64ICODE:LBL,
      A64ICODE:SP A64ICODE:SP 64 A64ICODE:SUBI,
      30 A64ICODE:SP 0 A64ICODE:STR,  9 A64ICODE:SP 8 A64ICODE:STR,  10 A64ICODE:SP 16 A64ICODE:STR,  11 A64ICODE:SP 24 A64ICODE:STR,
      12 A64ICODE:SP 32 A64ICODE:STR,  13 A64ICODE:SP 40 A64ICODE:STR,  14 A64ICODE:SP 48 A64ICODE:STR,  15 A64ICODE:SP 56 A64ICODE:STR,
      10 NDICT 0 A64ICODE:ADDI,
      9 10 A64ICODE:CMP,  A64ASM:C-LT lower A64ICODE:BCOND,  A64ASM:C-GT raise A64ICODE:BCOND,
       done A64ICODE:B,
       raise A64ICODE:LBL,
         11 XREG-RBASE DEF-OCC:PTR-CELL A64ICODE:LDR,  13 11 0 A64ICODE:LDR,
      loop A64ICODE:LBL,
         10 9 A64ICODE:CMP,  A64ASM:C-CS raised A64ICODE:BCOND,
         13 13 1 A64ICODE:ADDI,
         12 10 3 A64ICODE:LSLI,  12 11 12 A64ICODE:ADD,  13 12 8 A64ICODE:STR,
         10 10 1 A64ICODE:ADDI,  loop A64ICODE:B,
      raised A64ICODE:LBL,
         13 11 0 A64ICODE:STR,
         done A64ICODE:B,
      lower A64ICODE:LBL,
         PROF-ABI:LPROFTRIM A64ICODE:LABEL@ A64ICODE:BL,
         12 XREG-RBASE NDICT-HW-CELL A64ICODE:LDR,  12 NDICT A64ICODE:CMP,  12 12 NDICT A64ASM:C-HI A64ICODE:CSEL,
         12 XREG-RBASE NDICT-HW-CELL A64ICODE:STR,
         11 XREG-RBASE DEF-OCC:PTR-CELL A64ICODE:LDR,  10 9 0 A64ICODE:ADDI,
      clear-loop A64ICODE:LBL,
         10 NDICT A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
         12 10 3 A64ICODE:LSLI,  12 11 12 A64ICODE:ADD,
         13 0 A64ICODE:MOVZ,  13 12 8 A64ICODE:STR,
         10 10 1 A64ICODE:ADDI,  clear-loop A64ICODE:B,
      done A64ICODE:LBL,
      NDICT 9 0 A64ICODE:ADDI,
      30 A64ICODE:SP 0 A64ICODE:LDR,  9 A64ICODE:SP 8 A64ICODE:LDR,  10 A64ICODE:SP 16 A64ICODE:LDR,  11 A64ICODE:SP 24 A64ICODE:LDR,
      12 A64ICODE:SP 32 A64ICODE:LDR,  13 A64ICODE:SP 40 A64ICODE:LDR,  14 A64ICODE:SP 48 A64ICODE:LDR,  15 A64ICODE:SP 56 A64ICODE:LDR,
      A64ICODE:SP A64ICODE:SP 64 A64ICODE:ADDI,  A64ICODE:RET,
   EM-DEF-OCC:LAPPEND A64ICODE:LABEL@ A64ICODE:LBL,
      A64ICODE:SP A64ICODE:SP 80 A64ICODE:SUBI,
      30 A64ICODE:SP 0 A64ICODE:STR,  9 A64ICODE:SP 8 A64ICODE:STR,  10 A64ICODE:SP 16 A64ICODE:STR,  11 A64ICODE:SP 24 A64ICODE:STR,
      12 A64ICODE:SP 32 A64ICODE:STR,  13 A64ICODE:SP 40 A64ICODE:STR,  14 A64ICODE:SP 48 A64ICODE:STR,  15 A64ICODE:SP 56 A64ICODE:STR,
      16 A64ICODE:SP 64 A64ICODE:STR,  17 A64ICODE:SP 72 A64ICODE:STR,
      9 NDICT 1 A64ICODE:ADDI,  EM-DEF-OCC:LCOUNT A64ICODE:LABEL@ A64ICODE:BL,
      \ Only APPEND publishes a new record. COUNT also restores old rows.
      11 XREG-RBASE NCOMP-DISPATCH:DECL-CELL A64ICODE:LDR,
      11 append-done A64ICODE:CBZ,
      12 11 CHECKER-OWNER-ABI:HEADER-BYTES A64ICODE:SUBI,
      13 12 0 A64ICODE:LDR,  14 CHECKER-OWNER-ABI:MAGIC A64ICODE:LIT64,
      13 14 A64ICODE:CMP,  A64ASM:C-NE append-done A64ICODE:BCOND,
      13 12 8 A64ICODE:LDR,  14 CHECKER-OWNER-ABI:RECORD-PUBLISHED-OFF CELL + A64ICODE:LIT64,
      13 14 A64ICODE:CMP,  A64ASM:C-LT append-done A64ICODE:BCOND,
      11 11 CHECKER-OWNER-ABI:RECORD-PUBLISHED-OFF A64ICODE:LDR,  11 append-done A64ICODE:CBZ,
         9 NDICT 1 A64ICODE:SUBI,  9 RT:G-PUSH  11 A64ICODE:BLR,
      append-done A64ICODE:LBL,
      30 A64ICODE:SP 0 A64ICODE:LDR,  9 A64ICODE:SP 8 A64ICODE:LDR,  10 A64ICODE:SP 16 A64ICODE:LDR,  11 A64ICODE:SP 24 A64ICODE:LDR,
      12 A64ICODE:SP 32 A64ICODE:LDR,  13 A64ICODE:SP 40 A64ICODE:LDR,  14 A64ICODE:SP 48 A64ICODE:LDR,  15 A64ICODE:SP 56 A64ICODE:LDR,
      16 A64ICODE:SP 64 A64ICODE:LDR,  17 A64ICODE:SP 72 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 80 A64ICODE:ADDI,  A64ICODE:RET,
   EM-DEF-OCC:LRESET A64ICODE:LABEL@ A64ICODE:LBL,
      A64ICODE:SP A64ICODE:SP 64 A64ICODE:SUBI,
      30 A64ICODE:SP 0 A64ICODE:STR,  9 A64ICODE:SP 8 A64ICODE:STR,  10 A64ICODE:SP 16 A64ICODE:STR,  11 A64ICODE:SP 24 A64ICODE:STR,
      12 A64ICODE:SP 32 A64ICODE:STR,  13 A64ICODE:SP 40 A64ICODE:STR,  14 A64ICODE:SP 48 A64ICODE:STR,
      10 XREG-RBASE DEF-OCC:PTR-CELL A64ICODE:LDR,  11 0 A64ICODE:MOVZ,
      reset-loop A64ICODE:LBL,
         12 DICT-CAP A64ICODE:LIT64,  11 12 A64ICODE:CMP,  A64ASM:C-CS reset A64ICODE:BCOND,
         12 11 3 A64ICODE:LSLI,  12 10 12 A64ICODE:ADD,
         13 0 A64ICODE:MOVZ,  13 12 8 A64ICODE:STR,
         11 11 1 A64ICODE:ADDI,  reset-loop A64ICODE:B,
      reset A64ICODE:LBL,
      NDICT 0 A64ICODE:MOVZ,
      EM-DEF-OCC:LCOUNT A64ICODE:LABEL@ A64ICODE:BL,
      30 A64ICODE:SP 0 A64ICODE:LDR,  9 A64ICODE:SP 8 A64ICODE:LDR,  10 A64ICODE:SP 16 A64ICODE:LDR,  11 A64ICODE:SP 24 A64ICODE:LDR,
      12 A64ICODE:SP 32 A64ICODE:LDR,  13 A64ICODE:SP 40 A64ICODE:LDR,  14 A64ICODE:SP 48 A64ICODE:LDR,
      A64ICODE:SP A64ICODE:SP 64 A64ICODE:ADDI,  A64ICODE:RET,
   \ Fresh process installation. Seed records already occupy [0,NDICT).
   EM-DEF-OCC:LINIT A64ICODE:LABEL@ A64ICODE:LBL,
      A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,  13 A64ICODE:SP 8 A64ICODE:STR,  14 A64ICODE:SP 16 A64ICODE:STR,  15 A64ICODE:SP 24 A64ICODE:STR,
      0 0 A64ICODE:MOVZ,  1 DEF-OCC:STATE-BYTES A64ICODE:LIT64,  2 3 A64ICODE:MOVZ,
      3 SYSCALL:MAP-ANON-PRIVATE A64ICODE:LIT64,  4 0 A64ICODE:MOVN,  5 0 A64ICODE:MOVZ,
      SYSCALL:NR-MMAP SYSCALL:SYS,  A64ASM:C-CS init-bad A64ICODE:BCOND,
      0 XREG-RBASE DEF-OCC:PTR-CELL A64ICODE:STR,
      9 NDICT 0 A64ICODE:ADDI,  EM-DEF-OCC:LRESET A64ICODE:LABEL@ A64ICODE:BL,
      30 A64ICODE:SP 0 A64ICODE:LDR,  13 A64ICODE:SP 8 A64ICODE:LDR,  14 A64ICODE:SP 16 A64ICODE:LDR,  15 A64ICODE:SP 24 A64ICODE:LDR,
      A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI,  A64ICODE:RET,
   init-bad A64ICODE:LBL,
      3 DEF-OCC:STATE-BYTES A64ICODE:LIT64,
      s" hb: mapping the definition occurrence table failed; raise the address-space limit (ulimit -v) above its size in bytes:"
      EXIT-STATUS:SYSTEM 3 CRASH:DIE-LINE-VALUE, ;

\ x9 names the four-byte patch (zero for an effect-only change); x10 names the
\ created record. Every record covering changed bytes is replaced separately.
: EMIT-DEF-OCC-OVERLAP ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: hit:label miss:label span:label :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: clear-loop:label clear-next:label cleared:label
      issue-loop:label issue-next:label issued:label :}
   EM-DEF-OCC:LMATCH A64ICODE:LABEL@ A64ICODE:LBL,
      12 0 A64ICODE:MOVZ,
      11 10 A64ICODE:CMP,  A64ASM:C-EQ hit A64ICODE:BCOND,
      9 miss A64ICODE:CBZ,
      15 11 40 A64ICODE:LDR,  16 WL:RETIRED invert A64ICODE:MOVN,
      15 16 A64ICODE:CMP,  A64ASM:C-EQ span A64ICODE:BCOND,
      15 0 A64ICODE:CMPI,  A64ASM:C-LT miss A64ICODE:BCOND,                          \ a package row, live or retired
   span A64ICODE:LBL,
      15 11 0 A64ICODE:LDR,  15 9 A64ICODE:CMP,  A64ASM:C-HI miss A64ICODE:BCOND,
      16 11 8 A64ICODE:LDR,
      15 15 16 A64ICODE:ADD,  15 9 A64ICODE:CMP,  A64ASM:C-LS miss A64ICODE:BCOND,
   hit A64ICODE:LBL,
      12 1 A64ICODE:MOVZ,  A64ICODE:RET,
   miss A64ICODE:LBL,
      12 0 A64ICODE:MOVZ,  A64ICODE:RET,
   EM-DEF-OCC:LBEFORE A64ICODE:LABEL@ A64ICODE:LBL,
      A64ICODE:SP A64ICODE:SP 80 A64ICODE:SUBI,
      30 A64ICODE:SP 0 A64ICODE:STR,  9 A64ICODE:SP 8 A64ICODE:STR,  10 A64ICODE:SP 16 A64ICODE:STR,  11 A64ICODE:SP 24 A64ICODE:STR,
      12 A64ICODE:SP 32 A64ICODE:STR,  13 A64ICODE:SP 40 A64ICODE:STR,  14 A64ICODE:SP 48 A64ICODE:STR,  15 A64ICODE:SP 56 A64ICODE:STR,
      16 A64ICODE:SP 64 A64ICODE:STR,  17 A64ICODE:SP 72 A64ICODE:STR,
      13 0 A64ICODE:MOVZ,
   clear-loop A64ICODE:LBL,
      13 NDICT A64ICODE:CMP,  A64ASM:C-CS cleared A64ICODE:BCOND,
      11 DREC A64ICODE:MOVZ,  11 13 11 A64ICODE:MUL,  11 DBASE 11 A64ICODE:ADD,
      EM-DEF-OCC:LMATCH A64ICODE:LABEL@ A64ICODE:BL,
      12 clear-next A64ICODE:CBZ,
      15 XREG-RBASE DEF-OCC:PTR-CELL A64ICODE:LDR,
      16 13 3 A64ICODE:LSLI,  15 15 16 A64ICODE:ADD,
      16 0 A64ICODE:MOVZ,  16 15 8 A64ICODE:STR,
   clear-next A64ICODE:LBL,
      13 13 1 A64ICODE:ADDI,  clear-loop A64ICODE:B,
   cleared A64ICODE:LBL,
      30 A64ICODE:SP 0 A64ICODE:LDR,  9 A64ICODE:SP 8 A64ICODE:LDR,  10 A64ICODE:SP 16 A64ICODE:LDR,  11 A64ICODE:SP 24 A64ICODE:LDR,
      12 A64ICODE:SP 32 A64ICODE:LDR,  13 A64ICODE:SP 40 A64ICODE:LDR,  14 A64ICODE:SP 48 A64ICODE:LDR,  15 A64ICODE:SP 56 A64ICODE:LDR,
      16 A64ICODE:SP 64 A64ICODE:LDR,  17 A64ICODE:SP 72 A64ICODE:LDR,
      A64ICODE:SP A64ICODE:SP 80 A64ICODE:ADDI,  A64ICODE:RET,
   EM-DEF-OCC:LAFTER A64ICODE:LABEL@ A64ICODE:LBL,
      A64ICODE:SP A64ICODE:SP 80 A64ICODE:SUBI,
      30 A64ICODE:SP 0 A64ICODE:STR,  9 A64ICODE:SP 8 A64ICODE:STR,  10 A64ICODE:SP 16 A64ICODE:STR,  11 A64ICODE:SP 24 A64ICODE:STR,
      12 A64ICODE:SP 32 A64ICODE:STR,  13 A64ICODE:SP 40 A64ICODE:STR,  14 A64ICODE:SP 48 A64ICODE:STR,  15 A64ICODE:SP 56 A64ICODE:STR,
      16 A64ICODE:SP 64 A64ICODE:STR,  17 A64ICODE:SP 72 A64ICODE:STR,
      13 0 A64ICODE:MOVZ,
   issue-loop A64ICODE:LBL,
      13 NDICT A64ICODE:CMP,  A64ASM:C-CS issued A64ICODE:BCOND,
      11 DREC A64ICODE:MOVZ,  11 13 11 A64ICODE:MUL,  11 DBASE 11 A64ICODE:ADD,
      EM-DEF-OCC:LMATCH A64ICODE:LABEL@ A64ICODE:BL,
      12 issue-next A64ICODE:CBZ,
      9 13 0 A64ICODE:ADDI,  EM-DEF-OCC:LISSUE A64ICODE:LABEL@ A64ICODE:BL,
      9 A64ICODE:SP 8 A64ICODE:LDR,
   issue-next A64ICODE:LBL,
      13 13 1 A64ICODE:ADDI,  issue-loop A64ICODE:B,
   issued A64ICODE:LBL,
      30 A64ICODE:SP 0 A64ICODE:LDR,  9 A64ICODE:SP 8 A64ICODE:LDR,  10 A64ICODE:SP 16 A64ICODE:LDR,  11 A64ICODE:SP 24 A64ICODE:LDR,
      12 A64ICODE:SP 32 A64ICODE:LDR,  13 A64ICODE:SP 40 A64ICODE:LDR,  14 A64ICODE:SP 48 A64ICODE:LDR,  15 A64ICODE:SP 56 A64ICODE:LDR,
      16 A64ICODE:SP 64 A64ICODE:LDR,  17 A64ICODE:SP 72 A64ICODE:LDR,
      A64ICODE:SP A64ICODE:SP 80 A64ICODE:ADDI,  A64ICODE:RET, ;

variable FIND-LINEAR
variable FIND-HLOOP
variable FIND-HNEXT
variable FIND-HINL
variable FIND-HCMP
variable FIND-HMATCH

public
\ LHEADROW: the package row a qualifier's head names, read by LFIND, a
\ qualified definition and `using`. Inside package A, or a part of it, the
\ head names A's part `A:head` first, then a top-level package. In: x9 =
\ head, x17 = its length. Out: x12 = the row or 0. It preserves x0, x1, x9,
\ x10, x13 and x17 and clobbers x2-x8, x11, x14-x16.
variable LHEADROW
private
: EMIT-HEAD-ROW ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
   {: top:label inline:label bare:label done:label :}
   LHEADROW A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP 64 A64ICODE:SUBI,
   30 A64ICODE:SP 0 A64ICODE:STR,  0 A64ICODE:SP 8 A64ICODE:STR,  1 A64ICODE:SP 16 A64ICODE:STR,  9 A64ICODE:SP 24 A64ICODE:STR,
   10 A64ICODE:SP 32 A64ICODE:STR,  13 A64ICODE:SP 40 A64ICODE:STR,  17 A64ICODE:SP 48 A64ICODE:STR,
   0 9 0 A64ICODE:ADDI,  1 17 0 A64ICODE:ADDI,
   2 XREG-RBASE BOOT:NAMESPACE-WL-CELL A64ICODE:LDR,
   5 XREG-RBASE ENCLOSING:REC A64ICODE:LDR,  5 top A64ICODE:CBNZ,
   5 XREG-RBASE PKG-REC-CELL A64ICODE:LDR,  5 bare A64ICODE:CBZ,
   top A64ICODE:LBL,
   10 5 16 A64ICODE:LDR,  10 10 DNAME-FLAG-BITS A64ICODE:LSLI,  10 10 DNAME-FLAG-BITS A64ICODE:LSRI,
   9 5 24 A64ICODE:ADDI,
   14 5 16 A64ICODE:LDR,  14 14 DNAME-EXT A64ICODE:ANDI,  14 inline A64ICODE:CBZ,
      9 5 24 A64ICODE:LDR,
   inline A64ICODE:LBL,
   WLFIND:LPAIR A64ICODE:LABEL@ A64ICODE:BL,
   12 done A64ICODE:CBNZ,
   bare A64ICODE:LBL,
   WLFIND:LENTRY A64ICODE:LABEL@ A64ICODE:BL,
   done A64ICODE:LBL,
   30 A64ICODE:SP 0 A64ICODE:LDR,  0 A64ICODE:SP 8 A64ICODE:LDR,  1 A64ICODE:SP 16 A64ICODE:LDR,  9 A64ICODE:SP 24 A64ICODE:LDR,
   10 A64ICODE:SP 32 A64ICODE:LDR,  13 A64ICODE:SP 40 A64ICODE:LDR,  17 A64ICODE:SP 48 A64ICODE:LDR,
   A64ICODE:SP A64ICODE:SP 64 A64ICODE:ADDI,  A64ICODE:RET, ;

\ LFIND's register ABI. In: x9 = name address, x10 = name length. Out:
\ x13 = 0 when the name is not in scope, else 1 | the folded record flags
\ (bit 1 immediate, bit 3 wide-effect, bit 4 internal); on a hit x11 = the record's code cell [0], x12 = its length cell [8],
\ and x5 = the matched record, zero on a miss. It clobbers x2-x17 and
\ preserves XDS (x19) and XREG-RBASE (x20).
\ x5 is the only way to read another cell of the record the name resolved to
\ (the sealed-WID gate reads [40]): an xt does not name a record, because
\ `EXPORT` publishes one body under several records and wids. The linear scan
\ takes the LAST match, so it parks it in x17 and FIND-FOUND moves it to x5.
: EMIT-FIND ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
   {: open-pub:label enclosing-pri:label enclosing-pub:label global:label primitives:label :}
   LFIND A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL FIND-QSCAN !
   A64ICODE:LBL FIND-QNONE !
   A64ICODE:LBL FIND-QHAS !
   A64ICODE:LBL FIND-QBAD !
   A64ICODE:LBL FIND-QTAIL !
   A64ICODE:LBL FIND-QTAILOK !
   A64ICODE:LBL FIND-NEND !
   A64ICODE:LBL FIND-START !
   A64ICODE:LBL FIND-LOOP !
   A64ICODE:LBL FIND-DONE !
   A64ICODE:LBL FIND-NEXT !
   A64ICODE:LBL FIND-CMP !
   A64ICODE:LBL FIND-MATCH !
   A64ICODE:LBL FIND-INL !
   A64ICODE:LBL FIND-MISS !
   A64ICODE:LBL FIND-FOUND !
   A64ICODE:LBL FIND-LINEAR !
   A64ICODE:LBL FIND-HLOOP !
   A64ICODE:LBL FIND-HNEXT !
   A64ICODE:LBL FIND-HINL !
   A64ICODE:LBL FIND-HCMP !
   A64ICODE:LBL FIND-HMATCH !
   13 0 A64ICODE:MOVZ,
   17 0 A64ICODE:MOVZ,
   FIND-QSCAN A64ICODE:LABEL@ A64ICODE:LBL,
      17 10 A64ICODE:CMP,  A64ASM:C-GE FIND-QNONE A64ICODE:LABEL@ A64ICODE:BCOND,
      14 9 17 A64ICODE:ADD,  14 14 0 A64ICODE:LDRB,  14 $3A A64ICODE:CMPI,  A64ASM:C-EQ FIND-QHAS A64ICODE:LABEL@ A64ICODE:BCOND,
      17 17 1 A64ICODE:ADDI,  FIND-QSCAN A64ICODE:LABEL@ A64ICODE:B,
   FIND-QNONE A64ICODE:LABEL@ A64ICODE:LBL,
      11 0 A64ICODE:MOVZ,                                               \ a bare name
      2 XREG-RBASE PKG-PRI-CELL A64ICODE:LDR,  FIND-START A64ICODE:LABEL@ A64ICODE:B,
   FIND-QHAS A64ICODE:LABEL@ A64ICODE:LBL,
      17 0 A64ICODE:CMPI,  A64ASM:C-EQ FIND-QNONE A64ICODE:LABEL@ A64ICODE:BCOND,
      14 17 1 A64ICODE:ADDI,  14 10 A64ICODE:CMP,  A64ASM:C-GE FIND-QNONE A64ICODE:LABEL@ A64ICODE:BCOND,
      14 17 1 A64ICODE:ADDI,
   FIND-QTAIL A64ICODE:LABEL@ A64ICODE:LBL,
      14 10 A64ICODE:CMP,  A64ASM:C-GE FIND-QTAILOK A64ICODE:LABEL@ A64ICODE:BCOND,
      15 9 14 A64ICODE:ADD,  15 15 0 A64ICODE:LDRB,  15 $3A A64ICODE:CMPI,  A64ASM:C-EQ FIND-QBAD A64ICODE:LABEL@ A64ICODE:BCOND,
      14 14 1 A64ICODE:ADDI,  FIND-QTAIL A64ICODE:LABEL@ A64ICODE:B,
   FIND-QTAILOK A64ICODE:LABEL@ A64ICODE:LBL,
      A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,
      LHEADROW A64ICODE:LABEL@ A64ICODE:BL,
      30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI,
      12 FIND-NEND A64ICODE:LABEL@ A64ICODE:CBZ,
      5 12 0 A64ICODE:ADDI,
      2 5 0 A64ICODE:LDR,  11 1 A64ICODE:MOVZ,                                  \ a qualified name
      9 9 17 A64ICODE:ADD,  9 9 1 A64ICODE:ADDI,
      10 10 17 A64ICODE:SUB,  10 10 1 A64ICODE:SUBI,
      FIND-START A64ICODE:LABEL@ A64ICODE:B,
   FIND-NEND A64ICODE:LABEL@ A64ICODE:LBL,  5 0 A64ICODE:MOVZ,  A64ICODE:RET,       \ no such wordlist: no record, and x5 says so
   FIND-QBAD A64ICODE:LABEL@ A64ICODE:LBL,  5 0 A64ICODE:MOVZ,  A64ICODE:RET,       \ malformed NAME:a:b - the one exit that never touched x5
   FIND-START A64ICODE:LABEL@ A64ICODE:LBL,
      \ The hash probe answers as WLFIND's does and keeps x2 (wid), x9/x10
      \ (name) and x13 (result) for the linear scan.
      14 XREG-RBASE BOOT:GLOBAL-WL-CELL A64ICODE:LDR,  2 0 A64ICODE:CMPI,  2 14 2 A64ASM:C-EQ A64ICODE:CSEL,
      14 XREG-RBASE HIDXP-CELL A64ICODE:LDR,  14 FIND-LINEAR A64ICODE:LABEL@ A64ICODE:CBZ,
      9 10 15 4 16 7 C-HIDX-HASH
      6 15 2 5 C-HIDX-SLOT,
      8 HIDX-SLOTS A64ICODE:LIT64,
   FIND-HLOOP A64ICODE:LABEL@ A64ICODE:LBL,
      17 6 2 A64ICODE:LSLI,  17 14 17 A64ICODE:ADD,  3 17 0 A64ICODE:LDRW,
      3 FIND-DONE A64ICODE:LABEL@ A64ICODE:CBZ,                                 \ empty slot -> absent from this wordlist
      4 3 1 A64ICODE:SUBI,  4 NDICT A64ICODE:CMP,  A64ASM:C-GE FIND-HNEXT A64ICODE:LABEL@ A64ICODE:BCOND, \ stale (truncated) index
      5 DREC A64ICODE:MOVZ,  5 4 5 A64ICODE:MUL,  5 DBASE 5 A64ICODE:ADD,
      16 5 40 A64ICODE:LDR,  16 2 A64ICODE:CMP,  A64ASM:C-NE FIND-HNEXT A64ICODE:LABEL@ A64ICODE:BCOND,  \ wid mismatch (retired=-2 / other wl)
      16 5 16 A64ICODE:LDR,  16 16 DNAME-FLAG-BITS A64ICODE:LSLI,  16 16 DNAME-FLAG-BITS A64ICODE:LSRI,  16 10 A64ICODE:CMP,  A64ASM:C-NE FIND-HNEXT A64ICODE:LABEL@ A64ICODE:BCOND,
      16 5 24 A64ICODE:ADDI,
      3 5 16 A64ICODE:LDR,  3 3 DNAME-EXT A64ICODE:ANDI,  3 FIND-HINL A64ICODE:LABEL@ A64ICODE:CBZ,
         16 5 24 A64ICODE:LDR,
      FIND-HINL A64ICODE:LABEL@ A64ICODE:LBL,
      7 0 A64ICODE:MOVZ,
      FIND-HCMP A64ICODE:LABEL@ A64ICODE:LBL,
         7 10 A64ICODE:CMP,  A64ASM:C-GE FIND-HMATCH A64ICODE:LABEL@ A64ICODE:BCOND,
         15 16 7 A64ICODE:ADD,  15 15 0 A64ICODE:LDRB,
         3 15 $41 A64ICODE:SUBI,  3 $1A A64ICODE:CMPI,  3 A64ASM:C-CC A64ICODE:CSET,  3 3 5 A64ICODE:LSLI,  15 15 3 A64ICODE:ORR,
         4 9 7 A64ICODE:ADD,     4 4 0 A64ICODE:LDRB,
         3 4 $41 A64ICODE:SUBI,   3 $1A A64ICODE:CMPI,  3 A64ASM:C-CC A64ICODE:CSET,  3 3 5 A64ICODE:LSLI,  4 4 3 A64ICODE:ORR,
         15 4 A64ICODE:CMP,  A64ASM:C-NE FIND-HNEXT A64ICODE:LABEL@ A64ICODE:BCOND,
         7 7 1 A64ICODE:ADDI,  FIND-HCMP A64ICODE:LABEL@ A64ICODE:B,
      FIND-HMATCH A64ICODE:LABEL@ A64ICODE:LBL,
         11 5 0 A64ICODE:LDR,  12 5 8 A64ICODE:LDR,
         14 5 16 A64ICODE:LDR,
         15 14 DNAME-WIDE A64ICODE:ANDI,  15 15 59 A64ICODE:LSRI,
         16 14 DNAME-INT A64ICODE:ANDI,  16 16 59 A64ICODE:LSRI,
         15 15 16 A64ICODE:ORR,
         14 14 DNAME-IMM A64ICODE:ANDI,  14 14 59 A64ICODE:LSRI,
         14 14 15 A64ICODE:ORR,
         13 1 A64ICODE:MOVZ,  13 13 14 A64ICODE:ORR,  A64ICODE:RET,
      FIND-HNEXT A64ICODE:LABEL@ A64ICODE:LBL,
         8 8 1 A64ICODE:SUBI,  8 FIND-LINEAR A64ICODE:LABEL@ A64ICODE:CBZ,
         6 6 1 A64ICODE:ADDI,  5 HIDX-SLOTS 1 - A64ICODE:LIT64,  6 6 5 A64ICODE:AND,  FIND-HLOOP A64ICODE:LABEL@ A64ICODE:B,
   FIND-LINEAR A64ICODE:LABEL@ A64ICODE:LBL,
      5 DBASE 0 A64ICODE:ADDI,  6 NDICT 0 A64ICODE:ADDI,
   FIND-LOOP A64ICODE:LABEL@ A64ICODE:LBL,
      6 FIND-DONE A64ICODE:LABEL@ A64ICODE:CBZ,
      14 5 40 A64ICODE:LDR,  14 2 A64ICODE:CMP,  A64ASM:C-NE FIND-NEXT A64ICODE:LABEL@ A64ICODE:BCOND,
      14 5 16 A64ICODE:LDR,  14 14 DNAME-FLAG-BITS A64ICODE:LSLI,  14 14 DNAME-FLAG-BITS A64ICODE:LSRI,  14 10 A64ICODE:CMP,  A64ASM:C-NE FIND-NEXT A64ICODE:LABEL@ A64ICODE:BCOND,
      16 5 24 A64ICODE:ADDI,
      14 5 16 A64ICODE:LDR,  14 14 DNAME-EXT A64ICODE:ANDI,  14 FIND-INL A64ICODE:LABEL@ A64ICODE:CBZ,
         16 5 24 A64ICODE:LDR,
      FIND-INL A64ICODE:LABEL@ A64ICODE:LBL,
      7 0 A64ICODE:MOVZ,
      FIND-CMP A64ICODE:LABEL@ A64ICODE:LBL,
         7 10 A64ICODE:CMP,  A64ASM:C-GE FIND-MATCH A64ICODE:LABEL@ A64ICODE:BCOND,
         15 16 7 A64ICODE:ADD,  15 15 0 A64ICODE:LDRB,
         3 15 $41 A64ICODE:SUBI,  3 $1A A64ICODE:CMPI,  3 A64ASM:C-CC A64ICODE:CSET,  3 3 5 A64ICODE:LSLI,  15 15 3 A64ICODE:ORR,
         4 9 7 A64ICODE:ADD,     4 4 0 A64ICODE:LDRB,
         3 4 $41 A64ICODE:SUBI,   3 $1A A64ICODE:CMPI,  3 A64ASM:C-CC A64ICODE:CSET,  3 3 5 A64ICODE:LSLI,  4 4 3 A64ICODE:ORR,
         15 4 A64ICODE:CMP,  A64ASM:C-NE FIND-NEXT A64ICODE:LABEL@ A64ICODE:BCOND,
         7 7 1 A64ICODE:ADDI,  FIND-CMP A64ICODE:LABEL@ A64ICODE:B,
      FIND-MATCH A64ICODE:LABEL@ A64ICODE:LBL,
         11 5 0 A64ICODE:LDR,  12 5 8 A64ICODE:LDR,
         14 5 16 A64ICODE:LDR,
         15 14 DNAME-WIDE A64ICODE:ANDI,  15 15 59 A64ICODE:LSRI,
         16 14 DNAME-INT A64ICODE:ANDI,  16 16 59 A64ICODE:LSRI,
         15 15 16 A64ICODE:ORR,
         14 14 DNAME-IMM A64ICODE:ANDI,  14 14 59 A64ICODE:LSRI,
         14 14 15 A64ICODE:ORR,
         13 1 A64ICODE:MOVZ,  13 13 14 A64ICODE:ORR,
         17 5 0 A64ICODE:ADDI,                                         \ park the match: x5 is this loop's cursor, x17 is not touched by it
         FIND-NEXT A64ICODE:LABEL@ A64ICODE:B,
      FIND-NEXT A64ICODE:LABEL@ A64ICODE:LBL,  5 5 DREC A64ICODE:ADDI,  6 6 1 A64ICODE:SUBI,  FIND-LOOP A64ICODE:LABEL@ A64ICODE:B,
   FIND-DONE A64ICODE:LABEL@ A64ICODE:LBL,
      \ The global wordlist and then WL:PRIMITIVES are searched last: a miss in
      \ WL:PRIMITIVES ends the search; a qualified name (x11 = 1) skips the
      \ enclosing package.
      13 FIND-FOUND A64ICODE:LABEL@ A64ICODE:CBNZ,
      2 WL:PRIMITIVES A64ICODE:CMPI,  A64ASM:C-EQ FIND-MISS A64ICODE:LABEL@ A64ICODE:BCOND,
      14 XREG-RBASE BOOT:GLOBAL-WL-CELL A64ICODE:LDR,  2 14 A64ICODE:CMP,  A64ASM:C-EQ primitives A64ICODE:BCOND,
      14 XREG-RBASE PKG-PRI-CELL A64ICODE:LDR,  14 2 A64ICODE:CMP,  A64ASM:C-NE open-pub A64ICODE:BCOND,
         2 XREG-RBASE PKG-PUB-CELL A64ICODE:LDR,  FIND-START A64ICODE:LABEL@ A64ICODE:B,
      open-pub A64ICODE:LBL,
      14 XREG-RBASE PKG-PUB-CELL A64ICODE:LDR,  14 2 A64ICODE:CMP,  A64ASM:C-NE enclosing-pri A64ICODE:BCOND,
         11 global A64ICODE:CBNZ,
         2 XREG-RBASE ENCLOSING:PRI A64ICODE:LDR,  FIND-START A64ICODE:LABEL@ A64ICODE:B,
      enclosing-pri A64ICODE:LBL,
      11 FIND-MISS A64ICODE:LABEL@ A64ICODE:CBNZ,
      14 XREG-RBASE ENCLOSING:PRI A64ICODE:LDR,  14 2 A64ICODE:CMP,  A64ASM:C-NE enclosing-pub A64ICODE:BCOND,
         2 XREG-RBASE ENCLOSING:PUB A64ICODE:LDR,  FIND-START A64ICODE:LABEL@ A64ICODE:B,
      enclosing-pub A64ICODE:LBL,
      14 XREG-RBASE ENCLOSING:PUB A64ICODE:LDR,  14 2 A64ICODE:CMP,  A64ASM:C-NE FIND-MISS A64ICODE:LABEL@ A64ICODE:BCOND,
      global A64ICODE:LBL,
         2 0 A64ICODE:MOVZ,  FIND-START A64ICODE:LABEL@ A64ICODE:B,
      primitives A64ICODE:LBL,
         2 WL:PRIMITIVES A64ICODE:MOVZ,  FIND-START A64ICODE:LABEL@ A64ICODE:B,
      \ Only the LINEAR scan can arrive here with x13 set - the hash probe's own
      \ match returns from FIND-HMATCH - so the parked pointer is this call's.
      FIND-FOUND A64ICODE:LABEL@ A64ICODE:LBL,  5 17 0 A64ICODE:ADDI,  A64ICODE:RET,
      FIND-MISS A64ICODE:LABEL@ A64ICODE:LBL,   5 0 A64ICODE:MOVZ,  A64ICODE:RET, ;

: C-NUM-INIT-REGS ( -- )
   11 0 A64ICODE:MOVZ,  13 1 A64ICODE:MOVZ,  14 0 A64ICODE:MOVZ,  12 0 A64ICODE:MOVZ,  6 10 A64ICODE:MOVZ,
   16 0 A64ICODE:MOVZ,  17 0 A64ICODE:MOVZ, ;                                  \ overflow latch, final literal rejection

: C-NUM-SIGN ( -- )
   10 NUM-DONE A64ICODE:LABEL@ A64ICODE:CBZ,
   15 9 0 A64ICODE:LDRB,  15 45 A64ICODE:CMPI,  A64ASM:C-NE NUM-NDOLL A64ICODE:LABEL@ A64ICODE:BCOND,
      13 0 A64ICODE:MOVN,  14 1 A64ICODE:MOVZ,
   NUM-NDOLL A64ICODE:LABEL@ A64ICODE:LBL,
   14 10 A64ICODE:CMP,  A64ASM:C-GE NUM-DONE A64ICODE:LABEL@ A64ICODE:BCOND, ;

: C-NUM-BASE ( -- )
   5 9 14 A64ICODE:ADD,  15 5 0 A64ICODE:LDRB,  15 36 A64ICODE:CMPI,  A64ASM:C-NE NUM-NOHEX A64ICODE:LABEL@ A64ICODE:BCOND,
      6 16 A64ICODE:MOVZ,  14 14 1 A64ICODE:ADDI,
   NUM-NOHEX A64ICODE:LABEL@ A64ICODE:LBL,
   2 0 A64ICODE:MOVZ,
   14 10 A64ICODE:CMP,  A64ASM:C-GE NUM-DONE A64ICODE:LABEL@ A64ICODE:BCOND, ;

: C-NUM-DOT ( -- )
   15 46 A64ICODE:CMPI,  A64ASM:C-NE NUM-NDOT A64ICODE:LABEL@ A64ICODE:BCOND,                      \ '.' -> frac mode
      6 10 A64ICODE:CMPI,  A64ASM:C-NE NUM-DONE A64ICODE:LABEL@ A64ICODE:BCOND,                    \ only base 10
      2 NUM-DONE A64ICODE:LABEL@ A64ICODE:CBNZ,                                    \ second dot -> fail
      2 1 A64ICODE:MOVZ,  4 0 A64ICODE:MOVZ,  3 1 A64ICODE:MOVZ,                           \ frac=0 scale=1
      14 14 1 A64ICODE:ADDI,  NUM-LOOP A64ICODE:LABEL@ A64ICODE:B,
   NUM-NDOT A64ICODE:LABEL@ A64ICODE:LBL, ;

: C-NUM-DIGIT ( -- )
   15 48 A64ICODE:CMPI,  A64ASM:C-LT NUM-DONE A64ICODE:LABEL@ A64ICODE:BCOND,
   15 57 A64ICODE:CMPI,  A64ASM:C-GT NUM-ND A64ICODE:LABEL@ A64ICODE:BCOND,
      7 15 48 A64ICODE:SUBI,  NUM-GOTD A64ICODE:LABEL@ A64ICODE:B,
   NUM-ND A64ICODE:LABEL@ A64ICODE:LBL,
   6 16 A64ICODE:CMPI,  A64ASM:C-NE NUM-DONE A64ICODE:LABEL@ A64ICODE:BCOND,
   15 97 A64ICODE:CMPI,  A64ASM:C-LT NUM-NUC A64ICODE:LABEL@ A64ICODE:BCOND,  15 102 A64ICODE:CMPI,  A64ASM:C-GT NUM-DONE A64ICODE:LABEL@ A64ICODE:BCOND,
      7 15 87 A64ICODE:SUBI,  NUM-GOTD A64ICODE:LABEL@ A64ICODE:B,
   NUM-NUC A64ICODE:LABEL@ A64ICODE:LBL,
   15 65 A64ICODE:CMPI,  A64ASM:C-LT NUM-DONE A64ICODE:LABEL@ A64ICODE:BCOND,  15 70 A64ICODE:CMPI,  A64ASM:C-GT NUM-DONE A64ICODE:LABEL@ A64ICODE:BCOND,
      7 15 55 A64ICODE:SUBI, ;

: C-NUM-INT-STEP ( -- )
   5 0 A64ICODE:MOVN,  5 5 7 A64ICODE:SUB,                                    \ unsigned cell limit minus this digit
   8 5 6 A64ICODE:UDIV,  11 8 A64ICODE:CMP,  5 A64ASM:C-HI A64ICODE:CSET,  16 16 5 A64ICODE:ORR,
   11 11 6 A64ICODE:MUL,  11 11 7 A64ICODE:ADD,
   14 14 1 A64ICODE:ADDI,  NUM-LOOP A64ICODE:LABEL@ A64ICODE:B, ;

: C-NUM-FRAC-STEP ( -- )
   5 $7FFFFFFFFFFFFFFF A64ICODE:LIT64,  5 5 7 A64ICODE:SUB,
   8 10 A64ICODE:MOVZ,  5 5 8 A64ICODE:UDIV,  4 5 A64ICODE:CMP,  5 A64ASM:C-GT A64ICODE:CSET,  16 16 5 A64ICODE:ORR,
   5 $0CCCCCCCCCCCCCCC A64ICODE:LIT64,  3 5 A64ICODE:CMP,  5 A64ASM:C-GT A64ICODE:CSET,  16 16 5 A64ICODE:ORR,
   16 NUM-FRAC-NEXT A64ICODE:LABEL@ A64ICODE:CBNZ,
   5 10 A64ICODE:MOVZ,  4 4 5 A64ICODE:MUL,  4 4 7 A64ICODE:ADD,  3 3 5 A64ICODE:MUL,
   NUM-FRAC-NEXT A64ICODE:LABEL@ A64ICODE:LBL,
   14 14 1 A64ICODE:ADDI,  NUM-LOOP A64ICODE:LABEL@ A64ICODE:B, ;

: C-NUM-FLOAT-FINISH ( -- )
   \ x15 is the last consumed character; overflow can leave the scale at one.
   15 46 A64ICODE:CMPI,  A64ASM:C-EQ NUM-DONE A64ICODE:LABEL@ A64ICODE:BCOND,                     \ "1." (no frac digits) -> ordinary word
   5 $7FFFFFFFFFFFFFFF A64ICODE:LIT64,  11 5 A64ICODE:CMP,  5 A64ASM:C-HI A64ICODE:CSET,
   17 16 5 A64ICODE:ORR,  17 NUM-DONE A64ICODE:LABEL@ A64ICODE:CBNZ,                      \ a complete decimal owns its range rejection
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
   {: left:label right:label shift:label ready:label round:label pack:label sign:label :}
   \ x11 significand, x4 exact remainder, x3 scale; x6 exponent,
   \ x7 guard and x8 sticky. The remainder is below 10^18 throughout.
   5 11 4 A64ICODE:ORR,  5 sign A64ICODE:CBZ,                                    \ preserve signed zero
   6 52 A64ICODE:MOVZ,
   5 $20000000000000 A64ICODE:LIT64,  11 5 A64ICODE:CMP,  A64ASM:C-LT left A64ICODE:BCOND,
   4 0 A64ICODE:CMPI,  8 A64ASM:C-NE A64ICODE:CSET,  7 0 A64ICODE:MOVZ,
   right A64ICODE:LBL,
      8 8 7 A64ICODE:ORR,  7 11 1 A64ICODE:ANDI,  11 11 1 A64ICODE:LSRI,  6 6 1 A64ICODE:ADDI,
      11 5 A64ICODE:CMP,  A64ASM:C-GE right A64ICODE:BCOND,
   round A64ICODE:B,
   left A64ICODE:LBL,
   5 $10000000000000 A64ICODE:LIT64,
   shift A64ICODE:LBL,
      11 5 A64ICODE:CMP,  A64ASM:C-GE ready A64ICODE:BCOND,
      11 11 1 A64ICODE:LSLI,  4 4 1 A64ICODE:LSLI,
      4 3 A64ICODE:CMP,  14 A64ASM:C-GE A64ICODE:CSET,
      11 11 14 A64ICODE:ADD,  15 14 3 A64ICODE:MUL,  4 4 15 A64ICODE:SUB,
      6 6 1 A64ICODE:SUBI,  shift A64ICODE:B,
   ready A64ICODE:LBL,
   4 4 1 A64ICODE:LSLI,  4 3 A64ICODE:CMP,  7 A64ASM:C-GE A64ICODE:CSET,
   14 7 3 A64ICODE:MUL,  4 4 14 A64ICODE:SUB,  4 0 A64ICODE:CMPI,  8 A64ASM:C-NE A64ICODE:CSET,
   round A64ICODE:LBL,
   7 pack A64ICODE:CBZ,
   14 11 1 A64ICODE:ANDI,  14 14 8 A64ICODE:ORR,  14 pack A64ICODE:CBZ,                  \ ties retain an even low bit
   11 11 1 A64ICODE:ADDI,
   5 $20000000000000 A64ICODE:LIT64,  11 5 A64ICODE:CMP,  A64ASM:C-NE pack A64ICODE:BCOND,
      11 11 1 A64ICODE:LSRI,  6 6 1 A64ICODE:ADDI,                              \ rounding carried out of 53 bits
   pack A64ICODE:LBL,
   6 6 1023 A64ICODE:ADDI,  6 6 52 A64ICODE:LSLI,
   11 11 $FFFFFFFFFFFFF A64ICODE:ANDI,  11 11 6 A64ICODE:ORR,
   sign A64ICODE:LBL,
   13 13 63 A64ICODE:LSRI,  13 13 63 A64ICODE:LSLI,  11 11 13 A64ICODE:ORR,
   12 1 A64ICODE:MOVZ,  A64ICODE:RET, ;

: C-NUM-INT-FINISH ( -- )
   A64ICODE:LBL {: bounded:label :}
   17 16 0 A64ICODE:ADDI,
   6 10 A64ICODE:CMPI,  A64ASM:C-NE bounded A64ICODE:BCOND,                             \ hex preserves all 64 bits, with modular negation
   5 $7FFFFFFFFFFFFFFF A64ICODE:LIT64,  8 13 63 A64ICODE:LSRI,  5 5 8 A64ICODE:ADD,       \ decimal magnitude: INT64_MAX, or 2^63 when negative
   11 5 A64ICODE:CMP,  5 A64ASM:C-HI A64ICODE:CSET,  17 17 5 A64ICODE:ORR,
   bounded A64ICODE:LBL,
   17 NUM-DONE A64ICODE:LABEL@ A64ICODE:CBNZ,                                   \ a complete overflowed integer never becomes a name
   11 11 13 A64ICODE:MUL,  12 1 A64ICODE:MOVZ, ;

\ The engine's number reader ( x9=address, x10=length -> x11 value, x2 float?,
\ x12 read a number? ). It is registered as the sealed (NUM) helper because
\ `num-parse` (BNUMPARSE) reaches it by a direct BL.
: EMIT-NUM ( -- )
   A64ICODE:LBL {: end:label :}
   s" (NUM)" LNUM A64ICODE:LABEL@ LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   LNUM A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL NUM-DONE !
   A64ICODE:LBL NUM-NDOLL !
   A64ICODE:LBL NUM-NOHEX !
   A64ICODE:LBL NUM-LOOP !
   A64ICODE:LBL NUM-OK !
   A64ICODE:LBL NUM-GOTD !
   A64ICODE:LBL NUM-ND !
   A64ICODE:LBL NUM-NUC !
   A64ICODE:LBL NUM-NDOT !
   A64ICODE:LBL NUM-ISFRAC !
   A64ICODE:LBL NUM-FRAC-NEXT !
   A64ICODE:LBL NUM-LINT !
   C-NUM-INIT-REGS
   C-NUM-SIGN
   C-NUM-BASE
   NUM-LOOP A64ICODE:LABEL@ A64ICODE:LBL,
   14 10 A64ICODE:CMP,  A64ASM:C-GE NUM-OK A64ICODE:LABEL@ A64ICODE:BCOND,
   5 9 14 A64ICODE:ADD,  15 5 0 A64ICODE:LDRB,
   C-NUM-DOT
   C-NUM-DIGIT
   NUM-GOTD A64ICODE:LABEL@ A64ICODE:LBL,
   2 NUM-ISFRAC A64ICODE:LABEL@ A64ICODE:CBNZ,
   C-NUM-INT-STEP
   NUM-ISFRAC A64ICODE:LABEL@ A64ICODE:LBL,
   C-NUM-FRAC-STEP
   NUM-OK A64ICODE:LABEL@ A64ICODE:LBL,
   2 NUM-LINT A64ICODE:LABEL@ A64ICODE:CBZ,
   C-NUM-FLOAT-FINISH
   NUM-LINT A64ICODE:LABEL@ A64ICODE:LBL,  C-NUM-INT-FINISH
   NUM-DONE A64ICODE:LABEL@ A64ICODE:LBL,  A64ICODE:RET,
   end A64ICODE:LBL, ;
;package

package ENGINE-EMIT

\ Dictionary inline-name padding is always zero, independent of registry data.
create NAME-PADDING 0 , 0 ,

: EMIT-DICT ( -- )
   ENGINE-PRIMS:DISTINCT
   0 BEGIN dup ENGINE-PRIMS:COUNT < WHILE
      dup ENGINE-PRIMS:NAME-LEN DNAME-INL > IF
         A64ICODE:LBL over ENGINE-PRIMS:NAME-LABEL!
         dup ENGINE-PRIMS:NAME-LABEL A64ICODE:LBL,
         dup ENGINE-PRIMS:NAME$ A64ICODE:BYTES,
      ELSE
         -1 >LABEL over ENGINE-PRIMS:NAME-LABEL!
      THEN
      1 + REPEAT drop
   LNCOUNT A64ICODE:LABEL@ A64ICODE:LBL,  ENGINE-PRIMS:COUNT A64ICODE:DCQ,
   LDICT A64ICODE:LABEL@ A64ICODE:LBL,
   0 BEGIN dup ENGINE-PRIMS:COUNT < WHILE
      dup ENGINE-PRIMS:FIRST-LABEL A64ICODE:DLBL,
      dup ENGINE-PRIMS:LAST-LABEL A64ICODE:DLBL,
      dup ENGINE-PRIMS:NAME-LEN DNAME-INL > IF
         dup ENGINE-PRIMS:DNAME DNAME-EXT or A64ICODE:DCQ,
         dup ENGINE-PRIMS:NAME-LABEL A64ICODE:DLBL,
         0 A64ICODE:DCQ,
      ELSE
         dup ENGINE-PRIMS:DNAME A64ICODE:DCQ,
         dup ENGINE-PRIMS:NAME$ A64ICODE:BYTES,
         16 over ENGINE-PRIMS:NAME-LEN 3 + -4 and - dup 0 > IF NAME-PADDING swap >str A64ICODE:BYTES, ELSE drop THEN
      THEN
      dup ENGINE-PRIMS:HELPER-WID A64ICODE:DCQ,
      1 + REPEAT drop ;

;package
