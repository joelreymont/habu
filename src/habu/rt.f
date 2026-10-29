\ rt.f - native runtime routines for the ARM64 engine builder: data-stack ops,
\ guarded stack mapping and the output and printer sequences. Encoders are
\ package A64ASM (src/arch/arm64/asm.f).
require src/habu/stack-abi.f
require src/arch/arm64/icode.f
using A64ASM
using A64ICODE

package RT

72 constant EXIT-RC

\ The data-stack register is stated twice: mnem.f XDS, build-side vocabulary the
\ booted engine never re-reads, and layout.f ENGINE-GPR:DSTACK, re-read at every
\ load, from which the reserved-register mask derives. Both build chains
\ (hb-build and Gforth recovery) load mnem.f, layout.f, then this file, the
\ first consumer of XDS, so a mismatch dies here instead of shipping an engine
\ whose allocator can be handed the stack register.
: DSTACK-AGREE ( -- )
   XDS ENGINE-GPR:DSTACK <>
   if s" rt: mnem.f XDS and layout.f ENGINE-GPR:DSTACK disagree" EXIT-RC die then ;
DSTACK-AGREE

\ The guard granule is stated twice for the same reason: stack-abi.f is loaded
\ ahead of layout.f by every window, so layout.f cannot name it, and layout.f's
\ PROT-PAGE-MAX is what the region mapping and the image writers round to.
: PAGE-AGREE ( -- )
   STACK-ABI:PAGE-BYTES PROT-PAGE-MAX <>
   if s" rt: stack-abi.f PAGE-BYTES and layout.f PROT-PAGE-MAX disagree" EXIT-RC die then ;
PAGE-AGREE

;package

package STACK-GUARD

variable FAIL-MESSAGE
variable DESCRIPTOR-FAIL
variable DESCRIPTOR-BASE
variable DESCRIPTOR-CAP
variable CURSOR-ABOVE
variable MAP-CAP  variable MAP-DST
variable MAP-MSG  variable MAP-OK   variable MAP-ROK
variable MAP-BAD  variable MAP-DONE

: EMIT-FAIL ( -- )
   S\" hb: stack bounds exceeded\n" {: ma:ptr mu:n :}
   LBL FAIL-MESSAGE !
   0 2 MOVZ,  1 FAIL-MESSAGE LABEL@ ADR,  2 mu MOVZ,  NR-WRITE SYS,
   0 ENGINE-ERROR:STACK-BOUNDS MOVZ,  NR-EXIT-GROUP SYS,
   FAIL-MESSAGE LABEL@ LBL,  ma mu BYTES, ;

\ The descriptor itself must not wrap, even
\ when it came from a saved frame or a task rather than run-in-stack.
: ALIGNED ( n label -- )
   DESCRIPTOR-FAIL !
   31 swap 7 >LIMM ENC-ANDI $60000000 or EMITW    \ tst register,#7
   C-NE DESCRIPTOR-FAIL LABEL@ BCOND, ;

: DESCRIPTOR ( n n label -- )
   DESCRIPTOR-FAIL !  DESCRIPTOR-CAP !  DESCRIPTOR-BASE !
   DESCRIPTOR-BASE @ DESCRIPTOR-FAIL LABEL@ CBZ,
   DESCRIPTOR-BASE @ DESCRIPTOR-FAIL LABEL@ ALIGNED
   DESCRIPTOR-BASE @ dup ENC-MVN EMITW
   DESCRIPTOR-CAP @ DESCRIPTOR-BASE @ CMP,  C-HI DESCRIPTOR-FAIL LABEL@ BCOND,
   DESCRIPTOR-BASE @ dup ENC-MVN EMITW ;

public

78 constant MAP-FAIL-RC     \ the rc the two fixed-region mappings already use

\ Lifecycle admission at a stack switch (run-in-stack, catch, evaluate, task
\ and REPL recovery): x14=base, x10=capacity, x12=cursor. Leaves base intact;
\ x10 becomes remaining bytes and x12 becomes used bytes. No memory is changed.
\ This is the only bounds check the engine performs on a data stack: inside
\ compiled code the checker proves the stack effect, and a push past the
\ capacity faults on the guard page beyond the allocation.
: CHECK-CURSOR ( n label -- )
   DESCRIPTOR-FAIL !  CURSOR-ABOVE !
   14 10 DESCRIPTOR-FAIL LABEL@ DESCRIPTOR
   12 DESCRIPTOR-FAIL LABEL@ ALIGNED
   12 14 CMP,  C-CC DESCRIPTOR-FAIL LABEL@ BCOND,
   12 12 14 SUB,
   12 10 CMP,  C-HI DESCRIPTOR-FAIL LABEL@ BCOND,
   10 10 12 SUB,
   10 CURSOR-ABOVE @ CMPI,  C-CC DESCRIPTOR-FAIL LABEL@ BCOND, ;

: EXIT-BOUNDS ( -- ) EMIT-FAIL ;

\ EMIT-MAP ( cap dst -- ) maps one guarded VM stack of cap bytes and leaves its
\ base in register dst. It writes x0-x6, x9 and the syscall registers, never x10
\ or x11, which habu1.f B-EVAL-CLOSED keeps live across it.
\
\ A stack occupies [base - PAGE, base + cap + PAGE) with both outer pages
\ inaccessible: a push past the capacity or a read below the base takes SIGSEGV,
\ and src/habu/crash.f turns the address into the STACK-BOUNDS exit or, for a
\ data-stack access below the base, the underdepth throw (crash.f ONE FAULT IS
\ NOT AN EXIT). Compiled code carries no check.
\
\ TWO mmap CALLS, NO mprotect: the first maps cap + 3*PAGE PROT_NONE, which IS
\ the guard; the second reopens the middle read/write with MAP_FIXED. The third
\ page is alignment slack: the kernel returns its own granule (16 KiB on this
\ host) but the base must be PAGE-BYTES aligned, the granule run-in-stack
\ proves an extent against. Everything below the low guard stays PROT_NONE, so
\ the slack only widens the guard. A stack mapped this way is never released.
\
\ It runs before any crash handler exists and, for the boot data stack, before
\ any region does: the diagnostic is inline bytes reached by ADR and the exit is
\ 78, as for the two fixed-region mappings. The engine (habu2.f
\ EM-RUNTIME-STACK/EM-FRAME-STACKS) and the stripped AOT entry (aot-lib.f
\ EMIT-ENTRY) both emit it.
: EMIT-MAP ( n n -- )
   MAP-DST !  MAP-CAP !
   S\" hb: cannot map guarded VM stack\n" {: ma:ptr mu:n :}
   LBL MAP-MSG !  LBL MAP-OK !  LBL MAP-ROK !  LBL MAP-BAD !  LBL MAP-DONE !
   0 0 MOVZ,
   1 MAP-CAP @ STACK-ABI:PAGE-BYTES 3 * + LIT64,
   2 0 MOVZ,                                        \ PROT_NONE: the whole span is guard
   3 MAP-ANON-PRIVATE LIT64,
   4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   6 STACK-ABI:PAGE-BYTES LIT64,  0 6 CMP,
   C-GE MAP-OK LABEL@ BCOND,                        \ a -errno return is a small negative
   MAP-BAD LABEL@ B,
   MAP-OK LABEL@ LBL,
   9 0 0 ADDI,
   6 STACK-ABI:PAGE-BYTES 2 * 1 - LIT64,  9 9 6 ADD,
   6 STACK-ABI:PAGE-BYTES negate LIT64,   9 9 6 AND,   \ x9 = base, one guard page in
   0 9 0 ADDI,
   1 MAP-CAP @ LIT64,
   2 3 MOVZ,                                        \ PROT_READ|PROT_WRITE
   3 MAP-ANON-PRIVATE-FIXED LIT64,
   4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   0 9 CMP,
   C-EQ MAP-ROK LABEL@ BCOND,
   MAP-BAD LABEL@ B,
   MAP-ROK LABEL@ LBL,
   MAP-DST @ 9 0 ADDI,
   MAP-DONE LABEL@ B,
   MAP-BAD LABEL@ LBL,
   0 2 MOVZ,  1 MAP-MSG LABEL@ ADR,  2 mu MOVZ,  NR-WRITE SYS,
   0 MAP-FAIL-RC MOVZ,  NR-EXIT-GROUP SYS,
   MAP-MSG LABEL@ LBL,  ma mu BYTES,
   MAP-DONE LABEL@ LBL, ;

;package

\ XDS points just past the top cell (full-ascending): a push stores and advances
\ (post-index), a pop retreats and loads (pre-index), one instruction each.
\ Every baked primitive is built from these.
: G-PUSH ( n -- )
   XDS 8 STRPOST, ;

: G-POP ( n -- )
   XDS -8 LDRPRE, ;
variable DOT-LBL  variable ATOI-LBL
variable RT-LPOS  variable RT-LLOOP  variable RT-LDONE

\ The engine's one output funnel (src/lib/genio-core.f).
\ Every byte of ORDINARY program text the engine writes leaves through G-OUT:
\ `.` `u.` `f.` (G-PRINT9/G-PRINTU9, habu1.f BFDOT), `emit` `cr` `space`
\ (G-EMITC), `type` (habu1.f BTYPE), interpreted `."` and `.\"` (habu2.f
\ C-IDOTQ/C-EIDOTQ) and the REPL's ok prompt (habu2.f EM-COMPILE-EXIT).
\ DIAGNOSTICS DO NOT: crash.f, `die`, the E-* reports and the REPL's error line
\ write to fd 2 directly, so a failing engine reports on the descriptor it
\ started with however a program routed its output.
\
\ THE TERMINAL PATH PAYS ONE LOAD AND ONE COMPARE: only a non-zero device index
\ leaves for LGENIOOUT, and only that branch frames the call. x0 is the scratch
\ because every caller loads the descriptor into it next, so no site has to say
\ which of its registers are live.
\
\ CALLERS PASS x1 = span address, x2 = span length, as for the syscall.
variable LGENIOOUT
variable GO-DEV  variable GO-DONE

: G-OUT ( -- )
   LBL GO-DEV !  LBL GO-DONE !
   0 DATA GENIO-ABI:OUT-CELL LDR,  0 GO-DEV LABEL@ CBNZ,
   0 1 MOVZ,  NR-WRITE SYS,
   GO-DONE LABEL@ B,
   GO-DEV LABEL@ LBL,
   SP SP $10 SUBI,  30 SP 0 STR,
   LGENIOOUT LABEL@ BL,
   30 SP 0 LDR,  SP SP $10 ADDI,
   GO-DONE LABEL@ LBL, ;

\ Print x9 as signed decimal + newline. Clobbers x9-x14 + 32 bytes of sp
\ scratch; preserves XDS.
\
\ THE DIGIT LOOP DIVIDES UNSIGNED. A negative value is negated first; MIN-N
\ negates to itself, and UDIV reads that pattern as its magnitude (2^63). A
\ signed divide printed MIN-N as bytes below '0'.
: G-PRINT9 ( -- )
   LBL RT-LPOS !  LBL RT-LLOOP !  LBL RT-LDONE !
   SP SP $20 SUBI,  12 SP $20 ADDI,
   13 $A MOVZ,  12 12 1 SUBI,  13 12 0 STRB,
   14 0 MOVZ,  9 0 CMPI,
   C-GE RT-LPOS LABEL@ BCOND,
   14 1 MOVZ,  9 SP 9 SUB,  RT-LPOS LABEL@ LBL,
   10 $A MOVZ,
   RT-LLOOP LABEL@ LBL,
   11 9 10 UDIV,  13 11 10 MUL,  13 9 13 SUB,
   13 13 $30 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
   9 11 0 ADDI,  9 RT-LLOOP LABEL@ CBNZ,
   14 RT-LDONE LABEL@ CBZ,
   13 $2D MOVZ,  12 12 1 SUBI,  13 12 0 STRB,  RT-LDONE LABEL@ LBL,
   1 12 0 ADDI,  2 SP $20 ADDI,  2 2 12 SUB,
   G-OUT
   SP SP $20 ADDI, ;

: EMIT-DOT ( -- )
   DOT-LBL LABEL@ LBL,  9 G-POP  G-PRINT9  RET, ;

\ Print x9 as UNSIGNED decimal + newline. Clobbers x9-x13 + 32 bytes of sp
\ scratch.
: G-PRINTU9 ( -- )
   SP SP $20 SUBI,  12 SP $20 ADDI,
   13 $A MOVZ,  12 12 1 SUBI,  13 12 0 STRB,
   10 $A MOVZ,
   LBL RT-LLOOP !  RT-LLOOP LABEL@ LBL,
   11 9 10 UDIV,  13 11 10 MUL,  13 9 13 SUB,
   13 13 $30 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
   9 11 0 ADDI,  9 RT-LLOOP LABEL@ CBNZ,
   1 12 0 ADDI,  2 SP $20 ADDI,  2 2 12 SUB,
   G-OUT
   SP SP $20 ADDI, ;

\ Write the single byte in x13 to the current output device (emit/cr/space
\ share it). The byte goes to sp scratch first because a device write takes a
\ span like every other, and the frame G-OUT pushes on the device path sits
\ below this one.
: G-EMITC ( -- )
   SP SP $10 SUBI,  13 SP 0 STRB,
   1 SP 0 ADDI,  2 1 MOVZ,
   G-OUT
   SP SP $10 ADDI, ;

\ ATOI: NUL-terminated decimal string at x9 -> push i64 (leading '-' ok). Leaf.
: EMIT-ATOI ( -- )
   ATOI-LBL LABEL@ LBL,
   LBL RT-LPOS !  LBL RT-LLOOP !  LBL RT-LDONE !
   10 0 MOVZ,
   11 1 MOVZ,
   12 9 0 LDRB,  12 $2D CMPI,
   C-NE RT-LPOS LABEL@ BCOND,
   11 0 MOVN,  9 9 1 ADDI,
   RT-LPOS LABEL@ LBL,
   RT-LLOOP LABEL@ LBL,
   12 9 0 LDRB,
   12 $30 CMPI,  C-LT RT-LDONE LABEL@ BCOND,
   12 $39 CMPI,  C-GT RT-LDONE LABEL@ BCOND,
   12 12 $30 SUBI,
   13 $A MOVZ,  10 10 13 MUL,  10 10 12 ADD,
   9 9 1 ADDI,  RT-LLOOP LABEL@ B,
   RT-LDONE LABEL@ LBL,
   10 10 11 MUL,
   10 G-PUSH
   RET, ;

;using
;using
