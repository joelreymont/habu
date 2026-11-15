\ rt.f - native runtime routines for the ARM64 engine builder: data-stack ops,
\ guarded stack mapping and the output and printer sequences. Encoders are
\ package A64ASM (src/arch/arm64/asm.f).
require src/habu/stack-abi.f
require src/habu/crash.f
require src/arch/arm64/icode.f
using A64ASM
using A64ICODE
using SYSCALL
using CRASH

package STACK-GUARD

variable MAP-CAP  variable MAP-DST
variable MAP-OK   variable MAP-ROK
variable MAP-BAD  variable MAP-DONE

public

\ EMIT-MAP ( cap dst -- ) maps one guarded VM stack of cap bytes and leaves its
\ base in register dst. It writes x0-x6, x9 and the syscall registers, never x10
\ or x11, which habu1.f B-EVAL-CLOSED keeps live across it.
\
\ A stack occupies [base - PAGE, base + cap + PAGE) with both outer pages
\ inaccessible: a push past the capacity or a read below the base takes SIGSEGV,
\ and src/habu/crash.f turns the address into the STACK-BOUNDS exit or, for a
\ data-stack access below the base, the underdepth throw. Compiled code carries
\ no check.
\
\ TWO mmap CALLS, NO mprotect: the first maps cap + 3*PAGE PROT_NONE, which IS
\ the guard; the second reopens the middle read/write with MAP_FIXED. The third
\ page is alignment slack: the kernel returns its own granule (16 KiB on this
\ host) but the base must be PAGE-BYTES aligned, as src/habu/crash.f's guard
\ cases assume. Everything below the low guard stays PROT_NONE, so
\ the slack only widens the guard. A stack mapped this way is never released.
\
\ It runs before any crash handler exists and, for the boot data stack, before
\ any region does: its line's text is laid inline and the exit is 78, as for the
\ two fixed-region mappings. The engine (habu2.f
\ EM-RUNTIME-STACK/EM-FRAME-STACKS) and a product's start-up
\ (src/arch/arm64/boot.f ENTRY,) both emit it.
: EMIT-MAP ( n n -- )
   MAP-DST !  MAP-CAP !
   LBL MAP-OK !  LBL MAP-ROK !  LBL MAP-BAD !  LBL MAP-DONE !
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
   s" E-MAP-FAIL habu: cannot map guarded VM stack" STACK-ABI:MAP-FAIL-RC DIE-LINE,
   MAP-DONE LABEL@ LBL, ;

;package

package RT
public
\ XDS points just past the top cell (full-ascending): a push stores and advances
\ (post-index), a pop retreats and loads (pre-index), one instruction each.
\ Every baked primitive is built from these.
: G-PUSH ( n -- )
   XDS 8 STRPOST, ;

: G-POP ( n -- )
   XDS -8 LDRPRE, ;
private
variable RT-LPOS  variable RT-LLOOP  variable RT-LDONE

public
\ The engine's one output funnel (src/lib/genio-core.f).
\ Every byte of ORDINARY program text the engine writes leaves through G-OUT:
\ `.` `u.` `f.` (G-PRINT9/G-PRINTU9, habu1.f BFDOT), `emit` `cr` `space`
\ (G-EMITC), `type` (habu1.f BTYPE), interpreted `."` and `.\"` and the
\ REPL's ok prompt.
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
private
variable GO-DEV  variable GO-DONE

public
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
;package

;using
;using
;using
;using
