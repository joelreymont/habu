split-str
\ rt.f - native runtime routines for the ARM64 engine builder: data-stack ops,
\ guarded stack mapping and the output and printer sequences. Encoders are
\ package A64ASM (src/arch/arm64/asm.f).
require src/core/exit-status.f
require src/habu/stack-abi.f
require src/habu/crash.f
require src/arch/arm64/icode.f
require src/arch/arm64/asm.f
require src/arch/arm64/mnem.f
HB-TARGET-LINUX-ARM64? [if]
   require src/os/linux/sys.f
[then]
HB-TARGET-MACOS? [if]
   require src/os/macos/sys.f
[then]

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
\ any region does: its line's text is laid inline and the exit is 71, as for the
\ two fixed-region mappings.
: EMIT-MAP ( n n -- )
   MAP-DST !  MAP-CAP !
   A64ICODE:LBL MAP-OK !  A64ICODE:LBL MAP-ROK !  A64ICODE:LBL MAP-BAD !  A64ICODE:LBL MAP-DONE !
   0 0 A64ICODE:MOVZ,
   1 MAP-CAP @ STACK-ABI:PAGE-BYTES 3 * + A64ICODE:LIT64,
   2 0 A64ICODE:MOVZ,                                        \ PROT_NONE: the whole span is guard
   3 SYSCALL:MAP-ANON-PRIVATE A64ICODE:LIT64,
   4 0 A64ICODE:MOVN,  5 0 A64ICODE:MOVZ,
   SYSCALL:NR-MMAP SYSCALL:SYS,
   6 STACK-ABI:PAGE-BYTES A64ICODE:LIT64,  0 6 A64ICODE:CMP,
   A64ASM:C-GE MAP-OK A64ICODE:LABEL@ A64ICODE:BCOND,                        \ a -errno return is a small negative
   MAP-BAD A64ICODE:LABEL@ A64ICODE:B,
   MAP-OK A64ICODE:LABEL@ A64ICODE:LBL,
   9 0 0 A64ICODE:ADDI,
   6 STACK-ABI:PAGE-BYTES 2 * 1 - A64ICODE:LIT64,  9 9 6 A64ICODE:ADD,
   6 STACK-ABI:PAGE-BYTES negate A64ICODE:LIT64,   9 9 6 A64ICODE:AND,   \ x9 = base, one guard page in
   0 9 0 A64ICODE:ADDI,
   1 MAP-CAP @ A64ICODE:LIT64,
   2 3 A64ICODE:MOVZ,                                        \ PROT_READ|PROT_WRITE
   3 SYSCALL:MAP-ANON-PRIVATE-FIXED A64ICODE:LIT64,
   4 0 A64ICODE:MOVN,  5 0 A64ICODE:MOVZ,
   SYSCALL:NR-MMAP SYSCALL:SYS,
   0 9 A64ICODE:CMP,
   A64ASM:C-EQ MAP-ROK A64ICODE:LABEL@ A64ICODE:BCOND,
   MAP-BAD A64ICODE:LABEL@ A64ICODE:B,
   MAP-ROK A64ICODE:LABEL@ A64ICODE:LBL,
   MAP-DST @ 9 0 A64ICODE:ADDI,
   MAP-DONE A64ICODE:LABEL@ A64ICODE:B,
   MAP-BAD A64ICODE:LABEL@ A64ICODE:LBL,
   3 MAP-CAP @ STACK-ABI:PAGE-BYTES 3 * + A64ICODE:LIT64,
   s" hb: mapping a guarded VM stack failed; raise the address-space limit (ulimit -v) above its size in bytes:"
   EXIT-STATUS:SYSTEM 3 CRASH:DIE-LINE-VALUE,
   MAP-DONE A64ICODE:LABEL@ A64ICODE:LBL, ;

;package

package RT
public
\ XDS points just past the top cell (full-ascending): a push stores and advances
\ (post-index), a pop retreats and loads (pre-index), one instruction each.
\ Every baked primitive is built from these.
: G-PUSH ( n -- )
   A64ICODE:XDS 8 A64ICODE:STRPOST, ;

: G-POP ( n -- )
   A64ICODE:XDS -8 A64ICODE:LDRPRE, ;
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
   A64ICODE:LBL GO-DEV !  A64ICODE:LBL GO-DONE !
   0 XREG-RBASE GENIO-ABI:OUT-CELL A64ICODE:LDR,  0 GO-DEV A64ICODE:LABEL@ A64ICODE:CBNZ,
   0 1 A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   GO-DONE A64ICODE:LABEL@ A64ICODE:B,
   GO-DEV A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP $10 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,
   LGENIOOUT A64ICODE:LABEL@ A64ICODE:BL,
   30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP $10 A64ICODE:ADDI,
   GO-DONE A64ICODE:LABEL@ A64ICODE:LBL, ;

\ Print x9 as signed decimal + newline. Clobbers x9-x14 + 32 bytes of sp
\ scratch; preserves XDS.
\
\ THE DIGIT LOOP DIVIDES UNSIGNED. A negative value is negated first; MIN-N
\ negates to itself, and UDIV reads that pattern as its magnitude (2^63). A
\ signed divide printed MIN-N as bytes below '0'.
: G-PRINT9 ( -- )
   A64ICODE:LBL RT-LPOS !  A64ICODE:LBL RT-LLOOP !  A64ICODE:LBL RT-LDONE !
   A64ICODE:SP A64ICODE:SP $20 A64ICODE:SUBI,  12 A64ICODE:SP $20 A64ICODE:ADDI,
   13 $A A64ICODE:MOVZ,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,
   14 0 A64ICODE:MOVZ,  9 0 A64ICODE:CMPI,
   A64ASM:C-GE RT-LPOS A64ICODE:LABEL@ A64ICODE:BCOND,
   14 1 A64ICODE:MOVZ,  9 A64ICODE:SP 9 A64ICODE:SUB,  RT-LPOS A64ICODE:LABEL@ A64ICODE:LBL,
   10 $A A64ICODE:MOVZ,
   RT-LLOOP A64ICODE:LABEL@ A64ICODE:LBL,
   11 9 10 A64ICODE:UDIV,  13 11 10 A64ICODE:MUL,  13 9 13 A64ICODE:SUB,
   13 13 $30 A64ICODE:ADDI,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,
   9 11 0 A64ICODE:ADDI,  9 RT-LLOOP A64ICODE:LABEL@ A64ICODE:CBNZ,
   14 RT-LDONE A64ICODE:LABEL@ A64ICODE:CBZ,
   13 $2D A64ICODE:MOVZ,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,  RT-LDONE A64ICODE:LABEL@ A64ICODE:LBL,
   1 12 0 A64ICODE:ADDI,  2 A64ICODE:SP $20 A64ICODE:ADDI,  2 2 12 A64ICODE:SUB,
   G-OUT
   A64ICODE:SP A64ICODE:SP $20 A64ICODE:ADDI, ;

\ Print x9 as UNSIGNED decimal + newline. Clobbers x9-x13 + 32 bytes of sp
\ scratch.
: G-PRINTU9 ( -- )
   A64ICODE:SP A64ICODE:SP $20 A64ICODE:SUBI,  12 A64ICODE:SP $20 A64ICODE:ADDI,
   13 $A A64ICODE:MOVZ,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,
   10 $A A64ICODE:MOVZ,
   A64ICODE:LBL RT-LLOOP !  RT-LLOOP A64ICODE:LABEL@ A64ICODE:LBL,
   11 9 10 A64ICODE:UDIV,  13 11 10 A64ICODE:MUL,  13 9 13 A64ICODE:SUB,
   13 13 $30 A64ICODE:ADDI,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,
   9 11 0 A64ICODE:ADDI,  9 RT-LLOOP A64ICODE:LABEL@ A64ICODE:CBNZ,
   1 12 0 A64ICODE:ADDI,  2 A64ICODE:SP $20 A64ICODE:ADDI,  2 2 12 A64ICODE:SUB,
   G-OUT
   A64ICODE:SP A64ICODE:SP $20 A64ICODE:ADDI, ;

\ Write the single byte in x13 to the current output device (emit/cr/space
\ share it). The byte goes to sp scratch first because a device write takes a
\ span like every other, and the frame G-OUT pushes on the device path sits
\ below this one.
: G-EMITC ( -- )
   A64ICODE:SP A64ICODE:SP $10 A64ICODE:SUBI,  13 A64ICODE:SP 0 A64ICODE:STRB,
   1 A64ICODE:SP 0 A64ICODE:ADDI,  2 1 A64ICODE:MOVZ,
   G-OUT
   A64ICODE:SP A64ICODE:SP $10 A64ICODE:ADDI, ;
;package
