\ crash.f - the in-binary signal handlers the native engine bakes: the crash
\ handler, the raw sa_tramp, and EMIT-SIGNAL-HANDLER, the async-signal-safe
\ stub a program installs for an ordinary signal. Encoders are package A64ASM
\ (src/arch/arm64/asm.f).
\
\ The crash handler gives a SIGSEGV or SIGBUS the first of these rows that
\ holds:
\ 0. a fault in the crash handler itself: death by the signal, no dump;
\ 1. the fault within MACHINE-REACH of sp: the named machine exit 102;
\ 2. not Habu code -- a saved x20 of 0, code interrupted on this thread's
\    signal stack, a fetch (pc = fault address), a pc in the engine's signal
\    handlers (the crash, signal-stub, trap and profiler handlers), or a pc
\    outside the image's text and the live region: the dump, read through no
\    x20;
\ 3. FAULT-MARK-CELL set, a fault while one is delivered: the dump;
\ 4. the data stack's low guard page and FLOORREC-CELL set: the underdepth throw;
\ 5. a data, return or loop stack guard page: the named exit 102;
\ 6. anything else: the line on fd 2 and the throw of MEMORY-FAULT.
\ The dump, which every other signal takes too, writes sig, x0..x28 and
\ fp/lr/sp/pc as hex lines to stderr, and the process dies by the signal
\ itself (C-CRASH-RERAISE).
require src/habu/stack-abi.f
require src/arch/arm64/icode.f
using SYSCALL
package CRASH
public
variable LCRASHH   variable LHEX   variable LDIE-LINE   variable LHDR   variable LSIGH
variable LFAULTREC  variable LHANDLERS-END
private
create CRH 80 allot  variable CRHL
variable CR-L1  variable CR-L2
variable CR-OFF  variable CR-HANDLER  variable CR-FLAGS  variable CR-RERAISE
;package
using A64ASM
using A64ICODE

package CRASH
: CRH-BYTE+ ( ptr u8 n -- ptr u8 ) + ;

: CRH-INIT ( -- )
   s" habu-crash regs [sig x0..x28 fp lr sp pc], hex one-per-line:" {: a:ptr u:n :}
   0 BEGIN dup u < WHILE
      dup a swap CRH-BYTE+ c@  over CRH swap CRH-BYTE+ c!
      1 +
   REPEAT drop
   $A CRH u CRH-BYTE+ c!  u 1 + CRHL ! ;
CRH-INIT
\ The sa_flags bits the installs set. SA_ONSTACK runs the crash handler on the
\ thread's alternate signal stack (C-SIGNAL-STACK below); the trap handler
\ (src/arch/arm64/boot.f TRAP,) keeps the interrupted stack. SA_NODEFER leaves
\ the crash handler's signal unblocked, so a fault inside it enters it again
\ instead of re-executing the faulting load forever.
$40 constant MACOS-SA-SIGINFO
$1 constant MACOS-SA-ONSTACK
$10 constant MACOS-SA-NODEFER
$4 constant LINUX-SA-SIGINFO
$08000000 constant LINUX-SA-ONSTACK
$40000000 constant LINUX-SA-NODEFER
public
: SIGACT-SIGINFO ( -- n )  HB-TARGET-LINUX? IF LINUX-SA-SIGINFO ELSE MACOS-SA-SIGINFO THEN ;
private
: SIGACT-ONSTACK ( -- n )  HB-TARGET-LINUX? IF LINUX-SA-ONSTACK ELSE MACOS-SA-ONSTACK THEN ;
: SIGACT-NODEFER ( -- n )  HB-TARGET-LINUX? IF LINUX-SA-NODEFER ELSE MACOS-SA-NODEFER THEN ;
\ stack_t, the struct sigaltstack reads and writes: ss_sp first on both, then
\ Linux puts the int ss_flags before ss_size and Darwin after it.
32 constant SIGSTK-FRAME                \ the 24-byte struct, rounded to sp's alignment
: SIGSTK-FLAGS ( -- n )  HB-TARGET-LINUX? IF 8 ELSE 16 THEN ;
: SIGSTK-SIZE ( -- n )  HB-TARGET-LINUX? IF 16 ELSE 8 THEN ;
: SIGSTK-DISABLE ( -- n )  HB-TARGET-LINUX? IF 2 ELSE 4 THEN ;      \ SS_DISABLE
\ `sub/add xd, xn, #n` for n in whole 4 KiB units: the shifted-immediate form,
\ which also takes sp. A remainder is refused (asm.f SCALE/).
: SUBI-4K, ( n n n -- )  $1000 SCALE/ ENC-SUBI-LSL12 EMITW ;
public
: ADDI-4K, ( n n n -- )  $1000 SCALE/ ENC-ADDI-LSL12 EMITW ;
private
$8 constant CRASH-LINUX-SIGSET-SIZE
public
$30 constant MCTX-OFF           \ macOS ucontext -> mcontext pointer offset
$10 constant SS-OFF             \ macOS mcontext -> __ss.__x[0] offset
$B0 constant LINUX-UC-MCTX-OFF
$8 constant LINUX-MCTX-X0-OFF
$A0 constant LINUX-MCTX-X19-OFF
private
$F0 constant LINUX-MCTX-FP-OFF
public
$F8 constant LINUX-MCTX-LR-OFF
$100 constant LINUX-MCTX-SP-OFF
$108 constant LINUX-MCTX-PC-OFF
private
$10 constant LINUX-SI-ADDR-OFF  \ siginfo_t._sifields._sigfault.si_addr
$18 constant MACOS-SI-ADDR-OFF  \ __siginfo.si_addr, after six ints
public
$A8 constant MACOS-MCTX-X19-OFF
private
$F8 constant MACOS-MCTX-FP-OFF
public
$100 constant MACOS-MCTX-LR-OFF
$108 constant MACOS-MCTX-SP-OFF
$110 constant MACOS-MCTX-PC-OFF

private
\ x9's 16 hex digits, most significant first, at [x14, x14 + 16). Clobbers
\ x9, x11..x13 and x15.
: C-HEX-DIGITS ( -- )
   LBL LBL LBL {: next:label done:label digit:label :}
   11 $F MOVZ,
   next LBL,
      12 9 $F ANDI,
      13 12 $30 ADDI,
      12 $A CMPI,  C-LT digit BCOND,  13 13 $27 ADDI,
      digit LBL,
      15 14 11 ADD,  13 15 0 STRB,
      9 9 4 LSRI,
      11 done CBZ,
      11 11 1 SUBI,  next B,
   done LBL, ;

public
\ rd = the text content size of the image whose code offset 0 is at rd: its
\ header's text-size field less IMAGE-TEXT-CONTENT-ADJ. Clobbers tmp.
: TEXT-SIZE, ( n n -- )
   {: rd:n tmp:n :}
   tmp CODE-OFF LIT64,  rd rd tmp SUB,
   rd rd IMAGE-TEXT-SIZE-OFF LDR,  tmp IMAGE-TEXT-CONTENT-ADJ LIT64,  rd rd tmp SUB, ;

\ LHEX ( x9=val ): write 16 hex digits + newline to fd 2. Leaf; clobbers
\ x9..x15 and x0-x2/x16 (write syscall).
: EMIT-HEX ( -- )
   LHEX LABEL@ LBL,
   SP SP $20 SUBI,
   14 SP 0 ADDI,
   C-HEX-DIGITS
   12 $A MOVZ,  12 14 $10 STRB,
   0 2 MOVZ,  1 14 0 ADDI,  2 $11 MOVZ,  NR-WRITE SYS,
   SP SP $20 ADDI,  RET, ;

private
256 constant DIE-LINE-FRAME
\ The text's room in the frame: the rest holds " ", 16 hex digits, " lr ",
\ 16 hex digits and the newline.
DIE-LINE-FRAME 38 - constant DIE-LINE-ROOM

\ [x14] = the byte n, x14 one past it. Clobbers x13.
: DIE-LINE-BYTE, ( n -- )
   13 swap MOVZ,  13 14 0 STRB,  14 14 1 ADDI, ;

\ [x14, x14 + 16) = the register's 16 hex digits, x14 past them.
: DIE-LINE-HEX, ( n -- )
   9 swap 0 ADDI,  C-HEX-DIGITS  14 14 16 ADDI, ;

public
\ Entered by B, so x30 is the lr it prints. Writes `<text>[ <x3>] lr <x30>`,
\ 16 hex digits each, and a newline to fd 2 in one write, then exits x0.
\ x1/x2 = the text, 1 to DIE-LINE-ROOM bytes; x3 prints when x4 is nonzero.
: EMIT-DIE-LINE ( -- )
   LBL LBL {: copy:label bare:label :}
   LDIE-LINE LABEL@ LBL,
   5 0 0 ADDI,
   SP SP DIE-LINE-FRAME SUBI,
   14 SP 0 ADDI,
   copy LBL,
      13 1 0 LDRB,  13 14 0 STRB,
      1 1 1 ADDI,  14 14 1 ADDI,  2 2 1 SUBI,  2 copy CBNZ,
   4 bare CBZ,
   $20 DIE-LINE-BYTE,  3 DIE-LINE-HEX,
   bare LBL,
   $20 DIE-LINE-BYTE,  $6C DIE-LINE-BYTE,  $72 DIE-LINE-BYTE,  $20 DIE-LINE-BYTE,
   30 DIE-LINE-HEX,
   $A DIE-LINE-BYTE,
   0 2 MOVZ,  1 SP 0 ADDI,  2 14 1 SUB,  NR-WRITE SYS,
   0 5 0 ADDI,  NR-EXIT-GROUP SYS, ;

private
: DIE-LINE-SITE, ( ptr u8 n n n -- )
   LBL {: a:ptr u:n status:n flag:n msg:label :}
   u 0= u DIE-LINE-ROOM > or IF
      s" crash: a DIE-LINE, text is empty or longer than DIE-LINE-ROOM" ICODE-EXIT-RC die
   THEN
   0 status MOVZ,  1 msg ADR,  2 u MOVZ,  4 flag MOVZ,
   LDIE-LINE LABEL@ B,
   msg LBL,  a u BYTES, ;

public
\ A branch to LDIE-LINE with the text and the exit status, the text laid
\ after it. The caller places its own label before it.
: DIE-LINE, ( ptr u8 n n -- )  0 DIE-LINE-SITE, ;

\ DIE-LINE, that also prints the register's value; the register is copied
\ first, so it may be any of x0..x4.
: DIE-LINE-VALUE, ( ptr u8 n n n -- )
   {: a:ptr u:n status:n reg:n :}
   u 0= IF 0 ELSE a u + 1- c@ THEN $3A <> IF
      s" crash: a DIE-LINE-VALUE, text does not end with the ':' before its value" ICODE-EXIT-RC die
   THEN
   reg 3 <> IF 3 reg 0 ADDI, THEN
   a u status 1 DIE-LINE-SITE, ;

\ x30 = the caller's lr, saved at [sp + off], for a site whose own BL or BLR
\ replaced x30; LDIE-LINE prints it.
: CALLER-LR, ( n -- ) {: off:n :}
   30 SP off LDR, ;

\ x30 = the interrupted code's lr, read from the signal mcontext the register
\ points at; LDIE-LINE prints it.
: C-CRASH-LR>R30 ( n -- ) {: mctx:n :}
   HB-TARGET-LINUX? IF 30 mctx LINUX-MCTX-LR-OFF LDR, exit THEN
   30 mctx MACOS-MCTX-LR-OFF LDR, ;

private
\ x20 = signal number, x19 = ucontext, x23 = siginfo, whose si_addr tells a
\ SIGSEGV inside a VM stack's guard page from any other. Linux enters the
\ handler with (sig, info, uctx) in x0/x1/x2; the macOS trampoline passes
\ (catcher, style, sig, info, uctx) in x0..x4.
: C-CRASH-ENTRY ( -- )
   HB-TARGET-LINUX? IF
      20 0 0 ADDI,
      23 1 0 ADDI,
      19 2 0 ADDI,
      exit
   THEN
   20 2 0 ADDI,
   23 3 0 ADDI,
   19 4 0 ADDI, ;

\ si_addr's byte offset inside siginfo_t: Linux aarch64 pads si_signo/errno/code
\ to 16 bytes, macOS puts six ints ahead of it.
: C-CRASH-FAULT-ADDR>R25 ( -- )
   HB-TARGET-LINUX? IF 25 23 LINUX-SI-ADDR-OFF LDR, exit THEN
   25 23 MACOS-SI-ADDR-OFF LDR, ;

\ The interrupted x20 from the mcontext, as the handler's own x20 is the signal
\ number by now: this thread's DATA wherever kernel code can fault, 0 from a
\ task's or a callback's entry until DATA loads, the frame sp across an FFI
\ call's BLR.
: C-CRASH-DATA>R24 ( -- )
   HB-TARGET-LINUX? IF 24 21 LINUX-MCTX-X0-OFF DATA 8 * + LDR, exit THEN
   24 21 SS-OFF DATA 8 * + LDR, ;

: C-CRASH-MCTX>R21 ( -- )
   HB-TARGET-LINUX? IF 21 19 LINUX-UC-MCTX-OFF ADDI, exit THEN
   21 19 MCTX-OFF LDR, ;

: C-CRASH-XREG>R9 ( -- )
   22 20 3 LSLI,
   HB-TARGET-LINUX? IF
      22 22 LINUX-MCTX-X0-OFF ADDI,
   ELSE
      22 22 SS-OFF ADDI,
   THEN
   22 21 22 ADD,  9 22 0 LDR, ;

: C-CRASH-PC>R9 ( -- )
   HB-TARGET-LINUX? IF 9 21 LINUX-MCTX-PC-OFF LDR, exit THEN
   9 21 MACOS-MCTX-PC-OFF LDR, ;

: C-CRASH-SP>R9 ( -- )
   HB-TARGET-LINUX? IF 9 21 LINUX-MCTX-SP-OFF LDR, exit THEN
   9 21 MACOS-MCTX-SP-OFF LDR, ;

: C-CRASH-PRINT-REGS ( -- )
   HB-TARGET-LINUX? IF
      9 21 LINUX-MCTX-FP-OFF LDR,  LHEX LABEL@ BL,
      9 21 LINUX-MCTX-LR-OFF LDR,  LHEX LABEL@ BL,
   ELSE
      9 21 MACOS-MCTX-FP-OFF LDR,  LHEX LABEL@ BL,
      9 21 MACOS-MCTX-LR-OFF LDR,  LHEX LABEL@ BL,
   THEN
   C-CRASH-SP>R9  LHEX LABEL@ BL,
   C-CRASH-PC>R9  LHEX LABEL@ BL, ;

\ x10 = the live region's base, 0 when there is none, read at the absolute
\ address DATA-VA + DBASE-CELL and never from the saved x26: in foreign code
\ x26 is anything, and a forged base would pass a pc in C as Habu code.
: C-CRASH-REGION>R10 ( -- )
   10 DATA-VA VA>N DBASE-CELL + LIT64,  10 10 0 LDR, ;

: C-CRASH-PC-WORD ( n -- )
   CR-OFF !
   LBL CR-L1 !  LBL CR-L2 !
   C-CRASH-PC>R9
   CR-OFF @ 0< IF 9 9 CR-OFF @ negate SUBI, ELSE 9 9 CR-OFF @ ADDI, THEN
   \ x9 holds the PC being classified; x10 = region base then end-4.
   C-CRASH-REGION>R10  10 CR-L1 LABEL@ CBZ,
   9 10 CMP,  C-LT CR-L1 LABEL@ BCOND,
   11 REGION $4 - LIT64,  10 10 11 ADD,  9 10 CMP,  C-GT CR-L1 LABEL@ BCOND,
   9 9 0 LDRW,  LHEX LABEL@ BL,  CR-L2 LABEL@ B,
   CR-L1 LABEL@ LBL,  9 0 MOVZ,  LHEX LABEL@ BL,
   CR-L2 LABEL@ LBL, ;

: C-CRASH-PC-8 ( -- )
   -8 C-CRASH-PC-WORD ;

: C-CRASH-PC-4 ( -- )
   -4 C-CRASH-PC-WORD ;

: C-CRASH-PC0 ( -- )
   0 C-CRASH-PC-WORD ;

: C-CRASH-PC+4 ( -- )
   4 C-CRASH-PC-WORD ;

\ Every VM stack is a mapping with an inaccessible page on each side
\ (src/habu/rt.f STACK-GUARD:EMIT-MAP), so "this stack overflowed" is a fault
\ whose address lands in one of those pages. Compiled code carries no bounds
\ check; this is where the fault becomes the diagnostic.
\
\ A guard case is skipped rather than trusted when its descriptor is not a
\ plausible one -- a zero base, a base off a cell boundary, or a top (base +
\ capacity) off a PAGE-BYTES boundary. Every extent the engine installs starts
\ on a page boundary and ends where its high guard page begins (rt.f
\ STACK-GUARD:EMIT-MAP, habu1.f GUARDED-EXTENT?).
variable CRS-BASE   variable CRS-CAP   variable CRS-CAPCELL
variable CRS-HIT    variable CRS-HIT-LOW
variable CRS-NEXT   variable CRS-SKIP  variable CRS-FAULT
variable CRS-DATA-H variable CRS-RET-H variable CRS-LOOP-H
variable CRS-DATA-LOW-H
variable CRS-MACHINE-H
variable CRS-THROW  variable CRS-MEMORY-M

\ The four stack-bounds texts LDIE-LINE prints.
: CRS-DATA$ ( -- ptr u8 n ) s" E-STACK-BOUNDS habu: stack bounds exceeded (data)" ;
: CRS-RET$ ( -- ptr u8 n ) s" E-STACK-BOUNDS habu: stack bounds exceeded (return)" ;
: CRS-LOOP$ ( -- ptr u8 n ) s" E-STACK-BOUNDS habu: stack bounds exceeded (loop)" ;
: CRS-MACHINE$ ( -- ptr u8 n ) s" E-STACK-BOUNDS habu: stack bounds exceeded (machine)" ;

\ The memory fault's line, in two runs: the first is whole words, so BYTES,
\ lays the second right after it. The handler writes the fault address and the
\ pc over the zeros at byte offsets CRS-MEMORY-AT and CRS-MEMORY-PC.
: CRS-MEMORY$ ( -- ptr u8 n )
   S\" E-MEMORY-FAULT habu: memory fault at 0000000000000000 pc" ;
: CRS-MEMORY-TAIL$ ( -- ptr u8 n )  S\"  0000000000000000\n" ;
CRS-MEMORY$ nip CRS-MEMORY-TAIL$ nip + constant CRS-MEMORY-LEN
37 constant CRS-MEMORY-AT
57 constant CRS-MEMORY-PC
80 constant CRS-MEMORY-FRAME            \ the line's copy, rounded to sp's alignment

11 constant CRASH-SIGSEGV
: CRASH-SIGBUS ( -- n )  HB-TARGET-LINUX? IF 7 ELSE 10 THEN ;

\ x24 = DATA, x25 = fault address, x11 = PAGE-BYTES, x12 = PAGE-BYTES-1.
\ CRS-CAPCELL is a header offset to read the capacity from, or 0 to use CRS-CAP.
\ A low-guard hit branches to CRS-HIT-LOW, a high-guard hit to CRS-HIT.
: C-CRASH-GUARD-CASE ( -- )
   9 24 CRS-BASE @ LDR,
   9 CRS-NEXT LABEL@ CBZ,
   10 9 7 ANDI,  10 CRS-NEXT LABEL@ CBNZ,
   CRS-CAPCELL @ 0= IF 13 CRS-CAP @ LIT64, ELSE 13 24 CRS-CAPCELL @ LDR, THEN
   13 9 13 ADD,                                  \ high guard starts at base + capacity,
   10 13 12 AND,  10 CRS-NEXT LABEL@ CBNZ,       \ a page boundary
   10 25 13 SUB,
   10 11 CMP,  C-CC CRS-HIT LABEL@ BCOND,
   13 9 12 AND,  13 9 13 SUB,  13 13 11 SUB,     \ low guard: the page under the rounded base
   10 25 13 SUB,
   10 11 CMP,  C-CC CRS-HIT-LOW LABEL@ BCOND,
   CRS-NEXT LABEL@ LBL, ;

\ The line for the guard page hit at the label; x30 is the interrupted code's
\ lr. Never returns.
: C-CRASH-GUARD-REPORT ( ptr u8 n label -- )
   {: a:ptr u:n hit:label :}
   hit LBL,  21 C-CRASH-LR>R30
   a u ENGINE-ERROR:STACK-BOUNDS DIE-LINE, ;

\ x14 = the pc the interrupted thread resumes at; sigreturn resumes it there
\ with every other register as the saved context holds it. On macOS x1 and x5
\ still hold the trampoline's infostyle and token. Returns only if sigreturn
\ fails.
: C-CRASH-RESUME ( -- )
   HB-TARGET-LINUX? IF
      14 21 LINUX-MCTX-PC-OFF STR,
      NR-SIGRETURN SYS,                          \ rt_sigreturn reads the frame at the entry sp
      exit
   THEN
   14 21 MACOS-MCTX-PC-OFF STR,
   0 19 0 ADDI,  2 5 0 ADDI,
   NR-SIGRETURN SYS, ;                           \ sigreturn(uctx, infostyle, token)

\ x9 = nonzero if the interrupted sp lies in the signal stack this thread
\ registered, as sigaltstack(NULL, old) answers it; zero when none is. Not the
\ ucontext's flags: Linux records there the flags the thread registered, never
\ SS_ONSTACK. Writes x0, x9, x10 and the system call's registers; x1, the
\ trampoline's infostyle on macOS, is kept in the frame across the call.
: C-CRASH-ONSTACK>R9 ( -- )
   LBL {: off:label :}
   SP SP SIGSTK-FRAME SUBI,
   1 SP 24 STR,
   0 0 MOVZ,  1 SP 0 ADDI,  NR-SIGALTSTACK SYS,
   1 SP 24 LDR,
   9 0 MOVZ,
   10 SP SIGSTK-FLAGS LDRW,  10 10 SIGSTK-DISABLE ANDI,  10 off CBNZ,
   C-CRASH-SP>R9
   10 SP 0 LDR,  9 9 10 SUB,                     \ sp - ss_sp, unsigned
   10 SP SIGSTK-SIZE LDR,  9 10 CMP,  9 C-CC CSET,
   off LBL,
   SP SP SIGSTK-FRAME ADDI, ;

\ Row 2, from the ucontext, the saved registers and DBASE-CELL, reading through
\ none of the registers; falls through when Habu code faulted. A fetch's pc is
\ the jump's target and says nothing of the code that ran. The signal handlers
\ are the band [LCRASHH, LHANDLERS-END): the trap handler runs off the signal
\ stack with x20 = DATA. Leaves x24 = the saved x20, DATA, and x9 = the saved
\ pc.
: C-CRASH-HABU-PC ( -- )
   LBL LBL LBL {: text:label region:label habu:label :}
   C-CRASH-ONSTACK>R9  9 CRS-SKIP LABEL@ CBNZ,
   C-CRASH-DATA>R24  24 CRS-SKIP LABEL@ CBZ,
   C-CRASH-PC>R9  9 25 CMP,  C-EQ CRS-SKIP LABEL@ BCOND,
   10 LCRASHH LABEL@ ADR,  9 10 CMP,  C-CC text BCOND,
   11 LHANDLERS-END LABEL@ ADR,  9 11 CMP,  C-CC CRS-SKIP LABEL@ BCOND,
   text LBL,
   11 LCRASHH LABEL@ LOFF,  10 10 11 SUB,       \ code offset 0
   9 10 CMP,  C-CC region BCOND,
   11 10 0 ADDI,  11 12 TEXT-SIZE,  10 10 11 ADD,
   9 10 CMP,  C-CC habu BCOND,
   region LBL,
   C-CRASH-REGION>R10  10 CRS-SKIP LABEL@ CBZ,
   9 10 CMP,  C-CC CRS-SKIP LABEL@ BCOND,
   11 REGION LIT64,  10 10 11 ADD,  9 10 CMP,  C-CS CRS-SKIP LABEL@ BCOND,
   habu LBL, ;

\ Row 4: a word reached below the data stack's base -- `dup` on an empty
\ stack, `1 ' W execute` handing a two-cell word one cell, a closed text's xt
\ reaching under its floor. The thread resumes at FLOORREC-CELL's entry, the
\ underdepth throw. A zero entry and a sigreturn that comes back take the named
\ data exit emitted right after this.
: C-CRASH-DATA-RECOVER ( -- )
   CRS-DATA-LOW-H LABEL@ LBL,
   14 24 FLOORREC-CELL LDR,  14 CRS-DATA-H LABEL@ CBZ,
   C-CRASH-RESUME ;

\ Row 6, x24 = DATA and x25 = the fault address: set the mark, write the line
\ from a copy on the stack, store the code in the saved x9 and resume at the
\ fault entry. x26 keeps macOS's infostyle across the write.
: C-CRASH-THROW ( -- )
   LBL {: copy:label :}
   CRS-THROW LABEL@ LBL,
   HB-TARGET-LINUX? 0= IF 26 1 0 ADDI, THEN
   14 1 MOVZ,  14 24 FAULT-MARK-CELL STR,
   SP SP CRS-MEMORY-FRAME SUBI,
   10 CRS-MEMORY-M LABEL@ ADR,  11 SP 0 ADDI,  12 CRS-MEMORY-LEN MOVZ,
   copy LBL,
      13 10 0 LDRB,  13 11 0 STRB,
      10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  12 copy CBNZ,
   9 25 0 ADDI,  14 SP CRS-MEMORY-AT ADDI,  C-HEX-DIGITS
   C-CRASH-PC>R9  14 SP CRS-MEMORY-PC ADDI,  C-HEX-DIGITS
   0 2 MOVZ,  1 SP 0 ADDI,  2 CRS-MEMORY-LEN MOVZ,  NR-WRITE SYS,
   SP SP CRS-MEMORY-FRAME ADDI,
   10 ENGINE-ERROR:MEMORY-FAULT MOVZ,
   HB-TARGET-LINUX? IF
      10 21 LINUX-MCTX-X0-OFF 9 8 * + STR,
   ELSE
      10 21 SS-OFF 9 8 * + STR,
   THEN
   14 LFAULTREC LABEL@ ADR,
   HB-TARGET-LINUX? 0= IF 1 26 0 ADDI, THEN
   C-CRASH-RESUME ;

\ Rows 1 to 6 of the file header's list.
: C-CRASH-STACK-GUARDS ( -- )
   LBL CRS-SKIP !  LBL CRS-FAULT !  LBL CRS-THROW !  LBL CRS-MEMORY-M !
   LBL CRS-DATA-H !  LBL CRS-RET-H !  LBL CRS-LOOP-H !  LBL CRS-DATA-LOW-H !
   LBL CRS-MACHINE-H !
   \ si_addr only describes a memory fault; a trap or an FPE carries no address.
   20 CRASH-SIGSEGV CMPI,  C-EQ CRS-FAULT LABEL@ BCOND,
   20 CRASH-SIGBUS CMPI,   C-NE CRS-SKIP LABEL@ BCOND,
   CRS-FAULT LABEL@ LBL,
   C-CRASH-FAULT-ADDR>R25
   \ The machine stack first, by the interrupted sp alone (stack-abi.f
   \ MACHINE-REACH): it needs no DATA, so it holds in foreign code and on a
   \ thread whose x20 is not a region. x1 and x5 stay the trampoline's.
   C-CRASH-SP>R9
   11 STACK-ABI:MACHINE-REACH LIT64,
   10 25 9 SUB,  10 10 11 ADD,                   \ fault - sp + reach
   11 11 1 LSLI,
   10 11 CMP,  C-CC CRS-MACHINE-H LABEL@ BCOND,
   C-CRASH-HABU-PC
   14 24 FAULT-MARK-CELL LDR,  14 CRS-SKIP LABEL@ CBNZ,
   11 STACK-ABI:PAGE-BYTES LIT64,
   12 STACK-ABI:PAGE-BYTES 1 - LIT64,
   STACK-ABI:BASE-CELL CRS-BASE !  STACK-ABI:CAP-CELL CRS-CAPCELL !
   CRS-DATA-H @ CRS-HIT !  CRS-DATA-LOW-H @ CRS-HIT-LOW !
   LBL CRS-NEXT !  C-CRASH-GUARD-CASE
   STACK-ABI:RETURN-BASE-CELL CRS-BASE !  0 CRS-CAPCELL !
   STACK-ABI:RETURN-BYTES CRS-CAP !
   CRS-RET-H @ CRS-HIT !  CRS-RET-H @ CRS-HIT-LOW !
   LBL CRS-NEXT !  C-CRASH-GUARD-CASE
   STACK-ABI:LOOP-BASE-CELL CRS-BASE !  0 CRS-CAPCELL !
   STACK-ABI:LOOP-BYTES CRS-CAP !
   CRS-LOOP-H @ CRS-HIT !  CRS-LOOP-H @ CRS-HIT-LOW !
   LBL CRS-NEXT !  C-CRASH-GUARD-CASE
   CRS-THROW LABEL@ B,
   C-CRASH-DATA-RECOVER
   CRS-DATA$ CRS-DATA-H LABEL@ C-CRASH-GUARD-REPORT
   CRS-RET$ CRS-RET-H LABEL@ C-CRASH-GUARD-REPORT
   CRS-LOOP$ CRS-LOOP-H LABEL@ C-CRASH-GUARD-REPORT
   CRS-MACHINE$ CRS-MACHINE-H LABEL@ C-CRASH-GUARD-REPORT
   CRS-MEMORY-M LABEL@ LBL,  CRS-MEMORY$ BYTES,  CRS-MEMORY-TAIL$ BYTES,
   C-CRASH-THROW                                 \ a sigreturn that comes back dumps
   CRS-SKIP LABEL@ LBL, ;

\ sigaction(x0, the frame at sp, NULL).
: C-SIGACTION-CALL ( -- )
   1 SP 0 ADDI,  2 0 MOVZ,
   HB-TARGET-LINUX? IF 3 CRASH-LINUX-SIGSET-SIZE MOVZ, THEN
   NR-SIGACTION SYS, ;

public
: INSTALL-SIGACT ( n -- )
   0 swap MOVZ,  C-SIGACTION-CALL ;

\ The sigaction frame for the handler in a register and the sa_flags given.
\ Darwin's __sigaction is the handler, sa_tramp, then the u32 sa_mask and the
\ u32 sa_flags: the flags are the high half of the cell at +16.
: C-SIGACTION-FRAME ( n n -- )
   CR-FLAGS !  CR-HANDLER !
   SP SP $40 SUBI,
   CR-HANDLER @ SP 0 STR,
   HB-TARGET-LINUX? IF
      10 CR-FLAGS @ LIT64,  10 SP $8 STR,
      10 0 MOVZ,  10 SP $10 STR,  10 SP $18 STR,
      exit
   THEN
   CR-HANDLER @ SP $8 STR,
   10 CR-FLAGS @ $20 lshift LIT64,  10 SP $10 STR, ;

: C-SIGACTION-FRAME-DONE ( -- )
   SP SP $40 ADDI, ;

private
\ The end of a reported crash: the process dies by the signal itself, so the
\ parent sees it and a core (when RLIMIT_CORE allows) shows the faulting
\ context. x27 = the signal; on macOS x24 = the trampoline's infostyle and x5
\ its token. SIG_DFL goes in, the signal, which SA_NODEFER left unblocked, is
\ blocked and sent to this thread, where it stays pending, and sigreturn
\ restores the interrupted context and its mask: the signal is delivered at the
\ faulting pc and takes its default action there. Linux rt_sigreturn reads the
\ frame at the entry sp, which every call in the dump and here gives back.
: C-CRASH-RERAISE ( -- )
   CR-RERAISE LABEL@ LBL,
   11 0 MOVZ,  11 0 C-SIGACTION-FRAME            \ handler SIG_DFL, no flags
   0 27 0 ADDI,  C-SIGACTION-CALL
   C-SIGACTION-FRAME-DONE
   27 OS-BLOCK-SELF
   27 OS-RAISE-SELF
   HB-TARGET-LINUX? 0= IF 0 19 0 ADDI,  1 24 0 ADDI,  2 5 0 ADDI, THEN
   NR-SIGRETURN SYS,                             \ macOS: sigreturn(uctx, infostyle, token)
   \ Reached only if sigreturn fails.
   21 C-CRASH-LR>R30
   s" E-SIGRETURN habu: sigreturn failed in the crash handler" $86 DIE-LINE, ;

\ Row 0: a saved pc in [LCRASHH, LSIGH), the crash handler's own code, goes
\ straight to the re-raise. On macOS x1 is still the trampoline's infostyle.
: C-CRASH-NESTED ( -- )
   LBL {: outside:label :}
   C-CRASH-PC>R9
   10 LCRASHH LABEL@ ADR,  9 10 CMP,  C-CC outside BCOND,
   10 LSIGH LABEL@ ADR,  9 10 CMP,  C-CS outside BCOND,
   HB-TARGET-LINUX? 0= IF 24 1 0 ADDI, THEN
   CR-RERAISE LABEL@ B,
   outside LBL, ;

public
: EMIT-CRASH-HANDLER ( -- )
   LCRASHH LABEL@ LBL,
   LBL CR-L1 !  LBL CR-L2 !  LBL CR-RERAISE !
      C-CRASH-ENTRY
      27 20 0 ADDI,                                 \ x27 = the signal; the dump reuses x20
      C-CRASH-MCTX>R21
      C-CRASH-NESTED
      C-CRASH-STACK-GUARDS                          \ rows 2 and 3 fall through
      HB-TARGET-LINUX? 0= IF                        \ x24 = the trampoline's infostyle:
         24 1 0 ADDI,                               \ the dump's writes clobber x1
      THEN
      1 LHDR LABEL@ ADR,  0 2 MOVZ,  2 CRHL @ MOVZ,  NR-WRITE SYS,
      9 20 0 ADDI,  LHEX LABEL@ BL,
      20 0 MOVZ,
      CR-L1 LABEL@ LBL,  20 $1D CMPI,  C-GE CR-L2 LABEL@ BCOND,
         C-CRASH-XREG>R9  LHEX LABEL@ BL,
         20 20 1 ADDI,  CR-L1 LABEL@ B,
      CR-L2 LABEL@ LBL,
      C-CRASH-PRINT-REGS
      C-CRASH-PC-8
      C-CRASH-PC-4
      C-CRASH-PC0
      C-CRASH-PC+4
      C-CRASH-RERAISE
   LHDR LABEL@ LBL,  CRH CRHL @ BYTES, ;

\ The caller loads x11 with the handler's address: src/arch/arm64/boot.f
\ STACKS, reaches it from the text base through TEXT-AT,.
: G-INSTALL-CRASH-X11 ( -- )                     \ x11 = the crash handler's address
   11 SIGACT-SIGINFO SIGACT-ONSTACK or SIGACT-NODEFER or C-SIGACTION-FRAME
   HB-TARGET-LINUX? IF
      4 INSTALL-SIGACT  5 INSTALL-SIGACT  7 INSTALL-SIGACT  8 INSTALL-SIGACT  11 INSTALL-SIGACT
   ELSE
      4 INSTALL-SIGACT  5 INSTALL-SIGACT  8 INSTALL-SIGACT  10 INSTALL-SIGACT  11 INSTALL-SIGACT
   THEN
   C-SIGACTION-FRAME-DONE ;

private
\ A machine-stack overflow faults with sp in the kernel's guard, where no signal
\ frame fits: without an alternate stack the handler never runs and the kernel
\ kills the process unnamed. So the handler is installed SA_ONSTACK, and every
\ thread that runs Habu code registers its own alternate stack before it can
\ overflow, carved off its machine stack (stack-abi.f SIGNAL-BYTES): the
\ engine's and a linked product's start-up (src/arch/arm64/boot.f
\ ENTRY,), a task's thread (habu1.f BTASK-ENTRY) and a
\ thread C started, at the callback that brings it into Habu code (habu1.f
\ BCALLBACK-THUNK). The profiler's SA_ONSTACK handler runs on the same one.
\
\ Each of the three writes x0, x1, x12 and the system call's own registers, and
\ x2 on the path that exits, and nothing else, and holds no BL and no ADR into
\ x9.
: SIGSTKMSG$ ( -- ptr u8 n ) S\" hb: cannot install the signal stack\n" ;

public
\ sp drops SIGNAL-BYTES and the band above it is registered. A refused
\ registration is named and fatal: the handler would run on the interrupted
\ stack, and a machine-stack overflow would die unnamed again.
: C-SIGNAL-STACK ( -- )
   LBL LBL {: ok:label msg:label :}
   SP SP STACK-ABI:SIGNAL-BYTES SUBI-4K,
   SP SP SIGSTK-FRAME SUBI,
   12 0 MOVZ,  12 SP SIGSTK-FLAGS STR,
   12 STACK-ABI:SIGNAL-BYTES LIT64,  12 SP SIGSTK-SIZE STR,
   12 SP SIGSTK-FRAME ADDI,  12 SP 0 STR,             \ ss_sp: the band, right above this frame
   0 SP 0 ADDI,  1 0 MOVZ,  NR-SIGALTSTACK SYS,
   SP SP SIGSTK-FRAME ADDI,                           \ ADD, not ADDS: the flags are the call's
   C-CC ok BCOND,
      0 2 MOVZ,  1 msg ADR,  2 SIGSTKMSG$ nip MOVZ,  NR-WRITE SYS,
      0 STACK-ABI:MAP-FAIL-RC MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  SIGSTKMSG$ BYTES,
   ok LBL, ;

\ The way out of the code that registered: SS_DISABLE, then the caller gives
\ the band back. Its result goes unchecked because it cannot fail off the
\ alternate stack: the one refusal of a well-formed disable is a thread running
\ on its alternate stack, and sp is below the band.
: C-SIGNAL-STACK-OFF ( -- )
   SP SP SIGSTK-FRAME SUBI,
   12 SIGSTK-DISABLE MOVZ,  12 SP SIGSTK-FLAGS STR,
   0 SP 0 ADDI,  1 0 MOVZ,  NR-SIGALTSTACK SYS,
   SP SP SIGSTK-FRAME ADDI, ;

\ x12 = nonzero when this thread has no alternate stack registered: the query
\ sigaltstack(NULL, old) answers SS_DISABLE in old's flags. A query cannot be
\ refused: its one error is an unwritable buffer, and the buffer is this frame.
: C-SIGNAL-STACK-ABSENT>R12 ( -- )
   SP SP SIGSTK-FRAME SUBI,
   0 0 MOVZ,  1 SP 0 ADDI,  NR-SIGALTSTACK SYS,
   12 SP SIGSTK-FLAGS LDRW,                           \ ss_flags is an int
   12 12 SIGSTK-DISABLE ANDI,
   SP SP SIGSTK-FRAME ADDI, ;

\ LSIGH: the async-signal-safe stub a program installs through sigaction to get
\ a signal on a file descriptor. No Forth word is async-signal-safe, so this is
\ machine code: it reads the process-wide fd word and, when that is nonzero,
\ writes the four-byte signal number to it with one write syscall.
\
\ IT IS AN ORDINARY `void (int)` sa_handler, installed through libc sigaction,
\ whose trampoline passes the number in x0 on Linux and macOS alike. The crash
\ handler is the raw sa_tramp and reads it from x2 on macOS (C-CRASH-ENTRY).
\
\ IT READS THE FD WORD BY ITS ABSOLUTE ADDRESS, never as `DATA <off>`: it runs
\ on whichever thread the kernel picks, whose x20 is its own task region, so the
\ offset form would read a different word per task and zero on a new one.
\ DATA-VA is fixed (src/habu/layout.f package SIGNAL-ABI).
\
\ IT COSTS THE INTERRUPTED CONTEXT x0, x1, x2, the registers SYS, uses, the
\ flags and sixteen bytes below its sp, and touches no Forth state (no DATA
\ cell, dictionary or VM stack), so a program can carry it through any point
\ of its execution. A fd word of zero absorbs the signal.
\
\ THE WRITE IS FOUR BYTES AND ITS RESULT IS IGNORED. Four bytes is under
\ PIPE_BUF, so a pipe takes it whole or not at all: a reader that falls behind
\ loses whole signals, never framing. The CALLER makes the write end
\ non-blocking: a blocking write into a full pipe would stall the thread the
\ signal landed on.
: EMIT-SIGNAL-HANDLER ( -- )
   LSIGH LABEL@ LBL,
   LBL {: done:label :}
   SP SP $10 SUBI,
   0 SP 0 STR,                                   \ the four low bytes ARE the number the fd receives
   0 DATA-VA VA>N SIGNAL-ABI:FD-CELL + LIT64,
   0 0 0 LDR,
   0 done CBZ,
   1 SP 0 ADDI,  2 4 MOVZ,  NR-WRITE SYS,
   done LBL,
   SP SP $10 ADDI,  RET, ;
;package

;using
;using
;using
