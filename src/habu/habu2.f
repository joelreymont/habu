\ habu2.f — engine-builder part 2: the kernel's routines (definition writers,
\ snapshot relocation, compile-error tails), the primitive sections and kernel
\ emission. Needs habu1.f (part 1). Emission is split into phase words sharing
\ label VARIABLES (a giant single word would need dozens of locals); emission
\ order is stable so the self-rebuild reaches a fixpoint.
\
\ A does> clause gets a record of its own. LDOESPATCH patches the created
\ word's RET into `b D`, D inside the defining word's body. As a record, D is an
\ entry resolved by name like every BL; a name plus byte delta is rejected, since
\ a branch must not depend on a callee's interior. The record overlaps
\ its parent's span and trims nothing: the inliner, the snapshot and the size
\ census read that span. Its name is the parent's plus `;does`: the parent's
\ name is the only identity two engines spell alike (a `:` would make it
\ package-qualified). Its WID is the parent's, so the clause is as public as
\ its word. Its slot is &dict[NDICT+1], above the parent's pending slot; one
\ publish counts both and every abandon path leaves both uncounted.
\
\ The replay overlay: six rows typed only for package CHECKER-OVERLAY.
\ replay-open saves the scope into the REPLAY-SCOPE band and replay-close puts
\ it back, so no caller hands the engine a state to restore; the one mark a
\ caller hands, replay-widn!'s, the band bounds below. A pending definition is
\ part of the scope: replay-open moves its record out of slot NDICT, which the
\ first replay record takes, and clears PEND-CELL; replay-close puts the record
\ back in slot NDICT and PEND-CELL back. Both reject such a definition while a
\ task is live: a live task keeps the overlay's code, which would split the
\ definition's body. While the latch is set, replay-record and namespace-record
\ raise HW as they publish. The owner publishes through nothing else, nothing
\ while a definition is pending, and lowers NDICT only by its own rollback, so
\ [the saved NDICT, HW) is the overlay's and a record above HW is another
\ writer's. Every other record or wid writer dies while the latch is set, so a
\ definition pends in an open overlay only through a raw store to PEND-CELL,
\ which reaches the pending-definition rejections, as a raw ndict! reaches the
\ HW one.
require src/core/exit-status.f
require src/lib/fmt.f
require src/habu/address-cells.f
require src/compiler/fixed-body.f
require src/arch/arm64/icode.f
require src/arch/arm64/asm.f
require src/arch/arm64/mnem.f
require src/habu/crash.f
require src/habu/habu1.f
require src/habu/primitive-registry.f
require src/habu/prof-abi.f
require src/habu/prof.f
require src/habu/rt.f
HB-TARGET-LINUX-ARM64? [if]
   require src/os/linux/sys.f
[then]
HB-TARGET-MACOS? [if]
   require src/os/macos/sys.f
[then]

package ENGINE-EMIT
public
variable LTRAPH  variable LBPH  variable LBPSH  variable LBPWH
variable LOPENNL
public
variable LUNCAUGHT   variable LUNCMSG   \ uncaught-top-level-throw reporter + its fd-2 message
private
variable LUNCRPT   variable LUNCPOS   variable LUNCLOOP   variable LUNCDONE   \ reporter branch + itoa labels
: UNCMSG$ ( -- str )   s" hb: uncaught throw code " ;
: UNCMSG-LEN ( -- n )   UNCMSG$ STR:LEN ;
public

variable LFLOORREC   \ the underdepth throw for a data access below the base

variable LCOMPILEDIE   \ the die sites' tail, x0 = code: ends their line, then throws x0 inside a text, else exits x0
variable LPLACE        \ the place writer each die site calls after its class head
;package
\ Snapshot relocation labels and messages. This file compiles on a product
\ engine, where no source reopens a baked package, so the emitter's half of
\ snapshot relocation is its own package.
package RELOC-EMIT
public
variable LMARK      \ declare one DATA cell as holding a region address
variable LPTRMARK   \ declare one DATA cell as holding a DATA pointer
variable LSCRATCHMARK \ declare one DATA cell as a scratch pointer, saved as null

;package

package ENGINE-EMIT
public

variable LPLACEEND            \ ": " — what LPLACE writes after <path> or <path>:<line>
private
: PLACEEND$ ( -- str )   s" : " ;
: PLACEEND-LEN ( -- n )   PLACEEND$ STR:LEN ;

create BPH-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 58 c, 10 c,   \ habu-bp:\n
here BPH-KW - constant BPH-LEN
create BPS-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 45 c, 115 c, 116 c, 97 c, 99 c, 107 c, 58 c, 10 c,
here BPS-KW - constant BPS-LEN
create BPW-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 45 c, 119 c, 97 c, 116 c, 99 c, 104 c, 58 c, 10 c,
here BPW-KW - constant BPW-LEN
create NL-KW 10 c,
public
here NL-KW - constant NL-LEN

private
: C-TRAP-MCTX>R9 ( -- )
   HB-TARGET-LINUX-ARM64? IF 9 2 CRASH:LINUX-UC-MCTX-OFF A64ICODE:ADDI, exit THEN
   9 4 CRASH:MCTX-OFF A64ICODE:LDR, ;

: C-MCTX-PC>R10 ( -- )
   HB-TARGET-LINUX-ARM64? IF 10 9 CRASH:LINUX-MCTX-PC-OFF A64ICODE:LDR, exit THEN
   10 9 CRASH:MACOS-MCTX-PC-OFF A64ICODE:LDR, ;

: C-MCTX-X19>R12 ( -- )
   HB-TARGET-LINUX-ARM64? IF 12 9 CRASH:LINUX-MCTX-X19-OFF A64ICODE:LDR, exit THEN
   12 9 CRASH:MACOS-MCTX-X19-OFF A64ICODE:LDR, ;
;package

\ The `habu-bp-lr:` line: its label, bytes and the read of the interrupted x30
\ from the signal mcontext. The handler's own x30 is overwritten (LHEX is a BL)
\ before the first number reaches fd 2.
package BP-CALLER
public
variable LBPLH
create BPL-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 45 c, 108 c, 114 c, 58 c, 10 c,   \ habu-bp-lr:\n
here BPL-KW - constant BPL-LEN

: C-MCTX-LR>R9 ( -- )
   HB-TARGET-LINUX-ARM64? IF 9 9 CRASH:LINUX-MCTX-LR-OFF A64ICODE:LDR, exit THEN
   9 9 CRASH:MACOS-MCTX-LR-OFF A64ICODE:LDR, ;
;package

package ENGINE-EMIT
: C-BP-HIT-SAVE ( -- )
   A64ICODE:SP A64ICODE:SP 80 A64ICODE:SUBI,
   1 A64ICODE:SP 0 A64ICODE:STR,  4 A64ICODE:SP 8 A64ICODE:STR,  5 A64ICODE:SP 16 A64ICODE:STR,
   9 A64ICODE:SP 24 A64ICODE:STR,  10 A64ICODE:SP 32 A64ICODE:STR,  8 A64ICODE:SP 40 A64ICODE:STR,
   14 8 16 A64ICODE:LDR,  14 14 1 A64ICODE:ADDI,  14 8 16 A64ICODE:STR,
   15 8 24 A64ICODE:LDR,  12 15 1 A64ICODE:LSRI, ;

: C-BP-STACK-RANGE ( -- )
   14 XREG-RBASE STACK-ABI:BASE-CELL A64ICODE:LDR,
   9 A64ICODE:SP 24 A64ICODE:LDR,  C-MCTX-X19>R12
   12 A64ICODE:SP 56 A64ICODE:STR,  17 14 0 A64ICODE:ADDI, ;

: C-BP-PRINT-HIT ( -- )
   A64ICODE:LBL {: empty:label :}
   1 LBPH A64ICODE:LABEL@ A64ICODE:ADR,  0 2 A64ICODE:MOVZ,  2 BPH-LEN A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   9 A64ICODE:SP 32 A64ICODE:LDR,  CRASH:LHEX A64ICODE:LABEL@ A64ICODE:BL,
   \ An empty interrupted stack has no top cell to print.
   C-BP-STACK-RANGE
   12 17 A64ICODE:CMP, A64ASM:C-EQ empty A64ICODE:BCOND,
   9 12 8 A64ICODE:SUBI,  9 9 0 A64ICODE:LDR,  CRASH:LHEX A64ICODE:LABEL@ A64ICODE:BL,
   empty A64ICODE:LBL,
   1 BP-CALLER:LBPLH A64ICODE:LABEL@ A64ICODE:ADR,  0 2 A64ICODE:MOVZ,  2 BP-CALLER:BPL-LEN A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   9 A64ICODE:SP 24 A64ICODE:LDR,  BP-CALLER:C-MCTX-LR>R9  CRASH:LHEX A64ICODE:LABEL@ A64ICODE:BL,
   1 LBPSH A64ICODE:LABEL@ A64ICODE:ADR,  0 2 A64ICODE:MOVZ,  2 BPS-LEN A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS, ;

: C-BP-WATCH-HEAD ( -- )
   1 LBPWH A64ICODE:LABEL@ A64ICODE:ADR,  0 2 A64ICODE:MOVZ,  2 BPW-LEN A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   6 XREG-RBASE BPWN-CELL A64ICODE:LDR,  7 XREG-RBASE BPWBASE-CELL A64ICODE:LDR,
   17 0 A64ICODE:MOVZ, ;

: C-BP-WATCH-ROW ( -- )
   22 17 3 A64ICODE:LSLI,  22 7 22 A64ICODE:ADD,  23 22 0 A64ICODE:LDR,
   9 23 0 A64ICODE:ADDI,  CRASH:LHEX A64ICODE:LABEL@ A64ICODE:BL,
   9 23 0 A64ICODE:LDR,  CRASH:LHEX A64ICODE:LABEL@ A64ICODE:BL,
   17 17 1 A64ICODE:ADDI, ;

: C-BP-RESTORE-ONESHOT ( -- )
   8 A64ICODE:SP 40 A64ICODE:LDR,  11 8 0 A64ICODE:LDR,  12 8 8 A64ICODE:LDR,
   13 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  13 11 13 A64ICODE:ADD,  12 13 0 A64ICODE:STRW,
   9 11 0 A64ICODE:ADDI,  LFLUSH A64ICODE:LABEL@ A64ICODE:BL,
   8 A64ICODE:SP 40 A64ICODE:LDR,  12 0 A64ICODE:MOVZ,  12 8 0 A64ICODE:STR, ;

: C-MCTX-PC<R12 ( -- )
   HB-TARGET-LINUX-ARM64? IF 12 9 CRASH:LINUX-MCTX-PC-OFF A64ICODE:STR, exit THEN
   12 9 CRASH:MACOS-MCTX-PC-OFF A64ICODE:STR, ;

\ The slot's index is its offset in BPTAB over 32 bytes.
: C-BP-TRAMP ( -- )
   14 8 XREG-RBASE A64ICODE:SUB,  15 BPTAB-OFF A64ICODE:LIT64,  14 14 15 A64ICODE:SUB,  14 14 5 A64ICODE:LSRI,
   15 BREAKPOINT:TRAMP A64ICODE:MOVZ,  14 14 15 A64ICODE:MUL,
   12 XREG-RBASE BPTRAMP-CELL A64ICODE:LDR,  12 12 14 A64ICODE:ADD,
   9 A64ICODE:SP 24 A64ICODE:LDR,  C-MCTX-PC<R12 ;

\ A persistent slot resumes at its trampoline, a one-shot one on its entry.
: C-BP-RESUME ( -- )
   A64ICODE:LBL A64ICODE:LBL {: tramp:label done:label :}
   8 A64ICODE:SP 40 A64ICODE:LDR,  15 8 24 A64ICODE:LDR,  15 15 1 A64ICODE:ANDI,  15 tramp A64ICODE:CBNZ,
   C-BP-RESTORE-ONESHOT
   done A64ICODE:B,
   tramp A64ICODE:LBL,
   C-BP-TRAMP
   done A64ICODE:LBL, ;

: C-BP-SCAN ( label label label label -- )
   {: tno:label bscan:label bnext:label bhit:label :}
   6 BREAKPOINT:SLOTS A64ICODE:MOVZ,  7 0 A64ICODE:MOVZ,
   bscan A64ICODE:LBL,
      7 6 A64ICODE:CMP,  A64ASM:C-GE tno A64ICODE:BCOND,
      8 7 5 A64ICODE:LSLI,  14 BPTAB-OFF A64ICODE:LIT64,  8 8 14 A64ICODE:ADD,  8 XREG-RBASE 8 A64ICODE:ADD,
      13 8 0 A64ICODE:LDR,  13 bnext A64ICODE:CBZ,
      10 13 A64ICODE:CMP,  A64ASM:C-EQ bhit A64ICODE:BCOND,
      bnext A64ICODE:LBL,  7 7 1 A64ICODE:ADDI,  bscan A64ICODE:B, ;

: C-BP-STACK-DUMP ( label label -- )
   {: sdump:label sdone:label :}
   sdump A64ICODE:LBL,
      14 A64ICODE:SP 56 A64ICODE:LDR,  17 14 A64ICODE:CMP,  A64ASM:C-CS sdone A64ICODE:BCOND,
      9 17 0 A64ICODE:LDR,  17 A64ICODE:SP 48 A64ICODE:STR,  CRASH:LHEX A64ICODE:LABEL@ A64ICODE:BL,
      17 A64ICODE:SP 48 A64ICODE:LDR,  17 17 8 A64ICODE:ADDI,  sdump A64ICODE:B,
   sdone A64ICODE:LBL, ;

: C-BP-WATCH-DUMP ( label label -- )
   {: wloop:label wdone:label :}
   6 XREG-RBASE BPWN-CELL A64ICODE:LDR,  6 wdone A64ICODE:CBZ,
   7 XREG-RBASE BPWBASE-CELL A64ICODE:LDR,  7 wdone A64ICODE:CBZ,
   C-BP-WATCH-HEAD
   wloop A64ICODE:LBL,
      17 6 A64ICODE:CMP,  A64ASM:C-GE wdone A64ICODE:BCOND,
      C-BP-WATCH-ROW  wloop A64ICODE:B,
   wdone A64ICODE:LBL, ;

\ LTRAPH, the trap signal entry: a BPTAB hit past its skip count is reported,
\ and every hit is resumed; any other trap falls through to the crash dump with
\ x2/x4 untouched.
: EMIT-TRAPH ( -- )
   LTRAPH A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL {: tno :}
   C-TRAP-MCTX>R9
   C-MCTX-PC>R10
   A64ICODE:LBL {: bscan :}  A64ICODE:LBL {: bnext :}  A64ICODE:LBL {: bhit :}
   A64ICODE:LBL {: sdump :}  A64ICODE:LBL {: sdone :}  A64ICODE:LBL {: wloop :}  A64ICODE:LBL {: wdone :}
   A64ICODE:LBL {: resume :}
   tno bscan bnext bhit C-BP-SCAN
   \ slot layout: +0 addr  +8 saved-instr  +16 hits  +24 ctrl(skip<<1 | persist)
   bhit A64ICODE:LBL,                                         \ x8=&slot x9=mctx x10=pc
   C-BP-HIT-SAVE                                     \ x15=ctrl  x12=skip
   14 12 A64ICODE:CMP,  A64ASM:C-LS resume A64ICODE:BCOND,                    \ hits <= skip -> silent
   C-BP-PRINT-HIT
   C-BP-STACK-RANGE
   sdump sdone C-BP-STACK-DUMP
   wloop wdone C-BP-WATCH-DUMP
   resume A64ICODE:LBL,
   C-BP-RESUME
   0 A64ICODE:SP 8 A64ICODE:LDR,  1 A64ICODE:SP 0 A64ICODE:LDR,  2 A64ICODE:SP 16 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 80 A64ICODE:ADDI,
   SYSCALL:NR-SIGRETURN SYSCALL:SYS,                                 \ sigreturn(uctx, infostyle, token)
   tno A64ICODE:LBL,
   CRASH:LCRASHH A64ICODE:LABEL@ A64ICODE:B,
   LBPH A64ICODE:LABEL@ A64ICODE:LBL,  BPH-KW BPH-LEN >str A64ICODE:BYTES,
   BP-CALLER:LBPLH A64ICODE:LABEL@ A64ICODE:LBL, BP-CALLER:BPL-KW BP-CALLER:BPL-LEN >str A64ICODE:BYTES,
   LBPSH A64ICODE:LABEL@ A64ICODE:LBL, BPS-KW BPS-LEN >str A64ICODE:BYTES,
   LBPWH A64ICODE:LABEL@ A64ICODE:LBL, BPW-KW BPW-LEN >str A64ICODE:BYTES,
   LOPENNL A64ICODE:LABEL@ A64ICODE:LBL, NL-KW NL-LEN >str A64ICODE:BYTES,
   LUNCMSG A64ICODE:LABEL@ A64ICODE:LBL, UNCMSG$ A64ICODE:BYTES,                  \ LUNCAUGHT appends the signed code + newline
   LPLACEEND A64ICODE:LABEL@ A64ICODE:LBL, PLACEEND$ A64ICODE:BYTES, ;

: C-TARGET-UNKNOWN ( -- )
   s" hb: unknown target" EXIT-STATUS:INTERNAL die ;

private

\ (GENIO-OUT): the device half of the output funnel, reached when the task's
\ output device index is non-zero (x0 = index, x1 = span address, x2 = length).
\ It is an engine helper: prim bodies reach it by direct BL. ACTIVE-CELL holds
\ the device during the call, so a write shared by several devices finds its
\ own; the caller's value is restored from the frame. It falls back to write(1)
\ when the re-entrancy guard is set, the index is outside 1..DEVICES or the row
\ is empty: output keeps working while a device is built or gone. A device
\ reports a failure by latching it for its next checked operation; a throw out
\ of its write is caught with the device state its catch saved.
: EMIT-GENIO-OUT ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: term:label done:label end:label :}
   s" (GENIO-OUT)" RT:LGENIOOUT A64ICODE:LABEL@ LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   RT:LGENIOOUT A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP $10 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,
   9 XREG-RBASE GENIO-ABI:BUSY-CELL A64ICODE:LDR,  9 term A64ICODE:CBNZ,
   0 GENIO-ABI:DEVICES A64ICODE:CMPI,  A64ASM:C-HI term A64ICODE:BCOND,          \ index outside 1..DEVICES
   9 GENIO-ABI:WRITE-OFF A64ICODE:LIT64,  9 XREG-RBASE 9 A64ICODE:ADD,
   10 0 0 A64ICODE:ADDI,  10 10 1 A64ICODE:SUBI,  10 10 3 A64ICODE:LSLI,  9 9 10 A64ICODE:ADD,
   9 9 0 A64ICODE:LDR,  9 term A64ICODE:CBZ,                              \ row never registered
   10 XREG-RBASE GENIO-ABI:ACTIVE-CELL A64ICODE:LDR,  10 A64ICODE:SP 8 A64ICODE:STR,     \ save the caller's active device
   0 XREG-RBASE GENIO-ABI:ACTIVE-CELL A64ICODE:STR,                    \ the write runs for this one
   10 1 A64ICODE:MOVZ,  10 XREG-RBASE GENIO-ABI:BUSY-CELL A64ICODE:STR,
   1 RT:G-PUSH  2 RT:G-PUSH
   9 A64ICODE:BLR,
   10 0 A64ICODE:MOVZ,  10 XREG-RBASE GENIO-ABI:BUSY-CELL A64ICODE:STR,
   10 A64ICODE:SP 8 A64ICODE:LDR,  10 XREG-RBASE GENIO-ABI:ACTIVE-CELL A64ICODE:STR,
   done A64ICODE:B,
   term A64ICODE:LBL,
   0 1 A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   done A64ICODE:LBL,
   30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP $10 A64ICODE:ADDI,
   A64ICODE:RET,
   end A64ICODE:LBL, ;

;package

package ENGINE-EMIT
private
: RECWPUB$ ( -- str )      s" rec-wide-publish" ;
;package

\ The baked `trust-raw` and `trust-decl` strings a missing checker word names.
package KWDATA

public

variable LKWTRUSTRAW
: TRUSTRAW$ ( -- str )   s" trust-raw" ;

variable LKWTRUSTDECL
: TRUSTDECL$ ( -- str )   s" trust-decl" ;

: EMIT ( -- )
   LKWTRUSTDECL A64ICODE:LABEL@ A64ICODE:LBL, TRUSTDECL$ A64ICODE:BYTES,      LKWTRUSTRAW A64ICODE:LABEL@ A64ICODE:LBL, TRUSTRAW$ A64ICODE:BYTES, ;

;package

package ENGINE-EMIT

public

: C-EXIT-LABEL ( ^n n -- ) {: msg:^ len:n :}
   0 2 A64ICODE:MOVZ,  1 msg A64ICODE:LABEL@ A64ICODE:ADR,  2 len A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   0 2 A64ICODE:MOVZ,  1 LOPENNL A64ICODE:LABEL@ A64ICODE:ADR,  2 NL-LEN A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   0 EXIT-STATUS:INTERNAL A64ICODE:MOVZ,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS, ;

\ Push the pending definition's name, the body text's first token (DEF-TKA
\ is stale once a multi-line body refills the reader); x11 is left alone.
\ The token is PKG:tail, so the checker's row is ( pkg, tail ) PUBLIC.
: C-PUSH-DREC-NAME ( -- )
   A64ICODE:LBL A64ICODE:LBL {: scan:label done:label :}
   9 XREG-RBASE BODY-TEXT:BASE-CELL A64ICODE:LDR,      \ x9 = name start (body text base)
   10 0 A64ICODE:MOVZ,                           \ x10 = name length
   12 XREG-RBASE BODYLEN-CELL A64ICODE:LDR,            \ x12 = body length bound (fail-closed)
   scan A64ICODE:LBL,
      10 12 A64ICODE:CMP,  A64ASM:C-GE done A64ICODE:BCOND,
      13 9 10 A64ICODE:ADD,  13 13 0 A64ICODE:LDRB,       \ x13 = name[x10]
      13 $20 A64ICODE:CMPI,  A64ASM:C-EQ done A64ICODE:BCOND,    \ the seeded trailing space ends the name
      13 done A64ICODE:CBZ,
      10 10 1 A64ICODE:ADDI,  scan A64ICODE:B,
   done A64ICODE:LBL,
   9 RT:G-PUSH
   10 RT:G-PUSH ;

private
: C-PUSH-DATA-CELL ( n -- ) {: off :}
   9 XREG-RBASE off A64ICODE:LDR,  9 RT:G-PUSH ;

public
: C-PUSH-TRUST-SIG ( n n -- ) {: aoff uoff :}
   aoff C-PUSH-DATA-CELL
   uoff C-PUSH-DATA-CELL ;

: C-CALL-X11-SAVED ( -- )
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,
   11 A64ICODE:BLR,
   30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI, ;

;package

package DECL-OWNER
public

: SKIP-SAME ( n label -- ) {: off:n same:label :}
   A64ICODE:LBL {: different:label :}
   12 XREG-RBASE NCOMP-DISPATCH:DECL-CELL A64ICODE:LDR,  12 different A64ICODE:CBZ,
   12 12 off A64ICODE:LDR,  11 12 A64ICODE:CMP,  A64ASM:C-EQ same A64ICODE:BCOND,
   different A64ICODE:LBL, ;

;package

package DEF-TRUST

public

\ Resolve `trust-decl` into x11. A missing registrar names itself on fd 2 and
\ exits EXIT-STATUS:INTERNAL rather than publishing a definition whose effect nothing recorded.
\ Public because BDRAINPRETRUST needs the XT for its own push sequence.
: FIND ( -- )
   A64ICODE:LBL A64ICODE:LBL {: absent:label ready:label :}
   NCOMP-DISPATCH:DECL-EFFECT-OFF absent DECL-OWNER:TARGET
   ready A64ICODE:B,
   absent A64ICODE:LBL,  KWDATA:LKWTRUSTDECL KWDATA:TRUSTDECL$ STR:LEN ENGINE-EMIT:C-EXIT-LABEL
   ready A64ICODE:LBL, ;

;package

package LASTC-TRUST

\ Resolve `trust-raw`. A missing registrar names itself on fd 2 and exits EXIT-STATUS:INTERNAL.
: FIND-RAW ( -- )
   A64ICODE:LBL A64ICODE:LBL {: absent:label ready:label :}
   NCOMP-DISPATCH:DECL-RAW-OFF absent DECL-OWNER:TARGET
   ready A64ICODE:B,
   absent A64ICODE:LBL,  KWDATA:LKWTRUSTRAW KWDATA:TRUSTRAW$ STR:LEN ENGINE-EMIT:C-EXIT-LABEL
   ready A64ICODE:LBL, ;

public

: FIND-ACTIVE ( label -- ) {: absent:label :}
   A64ICODE:LBL {: ready:label :}
   NCOMP-DISPATCH:DECL-RAW-OFF DECL-OWNER:FIND  11 ready A64ICODE:CBNZ,
   9 XREG-RBASE HOOK-CELL A64ICODE:LDR,  9 absent A64ICODE:CBZ,
   FIND-RAW
   ready A64ICODE:LBL, ;

: FIND-TARGET ( label -- ) {: absent:label :}
   11 XREG-RBASE NCOMP-DISPATCH:DECL-CELL A64ICODE:LDR,  11 absent A64ICODE:CBZ,
   NCOMP-DISPATCH:DECL-RAW-OFF absent DECL-OWNER:TARGET
   NCOMP-DISPATCH:DECL-RAW-OFF absent DECL-OWNER:SKIP-SAME ;

: PUBLISH ( -- )
   A64ICODE:LBL {: nohook:label :}
   nohook FIND-ACTIVE
   ENGINE-EMIT:C-PUSH-DREC-NAME
   CRSIG-A-CELL CRSIG-U-CELL ENGINE-EMIT:C-PUSH-TRUST-SIG
   ENGINE-EMIT:C-CALL-X11-SAVED
   nohook FIND-TARGET
   ENGINE-EMIT:C-PUSH-DREC-NAME
   CRSIG-A-CELL CRSIG-U-CELL ENGINE-EMIT:C-PUSH-TRUST-SIG
   ENGINE-EMIT:C-CALL-X11-SAVED
   nohook A64ICODE:LBL, ;

;package

package ENGINE-EMIT

public
\ x11 = the operation at OFF in the source owner's record. An owner whose record
\ ends before OFF (its byte size at [record-8]) or holds 0 there writes the line
\ text to fd 2 and exits EXIT-STATUS:INTERNAL.
: C-OWNER-FIELD ( n str -- ) {: off:n text:str :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: absent:label msg:label ready:label :}
   11 XREG-RBASE NCOMP-DISPATCH:DECL-CELL A64ICODE:LDR,  11 absent A64ICODE:CBZ,
   10 11 8 A64ICODE:SUBI,  10 10 0 A64ICODE:LDR,
   10 off CELL + A64ICODE:CMPI,  A64ASM:C-LT absent A64ICODE:BCOND,
   11 11 off A64ICODE:LDR,  11 ready A64ICODE:CBNZ,
   absent A64ICODE:LBL,
      0 2 A64ICODE:MOVZ,  1 msg A64ICODE:ADR,  2 text STR:LEN A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
      0 EXIT-STATUS:INTERNAL A64ICODE:MOVZ,  SYSCALL:NR-EXIT-GROUP SYSCALL:SYS,
   msg A64ICODE:LBL,  text A64ICODE:BYTES,
   ready A64ICODE:LBL, ;

\ After ndict++ the checker's rec-wide-publish consumes the RECW latch for this
\ definition's effect and, when wide, marks record ndict-1 DNAME-WIDE. The
\ call goes through the source owner's record, so the tail runs once checker.f
\ has stored it (DECL-CELL), not on the checking switch: a linker-built hb has
\ no HOOK-CELL, and its does> runtime publishes here. The engine prefix
\ self-load before checker.f has no owner.
: EM-REC-WIDE-PUBLISH ( -- )
   A64ICODE:LBL {: nohook:label :}
   9 XREG-RBASE NCOMP-DISPATCH:DECL-CELL A64ICODE:LDR,  9 nohook A64ICODE:CBZ,
   NCOMP-DISPATCH:DECL-REC-WIDE-PUBLISH-OFF RECWPUB$ C-OWNER-FIELD
   C-CALL-X11-SAVED
   nohook A64ICODE:LBL, ;

public

: C-RUNTIME-CRSIG-CLEAR ( -- )
   9 0 A64ICODE:MOVZ,
   9 XREG-RBASE CRSIG-A-CELL A64ICODE:STR,
   9 XREG-RBASE CRSIG-U-CELL A64ICODE:STR, ;

;package

\ The does>-patch runtime routine emitter (LDOESPATCH): patches the created
\ word's RET into a branch and publishes the declared runtime effect. It clears
\ the created record's definer kind beside the patch: `create` stamped
\ DKIND:ADDR because its body pushed the data address, and a reader folding the
\ stale stamp would compile that address in place of a call. A zero clause entry
\ restores the bare body. A fresh created word keeps its RET and stamp; one an
\ earlier clause patched gets its RET back but keeps the cleared kind (LASTC may
\ name a constant, so the bare body is no evidence of a DATA push). The publish
\ tail is the colon tail: after LASTC-TRUST:PUBLISH, EM-REC-WIDE-PUBLISH drains
\ the checker's latch, so a wide clause effect marks the created record
\ (ndict-1, as `create` counted it) DNAME-WIDE.
package DOESPATCH

: NOTIFY-PUBLICATION ( -- )
   A64ICODE:LBL {: done:label :}
   11 XREG-RBASE NCOMP-DISPATCH:DECL-CELL A64ICODE:LDR,  11 done A64ICODE:CBZ,
   12 11 CHECKER-OWNER-ABI:HEADER-BYTES A64ICODE:SUBI,
   13 12 0 A64ICODE:LDR,  14 CHECKER-OWNER-ABI:MAGIC A64ICODE:LIT64,
   13 14 A64ICODE:CMP,  A64ASM:C-NE done A64ICODE:BCOND,
   13 12 8 A64ICODE:LDR,
   14 CHECKER-OWNER-EXT:PUBLICATION-USE-OFF CELL + A64ICODE:LIT64,
   13 14 A64ICODE:CMP,  A64ASM:C-LT done A64ICODE:BCOND,
   11 11 CHECKER-OWNER-EXT:PUBLICATION-USE-OFF A64ICODE:LDR,  11 done A64ICODE:CBZ,
   9 A64ICODE:SP 40 A64ICODE:LDR,  9 9 DBASE A64ICODE:SUB,
   12 DREC A64ICODE:MOVZ,  9 9 12 A64ICODE:UDIV,
   9 RT:G-PUSH  ENGINE-EMIT:C-CALL-X11-SAVED
   done A64ICODE:LBL, ;

\ Clear the kind of the patched record x11 and of every record that shares its
\ body. The body is a clause now, not a push or cell definer, so no record whose
\ code pointer [0] is that body may keep a stamp: an `EXPORT` alias copies its
\ target's code pointer and DKIND:VAL, and a stale VAL folds the alias into the
\ value it pushed when it was folded. An alias is made of an existing word, so
\ every one is newer than its target, and the scan walks only the records above
\ x11, in every wordlist. A record with no stamp has no alias with one, so it
\ scans nothing. Each match is rewritten through the alias. Clobbers x0, x5, x10
\ and x13-x15.
: CLEAR-KIND ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: scan:label next:label done:label :}
   5 DKIND:MASK DNAME-CELL or -1 xor A64ICODE:LIT64,
   13 11 16 A64ICODE:LDR,  14 DKIND:MASK A64ICODE:LIT64,  14 13 14 A64ICODE:AND,
   13 13 5 A64ICODE:AND,
   0 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  0 11 0 A64ICODE:ADD,  13 0 16 A64ICODE:STR,
   14 done A64ICODE:CBZ,
   13 11 0 A64ICODE:LDR,                                          \ x13 = the patched body
   14 11 DREC A64ICODE:ADDI,                                      \ x14 = the record above it
   15 XREG-RBASE NDICT-HW-CELL A64ICODE:LDR,  15 NDICT A64ICODE:CMP,  15 15 NDICT A64ASM:C-HI A64ICODE:CSEL,
   10 DREC A64ICODE:MOVZ,  15 15 10 A64ICODE:MUL,  15 DBASE 15 A64ICODE:ADD,       \ x15 = the end of every record ever published
   scan A64ICODE:LBL,
   14 15 A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
   10 14 0 A64ICODE:LDR,  10 13 A64ICODE:CMP,  A64ASM:C-NE next A64ICODE:BCOND,
      0 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  0 14 0 A64ICODE:ADD,                \ x0 = the record through the alias
      10 14 16 A64ICODE:LDR,  10 10 5 A64ICODE:AND,  10 0 16 A64ICODE:STR,
   next A64ICODE:LBL,
   14 14 DREC A64ICODE:ADDI,  scan A64ICODE:B,
   done A64ICODE:LBL, ;

public

\ Native defining words pass the measured clause entry and the already
\ parsed created-word signature.
: PRIM ( -- )
   ENGINE-EMIT:C RT:G-POP  ENGINE-EMIT:B RT:G-POP  ENGINE-EMIT:A RT:G-POP
   ENGINE-EMIT:B XREG-RBASE CRSIG-A-CELL A64ICODE:STR,  ENGINE-EMIT:C XREG-RBASE CRSIG-U-CELL A64ICODE:STR,
   ENGINE-EMIT:B ENGINE-EMIT:A 0 A64ICODE:ADDI,  ENGINE-EMIT:LDOESPATCH A64ICODE:LABEL@ A64ICODE:BL, ;

private

: NONE-CLASS$ ( -- str )
   s" E-DOES-UNCREATED habu: " ;

: NONE$ ( -- str )
   s" does> has no created word" ;

\ A LASTC of zero names no record: none was created since boot, or the one it
\ named was retired by a motion that lowered NDICT. A LASTC whose wordlist cell
\ is WL:RETIRED names a record `undefine` retired in place, which leaves NDICT
\ alone. Either is rejected before anything is patched, recoverable inside
\ evaluate like the other definition errors.
: EMIT-NONE ( label -- ) {: have:label :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: none:label class:label msg:label :}
   11 XREG-RBASE LASTC-CELL A64ICODE:LDR,  11 none A64ICODE:CBZ,
   12 11 40 A64ICODE:LDR,  13 WL:RETIRED invert A64ICODE:MOVN,  12 13 A64ICODE:CMP,  A64ASM:C-NE have A64ICODE:BCOND,
   none A64ICODE:LBL,
   0 2 A64ICODE:MOVZ,  1 class A64ICODE:ADR,  2 NONE-CLASS$ STR:LEN A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   ENGINE-EMIT:LPLACE A64ICODE:LABEL@ A64ICODE:BL,
   0 2 A64ICODE:MOVZ,  1 msg A64ICODE:ADR,  2 NONE$ STR:LEN A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   0 70 A64ICODE:MOVZ,  ENGINE-EMIT:LCOMPILEDIE A64ICODE:LABEL@ A64ICODE:B,
   class A64ICODE:LBL,  NONE-CLASS$ A64ICODE:BYTES,
   msg A64ICODE:LBL,  NONE$ A64ICODE:BYTES, ;

public

: EMIT ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
   {: nocr:label declared:label write:label flush:label
      have:label before:label :}
   ENGINE-EMIT:LDOESPATCH A64ICODE:LABEL@ A64ICODE:LBL,
   have EMIT-NONE
   have A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP 48 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,  10 A64ICODE:SP 8 A64ICODE:STR,
   11 XREG-RBASE LASTC-CELL A64ICODE:LDR,                               \ created slot
   11 A64ICODE:SP 40 A64ICODE:STR,
   12 11 0 A64ICODE:LDR,  13 11 8 A64ICODE:LDR,
   13 13 4 A64ICODE:SUBI,
   12 12 13 A64ICODE:ADD,                                         \ the final RET/B slot
   12 A64ICODE:SP 16 A64ICODE:STR,
   9 12 0 A64ICODE:ADDI,
   10 before A64ICODE:CBNZ,
   14 12 0 A64ICODE:LDRW,  5 ENGINE-EMIT:W-RET A64ICODE:LIT64,  14 5 A64ICODE:CMP,  A64ASM:C-NE before A64ICODE:BCOND,
   9 0 A64ICODE:MOVZ,
   before A64ICODE:LBL,
   9 A64ICODE:SP 24 A64ICODE:STR,
   10 11 0 A64ICODE:ADDI,  EM-DEF-OCC:LBEFORE A64ICODE:LABEL@ A64ICODE:BL,
   10 A64ICODE:SP 8 A64ICODE:LDR,  9 A64ICODE:SP 24 A64ICODE:LDR,
   9 declared A64ICODE:CBZ,
   10 A64ICODE:SP 8 A64ICODE:LDR,  12 A64ICODE:SP 16 A64ICODE:LDR,  11 XREG-RBASE LASTC-CELL A64ICODE:LDR,
   14 ENGINE-EMIT:W-RET A64ICODE:LIT64,  10 write A64ICODE:CBZ,                       \ an empty replacement cancels the previous clause
   14 10 12 A64ICODE:SUB,  14 14 2 A64ICODE:ASRI,                          \ delta words (negative)
   5 $3FFFFFF A64ICODE:LIT64,  14 14 5 A64ICODE:AND,
   5 $14000000 A64ICODE:LIT64,  14 14 5 A64ICODE:ORR,                      \ b D
   write A64ICODE:LBL,
   13 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  13 12 13 A64ICODE:ADD,  14 13 0 A64ICODE:STRW,
   10 flush A64ICODE:CBZ,
   CLEAR-KIND
   flush A64ICODE:LBL,
   12 A64ICODE:SP 16 A64ICODE:LDR,
   12 A64ICODE:DCCVAU,  A64ICODE:DSB-ISH,  12 A64ICODE:ICIVAU,  A64ICODE:DSB-ISH,  A64ICODE:ISB,
   declared A64ICODE:LBL,
   \ The patched body is now executable. Reissue its occurrence before the
   \ checker publishes the created word's contract through trust-raw.
   9 A64ICODE:SP 24 A64ICODE:LDR,  10 XREG-RBASE LASTC-CELL A64ICODE:LDR,  EM-DEF-OCC:LAFTER A64ICODE:LABEL@ A64ICODE:BL,
   A64ICODE:LBL {: noshadow:label :}
   11 XREG-RBASE NCOMP-DISPATCH:DOES-SHADOW-CELL A64ICODE:LDR,  11 noshadow A64ICODE:CBZ,
   9 XREG-RBASE LASTC-CELL A64ICODE:LDR,  9 9 DBASE A64ICODE:SUB,
   10 DREC A64ICODE:MOVZ,  9 9 10 A64ICODE:UDIV,  9 RT:G-PUSH
   9 A64ICODE:SP 8 A64ICODE:LDR,  9 RT:G-PUSH
   11 XREG-RBASE NCOMP-DISPATCH:DOES-SHADOW-CELL A64ICODE:LDR,  ENGINE-EMIT:C-CALL-X11-SAVED
   noshadow A64ICODE:LBL,
   9 XREG-RBASE CRSIG-U-CELL A64ICODE:LDR,  9 nocr A64ICODE:CBZ,
      LASTC-TRUST:PUBLISH
      \ A second does> may replace an existing clause. Its new effect replaces
      \ the old wide mark, so a scalar result clears it.
      9 XREG-RBASE LASTC-CELL A64ICODE:LDR,
      12 9 16 A64ICODE:LDR,  10 DNAME-WIDE -1 xor A64ICODE:LIT64,
      11 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  11 9 11 A64ICODE:ADD,              \ x11 = the record through the alias
      12 12 10 A64ICODE:AND,  12 11 16 A64ICODE:STR,
      ENGINE-EMIT:EM-REC-WIDE-PUBLISH
      ENGINE-EMIT:C-RUNTIME-CRSIG-CLEAR
   nocr A64ICODE:LBL,
   \ The source observer sees the final effect.
   NOTIFY-PUBLICATION
   30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 48 A64ICODE:ADDI,  A64ICODE:RET, ;

;package

\ A record's name is stored once for every definer.
package DEFWRITE

public

\ The comparisons are unsigned and never wrap: a CP already at the ceiling, and
\ a length no region holds (a negative one among them), are rejected rather than
\ added. x12 = the length. Clobbers x11, x15 and the flags.
: NAME-ROOM ( label -- ) {: full:label :}
   A64ICODE:LBL {: fits:label :}
   12 DNAME-INL A64ICODE:CMPI,  A64ASM:C-LS fits A64ICODE:BCOND,
   11 CODE-CEILING A64ICODE:LIT64,  11 DBASE 11 A64ICODE:ADD,
   CP 11 A64ICODE:CMP,  A64ASM:C-CS full A64ICODE:BCOND,                       \ CP is already at the ceiling
   11 11 CP A64ICODE:SUB,                                      \ x11 = the room below it
   12 11 A64ICODE:CMP,  A64ASM:C-CS full A64ICODE:BCOND,                       \ the bytes alone reach it
   15 12 3 A64ICODE:ADDI,  15 15 2 A64ICODE:LSRI,  15 15 2 A64ICODE:LSLI,
   15 11 A64ICODE:CMP,  A64ASM:C-CS full A64ICODE:BCOND,                       \ so do the padded bytes
   fits A64ICODE:LBL, ;

\ Store record x9's name, x12 >= 1 bytes at x10, in room NAME-ROOM admitted.
\ Clobbers x10 x11 x13-x16.
: NAME-COPY ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: short lcopy lcd scopy scd done :}
   11 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  11 9 11 A64ICODE:ADD,
   14 0 A64ICODE:MOVZ,  14 11 24 A64ICODE:STR,  14 11 32 A64ICODE:STR,           \ canonical name/padding before either form
   13 12 0 A64ICODE:ADDI,
   12 DNAME-INL A64ICODE:CMPI,  A64ASM:C-LS short A64ICODE:BCOND,
      14 DNAME-EXT A64ICODE:LIT64,  13 13 14 A64ICODE:ORR,  13 11 16 A64ICODE:STR,
      15 12 3 A64ICODE:ADDI,  15 15 2 A64ICODE:LSRI,  15 15 2 A64ICODE:LSLI,
      16 CP 15 A64ICODE:ADD,
      CP 11 24 A64ICODE:STR,
      11 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  11 CP 11 A64ICODE:ADD,
      14 12 0 A64ICODE:ADDI,
      lcopy A64ICODE:LBL,  14 lcd A64ICODE:CBZ,
         15 10 0 A64ICODE:LDRB,  15 11 0 A64ICODE:STRB,
         10 10 1 A64ICODE:ADDI,  11 11 1 A64ICODE:ADDI,  14 14 1 A64ICODE:SUBI,  lcopy A64ICODE:B,
      lcd A64ICODE:LBL,
      CP 16 0 A64ICODE:ADDI,
      done A64ICODE:B,
   short A64ICODE:LBL,
      13 11 16 A64ICODE:STR,
      11 11 24 A64ICODE:ADDI,  14 12 0 A64ICODE:ADDI,
      scopy A64ICODE:LBL,  14 scd A64ICODE:CBZ,
         15 10 0 A64ICODE:LDRB,  15 11 0 A64ICODE:STRB,
         10 10 1 A64ICODE:ADDI,  11 11 1 A64ICODE:ADDI,  14 14 1 A64ICODE:SUBI,  scopy A64ICODE:B,
      scd A64ICODE:LBL,
   done A64ICODE:LBL, ;

;package

package DOES-REC

\ SUF-LEN and COPY-NAME's five byte stores spell src/core/does-clause.f
\ DOES-CLAUSE:SUFFIX$; this emitter runs before that word exists, so the two
\ change together.
5 constant SUF-LEN                                     \ ";does"

public

\ x11 = the parent's pending record, x13 = the clause name's length, x14 = the
\ parent's name bytes, x15 = the clause name's length rounded up to a word.
: NAME$ ( -- )
   A64ICODE:LBL {: inl:label :}
   11 XREG-RBASE PEND-CELL A64ICODE:LDR,
   12 11 16 A64ICODE:LDR,
   13 12 0 A64ICODE:ADDI,  13 13 DNAME-FLAG-BITS A64ICODE:LSLI,  13 13 DNAME-FLAG-BITS A64ICODE:LSRI,       \ the parent's own name length
   14 11 24 A64ICODE:ADDI,
   12 12 DNAME-EXT A64ICODE:ANDI,  12 inl A64ICODE:CBZ,
      14 11 24 A64ICODE:LDR,                                    \ ... which is a pointer when it is long
   inl A64ICODE:LBL,
   13 13 SUF-LEN A64ICODE:ADDI,
   15 13 3 A64ICODE:ADDI,  15 15 2 A64ICODE:LSRI,  15 15 2 A64ICODE:LSLI, ;

\ Copy the derived name to x10, padded to x15 bytes. NAME$ has already supplied
\ x11..x15 and the caller owns the padded span.
: COPY-NAME ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: cpy:label cpd:label pad:label pend:label :}
   12 13 SUF-LEN A64ICODE:SUBI,
   cpy A64ICODE:LBL,  12 cpd A64ICODE:CBZ,
      9 14 0 A64ICODE:LDRB,  9 10 0 A64ICODE:STRB,
      14 14 1 A64ICODE:ADDI,  10 10 1 A64ICODE:ADDI,  12 12 1 A64ICODE:SUBI,  cpy A64ICODE:B,
   cpd A64ICODE:LBL,
   9 $3B A64ICODE:MOVZ,  9 10 0 A64ICODE:STRB,  10 10 1 A64ICODE:ADDI,            \ ';'
   9 $64 A64ICODE:MOVZ,  9 10 0 A64ICODE:STRB,  10 10 1 A64ICODE:ADDI,            \ 'd'
   9 $6F A64ICODE:MOVZ,  9 10 0 A64ICODE:STRB,  10 10 1 A64ICODE:ADDI,            \ 'o'
   9 $65 A64ICODE:MOVZ,  9 10 0 A64ICODE:STRB,  10 10 1 A64ICODE:ADDI,            \ 'e'
   9 $73 A64ICODE:MOVZ,  9 10 0 A64ICODE:STRB,  10 10 1 A64ICODE:ADDI,            \ 's'
   12 15 13 A64ICODE:SUB,  9 0 A64ICODE:MOVZ,
   pad A64ICODE:LBL,  12 pend A64ICODE:CBZ,
      9 10 0 A64ICODE:STRB,  10 10 1 A64ICODE:ADDI,  12 12 1 A64ICODE:SUBI,  pad A64ICODE:B,
   pend A64ICODE:LBL, ;

\ Write the clause record in the existing pending+1 slot; entry and len name
\ the registers that hold the clause entry and its recorded body length.
: RECORD ( n n -- )
   {: entry:n len:n :}
   14 NDICT 1 A64ICODE:ADDI,  9 DREC A64ICODE:MOVZ,  14 14 9 A64ICODE:MUL,  14 DBASE 14 A64ICODE:ADD,
   9 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  14 14 9 A64ICODE:ADD,
   9 0 A64ICODE:MOVZ,                                           \ a slot an abandoned definition may have written
   9 14 0 A64ICODE:STR,  9 14 8 A64ICODE:STR,  9 14 16 A64ICODE:STR,
   9 14 24 A64ICODE:STR,  9 14 32 A64ICODE:STR,  9 14 40 A64ICODE:STR,
   entry 14 0 A64ICODE:STR,                                     \ [0] = the clause entry
   len 14 8 A64ICODE:STR,                                       \ [8] = its recorded body length
   9 DNAME-EXT A64ICODE:LIT64,  9 13 9 A64ICODE:ORR,  9 14 16 A64ICODE:STR,       \ [16] = its name length, out of line
   9 CP 0 A64ICODE:ADDI,  9 14 24 A64ICODE:STR,                          \ [24] = the name bytes
   11 XREG-RBASE PEND-CELL A64ICODE:LDR,  9 11 40 A64ICODE:LDR,  9 14 40 A64ICODE:STR, ; \ [40] = the parent's wordlist

\ Native publication has already emitted the clause. Append only the permanent
\ derived name and record, using the measured entry and length it supplies.
: NATIVE-PRIM ( -- )
   ENGINE-EMIT:B RT:G-POP  ENGINE-EMIT:A RT:G-POP
   5 ENGINE-EMIT:A 0 A64ICODE:ADDI,  6 ENGINE-EMIT:B 0 A64ICODE:ADDI,
   NAME$
   10 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  10 CP 10 A64ICODE:ADD,  COPY-NAME
   5 6 RECORD
   9 1 A64ICODE:MOVZ,  9 XREG-RBASE CLAUSE-CELL A64ICODE:STR,                  \ an imported definer had no `does>`
   CP CP 15 A64ICODE:ADD, ;

;package

package ENGINE-EMIT
public
: C-CLEAR-TRUSTED-STATE ( -- )
   9 0 A64ICODE:MOVZ,
   9 XREG-RBASE NCOMP-DISPATCH:COMP-LATCH-CELL A64ICODE:STR,
   9 XREG-RBASE TSIG-A-CELL A64ICODE:STR,   9 XREG-RBASE TSIG-U-CELL A64ICODE:STR,
   9 XREG-RBASE TCSIG-A-CELL A64ICODE:STR,  9 XREG-RBASE TCSIG-U-CELL A64ICODE:STR,
   9 XREG-RBASE DOESB-CELL A64ICODE:STR,  9 XREG-RBASE CLAUSE-CELL A64ICODE:STR,
   9 XREG-RBASE TRUSTED-CELL A64ICODE:STR, ;

;package

\ The bodies of the definition and replay writers: each checks before it writes
\ a row, so no reader sees a half-written one. FPRIM frames x30 around each.
package DEFWRITE

\ The rejection at the label: the text and the primitive's lr, which BLs in the
\ body replace and the frame keeps.
: REJECT-AT ( str label n -- ) {: text:str at:label rc:n :}
   at A64ICODE:LBL,  0 CRASH:CALLER-LR,  text rc CRASH:DIE-LINE, ;

\ x<dst> = the address of record x<ix>; dst and ix differ.
: REC-AT, ( n n -- ) {: dst:n ix:n :}
   dst DREC A64ICODE:MOVZ,  dst ix dst A64ICODE:MUL,  dst DBASE dst A64ICODE:ADD, ;

\ A pending definition's record is slot NDICT, the slot a record writer fills.
: NOT-PENDING, ( label -- ) {: bad:label :}
   9 XREG-RBASE PEND-CELL A64ICODE:LDR,  9 bad A64ICODE:CBNZ, ;

\ Slot DICT-CAP is the control-flow band, not a record.
: DICT-ROOM, ( label -- ) {: bad:label :}
   9 DICT-CAP A64ICODE:LIT64,  NDICT 9 A64ICODE:CMP,  A64ASM:C-CS bad A64ICODE:BCOND, ;

\ An empty name, and one whose copy would reach the code ceiling. x1 = the
\ length; x12 takes a copy for NAME-ROOM.
: NAME-SIZE, ( label -- ) {: bad:label :}
   1 bad A64ICODE:CBZ,
   12 1 0 A64ICODE:ADDI,  bad NAME-ROOM ;

\ A live record for the folded name x0/x1 in wordlist x2. WLFIND keeps x0, x1,
\ x2 and x13, and clobbers x3-x12 and x14-x17.
: FRESH, ( label -- ) {: bad:label :}
   WLFIND:LENTRY A64ICODE:LABEL@ A64ICODE:BL,  12 bad A64ICODE:CBNZ, ;

\ Wid x2 is one of wl.f's WL markers, not a wordlist: every wordlist id
\ is at least 0.
: REAL-WID, ( label -- ) {: bad:label :}
   2 0 A64ICODE:CMPI,  A64ASM:C-LT bad A64ICODE:BCOND, ;

\ After the seal, wid x2 is protected when it is an OWNER-API wordlist.
: OPEN-WID, ( label -- ) {: prot:label :}
   A64ICODE:LBL {: open:label :}
   9 XREG-RBASE SEAL-NDICT-CELL A64ICODE:LDR,  9 open A64ICODE:CBZ,
   2 OWNER-API-PUB-WID A64ICODE:CMPI,  A64ASM:C-EQ prot A64ICODE:BCOND,
   2 OWNER-API-PRI-WID A64ICODE:CMPI,  A64ASM:C-EQ prot A64ICODE:BCOND,
   open A64ICODE:LBL, ;

\ Count record NDICT and index it.
: PUBLISH, ( -- )
   EM-DEF-OCC:LAPPEND A64ICODE:LABEL@ A64ICODE:BL,  ENGINE-EMIT:LHIDXADD A64ICODE:LABEL@ A64ICODE:BL, ;

\ x<r> = the replay scope band; its cells are reached as offsets from that base.
: BAND, ( n -- ) {: r:n :}
   r REPLAY-SCOPE:LATCH A64ICODE:LIT64,  r XREG-RBASE r A64ICODE:ADD, ;

: BAND-OFF ( n -- n ) REPLAY-SCOPE:LATCH - ;

\ While an overlay is open, raise its high-water mark to NDICT: the records the
\ overlay published are [the saved NDICT, HW). Clobbers x8 and x14.
: HW-RAISE, ( -- )
   A64ICODE:LBL {: keep:label :}
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF A64ICODE:LDR,  14 keep A64ICODE:CBZ,
   14 8 REPLAY-SCOPE:HW BAND-OFF A64ICODE:LDR,  NDICT 14 A64ICODE:CMP,  A64ASM:C-LS keep A64ICODE:BCOND,
   NDICT 8 REPLAY-SCOPE:HW BAND-OFF A64ICODE:STR,
   keep A64ICODE:LBL, ;

\ One scope cell, at off from x<base>, into band cell `cell` at x8 when save is
\ true and back out of it when it is false. Clobbers x14.
: SAVED, ( n n n bool -- ) {: base:n off:n cell:n save:bool :}
   save if
      14 base off A64ICODE:LDR,  14 8 cell BAND-OFF A64ICODE:STR,
   else
      14 8 cell BAND-OFF A64ICODE:LDR,  14 base off A64ICODE:STR,
   then ;

\ The scope replay-open saves and replay-close restores, beside NDICT and CP.
\ x8 = the band. Clobbers x10 and x14.
: SCOPE, ( bool -- ) {: save:bool :}
   XREG-RBASE WIDN-CELL REPLAY-SCOPE:WIDN save SAVED,
   XREG-RBASE CUR-CELL REPLAY-SCOPE:CUR save SAVED,
   XREG-RBASE PKG-PUB-CELL REPLAY-SCOPE:PKG-PUB save SAVED,
   XREG-RBASE PKG-PRI-CELL REPLAY-SCOPE:PKG-PRI save SAVED,
   XREG-RBASE PKG-PARENT-CELL REPLAY-SCOPE:PKG-PARENT save SAVED,
   XREG-RBASE PKG-REC-CELL REPLAY-SCOPE:PKG-REC save SAVED,
   ENCLOSING:END ENCLOSING:PUB - 0 do
      XREG-RBASE ENCLOSING:PUB i +  REPLAY-SCOPE:ENCLOSING i +  save SAVED,
   CELL +loop
   10 USE-BAND-OFF A64ICODE:LIT64,  10 XREG-RBASE 10 A64ICODE:ADD,
   10 USE-DEPTH-CELL USE-BAND-OFF - REPLAY-SCOPE:USE-DEPTH save SAVED,
   10 USE-PKG-SAVE-CELL USE-BAND-OFF - REPLAY-SCOPE:USE-PKG-SAVE save SAVED,
   USE-MAX 0 ?do
      10 USE-WIDS-OFF USE-BAND-OFF - i cells +  REPLAY-SCOPE:USE-WIDS i cells +  save SAVED,
   loop ;

\ Zero the cells [x10, x11). Clobbers x10 and x15.
: ZERO-SPAN, ( -- )
   A64ICODE:LBL A64ICODE:LBL {: next:label done:label :}
   15 0 A64ICODE:MOVZ,
   next A64ICODE:LBL,  10 11 A64ICODE:CMP,  A64ASM:C-CS done A64ICODE:BCOND,
      15 10 0 A64ICODE:STR,  10 10 8 A64ICODE:ADDI,  next A64ICODE:B,
   done A64ICODE:LBL, ;

public

\ The entry of every replay record: executing one is an error the record has no
\ code to report.
variable LREPLAYTRAP

: EMIT-REPLAY-TRAP ( -- )
   LREPLAYTRAP A64ICODE:LABEL@ A64ICODE:LBL,
   s" E-REPLAY-RECORD habu: replay record executed" EXIT-STATUS:INTERNAL CRASH:DIE-LINE, ;

\ namespace-record ( ^u8 n bool -- n )
\ A name holds at most one colon, never at an edge: a part's path `A:B`.
: NAMESPACE-RECORD ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: bad scan next clean nopri done :}
   s" E-TASK-COMPILE" s" namespace-record" ENGINE-EMIT:B-TASK-GUARD
   13 RT:G-POP  1 RT:G-POP  0 RT:G-POP                         \ x13 = the flag, x1 = the length, x0 = the name
   bad NOT-PENDING,
   bad DICT-ROOM,
   bad NAME-SIZE,
   14 0 A64ICODE:MOVZ,  16 0 A64ICODE:MOVZ,                             \ x16 = a colon seen
   scan A64ICODE:LBL,  14 1 A64ICODE:CMP,  A64ASM:C-CS clean A64ICODE:BCOND,
      15 0 14 A64ICODE:ADD,  15 15 0 A64ICODE:LDRB,  15 $3A A64ICODE:CMPI,  A64ASM:C-NE next A64ICODE:BCOND,
         16 bad A64ICODE:CBNZ,  14 bad A64ICODE:CBZ,
         15 14 1 A64ICODE:ADDI,  15 1 A64ICODE:CMP,  A64ASM:C-CS bad A64ICODE:BCOND,
         16 1 A64ICODE:MOVZ,
   next A64ICODE:LBL,  14 14 1 A64ICODE:ADDI,  scan A64ICODE:B,
   clean A64ICODE:LBL,
   2 XREG-RBASE BOOT:NAMESPACE-WL-CELL A64ICODE:LDR,
   bad FRESH,
   10 0 0 A64ICODE:ADDI,  12 1 0 A64ICODE:ADDI,  3 13 0 A64ICODE:ADDI,           \ the name, the flag
   9 NDICT REC-AT,
   NAME-COPY
   11 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  11 9 11 A64ICODE:ADD,
   14 XREG-RBASE WIDN-CELL A64ICODE:LDR,  14 11 0 A64ICODE:STR,                \ [0] = a fresh wid
   14 14 1 A64ICODE:ADDI,  15 0 A64ICODE:MOVZ,
   3 nopri A64ICODE:CBZ,
      15 14 0 A64ICODE:ADDI,  14 14 1 A64ICODE:ADDI,                    \ [8] = a second one
   nopri A64ICODE:LBL,
   15 11 8 A64ICODE:STR,  14 XREG-RBASE WIDN-CELL A64ICODE:STR,
   15 XREG-RBASE BOOT:NAMESPACE-WL-CELL A64ICODE:LDR,  15 11 40 A64ICODE:STR,
   PUBLISH,
   HW-RAISE,                                          \ an overlay's row dies at replay-close
   9 NDICT 1 A64ICODE:SUBI,  9 RT:G-PUSH
   done A64ICODE:B,
   s" E-SEAL-VIOLATION habu: namespace-record with a definition pending, no record room, an empty name or one reaching the code ceiling, a colon at an edge or twice, or a name already recorded"
   bad EXIT-STATUS:SEAL-VIOLATION REJECT-AT
   done A64ICODE:LBL, ;

\ namespace-private ( n -- )
: NAMESPACE-PRIVATE ( -- )
   A64ICODE:LBL A64ICODE:LBL {: bad done :}
   s" E-TASK-COMPILE" s" namespace-private" ENGINE-EMIT:B-TASK-GUARD
   OVERLAY-EMIT:GUARD,
   0 RT:G-POP
   0 NDICT A64ICODE:CMP,  A64ASM:C-CS bad A64ICODE:BCOND,
   9 0 REC-AT,
   14 9 40 A64ICODE:LDR,  14 0 A64ICODE:CMPI,  A64ASM:C-GE bad A64ICODE:BCOND,             \ a live package row
   15 WL:RETIRED invert A64ICODE:MOVN,  14 15 A64ICODE:CMP,  A64ASM:C-EQ bad A64ICODE:BCOND,
   15 WL:RETIRED-NAMESPACE invert A64ICODE:MOVN,  14 15 A64ICODE:CMP,  A64ASM:C-EQ bad A64ICODE:BCOND,
   14 9 8 A64ICODE:LDR,  14 bad A64ICODE:CBNZ,
   11 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  11 9 11 A64ICODE:ADD,
   14 XREG-RBASE WIDN-CELL A64ICODE:LDR,  14 11 8 A64ICODE:STR,
   14 14 1 A64ICODE:ADDI,  14 XREG-RBASE WIDN-CELL A64ICODE:STR,
   done A64ICODE:B,
   s" E-SEAL-VIOLATION habu: namespace-private index not a live package row below NDICT, or a row that has a private wordlist"
   bad EXIT-STATUS:SEAL-VIOLATION REJECT-AT
   done A64ICODE:LBL, ;

\ alias-record ( ^u8 n n n -- ) name, source index, wid
: ALIAS-RECORD ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: bad prot notval done :}
   s" E-TASK-COMPILE" s" alias-record" ENGINE-EMIT:B-TASK-GUARD
   OVERLAY-EMIT:GUARD,
   2 RT:G-POP  4 RT:G-POP  1 RT:G-POP  0 RT:G-POP                 \ x2 = the wid, x4 = the source, x1/x0 = the name
   bad REAL-WID,
   prot OPEN-WID,
   bad NOT-PENDING,
   4 NDICT A64ICODE:CMP,  A64ASM:C-CS bad A64ICODE:BCOND,
   5 4 REC-AT,
   14 5 40 A64ICODE:LDR,  14 0 A64ICODE:CMPI,  A64ASM:C-LT bad A64ICODE:BCOND,           \ a package row or a retired record has no live body
   14 5 16 A64ICODE:LDR,  14 14 DNAME-INT A64ICODE:ANDI,  14 bad A64ICODE:CBNZ,  \ an internal body: the alias would drop its gate
   bad DICT-ROOM,
   bad NAME-SIZE,
   13 4 0 A64ICODE:ADDI,
   bad FRESH,
   3 2 0 A64ICODE:ADDI,  4 13 0 A64ICODE:ADDI,  10 0 0 A64ICODE:ADDI,  12 1 0 A64ICODE:ADDI,
   9 NDICT REC-AT,
   NAME-COPY
   11 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  11 9 11 A64ICODE:ADD,
   5 4 REC-AT,
   14 5 0 A64ICODE:LDR,  14 11 0 A64ICODE:STR,                           \ [0] = the source's entry
   14 5 8 A64ICODE:LDR,  14 11 8 A64ICODE:STR,                           \ [8] = its recorded length
   16 5 16 A64ICODE:LDR,
   15 DNAME-IMM DNAME-WIDE or DNAME-TARGET or DNAME-COPY or A64ICODE:LIT64,  14 16 15 A64ICODE:AND,
   15 DKIND:MASK A64ICODE:LIT64,  16 16 15 A64ICODE:AND,
   15 DKIND:VAL A64ICODE:LIT64,  16 15 A64ICODE:CMP,  A64ASM:C-NE notval A64ICODE:BCOND,
      14 14 15 A64ICODE:ORR,                                    \ only an immutable value keeps its fixed kind
   notval A64ICODE:LBL,
   15 9 16 A64ICODE:LDR,  15 15 14 A64ICODE:ORR,  15 11 16 A64ICODE:STR,
   3 11 40 A64ICODE:STR,
   PUBLISH,
   done A64ICODE:B,
   s" E-SEAL-VIOLATION habu: alias-record with a marker wid, a definition pending, a source that is no live definition or is internal, no record room, an empty name or one reaching the code ceiling, or a name taken"
   bad EXIT-STATUS:SEAL-VIOLATION REJECT-AT
   s" E-SEAL-WORDLIST habu: alias-record into a wordlist protected by the seal"
   prot EXIT-STATUS:SEAL-WORDLIST REJECT-AT
   done A64ICODE:LBL, ;

\ package-scope! ( n n -- )
: PACKAGE-SCOPE ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: bad set inline scan part top store done :}
   1 RT:G-POP  0 RT:G-POP                                   \ x1 = the parent wid, x0 = the row
   15 0 A64ICODE:MOVN,  0 15 A64ICODE:CMP,  A64ASM:C-NE set A64ICODE:BCOND,
      1 bad A64ICODE:CBNZ,                                     \ only `-1 0` clears
      9 0 A64ICODE:MOVZ,
      9 XREG-RBASE PKG-PUB-CELL A64ICODE:STR,  9 XREG-RBASE PKG-PRI-CELL A64ICODE:STR,
      9 XREG-RBASE PKG-PARENT-CELL A64ICODE:STR,  9 XREG-RBASE PKG-REC-CELL A64ICODE:STR,
      9 XREG-RBASE ENCLOSING:PUB A64ICODE:STR,  9 XREG-RBASE ENCLOSING:PRI A64ICODE:STR,
      9 XREG-RBASE ENCLOSING:PARENT A64ICODE:STR,  9 XREG-RBASE ENCLOSING:REC A64ICODE:STR,
      done A64ICODE:B,
   set A64ICODE:LBL,
   0 NDICT A64ICODE:CMP,  A64ASM:C-CS bad A64ICODE:BCOND,
   9 0 REC-AT,
   14 9 40 A64ICODE:LDR,  14 0 A64ICODE:CMPI,  A64ASM:C-GE bad A64ICODE:BCOND,             \ a live package row
   15 WL:RETIRED invert A64ICODE:MOVN,  14 15 A64ICODE:CMP,  A64ASM:C-EQ bad A64ICODE:BCOND,
   15 WL:RETIRED-NAMESPACE invert A64ICODE:MOVN,  14 15 A64ICODE:CMP,  A64ASM:C-EQ bad A64ICODE:BCOND,
   12 9 8 A64ICODE:LDR,  12 bad A64ICODE:CBZ,
   2 9 16 A64ICODE:LDR,  2 2 DNAME-FLAG-BITS A64ICODE:LSLI,  2 2 DNAME-FLAG-BITS A64ICODE:LSRI,   \ x2 = the name's length
   4 9 24 A64ICODE:ADDI,
   14 9 16 A64ICODE:LDR,  14 14 DNAME-EXT A64ICODE:ANDI,  14 inline A64ICODE:CBZ,
      4 9 24 A64ICODE:LDR,                                     \ x4 = the name's bytes
   inline A64ICODE:LBL,
   3 0 A64ICODE:MOVZ,
   scan A64ICODE:LBL,  3 2 A64ICODE:CMP,  A64ASM:C-CS top A64ICODE:BCOND,
      15 4 3 A64ICODE:ADD,  15 15 0 A64ICODE:LDRB,  15 $3A A64ICODE:CMPI,  A64ASM:C-EQ part A64ICODE:BCOND,
      3 3 1 A64ICODE:ADDI,  scan A64ICODE:B,
   part A64ICODE:LBL,
      14 XREG-RBASE PKG-PUB-CELL A64ICODE:LDR,  14 bad A64ICODE:CBZ,          \ no package open
      14 XREG-RBASE ENCLOSING:PUB A64ICODE:LDR,  14 bad A64ICODE:CBNZ,        \ a part is open
      14 XREG-RBASE PKG-PUB-CELL A64ICODE:LDR,  14 XREG-RBASE ENCLOSING:PUB A64ICODE:STR,
      14 XREG-RBASE PKG-PRI-CELL A64ICODE:LDR,  14 XREG-RBASE ENCLOSING:PRI A64ICODE:STR,
      14 XREG-RBASE PKG-PARENT-CELL A64ICODE:LDR,  14 XREG-RBASE ENCLOSING:PARENT A64ICODE:STR,
      14 XREG-RBASE PKG-REC-CELL A64ICODE:LDR,  14 XREG-RBASE ENCLOSING:REC A64ICODE:STR,
      store A64ICODE:B,
   top A64ICODE:LBL,
      14 0 A64ICODE:MOVZ,
      14 XREG-RBASE ENCLOSING:PUB A64ICODE:STR,  14 XREG-RBASE ENCLOSING:PRI A64ICODE:STR,
      14 XREG-RBASE ENCLOSING:PARENT A64ICODE:STR,  14 XREG-RBASE ENCLOSING:REC A64ICODE:STR,
   store A64ICODE:LBL,
   11 9 0 A64ICODE:LDR,
   11 XREG-RBASE PKG-PUB-CELL A64ICODE:STR,  12 XREG-RBASE PKG-PRI-CELL A64ICODE:STR,
   1 XREG-RBASE PKG-PARENT-CELL A64ICODE:STR,  9 XREG-RBASE PKG-REC-CELL A64ICODE:STR,
   done A64ICODE:B,
   s" E-SEAL-VIOLATION habu: package-scope! row not a live package row below NDICT with a private wordlist, a part outside a package or inside a part, or -1 with a parent"
   bad EXIT-STATUS:SEAL-VIOLATION REJECT-AT
   done A64ICODE:LBL, ;

\ replay-open ( -- )
: REPLAY-OPEN ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: bad none done :}
   s" E-TASK-COMPILE" s" replay-open" ENGINE-EMIT:B-TASK-GUARD
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF A64ICODE:LDR,  14 bad A64ICODE:CBNZ,   \ an overlay is open
   9 XREG-RBASE PEND-CELL A64ICODE:LDR,  9 none A64ICODE:CBZ,
      14 XREG-RBASE TASKS-LIVE-CELL A64ICODE:LDR,  14 bad A64ICODE:CBNZ,
      10 NDICT REC-AT,
      DREC 0 do  14 10 i A64ICODE:LDR,  14 XREG-RBASE REPLAY-SCOPE:PEND-REC i + A64ICODE:STR,  CELL +loop
      9 XREG-RBASE REPLAY-SCOPE:PEND A64ICODE:STR,
      14 0 A64ICODE:MOVZ,  14 XREG-RBASE PEND-CELL A64ICODE:STR,
   none A64ICODE:LBL,
   NDICT 8 REPLAY-SCOPE:NDICT BAND-OFF A64ICODE:STR,
   CP 8 REPLAY-SCOPE:CP BAND-OFF A64ICODE:STR,
   0 0= SCOPE,
   NDICT 8 REPLAY-SCOPE:HW BAND-OFF A64ICODE:STR,
   14 1 A64ICODE:MOVZ,  14 8 REPLAY-SCOPE:LATCH BAND-OFF A64ICODE:STR,
   done A64ICODE:B,
   s" E-SEAL-VIOLATION habu: replay-open with an overlay open, or a definition pending while a task is live"
   bad EXIT-STATUS:SEAL-VIOLATION REJECT-AT
   done A64ICODE:LBL, ;

\ replay-record ( ^u8 n n -- ) name, wid: a codeless record, whose entry
\ is the trap. It needs no OPEN-WID: it can only trap, and replay-close
\ removes it.
: REPLAY-RECORD ( -- )
   A64ICODE:LBL A64ICODE:LBL {: bad done :}
   s" E-TASK-COMPILE" s" replay-record" ENGINE-EMIT:B-TASK-GUARD
   2 RT:G-POP  1 RT:G-POP  0 RT:G-POP                          \ x2 = the wid, x1/x0 = the name
   bad REAL-WID,
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF A64ICODE:LDR,  14 bad A64ICODE:CBZ,   \ no overlay
   bad NOT-PENDING,
   bad DICT-ROOM,
   bad NAME-SIZE,
   bad FRESH,
   3 2 0 A64ICODE:ADDI,  10 0 0 A64ICODE:ADDI,  12 1 0 A64ICODE:ADDI,
   9 NDICT REC-AT,
   NAME-COPY
   11 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  11 9 11 A64ICODE:ADD,
   14 LREPLAYTRAP A64ICODE:LABEL@ A64ICODE:ADR,  14 11 0 A64ICODE:STR,            \ [0] = the trap
   14 0 A64ICODE:MOVZ,  14 11 8 A64ICODE:STR,
   3 11 40 A64ICODE:STR,
   PUBLISH,
   HW-RAISE,
   done A64ICODE:B,
   s" E-SEAL-VIOLATION habu: replay-record with a marker wid, no overlay, a definition pending, no record room, an empty name or one reaching the code ceiling, or a name taken"
   bad EXIT-STATUS:SEAL-VIOLATION REJECT-AT
   done A64ICODE:LBL, ;

\ record-wid! ( n n -- ) wid, index: the overlay retires a record with wid -2
\ and later gives it back the wid it logged, in reverse order. The wid cell is
\ half the record's hash key, and a compaction between the two re-keys the
\ retired record on RETIRED's chain, so after each store the record is indexed
\ under the wid it now holds.
\ Between a restore and replay-close one wordlist can hold two live records of
\ one name, the restored record and a replay record, and the hash probe and
\ the wordlist scan may answer with different ones. The owner ends every such
\ window before any lookup: CLOSE's restore with replay-close, a frame
\ ROLLBACK's with the ndict! that drops the replay record.
: RECORD-WID ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: bad wid store done :}
   s" E-TASK-COMPILE" s" record-wid!" ENGINE-EMIT:B-TASK-GUARD
   3 RT:G-POP  13 RT:G-POP                                  \ x3 = the index, x13 = the wid
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF A64ICODE:LDR,  14 bad A64ICODE:CBZ,   \ no overlay
   3 NDICT A64ICODE:CMP,  A64ASM:C-CS bad A64ICODE:BCOND,
   9 3 REC-AT,
   14 9 40 A64ICODE:LDR,  14 0 A64ICODE:CMPI,  A64ASM:C-GE wid A64ICODE:BCOND,             \ a live package row
   15 WL:RETIRED invert A64ICODE:MOVN,  14 15 A64ICODE:CMP,  A64ASM:C-EQ wid A64ICODE:BCOND,
   15 WL:RETIRED-NAMESPACE invert A64ICODE:MOVN,  14 15 A64ICODE:CMP,  A64ASM:C-NE bad A64ICODE:BCOND,
   wid A64ICODE:LBL,
   13 0 A64ICODE:CMPI,  A64ASM:C-GE store A64ICODE:BCOND,                         \ a live package row's marker
   15 WL:RETIRED invert A64ICODE:MOVN,  13 15 A64ICODE:CMP,  A64ASM:C-EQ store A64ICODE:BCOND,
   15 WL:RETIRED-NAMESPACE invert A64ICODE:MOVN,  13 15 A64ICODE:CMP,  A64ASM:C-NE bad A64ICODE:BCOND,
   store A64ICODE:LBL,
   14 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  14 9 14 A64ICODE:ADD,  13 14 40 A64ICODE:STR,
   HIDX-EMIT:LREKEY A64ICODE:LABEL@ A64ICODE:BL,
   done A64ICODE:B,
   s" E-SEAL-VIOLATION habu: record-wid! with no overlay, an index at or past NDICT, or an old or new wid neither a wordlist nor a retired marker"
   bad EXIT-STATUS:SEAL-VIOLATION REJECT-AT
   done A64ICODE:LBL, ;

\ record-rekey ( n -- ) index, below NDICT: the body checks nothing.
: RECORD-REKEY ( -- )
   3 RT:G-POP  HIDX-EMIT:LREKEY A64ICODE:LABEL@ A64ICODE:BL, ;

\ replay-private ( n bool -- ) namespace index, flag: true gives the row a
\ fresh private wid, as `package` gives one to a row a qualified definition
\ made; false takes back the one it gave. replay-close puts WIDN back but
\ nothing of a row below the saved NDICT, so the owner logs each grant and
\ takes it back before the close.
: REPLAY-PRIVATE ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: bad give done :}
   s" E-TASK-COMPILE" s" replay-private" ENGINE-EMIT:B-TASK-GUARD
   13 RT:G-POP  3 RT:G-POP                                  \ x13 = the flag, x3 = the index
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF A64ICODE:LDR,  14 bad A64ICODE:CBZ,   \ no overlay
   3 NDICT A64ICODE:CMP,  A64ASM:C-CS bad A64ICODE:BCOND,
   9 3 REC-AT,
   14 9 40 A64ICODE:LDR,  14 0 A64ICODE:CMPI,  A64ASM:C-GE bad A64ICODE:BCOND,             \ a live package row
   15 WL:RETIRED invert A64ICODE:MOVN,  14 15 A64ICODE:CMP,  A64ASM:C-EQ bad A64ICODE:BCOND,
   15 WL:RETIRED-NAMESPACE invert A64ICODE:MOVN,  14 15 A64ICODE:CMP,  A64ASM:C-EQ bad A64ICODE:BCOND,
   14 9 8 A64ICODE:LDR,
   13 give A64ICODE:CBNZ,
      15 8 REPLAY-SCOPE:WIDN BAND-OFF A64ICODE:LDR,  14 15 A64ICODE:CMP,  A64ASM:C-CC bad A64ICODE:BCOND,
      15 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  15 9 15 A64ICODE:ADD,
      14 0 A64ICODE:MOVZ,  14 15 8 A64ICODE:STR,
      done A64ICODE:B,
   give A64ICODE:LBL,
   14 bad A64ICODE:CBNZ,
   15 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  15 9 15 A64ICODE:ADD,
   14 XREG-RBASE WIDN-CELL A64ICODE:LDR,  14 15 8 A64ICODE:STR,
   14 14 1 A64ICODE:ADDI,  14 XREG-RBASE WIDN-CELL A64ICODE:STR,
   done A64ICODE:B,
   s" E-SEAL-VIOLATION habu: replay-private with no overlay, an index not a live package row below NDICT, a grant to a row that has one, or a take-back below the saved WIDN"
   bad EXIT-STATUS:SEAL-VIOLATION REJECT-AT
   done A64ICODE:LBL, ;

\ replay-close ( -- ): every check comes before the first store, and the parked
\ definition goes back into slot NDICT last, after the records [NDICT, HW) are
\ zeroed. Engine writers only raise WIDN past the saved count, so the WIDN
\ rejection meets a raw store to WIDN-CELL, which nothing else rejects.
: REPLAY-CLOSE ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: bad unparked done :}
   s" E-TASK-COMPILE" s" replay-close" ENGINE-EMIT:B-TASK-GUARD
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF A64ICODE:LDR,  14 bad A64ICODE:CBZ,   \ no overlay
   bad NOT-PENDING,
   4 8 REPLAY-SCOPE:NDICT BAND-OFF A64ICODE:LDR,  NDICT 4 A64ICODE:CMP,  A64ASM:C-CC bad A64ICODE:BCOND,
   5 8 REPLAY-SCOPE:HW BAND-OFF A64ICODE:LDR,  NDICT 5 A64ICODE:CMP,  A64ASM:C-HI bad A64ICODE:BCOND,
   6 8 REPLAY-SCOPE:CP BAND-OFF A64ICODE:LDR,  CP 6 A64ICODE:CMP,  A64ASM:C-CC bad A64ICODE:BCOND,
   14 8 REPLAY-SCOPE:WIDN BAND-OFF A64ICODE:LDR,  15 XREG-RBASE WIDN-CELL A64ICODE:LDR,
   15 14 A64ICODE:CMP,  A64ASM:C-CC bad A64ICODE:BCOND,
   14 XREG-RBASE REPLAY-SCOPE:PEND A64ICODE:LDR,  14 unparked A64ICODE:CBZ,
   14 XREG-RBASE TASKS-LIVE-CELL A64ICODE:LDR,  14 bad A64ICODE:CBNZ,
   unparked A64ICODE:LBL,
   10 DREC A64ICODE:MOVZ,  11 5 10 A64ICODE:MUL,  10 NDICT 10 A64ICODE:MUL,
   10 DBASE 10 A64ICODE:ADD,  11 DBASE 11 A64ICODE:ADD,                 \ records [x10, x11)
   12 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  10 10 12 A64ICODE:ADD,  11 11 12 A64ICODE:ADD,
   ZERO-SPAN,
   8 BAND,
   0 0<> SCOPE,
   10 8 0 A64ICODE:ADDI,  11 8 REPLAY-SCOPE:END BAND-OFF A64ICODE:ADDI,
   ZERO-SPAN,
   10 8 REPLAY-SCOPE:ENCLOSING BAND-OFF A64ICODE:ADDI,
   11 10 ENCLOSING:END ENCLOSING:PUB - A64ICODE:ADDI,
   ZERO-SPAN,
   14 XREG-RBASE REPLAY-SCOPE:PEND A64ICODE:LDR,  14 done A64ICODE:CBZ,
   9 NDICT REC-AT,
   11 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  11 9 11 A64ICODE:ADD,
   DREC 0 do  14 XREG-RBASE REPLAY-SCOPE:PEND-REC i + A64ICODE:LDR,  14 11 i A64ICODE:STR,  CELL +loop
   9 XREG-RBASE PEND-CELL A64ICODE:STR,
   14 0 A64ICODE:MOVZ,  14 XREG-RBASE REPLAY-SCOPE:PEND A64ICODE:STR,
   DREC 0 do  14 XREG-RBASE REPLAY-SCOPE:PEND-REC i + A64ICODE:STR,  CELL +loop
   done A64ICODE:B,
   s" E-SEAL-VIOLATION habu: replay-close with no overlay, a definition pending, a parked definition while a task is live, NDICT below the saved count or above HW, CP below the saved CP, or WIDN below the saved WIDN"
   bad EXIT-STATUS:SEAL-VIOLATION REJECT-AT
   done A64ICODE:LBL, ;

\ replay-widn! ( n -- ): put WIDN back to N, a mark the owner read while the
\ overlay was open, as a rollback inside the overlay drops what it made since
\ the mark. The records ndict! drops and the private wids replay-private takes
\ back held every wid at or above N, so none is handed out twice. It rejects no
\ overlay, N below the saved WIDN, which would hand out again a wid from before
\ the overlay, and N above WIDN, both compared unsigned. A mark is never below
\ the saved WIDN, so replay-close's WIDN check keeps holding.
: REPLAY-WIDN ( -- )
   A64ICODE:LBL A64ICODE:LBL {: bad done :}
   s" E-TASK-COMPILE" s" replay-widn!" ENGINE-EMIT:B-TASK-GUARD
   13 RT:G-POP                                           \ x13 = the mark
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF A64ICODE:LDR,  14 bad A64ICODE:CBZ,   \ no overlay
   14 8 REPLAY-SCOPE:WIDN BAND-OFF A64ICODE:LDR,  13 14 A64ICODE:CMP,  A64ASM:C-CC bad A64ICODE:BCOND,
   15 XREG-RBASE WIDN-CELL A64ICODE:LDR,  13 15 A64ICODE:CMP,  A64ASM:C-HI bad A64ICODE:BCOND,
   13 XREG-RBASE WIDN-CELL A64ICODE:STR,
   done A64ICODE:B,
   s" E-SEAL-VIOLATION habu: replay-widn! with no overlay, or a mark below the saved WIDN or above WIDN"
   bad EXIT-STATUS:SEAL-VIOLATION REJECT-AT
   done A64ICODE:LBL, ;

\ def-open ( ^u8 n n n -- ) name, wid, kind
: DEF-OPEN ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: bad prot done :}
   s" E-TASK-COMPILE" s" def-open" ENGINE-EMIT:B-TASK-GUARD
   OVERLAY-EMIT:GUARD,
   4 RT:G-POP  2 RT:G-POP  1 RT:G-POP  0 RT:G-POP                 \ x4 = the kind, x2 = the wid, x1/x0 = the name
   14 DKIND:MASK DNAME-CELL or DNAME-ALLOC or DNAME-TARGET or invert A64ICODE:LIT64,  14 4 14 A64ICODE:AND,  14 bad A64ICODE:CBNZ,
   bad REAL-WID,
   prot OPEN-WID,
   bad NOT-PENDING,
   9 CODE-CEILING A64ICODE:LIT64,  9 DBASE 9 A64ICODE:ADD,  CP 9 A64ICODE:CMP,  A64ASM:C-CS bad A64ICODE:BCOND,
   bad DICT-ROOM,
   bad NAME-SIZE,
   13 4 0 A64ICODE:ADDI,
   bad FRESH,
   3 2 0 A64ICODE:ADDI,  4 13 0 A64ICODE:ADDI,  10 0 0 A64ICODE:ADDI,  12 1 0 A64ICODE:ADDI,
   9 NDICT REC-AT,
   NAME-COPY
   11 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  11 9 11 A64ICODE:ADD,
   CP 11 0 A64ICODE:STR,                                       \ [0] = the entry, past a long name
   14 0 A64ICODE:MOVZ,  14 11 8 A64ICODE:STR,
   14 9 16 A64ICODE:LDR,  14 14 4 A64ICODE:ORR,  14 11 16 A64ICODE:STR,          \ the kind beside the length
   3 11 40 A64ICODE:STR,
   9 XREG-RBASE PEND-CELL A64ICODE:STR,  14 XREG-RBASE TKA-CELL A64ICODE:LDR,  14 XREG-RBASE PENDTKA-CELL A64ICODE:STR,
   14 0 A64ICODE:MOVZ,
   14 XREG-RBASE TSIG-A-CELL A64ICODE:STR,   14 XREG-RBASE TSIG-U-CELL A64ICODE:STR,
   14 XREG-RBASE TCSIG-A-CELL A64ICODE:STR,  14 XREG-RBASE TCSIG-U-CELL A64ICODE:STR,
   14 XREG-RBASE DOESB-CELL A64ICODE:STR,    14 XREG-RBASE TRUSTED-CELL A64ICODE:STR,
   14 XREG-RBASE CLAUSE-CELL A64ICODE:STR,
   14 ENGINE-EMIT:COMP-LATCH  14 XREG-RBASE NCOMP-DISPATCH:COMP-LATCH-CELL A64ICODE:STR,
   done A64ICODE:B,
   s" E-SEAL-VIOLATION habu: def-open with a bad kind, a marker wid, a definition pending, CP at the code ceiling, no record room, an empty name or one reaching the code ceiling, or a name taken"
   bad EXIT-STATUS:SEAL-VIOLATION REJECT-AT
   s" E-SEAL-WORDLIST habu: def-open into a wordlist protected by the seal"
   prot EXIT-STATUS:SEAL-WORDLIST REJECT-AT
   done A64ICODE:LBL, ;

\ body-capture ( ^u8 n -- ): LBCS is the capture `:` appends through; it grows
\ the body text block as every capture does.
: BODY-APPEND ( -- )
   A64ICODE:LBL A64ICODE:LBL {: bad done :}
   12 RT:G-POP  11 RT:G-POP                                 \ LBCS: x11 = the bytes, x12 = their count
   14 XREG-RBASE BODYLEN-CELL A64ICODE:LDR,  15 XREG-RBASE BODY-TEXT:CAP-CELL A64ICODE:LDR,
   14 15 A64ICODE:CMP,  A64ASM:C-HI bad A64ICODE:BCOND,                        \ BODYLEN is already past the text capacity
   15 0 A64ICODE:MOVN,  15 15 14 A64ICODE:SUB,                          \ x15 = 2^64 - 1 - BODYLEN
   12 15 A64ICODE:CMP,  A64ASM:C-CS bad A64ICODE:BCOND,                        \ BODYLEN + u + 1 wraps
   ENGINE-EMIT:LBCS A64ICODE:LABEL@ A64ICODE:BL,
   done A64ICODE:B,
   s" E-SEAL-VIOLATION habu: body-capture past the body text capacity or wrapping"
   bad EXIT-STATUS:SEAL-VIOLATION REJECT-AT
   done A64ICODE:LBL, ;

\ ( ^u8 n -- ): the span into the cells aoff and uoff while a
\ definition is pending.
: PENDING-SPAN ( str n n -- ) {: text:str aoff:n uoff:n :}
   A64ICODE:LBL A64ICODE:LBL {: bad done :}
   12 RT:G-POP  11 RT:G-POP
   9 XREG-RBASE PEND-CELL A64ICODE:LDR,  9 bad A64ICODE:CBZ,
   11 XREG-RBASE aoff A64ICODE:STR,  12 XREG-RBASE uoff A64ICODE:STR,
   done A64ICODE:B,
   text bad EXIT-STATUS:SEAL-VIOLATION REJECT-AT
   done A64ICODE:LBL, ;

\ trust-sig! ( ^u8 n -- )
: TRUST-SIG ( -- )
   s" E-SEAL-VIOLATION habu: trust-sig! with no definition pending"
   TSIG-A-CELL TSIG-U-CELL PENDING-SPAN ;

\ created-sig! ( ^u8 n -- )
: CREATED-SIG ( -- )
   s" E-SEAL-VIOLATION habu: created-sig! with no definition pending"
   TCSIG-A-CELL TCSIG-U-CELL PENDING-SPAN ;

\ The close def-close runs; PEND-CELL clears last.
: NATIVE-CLOSE ( -- )
   ENGINE-EMIT:C-CLEAR-TRUSTED-STATE
   ENGINE-EMIT:C-CLEAR-LOCALS
   9 0 A64ICODE:MOVZ,  9 XREG-RBASE PEND-CELL A64ICODE:STR, ;

\ def-close ( -- )
: DEF-CLOSE ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: bad done nolast :}
   9 XREG-RBASE PEND-CELL A64ICODE:LDR,  9 bad A64ICODE:CBZ,
   10 XREG-RBASE NCOMP-DISPATCH:COMP-LATCH-CELL A64ICODE:LDR,  10 bad A64ICODE:CBZ,
   10 9 16 A64ICODE:LDR,  10 10 DKIND:MASK A64ICODE:ANDI,  10 nolast A64ICODE:CBZ,   \ a published body that pushes a cell is the slot `does>` patches (LDOESPATCH), set after the body as the kernel's definers set it
   11 DKIND:CAST A64ICODE:LIT64,  10 11 A64ICODE:CMP,  A64ASM:C-EQ nolast A64ICODE:BCOND,
   9 XREG-RBASE LASTC-CELL A64ICODE:STR,
   nolast A64ICODE:LBL,
   NATIVE-CLOSE
   done A64ICODE:B,
   s" E-SEAL-VIOLATION habu: def-close with no definition pending or no compiler latched"
   bad EXIT-STATUS:SEAL-VIOLATION REJECT-AT
   done A64ICODE:LBL, ;

;package

package ENGINE-EMIT

\ DRAIN-PRETRUST ( -- ): for each pending pre-trust defer, last slot first,
\ calls `trust-decl` with the slot's name and signature, then the target
\ owner's `checker-defer` with the name, and drops the slot; a target with no
\ `checker-defer` ends the drain. Runs once, after checker.f defines both
\ words. The band count is the loop state, so it survives the checker BLR
\ calls. A framed prim (FPRIM).
: BDRAINPRETRUST ( -- )
   A64ICODE:LBL A64ICODE:LBL {: loop done :}
   loop A64ICODE:LBL,
      12 PD-TABLE-OFF A64ICODE:LIT64,  12 XREG-RBASE 12 A64ICODE:ADD,  13 12 0 A64ICODE:LDR,  13 done A64ICODE:CBZ,   \ remaining==0 -> done
      DEF-TRUST:FIND                                  \ x11 = trust-decl XT (clobbers x12-x16)
      12 PD-TABLE-OFF A64ICODE:LIT64,  12 XREG-RBASE 12 A64ICODE:ADD,  13 12 0 A64ICODE:LDR,  13 13 1 A64ICODE:SUBI,  \ index = remaining-1
      15 PD-SLOT A64ICODE:MOVZ,  15 13 15 A64ICODE:MUL,  14 12 PD-SLOTS-REL A64ICODE:ADDI,  14 14 15 A64ICODE:ADD,   \ x14 = slot base (x11 preserved)
      9 14 PD-NAME-OFF A64ICODE:ADDI,  9 RT:G-PUSH   9 14 PD-NLEN-OFF A64ICODE:LDR,  9 RT:G-PUSH
      9 14 PD-SIG-OFF A64ICODE:ADDI,   9 RT:G-PUSH   9 14 PD-SLEN-OFF A64ICODE:LDR,  9 RT:G-PUSH
      C-CALL-X11-SAVED
      NCOMP-DISPATCH:DECL-DEFER-OFF done DECL-OWNER:TARGET  \ x11 = target defer XT
      12 PD-TABLE-OFF A64ICODE:LIT64,  12 XREG-RBASE 12 A64ICODE:ADD,  13 12 0 A64ICODE:LDR,  13 13 1 A64ICODE:SUBI,
      15 PD-SLOT A64ICODE:MOVZ,  15 13 15 A64ICODE:MUL,  14 12 PD-SLOTS-REL A64ICODE:ADDI,  14 14 15 A64ICODE:ADD,
      9 14 PD-NAME-OFF A64ICODE:ADDI,  9 RT:G-PUSH   9 14 PD-NLEN-OFF A64ICODE:LDR,  9 RT:G-PUSH
      C-CALL-X11-SAVED
      12 PD-TABLE-OFF A64ICODE:LIT64,  12 XREG-RBASE 12 A64ICODE:ADD,  13 12 0 A64ICODE:LDR,  13 13 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STR,
      loop A64ICODE:B,
   done A64ICODE:LBL, ;

public
\ `immediate` writes one flag cell of the record just published and emits
\ nothing, through the alias. The imm-mark row's body is this word too.
: C-IMMEDIATE ( -- )
   9 NDICT 0 A64ICODE:ADDI,  9 9 1 A64ICODE:SUBI,  10 DREC A64ICODE:MOVZ,  9 9 10 A64ICODE:MUL,  9 DBASE 9 A64ICODE:ADD,
   11 XREG-RBASE REGION-ALIAS:DELTA-CELL A64ICODE:LDR,  11 9 11 A64ICODE:ADD,   \ x11 = the record through the alias
   10 9 16 A64ICODE:LDR,  10 10 DNAME-IMM A64ICODE:ORRI,  10 11 16 A64ICODE:STR, ;

private

\ Blocks this thread for good: a poll of no descriptors with no timeout,
\ again after each signal that ends it. Linux's ppoll takes a NULL timespec
\ for none, Darwin's poll -1.
: EXIT-WAIT, ( -- )
   A64ICODE:LBL {: again:label :}
   again A64ICODE:LBL,  0 0 A64ICODE:MOVZ,  1 0 A64ICODE:MOVZ,
   HB-TARGET-LINUX-ARM64? IF
      2 0 A64ICODE:MOVZ,  3 0 A64ICODE:MOVZ,  4 0 A64ICODE:MOVZ,  SYSCALL:NR-POLL SYSCALL:SYS,  again A64ICODE:B,  exit
   THEN
   HB-TARGET-MACOS? IF 2 0 A64ICODE:MOVN,  SYSCALL:NR-POLL SYSCALL:SYS,  again A64ICODE:B,  exit THEN
   TARGET-UNKNOWN ;

\ LEXITHOOK: EXIT-HOOK-CELL's trampoline, BL-called with the code in x0; it
\ keeps x0 and LR only. The vector and the owner cell are the main region's, at
\ DATA-VA; the handler chain it empties is this thread's. The thread that claims
\ EXIT-OWNER-CELL runs the vector, again on an action's `die`; any other blocks.
: EMIT-EXITHOOK ( -- )
   A64ICODE:LBL A64ICODE:LBL {: nohk:label end:label :}
   A64ICODE:LBL {: owned:label :}
   s" (LEXITHOOK)" LEXITHOOK A64ICODE:LABEL@ LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   LEXITHOOK A64ICODE:LABEL@ A64ICODE:LBL,
   11 DATA-VA VA>N A64ICODE:LIT64,
   12 EXIT-OWNER-CELL A64ICODE:MOVZ,  12 11 12 A64ICODE:ADD,
   9 0 A64ICODE:MOVZ,  9 XREG-RBASE 12 A64ICODE:CASAL,
   9 owned A64ICODE:CBZ,  9 XREG-RBASE A64ICODE:CMP,  A64ASM:C-EQ owned A64ICODE:BCOND,
   EXIT-WAIT,
   owned A64ICODE:LBL,
   9 11 EXIT-HOOK-CELL A64ICODE:LDR,  9 nohk A64ICODE:CBZ,
   A64ICODE:SP A64ICODE:SP 16 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,  0 A64ICODE:SP 8 A64ICODE:STR,
   10 0 A64ICODE:MOVZ,  10 11 EXIT-HOOK-CELL A64ICODE:STR,  10 XREG-RBASE HND-CELL A64ICODE:STR,
   9 A64ICODE:BLR,
   0 A64ICODE:SP 8 A64ICODE:LDR,  30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 16 A64ICODE:ADDI,
   nohk A64ICODE:LBL,
   A64ICODE:RET,
   end A64ICODE:LBL, ;

;package

package ENGINE-EMIT

public
: EM-ENTRY-ARGS ( -- )
   HB-TARGET-LINUX-ARM64? IF
      13 A64ICODE:SP 0 A64ICODE:LDR,  14 A64ICODE:SP 8 A64ICODE:ADDI,
      15 13 1 A64ICODE:ADDI,  15 15 3 A64ICODE:LSLI,  15 14 15 A64ICODE:ADD,
      exit
   THEN
   HB-TARGET-MACOS? IF
      13 0 0 A64ICODE:ADDI,  14 1 0 A64ICODE:ADDI,  15 2 0 A64ICODE:ADDI,
      exit
   THEN
   C-TARGET-UNKNOWN ;

public

\ The boot task's return stack and DO/LOOP frame stack. They come after the DATA
\ region because their bases are published in header cells; a task maps its own
\ pair and publishes them into its own region copy.
: EM-FRAME-STACKS ( -- )
   STACK-ABI:RETURN-BYTES 10 STACK-GUARD:EMIT-MAP
   10 XREG-RBASE STACK-ABI:RETURN-BASE-CELL A64ICODE:STR,
   STACK-ABI:LOOP-BYTES 10 STACK-GUARD:EMIT-MAP
   10 XREG-RBASE STACK-ABI:LOOP-BASE-CELL A64ICODE:STR,
   10 XREG-RBASE LOOPSP-CELL A64ICODE:STR, ;

: EM-MMAP-CODE-REGION ( -- )
   \ Leaves x0 = the base code runs from and x21 = the RW alias - base; x13-x15
   \ (argc, argv, envp), x19 and x20 survive.
   HB-TARGET-LINUX-ARM64? IF
      \ The RX view maps fixed over the image's own load of the region, at its
      \ base plus REGION-OFF; the RW alias goes where the kernel puts it.
      A64ICODE:LBL A64ICODE:LBL {: vfail:label vdone:label :}
      9 CODE-OFF A64ICODE:MOVZ,  9 XREG-RBASE 9 A64ICODE:SUB,           \ x9 = this run's image base
      10 REGION-OFF A64ICODE:LIT64,  9 9 10 A64ICODE:ADD,               \ x9 = region base
      vfail LIBC-OS:REGION-MEMFD  10 0 0 A64ICODE:ADDI,        \ x10 = fd
      0 9 0 A64ICODE:ADDI,  1 REGION A64ICODE:LIT64,  2 5 A64ICODE:MOVZ,  3 SYSCALL:MAP-SHARED-FIXED A64ICODE:LIT64,
      4 10 0 A64ICODE:ADDI,  5 0 A64ICODE:MOVZ,  SYSCALL:NR-MMAP SYSCALL:SYS,  A64ASM:C-CS vfail A64ICODE:BCOND,
      0 0 A64ICODE:MOVZ,  1 REGION A64ICODE:LIT64,  2 3 A64ICODE:MOVZ,  3 SYSCALL:MAP-SHARED A64ICODE:LIT64,
      4 10 0 A64ICODE:ADDI,  5 0 A64ICODE:MOVZ,  SYSCALL:NR-MMAP SYSCALL:SYS,  A64ASM:C-CS vfail A64ICODE:BCOND,
      21 0 9 A64ICODE:SUB,
      0 10 0 A64ICODE:ADDI,  SYSCALL:NR-CLOSE SYSCALL:SYS,  A64ASM:C-CS vfail A64ICODE:BCOND,
      0 9 0 A64ICODE:ADDI,  vdone A64ICODE:B,
      vfail A64ICODE:LBL,
         s" hb: mapping the code region's views failed; raise the open-file limit (ulimit -n) or the address-space limit (ulimit -v)"
         EXIT-STATUS:SYSTEM CRASH:DIE-LINE,
      vdone A64ICODE:LBL,
   ELSE
      \ A macOS image is placed by ASLR and reserves no region: hint it at
      \ __text + REGION-OFF, past the image's last segment, and take any base.
      A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: rok:label rvok:label afail:label adone:label :}
      6 REGION-OFF A64ICODE:LIT64,  6 XREG-RBASE 6 A64ICODE:ADD,
      9 PROT-PAGE-MAX 1 - A64ICODE:MOVZ,  6 6 9 A64ICODE:ADD,
      9 PROT-PAGE-MAX negate A64ICODE:LIT64,  6 6 9 A64ICODE:AND,    \ x6 = page-aligned canonical hint
      9 CODE-OFF A64ICODE:MOVZ,  9 XREG-RBASE 9 A64ICODE:SUB,        \ x9 = this run's image base
      7 9 $1B0 A64ICODE:LDR,  8 9 $80 A64ICODE:LDR,  7 7 8 A64ICODE:SUB,
      8 9 $1B8 A64ICODE:LDR,
      7 7 8 A64ICODE:ADD,                                   \ x7 = final segment end offset
      7 9 7 A64ICODE:ADD,
      8 PROT-PAGE-MAX 1 - A64ICODE:MOVZ,  7 7 8 A64ICODE:ADD,
      8 PROT-PAGE-MAX negate A64ICODE:LIT64,  7 7 8 A64ICODE:AND,
      6 7 A64ICODE:CMP,  6 6 7 A64ASM:C-HI A64ICODE:CSEL,                   \ x6 = the higher of hint and image end
      \ PROT-PAGE-MAX spare bytes let the base round up to PROT-PAGE-MAX when
      \ the kernel aligns it only to a page.
      0 6 0 A64ICODE:ADDI,  1 REGION PROT-PAGE-MAX + A64ICODE:LIT64,  2 3 A64ICODE:MOVZ,  3 SYSCALL:MAP-ANON-PRIVATE A64ICODE:LIT64,  4 0 A64ICODE:MOVN,  5 0 A64ICODE:MOVZ,
      SYSCALL:NR-MMAP SYSCALL:SYS,
      \ Darwin sets the carry flag on a failed call, so a failed mapping is
      \ read from the flag, not guessed from the shape of x0.
      A64ASM:C-CC rok A64ICODE:BCOND,
         3 REGION PROT-PAGE-MAX + A64ICODE:LIT64,
         s" hb: mapping the code region failed; raise the address-space limit (ulimit -v) above its size in bytes:"
         EXIT-STATUS:SYSTEM 3 CRASH:DIE-LINE-VALUE,
      rok A64ICODE:LBL,
      9 PROT-PAGE-MAX 1 - A64ICODE:MOVZ,  0 0 9 A64ICODE:ADD,
      9 PROT-PAGE-MAX negate A64ICODE:LIT64,  0 0 9 A64ICODE:AND,    \ page-aligned region base
      \ Region-to-text calls are relocated, so any base serves whose region
      \ lies within BL-REACH of __text. x0 (region base) survives.
      9 REGION A64ICODE:LIT64,  9 0 9 A64ICODE:ADD,  9 9 XREG-RBASE A64ICODE:SUB,
      5 BL-REACH A64ICODE:LIT64,  9 5 A64ICODE:CMP,
      A64ASM:C-CC rvok A64ICODE:BCOND,
         s" E-BL-RANGE habu: code region out of BL range" EXIT-STATUS:BL-RANGE CRASH:DIE-LINE,
      rvok A64ICODE:LBL,
      \ The anonymous pages become the RX view, and the RW alias is remapped
      \ from them.
      A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,  13 A64ICODE:SP 0 A64ICODE:STR,  14 A64ICODE:SP 8 A64ICODE:STR,  15 A64ICODE:SP 16 A64ICODE:STR,  0 A64ICODE:SP 24 A64ICODE:STR,
      16 XREG-RBASE 0 A64ICODE:ADDI,  false afail LIBC-OS:REGION-REMAP
      21 0 0 A64ICODE:ADDI,
      0 A64ICODE:SP 24 A64ICODE:LDR,  1 REGION A64ICODE:LIT64,  2 5 A64ICODE:MOVZ,  SYSCALL:NR-MPROTECT SYSCALL:SYS,
      A64ASM:C-CS afail A64ICODE:BCOND,
      13 A64ICODE:SP 0 A64ICODE:LDR,  14 A64ICODE:SP 8 A64ICODE:LDR,  15 A64ICODE:SP 16 A64ICODE:LDR,  0 A64ICODE:SP 24 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI,
      adone A64ICODE:B,
      afail A64ICODE:LBL,
         s" hb: mapping the code region's writable alias failed; raise the address-space limit (ulimit -v)"
         EXIT-STATUS:SYSTEM CRASH:DIE-LINE,
      adone A64ICODE:LBL,
   THEN ;

;package

package ENGINE-EMIT

public
: EM-MMAP-DATA-REGION ( -- )
   A64ICODE:LBL {: dvok :}
   0 DATA-VA VA>N A64ICODE:LIT64,  1 DATA-SIZE A64ICODE:LIT64,  2 3 A64ICODE:MOVZ,  3 SYSCALL:MAP-DATA-RESERVE A64ICODE:LIT64,  4 0 A64ICODE:MOVN,  5 0 A64ICODE:MOVZ,
   SYSCALL:NR-MMAP SYSCALL:SYS,
   5 DATA-VA VA>N A64ICODE:LIT64,  0 5 A64ICODE:CMP,
   A64ASM:C-EQ dvok A64ICODE:BCOND,
      3 DATA-SIZE A64ICODE:LIT64,
      s" hb: mapping the fixed data region failed; raise the address-space limit (ulimit -v) above its size in bytes:"
      EXIT-STATUS:SYSTEM 3 CRASH:DIE-LINE-VALUE,
   dvok A64ICODE:LBL, ;

;package

package RELOC-EMIT
public

\ Saves x0-x17, all MARK writes: its callers are mid-handler or mid-word.
: MARK-SAVE ( -- )
   A64ICODE:SP A64ICODE:SP 192 A64ICODE:SUBI,
   18 0 ?do i A64ICODE:SP i cells A64ICODE:STR, loop
   16 DATA-VA VA>N A64ICODE:LIT64,  16 A64ICODE:SP 184 A64ICODE:STR, ;

: MARK-RESTORE ( -- )
   18 0 ?do i A64ICODE:SP i cells A64ICODE:LDR, loop
   A64ICODE:SP A64ICODE:SP 192 A64ICODE:ADDI, A64ICODE:RET, ;

\ x16 = the table's base from SP+184: DATA-VA, never a task's x20, or the open
\ image's base for a cell inside it. SYS, clobbers x16: reload after each.
: MARK-HEADER ( -- )
   16 A64ICODE:SP 184 A64ICODE:LDR,
   4 RELOC:XTCELL-N-CELL A64ICODE:LIT64, 4 16 4 A64ICODE:ADD, ;

\ One owner serializes lookup and publication, including backing replacement.
\ x16 is the table's base; all registers touched here are MARK-SAVE'd.
: MARK-LOCK ( -- )
   A64ICODE:LBL {: retry:label :}
   17 16 ADDRESS-CELLS:LOCK-CELL A64ICODE:ADDI,
   7 1 A64ICODE:MOVZ,
   retry A64ICODE:LBL, 6 0 A64ICODE:MOVZ,
   6 7 17 A64ICODE:CASAL,
   6 retry A64ICODE:CBNZ, ;

: MARK-UNLOCK ( -- )
   17 16 ADDRESS-CELLS:LOCK-CELL A64ICODE:ADDI,
   6 0 A64ICODE:MOVZ, 6 17 A64ICODE:STLR, ;

\ Reacquire the row backing after SYS,. Only ordinals live in the index, so
\ moving this backing does not invalidate any entry.
: MARK-ROWS ( -- )
   5 4 ADDRESS-CELLS:BASE-FIELD A64ICODE:LDR,
   11 4 ADDRESS-CELLS:MODE-FIELD A64ICODE:LDR,
   A64ICODE:LBL {: mapped:label :}
   11 mapped A64ICODE:CBNZ, 5 16 5 A64ICODE:ADD, mapped A64ICODE:LBL, ;

\ x8 = owned index mapping; x10 = slot capacity on return. Requested bytes
\ and geometric rounding are bounded separately. An incoming snapshot pointer
\ is cleared without entering here: only this process can own the mapping.
: INDEX-SHAPE ( label -- ) {: bad:label :}
   8 0 A64ICODE:CMPI, A64ASM:C-LE bad A64ICODE:BCOND,
   6 8 7 A64ICODE:ANDI, 6 bad A64ICODE:CBNZ,
   7 $7FFFFFFFFFFFFFFF ADDRESS-CELLS:INDEX-HEADER - A64ICODE:LIT64,
   8 7 A64ICODE:CMP, A64ASM:C-HI bad A64ICODE:BCOND,
   10 8 ADDRESS-CELLS:INDEX-SLOTS A64ICODE:LDR,
   10 2 A64ICODE:CMPI, A64ASM:C-LT bad A64ICODE:BCOND,
   7 ADDRESS-CELLS:INDEX-MAX-SLOTS A64ICODE:LIT64, 10 7 A64ICODE:CMP, A64ASM:C-HI bad A64ICODE:BCOND,
   7 10 1 A64ICODE:SUBI, 6 10 7 A64ICODE:AND, 6 bad A64ICODE:CBNZ,
   6 $7FFFFFFFFFFFFFFF A64ICODE:LIT64, 6 6 8 A64ICODE:SUB,
   7 10 3 A64ICODE:LSLI, 7 7 ADDRESS-CELLS:INDEX-HEADER A64ICODE:ADDI,
   7 6 A64ICODE:CMP, A64ASM:C-HI bad A64ICODE:BCOND, ;

\ x8 = index, x10 = slots, x5 = current rows, x13 = row count, x12 = key.
\ A slot holds ordinal+1; compare the exact offset in that authoritative row.
\ Return x7 = tagged row on a hit, or x6 = empty slot / x11 = its bucket.
\ Probe at most the capacity even if a malformed derived table has no hole.
: INDEX-PROBE ( label label label -- ) {: hit:label miss:label bad:label :}
   A64ICODE:LBL {: probe:label :}
   7 $9E3779B97F4A7C15 A64ICODE:LIT64, 11 12 7 A64ICODE:MUL,
   7 11 32 A64ICODE:LSRI, 11 11 7 A64ICODE:EOR,
   17 10 1 A64ICODE:SUBI, 11 11 17 A64ICODE:AND, 14 10 0 A64ICODE:ADDI,
   probe A64ICODE:LBL,
      14 bad A64ICODE:CBZ,
      6 11 3 A64ICODE:LSLI, 6 8 6 A64ICODE:ADD, 6 6 ADDRESS-CELLS:INDEX-HEADER A64ICODE:ADDI,
      9 6 0 A64ICODE:LDR, 9 miss A64ICODE:CBZ,
      9 9 1 A64ICODE:SUBI, 9 13 A64ICODE:CMP, A64ASM:C-CS bad A64ICODE:BCOND,
      7 9 3 A64ICODE:LSLI, 7 5 7 A64ICODE:ADD, 7 7 0 A64ICODE:LDR,
      9 RELOC:XTCELL-OFF-MASK A64ICODE:LIT64, 9 7 9 A64ICODE:AND,
      9 12 A64ICODE:CMP, A64ASM:C-EQ hit A64ICODE:BCOND,
      11 11 1 A64ICODE:ADDI, 11 11 17 A64ICODE:AND,
      14 14 1 A64ICODE:SUBI, probe A64ICODE:B, ;

\ Build a replacement solely from the ordered rows. It is complete before the
\ old index is unmapped; failure leaves authoritative rows/header untouched.
\ Existing duplicate keys retain their first ordinal.
\ Stack scratch: 144 new slots, 152 new map, 160 incoming key, 168 old map.
: INDEX-BUILD ( label label label -- ) {: lookup:label full:label bad:label :}
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
   {: size:label sized:label mapped:label rows:label hole:label next:label
      built:label publish:label released:label :}
   8 A64ICODE:SP 168 A64ICODE:STR, 12 A64ICODE:SP 160 A64ICODE:STR,
   7 ADDRESS-CELLS:INDEX-MAX-SLOTS 2 / 1- A64ICODE:LIT64,
   13 7 A64ICODE:CMP, A64ASM:C-HI full A64ICODE:BCOND,
   7 13 1 A64ICODE:ADDI, 7 7 1 A64ICODE:LSLI, 10 2 A64ICODE:MOVZ,
   size A64ICODE:LBL, 10 7 A64ICODE:CMP, A64ASM:C-CS sized A64ICODE:BCOND,
      6 ADDRESS-CELLS:INDEX-MAX-SLOTS 2 / A64ICODE:LIT64,
      10 6 A64ICODE:CMP, A64ASM:C-HI full A64ICODE:BCOND,
      10 10 1 A64ICODE:LSLI, size A64ICODE:B,
   sized A64ICODE:LBL,
   10 A64ICODE:SP 144 A64ICODE:STR,
   0 0 A64ICODE:MOVZ, 1 10 3 A64ICODE:LSLI, 1 1 ADDRESS-CELLS:INDEX-HEADER A64ICODE:ADDI,
   2 3 A64ICODE:MOVZ, 3 SYSCALL:MAP-ANON-PRIVATE A64ICODE:LIT64, 4 0 A64ICODE:MOVN, 5 0 A64ICODE:MOVZ,
   SYSCALL:NR-MMAP SYSCALL:SYS, A64ASM:C-CS full A64ICODE:BCOND,
   0 mapped A64ICODE:CBNZ,
      1 A64ICODE:SP 144 A64ICODE:LDR, 1 1 3 A64ICODE:LSLI, 1 1 ADDRESS-CELLS:INDEX-HEADER A64ICODE:ADDI,
      SYSCALL:NR-MUNMAP SYSCALL:SYS, full A64ICODE:B,
   mapped A64ICODE:LBL,
   0 A64ICODE:SP 152 A64ICODE:STR, 8 0 0 A64ICODE:ADDI,
   MARK-HEADER MARK-ROWS
   10 A64ICODE:SP 144 A64ICODE:LDR,
   10 8 ADDRESS-CELLS:INDEX-SLOTS A64ICODE:STR,
   13 8 ADDRESS-CELLS:INDEX-COUNT A64ICODE:STR,
   3 0 A64ICODE:MOVZ,
   rows A64ICODE:LBL, 3 13 A64ICODE:CMP, A64ASM:C-CS built A64ICODE:BCOND,
      7 3 3 A64ICODE:LSLI, 7 5 7 A64ICODE:ADD, 12 7 0 A64ICODE:LDR,
      7 RELOC:XTCELL-OFF-MASK A64ICODE:LIT64, 12 12 7 A64ICODE:AND,
      next hole bad INDEX-PROBE
   hole A64ICODE:LBL, 7 3 1 A64ICODE:ADDI, 7 6 0 A64ICODE:STR,
   next A64ICODE:LBL, 3 3 1 A64ICODE:ADDI, rows A64ICODE:B,
   built A64ICODE:LBL,
   0 A64ICODE:SP 168 A64ICODE:LDR, 0 publish A64ICODE:CBZ,
   1 0 ADDRESS-CELLS:INDEX-SLOTS A64ICODE:LDR,
   1 1 3 A64ICODE:LSLI, 1 1 ADDRESS-CELLS:INDEX-HEADER A64ICODE:ADDI,
   SYSCALL:NR-MUNMAP SYSCALL:SYS, 0 released A64ICODE:CBZ,
      0 A64ICODE:SP 152 A64ICODE:LDR, 1 A64ICODE:SP 144 A64ICODE:LDR,
      1 1 3 A64ICODE:LSLI, 1 1 ADDRESS-CELLS:INDEX-HEADER A64ICODE:ADDI,
      SYSCALL:NR-MUNMAP SYSCALL:SYS, full A64ICODE:B,
   released A64ICODE:LBL,
   MARK-HEADER
   publish A64ICODE:LBL,
   8 A64ICODE:SP 152 A64ICODE:LDR, 8 16 ADDRESS-CELLS:INDEX-CELL A64ICODE:STR,
   12 A64ICODE:SP 160 A64ICODE:LDR,
   MARK-ROWS
   lookup A64ICODE:B, ;

\ The row was written and every fallible allocation/release is over. Publish
\ its derived entry and finally the live count while still holding the mutex.
: MARK-COMMIT-INDEX ( -- )
   8 16 ADDRESS-CELLS:INDEX-CELL A64ICODE:LDR,
   11 A64ICODE:SP 176 A64ICODE:LDR, 7 11 3 A64ICODE:LSLI,
   7 8 7 A64ICODE:ADD, 7 7 ADDRESS-CELLS:INDEX-HEADER A64ICODE:ADDI,
   13 13 1 A64ICODE:ADDI, 13 7 0 A64ICODE:STR,
   13 8 ADDRESS-CELLS:INDEX-COUNT A64ICODE:STR,
   13 4 0 A64ICODE:STR, ;

\ (MARK) spans [LMARK, end) as one sealed record: xt!'s direct BL lands on its entry.
: EMIT-MARK ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL
   {: common:label lookup:label full:label band:label kind:label
      ret:label shape:label mapped:label ready:label grow:label copy:label
      copied:label publish:label rebuild:label hit:label missing:label :}
   A64ICODE:LBL {: end:label :}
   s" (MARK)" LMARK A64ICODE:LABEL@ LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   LMARK A64ICODE:LABEL@ A64ICODE:LBL, MARK-SAVE
   15 0 A64ICODE:MOVZ, common A64ICODE:B,
   LSCRATCHMARK A64ICODE:LABEL@ A64ICODE:LBL, MARK-SAVE
   15 RELOC:XTCELL-DATA-TAG RELOC:XTCELL-SCRATCH-TAG or A64ICODE:LIT64,
   common A64ICODE:B,
   LPTRMARK A64ICODE:LABEL@ A64ICODE:LBL, MARK-SAVE
   15 RELOC:XTCELL-DATA-TAG A64ICODE:LIT64,
   \ x9 = the cell, unchanged on return; x15 = its kind.
   common A64ICODE:LBL,
   A64ICODE:LBL {: host:label :}
   6 XREG-RBASE BOOT:IMAGE-BASE-CELL A64ICODE:LDR,  6 host A64ICODE:CBZ,      \ x6 = the image's base
   9 6 A64ICODE:CMP,  A64ASM:C-CC host A64ICODE:BCOND,
   7 DATA-SIZE A64ICODE:LIT64,  7 6 7 A64ICODE:ADD,  9 7 A64ICODE:CMP,  A64ASM:C-CS host A64ICODE:BCOND,
   6 A64ICODE:SP 184 A64ICODE:STR,
   host A64ICODE:LBL,
   MARK-HEADER
   \ A cell outside DATA and the open image stops the process: a dropped row
   \ would leave a stale address in a saved image.
   12 9 16 A64ICODE:SUB,
   6 RELOC:XTCELL-OFF-MAX A64ICODE:LIT64, 12 6 A64ICODE:CMP, A64ASM:C-HI band A64ICODE:BCOND,
   MARK-LOCK
   6 4 ADDRESS-CELLS:MAGIC-FIELD A64ICODE:LDR,
   7 ADDRESS-CELLS:MAGIC A64ICODE:LIT64, 6 7 A64ICODE:CMP, A64ASM:C-NE shape A64ICODE:BCOND,
   13 4 0 A64ICODE:LDR, 10 4 ADDRESS-CELLS:CAP-FIELD A64ICODE:LDR,
   7 ADDRESS-CELLS:MAX-ROWS A64ICODE:LIT64,
   10 shape A64ICODE:CBZ, 10 7 A64ICODE:CMP, A64ASM:C-HI shape A64ICODE:BCOND,
   13 10 A64ICODE:CMP, A64ASM:C-HI shape A64ICODE:BCOND,
   5 4 ADDRESS-CELLS:BASE-FIELD A64ICODE:LDR,
   11 4 ADDRESS-CELLS:MODE-FIELD A64ICODE:LDR,
   11 1 A64ICODE:CMPI, A64ASM:C-EQ mapped A64ICODE:BCOND, A64ASM:C-HI shape A64ICODE:BCOND,
   6 DATA-SIZE A64ICODE:LIT64, 5 6 A64ICODE:CMP, A64ASM:C-HI shape A64ICODE:BCOND,
   6 6 5 A64ICODE:SUB, 6 6 3 A64ICODE:LSRI, 10 6 A64ICODE:CMP, A64ASM:C-HI shape A64ICODE:BCOND,
   5 16 5 A64ICODE:ADD, ready A64ICODE:B,
   mapped A64ICODE:LBL,
   5 0 A64ICODE:CMPI, A64ASM:C-LE shape A64ICODE:BCOND,
   6 5 7 A64ICODE:ANDI, 6 shape A64ICODE:CBNZ,
   6 $7FFFFFFFFFFFFFFF A64ICODE:LIT64, 6 6 5 A64ICODE:SUB, 6 6 3 A64ICODE:LSRI,
   10 6 A64ICODE:CMP, A64ASM:C-HI shape A64ICODE:BCOND,
   ready A64ICODE:LBL,
   8 16 ADDRESS-CELLS:INDEX-CELL A64ICODE:LDR, 8 rebuild A64ICODE:CBZ,
   shape INDEX-SHAPE
   7 8 ADDRESS-CELLS:INDEX-COUNT A64ICODE:LDR,
   7 13 A64ICODE:CMP, A64ASM:C-NE rebuild A64ICODE:BCOND,
   7 10 1 A64ICODE:LSRI, 13 7 A64ICODE:CMP, A64ASM:C-HI shape A64ICODE:BCOND,
   lookup A64ICODE:LBL,
   8 16 ADDRESS-CELLS:INDEX-CELL A64ICODE:LDR,
   10 8 ADDRESS-CELLS:INDEX-SLOTS A64ICODE:LDR,
   hit missing shape INDEX-PROBE
   \ A cell declared again keeps one row, since a second would relocate it
   \ twice; a second kind stops the process.
   hit A64ICODE:LBL,
      6 12 15 A64ICODE:ORR, 7 6 A64ICODE:CMP, A64ASM:C-EQ ret A64ICODE:BCOND, kind A64ICODE:B,
   missing A64ICODE:LBL,
      7 13 1 A64ICODE:ADDI, 6 10 1 A64ICODE:LSRI, 7 6 A64ICODE:CMP, A64ASM:C-HI rebuild A64ICODE:BCOND,
      11 A64ICODE:SP 176 A64ICODE:STR,
      10 4 ADDRESS-CELLS:CAP-FIELD A64ICODE:LDR,
      13 10 A64ICODE:CMP, A64ASM:C-EQ grow A64ICODE:BCOND,
      6 13 3 A64ICODE:LSLI, 6 5 6 A64ICODE:ADD, 7 12 15 A64ICODE:ORR, 7 6 0 A64ICODE:STR,
      MARK-COMMIT-INDEX ret A64ICODE:B,
   rebuild A64ICODE:LBL,
      lookup full shape INDEX-BUILD
   grow A64ICODE:LBL,
      \ Overflow checks precede doubling and the byte product. mmap and copying
      \ complete before publication; failure preserves the old header and rows.
      7 ADDRESS-CELLS:MAX-ROWS 2 / A64ICODE:LIT64,
      10 7 A64ICODE:CMP, A64ASM:C-HI full A64ICODE:BCOND,
      10 10 1 A64ICODE:LSLI, 10 A64ICODE:SP 144 A64ICODE:STR,
      0 0 A64ICODE:MOVZ, 1 10 3 A64ICODE:LSLI, 2 3 A64ICODE:MOVZ,
      3 SYSCALL:MAP-ANON-PRIVATE A64ICODE:LIT64, 4 0 A64ICODE:MOVN, 5 0 A64ICODE:MOVZ,
      SYSCALL:NR-MMAP SYSCALL:SYS, A64ASM:C-CS full A64ICODE:BCOND,
      0 copied A64ICODE:CBNZ,
      10 A64ICODE:SP 144 A64ICODE:LDR, 1 10 3 A64ICODE:LSLI, SYSCALL:NR-MUNMAP SYSCALL:SYS, full A64ICODE:B,
   copied A64ICODE:LBL,
      0 A64ICODE:SP 152 A64ICODE:STR, 17 0 0 A64ICODE:ADDI,
      MARK-HEADER
      5 4 ADDRESS-CELLS:BASE-FIELD A64ICODE:LDR,
      11 4 ADDRESS-CELLS:MODE-FIELD A64ICODE:LDR,
      A64ICODE:LBL {: source:label :} 11 source A64ICODE:CBNZ, 5 16 5 A64ICODE:ADD, source A64ICODE:LBL,
      14 0 A64ICODE:MOVZ,
   copy A64ICODE:LBL, 14 13 A64ICODE:CMP, A64ASM:C-CS publish A64ICODE:BCOND,
      6 14 3 A64ICODE:LSLI, 7 5 6 A64ICODE:ADD, 7 7 0 A64ICODE:LDR,
      6 17 6 A64ICODE:ADD, 7 6 0 A64ICODE:STR,
      14 14 1 A64ICODE:ADDI, copy A64ICODE:B,
   publish A64ICODE:LBL,
      \ An owned mapping is released only after the replacement is complete.
      \ A failed release discards the replacement and leaves the old view live.
      A64ICODE:LBL A64ICODE:LBL {: commit:label released:label :}
      11 commit A64ICODE:CBZ,
      0 5 0 A64ICODE:ADDI, 1 4 ADDRESS-CELLS:CAP-FIELD A64ICODE:LDR, 1 1 3 A64ICODE:LSLI,
      SYSCALL:NR-MUNMAP SYSCALL:SYS, 0 released A64ICODE:CBZ,
      0 A64ICODE:SP 152 A64ICODE:LDR, 1 A64ICODE:SP 144 A64ICODE:LDR, 1 1 3 A64ICODE:LSLI,
      SYSCALL:NR-MUNMAP SYSCALL:SYS, full A64ICODE:B,
   released A64ICODE:LBL,
      MARK-HEADER
   commit A64ICODE:LBL,
      5 A64ICODE:SP 152 A64ICODE:LDR, 10 A64ICODE:SP 144 A64ICODE:LDR,
      6 13 3 A64ICODE:LSLI, 6 5 6 A64ICODE:ADD, 7 12 15 A64ICODE:ORR, 7 6 0 A64ICODE:STR,
      5 4 ADDRESS-CELLS:BASE-FIELD A64ICODE:STR,
      10 4 ADDRESS-CELLS:CAP-FIELD A64ICODE:STR,
      11 1 A64ICODE:MOVZ, 11 4 ADDRESS-CELLS:MODE-FIELD A64ICODE:STR,
      MARK-COMMIT-INDEX ret A64ICODE:B,
   full A64ICODE:LBL,
      \ Every branch here leaves the header, rows and index whole, so the throw is
      \ safe to catch.
      MARK-HEADER MARK-UNLOCK
      9 -9855 A64ICODE:LIT64,  CRASH:LFAULTREC A64ICODE:LABEL@ A64ICODE:B,
   shape A64ICODE:LBL,
      s" E-XTCELL habu: invalid address-cell storage header" EXIT-STATUS:ADDRESS-CELL CRASH:DIE-LINE,
   band A64ICODE:LBL,
      s" E-XTBAND habu: address cell out of range" EXIT-STATUS:ADDRESS-CELL CRASH:DIE-LINE,
   kind A64ICODE:LBL,
      s" E-XTKIND habu: address cell kind mismatch" EXIT-STATUS:ADDRESS-CELL CRASH:DIE-LINE,
   ret A64ICODE:LBL, MARK-UNLOCK MARK-RESTORE
   end A64ICODE:LBL, ;

: BPTRCELLMARK ( -- )
   ENGINE-EMIT:A RT:G-POP
   LPTRMARK A64ICODE:LABEL@ A64ICODE:BL, ;

: BSCRATCHCELLMARK ( -- )
   ENGINE-EMIT:A RT:G-POP
   LSCRATCHMARK A64ICODE:LABEL@ A64ICODE:BL, ;

\ While a build writes an image, a store into this engine's header goes to the
\ same offset in the image, and marks the cell: its address goes to the cell
\ ROUTED-BYTES below it and to the cell below the image, in the band the build
\ maps there. Clobbers x12 and x13, as the store's guard does.
: ROUTE-HEADER ( n -- ) {: reg:n :}
   A64ICODE:LBL {: own:label :}
   13 reg XREG-RBASE A64ICODE:SUB,  12 DATA-START A64ICODE:LIT64,  13 12 A64ICODE:CMP,  A64ASM:C-CS own A64ICODE:BCOND,
   12 XREG-RBASE BOOT:IMAGE-BASE-CELL A64ICODE:LDR,  12 own A64ICODE:CBZ,
   reg 12 13 A64ICODE:ADD,
   13 12 8 A64ICODE:SUBI,  reg 13 0 A64ICODE:STR,
   12 ROUTED-BYTES A64ICODE:LIT64,  13 reg 12 A64ICODE:SUB,  reg 13 0 A64ICODE:STR,
   own A64ICODE:LBL, ;

\ xt! ( q ^q -- ): store an execution token into a persisted cell and declare
\ the cell. `defer` and `is` declare cells known at compile time; a table of
\ dispatch cells picks its cell at run time, so only the store knows. The
\ declaration runs first, so a cell LMARK rejects is never written; LMARK wants
\ the cell in x9, so the token is parked in x11 (scratch here; LMARK preserves
\ x9 and saves the rest). Registered framed: LMARK is a BL.
: BXTSTORE ( -- )
   ENGINE-EMIT:B RT:G-POP  ENGINE-EMIT:A RT:G-POP
   ENGINE-EMIT:B ROUTE-HEADER
   ENGINE-EMIT:C ENGINE-EMIT:A 0 A64ICODE:ADDI,                                      \ x11 = the token
   ENGINE-EMIT:A ENGINE-EMIT:B 0 A64ICODE:ADDI,                                      \ x9 = the cell, for LMARK
   LMARK A64ICODE:LABEL@ A64ICODE:BL,
   ENGINE-EMIT:C ENGINE-EMIT:B 0 A64ICODE:STR, ;

\ ptr-cell! ( a ^ -- ): BXTSTORE for a DATA-pointer cell.
: BPTRCELLSTORE ( -- )
   ENGINE-EMIT:B RT:G-POP  ENGINE-EMIT:A RT:G-POP
   ENGINE-EMIT:B ROUTE-HEADER
   ENGINE-EMIT:C ENGINE-EMIT:A 0 A64ICODE:ADDI,                                      \ x11 = the pointer
   ENGINE-EMIT:A ENGINE-EMIT:B 0 A64ICODE:ADDI,                                      \ x9 = the cell, for LPTRMARK
   LPTRMARK A64ICODE:LABEL@ A64ICODE:BL,
   ENGINE-EMIT:C ENGINE-EMIT:B 0 A64ICODE:STR, ;

;package

package ENGINE-EMIT

public

: EM-TEXT-SIZE ( -- )
   9 XREG-RBASE RBASE-CELL A64ICODE:LDR,  9 10 CRASH:TEXT-SIZE,  9 XREG-RBASE TEXT-SIZE-CELL A64ICODE:STR, ;

;package

\ The def-abort and stack-clear bodies; FPRIM frames x30 around each.
package DEFWRITE

public

: DEF-ABORT ( -- )
   ENGINE-EMIT:EM-ABORT-COMPILE-STATE ;

: STACK-CLEAR ( -- )
   A64ICODE:XDS XREG-RBASE S0-CELL A64ICODE:LDR, ;

;package

package ENGINE-EMIT
\ LSCOPEREC: the name lookup as one leaf for every reader but the call site, in
\ the call site's order: LFIND (open package, enclosing one, global, or
\ NAME:tail). In: x9/x10 = the token, preserved. Out: x14 = the bound record
\ (0 when nothing binds), x15 = flags: 1 when the bound record is a seeded
\ primitive. Clobbers x2-x17 as the leaves do.
\ Seeded is the identity of an operator row's word: the boot copies the
\ primitives' records to dict[0..LNCOUNT), and every later record (a package
\ word, an EXPORT alias, a redefinition after `undefine`) sits above. An
\ operator row may claim its token only when the lookup bound that
\ primitive's own record.
: EMIT-SCOPE-REC ( -- )
   A64ICODE:LBL {: notseed:label :}
   LSCOPEREC A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:SP A64ICODE:SP 32 A64ICODE:SUBI,  30 A64ICODE:SP 0 A64ICODE:STR,  9 A64ICODE:SP 8 A64ICODE:STR,  10 A64ICODE:SP 16 A64ICODE:STR,
   LFIND A64ICODE:LABEL@ A64ICODE:BL,                                       \ x5 = the record, 0 on a miss
   14 5 0 A64ICODE:ADDI,  15 0 A64ICODE:MOVZ,
   14 notseed A64ICODE:CBZ,
   12 LNCOUNT A64ICODE:LABEL@ A64ICODE:ADR,  12 12 0 A64ICODE:LDR,  13 DREC A64ICODE:MOVZ,  12 12 13 A64ICODE:MUL,  12 DBASE 12 A64ICODE:ADD,
   14 12 A64ICODE:CMP,  A64ASM:C-CS notseed A64ICODE:BCOND,
      15 1 A64ICODE:MOVZ,
   notseed A64ICODE:LBL,
   9 A64ICODE:SP 8 A64ICODE:LDR,  10 A64ICODE:SP 16 A64ICODE:LDR,                             \ LFIND moved them onto a qualified tail
   30 A64ICODE:SP 0 A64ICODE:LDR,  A64ICODE:SP A64ICODE:SP 32 A64ICODE:ADDI,
   A64ICODE:RET, ;

;package

package ENGINE-EMIT

private
\ The uncaught-throw exit, x9 = code: one in [1,255] exits as itself; any other prints
\ "hb: uncaught throw code <n>" on fd 2 and exits EXIT-STATUS:UNCAUGHT. No RET: leaf-safe.
: EM-UNCAUGHT ( -- )
   LUNCAUGHT A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:LBL LUNCRPT !  A64ICODE:LBL LUNCPOS !  A64ICODE:LBL LUNCLOOP !  A64ICODE:LBL LUNCDONE !
   0 9 0 A64ICODE:ADDI,
   \ One hook call serves both legs below. x0 is the trampoline's to preserve;
   \ x9 and x15 are re-derived from it because the hook is compiled code and
   \ owns every caller-saved register.
   LEXITHOOK A64ICODE:LABEL@ A64ICODE:BL,  9 0 0 A64ICODE:ADDI,  15 0 0 A64ICODE:ADDI,
   9 1 A64ICODE:CMPI,    A64ASM:C-LT LUNCRPT A64ICODE:LABEL@ A64ICODE:BCOND,
   9 255 A64ICODE:CMPI,  A64ASM:C-GT LUNCRPT A64ICODE:LABEL@ A64ICODE:BCOND,
   SYSCALL:NR-EXIT-GROUP SYSCALL:SYS,
   LUNCRPT A64ICODE:LABEL@ A64ICODE:LBL,
   1 LUNCMSG A64ICODE:LABEL@ A64ICODE:ADR,  0 2 A64ICODE:MOVZ,  2 UNCMSG-LEN A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,   \ "hb: uncaught throw code "
   9 15 0 A64ICODE:ADDI,
   A64ICODE:SP A64ICODE:SP $20 A64ICODE:SUBI,  12 A64ICODE:SP $20 A64ICODE:ADDI,
   13 $A A64ICODE:MOVZ,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,           \ trailing newline
   14 0 A64ICODE:MOVZ,  9 0 A64ICODE:CMPI,
   A64ASM:C-GE LUNCPOS A64ICODE:LABEL@ A64ICODE:BCOND,
   14 1 A64ICODE:MOVZ,  9 A64ICODE:SP 9 A64ICODE:SUB,                             \ x9 = -x9 (abs); x14 = sign flag
   LUNCPOS A64ICODE:LABEL@ A64ICODE:LBL,
   10 $A A64ICODE:MOVZ,
   LUNCLOOP A64ICODE:LABEL@ A64ICODE:LBL,                                \ unsigned: -MIN-N is MIN-N, magnitude 2^63
   11 9 10 A64ICODE:UDIV,  13 11 10 A64ICODE:MUL,  13 9 13 A64ICODE:SUB,
   13 13 $30 A64ICODE:ADDI,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,
   9 11 0 A64ICODE:ADDI,  9 LUNCLOOP A64ICODE:LABEL@ A64ICODE:CBNZ,
   14 LUNCDONE A64ICODE:LABEL@ A64ICODE:CBZ,
   13 $2D A64ICODE:MOVZ,  12 12 1 A64ICODE:SUBI,  13 12 0 A64ICODE:STRB,          \ leading '-'
   LUNCDONE A64ICODE:LABEL@ A64ICODE:LBL,
   0 2 A64ICODE:MOVZ,  1 12 0 A64ICODE:ADDI,  2 A64ICODE:SP $20 A64ICODE:ADDI,  2 2 12 A64ICODE:SUB,
   SYSCALL:NR-WRITE SYSCALL:SYS,                                       \ write(2, digits, len)
   0 EXIT-STATUS:UNCAUGHT A64ICODE:MOVZ,
   SYSCALL:NR-EXIT-GROUP SYSCALL:SYS, ;

: EM-RECOVER-EXITS ( -- )
   \ LFLOORREC: C-CRASH-DATA-RECOVER resumes a thread here, every other register
   \ as it faulted, after an access to the data stack's low guard page: a word
   \ took more cells than the stack held, and no cell under the base was
   \ touched. XDS may be under the base, so the stub empties the stack before
   \ anything runs on it (a catch restores XDS from its frame; the uncaught exit
   \ runs the exit hook on the empty stack). It throws -9851, E-UNDERFLOW: the
   \ fault is inside running code, maybe under a catch, which receives the throw.
   LFLOORREC A64ICODE:LABEL@ A64ICODE:LBL,
   A64ICODE:XDS XREG-RBASE STACK-ABI:BASE-CELL A64ICODE:LDR,
   9 -9851 A64ICODE:LIT64,  CRASH:LFAULTREC A64ICODE:LABEL@ A64ICODE:B, ;                \ -9851 = E-UNDERFLOW

\ LPLACE, a leaf (x0-x2, x9-x14 and SYS,'s registers; x15 kept), writes
\ `<path>:<line>: `, or `<path>: ` off the file's bytes, or nothing outside one.
: EM-SOURCE-PLACE ( -- )
   A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL A64ICODE:LBL {: noline:label nlloop:label nldone:label digloop:label done:label :}
   LPLACE A64ICODE:LABEL@ A64ICODE:LBL,
   9 XREG-RBASE SRCLOC:PATHLEN-CELL A64ICODE:LDR,  9 done A64ICODE:CBZ,             \ no open source file -> nothing
   2 XREG-RBASE SRCLOC:PATHLEN-CELL A64ICODE:LDR,  1 XREG-RBASE SRCLOC:PATH-CELL A64ICODE:LDR,  0 2 A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   10 XREG-RBASE SRCLOC:OWNED-CELL A64ICODE:LDR,  10 noline A64ICODE:CBZ,           \ x10 = the file's first owned byte; none -> the path alone
   11 XREG-RBASE SRCLOC:INB-CELL A64ICODE:LDR,  11 10 A64ICODE:CMP,  A64ASM:C-CC noline A64ICODE:BCOND,       \ the buffer starts before the file -> the path alone
   12 XREG-RBASE SRCLOC:OWNEDLEN-CELL A64ICODE:LDR,  12 10 12 A64ICODE:ADD,
   11 XREG-RBASE INE-CELL A64ICODE:LDR,  11 12 A64ICODE:CMP,  A64ASM:C-HI noline A64ICODE:BCOND,    \ the buffer ends past the file -> the path alone
   11 XREG-RBASE INP-CELL A64ICODE:LDR,
   12 1 A64ICODE:MOVZ,                                               \ x12 = line, 1-based
   nlloop A64ICODE:LBL,
      10 11 A64ICODE:CMP,  A64ASM:C-CS nldone A64ICODE:BCOND,
      13 10 0 A64ICODE:LDRB,  10 10 1 A64ICODE:ADDI,
      13 $A A64ICODE:CMPI,  A64ASM:C-NE nlloop A64ICODE:BCOND,
      12 12 1 A64ICODE:ADDI,  nlloop A64ICODE:B,
   nldone A64ICODE:LBL,
   \ ":" + decimal line, built backwards in a 32-byte stack scratch.
   A64ICODE:SP A64ICODE:SP $20 A64ICODE:SUBI,  14 A64ICODE:SP $20 A64ICODE:ADDI,
   9 $A A64ICODE:MOVZ,
   digloop A64ICODE:LBL,
      11 12 9 A64ICODE:UDIV,  13 11 9 A64ICODE:MUL,  13 12 13 A64ICODE:SUB,
      13 13 $30 A64ICODE:ADDI,  14 14 1 A64ICODE:SUBI,  13 14 0 A64ICODE:STRB,
      12 11 0 A64ICODE:ADDI,  12 digloop A64ICODE:CBNZ,
   13 $3A A64ICODE:MOVZ,  14 14 1 A64ICODE:SUBI,  13 14 0 A64ICODE:STRB,
   2 A64ICODE:SP $20 A64ICODE:ADDI,  2 2 14 A64ICODE:SUB,  1 14 0 A64ICODE:ADDI,  0 2 A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   A64ICODE:SP A64ICODE:SP $20 A64ICODE:ADDI,
   noline A64ICODE:LBL,
   0 2 A64ICODE:MOVZ,  1 LPLACEEND A64ICODE:LABEL@ A64ICODE:ADR,  2 PLACEEND-LEN A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   done A64ICODE:LBL,
   A64ICODE:RET, ;

\ In x0 = the exit or throw code, the diagnostic written without its newline.
: EM-COMPILE-DIE ( -- )
   A64ICODE:LBL {: ldie:label :}
   A64ICODE:LBL {: dpbad-end:label :}
   LCOMPILEDIE A64ICODE:LABEL@ A64ICODE:LBL,
   15 0 0 A64ICODE:ADDI,                                             \ x15 = code across the write
   0 2 A64ICODE:MOVZ,  1 LOPENNL A64ICODE:LABEL@ A64ICODE:ADR,  2 NL-LEN A64ICODE:MOVZ,  SYSCALL:NR-WRITE SYSCALL:SYS,
   9 XREG-RBASE EVALD-CELL A64ICODE:LDR,  9 ldie A64ICODE:CBZ,
      9 15 0 A64ICODE:ADDI,  CRASH:LFAULTREC A64ICODE:LABEL@ A64ICODE:B,
   ldie A64ICODE:LBL,
   0 15 0 A64ICODE:ADDI,
   SYSCALL:NR-EXIT-GROUP SYSCALL:SYS,
   EM-SOURCE-PLACE
   \ DP-CHECK found the DP outside the data space: it throws -9850, E-DATA-SPACE.
   LDPBAD A64ICODE:LABEL@ A64ICODE:LBL,
   9 -9850 A64ICODE:LIT64,  CRASH:LFAULTREC A64ICODE:LABEL@ A64ICODE:B,                    \ -9850 = E-DATA-SPACE
   dpbad-end A64ICODE:LBL,
   s" (LDPBAD)" LDPBAD A64ICODE:LABEL@ LABEL>N dpbad-end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   \ A writer of a record or a wid ran while a checker overlay was open, before
   \ it wrote anything: it throws -9852, E-OVERLAY-OPEN. Each code here is a
   \ literal so a host without src/lib/error.f's word still builds.
   OVERLAY-EMIT:LDIE A64ICODE:LABEL@ A64ICODE:LBL,
   9 -9852 A64ICODE:LIT64,  CRASH:LFAULTREC A64ICODE:LABEL@ A64ICODE:B, ;                  \ -9852 = E-OVERLAY-OPEN

\ Growing the body text block failed: it throws -9853, E-BODY-TEXT.
\ Nothing is stored before the grow fails, so the throw is safe to catch.
: EM-BODY-GROW-DIE ( -- )
   LBGROWFAIL A64ICODE:LABEL@ A64ICODE:LBL,
   9 -9853 A64ICODE:LIT64,  CRASH:LFAULTREC A64ICODE:LABEL@ A64ICODE:B, ;                  \ -9853 = E-BODY-TEXT

;package

package ENGINE-EMIT

\ The routines primitives and the Habu loop's helpers branch to.
: EMIT-ROUTINES ( -- )
   EM-RECOVER-EXITS
   EM-COMPILE-DIE
   EM-BODY-GROW-DIE
   EM-UNCAUGHT ;

;package

package ENGINE-EMIT
public
: EMIT-RESET-BUILDER ( -- )
   A64ICODE:ASM-INIT  ENGINE-PRIMS:RESET ;
;package

package LABELS

: CORE ( -- )
   A64ICODE:LBL ENGINE-EMIT:LFIND !  A64ICODE:LBL ENGINE-EMIT:LNUM !  A64ICODE:LBL ENGINE-EMIT:LDICT !
   A64ICODE:LBL ENGINE-EMIT:LTOK !  A64ICODE:LBL ENGINE-EMIT:LFLUSH !  A64ICODE:LBL ENGINE-EMIT:LFLUSHTO !  A64ICODE:LBL ENGINE-EMIT:LNCOUNT !
   A64ICODE:LBL ENGINE-EMIT:LBCS !
   A64ICODE:LBL ENGINE-EMIT:LHIDXADD !  A64ICODE:LBL ENGINE-EMIT:LHIDXBUILD !
   A64ICODE:LBL HIDX-EMIT:LREBUILD !  A64ICODE:LBL HIDX-EMIT:LFULL !  A64ICODE:LBL HIDX-EMIT:LREKEY !
   A64ICODE:LBL WLFIND:LENTRY !  A64ICODE:LBL WLFIND:LPAIR !  A64ICODE:LBL ENGINE-EMIT:LHEADROW !
   A64ICODE:LBL EM-DEF-OCC:LCOUNT !  A64ICODE:LBL EM-DEF-OCC:LAPPEND !  A64ICODE:LBL EM-DEF-OCC:LRESET !
   A64ICODE:LBL EM-DEF-OCC:LISSUE !  A64ICODE:LBL EM-DEF-OCC:LINIT !
   A64ICODE:LBL EM-DEF-OCC:LBEFORE !  A64ICODE:LBL EM-DEF-OCC:LAFTER !  A64ICODE:LBL EM-DEF-OCC:LMATCH !
   A64ICODE:LBL EM-DEF-OCC:LRECLAIM !
   A64ICODE:LBL RELOC-EMIT:LMARK !  A64ICODE:LBL RELOC-EMIT:LPTRMARK !  A64ICODE:LBL RELOC-EMIT:LSCRATCHMARK ! ;

: CONTROL ( -- )
   A64ICODE:LBL KWDATA:LKWTRUSTDECL !  A64ICODE:LBL KWDATA:LKWTRUSTRAW !
   A64ICODE:LBL ENGINE-EMIT:LSCOPEREC !
   A64ICODE:LBL DEFWRITE:LREPLAYTRAP ! ;

: RUNTIME ( -- )
   A64ICODE:LBL ENGINE-EMIT:LDOESPATCH !  A64ICODE:LBL RT:LGENIOOUT !
   A64ICODE:LBL ENGINE-EMIT:LEXITHOOK !
   A64ICODE:LBL CRASH:LCRASHH !  A64ICODE:LBL CRASH:LHANDLERS-END !  A64ICODE:LBL CRASH:LFAULTREC !  A64ICODE:LBL CRASH:LSIGH !  A64ICODE:LBL CRASH:LHEX !  A64ICODE:LBL CRASH:LDIE-LINE !  A64ICODE:LBL CRASH:LFAULT-PUT !  A64ICODE:LBL CRASH:LFAULT-OWNER !  A64ICODE:LBL CRASH:LFAULT-DEC !  A64ICODE:LBL CRASH:LHDR !  A64ICODE:LBL ENGINE-EMIT:LTRAPH !  A64ICODE:LBL ENGINE-EMIT:LBPH !  A64ICODE:LBL BP-CALLER:LBPLH !  A64ICODE:LBL ENGINE-EMIT:LBPSH !  A64ICODE:LBL ENGINE-EMIT:LBPWH !
   A64ICODE:LBL ENGINE-EMIT:LOPENNL !
   A64ICODE:LBL ENGINE-EMIT:LUNCAUGHT !  A64ICODE:LBL ENGINE-EMIT:LUNCMSG !
   A64ICODE:LBL ENGINE-EMIT:LFLOORREC !
   A64ICODE:LBL ENGINE-EMIT:LDPBAD !
   A64ICODE:LBL OVERLAY-EMIT:LDIE !
   A64ICODE:LBL ENGINE-EMIT:LBGROWFAIL !
   A64ICODE:LBL ENGINE-EMIT:LCOMPILEDIE !  A64ICODE:LBL ENGINE-EMIT:LPLACE !
   A64ICODE:LBL ENGINE-EMIT:LPLACEEND !  ;

: PROF ( -- )
   A64ICODE:LBL PROF:LPROFH !  A64ICODE:LBL PROF:LPROFDUMP !  A64ICODE:LBL PROF:LPROFFIND !  A64ICODE:LBL PROF:LPROFEDGE !
   A64ICODE:LBL PROF:LPROFJSON !  A64ICODE:LBL PROF:LPROFNUM !  A64ICODE:LBL PROF:LPROFPCT !
   A64ICODE:LBL PROF:LPROFNAME !  A64ICODE:LBL PROF:LPROFSYNC !
   A64ICODE:LBL PROF:LPROFROW !
   A64ICODE:LBL PROF-ABI:LPROFTRIM !  A64ICODE:LBL PROF-ABI:LPROFRECLAIM ! ;

public

: INIT ( -- )
   CORE
   CONTROL
   RUNTIME
   PROF ;

;package

package ENGINE-EMIT

\ bare-tok-imm? ( ^u8 n -- n ): live-dictionary immediate probe for the checker.
\ Runs the interpret dispatch's LFIND on a token name and pushes flags&2, the
\ DNAME-IMM bit, so DO-TOK1 can reject a live immediate as a checked body step.
\ LFIND clobbers x3-x16 and preserves XDS (x19)/XREG-RBASE (x20); the FPRIM frame
\ holds x30 across the BL.
: BTOKIMM ( -- )
   10 RT:G-POP  9 RT:G-POP                   \ x10 = len (TOS), x9 = addr: LFIND's token registers
   LFIND A64ICODE:LABEL@ A64ICODE:BL,                    \ x13 = found | imm<<1 | int flags
   9 13 2 A64ICODE:ANDI,
   A RT:G-PUSH ;
;package

package ENGINE-EMIT

\ Nonzero while KERNEL emits scope-kind? for a link that writes its entries
\ (LINK-SCOPE-ENTRIES); OPEN clears it.
variable LINKED-SCOPES
\ Kind k's chain label at cell k, k from 2 to 8.
create SCOPE-SITES 9 cells allot

\ Answers kind k for the entry in A when it equals kind k's entry, a linked
\ engine's from a chain. Every test binds its next label, so the emitted bytes
\ and labels do not depend on which entries a complete runtime binds.
: SCOPE-TEST ( n label -- )
   {: k:n done:label :}
   A64ICODE:LBL {: next:label :}
   LINKED-SCOPES @ 0<> if
      A64ICODE:LBL dup  SCOPE-SITES k cells +  A64ICODE:LABEL!  A64ICODE:LBL,
      \ the adrp + add LINKER's ARM64:CODE-ADDRESS! writes, 0 until written
      5 0 A64ICODE:MOVZ,  A64ICODE:NOP,
      5 next A64ICODE:CBZ,
      A 5 A64ICODE:CMP,  A64ASM:C-NE next A64ICODE:BCOND,
      A k A64ICODE:MOVZ,  done A64ICODE:B,
   then
   next A64ICODE:LBL, ;

\ Entry kinds are fixed by the link's chains in code, never by source
\ declarations or writable DATA. A restored alias has the same
\ code entry and thus the same kind.
: BSCOPE-KIND ( -- )
   A RT:G-POP
   A64ICODE:LBL {: done:label :}
   6 done SCOPE-TEST
   7 done SCOPE-TEST
   8 done SCOPE-TEST
   5 done SCOPE-TEST
   3 done SCOPE-TEST
   4 done SCOPE-TEST
   2 done SCOPE-TEST
   A 0 A64ICODE:MOVZ,
   done A64ICODE:LBL,
   A RT:G-PUSH ;

public

\ The next KERNEL emits scope-kind? to read each entry from a chain the link
\ writes at SCOPE-ENTRY-SITE after it places the product's code.
: LINK-SCOPE-ENTRIES ( -- ) -1 LINKED-SCOPES ! ;

\ The label of the chain holding kind k's entry, k from 2 to 8.
: SCOPE-ENTRY-SITE ( n -- label ) cells SCOPE-SITES + A64ICODE:LABEL@ ;

;package

package ENGINE-EMIT
\ bare-scope-bind ( ^u8 n -- ^n n ): LSCOPEREC's two answers for a token (the
\ bound record and the flags), so the checker, tier 0 and the Habu interpreter
\ bind a spelling to the record a compiled call binds.
: BSCOPEBIND ( -- )
   10 RT:G-POP  9 RT:G-POP                   \ x10 = len (TOS), x9 = addr
   LSCOPEREC A64ICODE:LABEL@ A64ICODE:BL,
   14 RT:G-PUSH  15 RT:G-PUSH ;

\ bare-scope-find: stage 2 deletes it once its callers move to bare-scope-bind.
: BSCOPEFIND ( -- )
   10 RT:G-POP  9 RT:G-POP                   \ x10 = len (TOS), x9 = addr
   LSCOPEREC A64ICODE:LABEL@ A64ICODE:BL,
   16 0 A64ICODE:MOVZ,
   14 RT:G-PUSH  16 RT:G-PUSH  16 RT:G-PUSH  15 RT:G-PUSH ;

: EMIT-PRIMITIVE-SECTIONS ( -- )
   EMIT-PRIMS
   s" does-patch" ['] DOESPATCH:PRIM FPRIM
   s" does-record" ['] DOES-REC:NATIVE-PRIM FPRIM
   s" namespace-record" ['] DEFWRITE:NAMESPACE-RECORD FPRIM
   s" namespace-private" ['] DEFWRITE:NAMESPACE-PRIVATE FPRIM
   s" alias-record" ['] DEFWRITE:ALIAS-RECORD FPRIM
   s" package-scope!" ['] DEFWRITE:PACKAGE-SCOPE FPRIM
   s" def-open" ['] DEFWRITE:DEF-OPEN FPRIM
   s" body-capture" ['] DEFWRITE:BODY-APPEND FPRIM
   s" trust-sig!" ['] DEFWRITE:TRUST-SIG FPRIM
   s" created-sig!" ['] DEFWRITE:CREATED-SIG FPRIM
   s" def-close" ['] DEFWRITE:DEF-CLOSE FPRIM
   s" def-abort" ['] DEFWRITE:DEF-ABORT FPRIM
   s" stack-clear" ['] DEFWRITE:STACK-CLEAR FPRIM
   s" imm-mark" ['] C-IMMEDIATE FPRIM
   s" replay-open" ['] DEFWRITE:REPLAY-OPEN FPRIM
   s" replay-close" ['] DEFWRITE:REPLAY-CLOSE FPRIM
   s" replay-widn!" ['] DEFWRITE:REPLAY-WIDN FPRIM
   s" replay-record" ['] DEFWRITE:REPLAY-RECORD FPRIM
   s" record-wid!" ['] DEFWRITE:RECORD-WID FPRIM
   s" record-rekey" ['] DEFWRITE:RECORD-REKEY FPRIM
   s" replay-private" ['] DEFWRITE:REPLAY-PRIVATE FPRIM
   s" DRAIN-PRETRUST" ['] BDRAINPRETRUST FPRIM
   s" bare-tok-imm?" ['] BTOKIMM FPRIM
   s" bare-scope-find" ['] BSCOPEFIND FPRIM
   s" bare-scope-bind" ['] BSCOPEBIND FPRIM
   s" scope-kind?" ['] BSCOPE-KIND FPRIM
   s" xt!" ['] RELOC-EMIT:BXTSTORE FPRIM
   s" ptr-cell-mark" ['] RELOC-EMIT:BPTRCELLMARK FPRIM
   s" scratch-cell-mark" ['] RELOC-EMIT:BSCRATCHCELLMARK FPRIM
   s" ptr-cell!" ['] RELOC-EMIT:BPTRCELLSTORE FPRIM
   PROF:EMIT-PROF-PRIMS
   EMIT-FP-PRIMS
   ENGINE-PRIMS:COMPLETE
   EMIT-BCAP
   EMIT-TOK
   EMIT-FLUSH
   EMIT-FIND
   WLFIND:EMIT
   EMIT-HEAD-ROW
   EMIT-SCOPE-REC
   DEFWRITE:EMIT-REPLAY-TRAP
   EMIT-HIDX
   EMIT-DEF-OCC
   EMIT-DEF-OCC-OVERLAP
   EMIT-DEF-OCC-RECLAIM
   EMIT-NUM
   EMIT-EXITHOOK ;

: EMIT-DICTIONARY-SECTIONS ( -- )
   DOESPATCH:EMIT
   RELOC-EMIT:EMIT-MARK
   KWDATA:EMIT ;

\ [LCRASHH, LHANDLERS-END) holds the signal handlers and no code Habu code calls. Where an
\ emitter here runs into the next, that next is code a stripped program keeps anyway.
: EMIT-RUNTIME-SECTIONS ( -- )
   EMIT-GENIO-OUT
   PROF:EMIT-PROFFIND
   PROF:EMIT-PROFEDGE
   PROF:EMIT-PROF-REPORTS
   EMIT-FAULT-REC
   CRASH:EMIT-CRASH-HANDLER
   CRASH:EMIT-SIGNAL-HANDLER
   PROF:EMIT-PROF
   EMIT-TRAPH
   CRASH:LHANDLERS-END A64ICODE:LABEL@ A64ICODE:LBL,
   CRASH:EMIT-HEX
   CRASH:EMIT-DIE-LINE
   CRASH:EMIT-FAULT-RECORD ;

: EMIT-CODE-SECTIONS ( -- )
   EMIT-ROUTINES
   EMIT-PRIMITIVE-SECTIONS
   EMIT-DICTIONARY-SECTIONS
   EMIT-RUNTIME-SECTIONS
   EMIT-DICT ;

public

: OPEN ( -- )
   \ An emission chooses no scoped entry; a link binds entries through
   \ LINK-SCOPE-ENTRIES.
   0 LINKED-SCOPES !
   EMIT-RESET-BUILDER
   LABELS:INIT ;

: KERNEL ( -- )
   EMIT-CODE-SECTIONS ;

;package
