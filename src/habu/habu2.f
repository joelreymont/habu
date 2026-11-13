\ habu2.f — engine-builder part 2: the kernel's routines (definition writers,
\ snapshot relocation, compile-error tails), the primitive sections and kernel
\ emission. Needs habu1.f (part 1). Emission is split into phase words sharing
\ label VARIABLES (a giant single word would need dozens of locals); emission
\ order is stable so the self-rebuild reaches a fixpoint.
\
\ Every address literal is a fixed MOVZ/MOVK chain whose site the address map
\ records: a DATA address uses the three-half carrier, any other the absolute
\ one. DATA chains are recorded too: an AOT capture copies the blob to a
\ different DP and must find them. The map stores no kind: a consumer derives it
\ from which of its window's disjoint DATA and code spans the value lands in.
\
\ A does> clause gets a record of its own. LDOESPATCH patches the created
\ word's RET into `b D`, D inside the defining word's body: a raw PC-relative
\ branch the AOT seed cannot relocate (a capture whose definer lies outside the
\ window branched into another word). As a record, D is an entry the seed
\ resolves by name like every BL; a name plus byte delta is refused, since a
\ captured branch must not depend on a callee's interior. The record overlaps
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
\ caller hands, replay-widn!'s, the band bounds below. While the latch is
\ set, replay-record and namespace-record raise HW as they publish. The owner
\ publishes through nothing else, nothing while a definition is pending, and
\ lowers NDICT only by its own rollback, so [the saved NDICT, HW) is the
\ overlay's and a record above HW is another writer's. Every other record or
\ wid writer dies while the latch is set, so a definition pends in an open
\ overlay only through a raw store to PEND-CELL, which reaches the
\ pending-definition refusals, as a raw ndict! reaches the HW one.
require src/lib/fmt.f
require src/habu/address-cells.f
require src/habu/address-carrier.f
require src/habu/snapshot-format.f
require src/habu/code-span.f
require src/habu/aot-runtime.f
require src/habu/native-body-abi.f
require src/compiler/fixed-body.f
require src/arch/arm64/icode.f
using A64ASM
using A64ICODE
\ The AOT capture buffers, their caps and the section budget are src/habu/aot-decl.f,
\ loaded before this file so bin/hb's capture and this emitter share one
\ declaration of the format.
using AOT-BUF
using CRASH
using RT
using SYSCALL

package ENGINE-EMIT
private

\ A carrier word with its destination field cleared: the chain emitters OR in
\ the register x14 names.
: RD-FREE ( n -- n )
   ADDRESS-CARRIER:ADDR-RD-BITS rshift ADDRESS-CARRIER:ADDR-RD-BITS lshift ;

\ Fixed four-instruction MOVZ/MOVK chain of x11 into the register x14 names: the
\ relocatable form the AOT boot pass rewrites in place.
: C-ADDR-RAW ( -- )
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 RD-FREE LIT64,  8 8 14 ORR,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 RD-FREE LIT64,  8 8 14 ORR,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 RD-FREE LIT64,  8 8 14 ORR,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 RD-FREE LIT64,  8 8 14 ORR,  9 8 7 ORR,  LCEMIT LABEL@ BL, ;

\ Shared DATA must not use x20, which changes per task. Both fixed DATA mappings
\ fit three halves; high-to-low order distinguishes these from full addresses.
\ The chain loads the register x14 names.
: C-DATA-RAW ( -- )
   DATA-VA VA>N DATA-SIZE + $FFFFFFFFFFFF > if C-ADDR-RAW exit then
   LBL LBL {: absolute:label done:label :}
   5 DATA-VA VA>N LIT64,  6 11 5 SUB,  5 DATA-SIZE LIT64,
   6 5 CMP,  C-HI absolute BCOND,
   5 $FFFF MOVZ,
   7 11 32 LSRI,  7 7 5 AND,  7 7 5 LSLI,  8 ADDRESS-CARRIER:DATA-MOVZ2 LIT64,  8 8 14 ORR,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 11 16 LSRI,  7 7 5 AND,  7 7 5 LSLI,  8 W-MOVK1 RD-FREE LIT64,  8 8 14 ORR,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   7 11 5 AND,  7 7 5 LSLI,  8 ADDRESS-CARRIER:DATA-MOVK0 LIT64,  8 8 14 ORR,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   done B,
   absolute LBL, C-ADDR-RAW
   done LBL, ;
;package
using ENGINE-EMIT
\ C-DATA-ADDR and C-DATA-ADDR-RAW sit after the snapshot-relocation labels: both
\ record a site through RELOC-EMIT:MARK-SITE.

package ENGINE-EMIT
public
variable LTRAPH  variable LBPH  variable LBPSH  variable LBPWH
variable LOPENNL
public
variable LUNCAUGHT   variable LUNCMSG   \ uncaught-top-level-throw reporter + its fd-2 message
private
variable LUNCRPT   variable LUNCPOS   variable LUNCLOOP   variable LUNCDONE   \ reporter branch + itoa labels
: UNCMSG$ ( -- ptr u8 n )   s" hb: uncaught throw code " ;
: UNCMSG-LEN ( -- n )   UNCMSG$ nip ;
public

variable LMINCLASS   variable LMINMSG   \ the underdepth class head and prose LFLOORREC writes
private
: MINCLASS$ ( -- ptr u8 n )   s" E-UNDERFLOW habu: " ;
: MINMSG$ ( -- ptr u8 n )   s" interpret stack underdepth: " ;
: MINCLASS-LEN ( -- n )   MINCLASS$ nip ;
: MINMSG-LEN ( -- n )   MINMSG$ nip ;
public
variable LFLOORREC   \ the underdepth throw for a data access below the base

\ Capacity refusals that state a count: one line naming what filled up, the
\ ceiling and the count.
variable LDIAGU     \ ( x9 = value ) unsigned decimal on fd 2, no newline
variable LBGROWCLASS   variable LBGROWMSG   variable LBGROWUNIT   \ the body text block could not grow
variable LDPBADCLASS   variable LDPBADMSG   variable LDPBADOF   variable LDPBADUNIT   \ DP-heap bound reject: the class head, the prose before the DP's offset from the heap floor, the " of " before the ceiling's and the " bytes" that ends the line

private
: DPBAD-CLASS$ ( -- ptr u8 n )   s" E-DATA-SPACE habu: " ;
: DPBAD-MSG$ ( -- ptr u8 n )     s" data space out of range: heap byte " ;
: DPBAD-OF$ ( -- ptr u8 n )      s"  of " ;
: DPBAD-UNIT$ ( -- ptr u8 n )    s"  bytes" ;
: BGROW-CLASS$ ( -- ptr u8 n )   s" E-BODY-TEXT habu: " ;
: BGROW-MSG$ ( -- ptr u8 n )     s" cannot grow definition body text to " ;
: BGROW-UNIT$ ( -- ptr u8 n )    s"  bytes" ;

: DPBAD-CLASS-LEN ( -- n )   DPBAD-CLASS$ nip ;
: BGROW-CLASS-LEN ( -- n )   BGROW-CLASS$ nip ;
: DPBADMSG-LEN ( -- n )      DPBAD-MSG$ nip ;
: DPBAD-OF-LEN ( -- n )      DPBAD-OF$ nip ;
: DPBAD-UNIT-LEN ( -- n )    DPBAD-UNIT$ nip ;
: BGROW-MSG-LEN ( -- n )     BGROW-MSG$ nip ;
: BGROW-UNIT-LEN ( -- n )    BGROW-UNIT$ nip ;
public

variable LCOMPILEDIE   \ the die sites' tail, x0 = code: ends their line, then throws x0 inside evaluate (LEVALREC), else exits x0
variable LPLACE        \ the place writer each die site calls after its class head
;package
\ Snapshot relocation labels and messages; the band
\ offsets and exit statuses are src/habu/layout.f package SNAP-RELOC. This file
\ compiles on a product engine, where no source reopens a baked package, so the
\ emitter's half is its own package.
package RELOC-EMIT
public
variable LXTMSG     \ checked growth could not secure storage
: XTMSG$ ( -- ptr u8 n )   S\" hb: address-cell storage allocation failed\n" ;
: XTMSG-LEN ( -- n )   XTMSG$ nip ;
variable LMARK      \ declare one DATA cell as holding a region address
variable LPTRMARK   \ declare one DATA cell as holding a DATA pointer
variable LSCRATCHMARK \ declare one DATA cell as a scratch pointer, saved as null
variable LROLLBACK  \ retire address rows at or above the saved DATA cut
variable LADDRSITE  \ record the address-literal chain whose first word is in x9

\ Record an address chain whose first instruction is held in the named register.
\ Compile handlers use CP; boot relocation uses the captured site's address.
: MARK-SITE-AT ( n -- ) {: site:n :}
   9 site 0 ADDI,
   LADDRSITE LABEL@ BL, ;

\ The compile handler records CP before emitting the chain's first instruction.
: MARK-SITE ( -- )
   CP MARK-SITE-AT ;

: EMIT-FULL-MESSAGE ( -- )
   LXTMSG LABEL@ LBL, XTMSG$ BYTES, ;

;package

package ENGINE-EMIT
public
: C-DATA-ADDR ( -- )
   RELOC-EMIT:MARK-SITE
   14 9 MOVZ,  C-DATA-RAW
   9 W-PUSH9 LIT64,  LCEMIT LABEL@ BL, ;
private
\ x9, no push: the defer dispatch-cell address.
: C-DATA-ADDR-RAW ( -- )
   RELOC-EMIT:MARK-SITE
   14 9 MOVZ,  C-DATA-RAW ;

$D63F0200 constant C-CALL-BLR-X16     \ blr x16: deferred-word indirect call

public

variable LPLACEEND            \ ": " — what LPLACE writes after <path> or <path>:<line>
private
: PLACEEND$ ( -- ptr u8 n )   s" : " ;
: PLACEEND-LEN ( -- n )   PLACEEND$ nip ;

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
   HB-TARGET-LINUX? IF 9 2 LINUX-UC-MCTX-OFF ADDI, exit THEN
   9 4 MCTX-OFF LDR, ;

: C-MCTX-PC>R10 ( -- )
   HB-TARGET-LINUX? IF 10 9 LINUX-MCTX-PC-OFF LDR, exit THEN
   10 9 MACOS-MCTX-PC-OFF LDR, ;

: C-MCTX-X19>R12 ( -- )
   HB-TARGET-LINUX? IF 12 9 LINUX-MCTX-X19-OFF LDR, exit THEN
   12 9 MACOS-MCTX-X19-OFF LDR, ;
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
   HB-TARGET-LINUX? IF 9 9 LINUX-MCTX-LR-OFF LDR, exit THEN
   9 9 MACOS-MCTX-LR-OFF LDR, ;
;package

package ENGINE-EMIT
: C-MCTX-SP-16! ( -- )
   HB-TARGET-LINUX? IF
      12 9 LINUX-MCTX-SP-OFF LDR,  12 12 16 SUBI,  12 9 LINUX-MCTX-SP-OFF STR, exit
   THEN
   12 9 MACOS-MCTX-SP-OFF LDR,  12 12 16 SUBI,  12 9 MACOS-MCTX-SP-OFF STR, ;

: C-MCTX-PC+4! ( -- )
   HB-TARGET-LINUX? IF
      12 9 LINUX-MCTX-PC-OFF LDR,  12 12 4 ADDI,  12 9 LINUX-MCTX-PC-OFF STR, exit
   THEN
   12 9 MACOS-MCTX-PC-OFF LDR,  12 12 4 ADDI,  12 9 MACOS-MCTX-PC-OFF STR, ;

: C-BP-HIT-SAVE ( -- )
   SP SP 80 SUBI,
   1 SP 0 STR,  4 SP 8 STR,  5 SP 16 STR,
   9 SP 24 STR,  10 SP 32 STR,  8 SP 40 STR,
   14 8 16 LDR,  14 14 1 ADDI,  14 8 16 STR,
   15 8 24 LDR,  12 15 1 LSRI, ;

: C-BP-STACK-RANGE ( -- )
   14 DATA STACK-ABI:BASE-CELL LDR,
   9 SP 24 LDR,  C-MCTX-X19>R12
   12 SP 56 STR,  17 14 0 ADDI, ;

: C-BP-PRINT-HIT ( -- )
   LBL {: empty:label :}
   1 LBPH LABEL@ ADR,  0 2 MOVZ,  2 BPH-LEN MOVZ,  NR-WRITE SYS,
   9 SP 32 LDR,  LHEX LABEL@ BL,
   \ An empty interrupted stack has no top cell to print.
   C-BP-STACK-RANGE
   12 17 CMP, C-EQ empty BCOND,
   9 12 8 SUBI,  9 9 0 LDR,  LHEX LABEL@ BL,
   empty LBL,
   1 BP-CALLER:LBPLH LABEL@ ADR,  0 2 MOVZ,  2 BP-CALLER:BPL-LEN MOVZ,  NR-WRITE SYS,
   9 SP 24 LDR,  BP-CALLER:C-MCTX-LR>R9  LHEX LABEL@ BL,
   1 LBPSH LABEL@ ADR,  0 2 MOVZ,  2 BPS-LEN MOVZ,  NR-WRITE SYS, ;

: C-BP-WATCH-HEAD ( -- )
   1 LBPWH LABEL@ ADR,  0 2 MOVZ,  2 BPW-LEN MOVZ,  NR-WRITE SYS,
   6 DATA BPWN-CELL LDR,  7 DATA BPWBASE-CELL LDR,
   17 0 MOVZ, ;

: C-BP-WATCH-ROW ( -- )
   22 17 3 LSLI,  22 7 22 ADD,  23 22 0 LDR,
   9 23 0 ADDI,  LHEX LABEL@ BL,
   9 23 0 LDR,  LHEX LABEL@ BL,
   17 17 1 ADDI, ;

: C-BP-RESTORE-ONESHOT ( -- )
   8 SP 40 LDR,  11 8 0 LDR,  12 8 8 LDR,
   13 DATA REGION-ALIAS:DELTA-CELL LDR,  13 11 13 ADD,  12 13 0 STRW,
   9 11 0 ADDI,  LFLUSH LABEL@ BL,
   8 SP 40 LDR,  12 0 MOVZ,  12 8 0 STR, ;

\ Store the interrupted thread's link register at its (already lowered) stack
\ pointer: the second half of the pre-indexed save, in the ucontext.
: C-MCTX-LINK>SLOT ( -- )
   HB-TARGET-LINUX? IF
      12 9 LINUX-MCTX-SP-OFF LDR,  14 9 LINUX-MCTX-LR-OFF LDR,  14 12 0 STR, exit
   THEN
   12 9 MACOS-MCTX-SP-OFF LDR,  14 9 MACOS-MCTX-LR-OFF LDR,  14 12 0 STR, ;

\ The opcode classes of an instruction that reads or sets PC, each as
\ (word >> shift) [& mask] = value. The breakpoint handler below refuses to run
\ such an instruction away from its address.
0 ENC-B 26 rshift                constant CINL-B-OP
0 C-EQ ENC-BCOND 24 rshift       constant CINL-BCOND-OP
0 0 ENC-CBZ 25 rshift $3F and    constant CINL-CBZ-OP    \ cbz, cbnz, either width
0 0 ENC-ADR 24 rshift $1F and    constant CINL-ADR-OP    \ adr, adrp
$1B constant CINL-TBZ-OP         \ tbz, tbnz, either width: bits 30:25 = 011011
$3B constant CINL-LDRLIT-MASK    \ ldr, ldrsw, prfm (literal), any register file:
$18 constant CINL-LDRLIT-OP      \   bits 29:27 = 011 and 25:24 = 00

: C-MCTX-PC<R12 ( -- )
   HB-TARGET-LINUX? IF 12 9 LINUX-MCTX-PC-OFF STR, exit THEN
   12 9 MACOS-MCTX-PC-OFF STR, ;

\ Branch to `rel` when x13, the displaced instruction, reads or sets PC: the
\ PC-relative classes, and blr, whose return address would be the trampoline's.
\ Clobbers x14, x15.
: C-BP-PCREL ( label -- ) {: rel:label :}
   14 13 26 LSRI,  14 14 $1F ANDI,  14 CINL-B-OP CMPI,  C-EQ rel BCOND,   \ b, bl
   14 13 24 LSRI,  14 CINL-BCOND-OP CMPI,  C-EQ rel BCOND,
   14 13 25 LSRI,  14 14 $3F ANDI,  14 CINL-CBZ-OP CMPI,  C-EQ rel BCOND,
   14 CINL-TBZ-OP CMPI,  C-EQ rel BCOND,
   14 13 24 LSRI,  14 14 $1F ANDI,  14 CINL-ADR-OP CMPI,  C-EQ rel BCOND,
   14 13 24 LSRI,  15 CINL-LDRLIT-MASK MOVZ,  14 14 15 AND,
   14 CINL-LDRLIT-OP CMPI,  C-EQ rel BCOND,
   15 $FFFFFC1F LIT64,  14 13 15 AND,  15 0 ENC-BLR LIT64,  14 15 CMP,  C-EQ rel BCOND, ;

\ Resume at the slot's trampoline: [BPTRAMP-CELL] + index * 8, and the index * 8
\ is the slot's offset in BPTAB, 32 bytes a slot, shifted right by two.
: C-BP-TRAMP ( -- )
   14 8 DATA SUB,  15 BPTAB-OFF LIT64,  14 14 15 SUB,  14 14 2 LSRI,
   12 DATA BPTRAMP-CELL LDR,  12 12 14 ADD,
   C-MCTX-PC<R12 ;

\ A persistent breakpoint keeps its BRK, so the handler runs the displaced
\ instruction. The link-register save and the nop EM-COMPILE-RET leaves in a
\ leaf's slot are emulated; any other instruction runs in the slot's trampoline
\ (the instruction, then `b` back). One that reads or sets PC (C-BP-PCREL)
\ degrades the breakpoint to one-shot. The emulated `str x30,[sp,#-16]!` lands
\ in this handler's signal frame, over the arm64 frame_record {fp, lr}: nothing
\ reads it back (sigreturn restores from uc_mcontext, lower in the frame; the
\ handler addresses locals off sp, never x29), and that address is the slot the
\ resumed word's epilogue reads.
: C-BP-EMULATE ( -- )
   LBL LBL LBL LBL {: framed:label step:label oneshot:label done:label :}
   9 SP 24 LDR,                                       \ the interrupted mcontext
   8 SP 40 LDR,  13 8 8 LDR,                          \ the word the BRK replaced
   14 W-LINKSAVE LIT64,  13 14 CMP,  C-EQ framed BCOND,
   14 W-NOP LIT64,  13 14 CMP,  C-EQ step BCOND,
   oneshot C-BP-PCREL
      C-BP-TRAMP
      done B,
   oneshot LBL,
      \ Give the word its instruction back and resume ON it: the breakpoint
      \ degrades to one-shot, a visible loss, where running the instruction at
      \ the wrong address is a wrong result.
      C-BP-RESTORE-ONESHOT
      done B,
   framed LBL,
      C-MCTX-SP-16!
      C-MCTX-LINK>SLOT
   step LBL,
      C-MCTX-PC+4!
   done LBL, ;

: C-BP-SCAN ( label label label label -- )
   {: tno:label bscan:label bnext:label bhit:label :}
   6 8 MOVZ,  7 0 MOVZ,
   bscan LBL,
      7 6 CMP,  C-GE tno BCOND,
      8 7 5 LSLI,  14 BPTAB-OFF LIT64,  8 8 14 ADD,  8 DATA 8 ADD,
      13 8 0 LDR,  13 bnext CBZ,
      10 13 CMP,  C-EQ bhit BCOND,
      bnext LBL,  7 7 1 ADDI,  bscan B, ;

: C-BP-STACK-DUMP ( label label -- )
   {: sdump:label sdone:label :}
   sdump LBL,
      14 SP 56 LDR,  17 14 CMP,  C-CS sdone BCOND,
      9 17 0 LDR,  17 SP 48 STR,  LHEX LABEL@ BL,
      17 SP 48 LDR,  17 17 8 ADDI,  sdump B,
   sdone LBL, ;

: C-BP-WATCH-DUMP ( label label -- )
   {: wloop:label wdone:label :}
   6 DATA BPWN-CELL LDR,  6 wdone CBZ,
   7 DATA BPWBASE-CELL LDR,  7 wdone CBZ,
   C-BP-WATCH-HEAD
   wloop LBL,
      17 6 CMP,  C-GE wdone BCOND,
      C-BP-WATCH-ROW  wloop B,
   wdone LBL, ;

\ LTRAPH, the trap signal entry: a BPTAB hit is reported and resumed (one-shot
\ restores the instruction, persistent runs C-BP-EMULATE); any other trap falls
\ through to the crash dump with x2/x4 untouched.
: EMIT-TRAPH ( -- )
   LTRAPH LABEL@ LBL,
   LBL {: tno :}
   C-TRAP-MCTX>R9
   C-MCTX-PC>R10
   LBL {: bscan :}  LBL {: bnext :}  LBL {: bhit :}
   LBL {: sdump :}  LBL {: sdone :}  LBL {: wloop :}  LBL {: wdone :}
   LBL {: emu :}  LBL {: fin :}
   tno bscan bnext bhit C-BP-SCAN                    \ scan BPTAB[0..8)
   \ slot layout: +0 addr  +8 saved-instr  +16 hits  +24 ctrl(skip<<1 | persist)
   bhit LBL,                                         \ x8=&slot x9=mctx x10=pc
   C-BP-HIT-SAVE                                     \ x15=ctrl  x12=skip
   14 12 CMP,  C-LS emu BCOND,                       \ hits <= skip -> silent, just emulate
   C-BP-PRINT-HIT
   C-BP-STACK-RANGE
   sdump sdone C-BP-STACK-DUMP
   wloop wdone C-BP-WATCH-DUMP
   8 SP 40 LDR,  15 8 24 LDR,  15 15 1 ANDI,  15 emu CBNZ,   \ persistent -> emulate, keep BRK
   C-BP-RESTORE-ONESHOT                              \ clear slot addr; resume re-runs orig
   fin B,
   emu LBL,
   C-BP-EMULATE
   fin LBL,
   0 SP 8 LDR,  1 SP 0 LDR,  2 SP 16 LDR,  SP SP 80 ADDI,
   NR-SIGRETURN SYS,                                 \ sigreturn(uctx, infostyle, token)
   tno LBL,
   LCRASHH LABEL@ B,
   LBPH LABEL@ LBL,  BPH-KW BPH-LEN BYTES,
   BP-CALLER:LBPLH LABEL@ LBL, BP-CALLER:BPL-KW BP-CALLER:BPL-LEN BYTES,
   LBPSH LABEL@ LBL, BPS-KW BPS-LEN BYTES,
   LBPWH LABEL@ LBL, BPW-KW BPW-LEN BYTES,
   LOPENNL LABEL@ LBL, NL-KW NL-LEN BYTES,
   LUNCMSG LABEL@ LBL, UNCMSG$ BYTES,                  \ LUNCAUGHT appends the signed code + newline
   LMINCLASS LABEL@ LBL, MINCLASS$ BYTES,
   LMINMSG LABEL@ LBL, MINMSG$ BYTES,                  \ LFLOORREC appends the token + newline
   LDPBADCLASS LABEL@ LBL, DPBAD-CLASS$ BYTES,
   LDPBADMSG LABEL@ LBL, DPBAD-MSG$ BYTES,
   LDPBADOF LABEL@ LBL, DPBAD-OF$ BYTES,
   LDPBADUNIT LABEL@ LBL, DPBAD-UNIT$ BYTES,
   LBGROWCLASS LABEL@ LBL, BGROW-CLASS$ BYTES,
   LBGROWMSG LABEL@ LBL, BGROW-MSG$ BYTES,                               \ EM-BODY-GROW-DIE appends the capacity tried and the unit; LCOMPILEDIE the newline
   LBGROWUNIT LABEL@ LBL, BGROW-UNIT$ BYTES,
   RELOC-EMIT:EMIT-FULL-MESSAGE
   LPLACEEND LABEL@ LBL, PLACEEND$ BYTES, ;

: C-TARGET-UNKNOWN ( -- )
   s" hb: unknown target" 76 die ;

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
   LBL LBL LBL {: term:label done:label end:label :}
   LGENIOOUT LABEL@ {: start:label :}
   s" (GENIO-OUT)" start LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start LBL,
   SP SP $10 SUBI,  30 SP 0 STR,
   9 DATA GENIO-ABI:BUSY-CELL LDR,  9 term CBNZ,
   0 GENIO-ABI:DEVICES CMPI,  C-HI term BCOND,          \ index outside 1..DEVICES
   9 GENIO-ABI:WRITE-OFF LIT64,  9 DATA 9 ADD,
   10 0 0 ADDI,  10 10 1 SUBI,  10 10 3 LSLI,  9 9 10 ADD,
   9 9 0 LDR,  9 term CBZ,                              \ row never registered
   10 DATA GENIO-ABI:ACTIVE-CELL LDR,  10 SP 8 STR,     \ save the caller's active device
   0 DATA GENIO-ABI:ACTIVE-CELL STR,                    \ the write runs for this one
   10 1 MOVZ,  10 DATA GENIO-ABI:BUSY-CELL STR,
   1 G-PUSH  2 G-PUSH
   9 BLR,
   10 0 MOVZ,  10 DATA GENIO-ABI:BUSY-CELL STR,
   10 SP 8 LDR,  10 DATA GENIO-ABI:ACTIVE-CELL STR,
   done B,
   term LBL,
   0 1 MOVZ,  NR-WRITE SYS,
   done LBL,
   30 SP 0 LDR,  SP SP $10 ADDI,
   RET,
   end LBL, ;

;package

package ENGINE-EMIT
private
: RECWPUB$ ( -- ptr u8 n )      s" rec-wide-publish" ;
: RECMIQ$ ( -- ptr u8 n )       s" rec-min-in@" ;
;package

\ The baked `trust-raw` and `trust-decl` strings a missing checker word names.
package KWDATA

public

variable LKWTRUSTRAW
: TRUSTRAW$ ( -- ptr u8 n )   s" trust-raw" ;

variable LKWTRUSTDECL
: TRUSTDECL$ ( -- ptr u8 n )   s" trust-decl" ;

: EMIT ( -- )
   LKWTRUSTDECL LABEL@ LBL, TRUSTDECL$ BYTES,      LKWTRUSTRAW LABEL@ LBL, TRUSTRAW$ BYTES, ;

;package

package ENGINE-EMIT

private

: W-LDRX ( n n n -- n ) {: rt RN off :}                               \ ldr rt,[rn,#off]
   $F9400000  off 8 / 10 lshift or  RN 5 lshift or  rt or ;

;package

package ENGINE-EMIT

public

: C-EXIT-LABEL ( ptr n n -- ) {: msg:ptr len:n :}
   0 2 MOVZ,  1 msg LABEL@ ADR,  2 len MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   0 70 MOVZ,  NR-EXIT-GROUP SYS, ;

\ Push the pending definition's name, the body text's first token (DEF-TKA
\ is stale once a multi-line body refills the reader); x11 is left alone.
\ The token is PKG:tail, so the checker's row is ( pkg, tail ) PUBLIC.
: C-PUSH-DREC-NAME ( -- )
   LBL LBL {: scan:label done:label :}
   9 DATA BODY-TEXT:BASE-CELL LDR,      \ x9 = name start (body text base)
   10 0 MOVZ,                           \ x10 = name length
   12 DATA BODYLEN-CELL LDR,            \ x12 = body length bound (fail-closed)
   scan LBL,
      10 12 CMP,  C-GE done BCOND,
      13 9 10 ADD,  13 13 0 LDRB,       \ x13 = name[x10]
      13 $20 CMPI,  C-EQ done BCOND,    \ the seeded trailing space ends the name
      13 done CBZ,
      10 10 1 ADDI,  scan B,
   done LBL,
   9 G-PUSH
   10 G-PUSH ;

private
: C-PUSH-DATA-CELL ( n -- ) {: off :}
   9 DATA off LDR,  9 G-PUSH ;

public
: C-PUSH-TRUST-SIG ( n n -- ) {: aoff uoff :}
   aoff C-PUSH-DATA-CELL
   uoff C-PUSH-DATA-CELL ;

: C-CALL-X11-SAVED ( -- )
   SP SP 16 SUBI,  30 SP 0 STR,
   11 BLR,
   30 SP 0 LDR,  SP SP 16 ADDI, ;

;package

\ The active native compiler retains its checker even while a build replaces
\ the source dictionary. The record's fields are the actual checker operations;
\ a distinct target checker owns a second record during source replacement.
package DECL-OWNER
public

: TARGET ( n label -- ) {: off:n absent:label :}
   11 DATA NCOMP-DISPATCH:TARGET-DECL-CELL LDR,  11 absent CBZ,
   11 11 off LDR,  11 absent CBZ, ;

: SKIP-SAME ( n label -- ) {: off:n same:label :}
   LBL {: different:label :}
   12 DATA NCOMP-DISPATCH:DECL-CELL LDR,  12 different CBZ,
   12 12 off LDR,  11 12 CMP,  C-EQ same BCOND,
   different LBL, ;

;package

package DEF-TRUST

public

\ Resolve `trust-decl` into x11. A missing registrar names itself on fd 2 and
\ exits 70 rather than publishing a definition whose effect nothing recorded.
\ Public because BDRAINPRETRUST needs the XT for its own push sequence.
: FIND ( -- )
   LBL LBL {: absent:label ready:label :}
   NCOMP-DISPATCH:DECL-EFFECT-OFF absent DECL-OWNER:TARGET
   ready B,
   absent LBL,  KWDATA:LKWTRUSTDECL KWDATA:TRUSTDECL$ nip C-EXIT-LABEL
   ready LBL, ;

;package

package LASTC-TRUST

\ Resolve `trust-raw`. A missing registrar names itself on fd 2 and exits 70.
: FIND-RAW ( -- )
   LBL LBL {: absent:label ready:label :}
   NCOMP-DISPATCH:DECL-RAW-OFF absent DECL-OWNER:TARGET
   ready B,
   absent LBL,  KWDATA:LKWTRUSTRAW KWDATA:TRUSTRAW$ nip C-EXIT-LABEL
   ready LBL, ;

public

: FIND-ACTIVE ( label -- ) {: absent:label :}
   LBL {: ready:label :}
   NCOMP-DISPATCH:DECL-RAW-OFF DECL-OWNER:FIND  11 ready CBNZ,
   9 DATA HOOK-CELL LDR,  9 absent CBZ,
   FIND-RAW
   ready LBL, ;

: FIND-TARGET ( label -- ) {: absent:label :}
   11 DATA NCOMP-DISPATCH:DECL-CELL LDR,  11 absent CBZ,
   NCOMP-DISPATCH:DECL-RAW-OFF absent DECL-OWNER:TARGET
   NCOMP-DISPATCH:DECL-RAW-OFF absent DECL-OWNER:SKIP-SAME ;

: PUBLISH ( -- )
   LBL {: nohook:label :}
   nohook FIND-ACTIVE
   C-PUSH-DREC-NAME
   CRSIG-A-CELL CRSIG-U-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED
   nohook FIND-TARGET
   C-PUSH-DREC-NAME
   CRSIG-A-CELL CRSIG-U-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED
   nohook LBL, ;

;package

package ENGINE-EMIT

public
\ x11 = the operation at OFF in the source owner's record. An owner whose record
\ ends before OFF (its byte size at [record-8]) or holds 0 there writes the line
\ MA MU to fd 2 and exits 70.
: C-OWNER-FIELD ( n ptr u8 n -- ) {: off:n ma:ptr mu:n :}
   LBL LBL LBL {: absent:label msg:label ready:label :}
   11 DATA NCOMP-DISPATCH:DECL-CELL LDR,  11 absent CBZ,
   10 11 8 SUBI,  10 10 0 LDR,
   10 off CELL + CMPI,  C-LT absent BCOND,
   11 11 off LDR,  11 ready CBNZ,
   absent LBL,
      0 2 MOVZ,  1 msg ADR,  2 mu MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  ma mu BYTES,
   ready LBL, ;

\ After ndict++ the checker's rec-wide-publish consumes the RECW latch for this
\ definition's effect and, when wide, marks record ndict-1 DNAME-WIDE. Both
\ calls go through the source owner's record, so the tail runs once checker.f
\ has stored it (DECL-CELL), not on the checking switch: a linker-built hb has
\ no HOOK-CELL, and its does> runtime publishes here. The engine prefix
\ self-load before checker.f has no owner. The same tail drains the RECMI latch
\ through rec-min-in@ into DNAME-MIN-IN (flags bits 52-59) through the alias, so
\ interpret-find fails closed on a bare underdepth call; min-in 0 (unchecked)
\ pokes nothing.
: EM-REC-WIDE-PUBLISH ( -- )
   LBL LBL {: nohook:label nomark:label :}
   9 DATA NCOMP-DISPATCH:DECL-CELL LDR,  9 nohook CBZ,
   NCOMP-DISPATCH:DECL-REC-WIDE-PUBLISH-OFF RECWPUB$ C-OWNER-FIELD
   C-CALL-X11-SAVED
   NCOMP-DISPATCH:DECL-REC-MIN-IN-OFF RECMIQ$ C-OWNER-FIELD
   C-CALL-X11-SAVED
   10 G-POP                                            \ x10 = latched min-in cells (0 = none)
   10 nomark CBZ,
      9 NDICT 0 ADDI,  9 9 1 SUBI,  12 DREC MOVZ,  9 9 12 MUL,  9 DBASE 9 ADD,   \ x9 = &record[NDICT-1] (x10 latch survives via kernel x2-x15)
      10 10 $FF ANDI,  10 10 52 LSLI,
      11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,   \ x11 = the record through the alias
      12 11 16 LDR,  12 12 10 ORR,  12 11 16 STR,
   nomark LBL,
   nohook LBL, ;

public

: C-RUNTIME-CRSIG-CLEAR ( -- )
   9 0 MOVZ,
   9 DATA CRSIG-A-CELL STR,
   9 DATA CRSIG-U-CELL STR, ;

private
\ EMPTY-BODY, emits the link-register save and leaves the slot's address in
\ FRAME-CELL; the body calls nothing, so `;` rewrites the slot to a nop in
\ place, through the alias, and flushes it.
\ SLOT-NOP, rewrites the entry slot at x9 to a nop; clobbers x1, x11.
: SLOT-NOP, ( -- )
   1 DATA REGION-ALIAS:DELTA-CELL LDR,  1 9 1 ADD,
   11 W-NOP LIT64,  11 1 0 STRW, ;

public
: EM-COMPILE-RET ( -- )
   9 DATA FRAME-CELL LDR,  SLOT-NOP,
   9 W-RET LIT64,  LCEMIT LABEL@ BL,
   9 0 MOVZ,  9 DATA FRAME-CELL STR, ;

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
\ the checker's latches, so a wide clause effect marks the created record
\ (ndict-1, as `create` counted it) DNAME-WIDE and declared inputs poke
\ DNAME-MIN-IN.
package DOESPATCH

: GRID-BEFORE ( -- )
   LBL LBL {: absent:label done:label :}
   CHECKER-OWNER-ABI:GRID-BEFORE-OFF absent DECL-OWNER:TARGET
   10 SP 40 LDR,  10 G-PUSH
   C-CALL-X11-SAVED
   9 G-POP
   done B,
   absent LBL,  9 0 MOVZ,
   done LBL, ;

: GRID-AFTER ( -- )
   LBL {: done:label :}
   9 SP 32 LDR,  9 done CBZ,
   CHECKER-OWNER-ABI:GRID-AFTER-OFF done DECL-OWNER:TARGET
   10 SP 40 LDR,  10 G-PUSH
   9 SP 32 LDR,  9 G-PUSH
   C-CALL-X11-SAVED
   done LBL, ;

: NOTIFY-PUBLICATION ( -- )
   LBL {: done:label :}
   11 DATA NCOMP-DISPATCH:DECL-CELL LDR,  11 done CBZ,
   12 11 CHECKER-OWNER-ABI:HEADER-BYTES SUBI,
   13 12 0 LDR,  14 CHECKER-OWNER-ABI:MAGIC LIT64,
   13 14 CMP,  C-NE done BCOND,
   13 12 8 LDR,
   14 CHECKER-OWNER-EXT:PUBLICATION-USE-OFF CELL + LIT64,
   13 14 CMP,  C-LT done BCOND,
   11 11 CHECKER-OWNER-EXT:PUBLICATION-USE-OFF LDR,  11 done CBZ,
   9 SP 40 LDR,  9 9 DBASE SUB,
   12 DREC MOVZ,  9 9 12 UDIV,
   9 G-PUSH  C-CALL-X11-SAVED
   done LBL, ;

\ Clear the kind of the patched record x11 and of every record that shares its
\ body. The body is a clause now, not a push or cell definer, so no record whose
\ code pointer [0] is that body may keep a stamp: an `EXPORT` alias copies its
\ target's code pointer and DKIND:VAL, and a stale VAL folds the alias into the
\ value it pushed when it was folded. An alias is made of an existing word, so
\ every one is newer than its target, and the scan walks only the records above
\ x11, in every wordlist. It ends at the larger of NDICT and the dictionary
\ high-water, not at NDICT: an alias a lowered `ndict!` retired keeps its
\ record, and a later raise re-exposes it with whatever stamp it holds. A record
\ with no stamp has no alias with one, so it scans nothing. Each match is
\ rewritten through the alias. Clobbers x0, x5, x10 and x13-x15.
: CLEAR-KIND ( -- )
   LBL LBL LBL {: scan:label next:label done:label :}
   5 DKIND:MASK DNAME-CELL or -1 xor LIT64,
   13 11 16 LDR,  14 DKIND:MASK LIT64,  14 13 14 AND,
   13 13 5 AND,
   0 DATA REGION-ALIAS:DELTA-CELL LDR,  0 11 0 ADD,  13 0 16 STR,
   14 done CBZ,
   13 11 0 LDR,                                          \ x13 = the patched body
   14 11 DREC ADDI,                                      \ x14 = the record above it
   15 DATA NDICT-HW-CELL LDR,  15 NDICT CMP,  15 15 NDICT C-HI CSEL,
   10 DREC MOVZ,  15 15 10 MUL,  15 DBASE 15 ADD,       \ x15 = the end of every record ever published
   scan LBL,
   14 15 CMP,  C-CS done BCOND,
   10 14 0 LDR,  10 13 CMP,  C-NE next BCOND,
      0 DATA REGION-ALIAS:DELTA-CELL LDR,  0 14 0 ADD,                \ x0 = the record through the alias
      10 14 16 LDR,  10 10 5 AND,  10 0 16 STR,
   next LBL,
   14 14 DREC ADDI,  scan B,
   done LBL, ;

public

\ Native defining words pass the measured clause entry and the already
\ parsed created-word signature.
: PRIM ( -- )
   C G-POP  B G-POP  A G-POP
   B DATA CRSIG-A-CELL STR,  C DATA CRSIG-U-CELL STR,
   B A 0 ADDI,  LDOESPATCH LABEL@ BL, ;

private

: NONE-CLASS$ ( -- ptr u8 n )
   s" E-DOES-UNCREATED habu: " ;

: NONE$ ( -- ptr u8 n )
   s" does> has no created word" ;

\ A LASTC of zero names no record: none was created since boot, or the one it
\ named was retired by a motion that lowered NDICT. A LASTC whose wordlist cell
\ is WL:RETIRED names a record `undefine` retired in place, which leaves NDICT
\ alone. Either is refused before anything is patched, recoverable inside
\ evaluate like the other definition errors.
: EMIT-NONE ( label -- ) {: have:label :}
   LBL LBL LBL {: none:label class:label msg:label :}
   11 DATA LASTC-CELL LDR,  11 none CBZ,
   12 11 40 LDR,  13 WL:RETIRED invert MOVN,  12 13 CMP,  C-NE have BCOND,
   none LBL,
   0 2 MOVZ,  1 class ADR,  2 NONE-CLASS$ nip MOVZ,  NR-WRITE SYS,
   LPLACE LABEL@ BL,
   0 2 MOVZ,  1 msg ADR,  2 NONE$ nip MOVZ,  NR-WRITE SYS,
   0 70 MOVZ,  LCOMPILEDIE LABEL@ B,
   class LBL,  NONE-CLASS$ BYTES,
   msg LBL,  NONE$ BYTES, ;

public

: EMIT ( -- )
   LBL LBL LBL LBL LBL LBL LBL
   {: nocr:label slot:label declared:label write:label flush:label
      have:label before:label :}
   LDOESPATCH LABEL@ LBL,
   have EMIT-NONE
   have LBL,
   SP SP 48 SUBI,  30 SP 0 STR,  10 SP 8 STR,
   11 DATA LASTC-CELL LDR,                               \ created slot
   11 SP 40 STR,
   12 11 0 LDR,  13 11 8 LDR,
   14 13 CODE-SPAN:FULL ANDI,  13 13 CODE-SPAN:MASK ANDI,
   14 slot CBZ,  13 13 4 SUBI,
   slot LBL,  12 12 13 ADD,                              \ final RET/B slot from either representation
   12 SP 16 STR,
   9 12 0 ADDI,
   10 before CBNZ,
   14 12 0 LDRW,  5 W-RET LIT64,  14 5 CMP,  C-NE before BCOND,
   9 0 MOVZ,
   before LBL,
   9 SP 24 STR,
   GRID-BEFORE  9 SP 32 STR,
   9 SP 24 LDR,  11 SP 40 LDR,
   10 11 0 ADDI,  EM-DEF-OCC:LBEFORE LABEL@ BL,
   10 SP 8 LDR,  9 SP 24 LDR,
   9 declared CBZ,
   10 SP 8 LDR,  12 SP 16 LDR,  11 DATA LASTC-CELL LDR,
   14 W-RET LIT64,  10 write CBZ,                       \ an empty replacement cancels the previous clause
   14 10 12 SUB,  14 14 2 ASRI,                          \ delta words (negative)
   5 $3FFFFFF LIT64,  14 14 5 AND,
   5 $14000000 LIT64,  14 14 5 ORR,                      \ b D
   write LBL,
   13 DATA REGION-ALIAS:DELTA-CELL LDR,  13 12 13 ADD,  14 13 0 STRW,
   10 flush CBZ,
   CLEAR-KIND
   flush LBL,
   12 SP 16 LDR,
   12 DCCVAU,  DSB-ISH,  12 ICIVAU,  DSB-ISH,  ISB,
   declared LBL,
   \ The patched body is now executable. Reissue its occurrence before the
   \ checker publishes the created word's contract through trust-raw.
   9 SP 24 LDR,  10 DATA LASTC-CELL LDR,  EM-DEF-OCC:LAFTER LABEL@ BL,
   LBL {: noshadow:label :}
   11 DATA NCOMP-DISPATCH:DOES-SHADOW-CELL LDR,  11 noshadow CBZ,
   9 DATA LASTC-CELL LDR,  9 9 DBASE SUB,
   10 DREC MOVZ,  9 9 10 UDIV,  9 G-PUSH
   9 SP 8 LDR,  9 G-PUSH
   11 DATA NCOMP-DISPATCH:DOES-SHADOW-CELL LDR,  C-CALL-X11-SAVED
   noshadow LBL,
   9 DATA CRSIG-U-CELL LDR,  9 nocr CBZ,
      LASTC-TRUST:PUBLISH
      \ A second does> may replace an existing clause. Its new effect replaces
      \ the old guard facts, including a zero min-in or a scalar result.
      9 DATA LASTC-CELL LDR,
      12 9 16 LDR,  10 DNAME-WIDE DNAME-MIN-IN-MASK or -1 xor LIT64,
      11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,              \ x11 = the record through the alias
      12 12 10 AND,  12 11 16 STR,
      EM-REC-WIDE-PUBLISH
      C-RUNTIME-CRSIG-CLEAR
   nocr LBL,
   GRID-AFTER
   \ The source observer sees the final effect and retained storage grid.
   NOTIFY-PUBLICATION
   30 SP 0 LDR,  SP SP 48 ADDI,  RET, ;

;package

\ A record's name is stored once for every definer.
package DEFWRITE

public

\ The comparisons are unsigned and never wrap: a CP already at the ceiling, and
\ a length no region holds (a negative one among them), are refused rather than
\ added. x12 = the length. Clobbers x11, x15 and the flags.
: NAME-ROOM ( label -- ) {: full:label :}
   LBL {: fits:label :}
   12 DNAME-INL CMPI,  C-LS fits BCOND,
   11 REGION $4000 - LIT64,  11 DBASE 11 ADD,
   CP 11 CMP,  C-CS full BCOND,                       \ CP is already at the ceiling
   11 11 CP SUB,                                      \ x11 = the room below it
   12 11 CMP,  C-CS full BCOND,                       \ the bytes alone reach it
   15 12 3 ADDI,  15 15 2 LSRI,  15 15 2 LSLI,
   15 11 CMP,  C-CS full BCOND,                       \ so do the padded bytes
   fits LBL, ;

\ Store record x9's name, x12 >= 1 bytes at x10, in room NAME-ROOM admitted.
\ Clobbers x10 x11 x13-x16.
: NAME-COPY ( -- )
   LBL LBL LBL LBL LBL LBL {: short lcopy lcd scopy scd done :}
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,
   14 0 MOVZ,  14 11 24 STR,  14 11 32 STR,           \ canonical name/padding before either form
   13 12 0 ADDI,
   12 DNAME-INL CMPI,  C-LS short BCOND,
      14 DNAME-EXT LIT64,  13 13 14 ORR,  13 11 16 STR,
      15 12 3 ADDI,  15 15 2 LSRI,  15 15 2 LSLI,
      16 CP 15 ADD,
      CP 11 24 STR,
      11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 CP 11 ADD,
      14 12 0 ADDI,
      lcopy LBL,  14 lcd CBZ,
         15 10 0 LDRB,  15 11 0 STRB,
         10 10 1 ADDI,  11 11 1 ADDI,  14 14 1 SUBI,  lcopy B,
      lcd LBL,
      CP 16 0 ADDI,
      done B,
   short LBL,
      13 11 16 STR,
      11 11 24 ADDI,  14 12 0 ADDI,
      scopy LBL,  14 scd CBZ,
         15 10 0 LDRB,  15 11 0 STRB,
         10 10 1 ADDI,  11 11 1 ADDI,  14 14 1 SUBI,  scopy B,
      scd LBL,
   done LBL, ;

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
   LBL {: inl:label :}
   11 DATA PEND-CELL LDR,
   12 11 16 LDR,
   13 12 0 ADDI,  13 13 DNAME-FLAG-BITS LSLI,  13 13 DNAME-FLAG-BITS LSRI,       \ the parent's own name length
   14 11 24 ADDI,
   12 12 DNAME-EXT ANDI,  12 inl CBZ,
      14 11 24 LDR,                                    \ ... which is a pointer when it is long
   inl LBL,
   13 13 SUF-LEN ADDI,
   15 13 3 ADDI,  15 15 2 LSRI,  15 15 2 LSLI, ;

\ Copy the derived name to x10, padded to x15 bytes. NAME$ has already supplied
\ x11..x15 and the caller owns the padded span.
: COPY-NAME ( -- )
   LBL LBL LBL LBL {: cpy:label cpd:label pad:label pend:label :}
   12 13 SUF-LEN SUBI,
   cpy LBL,  12 cpd CBZ,
      9 14 0 LDRB,  9 10 0 STRB,
      14 14 1 ADDI,  10 10 1 ADDI,  12 12 1 SUBI,  cpy B,
   cpd LBL,
   9 $3B MOVZ,  9 10 0 STRB,  10 10 1 ADDI,            \ ';'
   9 $64 MOVZ,  9 10 0 STRB,  10 10 1 ADDI,            \ 'd'
   9 $6F MOVZ,  9 10 0 STRB,  10 10 1 ADDI,            \ 'o'
   9 $65 MOVZ,  9 10 0 STRB,  10 10 1 ADDI,            \ 'e'
   9 $73 MOVZ,  9 10 0 STRB,  10 10 1 ADDI,            \ 's'
   12 15 13 SUB,  9 0 MOVZ,
   pad LBL,  12 pend CBZ,
      9 10 0 STRB,  10 10 1 ADDI,  12 12 1 SUBI,  pad B,
   pend LBL, ;

\ Write the clause record in the existing pending+1 slot; entry and len name
\ the registers that hold the clause entry and its recorded body length.
: RECORD ( n n -- )
   {: entry:n len:n :}
   14 NDICT 1 ADDI,  9 DREC MOVZ,  14 14 9 MUL,  14 DBASE 14 ADD,
   9 DATA REGION-ALIAS:DELTA-CELL LDR,  14 14 9 ADD,
   9 0 MOVZ,                                           \ a slot an abandoned definition may have written
   9 14 0 STR,  9 14 8 STR,  9 14 16 STR,
   9 14 24 STR,  9 14 32 STR,  9 14 40 STR,
   entry 14 0 STR,                                     \ [0] = the clause entry
   len 14 8 STR,                                       \ [8] = its recorded body length
   9 DNAME-EXT LIT64,  9 13 9 ORR,  9 14 16 STR,       \ [16] = its name length, out of line
   9 CP 0 ADDI,  9 14 24 STR,                          \ [24] = the name bytes
   11 DATA PEND-CELL LDR,  9 11 40 LDR,  9 14 40 STR, ; \ [40] = the parent's wordlist

\ Native publication has already emitted the clause. Append only the permanent
\ derived name and record, using the measured entry and length it supplies.
: NATIVE-PRIM ( -- )
   B G-POP  A G-POP
   5 A 0 ADDI,  6 B 0 ADDI,
   NAME$
   10 DATA REGION-ALIAS:DELTA-CELL LDR,  10 CP 10 ADD,  COPY-NAME
   5 6 RECORD
   9 1 MOVZ,  9 DATA CLAUSE-CELL STR,                  \ an imported definer had no `does>`
   CP CP 15 ADD, ;

public

\ The clause record's own length, by the parent's rule: both bodies end at the
\ shared epilogue. It runs BEFORE the
\ parent's write, which leaves x9 live for the cache flush.
: FLUSH ( -- )
   LBL {: none:label :}
   9 DATA DOESB-CELL LDR,  9 none CBZ,
   11 NDICT 1 ADDI,  12 DREC MOVZ,  11 11 12 MUL,  11 DBASE 11 ADD,
   9 11 0 LDR,  10 CP 9 SUB,  10 10 4 SUBI,
   12 DATA REGION-ALIAS:DELTA-CELL LDR,  12 11 12 ADD,  10 12 8 STR,
   none LBL, ;

;package

package ENGINE-EMIT
public
: C-CLEAR-TRUSTED-STATE ( -- )
   9 0 MOVZ,
   9 DATA NCOMP-DISPATCH:COMP-LATCH-CELL STR,
   9 DATA NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:PENDING-TAKE cells + STR,
   9 DATA TSIG-A-CELL STR,   9 DATA TSIG-U-CELL STR,
   9 DATA TCSIG-A-CELL STR,  9 DATA TCSIG-U-CELL STR,
   9 DATA DOESB-CELL STR,  9 DATA CLAUSE-CELL STR,
   9 DATA TRUSTED-CELL STR, ;

\ No local outlives its definition. Wherever one ends, published or abandoned,
\ its locals' count, frame and block depth clear with their token-start marks,
\ so the next definition starts with none and no image holds them.
: C-CLEAR-LOCALS ( -- )
   9 0 MOVZ,
   9 DATA LOCN-CELL STR,  9 DATA CAPDEPTH-CELL STR,
   9 DATA TOKLOCN-CELL STR,  9 DATA TOKDEPTH-CELL STR, ;

;package

\ The bodies of the fourteen rows src/habu/prims.f specifies under "the
\ definition writers" and of the six replay writers after them. Each checks
\ everything before it writes, and a refusal exits the process, so no reader
\ ever sees a half-written row. FPRIM-WID frames x30 around each body.
package DEFWRITE

\ The refusal at the label: the text and the primitive's lr, which BLs in the
\ body replace and the frame keeps.
: REFUSE-AT ( ptr u8 n label n -- ) {: a:ptr u:n at:label rc:n :}
   at LBL,  0 CALLER-LR,  a u rc DIE-LINE, ;

\ x<dst> = the address of record x<ix>; dst and ix differ.
: REC-AT, ( n n -- ) {: dst:n ix:n :}
   dst DREC MOVZ,  dst ix dst MUL,  dst DBASE dst ADD, ;

\ A pending definition's record is slot NDICT, the slot a record writer fills.
: NOT-PENDING, ( label -- ) {: bad:label :}
   9 DATA PEND-CELL LDR,  9 bad CBNZ, ;

\ Slot DICT-CAP is the control-flow band, not a record.
: DICT-ROOM, ( label -- ) {: bad:label :}
   9 DICT-CAP LIT64,  NDICT 9 CMP,  C-CS bad BCOND, ;

\ An empty name, and one whose copy would reach the code ceiling. x1 = the
\ length; x12 takes a copy for NAME-ROOM.
: NAME-SIZE, ( label -- ) {: bad:label :}
   1 bad CBZ,
   12 1 0 ADDI,  bad NAME-ROOM ;

\ A live record for the folded name x0/x1 in wordlist x2. WLFIND keeps x0, x1,
\ x2 and x13, and clobbers x3-x12 and x14-x17.
: FRESH, ( label -- ) {: bad:label :}
   WLFIND:LENTRY LABEL@ BL,  12 bad CBNZ, ;

\ Wid x2 is one of wl.f's WL markers, not a wordlist: every wordlist id
\ is at least 0.
: REAL-WID, ( label -- ) {: bad:label :}
   2 0 CMPI,  C-LT bad BCOND, ;

\ After the seal, wid x2 is protected. LPROTWIDQ keeps x0-x4 and answers in x13.
: OPEN-WID, ( label -- ) {: prot:label :}
   LBL {: open:label :}
   9 DATA SEAL-NDICT-CELL LDR,  9 open CBZ,
   9 2 0 ADDI,  LPROTWIDQ LABEL@ BL,  13 prot CBNZ,
   open LBL, ;

\ Count record NDICT and index it.
: PUBLISH, ( -- )
   EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL, ;

\ x<r> = the replay scope band; its cells are reached as offsets from that base.
: BAND, ( n -- ) {: r:n :}
   r REPLAY-SCOPE:LATCH LIT64,  r DATA r ADD, ;

: BAND-OFF ( n -- n ) REPLAY-SCOPE:LATCH - ;

\ While an overlay is open, raise its high-water mark to NDICT: the records the
\ overlay published are [the saved NDICT, HW). Clobbers x8 and x14.
: HW-RAISE, ( -- )
   LBL {: keep:label :}
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF LDR,  14 keep CBZ,
   14 8 REPLAY-SCOPE:HW BAND-OFF LDR,  NDICT 14 CMP,  C-LS keep BCOND,
   NDICT 8 REPLAY-SCOPE:HW BAND-OFF STR,
   keep LBL, ;

\ One scope cell, at off from x<base>, into band cell `cell` at x8 when save is
\ true and back out of it when it is false. Clobbers x14.
: SAVED, ( n n n bool -- ) {: base:n off:n cell:n save:bool :}
   save if
      14 base off LDR,  14 8 cell BAND-OFF STR,
   else
      14 8 cell BAND-OFF LDR,  14 base off STR,
   then ;

\ The scope replay-open saves and replay-close restores, beside NDICT and CP.
\ x8 = the band. Clobbers x10 and x14.
: SCOPE, ( bool -- ) {: save:bool :}
   DATA WIDN-CELL REPLAY-SCOPE:WIDN save SAVED,
   DATA CUR-CELL REPLAY-SCOPE:CUR save SAVED,
   DATA PKG-PUB-CELL REPLAY-SCOPE:PKG-PUB save SAVED,
   DATA PKG-PRI-CELL REPLAY-SCOPE:PKG-PRI save SAVED,
   DATA PKG-PARENT-CELL REPLAY-SCOPE:PKG-PARENT save SAVED,
   DATA PKG-REC-CELL REPLAY-SCOPE:PKG-REC save SAVED,
   ENCLOSING:END ENCLOSING:PUB - 0 do
      DATA ENCLOSING:PUB i +  REPLAY-SCOPE:ENCLOSING i +  save SAVED,
   CELL +loop
   10 USE-BAND-OFF LIT64,  10 DATA 10 ADD,
   10 USE-DEPTH-CELL USE-BAND-OFF - REPLAY-SCOPE:USE-DEPTH save SAVED,
   10 USE-PKG-SAVE-CELL USE-BAND-OFF - REPLAY-SCOPE:USE-PKG-SAVE save SAVED,
   USE-MAX 0 ?do
      10 USE-WIDS-OFF USE-BAND-OFF - i cells +  REPLAY-SCOPE:USE-WIDS i cells +  save SAVED,
   loop ;

\ Zero the cells [x10, x11). Clobbers x10 and x15.
: ZERO-SPAN, ( -- )
   LBL LBL {: next:label done:label :}
   15 0 MOVZ,
   next LBL,  10 11 CMP,  C-CS done BCOND,
      15 10 0 STR,  10 10 8 ADDI,  next B,
   done LBL, ;

public

\ The entry of every replay record: executing one is an error the record has no
\ code to report.
variable LREPLAYTRAP

: EMIT-REPLAY-TRAP ( -- )
   LREPLAYTRAP LABEL@ LBL,
   s" E-REPLAY-RECORD habu: replay record executed" 76 DIE-LINE, ;

\ namespace-record ( ptr u8 n bool -- n )
\ A name holds at most one colon, never at an edge: a part's path `A:B`.
: NAMESPACE-RECORD ( -- )
   LBL LBL LBL LBL LBL LBL {: bad scan next clean nopri done :}
   s" E-TASK-COMPILE habu: namespace-record from a task" B-TASK-GUARD
   13 G-POP  1 G-POP  0 G-POP                         \ x13 = the flag, x1 = the length, x0 = the name
   bad NOT-PENDING,
   bad DICT-ROOM,
   bad NAME-SIZE,
   14 0 MOVZ,  16 0 MOVZ,                             \ x16 = a colon seen
   scan LBL,  14 1 CMP,  C-CS clean BCOND,
      15 0 14 ADD,  15 15 0 LDRB,  15 $3A CMPI,  C-NE next BCOND,
         16 bad CBNZ,  14 bad CBZ,
         15 14 1 ADDI,  15 1 CMP,  C-CS bad BCOND,
         16 1 MOVZ,
   next LBL,  14 14 1 ADDI,  scan B,
   clean LBL,
   2 DATA BOOT:NAMESPACE-WL-CELL LDR,
   bad FRESH,
   10 0 0 ADDI,  12 1 0 ADDI,  3 13 0 ADDI,           \ the name, the flag
   9 NDICT REC-AT,
   NAME-COPY
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,
   14 DATA WIDN-CELL LDR,  14 11 0 STR,                \ [0] = a fresh wid
   14 14 1 ADDI,  15 0 MOVZ,
   3 nopri CBZ,
      15 14 0 ADDI,  14 14 1 ADDI,                    \ [8] = a second one
   nopri LBL,
   15 11 8 STR,  14 DATA WIDN-CELL STR,
   15 DATA BOOT:NAMESPACE-WL-CELL LDR,  15 11 40 STR,
   PUBLISH,
   HW-RAISE,                                          \ an overlay's row dies at replay-close
   9 NDICT 1 SUBI,  9 G-PUSH
   done B,
   s" E-SEAL-VIOLATION habu: namespace-record with a definition pending, no record room, an empty name or one reaching the code ceiling, a colon at an edge or twice, or a name already recorded"
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ namespace-private ( n -- )
: NAMESPACE-PRIVATE ( -- )
   LBL LBL {: bad done :}
   s" E-TASK-COMPILE habu: namespace-private from a task" B-TASK-GUARD
   OVERLAY-EMIT:GUARD,
   0 G-POP
   0 NDICT CMP,  C-CS bad BCOND,
   9 0 REC-AT,
   14 9 40 LDR,  14 0 CMPI,  C-GE bad BCOND,             \ a live package row
   15 WL:RETIRED invert MOVN,  14 15 CMP,  C-EQ bad BCOND,
   15 WL:RETIRED-NAMESPACE invert MOVN,  14 15 CMP,  C-EQ bad BCOND,
   14 9 8 LDR,  14 bad CBNZ,
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,
   14 DATA WIDN-CELL LDR,  14 11 8 STR,
   14 14 1 ADDI,  14 DATA WIDN-CELL STR,
   done B,
   s" E-SEAL-VIOLATION habu: namespace-private index not a live package row below NDICT, or a row that has a private wordlist"
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ alias-record ( ptr u8 n n n -- ) name, source index, wid
: ALIAS-RECORD ( -- )
   LBL LBL LBL LBL {: bad prot notval done :}
   s" E-TASK-COMPILE habu: alias-record from a task" B-TASK-GUARD
   OVERLAY-EMIT:GUARD,
   2 G-POP  4 G-POP  1 G-POP  0 G-POP                 \ x2 = the wid, x4 = the source, x1/x0 = the name
   bad REAL-WID,
   prot OPEN-WID,
   bad NOT-PENDING,
   4 NDICT CMP,  C-CS bad BCOND,
   5 4 REC-AT,
   14 5 40 LDR,  14 0 CMPI,  C-LT bad BCOND,           \ a package row or a retired record has no live body
   14 5 16 LDR,  14 14 DNAME-INT ANDI,  14 bad CBNZ,  \ an internal body: the alias would drop its gate
   bad DICT-ROOM,
   bad NAME-SIZE,
   13 4 0 ADDI,
   bad FRESH,
   3 2 0 ADDI,  4 13 0 ADDI,  10 0 0 ADDI,  12 1 0 ADDI,
   9 NDICT REC-AT,
   NAME-COPY
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,
   5 4 REC-AT,
   14 5 0 LDR,  14 11 0 STR,                           \ [0] = the source's entry
   14 5 8 LDR,  14 11 8 STR,                           \ [8] = its recorded length
   16 5 16 LDR,
   15 DNAME-IMM DNAME-WIDE or DNAME-MIN-IN-MASK or DNAME-TARGET or LIT64,  14 16 15 AND,
   15 DKIND:MASK LIT64,  16 16 15 AND,
   15 DKIND:VAL LIT64,  16 15 CMP,  C-NE notval BCOND,
      14 14 15 ORR,                                    \ only an immutable value keeps its fixed kind
   notval LBL,
   15 9 16 LDR,  15 15 14 ORR,  15 11 16 STR,
   3 11 40 STR,
   PUBLISH,
   done B,
   s" E-SEAL-VIOLATION habu: alias-record with a marker wid, a definition pending, a source that is no live definition or is internal, no record room, an empty name or one reaching the code ceiling, or a name taken"
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   s" E-SEAL-PACKAGE habu: alias-record into a wordlist protected by the seal"
   prot ENGINE-ERROR:SEAL-PACKAGE REFUSE-AT
   done LBL, ;

\ package-scope! ( n n -- )
: PACKAGE-SCOPE ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL {: bad set inline scan part top store done :}
   1 G-POP  0 G-POP                                   \ x1 = the parent wid, x0 = the row
   15 0 MOVN,  0 15 CMP,  C-NE set BCOND,
      1 bad CBNZ,                                     \ only `-1 0` clears
      9 0 MOVZ,
      9 DATA PKG-PUB-CELL STR,  9 DATA PKG-PRI-CELL STR,
      9 DATA PKG-PARENT-CELL STR,  9 DATA PKG-REC-CELL STR,
      9 DATA ENCLOSING:PUB STR,  9 DATA ENCLOSING:PRI STR,
      9 DATA ENCLOSING:PARENT STR,  9 DATA ENCLOSING:REC STR,
      done B,
   set LBL,
   0 NDICT CMP,  C-CS bad BCOND,
   9 0 REC-AT,
   14 9 40 LDR,  14 0 CMPI,  C-GE bad BCOND,             \ a live package row
   15 WL:RETIRED invert MOVN,  14 15 CMP,  C-EQ bad BCOND,
   15 WL:RETIRED-NAMESPACE invert MOVN,  14 15 CMP,  C-EQ bad BCOND,
   12 9 8 LDR,  12 bad CBZ,
   2 9 16 LDR,  2 2 DNAME-FLAG-BITS LSLI,  2 2 DNAME-FLAG-BITS LSRI,   \ x2 = the name's length
   4 9 24 ADDI,
   14 9 16 LDR,  14 14 DNAME-EXT ANDI,  14 inline CBZ,
      4 9 24 LDR,                                     \ x4 = the name's bytes
   inline LBL,
   3 0 MOVZ,
   scan LBL,  3 2 CMP,  C-CS top BCOND,
      15 4 3 ADD,  15 15 0 LDRB,  15 $3A CMPI,  C-EQ part BCOND,
      3 3 1 ADDI,  scan B,
   part LBL,
      14 DATA PKG-PUB-CELL LDR,  14 bad CBZ,          \ no package open
      14 DATA ENCLOSING:PUB LDR,  14 bad CBNZ,        \ a part is open
      14 DATA PKG-PUB-CELL LDR,  14 DATA ENCLOSING:PUB STR,
      14 DATA PKG-PRI-CELL LDR,  14 DATA ENCLOSING:PRI STR,
      14 DATA PKG-PARENT-CELL LDR,  14 DATA ENCLOSING:PARENT STR,
      14 DATA PKG-REC-CELL LDR,  14 DATA ENCLOSING:REC STR,
      store B,
   top LBL,
      14 0 MOVZ,
      14 DATA ENCLOSING:PUB STR,  14 DATA ENCLOSING:PRI STR,
      14 DATA ENCLOSING:PARENT STR,  14 DATA ENCLOSING:REC STR,
   store LBL,
   11 9 0 LDR,
   11 DATA PKG-PUB-CELL STR,  12 DATA PKG-PRI-CELL STR,
   1 DATA PKG-PARENT-CELL STR,  9 DATA PKG-REC-CELL STR,
   done B,
   s" E-SEAL-VIOLATION habu: package-scope! row not a live package row below NDICT with a private wordlist, a part outside a package or inside a part, or -1 with a parent"
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ replay-open ( -- )
: REPLAY-OPEN ( -- )
   LBL LBL {: bad done :}
   s" E-TASK-COMPILE habu: replay-open from a task" B-TASK-GUARD
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF LDR,  14 bad CBNZ,   \ an overlay is open
   bad NOT-PENDING,
   NDICT 8 REPLAY-SCOPE:NDICT BAND-OFF STR,
   CP 8 REPLAY-SCOPE:CP BAND-OFF STR,
   0 0= SCOPE,
   NDICT 8 REPLAY-SCOPE:HW BAND-OFF STR,
   14 1 MOVZ,  14 8 REPLAY-SCOPE:LATCH BAND-OFF STR,
   done B,
   s" E-SEAL-VIOLATION habu: replay-open with an overlay open or a definition pending"
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ replay-record ( ptr u8 n n -- ) name, wid: a codeless record, whose entry
\ is the trap. It needs no OPEN-WID: it can only trap, and replay-close
\ removes it.
: REPLAY-RECORD ( -- )
   LBL LBL {: bad done :}
   s" E-TASK-COMPILE habu: replay-record from a task" B-TASK-GUARD
   2 G-POP  1 G-POP  0 G-POP                          \ x2 = the wid, x1/x0 = the name
   bad REAL-WID,
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF LDR,  14 bad CBZ,   \ no overlay
   bad NOT-PENDING,
   bad DICT-ROOM,
   bad NAME-SIZE,
   bad FRESH,
   3 2 0 ADDI,  10 0 0 ADDI,  12 1 0 ADDI,
   9 NDICT REC-AT,
   NAME-COPY
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,
   14 LREPLAYTRAP LABEL@ ADR,  14 11 0 STR,            \ [0] = the trap
   14 0 MOVZ,  14 11 8 STR,
   3 11 40 STR,
   PUBLISH,
   HW-RAISE,
   done B,
   s" E-SEAL-VIOLATION habu: replay-record with a marker wid, no overlay, a definition pending, no record room, an empty name or one reaching the code ceiling, or a name taken"
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

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
   LBL LBL LBL LBL {: bad wid store done :}
   s" E-TASK-COMPILE habu: record-wid! from a task" B-TASK-GUARD
   3 G-POP  13 G-POP                                  \ x3 = the index, x13 = the wid
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF LDR,  14 bad CBZ,   \ no overlay
   3 NDICT CMP,  C-CS bad BCOND,
   9 3 REC-AT,
   14 9 40 LDR,  14 0 CMPI,  C-GE wid BCOND,             \ a live package row
   15 WL:RETIRED invert MOVN,  14 15 CMP,  C-EQ wid BCOND,
   15 WL:RETIRED-NAMESPACE invert MOVN,  14 15 CMP,  C-NE bad BCOND,
   wid LBL,
   13 0 CMPI,  C-GE store BCOND,                         \ a live package row's marker
   15 WL:RETIRED invert MOVN,  13 15 CMP,  C-EQ store BCOND,
   15 WL:RETIRED-NAMESPACE invert MOVN,  13 15 CMP,  C-NE bad BCOND,
   store LBL,
   14 DATA REGION-ALIAS:DELTA-CELL LDR,  14 9 14 ADD,  13 14 40 STR,
   HIDX-EMIT:LREKEY LABEL@ BL,
   done B,
   s" E-SEAL-VIOLATION habu: record-wid! with no overlay, an index at or past NDICT, or an old or new wid neither a wordlist nor a retired marker"
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ replay-private ( n bool -- ) namespace index, flag: true gives the row a
\ fresh private wid, as `package` gives one to a row a qualified definition
\ made; false takes back the one it gave. replay-close puts WIDN back but
\ nothing of a row below the saved NDICT, so the owner logs each grant and
\ takes it back before the close.
: REPLAY-PRIVATE ( -- )
   LBL LBL LBL {: bad give done :}
   s" E-TASK-COMPILE habu: replay-private from a task" B-TASK-GUARD
   13 G-POP  3 G-POP                                  \ x13 = the flag, x3 = the index
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF LDR,  14 bad CBZ,   \ no overlay
   3 NDICT CMP,  C-CS bad BCOND,
   9 3 REC-AT,
   14 9 40 LDR,  14 0 CMPI,  C-GE bad BCOND,             \ a live package row
   15 WL:RETIRED invert MOVN,  14 15 CMP,  C-EQ bad BCOND,
   15 WL:RETIRED-NAMESPACE invert MOVN,  14 15 CMP,  C-EQ bad BCOND,
   14 9 8 LDR,
   13 give CBNZ,
      15 8 REPLAY-SCOPE:WIDN BAND-OFF LDR,  14 15 CMP,  C-CC bad BCOND,
      15 DATA REGION-ALIAS:DELTA-CELL LDR,  15 9 15 ADD,
      14 0 MOVZ,  14 15 8 STR,
      done B,
   give LBL,
   14 bad CBNZ,
   15 DATA REGION-ALIAS:DELTA-CELL LDR,  15 9 15 ADD,
   14 DATA WIDN-CELL LDR,  14 15 8 STR,
   14 14 1 ADDI,  14 DATA WIDN-CELL STR,
   done B,
   s" E-SEAL-VIOLATION habu: replay-private with no overlay, an index not a live package row below NDICT, a grant to a row that has one, or a take-back below the saved WIDN"
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ replay-close ( -- ): every check comes before the first store. It refuses
\ NDICT below the saved count or above HW, CP below the saved CP and WIDN below
\ the saved WIDN; then it zeroes the overlay's records, puts NDICT, CP and the
\ scope back and zeroes the band, the latch with it, and its enclosing copy.
\ While a task is live it retires the records instead and keeps NDICT and CP.
\ Engine writers only raise WIDN past the saved count, so the WIDN refusal meets
\ a raw store to WIDN-CELL, which nothing else refuses.
: REPLAY-CLOSE ( -- )
   LBL LBL LBL {: bad kept done :}
   s" E-TASK-COMPILE habu: replay-close from a task" B-TASK-GUARD
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF LDR,  14 bad CBZ,   \ no overlay
   bad NOT-PENDING,
   4 8 REPLAY-SCOPE:NDICT BAND-OFF LDR,  NDICT 4 CMP,  C-CC bad BCOND,
   5 8 REPLAY-SCOPE:HW BAND-OFF LDR,  NDICT 5 CMP,  C-HI bad BCOND,
   6 8 REPLAY-SCOPE:CP BAND-OFF LDR,  CP 6 CMP,  C-CC bad BCOND,
   14 8 REPLAY-SCOPE:WIDN BAND-OFF LDR,  15 DATA WIDN-CELL LDR,
   15 14 CMP,  C-CC bad BCOND,
   9 4 0 ADDI,  EM-DEF-OCC:LRETIRE LABEL@ BL,  14 kept CBNZ,
   EM-DEF-OCC:LCOUNT LABEL@ BL,  10 11 LASTC-TRIM,
   CP 6 0 ADDI,
   10 DREC MOVZ,  11 5 10 MUL,  10 4 10 MUL,
   10 DBASE 10 ADD,  11 DBASE 11 ADD,                 \ records [x10, x11)
   12 DATA REGION-ALIAS:DELTA-CELL LDR,  10 10 12 ADD,  11 11 12 ADD,
   ZERO-SPAN,
   kept LBL,
   8 BAND,
   0 0<> SCOPE,
   10 8 0 ADDI,  11 8 REPLAY-SCOPE:END BAND-OFF ADDI,
   ZERO-SPAN,
   10 8 REPLAY-SCOPE:ENCLOSING BAND-OFF ADDI,
   11 10 ENCLOSING:END ENCLOSING:PUB - ADDI,
   ZERO-SPAN,
   done B,
   s" E-SEAL-VIOLATION habu: replay-close with no overlay, a definition pending, NDICT below the saved count or above HW, CP below the saved CP, or WIDN below the saved WIDN"
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ replay-widn! ( n -- ): put WIDN back to N, a mark the owner read while the
\ overlay was open, as a rollback inside the overlay drops what it made since
\ the mark. The records ndict! drops and the private wids replay-private takes
\ back held every wid at or above N, so none is handed out twice. It refuses no
\ overlay, N below the saved WIDN, which would hand out again a wid from before
\ the overlay, and N above WIDN, both compared unsigned. A mark is never below
\ the saved WIDN, so replay-close's WIDN check keeps holding.
: REPLAY-WIDN ( -- )
   LBL LBL {: bad done :}
   s" E-TASK-COMPILE habu: replay-widn! from a task" B-TASK-GUARD
   13 G-POP                                           \ x13 = the mark
   8 BAND,
   14 8 REPLAY-SCOPE:LATCH BAND-OFF LDR,  14 bad CBZ,   \ no overlay
   14 8 REPLAY-SCOPE:WIDN BAND-OFF LDR,  13 14 CMP,  C-CC bad BCOND,
   15 DATA WIDN-CELL LDR,  13 15 CMP,  C-HI bad BCOND,
   13 DATA WIDN-CELL STR,
   done B,
   s" E-SEAL-VIOLATION habu: replay-widn! with no overlay, or a mark below the saved WIDN or above WIDN"
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ def-open ( ptr u8 n n n -- ) name, wid, kind
: DEF-OPEN ( -- )
   LBL LBL LBL {: bad prot done :}
   s" E-TASK-COMPILE habu: def-open from a task" B-TASK-GUARD
   OVERLAY-EMIT:GUARD,
   4 G-POP  2 G-POP  1 G-POP  0 G-POP                 \ x4 = the kind, x2 = the wid, x1/x0 = the name
   14 DKIND:MASK DNAME-CELL or DNAME-ALLOC or DNAME-TARGET or invert LIT64,  14 4 14 AND,  14 bad CBNZ,
   bad REAL-WID,
   prot OPEN-WID,
   bad NOT-PENDING,
   9 REGION $4000 - LIT64,  9 DBASE 9 ADD,  CP 9 CMP,  C-CS bad BCOND,
   bad DICT-ROOM,
   bad NAME-SIZE,
   13 4 0 ADDI,
   bad FRESH,
   3 2 0 ADDI,  4 13 0 ADDI,  10 0 0 ADDI,  12 1 0 ADDI,
   9 NDICT REC-AT,
   NAME-COPY
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,
   CP 11 0 STR,                                       \ [0] = the entry, past a long name
   14 0 MOVZ,  14 11 8 STR,
   14 9 16 LDR,  14 14 4 ORR,  14 11 16 STR,          \ the kind beside the length
   3 11 40 STR,
   9 DATA PEND-CELL STR,  14 DATA TKA-CELL LDR,  14 DATA PENDTKA-CELL STR,
   14 0 MOVZ,
   14 DATA NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:PENDING-TAKE cells + STR,
   14 DATA TSIG-A-CELL STR,   14 DATA TSIG-U-CELL STR,
   14 DATA TCSIG-A-CELL STR,  14 DATA TCSIG-U-CELL STR,
   14 DATA DOESB-CELL STR,    14 DATA TRUSTED-CELL STR,
   14 DATA CLAUSE-CELL STR,
   14 COMP-LATCH  14 DATA NCOMP-DISPATCH:COMP-LATCH-CELL STR,
   done B,
   s" E-SEAL-VIOLATION habu: def-open with a bad kind, a marker wid, a definition pending, CP at the code ceiling, no record room, an empty name or one reaching the code ceiling, or a name taken"
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   s" E-SEAL-PACKAGE habu: def-open into a wordlist protected by the seal"
   prot ENGINE-ERROR:SEAL-PACKAGE REFUSE-AT
   done LBL, ;

\ body-capture ( ptr u8 n -- ): LBCS is the capture `:` appends through; it grows
\ the body text block as every capture does.
: BODY-APPEND ( -- )
   LBL LBL {: bad done :}
   12 G-POP  11 G-POP                                 \ LBCS: x11 = the bytes, x12 = their count
   14 DATA BODYLEN-CELL LDR,  15 DATA BODY-TEXT:CAP-CELL LDR,
   14 15 CMP,  C-HI bad BCOND,                        \ BODYLEN is already past the text capacity
   15 0 MOVN,  15 15 14 SUB,                          \ x15 = 2^64 - 1 - BODYLEN
   12 15 CMP,  C-CS bad BCOND,                        \ BODYLEN + u + 1 wraps
   LBCS LABEL@ BL,
   done B,
   s" E-SEAL-VIOLATION habu: body-capture past the body text capacity or wrapping"
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ ( ptr u8 n -- ): the span into the cells aoff and uoff while a
\ definition is pending.
: PENDING-SPAN ( ptr u8 n n n -- ) {: a:ptr u:n aoff:n uoff:n :}
   LBL LBL {: bad done :}
   12 G-POP  11 G-POP
   9 DATA PEND-CELL LDR,  9 bad CBZ,
   11 DATA aoff STR,  12 DATA uoff STR,
   done B,
   a u bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ trust-sig! ( ptr u8 n -- )
: TRUST-SIG ( -- )
   s" E-SEAL-VIOLATION habu: trust-sig! with no definition pending"
   TSIG-A-CELL TSIG-U-CELL PENDING-SPAN ;

\ created-sig! ( ptr u8 n -- )
: CREATED-SIG ( -- )
   s" E-SEAL-VIOLATION habu: created-sig! with no definition pending"
   TCSIG-A-CELL TCSIG-U-CELL PENDING-SPAN ;

\ The close def-close runs; PEND-CELL clears last.
: NATIVE-CLOSE ( -- )
   LBL {: noaccept:label :}
   11 DATA NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:PENDING-TAKE cells + LDR,
   11 noaccept CBZ,
      SP SP 16 SUBI,  11 SP 8 STR,
      9 0 MOVZ,
      9 DATA NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:PENDING-TAKE cells + STR,
      10 DATA BODY-TEXT:BASE-CELL LDR,  10 G-PUSH
      10 DATA BODYLEN-CELL LDR,  10 G-PUSH
      11 SP 8 LDR,
      C-CALL-X11-SAVED
      SP SP 16 ADDI,
   noaccept LBL,
   C-CLEAR-TRUSTED-STATE
   C-CLEAR-LOCALS
   9 0 MOVZ,  9 DATA PEND-CELL STR, ;

\ def-close ( -- )
: DEF-CLOSE ( -- )
   LBL LBL LBL {: bad done nolast :}
   9 DATA PEND-CELL LDR,  9 bad CBZ,
   10 DATA NCOMP-DISPATCH:COMP-LATCH-CELL LDR,  10 bad CBZ,
   10 9 16 LDR,  10 10 DKIND:MASK ANDI,  10 nolast CBZ,   \ a published body that pushes a cell is the slot `does>` patches (LDOESPATCH), set after the body as the kernel's definers set it
   11 DKIND:CAST LIT64,  10 11 CMP,  C-EQ nolast BCOND,
   9 DATA LASTC-CELL STR,
   nolast LBL,
   NATIVE-CLOSE
   done B,
   s" E-SEAL-VIOLATION habu: def-close with no definition pending or no compiler latched"
   bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

\ The end of the def-create row, for record x9 in slot NDICT.
: ADDR-TAIL, ( -- )
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,  CP 11 0 STR,
   11 DATA 0 LDR,  11 11 7 ADDI,  11 11 3 LSRI,  11 11 3 LSLI,   \ standard CREATE: round the data field up to a cell
   11 DP-CHECK  11 DATA 0 STR,
   11 DATA COMMA-BASE-CELL STR,
   C-DATA-ADDR
   9 W-RET LIT64,  LCEMIT LABEL@ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 11 8 STR,
   10 9 16 LDR,  10 10 DKIND:ADDR ORRI,
   10 10 DNAME-ALLOC ORRI,  10 11 16 STR,  \ the DATA allocation starts at its leading carrier
   9 DATA LASTC-CELL STR,
   EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL,  9 9 0 LDR,   \ publish record NDICT-1; x9 = body start for the flush
   LFLUSH LABEL@ BL, ;

\ The fixed shadow callback, when one is installed, learns the created record
\ ADDR-TAIL, just published, NDICT - 1, and the DATA address its body pushes.
: ADDR-SHADOW, ( -- )
   LBL {: noshadow:label :}
   11 DATA NCOMP-DISPATCH:FIXED-SHADOW-CELL LDR,  11 noshadow CBZ,
   9 DATA DP-CELL LDR,  9 G-PUSH
   9 COMP-BODY:address COMP:BODY>N MOVZ,  9 G-PUSH
   9 NDICT 0 ADDI,  9 9 1 SUBI,  9 G-PUSH
   11 DATA NCOMP-DISPATCH:FIXED-SHADOW-CELL LDR,  C-CALL-X11-SAVED
   noshadow LBL, ;

private

\ The end of a definition with no compiled body, the def-create and def-cast
\ rows alike: the pending record must be of kind `kind`, with CP below the
\ code ceiling. `tail` writes the row's code, then the state def-open set
\ clears, PEND-CELL last.
: TAIL-CLOSE ( ptr u8 n n [ -- ] -- )
   {: a:ptr u:n kind:n tail :}
   LBL LBL {: bad done :}
   9 DATA PEND-CELL LDR,  9 bad CBZ,
   9 NDICT REC-AT,
   10 9 16 LDR,  10 10 DKIND:MASK ANDI,  11 kind LIT64,  10 11 CMP,  C-NE bad BCOND,
   10 REGION $4000 - LIT64,  10 DBASE 10 ADD,  CP 10 CMP,  C-CS bad BCOND,
   tail execute
   C-CLEAR-TRUSTED-STATE
   9 0 MOVZ,  9 DATA PEND-CELL STR,
   done B,
   a u bad ENGINE-ERROR:SEAL-VIOLATION REFUSE-AT
   done LBL, ;

public

\ def-create ( -- ): a pending DKIND:ADDR record takes ADDR-TAIL, and its
\ shadow, as `create` does.
: DEF-CREATE ( -- )
   s" E-SEAL-VIOLATION habu: def-create with no definition pending, one not of the kind, or CP at the code ceiling"
   DKIND:ADDR [: ADDR-TAIL,  ADDR-SHADOW, ;] TAIL-CLOSE ;

;package

package ENGINE-EMIT
public

\ The second end of a cast's publish, emitted by its empty body; the first end
\ is EM-COMPILE-RET. The recorded length leaves the return out.
: EM-COMPILE-FLUSH-PEND ( -- )
   DOES-REC:FLUSH
   11 DATA PEND-CELL LDR,
   9 11 0 LDR,  10 CP 9 SUB,
   10 10 4 SUBI,
   12 DATA REGION-ALIAS:DELTA-CELL LDR,  12 11 12 ADD,  10 12 8 STR,
   LFLUSH LABEL@ BL, ;
;package

\ The cast's body and the def-cast row join the definition writers, past the
\ two ends of a publish they emit.
package DEFWRITE
public

\ The empty checked body of a cast, for record x9 pending in slot NDICT. The end
\ of `cast:` and of the def-cast row alike, as ADDR-TAIL, is of `create` and
\ def-create.
: EMPTY-BODY, ( -- )
   10 DATA REGION-ALIAS:DELTA-CELL LDR,  10 9 10 ADD,  CP 10 0 STR,
   9 CP 0 ADDI,  9 DATA FRAME-CELL STR,              \ the entry slot
   9 W-LINKSAVE LIT64,  LCEMIT LABEL@ BL,            \ str x30,[sp,#-16]!
   EM-COMPILE-RET                                    \ empty body: the slot becomes a nop
   9 DATA PEND-CELL LDR,  9 9 0 LDR,
   EM-COMPILE-FLUSH-PEND ;

;package

package INTERP-EMIT

public
: C-IDENTITY-SHADOW ( -- )
   LBL {: noshadow:label :}
   11 DATA NCOMP-DISPATCH:FIXED-SHADOW-CELL LDR,  11 noshadow CBZ,
   9 0 MOVZ,  9 G-PUSH
   9 COMP-BODY:cast COMP:BODY>N MOVZ,  9 G-PUSH
   9 NDICT 0 ADDI,  9 9 1 SUBI,  9 G-PUSH
   11 DATA NCOMP-DISPATCH:FIXED-SHADOW-CELL LDR,  C-CALL-X11-SAVED
   noshadow LBL, ;
;package

package DEFWRITE
public

: DEF-CAST ( -- )
   s" E-SEAL-VIOLATION habu: def-cast with no definition pending, one not of the kind, or CP at the code ceiling"
   DKIND:CAST [: EMPTY-BODY,  EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL,
      INTERP-EMIT:C-IDENTITY-SHADOW ;]
   TAIL-CLOSE ;

;package

package ENGINE-EMIT

: C-EMITW ( n -- ) 9 swap LIT64,  LCEMIT LABEL@ BL, ;

\ The whole body in one emission, frame included: it reaches its target through
\ `blr x16`, so the frame is never the leaf kind and there is nothing for
\ EM-COMPILE-RET to decide. FRAME-CELL is not touched -- this word is emitted
\ from the interpret-mode `defer` declarer, outside any open body.
: C-DEFER-EMIT-CODE ( -- )
   W-LINKSAVE C-EMITW
   11 DATA DEFER-XT-CELL LDR,
   C-DATA-ADDR-RAW                                 \ movz/movk x9 = dispatch-cell addr (relocatable data addr)
   16 9 0 W-LDRX C-EMITW
   16 CP ENC-CMP C-EMITW                           \ `is` stores an xt
   8 C-CC ENC-BCOND C-EMITW                        \ below CP: past the ISB
   ENC-ISB C-EMITW
   C-CALL-BLR-X16 C-EMITW
   W-LINKREST C-EMITW
   W-RET C-EMITW ;

: C-DEFER-META-WRITE ( -- )
   1 DATA REGION-ALIAS:DELTA-CELL LDR,  1 CP 1 ADD,
   11 DEFER-MAGIC LIT64,  11 1 0 STR,
   11 DATA DEFER-XT-CELL LDR,  11 1 8 STR,  28 28 16 ADDI, ;

public
\ The end of `defer` and of the def-defer row alike, for record x9 in slot
\ NDICT and the dispatch cell in DEFER-XT-CELL: [0] is CP, the code loads the
\ cell and calls what it holds, [8] takes the code's length, the DEFER-MAGIC
\ trailer naming the cell follows, the code is native and flushed.
: DEFER-BODY, ( -- )
   10 DATA REGION-ALIAS:DELTA-CELL LDR,  10 9 10 ADD,  CP 10 0 STR,
   C-DEFER-EMIT-CODE
   9 DATA PEND-CELL LDR,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 CODE-SPAN:FULL ORRI,
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,  10 11 8 STR,
   C-DEFER-META-WRITE
   9 DATA PEND-CELL LDR,  9 9 0 LDR,
   LFLUSH LABEL@ BL, ;

\ The native compiler's fixed shadow callback, when one is installed, learns
\ the deferred record just published, NDICT - 1, and its dispatch cell.
: DEFER-SHADOW, ( -- )
   LBL {: noshadow:label :}
   11 DATA NCOMP-DISPATCH:FIXED-SHADOW-CELL LDR,  11 noshadow CBZ,
   9 DATA DEFER-XT-CELL LDR,  9 G-PUSH
   9 COMP-BODY:defer COMP:BODY>N MOVZ,  9 G-PUSH
   9 NDICT 0 ADDI,  9 9 1 SUBI,  9 G-PUSH
   11 DATA NCOMP-DISPATCH:FIXED-SHADOW-CELL LDR,  C-CALL-X11-SAVED
   noshadow LBL, ;

;package

\ def-defer ( n -- ): a pending kind-0 record takes DEFER-BODY, for dispatch
\ cell n. The cell is popped before TAIL-CLOSE checks: its refusals exit, never
\ throw.
package DEFWRITE
public

: DEF-DEFER ( -- )
   11 G-POP  11 DATA DEFER-XT-CELL STR,
   s" E-SEAL-VIOLATION habu: def-defer with no definition pending, one not of the kind, or CP at the code ceiling"
   0 [: DEFER-BODY,  EM-DEF-OCC:LAPPEND LABEL@ BL,  LHIDXADD LABEL@ BL,
      DEFER-SHADOW, ;]
   TAIL-CLOSE ;

;package

package ENGINE-EMIT

\ DRAIN-PRETRUST ( -- ): for each pending pre-trust defer, last slot first,
\ calls `trust-decl` with the slot's name and signature, then the target
\ owner's `checker-defer` with the name, and drops the slot; a target with no
\ `checker-defer` ends the drain. Runs once after `: TRUST`, where both words
\ exist. The band count is the loop state, so it survives the checker BLR
\ calls. A framed prim (FPRIM).
: BDRAINPRETRUST ( -- )
   LBL LBL {: loop done :}
   loop LBL,
      12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,  13 12 0 LDR,  13 done CBZ,   \ remaining==0 -> done
      DEF-TRUST:FIND                                  \ x11 = trust-decl XT (clobbers x12-x16)
      12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,  13 12 0 LDR,  13 13 1 SUBI,  \ index = remaining-1
      15 PD-SLOT MOVZ,  15 13 15 MUL,  14 12 PD-SLOTS-REL ADDI,  14 14 15 ADD,   \ x14 = slot base (x11 preserved)
      9 14 PD-NAME-OFF ADDI,  9 G-PUSH   9 14 PD-NLEN-OFF LDR,  9 G-PUSH
      9 14 PD-SIG-OFF ADDI,   9 G-PUSH   9 14 PD-SLEN-OFF LDR,  9 G-PUSH
      C-CALL-X11-SAVED
      NCOMP-DISPATCH:DECL-DEFER-OFF done DECL-OWNER:TARGET  \ x11 = target defer XT
      12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,  13 12 0 LDR,  13 13 1 SUBI,
      15 PD-SLOT MOVZ,  15 13 15 MUL,  14 12 PD-SLOTS-REL ADDI,  14 14 15 ADD,
      9 14 PD-NAME-OFF ADDI,  9 G-PUSH   9 14 PD-NLEN-OFF LDR,  9 G-PUSH
      C-CALL-X11-SAVED
      12 PD-TABLE-OFF LIT64,  12 DATA 12 ADD,  13 12 0 LDR,  13 13 1 SUBI,  13 12 0 STR,
      loop B,
   done LBL, ;

public
\ `immediate` writes one flag cell of the record just published and emits
\ nothing, through the alias. The imm-mark row's body is this word too.
: C-IMMEDIATE ( -- )
   9 NDICT 0 ADDI,  9 9 1 SUBI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   11 DATA REGION-ALIAS:DELTA-CELL LDR,  11 9 11 ADD,   \ x11 = the record through the alias
   10 9 16 LDR,  10 10 DNAME-IMM ORRI,  10 11 16 STR, ;

private

\ LEXITHOOK: EXIT-HOOK-CELL's trampoline, BL-called with the code in x0; it
\ keeps x0 and LR only.
: EMIT-EXITHOOK ( -- )
   LBL LBL {: nohk:label end:label :}
   LEXITHOOK LABEL@ {: start:label :}
   s" (LEXITHOOK)" start LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start LBL,
   9 DATA EXIT-HOOK-CELL LDR,  9 nohk CBZ,
   SP SP 16 SUBI,  30 SP 0 STR,  0 SP 8 STR,
   10 0 MOVZ,  10 DATA EXIT-HOOK-CELL STR,  10 DATA HND-CELL STR,
   9 BLR,
   0 SP 8 LDR,  30 SP 0 LDR,  SP SP 16 ADDI,
   nohk LBL,
   RET,
   end LBL, ;

;package

package ENGINE-EMIT

public

public
variable LEVALREC             \ re-entrant evaluate: throw-escape recovery entry
private
variable LEVLL  variable LEVLP  variable LEVLD  variable LEVLN \ LEVALREC internal labels

public
: EM-ENTRY-ARGS ( -- )
   HB-TARGET-LINUX? IF
      13 SP 0 LDR,  14 SP 8 ADDI,
      15 13 1 ADDI,  15 15 3 LSLI,  15 14 15 ADD,
      exit
   THEN
   HB-TARGET-MACOS? IF
      13 0 0 ADDI,  14 1 0 ADDI,  15 2 0 ADDI,
      exit
   THEN
   C-TARGET-UNKNOWN ;

public

\ The boot task's return stack and DO/LOOP frame stack. They come after the DATA
\ region because their bases are published in header cells; a task maps its own
\ pair and publishes them into its own region copy.
: EM-FRAME-STACKS ( -- )
   STACK-ABI:RETURN-BYTES 10 STACK-GUARD:EMIT-MAP
   10 DATA STACK-ABI:RETURN-BASE-CELL STR,
   STACK-ABI:LOOP-BYTES 10 STACK-GUARD:EMIT-MAP
   10 DATA STACK-ABI:LOOP-BASE-CELL STR, ;

: EM-MMAP-CODE-REGION ( -- )
   LBL LBL {: rok:label rvok:label :}
   \ Hint the region at __text (XREG-RBASE/x20) + REGION-OFF and take the base
   \ the kernel returns. A fixed address cannot be reserved. Region-to-text call
   \ displacements are relocated, so the base only has to lie within BL reach of
   \ __text, which the assertion below checks. PROT-PAGE-MAX extra bytes and a
   \ round-up keep the base PROT-PAGE-MAX aligned when the kernel returns an
   \ address only page aligned.
   6 REGION-OFF LIT64,  6 XREG-RBASE 6 ADD,
   9 PROT-PAGE-MAX 1 - MOVZ,  6 6 9 ADD,
   9 PROT-PAGE-MAX negate LIT64,  6 6 9 AND,       \ x6 = page-aligned canonical hint
   \ Clamp the hint after the last segment described by the loaded image.
   9 CODE-OFF MOVZ,  9 XREG-RBASE 9 SUB,           \ x9 = this run's image base
   HB-TARGET-LINUX? IF
      7 9 136 LDR,  8 9 80 LDR,  7 7 8 SUB,
      8 9 160 LDR,
   ELSE
      7 9 $1B0 LDR,  8 9 $80 LDR,  7 7 8 SUB,
      8 9 $1B8 LDR,
   THEN
   7 7 8 ADD,                                      \ x7 = final segment end offset
   7 9 7 ADD,
   8 PROT-PAGE-MAX 1 - MOVZ,  7 7 8 ADD,
   8 PROT-PAGE-MAX negate LIT64,  7 7 8 AND,
   6 7 CMP,  6 6 7 C-HI CSEL,                      \ x6 = the higher of hint and image end
   0 6 0 ADDI,  1 REGION PROT-PAGE-MAX + LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   \ SYS, leaves the same syscall-error convention on both targets -- Darwin sets the
   \ carry flag, and the Linux emitter reconciles -errno into it -- so a failed mapping
   \ is read from the flag the kernel set, not guessed from the shape of x0.
   C-CC rok BCOND,
      s" E-MAP-FAIL habu: cannot map fixed code region" 78 DIE-LINE,
   rok LBL,
   9 PROT-PAGE-MAX 1 - MOVZ,  0 0 9 ADD,
   9 PROT-PAGE-MAX negate LIT64,  0 0 9 AND,       \ page-aligned region base
   \ Boot BL-range assertion: the whole region must lie within +/-128 MiB of
   \ __text. x0 (region base) survives.
   9 REGION LIT64,  9 0 9 ADD,  9 9 XREG-RBASE SUB,
   5 BL-REACH LIT64,  9 5 CMP,
   C-CC rvok BCOND,
      s" E-BL-RANGE habu: code region out of BL range" BL-RANGE-RC DIE-LINE,
   rvok LBL,
   \ The region's two views: code runs from this base, which turns RX here, and
   \ stores go through a RW alias of [base, base+REGION). x21 carries alias -
   \ base to the start-up that publishes it; nothing from here to there uses
   \ x21. The calls keep x19/x20 and argc/argv/envp. macOS remaps the anonymous
   \ pages; Linux maps one memfd twice, the RX view over the reservation, whose
   \ slack on both sides of [base, base+REGION), below the rounded-up base and
   \ past its end, stays anonymous RW.
   HB-TARGET-MACOS? IF
      LBL LBL {: afail:label adone:label :}
      SP SP 32 SUBI,  13 SP 0 STR,  14 SP 8 STR,  15 SP 16 STR,  0 SP 24 STR,
      16 XREG-RBASE 0 ADDI,  false afail LIBC-OS:REGION-REMAP
      21 0 0 ADDI,
      0 SP 24 LDR,  1 REGION LIT64,  2 5 MOVZ,  NR-MPROTECT SYS,
      C-CS afail BCOND,
      13 SP 0 LDR,  14 SP 8 LDR,  15 SP 16 LDR,  0 SP 24 LDR,  SP SP 32 ADDI,
      adone B,
      afail LBL,
         s" E-MAP-FAIL habu: cannot map the code region's alias" 78 DIE-LINE,
      adone LBL,
   ELSE
      LBL LBL {: vfail:label vdone:label :}
      9 0 0 ADDI,  vfail LIBC-OS:REGION-MEMFD  10 0 0 ADDI,   \ x9 = base, x10 = fd
      0 9 0 ADDI,  1 REGION LIT64,  2 5 MOVZ,  3 MAP-SHARED-FIXED LIT64,
      4 10 0 ADDI,  5 0 MOVZ,  NR-MMAP SYS,  C-CS vfail BCOND,
      0 0 MOVZ,  1 REGION LIT64,  2 3 MOVZ,  3 MAP-SHARED LIT64,
      4 10 0 ADDI,  5 0 MOVZ,  NR-MMAP SYS,  C-CS vfail BCOND,
      21 0 9 SUB,
      0 10 0 ADDI,  NR-CLOSE SYS,  C-CS vfail BCOND,
      0 9 0 ADDI,  vdone B,
      vfail LBL,
         s" E-MAP-FAIL habu: cannot map the code region's views" 78 DIE-LINE,
      vdone LBL,
   THEN ;

;package

package ENGINE-EMIT

public
: EM-MMAP-DATA-REGION ( -- )
   LBL {: dvok :}
   0 DATA-VA VA>N LIT64,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 MAP-DATA-RESERVE LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 DATA-VA VA>N LIT64,  0 5 CMP,
   C-EQ dvok BCOND,
      s" E-MAP-FAIL habu: cannot map fixed data region" 78 DIE-LINE,
   dvok LBL, ;

;package

package RELOC-EMIT
public

\ Saves x0-x17, all MARK writes: its callers are mid-handler or mid-word.
: MARK-SAVE ( -- )
   SP SP 192 SUBI,
   18 0 ?do i SP i cells STR, loop
   16 DATA-VA VA>N LIT64,  16 SP 184 STR, ;

: MARK-RESTORE ( -- )
   18 0 ?do i SP i cells LDR, loop
   SP SP 192 ADDI, RET, ;

\ x16 = the table's base from SP+184: DATA-VA, never a task's x20, or the open
\ image's base for a cell inside it. SYS, clobbers x16: reload after each.
: MARK-HEADER ( -- )
   16 SP 184 LDR,
   4 SNAP-RELOC:XTCELL-N-CELL LIT64, 4 16 4 ADD, ;

\ One owner serializes lookup and publication, including backing replacement.
\ x16 is the table's base; all registers touched here are MARK-SAVE'd.
: MARK-LOCK ( -- )
   LBL {: retry:label :}
   17 16 ADDRESS-CELLS:LOCK-CELL ADDI,
   7 1 MOVZ,
   retry LBL, 6 0 MOVZ,
   6 7 17 CASAL,
   6 retry CBNZ, ;

: MARK-UNLOCK ( -- )
   17 16 ADDRESS-CELLS:LOCK-CELL ADDI,
   6 0 MOVZ, 6 17 STLR, ;

\ Reacquire the row backing after SYS,. Only ordinals live in the index, so
\ moving this backing does not invalidate any entry.
: MARK-ROWS ( -- )
   5 4 ADDRESS-CELLS:BASE-FIELD LDR,
   11 4 ADDRESS-CELLS:MODE-FIELD LDR,
   LBL {: mapped:label :}
   11 mapped CBNZ, 5 16 5 ADD, mapped LBL, ;

\ x8 = owned index mapping; x10 = slot capacity on return. Requested bytes
\ and geometric rounding are bounded separately. An incoming snapshot pointer
\ is cleared without entering here: only this process can own the mapping.
: INDEX-SHAPE ( label -- ) {: bad:label :}
   8 0 CMPI, C-LE bad BCOND,
   6 8 7 ANDI, 6 bad CBNZ,
   7 $7FFFFFFFFFFFFFFF ADDRESS-CELLS:INDEX-HEADER - LIT64,
   8 7 CMP, C-HI bad BCOND,
   10 8 ADDRESS-CELLS:INDEX-SLOTS LDR,
   10 2 CMPI, C-LT bad BCOND,
   7 ADDRESS-CELLS:INDEX-MAX-SLOTS LIT64, 10 7 CMP, C-HI bad BCOND,
   7 10 1 SUBI, 6 10 7 AND, 6 bad CBNZ,
   6 $7FFFFFFFFFFFFFFF LIT64, 6 6 8 SUB,
   7 10 3 LSLI, 7 7 ADDRESS-CELLS:INDEX-HEADER ADDI,
   7 6 CMP, C-HI bad BCOND, ;

\ x8 = index, x10 = slots, x5 = current rows, x13 = row count, x12 = key.
\ A slot holds ordinal+1; compare the exact offset in that authoritative row.
\ Return x7 = tagged row on a hit, or x6 = empty slot / x11 = its bucket.
\ Probe at most the capacity even if a malformed derived table has no hole.
: INDEX-PROBE ( label label label -- ) {: hit:label miss:label bad:label :}
   LBL {: probe:label :}
   7 $9E3779B97F4A7C15 LIT64, 11 12 7 MUL,
   7 11 32 LSRI, 11 11 7 EOR,
   17 10 1 SUBI, 11 11 17 AND, 14 10 0 ADDI,
   probe LBL,
      14 bad CBZ,
      6 11 3 LSLI, 6 8 6 ADD, 6 6 ADDRESS-CELLS:INDEX-HEADER ADDI,
      9 6 0 LDR, 9 miss CBZ,
      9 9 1 SUBI, 9 13 CMP, C-CS bad BCOND,
      7 9 3 LSLI, 7 5 7 ADD, 7 7 0 LDR,
      9 SNAP-RELOC:XTCELL-OFF-MASK LIT64, 9 7 9 AND,
      9 12 CMP, C-EQ hit BCOND,
      11 11 1 ADDI, 11 11 17 AND,
      14 14 1 SUBI, probe B, ;

\ Build a replacement solely from the ordered rows. It is complete before the
\ old index is unmapped; failure leaves authoritative rows/header untouched.
\ Existing duplicate keys retain their first ordinal.
\ Stack scratch: 144 new slots, 152 new map, 160 incoming key, 168 old map.
: INDEX-BUILD ( label label label -- ) {: lookup:label full:label bad:label :}
   LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: size:label sized:label mapped:label rows:label hole:label next:label
      built:label publish:label released:label :}
   8 SP 168 STR, 12 SP 160 STR,
   7 ADDRESS-CELLS:INDEX-MAX-SLOTS 2 / 1- LIT64,
   13 7 CMP, C-HI full BCOND,
   7 13 1 ADDI, 7 7 1 LSLI, 10 2 MOVZ,
   size LBL, 10 7 CMP, C-CS sized BCOND,
      6 ADDRESS-CELLS:INDEX-MAX-SLOTS 2 / LIT64,
      10 6 CMP, C-HI full BCOND,
      10 10 1 LSLI, size B,
   sized LBL,
   10 SP 144 STR,
   0 0 MOVZ, 1 10 3 LSLI, 1 1 ADDRESS-CELLS:INDEX-HEADER ADDI,
   2 3 MOVZ, 3 MAP-ANON-PRIVATE LIT64, 4 0 MOVN, 5 0 MOVZ,
   NR-MMAP SYS, C-CS full BCOND,
   0 mapped CBNZ,
      1 SP 144 LDR, 1 1 3 LSLI, 1 1 ADDRESS-CELLS:INDEX-HEADER ADDI,
      NR-MUNMAP SYS, full B,
   mapped LBL,
   0 SP 152 STR, 8 0 0 ADDI,
   MARK-HEADER MARK-ROWS
   10 SP 144 LDR,
   10 8 ADDRESS-CELLS:INDEX-SLOTS STR,
   13 8 ADDRESS-CELLS:INDEX-COUNT STR,
   3 0 MOVZ,
   rows LBL, 3 13 CMP, C-CS built BCOND,
      7 3 3 LSLI, 7 5 7 ADD, 12 7 0 LDR,
      7 SNAP-RELOC:XTCELL-OFF-MASK LIT64, 12 12 7 AND,
      next hole bad INDEX-PROBE
   hole LBL, 7 3 1 ADDI, 7 6 0 STR,
   next LBL, 3 3 1 ADDI, rows B,
   built LBL,
   0 SP 168 LDR, 0 publish CBZ,
   1 0 ADDRESS-CELLS:INDEX-SLOTS LDR,
   1 1 3 LSLI, 1 1 ADDRESS-CELLS:INDEX-HEADER ADDI,
   NR-MUNMAP SYS, 0 released CBZ,
      0 SP 152 LDR, 1 SP 144 LDR,
      1 1 3 LSLI, 1 1 ADDRESS-CELLS:INDEX-HEADER ADDI,
      NR-MUNMAP SYS, full B,
   released LBL,
   MARK-HEADER
   publish LBL,
   8 SP 152 LDR, 8 16 ADDRESS-CELLS:INDEX-CELL STR,
   12 SP 160 LDR,
   MARK-ROWS
   lookup B, ;

\ The row was written and every fallible allocation/release is over. Publish
\ its derived entry and finally the live count while still holding the mutex.
: MARK-COMMIT-INDEX ( -- )
   8 16 ADDRESS-CELLS:INDEX-CELL LDR,
   11 SP 176 LDR, 7 11 3 LSLI,
   7 8 7 ADD, 7 7 ADDRESS-CELLS:INDEX-HEADER ADDI,
   13 13 1 ADDI, 13 7 0 STR,
   13 8 ADDRESS-CELLS:INDEX-COUNT STR,
   13 4 0 STR, ;

\ Raise register rd to DATA plus the DATA floor when it is lower, through the
\ scratch register rt: a rewind of DATA puts DP no lower than the floor.
: FLOOR-CLAMP, ( n n -- ) {: rd:n rt:n :}
   rt DATA DATA-FLOOR-CELL LDR,  rt DATA rt ADD,
   rd rt CMP,  rd rd rt C-HI CSEL, ;

\ x12 = absolute DP cut. Preserve x0..x17 for both recovery callers.
: EMIT-ROLLBACK ( -- )
   LBL LBL LBL {: addrfull:label no-owner:label missing:label :}
   LROLLBACK LABEL@ LBL,
   \ Registration order is not address order: a late xt! can declare an
   \ older cell. Keep every row below the cut, preserving its relative order.
   \ The index records its row count; a reduced count makes the next marker
   \ rebuild it before lookup, so stale ordinals cannot survive this cut.
   MARK-SAVE
   SP SP 16 SUBI,  30 SP 0 STR,
   11 DATA NCOMP-DISPATCH:TARGET-DECL-CELL LDR,  11 no-owner CBZ,
   11 11 CHECKER-OWNER-ABI:GRID-REVOKE-OFF LDR,  11 missing CBZ,
   9 12 DATA SUB,  9 G-PUSH  11 BLR,
   no-owner LBL,
   30 SP 0 LDR,  SP SP 16 ADDI,
   12 SP 96 LDR,
   MARK-HEADER MARK-LOCK MARK-ROWS
   12 12 16 SUB,                                  \ cut as DATA offset
   13 4 0 LDR, 10 4 ADDRESS-CELLS:CAP-FIELD LDR,
   LBL LBL LBL LBL LBL LBL LBL
   {: acopy:label acloop:label acdone:label afilter:label
      arloop:label arnext:label ardone:label :}
   11 afilter CBNZ,                               \ mapped backing survives DP rewind
   6 5 16 SUB, 6 12 CMP, C-HI acopy BCOND,
   7 12 6 SUB, 7 7 3 LSRI, 10 7 CMP, C-LS afilter BCOND,
acopy LBL,
   \ A snapshot/PERSIST may have placed the row backing in the abandoned
   \ DATA suffix. Move that existing backing outside DATA before rewinding.
   5 SP 144 STR, 10 SP 152 STR, 12 SP 160 STR, 13 SP 168 STR,
   0 0 MOVZ, 1 10 3 LSLI, 2 3 MOVZ,
   3 MAP-ANON-PRIVATE LIT64, 4 0 MOVN, 5 0 MOVZ,
   NR-MMAP SYS, C-CS addrfull BCOND, 0 addrfull CBZ,
   17 0 0 ADDI,
   MARK-HEADER
   5 SP 144 LDR, 10 SP 152 LDR, 12 SP 160 LDR, 13 SP 168 LDR,
   3 0 MOVZ,
acloop LBL,
   3 13 CMP, C-CS acdone BCOND,
   6 3 3 LSLI, 7 5 6 ADD, 7 7 0 LDR,
   6 17 6 ADD, 7 6 0 STR,
   3 3 1 ADDI, acloop B,
acdone LBL,
   17 4 ADDRESS-CELLS:BASE-FIELD STR,
   6 1 MOVZ, 6 4 ADDRESS-CELLS:MODE-FIELD STR,
   5 17 0 ADDI,
afilter LBL,
   3 0 MOVZ, 10 0 MOVZ,
   6 SNAP-RELOC:XTCELL-OFF-MASK LIT64,
arloop LBL,
   3 13 CMP, C-CS ardone BCOND,
   9 3 3 LSLI, 9 5 9 ADD, 7 9 0 LDR,
   8 7 6 AND, 8 12 CMP, C-CS arnext BCOND,
   9 10 3 LSLI, 9 5 9 ADD, 7 9 0 STR,
   10 10 1 ADDI,
arnext LBL,
   3 3 1 ADDI, arloop B,
ardone LBL,
   10 4 0 STR,
   MARK-UNLOCK
   MARK-RESTORE
   addrfull LBL,
   1 LXTMSG LABEL@ ADR, 0 2 MOVZ,
   2 XTMSG-LEN MOVZ, NR-WRITE SYS,
   0 SNAP-RELOC:XTCELL-RC MOVZ, NR-EXIT-GROUP SYS,
   missing LBL,
   9 -8574 LIT64,  9 G-PUSH  BTHROW ;

\ (MARK) spans [LMARK, end) as one sealed record: xt!'s direct BL lands on its entry.
: EMIT-MARK ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: common:label lookup:label full:label band:label kind:label
      ret:label shape:label mapped:label ready:label grow:label copy:label
      copied:label publish:label rebuild:label hit:label missing:label :}
   LMARK LABEL@ {: start:label :}
   LBL {: end:label :}
   s" (MARK)" start LABEL>N end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   start LBL, MARK-SAVE
   15 0 MOVZ, common B,
   LSCRATCHMARK LABEL@ LBL, MARK-SAVE
   15 SNAP-RELOC:XTCELL-DATA-TAG SNAP-RELOC:XTCELL-SCRATCH-TAG or LIT64,
   common B,
   LPTRMARK LABEL@ LBL, MARK-SAVE
   15 SNAP-RELOC:XTCELL-DATA-TAG LIT64,
   \ x9 = the cell, unchanged on return; x15 = its kind.
   common LBL,
   LBL {: host:label :}
   6 DATA BOOT:IMAGE-BASE-CELL LDR,  6 host CBZ,      \ x6 = the image's base
   9 6 CMP,  C-CC host BCOND,
   7 DATA-SIZE LIT64,  7 6 7 ADD,  9 7 CMP,  C-CS host BCOND,
   6 SP 184 STR,
   host LBL,
   MARK-HEADER
   \ A cell outside DATA and the open image, or a table that cannot grow, stops
   \ the process: a dropped row would leave a stale address in a saved image.
   12 9 16 SUB,
   6 SNAP-RELOC:XTCELL-OFF-MAX LIT64, 12 6 CMP, C-HI band BCOND,
   MARK-LOCK
   6 4 ADDRESS-CELLS:MAGIC-FIELD LDR,
   7 ADDRESS-CELLS:MAGIC LIT64, 6 7 CMP, C-NE shape BCOND,
   13 4 0 LDR, 10 4 ADDRESS-CELLS:CAP-FIELD LDR,
   7 ADDRESS-CELLS:MAX-ROWS LIT64,
   10 shape CBZ, 10 7 CMP, C-HI shape BCOND,
   13 10 CMP, C-HI shape BCOND,
   5 4 ADDRESS-CELLS:BASE-FIELD LDR,
   11 4 ADDRESS-CELLS:MODE-FIELD LDR,
   11 1 CMPI, C-EQ mapped BCOND, C-HI shape BCOND,
   6 DATA-SIZE LIT64, 5 6 CMP, C-HI shape BCOND,
   6 6 5 SUB, 6 6 3 LSRI, 10 6 CMP, C-HI shape BCOND,
   5 16 5 ADD, ready B,
   mapped LBL,
   5 0 CMPI, C-LE shape BCOND,
   6 5 7 ANDI, 6 shape CBNZ,
   6 $7FFFFFFFFFFFFFFF LIT64, 6 6 5 SUB, 6 6 3 LSRI,
   10 6 CMP, C-HI shape BCOND,
   ready LBL,
   8 16 ADDRESS-CELLS:INDEX-CELL LDR, 8 rebuild CBZ,
   shape INDEX-SHAPE
   7 8 ADDRESS-CELLS:INDEX-COUNT LDR,
   7 13 CMP, C-NE rebuild BCOND,
   7 10 1 LSRI, 13 7 CMP, C-HI shape BCOND,
   lookup LBL,
   8 16 ADDRESS-CELLS:INDEX-CELL LDR,
   10 8 ADDRESS-CELLS:INDEX-SLOTS LDR,
   hit missing shape INDEX-PROBE
   \ A cell declared again keeps one row, since a second would relocate it
   \ twice; a second kind stops the process.
   hit LBL,
      6 12 15 ORR, 7 6 CMP, C-EQ ret BCOND, kind B,
   missing LBL,
      7 13 1 ADDI, 6 10 1 LSRI, 7 6 CMP, C-HI rebuild BCOND,
      11 SP 176 STR,
      10 4 ADDRESS-CELLS:CAP-FIELD LDR,
      13 10 CMP, C-EQ grow BCOND,
      6 13 3 LSLI, 6 5 6 ADD, 7 12 15 ORR, 7 6 0 STR,
      MARK-COMMIT-INDEX ret B,
   rebuild LBL,
      lookup full shape INDEX-BUILD
   grow LBL,
      \ Overflow checks precede doubling and the byte product. mmap and copying
      \ complete before publication; failure preserves the old header and rows.
      7 ADDRESS-CELLS:MAX-ROWS 2 / LIT64,
      10 7 CMP, C-HI full BCOND,
      10 10 1 LSLI, 10 SP 144 STR,
      0 0 MOVZ, 1 10 3 LSLI, 2 3 MOVZ,
      3 MAP-ANON-PRIVATE LIT64, 4 0 MOVN, 5 0 MOVZ,
      NR-MMAP SYS, C-CS full BCOND,
      0 copied CBNZ,
      10 SP 144 LDR, 1 10 3 LSLI, NR-MUNMAP SYS, full B,
   copied LBL,
      0 SP 152 STR, 17 0 0 ADDI,
      MARK-HEADER
      5 4 ADDRESS-CELLS:BASE-FIELD LDR,
      11 4 ADDRESS-CELLS:MODE-FIELD LDR,
      LBL {: source:label :} 11 source CBNZ, 5 16 5 ADD, source LBL,
      14 0 MOVZ,
   copy LBL, 14 13 CMP, C-CS publish BCOND,
      6 14 3 LSLI, 7 5 6 ADD, 7 7 0 LDR,
      6 17 6 ADD, 7 6 0 STR,
      14 14 1 ADDI, copy B,
   publish LBL,
      \ An owned mapping is released only after the replacement is complete.
      \ A failed release discards the replacement and leaves the old view live.
      LBL LBL {: commit:label released:label :}
      11 commit CBZ,
      0 5 0 ADDI, 1 4 ADDRESS-CELLS:CAP-FIELD LDR, 1 1 3 LSLI,
      NR-MUNMAP SYS, 0 released CBZ,
      0 SP 152 LDR, 1 SP 144 LDR, 1 1 3 LSLI,
      NR-MUNMAP SYS, full B,
   released LBL,
      MARK-HEADER
   commit LBL,
      5 SP 152 LDR, 10 SP 144 LDR,
      6 13 3 LSLI, 6 5 6 ADD, 7 12 15 ORR, 7 6 0 STR,
      5 4 ADDRESS-CELLS:BASE-FIELD STR,
      10 4 ADDRESS-CELLS:CAP-FIELD STR,
      11 1 MOVZ, 11 4 ADDRESS-CELLS:MODE-FIELD STR,
      MARK-COMMIT-INDEX ret B,
   full LBL,
      s" E-XTCELL habu: address-cell storage allocation failed" SNAP-RELOC:XTCELL-RC DIE-LINE,
   shape LBL,
      s" E-XTCELL habu: invalid address-cell storage header" SNAP-RELOC:XTCELL-RC DIE-LINE,
   band LBL,
      s" E-XTBAND habu: snapshot address cell out of range" SNAP-RELOC:XTBAND-RC DIE-LINE,
   kind LBL,
      s" E-XTKIND habu: snapshot address cell kind mismatch" SNAP-RELOC:XTKIND-RC DIE-LINE,
   ret LBL, MARK-UNLOCK MARK-RESTORE
   end LBL, ;

: BSNAPSHOTFORMAT ( -- ) A SNAPSHOT-FORMAT:VERSION MOVZ, A G-PUSH ;

: BPTRCELLMARK ( -- )
   A G-POP
   LPTRMARK LABEL@ BL, ;

: BSCRATCHCELLMARK ( -- )
   A G-POP
   LSCRATCHMARK LABEL@ BL, ;

\ While a build writes an image, a store into this engine's header goes to the
\ same offset in the image, and marks the cell: its address goes to the cell
\ ROUTED-BYTES below it and to the cell below the image, in the band the build
\ maps there. Clobbers x12 and x13, as the store's guard does.
: ROUTE-HEADER ( n -- ) {: reg:n :}
   LBL {: own:label :}
   13 reg DATA SUB,  12 DATA-START LIT64,  13 12 CMP,  C-CS own BCOND,
   12 DATA BOOT:IMAGE-BASE-CELL LDR,  12 own CBZ,
   reg 12 13 ADD,
   13 12 8 SUBI,  reg 13 0 STR,
   12 ROUTED-BYTES LIT64,  13 reg 12 SUB,  reg 13 0 STR,
   own LBL, ;

\ xt! ( q ptr q -- ): store an execution token into a persisted cell and declare
\ the cell. `defer` and `is` declare cells known at compile time; a table of
\ dispatch cells picks its cell at run time, so only the store knows. The
\ declaration runs first, so a cell LMARK refuses is never written; LMARK wants
\ the cell in x9, so the token is parked in x11 (scratch here; LMARK preserves
\ x9 and saves the rest). Registered framed: LMARK is a BL.
: BXTSTORE ( -- )
   B G-POP  A G-POP
   B ROUTE-HEADER
   C A 0 ADDI,                                      \ x11 = the token
   A B 0 ADDI,                                      \ x9 = the cell, for LMARK
   LMARK LABEL@ BL,
   C B 0 STR, ;

\ ptr-cell! ( a ptr -- ): BXTSTORE for a DATA-pointer cell.
: BPTRCELLSTORE ( -- )
   B G-POP  A G-POP
   B ROUTE-HEADER
   C A 0 ADDI,                                      \ x11 = the pointer
   A B 0 ADDI,                                      \ x9 = the cell, for LPTRMARK
   LPTRMARK LABEL@ BL,
   C B 0 STR, ;

\ Record the MOVZ/MOVK address carrier whose first word is in x9.
\ Compile handlers supply CP before emission; seed relocation supplies its site.
\ Preserve x11, the literal value held by compile and seed callers. Both callers
\ have no live x30 across this call; compile handlers also call LCEMIT afterward.
\ Clobbers x5, x6, x9.
: EMIT-ADDR-SITE ( -- )
   LADDRSITE LABEL@ LBL,
   6 9 DBASE SUB,                                   \ x6 = chain's byte offset within the region
   9 6 0 ADDI,  9 9 2 LSRI,  9 9 7 ANDI,            \ x9 = bit number = word index & 7
   6 6 5 LSRI,                                      \ x6 = map byte index = offset >> 5
   5 SNAP-RELOC:ADDRMAP-OFF LIT64,  6 6 5 ADD,  6 DATA 6 ADD, \ x6 = map byte address
   5 1 MOVZ,  5 5 9 LSLV,  9 6 0 LDRB,  9 9 5 ORR,  9 6 0 STRB,
   RET, ;

;package

package ENGINE-EMIT

public

: EM-TEXT-SIZE ( -- )
   9 DATA RBASE-CELL LDR,  9 10 TEXT-SIZE,  9 DATA TEXT-SIZE-CELL STR, ;

private

\ A throw's recovery rolls the engine package scope back, but the checker's own
\ mirror moved with the failed input. Recovery arms PKGRESYNC-CELL; this drain
\ clears it and has the checker re-read the restored scope whatever it is:
\ resyncing only a global scope left a package open across the boundary refusing
\ E-PKG-CONTEXT. LEVALREC drains before delivering the throw.
: EM-PKG-RESYNC ( -- )
   LBL {: nosync:label :}
   9 DATA PKGRESYNC-CELL LDR,  9 nosync CBZ,
      9 0 MOVZ,  9 DATA PKGRESYNC-CELL STR,
      NCOMP-DISPATCH:DECL-PKG-RESYNC-OFF nosync DECL-OWNER:TARGET   \ x11 = the checker's resync (nosync if absent: cold load / no checker)
      C-CALL-X11-SAVED
   nosync LBL, ;

public
\ Definition state only: a Habu catch that survived the engine's BTHROW still owns
\ the handler, return and loop stacks that receive the error.
: EM-ABORT-COMPILE-STATE ( -- )
   9 0 MOVZ,
   9 DATA NCOMP-DISPATCH:COMP-LATCH-CELL STR,
   9 DATA NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:PENDING-TAKE cells + STR,
   9 DATA VSP-CELL STR,  9 DATA FRAME-CELL STR,
   9 DATA BODYLEN-CELL STR,  9 DATA EXITH-CELL STR,
   9 DATA PEND-CELL STR,  9 DATA CMM-CELL STR,
   9 0 MOVZ,
   9 DATA TSIG-A-CELL STR,   9 DATA TSIG-U-CELL STR,
   9 DATA TCSIG-A-CELL STR,  9 DATA TCSIG-U-CELL STR,
   9 DATA CRSIG-A-CELL STR,  9 DATA CRSIG-U-CELL STR,
   9 DATA DOESB-CELL STR,  9 DATA CLAUSE-CELL STR,
   9 DATA TRUSTED-CELL STR,
   C-CLEAR-LOCALS ;
;package

\ The def-abort and stack-clear bodies; FPRIM-WID frames x30 around each.
package DEFWRITE

public

: DEF-ABORT ( -- )
   EM-ABORT-COMPILE-STATE ;

: STACK-CLEAR ( -- )
   XDS DATA S0-CELL LDR, ;

;package

package ENGINE-EMIT
private

\ In x9/x10 = the token; out x13 = 0, or the FIND flags with x11 = code, x12 =
\ body-len and x5 = the record. Two distinct records bind nothing: x6 = 2.
: EMIT-FIND-USED ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: qscan:label qnone:label ret:label uloop:label mloop:label member:label
      ninl:label ncmp:label nmatch:label unext:label udone:label
      lscan:label kloop:label knext:label ploop:label pnext:label pinl:label
      pcmp:label pmatch:label pamb:label :}
   LFINDUSED-CORE LABEL@ LBL,
   13 0 MOVZ,
   17 0 MOVZ,
   6 0 MOVZ,                                                        \ the match count every exit answers
   qscan LBL,
      17 10 CMP,  C-GE qnone BCOND,
      14 9 17 ADD,  14 14 0 LDRB,  14 $3A CMPI,  C-EQ ret BCOND,   \ colon -> not a bare tail; miss
      17 17 1 ADDI,  qscan B,
   qnone LBL,
   14 USE-DEPTH-CELL LIT64,  14 DATA 14 ADD,  8 14 0 LDR,
   8 ret CBZ,
   7 USE-WIDS-OFF LIT64,  7 DATA 7 ADD,
   14 DATA HIDXP-CELL LDR,  14 lscan CBZ,                          \ no table -> the scan answers
   9 10 15 4 5 17 C-HIDX-HASH                                      \ x15 = folded-name hash, once
   16 0 MOVZ,  6 0 MOVZ,                                            \ x16 = matched record (0=none), x6 = match count
   3 0 MOVZ,                                                        \ x3 = used-wid index
   kloop LBL,
      3 8 CMP,  C-GE udone BCOND,
      2 3 3 LSLI,  2 7 2 ADD,  2 2 0 LDR,
      5 15 2 EOR,  4 HIDX-SLOTS 1 - LIT64,  5 5 4 AND,             \ x5 = slot = (hash XOR wid) & mask
      4 HIDX-SLOTS LIT64,                                          \ x4 = step budget
   ploop LBL,
      12 5 2 LSLI,  12 14 12 ADD,  11 12 0 LDRW,                   \ x11 = slot value (index+1)
      11 knext CBZ,                                                \ empty slot -> absent from this wid
      11 11 1 SUBI,  11 NDICT CMP,  C-GE pnext BCOND,              \ stale (truncated) index
      12 DREC MOVZ,  12 11 12 MUL,  12 DBASE 12 ADD,
      11 12 40 LDR,  11 2 CMP,  C-NE pnext BCOND,                  \ wid mismatch
      11 12 16 LDR,  11 11 DNAME-FLAG-BITS LSLI,  11 11 DNAME-FLAG-BITS LSRI,  11 10 CMP,  C-NE pnext BCOND,  \ name-len mismatch
      17 12 24 ADDI,
      11 12 16 LDR,  11 11 DNAME-EXT ANDI,  11 pinl CBZ,
         17 12 24 LDR,
      pinl LBL,
      13 0 MOVZ,
      pcmp LBL,
         \ x2 and x11 carry the two bytes, x12 is the fold scratch; the record
         \ pointer is recomputed at pmatch and the wid reloaded from USE-WIDS[x3]
         \ at pnext.
         13 10 CMP,  C-GE pmatch BCOND,
         11 17 13 ADD,  11 11 0 LDRB,
         12 11 $41 SUBI,  12 $1A CMPI,  12 C-CC CSET,  12 12 5 LSLI,  11 11 12 ORR,
         2 9 13 ADD,  2 2 0 LDRB,
         12 2 $41 SUBI,  12 $1A CMPI,  12 C-CC CSET,  12 12 5 LSLI,  2 2 12 ORR,
         11 2 CMP,  C-NE pnext BCOND,
         13 13 1 ADDI,  pcmp B,
      pmatch LBL,
         12 5 2 LSLI,  12 14 12 ADD,  12 12 0 LDRW,
         12 12 1 SUBI,  11 DREC MOVZ,  12 12 11 MUL,  12 DBASE 12 ADD,
         12 16 CMP,  C-EQ knext BCOND,                             \ same record again (same package used twice)
         6 6 1 ADDI,  6 2 CMPI,  C-GE pamb BCOND,                  \ 2nd distinct record -> ambiguous: x16 1st, x12 2nd
         16 12 0 ADDI,
   knext LBL,
      3 3 1 ADDI,  kloop B,
   pnext LBL,
      2 3 3 LSLI,  2 7 2 ADD,  2 2 0 LDR,
      4 4 1 SUBI,  4 lscan CBZ,                                    \ full wrap -> the scan answers
      5 5 1 ADDI,  12 HIDX-SLOTS 1 - LIT64,  5 5 12 AND,  ploop B,
   lscan LBL,
   16 0 MOVZ,  6 0 MOVZ,                                            \ x16 = matched record (0=none), x6 = match count
   5 DBASE 0 ADDI,  4 NDICT 0 ADDI,
   uloop LBL,
      4 udone CBZ,
      14 5 40 LDR,                                                 \ x14 = record wid
      3 0 MOVZ,
      mloop LBL,
         3 8 CMP,  C-GE unext BCOND,
         2 3 3 LSLI,  2 7 2 ADD,  15 2 0 LDR,
         15 14 CMP,  C-EQ member BCOND,
         3 3 1 ADDI,  mloop B,
      member LBL,
         15 5 16 LDR,  15 15 DNAME-FLAG-BITS LSLI,  15 15 DNAME-FLAG-BITS LSRI,  15 10 CMP,  C-NE unext BCOND,   \ name-len mismatch
         2 5 24 ADDI,
         14 5 16 LDR,  14 14 DNAME-EXT ANDI,  14 ninl CBZ,
            2 5 24 LDR,
         ninl LBL,
         17 0 MOVZ,
         ncmp LBL,
            17 10 CMP,  C-GE nmatch BCOND,
            15 2 17 ADD,  15 15 0 LDRB,
            3 15 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  15 15 3 ORR,
            14 9 17 ADD,  14 14 0 LDRB,
            3 14 $41 SUBI,  3 $1A CMPI,  3 C-CC CSET,  3 3 5 LSLI,  14 14 3 ORR,
            15 14 CMP,  C-NE unext BCOND,
            17 17 1 ADDI,  ncmp B,
         nmatch LBL,
            6 6 1 ADDI,  12 5 0 ADDI,
            6 2 CMPI,  C-GE pamb BCOND,                            \ 2nd distinct match -> ambiguous: x16 1st, x12 2nd
            16 5 0 ADDI,
      unext LBL,  5 5 DREC ADDI,  4 4 1 SUBI,  uloop B,
   udone LBL,
   13 0 MOVZ,                                                      \ the probes used x13 as a byte index
   16 ret CBZ,
   5 16 0 ADDI,
   11 5 0 LDR,  12 5 8 LDR,
   14 5 16 LDR,
   15 14 DNAME-WIDE ANDI,  15 15 59 LSRI,
   16 14 DNAME-INT ANDI,  16 16 59 LSRI,
   15 15 16 ORR,
   16 14 DNAME-MIN-IN-MASK ANDI,  16 16 44 LSRI,
   15 15 16 ORR,
   14 14 DNAME-IMM ANDI,  14 14 59 LSRI,
   14 14 15 ORR,
   13 1 MOVZ,  13 13 14 ORR,
   RET,                                                            \ hit: x5 is the matched record, LFIND's own output shape
   ret LBL,  5 0 MOVZ,  RET,                                       \ every miss leaves x5 = 0, including the two that never wrote it
   pamb LBL,  13 0 MOVZ,  5 0 MOVZ,  RET, ;
;package

package ENGINE-EMIT
\ LSCOPEREC: the name lookup as one leaf for every reader but the call site, in
\ the call site's order: LFIND (open package, enclosing one, global, or
\ NAME:tail), then the used publics only when LFIND missed. In: x9/x10 = the
\ token, preserved. Out: x14 = the bound record (0 when nothing binds), x16 =
\ the used publics' record (0 when none, the first of two), x17 = the second of
\ two, x15 = flags: 1 when the bound record is a seeded primitive, 2 when the
\ used publics hold two distinct records (then nothing binds and both are
\ named). Clobbers x2-x17 as the leaves do.
\ Seeded is the identity of an operator row's word: the boot copies the
\ primitives' records to dict[0..LNCOUNT), and every later record (a package
\ word, an EXPORT alias, a redefinition after `undefine`) sits above. An
\ operator row may claim its token only when the lookup bound that
\ primitive's own record.
: EMIT-SCOPE-REC ( -- )
   LBL LBL LBL {: two:label have:label notseed:label :}
   LSCOPEREC LABEL@ LBL,
   SP SP 32 SUBI,  30 SP 0 STR,  9 SP 8 STR,  10 SP 16 STR,
   LFIND LABEL@ BL,                                       \ x5 = the record, 0 on a miss
   5 SP 24 STR,
   9 SP 8 LDR,  10 SP 16 LDR,                             \ LFIND moved them onto a qualified tail
   LFINDUSED-CORE LABEL@ BL,                              \ x5 = the one used record, x6 = records met
   17 12 0 ADDI,
   6 2 CMPI,  C-GE two BCOND,
      16 5 0 ADDI,  17 0 MOVZ,
   two LBL,
   14 SP 24 LDR,  14 have CBNZ,
      14 5 0 ADDI,
   have LBL,
   6 2 CMPI,  15 C-GE CSET,  15 15 1 LSLI,
   14 notseed CBZ,
   12 LNCOUNT LABEL@ ADR,  12 12 0 LDR,  13 DREC MOVZ,  12 12 13 MUL,  12 DBASE 12 ADD,
   14 12 CMP,  C-CS notseed BCOND,
      13 1 MOVZ,  15 15 13 ORR,
   notseed LBL,
   9 SP 8 LDR,  10 SP 16 LDR,
   30 SP 0 LDR,  SP SP 32 ADDI,
   RET, ;

;package

package ENGINE-EMIT

private

: EM-RESET-COMPILE-STATE ( -- )
   EM-ABORT-COMPILE-STATE
   9 0 MOVZ,
   9 DATA RSP-CELL STR,  9 DATA HND-CELL STR,  9 DATA LOOPSP-CELL STR, ;

public
\ An undefined word in a `:` body is a rejected definition: 70 is both the
\ catchable throw code and the uncaught exit.
70 constant RC-REJECT

private
\ LEVALREC: a throw whose nearest handler lies beyond active evaluate boundaries
\ lands here, x15 = throw code. Each escaped frame gives back its input cursor,
\ data-stack base (XDS), package and using scope, and EVALERR-CELL records the
\ code. A frame begun with no definition open also rolls back everything
\ compiled since: CP/NDICT, DP, compile state and the checker's records; while a
\ task is live it retires the dictionary records made since instead and keeps
\ CP, NDICT and DP, rolling back the rest. A frame begun inside an open
\ definition is an immediate's evaluate compiling into an outer buffer's
\ definition, which it may not close: that definition stays open, put back to
\ the token-start mark. The mark is the innermost buffer's current token, or its
\ end when it exited cleanly; frames begun in an open definition nest inside the
\ one that owns the compile, so all pop while the mark holds. Popping stops when
\ EVALD reaches 0 or the nearest handler (x11, read once because
\ EM-RESET-COMPILE-STATE zeroes HND-CELL) is inside the current frame; delivery
\ then matches the non-evaluate path.
: EM-EVAL-THROW-RECOVER ( -- )
   LBL LBL LBL LBL {: owned:label scope:label unrefused:label kept:label :}
   LBL {: retired:label :}
   LEVALREC LABEL@ LBL,
   LBL LEVLL !  LBL LEVLP !  LBL LEVLD !  LBL LEVLN !
   11 DATA 8 LDR,
   LEVLL LABEL@ LBL,
      12 DATA EVALD-CELL LDR,  12 LEVLD LABEL@ CBZ,   \ no eval frame left → unwind to handler
      13 DATA EVAL-TOP-CELL LDR,
      14 13 24 LDR,                                   \ x14 = the frame's boundary; x13 stays &frame
      11 LEVLP LABEL@ CBZ,                            \ no handler → pop (escape)
      \ Handler strictly inside this frame -> unwind to it. The boundary is the
      \ evaluate's own catch handler, so an equal handler lies outside the
      \ buffer and the frame pops.
      11 14 CMP,  C-CC LEVLD LABEL@ BCOND,
   LEVLP LABEL@ LBL,
      14 13 STACK-ABI:EVAL-BASE LDR,
      14 DATA STACK-ABI:BASE-CELL STR,
      12 13 STACK-ABI:EVAL-CAP LDR,  12 DATA STACK-ABI:CAP-CELL STR,
      12 13 0 LDR,   12 DATA INP-CELL STR,
      12 13 8 LDR,   12 DATA INE-CELL STR,
      12 13 EVAL-INB LDR,  12 DATA SRCLOC:INB-CELL STR,
      XDS 13 32 LDR,
      12 13 EVAL-FRAME:PEND LDR,  12 owned CBZ,       \ no definition open as it began: it owns the compile
         \ The outer definition as the interrupted token began.
         12 DATA TOKBODY-CELL LDR,  12 DATA BODYLEN-CELL STR,
         12 DATA TOKLOCN-CELL LDR,  12 DATA LOCN-CELL STR,
         12 DATA TOKDEPTH-CELL LDR,  12 DATA CAPDEPTH-CELL STR,
         12 DATA TOKCP-CELL LDR,  CP DBASE 12 ADD,
         12 DATA TOKVSP-CELL LDR,  12 DATA VSP-CELL STR,
         scope B,
      owned LBL,
      SP SP 32 SUBI,  11 SP 0 STR,  13 SP 8 STR,  15 SP 16 STR,  30 SP 24 STR,   \ x30 for the exits below
      \ The checker's records go with the definitions, back to the end as the
      \ buffer began (EVAL-FRAME:ROWS: end+1, 0 with no owner then).
      13 EVAL-FRAME:ROWS kept DECL-OWNER:RETRACT-TO
      kept LBL,
      13 SP 8 LDR,  9 13 48 LDR,  EM-DEF-OCC:LRETIRE LABEL@ BL,  14 retired CBNZ,
      9 13 40 LDR,  EM-DEF-OCC:LRECLAIM LABEL@ BL,
      13 SP 8 LDR,
      CP 13 40 LDR,
      9 13 48 LDR,  EM-DEF-OCC:LCOUNT LABEL@ BL,  NDICT 13 48 LDR,
      12 13 56 LDR,  12 9 RELOC-EMIT:FLOOR-CLAMP,
      RELOC-EMIT:LROLLBACK LABEL@ BL,
      9 10 DP-LOWER,
      12 DATA DP-CELL STR,
      9 10 LASTC-TRIM,
      retired LBL,
      11 SP 0 LDR,  13 SP 8 LDR,  15 SP 16 LDR,  30 SP 24 LDR,  SP SP 32 ADDI,
      scope LBL,
      10 13 EVAL-PKG ADDI,
      9 10 PKGSNAP-CUR LDR,     9 DATA CUR-CELL STR,
      9 10 PKGSNAP-PUB LDR,     9 DATA PKG-PUB-CELL STR,
      9 10 PKGSNAP-PRI LDR,     9 DATA PKG-PRI-CELL STR,
      9 10 PKGSNAP-PARENT LDR,  9 DATA PKG-PARENT-CELL STR,
      9 10 PKGSNAP-REC LDR,     9 DATA PKG-REC-CELL STR,
      13 EVAL-FRAME:ENCLOSING 9 ENGINE-EMIT:ENCLOSING-RESTORE,
      9 10 PKGSNAP-USE LDR,     12 USE-DEPTH-CELL LIT64,  12 DATA 12 ADD,  9 12 0 STR,   \ roll the using-scope depth back too (x12 reloaded below)
      9 10 PKGSNAP:FLOOR LDR,   12 USE-PKG-SAVE-CELL LIT64,  12 DATA 12 ADD,  9 12 0 STR,   \ and the open package's using floor
      9 13 EVAL-FRAME:PKG-OUTSIDE LDR,  9 DATA PKG-OUTSIDE-CELL STR,   \ and whether the includer opened it
      9 13 EVAL-FRAME:SECTION LDR,  9 DATA SECTION-CELL STR,   \ and the section in force when it began
      13 EVAL-FRAME:USE-WIDS 12 9 ENGINE-EMIT:USE-WIDS-RESTORE,  \ and the includer's used publics, which the resync names
      9 1 MOVZ,  9 DATA PKGRESYNC-CELL STR,                  \ arm the checker resync (drained below, before delivery)
      12 13 EVAL-PREV LDR,  12 DATA EVAL-TOP-CELL STR,
      15 DATA EVALERR-CELL STR,
      12 DATA EVALD-CELL LDR,  12 12 1 SUBI,  12 DATA EVALD-CELL STR,
      12 13 EVAL-FRAME:PEND LDR,  12 LEVLL LABEL@ CBNZ, \ the outer buffer's definition keeps compiling
      EM-RESET-COMPILE-STATE                          \ clobbers x9 only; x11,x15 preserved
      LEVLL LABEL@ B,
   LEVLD LABEL@ LBL,
   \ The handler is compiled code and may ask the checker before the loop reads
   \ on, so the checker re-reads the restored package scope here. The call owns
   \ every caller-saved register, so x11 and x15 ride on the stack.
   SP SP 16 SUBI,  11 SP 0 STR,  15 SP 8 STR,
   EM-PKG-RESYNC
   11 SP 0 LDR,  15 SP 8 LDR,  SP SP 16 ADDI,
   9 15 0 ADDI,
   11 LEVLN LABEL@ CBZ,
   14 11 STACK-ABI:CATCH-BASE LDR,
   19 11 8 LDR,
   14 DATA STACK-ABI:BASE-CELL STR,
   10 11 STACK-ABI:CATCH-CAP LDR,  10 DATA STACK-ABI:CAP-CELL STR,
   10 11 STACK-ABI:CATCH-BUSY LDR,  10 DATA GENIO-ABI:BUSY-CELL STR,
   10 11 STACK-ABI:CATCH-ACTIVE LDR,  10 DATA GENIO-ABI:ACTIVE-CELL STR,
   10 11 40 LDR,  10 DATA RSP-CELL STR,
   10 11 48 LDR,  10 DATA LOOPSP-CELL STR,
   10 11 0 LDR,  10 DATA 8 STR,                       \ HND = prev
   30 11 32 LDR,  12 11 24 LDR,  13 11 16 LDR,
   SP 13 0 ADDI,  12 BR,
   LEVLN LABEL@ LBL,
   \ No handler: the uncaught-throw exit, x9 = x15 = code. LEVLN falls in;
   \ BTHROW reaches LUNCAUGHT through UNCGH-CELL. A code in [1,255] is a
   \ deliberate exit contract and exits as itself with no output. Any other code
   \ would be masked to `code & 0xFF`, so it is reported as "hb: uncaught throw
   \ code <n>\n" on fd 2 and exits UNCAUGHT-RC, or RC-REJECT when it is the
   \ refusal the checker last rendered, already named on fd 2. x15 keeps the
   \ code across the write (the kernel preserves x2-x15). Never returns: no RET
   \ keeps the FPRIM-L throw leaf-safe.
     LUNCAUGHT LABEL@ LBL,
   LBL LUNCRPT !  LBL LUNCPOS !  LBL LUNCLOOP !  LBL LUNCDONE !
   0 9 0 ADDI,
   \ One hook call serves both legs below. x0 is the trampoline's to preserve;
   \ x9 and x15 are re-derived from it because the hook is compiled code and
   \ owns every caller-saved register.
   LEXITHOOK LABEL@ BL,  9 0 0 ADDI,  15 0 0 ADDI,
   9 1 CMPI,    C-LT LUNCRPT LABEL@ BCOND,
   9 255 CMPI,  C-GT LUNCRPT LABEL@ BCOND,
   NR-EXIT-GROUP SYS,
   LUNCRPT LABEL@ LBL,
   1 LUNCMSG LABEL@ ADR,  0 2 MOVZ,  2 UNCMSG-LEN MOVZ,  NR-WRITE SYS,   \ "hb: uncaught throw code "
   9 15 0 ADDI,
   SP SP $20 SUBI,  12 SP $20 ADDI,
   13 $A MOVZ,  12 12 1 SUBI,  13 12 0 STRB,           \ trailing newline
   14 0 MOVZ,  9 0 CMPI,
   C-GE LUNCPOS LABEL@ BCOND,
   14 1 MOVZ,  9 SP 9 SUB,                             \ x9 = -x9 (abs); x14 = sign flag
   LUNCPOS LABEL@ LBL,
   10 $A MOVZ,
   LUNCLOOP LABEL@ LBL,                                \ unsigned: -MIN-N is MIN-N, magnitude 2^63
   11 9 10 UDIV,  13 11 10 MUL,  13 9 13 SUB,
   13 13 $30 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
   9 11 0 ADDI,  9 LUNCLOOP LABEL@ CBNZ,
   14 LUNCDONE LABEL@ CBZ,
   13 $2D MOVZ,  12 12 1 SUBI,  13 12 0 STRB,          \ leading '-'
   LUNCDONE LABEL@ LBL,
   0 2 MOVZ,  1 12 0 ADDI,  2 SP $20 ADDI,  2 2 12 SUB,
   NR-WRITE SYS,                                       \ write(2, digits, len)
   0 UNCAUGHT-RC MOVZ,
   9 DATA REFUSAL-ABI:CODE-CELL LDR,  9 unrefused CBZ,
   9 15 CMP,  C-NE unrefused BCOND,
   0 RC-REJECT MOVZ,
   unrefused LBL,
   NR-EXIT-GROUP SYS, ;

: EM-RECOVER-EXITS ( -- )
   \ LFLOORREC: C-CRASH-DATA-RECOVER resumes a thread here, every other register
   \ as it faulted, after an access to the data stack's low guard page: a word
   \ took more cells than the stack held, and no cell under the base was
   \ touched. XDS may be under the base, so the stub empties the stack before
   \ anything runs on it (LEVALREC restores XDS from the frame it delivers to;
   \ the uncaught exit runs the exit hook on the empty stack). TKA/TKL still
   \ name the token. The throw goes through LEVALREC: the fault is inside
   \ running code, maybe under a catch, which receives it as a throw.
   LFLOORREC LABEL@ LBL,
   XDS DATA STACK-ABI:BASE-CELL LDR,
   0 2 MOVZ,  1 LMINCLASS LABEL@ ADR,  2 MINCLASS-LEN MOVZ,  NR-WRITE SYS,
   15 30 0 ADDI,  LPLACE LABEL@ BL,  30 15 0 ADDI,   \ LEVALREC's exits print the faulting lr
   0 2 MOVZ,  1 LMINMSG LABEL@ ADR,  2 MINMSG-LEN MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   15 RC-REJECT MOVZ,  LEVALREC LABEL@ B, ;

\ LPLACE, a leaf (x0-x2, x9-x14 and SYS,'s registers; x15 kept), writes
\ `<path>:<line>: `, or `<path>: ` off the file's bytes, or nothing outside one.
: EM-SOURCE-PLACE ( -- )
   LBL LBL LBL LBL LBL {: noline:label nlloop:label nldone:label digloop:label done:label :}
   LPLACE LABEL@ LBL,
   9 DATA SRCLOC:PATHLEN-CELL LDR,  9 done CBZ,             \ no open source file -> nothing
   2 DATA SRCLOC:PATHLEN-CELL LDR,  1 DATA SRCLOC:PATH-CELL LDR,  0 2 MOVZ,  NR-WRITE SYS,
   10 DATA SRCLOC:OWNED-CELL LDR,  10 noline CBZ,           \ x10 = the file's first owned byte; none -> the path alone
   11 DATA SRCLOC:INB-CELL LDR,  11 10 CMP,  C-CC noline BCOND,       \ the buffer starts before the file -> the path alone
   12 DATA SRCLOC:OWNEDLEN-CELL LDR,  12 10 12 ADD,
   11 DATA INE-CELL LDR,  11 12 CMP,  C-HI noline BCOND,    \ the buffer ends past the file -> the path alone
   11 DATA INP-CELL LDR,
   12 1 MOVZ,                                               \ x12 = line, 1-based
   nlloop LBL,
      10 11 CMP,  C-CS nldone BCOND,
      13 10 0 LDRB,  10 10 1 ADDI,
      13 $A CMPI,  C-NE nlloop BCOND,
      12 12 1 ADDI,  nlloop B,
   nldone LBL,
   \ ":" + decimal line, built backwards in a 32-byte stack scratch.
   SP SP $20 SUBI,  14 SP $20 ADDI,
   9 $A MOVZ,
   digloop LBL,
      11 12 9 UDIV,  13 11 9 MUL,  13 12 13 SUB,
      13 13 $30 ADDI,  14 14 1 SUBI,  13 14 0 STRB,
      12 11 0 ADDI,  12 digloop CBNZ,
   13 $3A MOVZ,  14 14 1 SUBI,  13 14 0 STRB,
   2 SP $20 ADDI,  2 2 14 SUB,  1 14 0 ADDI,  0 2 MOVZ,  NR-WRITE SYS,
   SP SP $20 ADDI,
   noline LBL,
   0 2 MOVZ,  1 LPLACEEND LABEL@ ADR,  2 PLACEEND-LEN MOVZ,  NR-WRITE SYS,
   done LBL,
   RET, ;

\ In x0 = the exit or throw code, the diagnostic written without its newline.
: EM-COMPILE-DIE ( -- )
   LBL {: ldie:label :}
   LBL S" E-OVERLAY-OPEN habu: " {: oclass:label oca:ptr ocu:n :}
   LBL S" definition while a checker replay is open: " {: omsg:label oma:ptr omu:n :}
   LBL {: dpbad-end:label :}
   LCOMPILEDIE LABEL@ LBL,
   15 0 0 ADDI,                                             \ x15 = code (LEVALREC delivers it as the throw code)
   0 2 MOVZ,  1 LOPENNL LABEL@ ADR,  2 NL-LEN MOVZ,  NR-WRITE SYS,
   9 DATA EVALD-CELL LDR,  9 ldie CBZ,
      10 DATA EVALREC-CELL LDR,  10 BR,                     \ -> LEVALREC (escaped-frame unwind + deliver x15)
   ldie LBL,
   0 15 0 ADDI,
   NR-EXIT-GROUP SYS,
   EM-SOURCE-PLACE
   \ DP-CHECK hands the refused DP in x15. No definition is named: no DP sink
   \ runs inside a colon body (its string literals go to the code region).
   LDPBAD LABEL@ LBL,
   0 2 MOVZ,  1 LDPBADCLASS LABEL@ ADR,  2 DPBAD-CLASS-LEN MOVZ,  NR-WRITE SYS,
   LPLACE LABEL@ BL,
   0 2 MOVZ,  1 LDPBADMSG LABEL@ ADR,  2 DPBADMSG-LEN MOVZ,  NR-WRITE SYS,
   9 DATA BOOT:DP-FLOOR-CELL LDR,
   9 15 9 SUB,  LDIAGU LABEL@ BL,                           \ the refused DP
   0 2 MOVZ,  1 LDPBADOF LABEL@ ADR,  2 DPBAD-OF-LEN MOVZ,  NR-WRITE SYS,
   9 DATA BOOT:DP-FLOOR-CELL LDR,
   10 DATA BOOT:DP-CEILING-CELL LDR,  9 10 9 SUB,
   LDIAGU LABEL@ BL,                                        \ the ceiling DP-CHECK enforces
   0 2 MOVZ,  1 LDPBADUNIT LABEL@ ADR,  2 DPBAD-UNIT-LEN MOVZ,  NR-WRITE SYS,
   0 76 MOVZ,  LCOMPILEDIE LABEL@ B,
   dpbad-end LBL,
   s" (LDPBAD)" LDPBAD LABEL@ LABEL>N dpbad-end LABEL>N ENGINE-PRIMS:HELPER-REGISTER
   \ A writer of a record or a wid ran while a checker overlay was open,
   \ before it wrote anything. TKA/TKL hold its token: the definition's name,
   \ the package's, the EXPORT operand, or the word that ran `wordlist` or a
   \ writer primitive. The code is a literal so a pre-window host need not
   \ carry the constant.
   OVERLAY-EMIT:LDIE LABEL@ LBL,
   0 2 MOVZ,  1 oclass ADR,  2 ocu MOVZ,  NR-WRITE SYS,
   LPLACE LABEL@ BL,
   0 2 MOVZ,  1 omsg ADR,  2 omu MOVZ,  NR-WRITE SYS,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 108 MOVZ,  LCOMPILEDIE LABEL@ B,                     \ 108 = ENGINE-ERROR:OVERLAY-OPEN
   oclass LBL,  oca ocu BYTES,
   omsg LBL,  oma omu BYTES, ;

\ LDIAGU ( x9 = value ): unsigned decimal on fd 2, no newline. A diagnostic
\ never leaves through the output device, so a refusal stating a count calls
\ this, not `u.`/G-PRINTU9. Clobbers x0-x2, x9-x13, x16 (SYS,'s errno
\ reconciliation) and x30, plus 32 bytes of sp scratch; x14, x15 and x17 survive
\ for a caller's count.
: EMIT-DIAGU ( -- )
   LDIAGU LABEL@ LBL,
   LBL {: dl:label :}
   SP SP 32 SUBI,
   12 SP 32 ADDI,
   11 $A MOVZ,
   dl LBL,
      13 9 11 UDIV,  10 13 11 MUL,  10 9 10 SUB,
      10 10 $30 ADDI,  12 12 1 SUBI,  10 12 0 STRB,
      9 13 0 ADDI,  9 dl CBNZ,
   0 2 MOVZ,  1 12 0 ADDI,  2 SP 32 ADDI,  2 2 12 SUB,  NR-WRITE SYS,
   SP SP 32 ADDI,  RET, ;

\ x16 = the capacity BCAP-GROW tried, moved to x15 first since SYS, uses x16.
\ Nothing is stored before the grow fails, so the throw inside evaluate is safe.
: EM-BODY-GROW-DIE ( -- )
   LBGROWFAIL LABEL@ LBL,
   15 16 0 ADDI,
   0 2 MOVZ,  1 LBGROWCLASS LABEL@ ADR,  2 BGROW-CLASS-LEN MOVZ,  NR-WRITE SYS,
   LPLACE LABEL@ BL,
   0 2 MOVZ,  1 LBGROWMSG LABEL@ ADR,  2 BGROW-MSG-LEN MOVZ,  NR-WRITE SYS,
   9 15 0 ADDI,  LDIAGU LABEL@ BL,
   0 2 MOVZ,  1 LBGROWUNIT LABEL@ ADR,  2 BGROW-UNIT-LEN MOVZ,  NR-WRITE SYS,
   0 71 MOVZ,  LCOMPILEDIE LABEL@ B, ;

;package

package ENGINE-EMIT

\ The routines primitives and the Habu loop's helpers branch to.
: EMIT-ROUTINES ( -- )
   EM-RECOVER-EXITS
   EM-COMPILE-DIE
   EMIT-DIAGU
   EM-BODY-GROW-DIE
   EM-EVAL-THROW-RECOVER ;

;package

package ENGINE-EMIT
public
: EMIT-RESET-BUILDER ( -- )
   ASM-INIT  ENGINE-PRIMS:RESET ;
;package

package LABELS

: CORE ( -- )
   LBL LFIND !  LBL LNUM !  LBL LDICT !
   LBL LCEMIT !  LBL LTOK !  LBL LFLUSH !  LBL LFLUSHTO !  LBL LNCOUNT !
   LBL LDIVZERO !
   LBL LPROTWIDQ !  LBL LPROTWIDADD !
   LBL LBCS !
   LBL LHIDXADD !  LBL LHIDXBUILD !
   LBL HIDX-EMIT:LREBUILD !  LBL HIDX-EMIT:LFULL !  LBL HIDX-EMIT:LREKEY !
   LBL WLFIND:LENTRY !  LBL LHEADROW !
   LBL EM-DEF-OCC:LCOUNT !  LBL EM-DEF-OCC:LAPPEND !  LBL EM-DEF-OCC:LRESET !
   LBL EM-DEF-OCC:LROOM !  LBL EM-DEF-OCC:LISSUE !  LBL EM-DEF-OCC:LINIT !
   LBL EM-DEF-OCC:LBEFORE !  LBL EM-DEF-OCC:LAFTER !  LBL EM-DEF-OCC:LMATCH !
   LBL EM-DEF-OCC:LRECLAIM !  LBL EM-DEF-OCC:LRETIRE !
   LBL RELOC-EMIT:LMARK !  LBL RELOC-EMIT:LPTRMARK !  LBL RELOC-EMIT:LSCRATCHMARK !
   LBL RELOC-EMIT:LROLLBACK !
   LBL RELOC-EMIT:LADDRSITE ! ;

: CONTROL ( -- )
   LBL KWDATA:LKWTRUSTDECL !  LBL KWDATA:LKWTRUSTRAW !
   LBL LFINDUSED-CORE !  LBL LSCOPEREC !
   LBL DEFWRITE:LREPLAYTRAP ! ;

: RUNTIME ( -- )
   LBL LDOESPATCH !  LBL LGENIOOUT !
   LBL LEVALREC !  LBL LEXITHOOK !
   LBL LCRASHH !  LBL LHANDLERS-END !  LBL LFAULTREC !  LBL LSIGH !  LBL LHEX !  LBL LDIE-LINE !  LBL LHDR !  LBL LTRAPH !  LBL LBPH !  LBL BP-CALLER:LBPLH !  LBL LBPSH !  LBL LBPWH !
   LBL LOPENNL !
   LBL LUNCAUGHT !  LBL LUNCMSG !
   LBL LMINCLASS !  LBL LMINMSG !  LBL LFLOORREC !
   LBL LDPBAD !  LBL LDPBADCLASS !  LBL LDPBADMSG !  LBL LDPBADOF !  LBL LDPBADUNIT !
   LBL OVERLAY-EMIT:LDIE !
   LBL LDIAGU !
   LBL LBGROWFAIL !  LBL LBGROWCLASS !  LBL LBGROWMSG !  LBL LBGROWUNIT !
   LBL LCOMPILEDIE !  LBL LPLACE !
   LBL RELOC-EMIT:LXTMSG !
   LBL LPLACEEND !  ;

: PROF ( -- )
   LBL PROF:LPROFH !  LBL PROF:LPROFDUMP !  LBL PROF:LPROFFIND !  LBL PROF:LPROFEDGE !
   LBL PROF:LPROFJSON !  LBL PROF:LPROFNUM !  LBL PROF:LPROFPCT !
   LBL PROF:LPROFNAME !  LBL PROF:LPROFSYNC !
   LBL PROF:LPROFROW !
   LBL PROF-ABI:LPROFTRIM !  LBL PROF-ABI:LPROFRECLAIM ! ;

public

: INIT ( -- )
   CORE
   CONTROL
   RUNTIME
   PROF ;

;package

package ENGINE-EMIT

\ tok-imm? ( ptr u8 n -- n ): live-dictionary immediate probe for the checker.
\ Runs the interpret dispatch's LFIND on a token name and pushes flags&2, the
\ DNAME-IMM bit, so DO-TOK1 can reject a live immediate as a checked body step.
\ LFIND clobbers x3-x16 and preserves XDS (x19)/DATA (x20); the FPRIM frame
\ holds x30 across the BL.
: BTOKIMM ( -- )
   10 G-POP  9 G-POP                   \ x10 = len (TOS), x9 = addr: LFIND's token registers
   LFIND LABEL@ BL,                    \ x13 = found | imm<<1 | min-in byte | int flags
   9 13 2 ANDI,
   A G-PUSH ;
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
   LBL {: next:label :}
   LINKED-SCOPES @ 0<> if
      LBL dup  SCOPE-SITES k cells +  LABEL!  LBL,
      \ the 16-byte chain LINKER's ARM64:CODE-ADDRESS! writes, 0 until written
      5 0 MOVZ,  NOP,  NOP,  NOP,
      5 next CBZ,
      A 5 CMP,  C-NE next BCOND,
      A k MOVZ,  done B,
   then
   next LBL, ;

\ Entry kinds are fixed by the link's chains in code, never by source
\ declarations or writable DATA. A restored alias has the same
\ code entry and thus the same kind.
: BSCOPE-KIND ( -- )
   A G-POP
   LBL {: done:label :}
   6 done SCOPE-TEST
   7 done SCOPE-TEST
   8 done SCOPE-TEST
   5 done SCOPE-TEST
   3 done SCOPE-TEST
   4 done SCOPE-TEST
   2 done SCOPE-TEST
   A 0 MOVZ,
   done LBL,
   A G-PUSH ;

public

\ The next KERNEL emits scope-kind? to read each entry from a chain the link
\ writes at SCOPE-ENTRY-SITE after it places the product's code.
: LINK-SCOPE-ENTRIES ( -- ) -1 LINKED-SCOPES ! ;

\ The label of the chain holding kind k's entry, k from 2 to 8.
: SCOPE-ENTRY-SITE ( n -- label ) cells SCOPE-SITES + LABEL@ ;

;package

package ENGINE-EMIT
\ scope-find ( ptr u8 n -- ptr n ptr n ptr n n ): LSCOPEREC's four answers for a
\ token (bound record, used publics' record, second used record when two hold
\ the tail, flags), so the checker, tier 0 and the Habu interpreter bind a
\ spelling to the record a compiled call binds. A global row, checked like any
\ other.
: BSCOPEFIND ( -- )
   10 G-POP  9 G-POP                   \ x10 = len (TOS), x9 = addr
   LSCOPEREC LABEL@ BL,
   14 G-PUSH  16 G-PUSH  17 G-PUSH  15 G-PUSH ;

: EMIT-PRIMITIVE-SECTIONS ( -- )
   EMIT-PRIMS
   s" does-patch" ['] DOESPATCH:PRIM FPRIM
   s" does-record" ['] DOES-REC:NATIVE-PRIM FPRIM
   s" namespace-record" ['] DEFWRITE:NAMESPACE-RECORD ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" namespace-private" ['] DEFWRITE:NAMESPACE-PRIVATE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" alias-record" ['] DEFWRITE:ALIAS-RECORD ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" package-scope!" ['] DEFWRITE:PACKAGE-SCOPE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" def-open" ['] DEFWRITE:DEF-OPEN ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" body-capture" ['] DEFWRITE:BODY-APPEND ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" trust-sig!" ['] DEFWRITE:TRUST-SIG ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" created-sig!" ['] DEFWRITE:CREATED-SIG ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" def-close" ['] DEFWRITE:DEF-CLOSE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" def-create" ['] DEFWRITE:DEF-CREATE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" def-abort" ['] DEFWRITE:DEF-ABORT ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" stack-clear" ['] DEFWRITE:STACK-CLEAR ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" imm-mark" ['] C-IMMEDIATE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" def-cast" ['] DEFWRITE:DEF-CAST ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" def-defer" ['] DEFWRITE:DEF-DEFER ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" replay-open" ['] DEFWRITE:REPLAY-OPEN ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" replay-close" ['] DEFWRITE:REPLAY-CLOSE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" replay-widn!" ['] DEFWRITE:REPLAY-WIDN ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" replay-record" ['] DEFWRITE:REPLAY-RECORD ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" record-wid!" ['] DEFWRITE:RECORD-WID ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" replay-private" ['] DEFWRITE:REPLAY-PRIVATE ENGINE-PRIMS:GLOBAL-INT-WID FPRIM-WID
   s" DRAIN-PRETRUST" ['] BDRAINPRETRUST FPRIM
   s" tok-imm?" ['] BTOKIMM FPRIM
   s" scope-find" ['] BSCOPEFIND FPRIM
   s" scope-kind?" ['] BSCOPE-KIND FPRIM
   s" xt!" ['] RELOC-EMIT:BXTSTORE FPRIM
   s" ptr-cell-mark" ['] RELOC-EMIT:BPTRCELLMARK FPRIM
   s" scratch-cell-mark" ['] RELOC-EMIT:BSCRATCHCELLMARK FPRIM
   s" ptr-cell!" ['] RELOC-EMIT:BPTRCELLSTORE FPRIM
   s" snapshot-format" ['] RELOC-EMIT:BSNAPSHOTFORMAT FPRIM
   PROF:EMIT-PROF-PRIMS
   EMIT-FP-PRIMS
   ENGINE-PRIMS:COMPLETE
   EMIT-CEMIT
   EMIT-BCAP
   EMIT-TOK
   EMIT-DIV-ZERO
   EMIT-PROTWID
   EMIT-PROTWIDADD
   EMIT-FLUSH
   EMIT-FIND
   WLFIND:EMIT
   EMIT-HEAD-ROW
   EMIT-FIND-USED
   EMIT-SCOPE-REC
   DEFWRITE:EMIT-REPLAY-TRAP
   EMIT-HIDX
   EMIT-DEF-OCC
   EMIT-DEF-OCC-OVERLAP
   EMIT-DEF-OCC-RECLAIM
   EMIT-DEF-OCC-RETIRE
   EMIT-NUM
   EMIT-EXITHOOK ;

: EMIT-DICTIONARY-SECTIONS ( -- )
   DOESPATCH:EMIT
   RELOC-EMIT:EMIT-MARK
   RELOC-EMIT:EMIT-ROLLBACK
   RELOC-EMIT:EMIT-ADDR-SITE
   KWDATA:EMIT ;

\ [LCRASHH, LHANDLERS-END) holds the signal handlers and no code Habu code calls. Where an
\ emitter here runs into the next, that next is code a stripped program keeps anyway.
: EMIT-RUNTIME-SECTIONS ( -- )
   EMIT-GENIO-OUT
   PROF:EMIT-PROFFIND
   PROF:EMIT-PROFEDGE
   PROF:EMIT-PROF-REPORTS
   EMIT-FAULT-REC
   EMIT-CRASH-HANDLER
   EMIT-SIGNAL-HANDLER
   PROF:EMIT-PROF
   EMIT-TRAPH
   LHANDLERS-END LABEL@ LBL,
   EMIT-HEX
   EMIT-DIE-LINE ;

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

;using
;using
;using
;using
;using
;using
;using
