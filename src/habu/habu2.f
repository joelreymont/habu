\ habu2.f — engine-builder part 2: the JIT compiler
\ emitters (literal/call/keywords/locals/strings/do-loop), the outer-interpreter
\ main loop, and EMIT-FORTH. Needs habu1.f (part 1). EMIT-MAIN is split into
\ phase words sharing label VARIABLES (a giant single word would need dozens of
\ locals); emission order is stable so the self-rebuild reaches a fixpoint.
\ ---- compile-mode literal: emit movz/movk x9=val then the push stencil ----
: C-LIT ( -- )
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   9 W-PUSH0 LIT64,  LCEMIT @ BL,  9 W-PUSH1 LIT64,  LCEMIT @ BL, ;
\ compile-mode raw literal materialization: emit movz/movk x9=val.  `val` is in
\ the compiler's x11 at definition time; unlike C-LIT this does not push it.
: C-X9-LIT ( -- )
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 LIT64,  9 8 7 ORR,  LCEMIT @ BL, ;
\ ---- compile-mode CALL-or-INLINE (x11=target addr, x12=clen from FIND) ----
$28 constant INL-MAX
$D10043FF constant C-CALL-PROLOGUE-INSTR
$D65F03C0 constant C-CALL-RET-INSTR
$FC000000 constant C-CALL-B-IMM-MASK
$94000000 constant C-CALL-BL-IMM
$14000000 constant C-CALL-B-IMM
$FF000010 constant C-CALL-B-COND-MASK
$54000000 constant C-CALL-B-COND
$7E000000 constant C-CALL-CBZ-TBZ-MASK
$34000000 constant C-CALL-CBZ
$36000000 constant C-CALL-TBZ
$FFFFFC1F constant C-CALL-BR-MASK
$D63F0000 constant C-CALL-BLR
$D61F0000 constant C-CALL-BR
$1F000000 constant C-CALL-ADR-MASK
$10000000 constant C-CALL-ADR
$D2800010 constant C-CALL-MOVZ-X16
$F2A00010 constant C-CALL-MOVK-X16-16
$F2C00010 constant C-CALL-MOVK-X16-32
$D63F0200 constant C-CALL-BLR-X16

: C-CALL-BRANCH-NO-PROLOGUE ( n -- ) {: lnopro :}
   9 11 0 LDRW,  8 C-CALL-PROLOGUE-INSTR LIT64,
   9 8 CMP,  C-NE lnopro BCOND, ;

: C-CALL-PROLOGUE-SPAN ( n -- ) {: lcall :}
   12 INL-MAX 16 + CMPI,  C-GT lcall BCOND,
   13 11 8 ADDI,  14 11 12 ADD,  14 14 8 SUBI, ;

: C-CALL-REQUIRE-RET-SLOT ( n -- ) {: lcall :}
   9 14 0 LDRW,  8 C-CALL-RET-INSTR LIT64,
   9 8 CMP,  C-NE lcall BCOND, ;

: C-CALL-PLAIN-SPAN ( n -- ) {: lcall :}
   12 INL-MAX CMPI,  C-GT lcall BCOND,
   13 11 0 ADDI,  14 11 12 ADD,
   lcall C-CALL-REQUIRE-RET-SLOT ;   \ ret slot patched (does>) -> never inline

: C-CALL-REJECT-MASKED ( n n n -- ) {: mask op lcall :}
   8 mask LIT64,  10 9 8 AND,
   8 op LIT64,  10 8 CMP,  C-EQ lcall BCOND, ;

: C-CALL-REJECT-EXACT ( n n -- ) {: op lcall :}
   8 op LIT64,  9 8 CMP,  C-EQ lcall BCOND, ;

: C-CALL-REJECT-UNSAFE ( n -- ) {: lcall :}
   C-CALL-B-IMM-MASK C-CALL-BL-IMM lcall C-CALL-REJECT-MASKED
   C-CALL-B-IMM-MASK C-CALL-B-IMM lcall C-CALL-REJECT-MASKED
   C-CALL-B-COND-MASK C-CALL-B-COND lcall C-CALL-REJECT-MASKED
   C-CALL-CBZ-TBZ-MASK C-CALL-CBZ lcall C-CALL-REJECT-MASKED
   C-CALL-CBZ-TBZ-MASK C-CALL-TBZ lcall C-CALL-REJECT-MASKED
   C-CALL-BR-MASK C-CALL-BLR lcall C-CALL-REJECT-MASKED
   C-CALL-BR-MASK C-CALL-BR lcall C-CALL-REJECT-MASKED
   C-CALL-RET-INSTR lcall C-CALL-REJECT-EXACT
   C-CALL-ADR-MASK C-CALL-ADR lcall C-CALL-REJECT-MASKED ;

: C-CALL-SCAN-SAFE ( n n n -- ) {: lcopy lcall lsbody :}
   15 13 0 ADDI,
   lsbody LBL,  15 14 CMP,  C-GE lcopy BCOND,
      9 15 0 LDRW,  15 15 4 ADDI,
      lcall C-CALL-REJECT-UNSAFE
      lsbody B, ;

: C-CALL-COPY-INLINE ( n n -- ) {: linl ldone :}
   15 13 0 ADDI,
   linl LBL,  15 14 CMP,  C-GE ldone BCOND,
      9 15 0 LDRW,  15 15 4 ADDI,  LCEMIT @ BL,  linl B, ;

: C-CALL-EMIT-MOVZ-X16 ( -- )
   5 $FFFF MOVZ,
   7 11 5 AND,  7 7 5 LSLI,
   8 C-CALL-MOVZ-X16 LIT64,  9 8 7 ORR,  LCEMIT @ BL, ;

: C-CALL-EMIT-MOVK-X16 ( n n -- ) {: sh op :}
   7 11 sh LSRI,  7 7 5 AND,  7 7 5 LSLI,
   8 op LIT64,  9 8 7 ORR,  LCEMIT @ BL, ;

: C-CALL-EMIT-ABSOLUTE ( -- )
   C-CALL-EMIT-MOVZ-X16
   16 C-CALL-MOVK-X16-16 C-CALL-EMIT-MOVK-X16
   32 C-CALL-MOVK-X16-32 C-CALL-EMIT-MOVK-X16
   9 C-CALL-BLR-X16 LIT64,  LCEMIT @ BL, ;

: C-CALL ( -- )
   LBL LBL LBL LBL LBL LBL LBL {: lcall lcopy lscan lsbody lnopro linl ldone :}
   lnopro C-CALL-BRANCH-NO-PROLOGUE
      lcall C-CALL-PROLOGUE-SPAN
      lscan B,
   lnopro LBL,
      lcall C-CALL-PLAIN-SPAN
   lscan LBL,
      lcopy lcall lsbody C-CALL-SCAN-SAFE
   lcopy LBL,
      linl ldone C-CALL-COPY-INLINE
   lcall LBL,
      C-CALL-EMIT-ABSOLUTE
   ldone LBL, ;

\ ---- source setup: baked LSRC or stdin ----
variable LTRAPH   variable LBPH   variable LBPSH   variable LBPWH
variable LSRCRD   variable LSHBANG
variable LPLINUXTARGET  variable LPMACOSTARGET
variable LPUTIL         variable LPCHECKER      variable LPRENDER
variable LPHOOK         variable LPHABULAYOUT   variable LPLINUXENV     variable LPMACOSENV
variable LPROLES        variable LPCOMBINATORS
create BPH-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 58 c, 10 c,   \ habu-bp:\n
create BPS-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 45 c, 115 c, 116 c, 97 c, 99 c, 107 c, 58 c, 10 c,
create BPW-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 45 c, 119 c, 97 c, 116 c, 99 c, 104 c, 58 c, 10 c,
create ZBYTE 0 c,

: ZBYTES, ( ptr u8 n -- )
   BYTES, ZBYTE 1 BYTES, ;

: C-TRAP-MCTX>R9 ( -- )
   HB-TARGET-LINUX? IF 9 2 LINUX-UC-MCTX-OFF ADDI, exit THEN
   9 4 MCTX-OFF LDR, ;
s" c-trap-mctx>r9" s" --" TRUST

: C-MCTX-PC>R10 ( -- )
   HB-TARGET-LINUX? IF 10 9 LINUX-MCTX-PC-OFF LDR, exit THEN
   10 9 MACOS-MCTX-PC-OFF LDR, ;
s" c-mctx-pc>r10" s" --" TRUST

: C-MCTX-X19>R12 ( -- )
   HB-TARGET-LINUX? IF 12 9 LINUX-MCTX-X19-OFF LDR, exit THEN
   12 9 MACOS-MCTX-X19-OFF LDR, ;
s" c-mctx-x19>r12" s" --" TRUST

: C-MCTX-SP-16! ( -- )
   HB-TARGET-LINUX? IF
      12 9 LINUX-MCTX-SP-OFF LDR,  12 12 16 SUBI,  12 9 LINUX-MCTX-SP-OFF STR, exit
   THEN
   12 9 MACOS-MCTX-SP-OFF LDR,  12 12 16 SUBI,  12 9 MACOS-MCTX-SP-OFF STR, ;
s" c-mctx-sp-16!" s" --" TRUST

: C-MCTX-PC+4! ( -- )
   HB-TARGET-LINUX? IF
      12 9 LINUX-MCTX-PC-OFF LDR,  12 12 4 ADDI,  12 9 LINUX-MCTX-PC-OFF STR, exit
   THEN
   12 9 MACOS-MCTX-PC-OFF LDR,  12 12 4 ADDI,  12 9 MACOS-MCTX-PC-OFF STR, ;
s" c-mctx-pc+4!" s" --" TRUST

: C-BP-HIT-SAVE ( -- )
   SP SP 80 SUBI,
   1 SP 0 STR,  4 SP 8 STR,  5 SP 16 STR,
   9 SP 24 STR,  10 SP 32 STR,  8 SP 40 STR,
   14 8 16 LDR,  14 14 1 ADDI,  14 8 16 STR,
   15 8 24 LDR,  12 15 1 LSRI, ;
s" c-bp-hit-save" s" --" TRUST

: C-BP-PRINT-HIT ( -- )
   1 LBPH @ ADR,  0 2 MOVZ,  2 9 MOVZ,  NR-WRITE SYS,
   9 SP 32 LDR,  LHEX @ BL,
   9 SP 24 LDR,  C-MCTX-X19>R12
   9 12 8 SUBI,  9 9 0 LDR,  LHEX @ BL,
   1 LBPSH @ ADR,  0 2 MOVZ,  2 15 MOVZ,  NR-WRITE SYS, ;
s" c-bp-print-hit" s" --" TRUST

: C-BP-STACK-RANGE ( -- )
   17 DATA S0-CELL LDR,
   9 SP 24 LDR,  C-MCTX-X19>R12
   18 12 0 ADDI, ;
s" c-bp-stack-range" s" --" TRUST

: C-BP-WATCH-HEAD ( -- )
   1 LBPWH @ ADR,  0 2 MOVZ,  2 15 MOVZ,  NR-WRITE SYS,
   6 DATA BPWN-CELL LDR,  7 DATA BPWBASE-CELL LDR,
   17 0 MOVZ, ;
s" c-bp-watch-head" s" --" TRUST

: C-BP-WATCH-ROW ( -- )
   22 17 3 LSLI,  22 7 22 ADD,  23 22 0 LDR,
   9 23 0 ADDI,  LHEX @ BL,
   9 23 0 LDR,  LHEX @ BL,
   17 17 1 ADDI, ;
s" c-bp-watch-row" s" --" TRUST

: C-BP-RESTORE-ONESHOT ( -- )
   2 3 MOVZ,  LPROT @ BL,
   8 SP 40 LDR,  11 8 0 LDR,  12 8 8 LDR,  12 11 0 STRW,
   2 5 MOVZ,  LPROT @ BL,
   9 11 0 ADDI,  LFLUSH @ BL,
   8 SP 40 LDR,  12 0 MOVZ,  12 8 0 STR, ;
s" c-bp-restore-oneshot" s" --" TRUST

: C-BP-EMULATE ( -- )
   9 SP 24 LDR,
   C-MCTX-SP-16!
   C-MCTX-PC+4! ;
s" c-bp-emulate" s" --" TRUST

: C-BP-SCAN ( n n n n -- )
   {: tno bscan bnext bhit :}
   6 8 MOVZ,  7 0 MOVZ,
   bscan LBL,
      7 6 CMP,  C-GE tno BCOND,
      8 7 5 LSLI,  14 BPTAB-OFF LIT64,  8 8 14 ADD,  8 DATA 8 ADD,
      13 8 0 LDR,  13 bnext CBZ,
      10 13 CMP,  C-EQ bhit BCOND,
      bnext LBL,  7 7 1 ADDI,  bscan B, ;
s" c-bp-scan" s" n n n n --" TRUST

: C-BP-STACK-DUMP ( n n -- )
   {: sdump sdone :}
   sdump LBL,
      17 18 CMP,  C-GE sdone BCOND,
      9 17 0 LDR,  17 SP 48 STR,  18 SP 56 STR,  LHEX @ BL,
      17 SP 48 LDR,  18 SP 56 LDR,  17 17 8 ADDI,  sdump B,
   sdone LBL, ;
s" c-bp-stack-dump" s" n n --" TRUST

: C-BP-WATCH-DUMP ( n n -- )
   {: wloop wdone :}
   6 DATA BPWN-CELL LDR,  6 wdone CBZ,
   7 DATA BPWBASE-CELL LDR,  7 wdone CBZ,
   C-BP-WATCH-HEAD
   wloop LBL,
      17 6 CMP,  C-GE wdone BCOND,
      C-BP-WATCH-ROW  wloop B,
   wdone LBL, ;
s" c-bp-watch-dump" s" n n --" TRUST

\ LTRAPH: target signal entry. A one-shot
\ breakpoint at [BPA-CELL]: print habu-bp, pc, data-stack, and watch cells;
\ restore the original instruction, clear the bp, sigreturn to re-execute the word.
\ Any other trap falls through to the crash dump (x2/x4 untouched).
: EMIT-TRAPH ( -- )
   LTRAPH @ LBL,
   LBL {: tno :}
   C-TRAP-MCTX>R9                                    \ x9 = mcontext
   C-MCTX-PC>R10                                     \ x10 = pc
   LBL {: bscan :}  LBL {: bnext :}  LBL {: bhit :}
   LBL {: sdump :}  LBL {: sdone :}  LBL {: wloop :}  LBL {: wdone :}
   LBL {: emu :}  LBL {: fin :}
   tno bscan bnext bhit C-BP-SCAN                    \ scan BPTAB[0..8)
   \ slot layout: +0 addr  +8 saved-instr  +16 hits  +24 ctrl(skip<<1 | persist)
   bhit LBL,                                         \ x8=&slot x9=mctx x10=pc
   C-BP-HIT-SAVE                                     \ x15=ctrl  x12=skip
   14 12 CMP,  C-LS emu BCOND,                       \ hits <= skip -> silent, just emulate
   C-BP-PRINT-HIT
   C-BP-STACK-RANGE                                  \ x17=base x18=x19
   sdump sdone C-BP-STACK-DUMP
   wloop wdone C-BP-WATCH-DUMP
   8 SP 40 LDR,  15 8 24 LDR,  15 15 1 ANDI,  15 emu CBNZ,   \ persistent -> emulate, keep BRK
   C-BP-RESTORE-ONESHOT                              \ clear slot addr; resume re-runs orig
   fin B,
   emu LBL,                                          \ emulate the entry prologue, keep BRK:
   C-BP-EMULATE
   fin LBL,
   0 SP 8 LDR,  1 SP 0 LDR,  2 SP 16 LDR,  SP SP 80 ADDI,
   NR-SIGRETURN SYS,                                 \ sigreturn(uctx, infostyle, token)
   tno LBL,
   LCRASHH @ B,
   LBPH @ LBL,  BPH-KW 9 BYTES,
   LBPSH @ LBL, BPS-KW 15 BYTES,
   LBPWH @ LBL, BPW-KW 15 BYTES, ;

\ override SIGTRAP(5) to the resuming handler (G-INSTALL-CRASH pointed all four
\ at the dumper; this repoints just TRAP once LTRAPH is bound).
: G-INSTALL-TRAP ( -- )
   9 LTRAPH @ ADR,  9 C-SIGACTION-FRAME
   5 INSTALL-SIGACT
   C-SIGACTION-FRAME-DONE ;

: EMIT-SHEBANG-COMMENT ( -- )
   LSHBANG @ LBL,
   LBL {: done :}
   4 9 17 SUB,  4 2 CMPI,  C-LT done BCOND,
   4 17 0 LDRB,  4 $23 CMPI,  C-NE done BCOND,
   4 17 1 LDRB,  4 $21 CMPI,  C-NE done BCOND,
   4 92 MOVZ,  4 17 0 STRB,
   4 32 MOVZ,  4 17 1 STRB,
   done LBL,
   RET, ;

: EMIT-SOURCE-READ ( -- )
   LSRCRD @ LBL,
   LBL LBL LBL LBL {: srl sdone sreaderr sopenerr :}
   12 OS-OPEN-RD
   13 C-CS CSET,  13 sopenerr CBNZ,
   12 0 0 ADDI,
   17 9 0 ADDI,
   srl LBL,
      0 12 0 ADDI,  1 9 0 ADDI,
      2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  2 2 9 SUB,
      2 sreaderr CBZ,
      NR-READ SYS,
      13 C-CS CSET,  13 sreaderr CBNZ,
      0 sdone CBZ,
      9 9 0 ADD,  srl B,
   sdone LBL,
   0 12 0 ADDI,  NR-CLOSE SYS,
   SP SP 16 SUBI,  30 SP 0 STR,
   LSHBANG @ BL,
   30 SP 0 LDR,  SP SP 16 ADDI,
   RET,
   sreaderr LBL,  0 12 0 ADDI,  NR-CLOSE SYS,
   sopenerr LBL,
   0 74 MOVZ,  NR-EXIT SYS, ;

: C-TARGET-UNKNOWN ( -- )
   s" hb: unknown target" 76 die ;

0 constant PFX-COMMON
1 constant PFX-LINUX
2 constant PFX-MACOS

: PFX-TARGET-OK ( -- )
   HB-TARGET-LINUX? if exit then
   HB-TARGET-MACOS? if exit then
   C-TARGET-UNKNOWN ;

: PFX-LOAD? ( n -- n )
   dup PFX-COMMON = if drop -1 exit then
   dup PFX-LINUX = if drop HB-TARGET-LINUX? if -1 else 0 then exit then
   PFX-MACOS = if HB-TARGET-MACOS? if -1 else 0 then else 0 then ;

: PFX-ROW ( xt n n ptr u8 n -- ) {: xt kind var a u :}
   kind var a u xt execute ;

: PFX-FILES ( xt -- ) {: xt :}
   xt PFX-COMMON LPUTIL         s" src/core/util.f"        PFX-ROW
   xt PFX-LINUX  LPLINUXTARGET  s" src/os/linux/target.f"  PFX-ROW
   xt PFX-MACOS  LPMACOSTARGET  s" src/os/macos/target.f"  PFX-ROW
   xt PFX-COMMON LPCHECKER      s" src/core/checker.f"     PFX-ROW
   xt PFX-COMMON LPRENDER       s" src/core/render.f"      PFX-ROW
   xt PFX-COMMON LPHABULAYOUT   s" src/habu/layout.f"      PFX-ROW
   xt PFX-LINUX  LPLINUXENV     s" src/os/linux/env.f"     PFX-ROW
   xt PFX-MACOS  LPMACOSENV     s" src/os/macos/env.f"     PFX-ROW
   xt PFX-COMMON LPHOOK         s" src/core/check-hook.f"  PFX-ROW
   xt PFX-COMMON LPROLES        s" src/core/roles.f"       PFX-ROW
   xt PFX-COMMON LPCOMBINATORS  s" src/core/combinators.f" PFX-ROW ;

: PFX-LOAD-ROW ( n n ptr u8 n -- ) {: kind var a u :}
   kind PFX-LOAD? if 12 var @ ADR,  LSRCRD @ BL, then ;

: PFX-PATH-ROW ( n n ptr u8 n -- ) {: kind var a u :}
   var @ LBL,  a u ZBYTES, ;

: EMIT-HOST-LOAD-PREFIX ( -- )
   16 0 MOVZ,  16 DATA HOOK-CELL STR,
   PFX-TARGET-OK
   ['] PFX-LOAD-ROW PFX-FILES ;

: EMIT-COLD-PREFIX ( -- )
   LBL {: done :}
   12 DATA SNAP-CELL LDR,
   12 done CBNZ,
   EMIT-HOST-LOAD-PREFIX
   done LBL, ;

: C-EMIT-TTY-PROBE ( -- )
   0 0 MOVZ,
   HB-TARGET-LINUX? if 1 $5401 LIT64, else
      HB-TARGET-MACOS? if 1 $40487413 LIT64, else C-TARGET-UNKNOWN then
   then
   2 DATA BODYBUF-OFF ADDI,
   NR-IOCTL SYS, ;
s" c-emit-tty-probe" s" --" TRUST

variable SRC-TTY  variable SRC-FILE  variable SRC-SFAIL
variable SRC-RL   variable SRC-RD    variable SRC-PIPEOK
variable SRC-REPL variable SRC-DONE  variable SRC-FSCAN
variable SRC-FNEXT variable SRC-FREADY variable SRC-FPLAIN
variable SRC-FLOOP variable SRC-SHLOOP variable SRC-STDINPROG
variable SRC-BLOOP variable SRC-BDONE  variable SRC-BFAIL

: C-SOURCE-LABELS ( -- )
   LBL SRC-TTY !   LBL SRC-FILE !  LBL SRC-SFAIL !
   LBL SRC-RL !    LBL SRC-RD !    LBL SRC-PIPEOK !
   LBL SRC-REPL !  LBL SRC-DONE !  LBL SRC-FSCAN !
   LBL SRC-FNEXT ! LBL SRC-FREADY ! LBL SRC-FPLAIN !
   LBL SRC-FLOOP ! LBL SRC-SHLOOP ! LBL SRC-STDINPROG !
   LBL SRC-BLOOP ! LBL SRC-BDONE ! LBL SRC-BFAIL ! ;

: C-SOURCE-MMAP ( n -- ) {: fail :}
   0 0 MOVZ,  1 IBUFSZ LIT64,  2 3 MOVZ,
   3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   13 C-CS CSET,  13 fail CBNZ, ;

: C-ARG--LOAD? ( n -- ) {: notload :}
   4 12 0 LDRB,  4 $2D CMPI,  C-NE notload BCOND,
   4 12 1 LDRB,  4 $2D CMPI,  C-NE notload BCOND,
   4 12 2 LDRB,  4 108 CMPI,  C-NE notload BCOND,
   4 12 3 LDRB,  4 111 CMPI,  C-NE notload BCOND,
   4 12 4 LDRB,  4 97 CMPI,   C-NE notload BCOND,
   4 12 5 LDRB,  4 100 CMPI,  C-NE notload BCOND,
   4 12 6 LDRB,  4 0 CMPI,    C-NE notload BCOND, ;

: C-ARG-SEP? ( n -- ) {: notsep :}
   4 12 0 LDRB,  4 $2D CMPI,  C-NE notsep BCOND,
   4 12 1 LDRB,  4 $2D CMPI,  C-NE notsep BCOND,
   4 12 2 LDRB,  4 0 CMPI,    C-NE notsep BCOND, ;

: C-SOURCE-SKIP-SHEBANG ( -- )
   12 9 11 SUB,  12 2 CMPI,  C-LT SRC-DONE @ BCOND,
   4 11 0 LDRB,  4 $23 CMPI,  C-NE SRC-DONE @ BCOND,
   4 11 1 LDRB,  4 $21 CMPI,  C-NE SRC-DONE @ BCOND,
   11 11 2 ADDI,
   SRC-SHLOOP @ LBL,
      11 9 CMP,  C-GE SRC-DONE @ BCOND,
      4 11 0 LDRB,  11 11 1 ADDI,
      11 DATA INP-CELL STR,
      4 10 CMPI,  C-EQ SRC-DONE @ BCOND,
      SRC-SHLOOP @ B, ;

: C-SOURCE-PIPE ( -- )
   SRC-STDINPROG @ LBL,
   SRC-SFAIL @ C-SOURCE-MMAP
   11 0 0 ADDI,  9 0 0 ADDI,
   EMIT-COLD-PREFIX
   17 9 0 ADDI,
   SRC-RL @ LBL,
      0 0 MOVZ,  1 9 0 ADDI,
      2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  2 2 9 SUB,
      2 SRC-SFAIL @ CBZ,
      NR-READ SYS,
      13 C-CS CSET,  13 SRC-SFAIL @ CBNZ,
      0 SRC-RD @ CBZ,
      9 9 0 ADD,  SRC-RL @ B,
   SRC-RD @ LBL,
   LSHBANG @ BL,
   9 17 CMP,  C-NE SRC-PIPEOK @ BCOND,
   10 DATA ARGC-CELL LDR,  10 1 CMPI,  C-GT SRC-FILE @ BCOND,
   SRC-PIPEOK @ LBL,
   11 DATA INP-CELL STR,  9 DATA INE-CELL STR,
   C-SOURCE-SKIP-SHEBANG ;

: C-SOURCE-FIND-SEP ( -- )
   SRC-FSCAN @ LBL,
      13 10 CMP,  C-GE SRC-FREADY @ BCOND,
      12 DATA ARGV-CELL LDR,  5 13 3 LSLI,  12 12 5 ADD,  12 12 0 LDR,
      SRC-FNEXT @ C-ARG-SEP?
      15 13 0 ADDI,  SRC-FREADY @ B,
   SRC-FNEXT @ LBL,  13 13 1 ADDI,  SRC-FSCAN @ B, ;

: C-SOURCE-ARGV1 ( -- )
   12 DATA ARGV-CELL LDR,  12 12 8 LDR, ;

: C-SOURCE-FILE-MAP ( -- )
   SRC-SFAIL @ C-SOURCE-MMAP
   11 0 0 ADDI, ;

: C-SOURCE-FILE-INIT ( -- )
   9 11 0 ADDI,
   10 DATA ARGC-CELL LDR,
   14 1 MOVZ,  15 2 MOVZ,
   C-SOURCE-ARGV1 ;

: C-SOURCE-FILE-PREFIX ( -- )
   SRC-FPLAIN @ C-ARG--LOAD?
   14 2 MOVZ,  15 10 0 ADDI,  13 2 MOVZ,
   EMIT-COLD-PREFIX
   C-SOURCE-FIND-SEP
   SRC-FPLAIN @ LBL,
   EMIT-COLD-PREFIX
   SRC-FREADY @ LBL, ;

: C-SOURCE-APPEND-ARG ( -- )
   12 DATA ARGV-CELL LDR,  5 14 3 LSLI,
   12 12 5 ADD,  12 12 0 LDR,
   LSRCRD @ BL,
   14 14 1 ADDI, ;

: C-SOURCE-APPEND-LF ( -- )
   2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,
   9 2 CMP,  C-GE SRC-SFAIL @ BCOND,
   5 10 MOVZ,  5 9 0 STRB,  9 9 1 ADDI, ;

: C-SOURCE-FILE-LOOP ( -- )
   SRC-FLOOP @ LBL,
      14 15 CMP,  C-GE SRC-PIPEOK @ BCOND,
      C-SOURCE-APPEND-ARG
      14 15 CMP,  C-GE SRC-PIPEOK @ BCOND,
      C-SOURCE-APPEND-LF
      SRC-FLOOP @ B, ;

: C-SOURCE-FAIL-REPL-DONE ( -- )
   SRC-SFAIL @ LBL,  0 74 MOVZ,  NR-EXIT SYS,
   SRC-REPL @ LBL,
   11 LSRC @ ADR,  11 DATA INP-CELL STR,
   5 SRCN @ LIT64,  11 11 5 ADD,  11 DATA INE-CELL STR,
   SRC-DONE @ B,
   SRC-DONE @ LBL, ;

: C-SOURCE-FILE-LIST ( -- )
   9 DATA ARGC-CELL LDR,  9 1 CMPI,  C-LE SRC-REPL @ BCOND,
   C-SOURCE-FILE-MAP
   SRC-FILE @ LBL,
   C-SOURCE-FILE-INIT
   C-SOURCE-FILE-PREFIX
   14 15 CMP,  C-GE SRC-SFAIL @ BCOND,
   C-SOURCE-FILE-LOOP
   C-SOURCE-FAIL-REPL-DONE ;

: C-SOURCE-STDIN ( -- )
   C-EMIT-TTY-PROBE
   0 SRC-TTY @ CBZ,
   10 DATA ARGC-CELL LDR,  10 1 CMPI,  C-LE SRC-STDINPROG @ BCOND,
   C-SOURCE-ARGV1
   SRC-STDINPROG @ C-ARG--LOAD?
   SRC-TTY @ B,
   C-SOURCE-PIPE
   SRC-TTY @ LBL,
   C-SOURCE-FILE-LIST ;

: C-SOURCE-BAKED ( -- )
   SRC-BFAIL @ C-SOURCE-MMAP
   11 0 0 ADDI,  9 0 0 ADDI,
   EMIT-COLD-PREFIX
   17 9 0 ADDI,
   12 LSRC @ ADR,  5 SRCN @ LIT64,  13 12 5 ADD,
   SRC-BLOOP @ LBL,
      12 13 CMP,  C-GE SRC-BDONE @ BCOND,
      2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  9 2 CMP,  C-GE SRC-BFAIL @ BCOND,
      4 12 0 LDRB,  4 9 0 STRB,
      12 12 1 ADDI,  9 9 1 ADDI,
      SRC-BLOOP @ B,
   SRC-BDONE @ LBL,
   LSHBANG @ BL,
   11 DATA INP-CELL STR,  9 DATA INE-CELL STR,  SRC-DONE @ B,
   SRC-BFAIL @ LBL,  0 74 MOVZ,  NR-EXIT SYS,
   SRC-DONE @ LBL, ;

: EMIT-SOURCE ( -- )
   C-SOURCE-LABELS
   STDIN? @ IF C-SOURCE-STDIN ELSE C-SOURCE-BAKED THEN ;

\ ---- control-flow JIT helpers ----
: EMIT-CF-HELPERS ( -- )
   LBL LBL LBL LBL LBL LBL {: pisb pdone kno kyes kchk knf :}
   LCFPUSH @ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,
      12 11 3 LSLI,  12 12 10 ADD,  12 12 8 ADDI,  9 12 0 STR,
      11 11 1 ADDI,  11 10 0 STR,  RET,
   LCFPOP @ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,  11 11 1 SUBI,  11 10 0 STR,
      12 11 3 LSLI,  12 12 10 ADD,  12 12 8 ADDI,  9 12 0 LDR,  RET,
   LPAT @ LBL,
      11 9 0 LDRW,  10 CP 9 SUB,  10 10 2 ASRI,
      5 $80000000 LIT64,  13 11 5 AND,
      13 pisb CBZ,
         5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,  pdone B,
      pisb LBL,  5 $3FFFFFF LIT64,  10 10 5 AND,
      pdone LBL,  11 11 10 ORR,  11 9 0 STRW,  RET,
   LKWCMP @ LBL,
      2 DATA TKL-CELL LDR,  2 1 CMP,  C-NE kno BCOND,
      2 0 MOVZ,  3 $20 MOVZ,
      kchk LBL,
         2 1 CMP,  C-GE kyes BCOND,
         4 DATA TKA-CELL LDR,  4 4 2 ADD,  4 4 0 LDRB,
         4 $41 CMPI,  C-LT knf BCOND,  4 $5A CMPI,  C-GT knf BCOND,  4 4 3 ORR,
         knf LBL,
         5 0 2 ADD,    5 5 0 LDRB,
         4 5 CMP,  C-NE kno BCOND,
         2 2 1 ADDI,  kchk B,
      kyes LBL,  0 1 MOVZ,  RET,
      kno  LBL,  0 0 MOVZ,  RET,
   LBCHAIN @ LBL,                                    \ patch a B-placeholder chain:
      LBL LBL {: bcl bcd :}                    \ x9=head offset, x14=target;
      bcl LBL,  9 bcd CBZ,                           \ clobbers x5,x10-x12
         10 DBASE 9 ADD,  11 10 0 LDRW,
         12 14 10 SUB,  12 12 2 ASRI,
         5 $3FFFFFF LIT64,  12 12 5 AND,
         5 $14000000 LIT64,  12 12 5 ORR,
         12 10 0 STRW,
         9 11 0 ADDI,  bcl B,
      bcd LBL,  RET, ;

: EMIT-LOC-FIND ( -- )
   LBL LBL LBL LBL LBL {: ll lmiss lhit lcmp lnext :}
   LLOC-FIND @ LBL,
   9 DATA LOCN-CELL LDR,  10 0 MOVZ,
   6 DATA TKL-CELL LDR,  7 DATA TKA-CELL LDR,
   ll LBL,  10 9 CMP,  C-GE lmiss BCOND,
      12 LOC-REC MOVZ,  11 10 12 MUL,  5 LOCNAMES LIT64,  11 11 5 ADD,  11 DATA 11 ADD,
      12 11 0 LDR,  12 6 CMP,  C-NE lnext BCOND,
      13 0 MOVZ,
      lcmp LBL,  13 6 CMP,  C-GE lhit BCOND,
         14 11 13 ADD,  14 14 8 ADDI,  14 14 0 LDRB,
         15 7 13 ADD,  15 15 0 LDRB,
         14 15 CMP,  C-NE lnext BCOND,
         13 13 1 ADDI,  lcmp B,
      lhit LBL,  0 10 0 ADDI,  RET,
      lnext LBL,  10 10 1 ADDI,  ll B,
   lmiss LBL,  0 0 MOVN,  RET, ;
\ keyword bytes (lower-case / literal) at known labels
create SQ-KW  115 c, 34 c,
create CQ-KW  99 c, 34 c,
create DOTQ-KW 46 c, 34 c,
create BCHAR-KW 91 c, 99 c, 104 c, 97 c, 114 c, 93 c,   \ [char]
create QUOT-KW 91 c, 58 c,      \ [:
create SEMIQ-KW 59 c, 93 c,     \ ;]
variable LREAD  variable LRBYE  variable LRDIE  variable LRREC  variable LQNL  variable LOKS
create QNL-KW 63 c, 10 c,
create OKS-KW 32 c, 111 c, 107 c, 10 c,
create TICK-KW   39 c,
create BTICK-KW  91 c, 39 c, 93 c,
create LBRACE-KW 123 c, 58 c,
create ENDLOC-KW 58 c, 125 c,

: EMIT-KWDATA ( -- )
   LKWIF @ LBL,     s" if"     BYTES,    LKWTHEN @ LBL,   s" then"   BYTES,
   LKWELSE @ LBL,   s" else"   BYTES,    LKWBEGIN @ LBL,  s" begin"  BYTES,
   LKWUNTIL @ LBL,  s" until"  BYTES,    LKWAGAIN @ LBL,  s" again"  BYTES,
   LKWWHILE @ LBL,  s" while"  BYTES,    LKWREPEAT @ LBL, s" repeat" BYTES,
   LKWCREATE @ LBL, s" create" BYTES,    LKWVAR @ LBL,    s" variable" BYTES,
   LKWSQ @ LBL,     SQ-KW 2 BYTES,
   LKWCQ @ LBL,     CQ-KW 2 BYTES,
   LKWDOTQ @ LBL,   DOTQ-KW 2 BYTES,
   LKWTYPE @ LBL,   s" type" BYTES,
   LKWTICK @ LBL,   TICK-KW 1 BYTES,    LKWBTICK @ LBL,  BTICK-KW 3 BYTES,
   LKWLBRACE @ LBL, LBRACE-KW 2 BYTES,  LKWENDLOC @ LBL, ENDLOC-KW 2 BYTES,
   LKWCONST @ LBL,  s" constant" BYTES,
   LQNL @ LBL,  QNL-KW 2 BYTES,   LOKS @ LBL,  OKS-KW 4 BYTES,
   LKWDO @ LBL,  s" do" BYTES,    LKWLOOP @ LBL,  s" loop" BYTES,    LKWI @ LBL,  s" i" BYTES,
   LKWTOR @ LBL,  s" >r" BYTES,   LKWRFROM @ LBL,  s" r>" BYTES,   LKWRFET @ LBL,  s" r@" BYTES,
   LKWEXIT @ LBL,  s" exit" BYTES,   LKWREC @ LBL,  s" recurse" BYTES,
   LKWQDO @ LBL,  s" ?do" BYTES,   LKWPLOOP @ LBL,  s" +loop" BYTES,   LKWJ @ LBL,  s" j" BYTES,
   LKWLEAVE @ LBL,  s" leave" BYTES,   LKWUNLOOP @ LBL,  s" unloop" BYTES,
   LKWCHAR @ LBL,  s" char" BYTES,   LKWBCHAR @ LBL,  BCHAR-KW 6 BYTES,
   LKWIMM @ LBL,  s" immediate" BYTES,   LKWPOST @ LBL,  s" postpone" BYTES,
   LKWCOMPC @ LBL,  s" compile," BYTES,
   LKWDOES @ LBL,  s" does>" BYTES,
   LKWTRUSTED @ LBL, s" trusted:" BYTES,
   LKWTRUST @ LBL, s" trust" BYTES,      LKWCHKDOES @ LBL, s" check-does!" BYTES,
   LKWQUOT @ LBL,  QUOT-KW 2 BYTES,   LKWSEMIQ @ LBL,  SEMIQ-KW 2 BYTES,
   ['] PFX-PATH-ROW PFX-FILES ;

\ ---- compile-time keyword handlers (append JIT-emitter code at BUILD time) ----
: C-EMITW ( n -- ) {: w :}  9 w LIT64,  LCEMIT @ BL, ;

: C-POPFLAG ( -- )  $D1002273 C-EMITW  $F9400269 C-EMITW ;

: C-PUSHCP ( -- )   9 CP 0 ADDI,  LCFPUSH @ BL, ;

: C-BBACK ( n n -- ) {: opc mask :}
   10 9 CP SUB,  10 10 2 ASRI,  5 mask LIT64,  10 10 5 AND,  9 opc LIT64,  9 9 10 ORR,  LCEMIT @ BL, ;

: J-IF ( -- )    C-POPFLAG  C-PUSHCP  $B4000009 C-EMITW ;

: J-THEN ( -- )  LCFPOP @ BL,  LPAT @ BL, ;

: J-ELSE ( -- )  LCFPOP @ BL,  14 9 0 ADDI,  C-PUSHCP  $14000000 C-EMITW  9 14 0 ADDI,  LPAT @ BL, ;

\ BEGIN loops are register-resident: J-BEGIN snapshots the VS into registers
\ (Lvsnap), the back edges reconcile to that snapshot (Lvrecon) and branch on
\ x17 — never a VS register, so the reconcile reload can't clobber the flag.
: J-BEGIN ( -- )  LVSNAP @ BL,  C-PUSHCP ;

: J-AGAIN ( -- )  LVRECON @ BL,  LCFPOP @ BL,  $14000000 $3FFFFFF C-BBACK ;

: J-UNTILX ( -- )                                 \ shared tail: reconcile + cbz x17,top
   LVRECON @ BL,
   LCFPOP @ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $B4000011 LIT64,  9 9 10 ORR,  LCEMIT @ BL, ;

: J-UNTIL ( -- )  $D1002273 C-EMITW  $F9400271 C-EMITW  J-UNTILX ;   \ pop flag -> x17

: J-WHILE ( -- ) C-POPFLAG  C-PUSHCP  $B4000009 C-EMITW ;

: J-REPEAT ( -- ) LVRECON @ BL,  LCFPOP @ BL,  14 9 0 ADDI,  LCFPOP @ BL,  $14000000 $3FFFFFF C-BBACK
   12 0 MOVZ,  12 DATA VSP-CELL STR,                  \ exit path arrives from
   12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,           \ WHILE's spilled state
   12 FRALL MOVZ,  12 DATA FRFREE-CELL STR,
   9 14 0 ADDI,  LPAT @ BL, ;

: J-FRAME ( -- )                                \ pop limit/start, push a loop frame
   3506446963 C-EMITW  4181721705 C-EMITW  3506446963 C-EMITW  4181721706 C-EMITW
   4181780107 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4177527177 C-EMITW  4177528202 C-EMITW  2432697707 C-EMITW  4177585803 C-EMITW ;

: J-LVOPEN ( -- )                               \ open a LEAVE-chain level: LVH[LVD]=0, LVD++
   9 DATA LVD-CELL LDR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   12 0 MOVZ,  12 10 0 STR,
   9 9 1 ADDI,  9 DATA LVD-CELL STR, ;

: J-LVLEAVE ( -- )                              \ chain a B placeholder on the current level
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   9 10 0 LDR,
   11 CP DBASE SUB,  11 10 0 STR,
   LCEMIT @ BL, ;

: J-DO ( -- )
   J-FRAME  J-LVOPEN  C-PUSHCP ;

: J-?DO ( -- )                                  \ DO, but skip the loop when limit = start
   J-FRAME  J-LVOPEN
   $EB0A013F C-EMITW                     \ cmp x9,x10  (start/limit still live)
   $54000041 C-EMITW                     \ b.ne +8 (over the skip placeholder)
   J-LVLEAVE
   C-PUSHCP ;

: J-LEAVE ( -- )  J-LVLEAVE ;

: J-UNLOOP ( -- )                               \ pop one loop frame, no branch
   4181780107 C-EMITW  3506439531 C-EMITW  4177585803 C-EMITW ;

: J-LOOPEND ( -- )                              \ shared LOOP/+LOOP tail: pop frame, patch
   14 CP 0 ADDI,                         \ LEAVE/?DO skips to the pop point, LVD--
   4181780107 C-EMITW  3506439531 C-EMITW  4177585803 C-EMITW
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,  9 DATA LVD-CELL STR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,  9 10 0 LDR,
   LBCHAIN @ BL, ;

: J-LOOP ( -- )
   4181780107 C-EMITW  3506439531 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4181722506 C-EMITW  2432697641 C-EMITW  4177527177 C-EMITW  3943301439 C-EMITW
   LCFPOP @ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000B LIT64,  9 9 10 ORR,  LCEMIT @ BL,
   J-LOOPEND ;

: J-+LOOP ( -- )                                \ index += n; loop while (old-limit) and
   $D1002273 C-EMITW  $F9400269 C-EMITW  \ (new-limit) agree in sign (ANS crossing)
   4181780107 C-EMITW  3506439531 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   $F940018D C-EMITW                     \ ldr x13,[x12]      index
   4181722506 C-EMITW                    \ ldr x10,[x12,#8]   limit
   $CB0A01AF C-EMITW                     \ sub x15,x13,x10    old
   $8B0901AD C-EMITW                     \ add x13,x13,x9
   $F900018D C-EMITW                     \ str x13,[x12]
   $CB0A01B0 C-EMITW                     \ sub x16,x13,x10    new
   $CA1001EF C-EMITW                     \ eor x15,x15,x16
   $F10001FF C-EMITW                     \ cmp x15,#0
   LCFPOP @ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000A LIT64,  9 9 10 ORR,  LCEMIT @ BL,       \ b.ge loop-top
   J-LOOPEND ;

: J-I ( -- )
   4181780107 C-EMITW  3506439531 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4177527401 C-EMITW  2432705139 C-EMITW ;

: J-J ( -- )                                    \ outer loop index: frame[LOOPSP-2]
   4181780107 C-EMITW  $D100096B C-EMITW 3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4177527401 C-EMITW  2432705139 C-EMITW ;

\ >R R> R@ — the user return stack lives in a data-region stack ([x20+RSTK-OFF],
\ depth at [x20+RSP-CELL]), like the DO/LOOP frames: x25/x28 belong to the
\ compiler, and word frames on the machine stack would unbalance the epilogue.
: W-LDRX ( n n n -- n ) {: rt RN off :}                               \ ldr rt,[rn,#off]
   $F9400000  off 8 / 10 lshift or  RN 5 lshift or  rt or ;

: W-STRX ( n n n -- n ) {: rt RN off :}                               \ str rt,[rn,#off]
   $F9000000  off 8 / 10 lshift or  RN 5 lshift or  rt or ;

: C-FIND-TRUST ( -- )  LBL {: ok :}
   9 LKWTRUST @ ADR,  10 5 MOVZ,  LFIND @ BL,
   13 ok CBNZ,
      0 2 MOVZ,  1 LKWTRUST @ ADR,  2 5 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT SYS,
   ok LBL, ;

: C-PUSH-DREC-NAME ( -- )
   LBL {: pinl :}
   9 12 24 ADDI,
   10 12 16 LDR,  10 10 DNAME-EXT ANDI,  10 pinl CBZ,
      9 12 24 LDR,
   pinl LBL,
   9 G-PUSH
   9 12 16 LDR,  9 9 4 LSLI,  9 9 4 LSRI,  9 G-PUSH ;

: C-PUSH-DATA-CELL ( n -- ) {: off :}
   9 DATA off LDR,  9 G-PUSH ;

: C-PUSH-TRUST-SIG ( n n -- ) {: aoff uoff :}
   aoff C-PUSH-DATA-CELL
   uoff C-PUSH-DATA-CELL ;

: C-CALL-X11-SAVED ( -- )
   SP SP 16 SUBI,  30 SP 0 STR,
   11 BLR,
   30 SP 0 LDR,  SP SP 16 ADDI, ;

: C-CALL-TRUST-PEND ( -- )
   C-FIND-TRUST
   12 DATA PEND-CELL LDR,
   C-PUSH-DREC-NAME
   TSIG-A-CELL TSIG-U-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED ;

: C-CALL-TRUST-LASTC ( -- )
   C-FIND-TRUST
   12 DATA LASTC-CELL LDR,
   C-PUSH-DREC-NAME
   CRSIG-A-CELL CRSIG-U-CELL C-PUSH-TRUST-SIG
   C-CALL-X11-SAVED ;

: C-DIE-DOES ( -- )
   0 2 MOVZ,  1 LKWDOES @ ADR,  2 5 MOVZ,  NR-WRITE SYS,
   0 70 MOVZ,  NR-EXIT SYS, ;

: C-CALL-CHECK-DOES ( -- )
   LBL LBL {: found good :}
   9 LKWCHKDOES @ ADR,  10 11 MOVZ,  LFIND @ BL,
   13 found CBNZ,
      0 2 MOVZ,  1 LKWCHKDOES @ ADR,  2 11 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT SYS,
   found LBL,
   9 DATA BODYBUF-OFF ADDI,
   10 DATA DOESB-CELL LDR,
   9 9 10 ADD,  9 G-PUSH
   12 DATA BODYLEN-CELL LDR,  12 12 10 SUB,  12 G-PUSH
   9 DATA TCSIG-A-CELL LDR,  9 G-PUSH
   9 DATA TCSIG-U-CELL LDR,  9 G-PUSH
   SP SP 16 SUBI,  30 SP 0 STR,  11 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
   10 G-POP  11 0 MOVN,  10 11 CMP,  C-EQ good BCOND,
      C-DIE-DOES
   good LBL, ;

: C-CALL-CHECK-DEFINER ( -- )
   LBL LBL LBL LBL {: nohook fulllen lenok good :}
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
   10 DATA BODYBUF-OFF ADDI,  10 G-PUSH
   10 DATA DOESB-CELL LDR,  10 fulllen CBZ,
      10 10 6 SUBI,  lenok B,
   fulllen LBL,
      10 DATA BODYLEN-CELL LDR,
   lenok LBL,
   10 G-PUSH
   9 DATA HOOK-CELL LDR,
   SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
   10 G-POP  10 good CBNZ,
      C-DIE-DOES
   good LBL,
   nohook LBL, ;

: C-EMIT-DATA-X9! ( n -- ) {: off :}
   9 20 off W-STRX C-EMITW ;

: C-EMIT-CRSIG-PART! ( n n -- ) {: src dst :}
   11 DATA src LDR,  C-X9-LIT
   dst C-EMIT-DATA-X9! ;

: C-EMIT-CRSIG-A! ( -- )
   TCSIG-A-CELL CRSIG-A-CELL C-EMIT-CRSIG-PART! ;

: C-EMIT-CRSIG-U! ( -- )
   TCSIG-U-CELL CRSIG-U-CELL C-EMIT-CRSIG-PART! ;

: C-EMIT-CRSIG-SET ( -- )
   LBL {: none :}
   9 DATA TCSIG-U-CELL LDR,  9 none CBZ,
      C-EMIT-CRSIG-A!
      C-EMIT-CRSIG-U!
   none LBL, ;

: C-RUNTIME-CRSIG-CLEAR ( -- )
   9 0 MOVZ,
   9 DATA CRSIG-A-CELL STR,
   9 DATA CRSIG-U-CELL STR, ;

: J-TOR ( -- )                                                \ pop data -> push RSTK
   $D1002273 C-EMITW  $F9400269 C-EMITW                \ sub x19,#8 ; ldr x9,[x19]
   10 20 RSP-CELL W-LDRX C-EMITW
   $8B0A0E8B C-EMITW                                   \ add x11,x20,x10,lsl#3
   9 11 RSTK-OFF W-STRX C-EMITW
   $9100054A C-EMITW                                   \ add x10,x10,#1
   10 20 RSP-CELL W-STRX C-EMITW ;

: J-RPOP ( -- )                                               \ x9 = RSTK top, x10 = RSP-1
   10 20 RSP-CELL W-LDRX C-EMITW
   $D100054A C-EMITW                                   \ sub x10,x10,#1
   $8B0A0E8B C-EMITW                                   \ add x11,x20,x10,lsl#3
   9 11 RSTK-OFF W-LDRX C-EMITW ;

: J-RFROM ( -- )  J-RPOP                                      \ pop RSTK -> push data
   10 20 RSP-CELL W-STRX C-EMITW
   $F9000269 C-EMITW  $91002273 C-EMITW ;              \ str x9,[x19] ; add x19,#8

: J-RFETCH ( -- )  J-RPOP                                     \ peek RSTK -> push data
   $F9000269 C-EMITW  $91002273 C-EMITW ;

\ EXIT: emit a placeholder word holding the PREVIOUS chain offset (0 = end);
\ `;` walks the chain and patches each into `b epilogue`. RECURSE: bl back to
\ the current word's entry (PEND slot.addr) — every word has the standard
\ prologue/epilogue, so calling into the open definition is well-formed.
: J-EXIT ( -- )
   9 DATA EXITH-CELL LDR,                              \ x9 = prev chain offset
   10 CP DBASE SUB,  10 DATA EXITH-CELL STR,           \ head := this placeholder
   LCEMIT @ BL, ;

: J-RECURSE ( -- )
   9 DATA PEND-CELL LDR,  9 9 0 LDR,  $94000000 $3FFFFFF C-BBACK ;   \ bl entry

: C-SIG-START ( n -- ) {: lmiss :}
   LBL LBL {: ws got :}
   11 DATA INP-CELL LDR,  12 DATA INE-CELL LDR,
   ws LBL,  11 12 CMP,  C-GE lmiss BCOND,
      13 11 0 LDRB,  13 32 CMPI,  C-HI got BCOND,
      11 11 1 ADDI,  ws B,
   got LBL,  13 40 CMPI,  C-NE lmiss BCOND,
   14 11 0 ADDI,  15 11 0 ADDI, ;

: C-SIG-END ( n -- ) {: lmiss :}
   LBL {: scan :}
   scan LBL,  15 12 CMP,  C-GE lmiss BCOND,
      13 15 0 LDRB,  15 15 1 ADDI,  13 41 CMPI,  C-NE scan BCOND, ;

: C-SIG-INNER$ ( -- )
   11 14 1 ADDI,  12 15 14 SUB,  12 12 2 SUBI, ;

: C-SIG-FULL$ ( -- )
   11 14 0 ADDI,  12 15 14 SUB, ;

: C-SIG-CAPTURE-TSIG ( -- )
   15 DATA INP-CELL STR,
   C-SIG-INNER$
   11 DATA TSIG-A-CELL STR,  12 DATA TSIG-U-CELL STR,
   C-SIG-FULL$  LBCS @ BL, ;

: C-SIG-BAD ( -- )
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   0 76 MOVZ,  NR-EXIT SYS, ;

: C-PARSE-CREATED-SIG ( -- )
   LBL LBL LBL LBL {: cpy cpd done bad :}
   bad C-SIG-START
   bad C-SIG-END
   15 DATA INP-CELL STR,
   C-SIG-INNER$
   10 12 0 ADDI,
   12 DATA 0 LDR,  15 12 0 ADDI,
   14 12 10 ADD,  14 DP-CHECK
   9 10 0 ADDI,
   cpy LBL,  9 cpd CBZ,
      13 11 0 LDRB,  13 12 0 STRB,
      12 12 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cpy B,
   cpd LBL,
   12 DATA 0 STR,
   15 DATA TCSIG-A-CELL STR,  10 DATA TCSIG-U-CELL STR,
   done B,
   bad LBL,  C-SIG-BAD
   done LBL, ;

: J-DOES ( -- )
   LBL {: dok :}
   12 DATA LOCF-CELL LDR,  12 dok CBZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   dok LBL,
   9 DATA BODYLEN-CELL LDR,  9 DATA DOESB-CELL STR,
   C-PARSE-CREATED-SIG
   C-EMIT-CRSIG-SET
   $1000008A C-EMITW                     \ adr x10, #+16 = D (4 words ahead)
   16 20 DOESP-CELL W-LDRX C-EMITW       \ x16 = LDOESPATCH runtime addr
   $D63F0200 C-EMITW                     \ blr x16
   J-EXIT                                \ word 4: the defining word ends here
   9 $D10043FF LIT64,  LCEMIT @ BL,      \ D: fresh prologue for the does-body
   9 $F90003FE LIT64,  LCEMIT @ BL, ;

: J-QUOT ( -- )
   LBL {: qok :}
   9 DATA QPATCH-CELL LDR,  9 qok CBZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   qok LBL,
   9 CP 0 ADDI,  9 DATA QPATCH-CELL STR,
   9 $14000000 LIT64,  LCEMIT @ BL,               \ b-over placeholder
   9 CP 0 ADDI,  9 DATA QENT-CELL STR,            \ the quotation's entry
   9 DATA EXITH-CELL LDR,  9 DATA QXH-CELL STR,   \ scope the EXIT chain
   12 0 MOVZ,  12 DATA EXITH-CELL STR,
   9 $D10043FF LIT64,  LCEMIT @ BL,               \ its own prologue
   9 $F90003FE LIT64,  LCEMIT @ BL, ;

: J-SEMIQUOT ( -- )
   LBL {: sqok :}
   9 DATA QPATCH-CELL LDR,  9 sqok CBNZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   sqok LBL,
   14 CP 0 ADDI,  9 DATA EXITH-CELL LDR,  LBCHAIN @ BL,   \ exits -> this epilogue
   9 DATA QXH-CELL LDR,  9 DATA EXITH-CELL STR,
   9 $F94003FE LIT64,  LCEMIT @ BL,                \ epilogue: ldr x30,[sp]
   9 $910043FF LIT64,  LCEMIT @ BL,                \ add sp,#16
   9 W-RET LIT64,  LCEMIT @ BL,
   9 DATA QPATCH-CELL LDR,  LPAT @ BL,             \ b-over lands here
   11 DATA QENT-CELL LDR,  C-LIT                   \ push the xt in the outer word
   12 0 MOVZ,  12 DATA QPATCH-CELL STR, ;

: EMIT-DOESPATCH ( -- )
   LBL {: nocr :}
   LDOESPATCH @ LBL,
   SP SP 32 SUBI,  30 SP 0 STR,  10 SP 8 STR,
   2 3 MOVZ,  LPROT @ BL,                                \ region -> RW
   10 SP 8 LDR,
   11 DATA LASTC-CELL LDR,                               \ created slot
   12 11 0 LDR,  13 11 8 LDR,  12 12 13 ADD,             \ x12 = RET addr
   14 10 12 SUB,  14 14 2 ASRI,                          \ delta words (negative)
   5 $3FFFFFF LIT64,  14 14 5 AND,
   5 $14000000 LIT64,  14 14 5 ORR,                      \ b D
   14 12 0 STRW,
   12 SP 16 STR,
   2 5 MOVZ,  LPROT @ BL,                                \ region -> RX
   12 SP 16 LDR,
   12 DCCVAU,  DSB-ISH,  12 ICIVAU,  DSB-ISH,  ISB,      \ flush the patched line
   9 DATA CRSIG-U-CELL LDR,  9 nocr CBZ,
      C-CALL-TRUST-LASTC
      C-RUNTIME-CRSIG-CLEAR
   nocr LBL,
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

\ ---- interpret-mode defining words ----
\ record defining words for the checker: append the kind token + run the hook
\ (verdict ignored — create/variable/constant always publish).
: C-DEFHOOK ( ptr u8 n -- )  LBL {: kwv klen nohk :}
   11 kwv @ ADR,  12 klen MOVZ,  LBCS @ BL,
   9 DATA HOOK-CELL LDR,  9 nohk CBZ,
   10 DATA BODYBUF-OFF ADDI,  10 G-PUSH
   10 DATA BODYLEN-CELL LDR,  10 G-PUSH
   SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
   10 G-POP
   nohk LBL, ;

: C-STORE-NAME ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL {: short fail capok lcopy lcd scopy scd done :}
   12 DATA TKL-CELL LDR,
   13 12 0 ADDI,
   12 DNAME-INL CMPI,  C-LE short BCOND,
      14 DNAME-EXT LIT64,  13 13 14 ORR,  13 9 16 STR,
      15 12 3 ADDI,  15 15 2 LSRI,  15 15 2 LSLI,
      16 CP 15 ADD,
      10 REGION $4000 - LIT64,  10 DBASE 10 ADD,  16 10 CMP,  C-LT capok BCOND,
         fail B,
      capok LBL,
      CP 9 24 STR,
      10 DATA TKA-CELL LDR,
      11 CP 0 ADDI,
      14 12 0 ADDI,
      lcopy LBL,  14 lcd CBZ,
         15 10 0 LDRB,  15 11 0 STRB,
         10 10 1 ADDI,  11 11 1 ADDI,  14 14 1 SUBI,  lcopy B,
      lcd LBL,
      CP 16 0 ADDI,
      done B,
   short LBL,
      13 9 16 STR,
      11 9 24 ADDI,  10 DATA TKA-CELL LDR,  14 12 0 ADDI,
      scopy LBL,  14 scd CBZ,
         15 10 0 LDRB,  15 11 0 STRB,
         10 10 1 ADDI,  11 11 1 ADDI,  14 14 1 SUBI,  scopy B,
      scd LBL,
      done B,
   fail LBL,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 76 MOVZ,  NR-EXIT SYS,
   done LBL, ;

: EMIT-CREATE ( -- )
   LBL {: nokind :}
   LCREATE @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,  15 SP 8 STR,
   2 3 MOVZ,  LPROT @ BL,
   LTOK @ BL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP @ BL,   \ seed "NAME " for the hook
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   C-STORE-NAME
   CP 9 0 STR,
   14 DATA CUR-CELL LDR,  14 9 40 STR,
   11 DATA 0 LDR,
   C-LIT
   9 W-RET LIT64,  LCEMIT @ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,
   9 DATA LASTC-CELL STR,
   NDICT NDICT 1 ADDI,  9 9 0 LDR,                      \ x9 = body start for the flush
   2 5 MOVZ,  LPROT @ BL,  LFLUSH @ BL,
   15 SP 8 LDR,  15 nokind CBZ,
   LKWCREATE 6 C-DEFHOOK
   nokind LBL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: C-CREATE ( -- )  15 1 MOVZ,  LCREATE @ BL, ;

: C-VARIABLE ( -- )  C-CREATE
   7 DATA 0 LDR,  7 7 8 ADDI,  7 DP-CHECK  7 DATA 0 STR, ;

: C-CONSTANT ( -- )
   2 3 MOVZ,  LPROT @ BL,  LTOK @ BL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP @ BL,   \ seed "NAME " for the hook
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   C-STORE-NAME
   15 G-POP                                             \ n -> x15 after name storage (clobbers x15)
   CP 9 0 STR,  14 DATA CUR-CELL LDR,  14 9 40 STR,
   11 15 0 ADDI,  C-LIT
   9 W-RET LIT64,  LCEMIT @ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,
   NDICT NDICT 1 ADDI,  9 9 0 LDR,                      \ x9 = body start for the flush
   2 5 MOVZ,  LPROT @ BL,  LFLUSH @ BL,
   LKWCONST 8 C-DEFHOOK ;

: C-CLEAR-TRUSTED-STATE ( -- )
   9 0 MOVZ,
   9 DATA TSIG-A-CELL STR,   9 DATA TSIG-U-CELL STR,
   9 DATA TCSIG-A-CELL STR,  9 DATA TCSIG-U-CELL STR,
   9 DATA DOESB-CELL STR,
   9 DATA TRUSTED-CELL STR, ;

: C-PARSE-REQUIRED-SIG ( -- )
   LBL LBL {: done bad :}
   bad C-SIG-START
   bad C-SIG-END
   C-SIG-CAPTURE-TSIG
   done B,
   bad LBL,  C-SIG-BAD
   done LBL, ;

: C-PARSE-TRUST-SIG ( -- )
   C-PARSE-REQUIRED-SIG ;

: C-COLON-MAYBE-SIG ( -- )
   LBL LBL {: nsig scd :}
   nsig C-SIG-START
   scd C-SIG-END
   scd LBL,
   C-SIG-CAPTURE-TSIG
   nsig LBL, ;

: C-TRUSTED ( -- )
   LBL LBL LBL {: cpok ndok done :}
   2 3 MOVZ,  LPROT @ BL,
   9 REGION $4000 - LIT64,  9 DBASE 9 ADD,  CP 9 CMP,  C-LT cpok BCOND,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 76 MOVZ,  NR-EXIT SYS,
   cpok LBL,
   9 DICT-CAP MOVZ,  NDICT 9 CMP,  C-LT ndok BCOND,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 77 MOVZ,  NR-EXIT SYS,
   ndok LBL,
   LTOK @ BL,  0 done CBZ,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   9 DATA PEND-CELL STR,
   C-STORE-NAME
   CP 9 0 STR,
   14 DATA CUR-CELL LDR,  14 9 40 STR,
   5 CFSTK-OFF LIT64,  11 DBASE 5 ADD,  12 0 MOVZ,  12 11 0 STR,
   12 DATA LOCN-CELL STR,  12 DATA LOCF-CELL STR,
   12 DATA BODYLEN-CELL STR,
   C-CLEAR-TRUSTED-STATE
   12 1 MOVZ,  12 DATA TRUSTED-CELL STR,
   LBCAP @ BL,
   C-PARSE-TRUST-SIG
   12 0 MOVZ,  12 DATA VSP-CELL STR,  12 DATA SNAPSP-CELL STR,
   12 DATA EXITH-CELL STR,  12 DATA LVD-CELL STR,
   12 DATA QPATCH-CELL STR,
   12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,
   12 FRALL MOVZ,  12 DATA FRFREE-CELL STR,
   9 $D10043FF LIT64,  LCEMIT @ BL,
   9 $F90003FE LIT64,  LCEMIT @ BL,
   done LBL, ;

: C-IMMEDIATE ( -- )
   2 3 MOVZ,  LPROT @ BL,
   9 NDICT 0 ADDI,  9 9 1 SUBI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 16 LDR,  10 10 DNAME-IMM ORRI,  10 9 16 STR,
   2 5 MOVZ,  LPROT @ BL, ;

: C-POSTPONE ( -- )
   LBL LBL LBL {: pok pnimm pdone :}
   LTOK @ BL,  9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND @ BL,
   13 pok CBNZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT SYS,
   pok LBL,
   14 13 2 ANDI,  14 pnimm CBZ,
      C-CALL  pdone B,
   pnimm LBL,
      C-LIT
      9 LKWCOMPC @ ADR,  10 8 MOVZ,  LFIND @ BL,
      C-CALL
   pdone LBL, ;

: C-QUOTE-START ( -- )
   12 DATA INP-CELL LDR,  12 12 1 ADDI,  13 12 0 ADDI, ;

: C-QUOTE-EOF ( -- )
   0 74 MOVZ,  NR-EXIT SYS, ;

: C-QUOTE-SCAN ( -- )
   LBL LBL LBL {: sl sd eof :}
   sl LBL,
      14 DATA INE-CELL LDR,
      12 14 CMP,  C-GE eof BCOND,
      9 12 0 LDRB,  9 $22 CMPI,  C-EQ sd BCOND,
      12 12 1 ADDI,  sl B,
   eof LBL,  C-QUOTE-EOF
   sd LBL, ;

: C-QUOTE-CONSUME ( -- )
   10 12 13 SUB,  16 13 0 ADDI,  12 12 1 ADDI,  12 DATA INP-CELL STR, ;

: C-QUOTE-SAVE ( -- )
   SP SP 16 SUBI,  16 SP 0 STR,  10 SP 8 STR, ;

: C-QUOTE-RESTORE ( -- )
   16 SP 0 LDR,  10 SP 8 LDR, ;

: C-QUOTE-SAVED-DROP ( -- )
   SP SP 16 ADDI, ;

: C-ISDQ ( -- )
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   LBL LBL {: cl cd :}
   12 DATA 0 LDR,  15 12 0 ADDI,                        \ x12 = DP, x15 = string base
   14 12 10 ADD,  14 DP-CHECK
   11 13 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 12 0 STRB,  12 12 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   12 DATA 0 STR,                                       \ allot: DP advances past the copy
   15 G-PUSH  10 G-PUSH ;

: C-ICQ ( -- )
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   LBL LBL LBL {: capok cl cd :}
   10 255 CMPI,  C-LE capok BCOND,  0 76 MOVZ,  NR-EXIT SYS,
   capok LBL,
   12 DATA 0 LDR,  15 12 0 ADDI,                       \ x15 = counted string base
   14 12 10 ADD,  14 14 1 ADDI,  14 DP-CHECK
   10 12 0 STRB,  12 12 1 ADDI,
   11 13 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 12 0 STRB,  12 12 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   12 DATA 0 STR,
   15 G-PUSH ;

: C-IDOTQ ( -- )
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   0 1 MOVZ,  1 13 0 ADDI,  2 10 0 ADDI,  NR-WRITE SYS, ;

: C-CHAR ( -- )   LTOK @ BL,  9 DATA TKA-CELL LDR,  9 9 0 LDRB,  9 G-PUSH ;

: C-BCHAR ( -- )  LTOK @ BL,  11 DATA TKA-CELL LDR,  11 11 0 LDRB,  LVPUSHC @ BL, ;

: C-TICK ( -- )
   LBL {: tk :}
   LTOK @ BL,  9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND @ BL,
   13 tk CBZ,  11 G-PUSH  tk LBL, ;

: C-BTICK ( -- )
   LBL {: bk :}
   LTOK @ BL,  9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND @ BL,
   13 bk CBZ,  C-LIT  bk LBL, ;

: C-LBRACE-GUARDS ( -- )
   LBL LBL LBL {: cfok xok qlok :}
   5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,  11 cfok CBZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   cfok LBL,
   11 DATA QPATCH-CELL LDR,  11 qlok CBZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   qlok LBL,
   11 DATA EXITH-CELL LDR,  11 xok CBZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   xok LBL, ;

: C-LBRACE-STORE-ONE ( -- )
   LBL LBL LBL LBL LBL LBL {: nlok noti ncp ncd tsl tsd :}
   11 DATA LOCN-CELL LDR,  11 64 CMPI,  C-LT nlok BCOND,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   nlok LBL,
   13 DATA TKL-CELL LDR,  13 1 CMPI,  C-NE noti BCOND,
   13 DATA TKA-CELL LDR,  13 13 0 LDRB,  14 $20 MOVZ,  13 13 14 ORR,  13 105 CMPI,  C-NE noti BCOND,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   noti LBL,
   11 DATA LOCN-CELL LDR,  12 LOC-REC MOVZ,  11 11 12 MUL,  5 LOCNAMES LIT64,  11 11 5 ADD,  11 DATA 11 ADD,
   14 0 MOVZ,  8 DATA TKL-CELL LDR,  10 DATA TKA-CELL LDR,
   tsl LBL,  14 8 CMP,  C-GE tsd BCOND,
      15 10 14 ADD,  15 15 0 LDRB,  15 58 CMPI,  C-EQ tsd BCOND,
      14 14 1 ADDI,  tsl B,
   tsd LBL,
   14 11 0 STR,
   12 11 8 ADDI,  13 DATA TKA-CELL LDR,
   ncp LBL,  14 ncd CBZ,  15 13 0 LDRB, 15 12 0 STRB, 12 12 1 ADDI, 13 13 1 ADDI, 14 14 1 SUBI, ncp B,
   ncd LBL,
   11 DATA LOCN-CELL LDR,  11 11 1 ADDI,  11 DATA LOCN-CELL STR, ;

: C-LBRACE-PARSE-NAMES ( -- )
   LBL LBL LBL {: nl nd nstore :}
   6 DATA LOCN-CELL LDR,
   nl LBL,
      LTOK @ BL,  0 nd CBZ,
      LBCAP @ BL,                                          \ locals reach the checker too
      0 LKWENDLOC @ ADR,  1 2 MOVZ,  LKWCMP @ BL,  0 nstore CBZ,  nd B,
      nstore LBL,
      C-LBRACE-STORE-ONE
      nl B,
   nd LBL, ;

: C-LBRACE-CARVE-FRAME ( -- )
   LBL LBL {: pl pd :}
   13 DATA LOCN-CELL LDR,  14 13 6 SUB,
   5 14 3 LSLI,  5 5 15 ADDI,  5 5 $FFFFFFFFFFFFFFF0 ANDI,
   9 $D10003FF LIT64,  15 5 10 LSLI,  9 9 15 ORR,  LCEMIT @ BL,
   15 DATA LOCF-CELL LDR,  15 15 5 ADD,  15 DATA LOCF-CELL STR,
   12 DATA LOCF-CELL LDR,  12 12 3 LSRI,
   13 DATA LOCN-CELL LDR,  13 13 1 SUBI,
   pl LBL,
      13 6 CMP,  C-LT pd BCOND,
      9 $D1002273 LIT64,  LCEMIT @ BL,
      9 $F9400269 LIT64,  LCEMIT @ BL,
      5 12 13 SUB,  5 5 1 SUBI,
      9 $F90003E9 LIT64,  5 5 10 LSLI,  9 9 5 ORR,  LCEMIT @ BL,
      13 13 1 SUBI,  pl B,
   pd LBL, ;

: C-LBRACE ( -- )
   C-LBRACE-GUARDS
   C-LBRACE-PARSE-NAMES
   C-LBRACE-CARVE-FRAME ;

\ compile-mode PC-RELATIVE address push: emit `adr x9, target` then the push
\ stencil. Unlike C-LIT's absolute movz/movk, the offset survives the AOT blob
\ copy and the ASLR slide, because the target (an embedded S" body) moves WITH
\ this instruction. target in x11; CP (the emit cursor / future ADR pc) is x28.
: C-ADR ( -- )
   5 11 28 SUB,                                                       \ x5 = d = target - CP
   8 $10000009 LIT64,                                                 \ ADR opcode | Rd=x9
   6 3 MOVZ,  7 5 6 AND,  7 7 29 LSLI,  8 8 7 ORR,                    \ | (d & 3) << 29
   7 5 2 LSRI,  6 $7FFFF LIT64,  7 7 6 AND,  7 7 5 LSLI,  8 8 7 ORR,  \ | ((d>>2) & 0x7FFFF) << 5
   9 8 0 ADDI,  LCEMIT @ BL,                                          \ emit the ADR word
   9 W-PUSH0 LIT64,  LCEMIT @ BL,  9 W-PUSH1 LIT64,  LCEMIT @ BL, ;

: C-SDQ ( -- )
   LBL LBL {: cl cd :}
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   C-QUOTE-SAVE
   C-QUOTE-RESTORE
   11 16 0 ADDI,  12 10 1 ADDI,  LBCS @ BL,
   15 CP 0 ADDI,  9 $14000000 LIT64,  LCEMIT @ BL,
   12 CP 0 ADDI,
   C-QUOTE-RESTORE
   11 16 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   9 15 0 ADDI,  15 10 0 ADDI,  LPAT @ BL,
   11 12 0 ADDI,  C-ADR                                \ push byte addr PC-relative (AOT/ASLR-safe)
   11 15 0 ADDI,  C-LIT                                \ push len (a value, absolute is fine)
   C-QUOTE-SAVED-DROP ;

: C-CQ ( -- )
   LBL LBL LBL {: capok cl cd :}
   C-QUOTE-START
   C-QUOTE-SCAN
   C-QUOTE-CONSUME
   C-QUOTE-SAVE
   10 255 CMPI,  C-LE capok BCOND,  0 76 MOVZ,  NR-EXIT SYS,
   capok LBL,
   C-QUOTE-RESTORE
   11 16 0 ADDI,  12 10 1 ADDI,  LBCS @ BL,
   15 CP 0 ADDI,  9 $14000000 LIT64,  LCEMIT @ BL,
   12 CP 0 ADDI,
   C-QUOTE-RESTORE
   10 28 0 STRB,  28 28 1 ADDI,
   11 16 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   9 15 0 ADDI,  15 10 1 ADDI,  LPAT @ BL,
   11 12 0 ADDI,  C-ADR
   C-QUOTE-SAVED-DROP ;

: C-DOTQ ( -- )
   LBL {: ok :}
   C-SDQ
   9 LKWTYPE @ ADR,  10 4 MOVZ,  LFIND @ BL,
   13 ok CBNZ,  0 70 MOVZ,  NR-EXIT SYS,
   ok LBL,
   C-CALL ;
variable CFSK

: CF-ENTRY ( n ptr u8 n n -- ) {: lmainlbl kwvar:ptr kwlen hxt :}
   LBL CFSK !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 CFSK @ CBZ,
   LVSPILL @ BL,
   hxt execute  lmainlbl B,
   CFSK @ LBL, ;
s" cf-entry" s" n ptr a n n --" TRUST

\ cfn-entry: keyword case WITHOUT the spill — loop words manage the VS
\ themselves (BEGIN snapshots it, AGAIN/REPEAT reconcile to the snapshot).
: CFN-ENTRY ( n ptr u8 n n -- ) {: lmainlbl kwvar:ptr kwlen hxt :}
   LBL CFSK !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 CFSK @ CBZ,
   hxt execute  lmainlbl B,
   CFSK @ LBL, ;
s" cfn-entry" s" n ptr a n n --" TRUST
\ ---- MAIN, split into emission-ordered phases sharing label variables ----
variable LMAIN  variable LEXIT  variable LCOMPILE  variable LUNDEF
variable LEX0  variable LUN0   \ re-entrant evaluate: original-path continuations of LEXIT / LUNDEF
variable CLOC-MAIN  variable CLOC-NOT
variable CLOC-MEM   variable CLOC-QOK
variable CFSK2

\ cfb-entry: branch keywords (if/until/while) with the condition on the VS —
\ a REGISTER top branches directly (no spill + memory pop); con or empty falls
\ back to the spill + pop path. hxtr gets the condition reg in x14.
: CFB-ENTRY ( n ptr u8 n n n -- ) {: lmainlbl kwvar:ptr kwlen hxtm hxtr :}
   LBL CFSK !  LBL CFSK2 !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 CFSK @ CBZ,
   6 DATA VSP-CELL LDR,  6 CFSK2 @ CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 CFSK2 @ CBNZ,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 LDR,
   SP SP 16 SUBI,  14 SP 8 STR,
   LVDROP @ BL,  LVSPILL @ BL,
   14 SP 8 LDR,  SP SP 16 ADDI,
   hxtr execute
   lmainlbl B,
   CFSK2 @ LBL,
   LVSPILL @ BL,
   hxtm execute
   lmainlbl B,
   CFSK @ LBL, ;
s" cfb-entry" s" n ptr a n n n --" TRUST

\ cfbn-entry: like CFB-ENTRY but the register path neither spills nor saves —
\ UNTIL reconciles to the BEGIN snapshot itself; the condition reg x14 survives
\ LVDROP (which only relabels the VS, no emission).
: CFBN-ENTRY ( n ptr u8 n n n -- ) {: lmainlbl kwvar:ptr kwlen hxtm hxtr :}
   LBL CFSK !  LBL CFSK2 !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 CFSK @ CBZ,
   6 DATA VSP-CELL LDR,  6 CFSK2 @ CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 CFSK2 @ CBNZ,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 LDR,
   LVDROP @ BL,
   hxtr execute
   lmainlbl B,
   CFSK2 @ LBL,
   LVSPILL @ BL,
   hxtm execute
   lmainlbl B,
   CFSK @ LBL, ;
s" cfbn-entry" s" n ptr a n n n --" TRUST

: J-IFR ( -- )  C-PUSHCP  8 $B4000000 LIT64,  9 8 14 ORR,  LCEMIT @ BL, ;

: J-WHILER ( -- )  J-IFR ;

: J-UNTILR ( -- )                                 \ reg flag -> x17 first: the reconcile
   8 $AA0003F1 LIT64,  7 14 16 LSLI,  9 8 7 ORR,  LCEMIT @ BL,   \ may reload into it
   J-UNTILX ;

: C-LOCAL-REF-LABELS ( -- )
   LBL CLOC-MEM !  LBL CLOC-QOK ! ;

: C-LOCAL-REF-ARGS ( n n -- )
   CLOC-NOT !  CLOC-MAIN ! ;

: C-LOCAL-REF ( n n -- )
   C-LOCAL-REF-ARGS
   C-LOCAL-REF-LABELS
   LLOC-FIND @ BL,  0 0 CMPI,  C-LT CLOC-NOT @ BCOND,
   11 DATA QPATCH-CELL LDR,  11 CLOC-QOK @ CBZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   CLOC-QOK @ LBL,
   LVRALLOC @ BL,  14 CLOC-MEM @ CBZ,
   7 DATA LOCF-CELL LDR,  7 7 3 LSRI,  7 7 0 SUB,  7 7 1 SUBI,
   9 $F94003E0 LIT64,  9 9 14 ORR,  7 7 10 LSLI,  9 9 7 ORR,  LCEMIT @ BL,
   LVPUSHR @ BL,
   CLOC-MAIN @ B,
   CLOC-MEM @ LBL,
   LVSPILL @ BL,
   7 DATA LOCF-CELL LDR,  7 7 3 LSRI,  7 7 0 SUB,  7 7 1 SUBI,
   9 $F94003E9 LIT64,  7 7 10 LSLI,  9 9 7 ORR,  LCEMIT @ BL,
   9 W-PUSH0 LIT64,  LCEMIT @ BL,  9 W-PUSH1 LIT64,  LCEMIT @ BL,
   CLOC-MAIN @ B, ;
s" c-local-ref" s" n n --" TRUST

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

: EM-RUNTIME-STACK ( -- )
   XREG-RBASE LANCHOR @ ADR,
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   XDS SP 0 ADDI, ;

: EM-MMAP-CODE-REGION ( -- )
   LBL {: rvok :}
   0 RBASE-VA LIT64,  1 REGION LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE-FIXED LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 RBASE-VA LIT64,  0 5 CMP,
   C-EQ rvok BCOND,
      0 78 MOVZ,  NR-EXIT SYS,
   rvok LBL, ;

: EM-SEED-DICT ( -- )
   LBL LBL {: scopy scdone :}
   DBASE 0 0 ADDI,
   CP DBASE 0 ADDI,  5 DICT-SIZE LIT64,  CP CP 5 ADD,
   11 LNCOUNT @ ADR,  11 11 0 LDR,  NDICT 11 0 ADDI,
   9 LDICT @ ADR,  10 DBASE 0 ADDI,  12 11 0 ADDI,
   scopy LBL,
      12 scdone CBZ,
      5 9 0 LDR,  6 9 8 LDR,
      7 XREG-RBASE 5 ADD,  7 10 0 STR,
      6 6 5 SUB,  6 6 4 SUBI,  6 10 8 STR,
      5 9 16 LDR,  5 10 16 STR,
      6 9 24 LDR,
      LBL {: inl-name :}
      8 DNAME-EXT LIT64,  8 5 8 AND,  8 inl-name CBZ,
         6 XREG-RBASE 6 ADD,
      inl-name LBL,
      6 10 24 STR,  5 9 32 LDR,  5 10 32 STR,
      5 9 40 LDR,  5 10 40 STR,
      9 9 DREC ADDI,  10 10 DREC ADDI,  12 12 1 SUBI,  scopy B,
   scdone LBL, ;

: EM-MMAP-DATA-REGION ( -- )
   LBL {: dvok :}
   0 DATA-VA LIT64,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE-FIXED LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 DATA-VA LIT64,  0 5 CMP,
   C-EQ dvok BCOND,
      0 78 MOVZ,  NR-EXIT SYS,
   dvok LBL, ;

: EM-DATA-INIT ( -- )
   20 0 RBASE-CELL STR,
   DATA 0 0 ADDI,
   XDS DATA S0-CELL STR,
   13 DATA ARGC-CELL STR,  14 DATA ARGV-CELL STR,  15 DATA ENVP-CELL STR,
   5 DATA-START MOVZ,  7 DATA 5 ADD,  7 DATA DP-CELL STR, ;

: EM-SNAPSHOT-COPY-CODE ( -- )
   LBL LBL {: sc1 sc1d :}
   13 DBASE 0 ADDI,  14 0 MOVZ,
   sc1 LBL,  14 6 CMP,  C-GE sc1d BCOND,
      3 8 14 ADD,  3 3 0 LDRB,  4 13 14 ADD,  3 4 0 STRB,
      14 14 1 ADDI,  sc1 B,
   sc1d LBL, ;

: EM-SNAPSHOT-COPY-DATA ( -- )
   LBL LBL {: sc2 sc2d :}
   8 12 7 SUB,  13 DATA 0 ADDI,  14 0 MOVZ,
   sc2 LBL,  14 7 CMP,  C-GE sc2d BCOND,
      3 8 14 ADD,  3 3 0 LDRB,  4 13 14 ADD,  3 4 0 STRB,
      14 14 1 ADDI,  sc2 B,
   sc2d LBL, ;

: EM-SNAPSHOT-REBASE-DICT ( -- )
   LBL LBL LBL LBL {: sdl2 sdn2 sds2 srn :}
   9 DBASE 0 ADDI,  10 0 MOVZ,
   sdl2 LBL,  10 NDICT CMP,  C-GE sdn2 BCOND,
      13 9 0 LDR,
      13 21 CMP,  C-LT sds2 BCOND,
      14 21 22 ADD,  13 14 CMP,  C-GE sds2 BCOND,
      13 13 21 SUB,  13 13 25 ADD,  13 9 0 STR,
      sds2 LBL,
      13 9 16 LDR,  13 13 DNAME-EXT ANDI,  13 srn CBZ,
      13 9 24 LDR,
      13 21 CMP,  C-LT srn BCOND,
      14 21 22 ADD,  13 14 CMP,  C-GE srn BCOND,
      13 13 21 SUB,  13 13 25 ADD,  13 9 24 STR,
      srn LBL,  9 9 DREC ADDI,  10 10 1 ADDI,  sdl2 B,
   sdn2 LBL, ;

: EM-SNAPSHOT-REBASE-CALLS ( -- )
   LBL LBL LBL {: srl srn srx :}
   9 DBASE 0 ADDI,  5 DICT-SIZE LIT64,  9 9 5 ADD,
   srl LBL,  9 CP CMP,  C-GE srx BCOND,
      10 9 0 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
      5 $D2800010 LIT64,  10 5 CMP,  C-NE srn BCOND,
      10 9 4 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
      5 $F2A00010 LIT64,  10 5 CMP,  C-NE srn BCOND,
      10 9 8 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
      5 $F2C00010 LIT64,  10 5 CMP,  C-NE srn BCOND,
      10 9 12 LDRW,  5 $D63F0200 LIT64,  10 5 CMP,  C-NE srn BCOND,
      10 9 0 LDRW,  10 10 5 LSRI,  5 $FFFF LIT64,  10 10 5 AND,  13 10 0 ADDI,
      10 9 4 LDRW,  10 10 5 LSRI,  5 $FFFF LIT64,  10 10 5 AND,  10 10 16 LSLI,  13 13 10 ORR,
      10 9 8 LDRW,  10 10 5 LSRI,  5 $FFFF LIT64,  10 10 5 AND,  10 10 32 LSLI,  13 13 10 ORR,
      13 21 CMP,  C-LT srn BCOND,
      14 21 22 ADD,  13 14 CMP,  C-GE srn BCOND,
      13 13 21 SUB,  13 13 25 ADD,
      10 9 0 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 0 ADDI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 0 STRW,
      10 9 4 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 16 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 4 STRW,
      10 9 8 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 32 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 8 STRW,
      9 9 12 ADDI,
   srn LBL,  9 9 4 ADDI,  srl B,
   srx LBL, ;

: EM-SNAPSHOT-RX-FLUSH ( -- )
   2 5 MOVZ,  LPROT @ BL,
   9 DBASE 0 ADDI,  5 DICT-SIZE LIT64,  9 9 5 ADD,  LFLUSH @ BL, ;

\ ---- AOT snapshot? (trailer at the end of our own __text). If present:
\ restore both regions verbatim (fixed VAs keep region addresses valid),
\ relocate engine-text call chains (the only ASLR-movers), boot WARM. ----
: EM-SNAPSHOT-RESTORE ( -- )
   LBL LBL LBL {: snomag snbad snok :}
   24 0 MOVZ,                                       \ x24 = snapshot flag
   9 DATA RBASE-CELL LDR,  25 9 0 ADDI,             \ x25 = live text CONTENT base
   10 9 0 ADDI,  5 $1000 LIT64,  10 10 5 SUB,
   11 10 IMAGE-TEXT-SIZE-OFF LDR,                   \ S = our executable text size
   12 10 11 ADD,  5 IMAGE-TEXT-TRAILER-ADJ LIT64,  12 12 5 ADD,  12 12 40 SUBI,                    \ trailer from image base
   13 12 0 LDR,  5 SNAP-MAGIC LIT64,  13 5 CMP,  C-NE snomag BCOND,
   5 IMAGE-TEXT-CONTENT-ADJ LIT64,  11 11 5 SUB,
   21 12 8 LDR,                                     \ x21 = snapshot-time text base
   15 12 16 LDR,                                    \ x15 = ndict
   6 12 24 LDR,                                     \ x6 = region payload len
   7 12 32 LDR,                                     \ x7 = data payload len
   \ corrupt/truncated trailer must never smear the regions: exit 79
   5 REGION LIT64,  6 5 CMP,  C-GT snbad BCOND,
   5 DATA-SIZE LIT64,  7 5 CMP,  C-GT snbad BCOND,
   5 DICT-CAP MOVZ,  15 5 CMP,  C-GT snbad BCOND,
   snok B,
   snbad LBL,  0 79 MOVZ,  NR-EXIT SYS,
   snok LBL,
   9 DATA ARGC-CELL LDR,  10 DATA ARGV-CELL LDR,  0 DATA ENVP-CELL LDR,
   22 11 6 SUB,  22 22 7 SUB,  22 22 40 SUBI,       \ x22 = engine text len then
   8 12 7 SUB,  8 8 6 SUB,                          \ region payload src
   EM-SNAPSHOT-COPY-CODE
   EM-SNAPSHOT-COPY-DATA
   25 DATA RBASE-CELL STR,                          \ live values over stale copies
   XDS DATA S0-CELL STR,
   9 DATA ARGC-CELL STR,  10 DATA ARGV-CELL STR,  0 DATA ENVP-CELL STR,
   NDICT 15 0 ADDI,
   CP DBASE 6 ADD,
   EM-SNAPSHOT-REBASE-DICT
   EM-SNAPSHOT-REBASE-CALLS
   EM-SNAPSHOT-RX-FLUSH
   24 1 MOVZ,
   24 DATA SNAP-CELL STR,
   snomag LBL, ;

: EM-STARTUP-RUNTIME-STATE ( -- )
   LBL {: cwok :}
   9 0 MOVZ,  9 DATA HND-CELL STR,
   9 DATA SNAP-CELL LDR,
   9 cwok CBNZ,

   9 0 MOVZ,  9 DATA CUR-CELL STR,
   9 1 MOVZ,  9 DATA WIDN-CELL STR,
   9 0 MOVZ,  9 DATA HOOK-CELL STR,
   cwok LBL,
   9 0 MOVZ,  9 DATA LOOPSP-CELL STR,
   G-INSTALL-CRASH
   G-INSTALL-TRAP
   9 LDOESPATCH @ ADR,  9 DATA DOESP-CELL STR,
   9 LCREATE @ ADR,  9 DATA CREATEP-CELL STR,
   9 LRREC @ ADR,  9 DATA RRECP-CELL STR,
   9 LMAIN @ ADR,  9 DATA LMAINP-CELL STR,            \ interpret-loop top (B-EVAL branches here)
   LVRINIT @ BL,                                     \ fill VRTAB/VRITAB from VRPACK
   EMIT-SOURCE
   9 0 MOVZ,  9 DATA PEND-CELL STR,
   9 DATA TSIG-A-CELL STR,   9 DATA TSIG-U-CELL STR,
   9 DATA TCSIG-A-CELL STR,  9 DATA TCSIG-U-CELL STR,
   9 DATA CRSIG-A-CELL STR,  9 DATA CRSIG-U-CELL STR,
   9 DATA DOESB-CELL STR,
   9 DATA TRUSTED-CELL STR, ;

: EM-STARTUP ( -- )
   LANCHOR @ LBL,
   EM-ENTRY-ARGS
   EM-RUNTIME-STACK
   EM-MMAP-CODE-REGION
   EM-SEED-DICT
   EM-MMAP-DATA-REGION
   EM-DATA-INIT
   EM-SNAPSHOT-RESTORE
   EM-STARTUP-RUNTIME-STATE ;

: EM-COMMENT ( -- )
   LBL LBL LBL {: notcom skln skpar :}
   LMAIN @ LBL,
      LTOK @ BL,  0 LEXIT @ CBZ,
      9 DATA TKL-CELL LDR,  9 1 CMPI,  C-NE notcom BCOND,
      9 DATA TKA-CELL LDR,  9 9 0 LDRB,
      9 92 CMPI,  C-EQ skln BCOND,
      9 40 CMPI,  C-NE notcom BCOND,
      skpar LBL,  11 DATA INP-CELL LDR,  12 DATA INE-CELL LDR,  11 12 CMP,  C-GE LMAIN @ BCOND,
         9 11 0 LDRB,  11 11 1 ADDI,  11 DATA INP-CELL STR,  9 41 CMPI,  C-NE skpar BCOND,  LMAIN @ B,
      skln LBL,   11 DATA INP-CELL LDR,  12 DATA INE-CELL LDR,  11 12 CMP,  C-GE LMAIN @ BCOND,
         9 11 0 LDRB,  11 11 1 ADDI,  11 DATA INP-CELL STR,  9 10 CMPI,  C-NE skln BCOND,  LMAIN @ B,
      notcom LBL,
      9 DATA PEND-CELL LDR,  9 LCOMPILE @ CBNZ, ;

: EM-INTERPRET-COLON ( n -- ) {: lnotcolon :}
   LBL LBL {: cpok ndok :}
   9 DATA TKL-CELL LDR,  9 1 CMPI,  C-NE lnotcolon BCOND,
   9 DATA TKA-CELL LDR,  9 9 0 LDRB,  9 58 CMPI,  C-NE lnotcolon BCOND,
      2 3 MOVZ,  LPROT @ BL,
      9 REGION $4000 - LIT64,  9 DBASE 9 ADD,  CP 9 CMP,  C-LT cpok BCOND,
         0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
         0 76 MOVZ,  NR-EXIT SYS,
      cpok LBL,
      9 DICT-CAP MOVZ,  NDICT 9 CMP,  C-LT ndok BCOND,      \ slots end at CFSTK-OFF
         0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
         0 77 MOVZ,  NR-EXIT SYS,
      ndok LBL,
      LTOK @ BL,
      9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
      9 DATA PEND-CELL STR,
      C-STORE-NAME
      CP 9 0 STR,
      14 DATA CUR-CELL LDR,  14 9 40 STR,
      5 CFSTK-OFF LIT64,  11 DBASE 5 ADD,  12 0 MOVZ,  12 11 0 STR,
      12 0 MOVZ,  12 DATA LOCN-CELL STR,  12 DATA LOCF-CELL STR,
      12 0 MOVZ,  12 DATA BODYLEN-CELL STR,
      C-CLEAR-TRUSTED-STATE
      LBCAP @ BL,             \ seed with the NAME (checker records certified sigs)
      C-COLON-MAYBE-SIG
         12 0 MOVZ,  12 DATA VSP-CELL STR,  12 DATA SNAPSP-CELL STR,
         12 DATA EXITH-CELL STR,  12 DATA LVD-CELL STR,
         12 DATA QPATCH-CELL STR,
         12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,
         12 FRALL MOVZ,  12 DATA FRFREE-CELL STR,
         9 $D10043FF LIT64,  LCEMIT @ BL,
         9 $F90003FE LIT64,  LCEMIT @ BL,
         LMAIN @ B,
   lnotcolon LBL, ;
s" em-interpret-colon" s" n --" TRUST

: EM-INTERPRET-DEFINE-KEYWORDS ( -- )
   s" trusted:" KEEP? IF LMAIN @ LKWTRUSTED 8 ['] C-TRUSTED CF-ENTRY THEN
   s" create" KEEP? IF LMAIN @ LKWCREATE 6 ['] C-CREATE   CF-ENTRY THEN
   s" variable" KEEP? IF LMAIN @ LKWVAR    8 ['] C-VARIABLE CF-ENTRY THEN
   s" constant" KEEP? IF LMAIN @ LKWCONST  8 ['] C-CONSTANT CF-ENTRY THEN
   s" '" KEEP? IF LMAIN @ LKWTICK   1 ['] C-TICK     CF-ENTRY THEN
   s" char" KEEP? IF LMAIN @ LKWCHAR   4 ['] C-CHAR     CF-ENTRY THEN
   s" immediate" KEEP? IF LMAIN @ LKWIMM    9 ['] C-IMMEDIATE CF-ENTRY THEN ;
s" em-interpret-define-keywords" s" --" TRUST

: EM-INTERPRET-STRING-KEYWORDS ( -- )
   LMAIN @ LKWSQ     2 ['] C-ISDQ     CF-ENTRY
   LMAIN @ LKWCQ     2 ['] C-ICQ      CF-ENTRY
   LMAIN @ LKWDOTQ   2 ['] C-IDOTQ    CF-ENTRY ;
s" em-interpret-string-keywords" s" --" TRUST

: EM-INTERPRET-NUMBER ( n -- ) {: lnotnum :}
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LNUM @ BL,
   12 lnotnum CBZ,  11 G-PUSH  LMAIN @ B, ;
s" em-interpret-number" s" n --" TRUST

: EM-INTERPRET-FIND ( -- )
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND @ BL,
   13 LUNDEF @ CBZ,
   11 BLR,  LMAIN @ B, ;
s" em-interpret-find" s" --" TRUST

: EM-INTERPRET-WORDS ( -- )
   LBL {: lnotnum :}
   EM-INTERPRET-DEFINE-KEYWORDS
   EM-INTERPRET-STRING-KEYWORDS
   lnotnum EM-INTERPRET-NUMBER
   lnotnum LBL,
   EM-INTERPRET-FIND ;
s" em-interpret-words" s" --" TRUST

: EM-INTERPRET ( -- )
   LBL {: lnotcolon :}
   lnotcolon EM-INTERPRET-COLON
   EM-INTERPRET-WORDS ;
s" em-interpret" s" --" TRUST

: EM-COMPILE-DROP-LOCALS ( -- )
   LBL {: done :}
   12 DATA LOCF-CELL LDR,  12 done CBZ,
      9 $910003FF LIT64,  14 12 10 LSLI,  9 9 14 ORR,  LCEMIT @ BL,
   done LBL, ;
s" em-compile-drop-locals" s" --" TRUST

: EM-COMPILE-RET ( -- )
   9 $F94003FE LIT64,  LCEMIT @ BL,
   9 $910043FF LIT64,  LCEMIT @ BL,
   9 W-RET LIT64,  LCEMIT @ BL, ;
s" em-compile-ret" s" --" TRUST

: EM-COMPILE-FLUSH-PEND ( -- )
   11 DATA PEND-CELL LDR,
   9 11 0 LDR,  10 CP 9 SUB,  10 10 4 SUBI,  10 11 8 STR,
   2 5 MOVZ,  LPROT @ BL,  LFLUSH @ BL, ;
s" em-compile-flush-pend" s" --" TRUST

: EM-COMPILE-PUBLISH-TRUSTED ( -- )
   LBL LBL LBL {: ttrusted ndhas ndchk :}
   10 DATA TRUSTED-CELL LDR,  10 ttrusted CBNZ,
      C-CALL-CHECK-DEFINER
   ttrusted LBL,
   10 DATA TCSIG-U-CELL LDR,  10 ndhas CBNZ,
   10 DATA DOESB-CELL LDR,  10 ndchk CBZ,
      C-DIE-DOES
   ndhas LBL,
   10 DATA DOESB-CELL LDR,  10 ndchk CBZ,
      C-CALL-CHECK-DOES
   ndchk LBL,
   C-CALL-TRUST-PEND
   NDICT NDICT 1 ADDI,
   C-CLEAR-TRUSTED-STATE
   9 0 MOVZ,  9 DATA PEND-CELL STR,
   LMAIN @ B, ;
s" em-compile-publish-trusted" s" --" TRUST

: EM-COMPILE-PUBLISH-HOOKED ( -- )
   LBL LBL {: nohook rejected :}
   9 DATA HOOK-CELL LDR,  9 nohook CBZ,
      10 DATA BODYBUF-OFF ADDI,  10 G-PUSH
      10 DATA BODYLEN-CELL LDR,  10 G-PUSH
      SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
      10 G-POP  10 rejected CBZ,
   nohook LBL,
      NDICT NDICT 1 ADDI,
   rejected LBL,
   C-CLEAR-TRUSTED-STATE
   9 0 MOVZ,  9 DATA PEND-CELL STR,
   LMAIN @ B, ;
s" em-compile-publish-hooked" s" --" TRUST

: EM-COMPILE-PUBLISH ( -- )
   LBL {: hooked :}
   9 DATA TSIG-U-CELL LDR,  9 hooked CBZ,
      EM-COMPILE-PUBLISH-TRUSTED
   hooked LBL,
   EM-COMPILE-PUBLISH-HOOKED ;
s" em-compile-publish" s" --" TRUST

: EM-COMPILE-SEMI ( n -- ) {: lnotsemi :}
   9 DATA TKL-CELL LDR,  9 1 CMPI,  C-NE lnotsemi BCOND,
   9 DATA TKA-CELL LDR,  9 9 0 LDRB,  9 59 CMPI,  C-NE lnotsemi BCOND,
      LVSPILL @ BL,
      14 CP 0 ADDI,  9 DATA EXITH-CELL LDR,  LBCHAIN @ BL,
      EM-COMPILE-DROP-LOCALS
      EM-COMPILE-RET
      EM-COMPILE-FLUSH-PEND
      EM-COMPILE-PUBLISH
   lnotsemi LBL, ;
s" em-compile-semi" s" n --" TRUST

: EM-COMPILE-CONTROL-KEYWORDS ( -- )
   s" if" KEEP? IF LMAIN @ LKWIF     2 ['] J-IF   ['] J-IFR    CFB-ENTRY THEN
   s" then" KEEP? IF LMAIN @ LKWTHEN   4 ['] J-THEN   CF-ENTRY THEN
   s" else" KEEP? IF LMAIN @ LKWELSE   4 ['] J-ELSE   CF-ENTRY THEN
   s" begin" KEEP? IF LMAIN @ LKWBEGIN  5 ['] J-BEGIN  CFN-ENTRY THEN
   s" until" KEEP? IF LMAIN @ LKWUNTIL  5 ['] J-UNTIL ['] J-UNTILR CFBN-ENTRY THEN
   s" again" KEEP? IF LMAIN @ LKWAGAIN  5 ['] J-AGAIN  CFN-ENTRY THEN
   s" while" KEEP? IF LMAIN @ LKWWHILE  5 ['] J-WHILE ['] J-WHILER CFB-ENTRY THEN
   s" repeat" KEEP? IF LMAIN @ LKWREPEAT 6 ['] J-REPEAT CFN-ENTRY THEN ;
s" em-compile-control-keywords" s" --" TRUST

: EM-COMPILE-STRING-KEYWORDS ( -- )
   LMAIN @ LKWSQ     2 ['] C-SDQ    CF-ENTRY
   LMAIN @ LKWCQ     2 ['] C-CQ     CF-ENTRY
   LMAIN @ LKWDOTQ   2 ['] C-DOTQ   CF-ENTRY ;
s" em-compile-string-keywords" s" --" TRUST

: EM-COMPILE-META-KEYWORDS ( -- )
   s" [']" KEEP? IF LMAIN @ LKWBTICK  3 ['] C-BTICK  CF-ENTRY THEN
   s" [char]" KEEP? IF LMAIN @ LKWBCHAR  6 ['] C-BCHAR  CF-ENTRY THEN
   s" postpone" KEEP? IF LMAIN @ LKWPOST   8 ['] C-POSTPONE CF-ENTRY THEN
   s" does>" KEEP? IF LMAIN @ LKWDOES   5 ['] J-DOES     CF-ENTRY THEN
   s" [:" KEEP? IF LMAIN @ LKWQUOT   2 ['] J-QUOT     CF-ENTRY THEN
   s" ;]" KEEP? IF LMAIN @ LKWSEMIQ  2 ['] J-SEMIQUOT CF-ENTRY THEN ;
s" em-compile-meta-keywords" s" --" TRUST

: EM-COMPILE-LOOP-KEYWORDS ( -- )
   s" do" KEEP? IF LMAIN @ LKWDO     2 ['] J-DO     CF-ENTRY THEN
   s" loop" KEEP? IF LMAIN @ LKWLOOP   4 ['] J-LOOP   CF-ENTRY THEN
   s" i" KEEP? IF LMAIN @ LKWI      1 ['] J-I      CF-ENTRY THEN
   s" >r" KEEP? IF LMAIN @ LKWTOR    2 ['] J-TOR    CF-ENTRY THEN
   s" r>" KEEP? IF LMAIN @ LKWRFROM  2 ['] J-RFROM  CF-ENTRY THEN
   s" r@" KEEP? IF LMAIN @ LKWRFET   2 ['] J-RFETCH CF-ENTRY THEN
   s" exit" KEEP? IF LMAIN @ LKWEXIT   4 ['] J-EXIT    CF-ENTRY THEN
   s" recurse" KEEP? IF LMAIN @ LKWREC    7 ['] J-RECURSE CF-ENTRY THEN
   s" ?do" KEEP? IF LMAIN @ LKWQDO    3 ['] J-?DO     CF-ENTRY THEN
   s" +loop" KEEP? IF LMAIN @ LKWPLOOP  5 ['] J-+LOOP   CF-ENTRY THEN
   s" j" KEEP? IF LMAIN @ LKWJ      1 ['] J-J       CF-ENTRY THEN
   s" leave" KEEP? IF LMAIN @ LKWLEAVE  5 ['] J-LEAVE   CF-ENTRY THEN
   s" unloop" KEEP? IF LMAIN @ LKWUNLOOP 6 ['] J-UNLOOP  CF-ENTRY THEN
   s" {:" KEEP? IF LMAIN @ LKWLBRACE 2 ['] C-LBRACE CF-ENTRY THEN ;
s" em-compile-loop-keywords" s" --" TRUST

: EM-COMPILE-KEYWORDS ( -- )
   LBCAP @ BL,
   EM-COMPILE-CONTROL-KEYWORDS
   EM-COMPILE-STRING-KEYWORDS
   EM-COMPILE-META-KEYWORDS
   EM-COMPILE-LOOP-KEYWORDS ;
s" em-compile-keywords" s" --" TRUST

: EM-COMPILE-LOCAL ( -- )
   LBL {: notloc :}
   LMAIN @ notloc C-LOCAL-REF
   notloc LBL, ;
s" em-compile-local" s" --" TRUST

: EM-COMPILE-LITERAL ( -- )
   LBL LBL {: lcnotnum lcflt :}
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LNUM @ BL,
   12 lcnotnum CBZ,
   2 lcflt CBNZ,
      LVPUSHC @ BL,  LMAIN @ B,
   lcflt LBL,
      LVPUSHF @ BL,  LMAIN @ B,
   lcnotnum LBL, ;
s" em-compile-literal" s" --" TRUST

: EM-COMPILE-ARITH-OPS ( -- )
   s" +" KEEP? IF LMAIN @ LKWPLUS  1 ['] VF+ ['] E+ ['] EI+ VOPI-ENTRY THEN
   s" -" KEEP? IF LMAIN @ LKWMINUS 1 ['] VF- ['] E- ['] EI- VOPI-ENTRY THEN
   s" *" KEEP? IF LMAIN @ LKWSTAR  1 ['] VF* ['] E* VOP-ENTRY THEN
   s" and" KEEP? IF LMAIN @ LKWAND2  3 ['] FAND ['] EAND VOP-ENTRY THEN
   s" or" KEEP? IF LMAIN @ LKWOR2   2 ['] FOR2 ['] EOR2 VOP-ENTRY THEN
   s" xor" KEEP? IF LMAIN @ LKWXOR2  3 ['] FXOR2 ['] EXOR VOP-ENTRY THEN ;
s" em-compile-arith-ops" s" --" TRUST

: EM-COMPILE-SHUFFLE-OPS ( -- )
   s" dup" KEEP? IF LMAIN @ LKWDUP2  3 1 ['] XDUP  VSHUF-ENTRY THEN
   s" drop" KEEP? IF LMAIN @ LKWDROP2 4 1 ['] XDROP VSHUF-ENTRY THEN
   s" swap" KEEP? IF LMAIN @ LKWSWAP2 4 2 ['] XSWAP VSHUF-ENTRY THEN
   s" over" KEEP? IF LMAIN @ LKWOVER2 4 2 ['] XOVER VSHUF-ENTRY THEN
   s" nip" KEEP? IF LMAIN @ LKWNIP2  3 2 ['] XNIP  VSHUF-ENTRY THEN ;
s" em-compile-shuffle-ops" s" --" TRUST

: EM-COMPILE-COMPARE-OPS ( -- )
   s" =" KEEP? IF LMAIN @ LKWEQ2 1 0 VCMP-ENTRY THEN
   s" <>" KEEP? IF LMAIN @ LKWNE2 2 1 VCMP-ENTRY THEN
   s" <" KEEP? IF LMAIN @ LKWLT2 1 11 VCMP-ENTRY THEN
   s" >" KEEP? IF LMAIN @ LKWGT2 1 12 VCMP-ENTRY THEN
   s" <=" KEEP? IF LMAIN @ LKWLE2 2 13 VCMP-ENTRY THEN
   s" >=" KEEP? IF LMAIN @ LKWGE2 2 10 VCMP-ENTRY THEN ;
s" em-compile-compare-ops" s" --" TRUST

: EM-COMPILE-UNARY-OPS ( -- )
   s" 1+" KEEP? IF LMAIN @ LKWINC  2 ['] FU1+ ['] EU1+ VUN-ENTRY THEN
   s" 1-" KEEP? IF LMAIN @ LKWDEC  2 ['] FU1- ['] EU1- VUN-ENTRY THEN
   s" 0=" KEEP? IF LMAIN @ LKWZEQ  2 ['] FU0= ['] EU0= VUN-ENTRY THEN
   s" 0<" KEEP? IF LMAIN @ LKWZLT  2 ['] FU0< ['] EU0< VUN-ENTRY THEN
   s" negate" KEEP? IF LMAIN @ LKWNEG2 6 ['] FUNEG ['] EUNEG VUN-ENTRY THEN
   s" invert" KEEP? IF LMAIN @ LKWINV2 6 ['] FUINV ['] EUINV VUN-ENTRY THEN ;
s" em-compile-unary-ops" s" --" TRUST

: EM-COMPILE-FLOAT-OPS ( -- )
   s" f+" KEEP? IF LMAIN @ LKWFPLUS  2 $1E602800 FOP-ENTRY THEN
   s" f-" KEEP? IF LMAIN @ LKWFMINUS 2 $1E603800 FOP-ENTRY THEN
   s" f*" KEEP? IF LMAIN @ LKWFSTAR  2 $1E600800 FOP-ENTRY THEN
   s" f/" KEEP? IF LMAIN @ LKWFSLASH 2 $1E601800 FOP-ENTRY THEN ;
s" em-compile-float-ops" s" --" TRUST

: EM-COMPILE-OPS ( -- )
   EM-COMPILE-ARITH-OPS
   EM-COMPILE-SHUFFLE-OPS
   EM-COMPILE-COMPARE-OPS
   EM-COMPILE-UNARY-OPS
   EM-COMPILE-FLOAT-OPS ;
s" em-compile-ops" s" --" TRUST

: EM-COMPILE-CALL ( -- )
   LBL {: notimm :}
   LVSPILL @ BL,
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND @ BL,
   13 LUNDEF @ CBZ,
   14 13 2 ANDI,  14 notimm CBZ,
      SP SP 16 SUBI,  30 SP 0 STR,  11 SP 8 STR,
      2 5 MOVZ,  LPROT @ BL,
      11 SP 8 LDR,  11 BLR,
      2 3 MOVZ,  LPROT @ BL,
      30 SP 0 LDR,  SP SP 16 ADDI,
      LMAIN @ B,
   notimm LBL,
   C-CALL  LMAIN @ B, ;
s" em-compile-call" s" --" TRUST

: EM-RESET-COMPILE-STATE ( -- )
   9 0 MOVZ,
   9 DATA RSP-CELL STR,  9 DATA HND-CELL STR,  9 DATA LOOPSP-CELL STR,
   9 DATA LVD-CELL STR,  9 DATA VSP-CELL STR,  9 DATA QPATCH-CELL STR,
   9 DATA LOCN-CELL STR,  9 DATA BODYLEN-CELL STR,  9 DATA EXITH-CELL STR,
   9 DATA PEND-CELL STR,
   9 0 MOVZ,
   9 DATA TSIG-A-CELL STR,   9 DATA TSIG-U-CELL STR,
   9 DATA TCSIG-A-CELL STR,  9 DATA TCSIG-U-CELL STR,
   9 DATA CRSIG-A-CELL STR,  9 DATA CRSIG-U-CELL STR,
   9 DATA DOESB-CELL STR,
   9 DATA TRUSTED-CELL STR,
   9 VRALL MOVZ,  9 DATA VRFREE-CELL STR, ;
s" em-reset-compile-state" s" --" TRUST

: EM-EVAL-UNDEF-ROLLBACK ( -- )
   14 EVAL-FRAME LIT64,  14 DATA 14 ADD,
   9 DATA EVALD-CELL LDR,  9 9 1 SUBI,  9 DATA EVALD-CELL STR,
   CP 14 40 LDR,  NDICT 14 48 LDR,  XDS 14 32 LDR,
   9 14 56 LDR,  9 DATA DP-CELL STR,
   EM-RESET-COMPILE-STATE
   9 14 0 LDR,  9 DATA INP-CELL STR,
   9 14 8 LDR,  9 DATA INE-CELL STR,
   9 1 MOVZ,  9 DATA EVALERR-CELL STR,
   9 14 24 LDR,  SP 9 0 ADDI,
   9 14 16 LDR,  9 BR, ;
s" em-eval-undef-rollback" s" --" TRUST

: EM-REPL-RECOVER ( -- )
   LRREC @ LBL,
   0 2 MOVZ,  1 LQNL @ ADR,  2 2 MOVZ,  NR-WRITE SYS,
   CP DATA RSAVCP-CELL LDR,
   NDICT DATA RSAVND-CELL LDR,
   9 DATA RSAVDP-CELL LDR,  9 DATA DP-CELL STR,
   9 DATA S0-CELL LDR,  XDS 9 0 ADDI,
   EM-RESET-COMPILE-STATE
   9 DATA RSAVSP-CELL LDR,  SP 9 0 ADDI,
   LREAD @ B, ;
s" em-repl-recover" s" --" TRUST

: EM-COMPILE-UNDEF ( -- )
   LUNDEF @ LBL,
   0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
   9 DATA EVALD-CELL LDR,  9 LUN0 @ CBZ,
      EM-EVAL-UNDEF-ROLLBACK
   LUN0 @ LBL,
   9 DATA REPLH-CELL LDR,  9 LRDIE @ CBZ,
   EM-REPL-RECOVER
   LRDIE @ LBL,
   0 70 MOVZ,  NR-EXIT SYS, ;
s" em-compile-undef" s" --" TRUST

: EM-EVAL-CLEAN-EXIT ( -- )
   14 EVAL-FRAME LIT64,  14 DATA 14 ADD,
   9 DATA EVALD-CELL LDR,  9 9 1 SUBI,  9 DATA EVALD-CELL STR,
   9 14 0 LDR,  9 DATA INP-CELL STR,
   9 14 8 LDR,  9 DATA INE-CELL STR,
   9 0 MOVZ,  9 DATA EVALERR-CELL STR,
   9 14 24 LDR,  SP 9 0 ADDI,
   9 14 16 LDR,  9 BR, ;
s" em-eval-clean-exit" s" --" TRUST

: EM-REPL-READ ( -- )
   LREAD @ LBL,
   9 SP 0 ADDI,  9 DATA RSAVSP-CELL STR,
   CP DATA RSAVCP-CELL STR,
   NDICT DATA RSAVND-CELL STR,
   9 DATA DP-CELL LDR,  9 DATA RSAVDP-CELL STR,
   9 DATA REPLH-CELL LDR,  9 BLR,
   XDS XDS 8 SUBI,  10 XDS 0 LDR,
   XDS XDS 8 SUBI,  11 XDS 0 LDR,
   10 LRBYE @ CBZ,
   11 DATA INP-CELL STR,  11 11 10 ADD,  11 DATA INE-CELL STR,  LMAIN @ B, ;
s" em-repl-read" s" --" TRUST

: EM-COMPILE-EXIT ( -- )
   LEXIT @ LBL,
   9 DATA EVALD-CELL LDR,  9 LEX0 @ CBZ,
      EM-EVAL-CLEAN-EXIT
   LEX0 @ LBL,
   9 DATA REPLH-CELL LDR,  9 LRBYE @ CBZ,
   0 1 MOVZ,  1 LOKS @ ADR,  2 4 MOVZ,  NR-WRITE SYS,
   EM-REPL-READ
   LRBYE @ LBL,
   0 0 MOVZ,  NR-EXIT SYS, ;
s" em-compile-exit" s" --" TRUST

: EM-COMPILE ( -- )
   LBL {: lnotsemi :}
   LCOMPILE @ LBL,
   lnotsemi EM-COMPILE-SEMI
   EM-COMPILE-KEYWORDS
   EM-COMPILE-LOCAL
   EM-COMPILE-LITERAL
   EM-COMPILE-OPS
   EM-COMPILE-CALL
   EM-COMPILE-UNDEF
   EM-COMPILE-EXIT ;
s" em-compile" s" --" TRUST

: EMIT-MAIN
   LBL LMAIN !  LBL LEXIT !  LBL LCOMPILE !  LBL LUNDEF !
   EM-STARTUP  EM-COMMENT  EM-INTERPRET  EM-COMPILE ;
s" emit-main" s" --" TRUST
variable SRCA
: SRCA@ SRCA @ ;
s" SRCA@" s" -- ptr u8" TRUST

: EMIT-RESET-BUILDER ( ptr u8 n -- )
   SRCN !  SRCA !
   ASM-INIT  0 #PL !  0 PNP ! ;

: EMIT-LABEL-CORE ( -- )
   LBL LANCHOR !  LBL LFIND !  LBL LNUM !  LBL LDICT !  LBL LSRC !
   LBL LCEMIT !  LBL LTOK !  LBL LPROT !  LBL LFLUSH !  LBL LNCOUNT !
   LBL LBCAP !  LBL LBCS !
   LBL LCFPUSH !  LBL LCFPOP !  LBL LPAT !  LBL LKWCMP ! ;

: EMIT-LABEL-CONTROL ( -- )
   LBL LKWIF !  LBL LKWTHEN !  LBL LKWELSE !  LBL LKWBEGIN !
   LBL LKWUNTIL !  LBL LKWAGAIN !  LBL LKWWHILE !  LBL LKWREPEAT !
   LBL LKWCREATE !  LBL LKWVAR !  LBL LKWSQ !  LBL LKWCQ !  LBL LKWDOTQ !
   LBL LKWTYPE !
   LBL LKWTICK !  LBL LKWBTICK !
   LBL LKWLBRACE !  LBL LKWENDLOC !  LBL LLOC-FIND !  LBL LKWCONST !
   LBL LKWDO !  LBL LKWLOOP !  LBL LKWI !
   LBL LKWTOR !  LBL LKWRFROM !  LBL LKWRFET !
   LBL LKWEXIT !  LBL LKWREC !
   LBL LKWQDO !  LBL LKWPLOOP !  LBL LKWJ !  LBL LKWLEAVE !  LBL LKWUNLOOP !
   LBL LKWCHAR !  LBL LKWBCHAR !
   LBL LKWIMM !  LBL LKWPOST !  LBL LKWCOMPC !  LBL LKWDOES !
   LBL LKWTRUSTED !  LBL LKWTRUST !  LBL LKWCHKDOES !
   LBL LKWQUOT !  LBL LKWSEMIQ ! ;

: EMIT-LABEL-RUNTIME ( -- )
   LBL LBCHAIN !  LBL LCREATE !  LBL LDOESPATCH !
   LBL LREAD !  LBL LRBYE !  LBL LRDIE !  LBL LRREC !  LBL LQNL !  LBL LOKS !
   LBL LEX0 !  LBL LUN0 !
   LBL LCRASHH !  LBL LHEX !  LBL LHDR !  LBL LTRAPH !  LBL LBPH !  LBL LBPSH !  LBL LBPWH !
   LBL LSRCRD !  LBL LSHBANG ! ;

: EMIT-LABEL-SOURCES ( -- )
   LBL LPLINUXTARGET !  LBL LPMACOSTARGET !
   LBL LPUTIL !  LBL LPCHECKER !  LBL LPRENDER !  LBL LPHOOK !  LBL LPHABULAYOUT !
   LBL LPLINUXENV !  LBL LPMACOSENV !  LBL LPROLES !  LBL LPCOMBINATORS ! ;

: EMIT-LABEL-JIT ( -- )
   LBL LPROFH !  LBL LPROFDUMP !
   LBL LVSPILL !  LBL LVLITPUSH !  LBL LVPUSHC !
   LBL LVTOP2C !  LBL LVFOLDPUT !
   LBL LVRALLOC !  LBL LVBIT !  LBL LVRINIT !  LBL LVMOVK !  LBL LVFORCEK !  LBL LVBINPREP !  LBL LVBINIPREP !  LBL LVPUSHR !
   LBL LVPUSHF !  LBL LFRALLOC !  LBL LFFORCEK !  LBL LFBINPREP !
   LBL LKWFPLUS !  LBL LKWFMINUS !  LBL LKWFSTAR !  LBL LKWFSLASH !
   LBL LVDROP !  LBL LVSWAPX !  LBL LVNIPX !  LBL LVCOPY !
   LBL LVSNAP !  LBL LVRECON ! ;

: EMIT-LABEL-OPS ( -- )
   LBL LKWPLUS !  LBL LKWMINUS !  LBL LKWSTAR !
   LBL LKWAND2 !  LBL LKWOR2 !  LBL LKWXOR2 !
   LBL LKWDUP2 !  LBL LKWDROP2 !  LBL LKWSWAP2 !
   LBL LKWOVER2 !  LBL LKWNIP2 !
   LBL LKWEQ2 !  LBL LKWNE2 !  LBL LKWLT2 !
   LBL LKWGT2 !  LBL LKWLE2 !  LBL LKWGE2 !
   LBL LKWINC !  LBL LKWDEC !  LBL LKWZEQ !
   LBL LKWZLT !  LBL LKWNEG2 !  LBL LKWINV2 ! ;

: EMIT-LABELS ( -- )
   EMIT-LABEL-CORE
   EMIT-LABEL-CONTROL
   EMIT-LABEL-RUNTIME
   EMIT-LABEL-SOURCES
   EMIT-LABEL-JIT
   EMIT-LABEL-OPS ;

: EMIT-PRIMITIVE-SECTIONS ( -- )
   EMIT-PRIMS
   EMIT-PROF-PRIMS
   EMIT-FP-PRIMS
   EMIT-CEMIT
   EMIT-BCAP
   EMIT-TOK
   EMIT-PROT
   EMIT-FLUSH
   EMIT-FIND
   EMIT-NUM ;

: EMIT-DICTIONARY-SECTIONS ( -- )
   EMIT-CREATE
   EMIT-DOESPATCH
   EMIT-CF-HELPERS
   EMIT-LOC-FIND
   EMIT-KWDATA
   EMIT-FOLDKW
   EMIT-SHUFKW
   EMIT-CMPKW
   EMIT-UNKW ;

: EMIT-RUNTIME-SECTIONS ( -- )
   EMIT-CRASH-HANDLER
   EMIT-TRAPH
   EMIT-HEX
   EMIT-PROFDUMP
   EMIT-PROF
   EMIT-SHEBANG-COMMENT
   EMIT-SOURCE-READ
   EMIT-JIT ;

: EMIT-CODE-SECTIONS ( -- )
   EMIT-MAIN
   EMIT-PRIMITIVE-SECTIONS
   EMIT-DICTIONARY-SECTIONS
   EMIT-RUNTIME-SECTIONS
   EMIT-DICT ;

: EMIT-SOURCE-BYTES ( -- )
   LSRC @ LBL,  SRCA@ SRCN @ BYTES, ;

: EMIT-FORTH ( ptr u8 n -- )
   EMIT-RESET-BUILDER
   EMIT-LABELS
   EMIT-CODE-SECTIONS
   EMIT-SOURCE-BYTES ;
s" emit-forth" s" ptr u8 n --" TRUST
