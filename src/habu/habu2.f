\ habu2.f — engine-builder port, part 2 (from bootstrap/cg/forth.fs): the JIT compiler
\ emitters (literal/call/keywords/locals/strings/do-loop), the outer-interpreter
\ main loop, and EMIT-FORTH. Needs habu1.f (part 1). EMIT-MAIN is split into
\ phase words sharing label VARIABLES (a giant single word would need dozens of
\ locals); emission ORDER is exactly src/cg's, so the output is byte-identical.
\ ---- compile-mode literal: emit movz/movk x9=val then the push stencil ----
: C-LIT
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
   9 W-PUSH0 LIT64,  LCEMIT @ BL,  9 W-PUSH1 LIT64,  LCEMIT @ BL, ;
\ ---- compile-mode CALL-or-INLINE (x11=target addr, x12=clen from FIND) ----
$28 constant INL-MAX

: C-CALL
   LBL LBL LBL LBL LBL LBL LBL {: lcall lcopy lscan lsbody lnopro linl ldone :}
   9 11 0 LDRW,  8 $D10043FF LIT64,  9 8 CMP,  C-NE lnopro BCOND,
      12 INL-MAX 16 + CMPI,  C-GT lcall BCOND,
      13 11 8 ADDI,  14 11 12 ADD,  14 14 8 SUBI,  lscan B,
   lnopro LBL,
      12 INL-MAX CMPI,  C-GT lcall BCOND,
      13 11 0 ADDI,  14 11 12 ADD,
      9 14 0 LDRW,  8 $D65F03C0 LIT64,  9 8 CMP,  C-NE lcall BCOND,   \ ret slot patched
                                                               \ (does>) -> never inline
   lscan LBL,
      15 13 0 ADDI,
   lsbody LBL,  15 14 CMP,  C-GE lcopy BCOND,
      9 15 0 LDRW,  15 15 4 ADDI,
      8 $FC000000 LIT64,  10 9 8 AND,  8 $94000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
      8 $FC000000 LIT64,  10 9 8 AND,  8 $14000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
      8 $FF000010 LIT64,  10 9 8 AND,  8 $54000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
      8 $7E000000 LIT64,  10 9 8 AND,  8 $34000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
      8 $7E000000 LIT64,  10 9 8 AND,  8 $36000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
      8 $FFFFFC1F LIT64,  10 9 8 AND,
         8 $D63F0000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
         8 $D61F0000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
      8 $D65F03C0 LIT64,  9 8 CMP,  C-EQ lcall BCOND,
      8 $1F000000 LIT64,  10 9 8 AND,  8 $10000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
      lsbody B,
   lcopy LBL,
      15 13 0 ADDI,
   linl LBL,  15 14 CMP,  C-GE ldone BCOND,
      9 15 0 LDRW,  15 15 4 ADDI,  LCEMIT @ BL,  linl B,
   lcall LBL,
      5 $FFFF MOVZ,
      7 11 5 AND,    7 7 5 LSLI,  8 $D2800010 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
      7 11 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2A00010 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
      7 11 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2C00010 LIT64,  9 8 7 ORR,  LCEMIT @ BL,
      9 $D63F0200 LIT64,  LCEMIT @ BL,
   ldone LBL, ;

\ ---- source setup: baked LSRC or stdin ----
variable LTRAPH   variable LBPH
create BPH-KW 104 c, 97 c, 98 c, 117 c, 45 c, 98 c, 112 c, 58 c, 10 c,   \ habu-bp:\n

\ LTRAPH: SIGTRAP entry (x1=infostyle x2=sig x4=ucontext). A one-shot
\ breakpoint at [BPA-CELL]: print habu-bp: + pc + the data-stack top, restore
\ the original instruction, clear the bp, sigreturn to re-execute the word.
\ Any other trap falls through to the crash dump (x2/x4 untouched).
: EMIT-TRAPH
   LTRAPH @ LBL,
   LBL {: tno :}
   9 4 MCTX-OFF LDR,                                 \ x9 = mcontext
   10 9 272 LDR,                                     \ x10 = pc
   LBL {: bscan :}  LBL {: bnext :}  LBL {: bhit :}
   LBL {: emu :}  LBL {: fin :}
   6 8 MOVZ,  7 0 MOVZ,                              \ MAXBP=8, i  (scan BPTAB[0..8))
   bscan LBL,
      7 6 CMP,  C-GE tno BCOND,
      8 7 5 LSLI,  14 BPTAB-OFF LIT64,  8 8 14 ADD,  8 DATA 8 ADD,   \ &BPTAB[i] (32 B stride)
      13 8 0 LDR,  13 bnext CBZ,                     \ empty slot (addr 0)
      10 13 CMP,  C-EQ bhit BCOND,
      bnext LBL,  7 7 1 ADDI,  bscan B,
   \ slot layout: +0 addr  +8 saved-instr  +16 hits  +24 ctrl(skip<<1 | persist)
   bhit LBL,                                         \ x8=&slot x9=mctx x10=pc
   SP SP 48 SUBI,
   1 SP 0 STR,  4 SP 8 STR,  5 SP 16 STR,  9 SP 24 STR,  10 SP 32 STR,  8 SP 40 STR,
   14 8 16 LDR,  14 14 1 ADDI,  14 8 16 STR,         \ hits++
   15 8 24 LDR,  12 15 1 LSRI,                       \ x15=ctrl  x12=skip
   14 12 CMP,  C-LS emu BCOND,                       \ hits <= skip -> silent, just emulate
   1 LBPH @ ADR,  0 2 MOVZ,  2 9 MOVZ,  NR-WRITE SYS,   \ "habu-bp:"
   9 SP 32 LDR,  LHEX @ BL,                          \ pc
   9 SP 24 LDR,  12 9 168 LDR,  9 12 8 SUBI,  9 9 0 LDR,  LHEX @ BL,   \ [x19-8] = tos
   8 SP 40 LDR,  15 8 24 LDR,  15 15 1 ANDI,  15 emu CBNZ,   \ persistent -> emulate, keep BRK
   2 3 MOVZ,  LPROT @ BL,                            \ one-shot: restore + remove
   8 SP 40 LDR,  11 8 0 LDR,  12 8 8 LDR,  12 11 0 STRW,
   2 5 MOVZ,  LPROT @ BL,
   9 11 0 ADDI,  LFLUSH @ BL,
   8 SP 40 LDR,  12 0 MOVZ,  12 8 0 STR,             \ clear slot addr (resume re-runs orig)
   fin B,
   emu LBL,                                          \ emulate the entry prologue, keep BRK:
   9 SP 24 LDR,                                      \ mctx
   12 9 264 LDR,  12 12 16 SUBI,  12 9 264 STR,      \ sp -= 16  (sub sp,sp,#16)
   12 9 272 LDR,  12 12 4 ADDI,  12 9 272 STR,       \ pc += 4   (skip the BRK)
   fin LBL,
   0 SP 8 LDR,  1 SP 0 LDR,  2 SP 16 LDR,  SP SP 48 ADDI,
   NR-SIGRETURN SYS,                                 \ sigreturn(uctx, infostyle, token)
   tno LBL,
   LCRASHH @ B,
   LBPH @ LBL,  BPH-KW 9 BYTES, ;

\ override SIGTRAP(5) to the resuming handler (G-INSTALL-CRASH pointed all four
\ at the dumper; this repoints just TRAP once LTRAPH is bound).
: G-INSTALL-TRAP
   SP SP 32 SUBI,
   9 LTRAPH @ ADR,  9 SP 0 STR,  9 SP 8 STR,
   10 SA-SIGINFO MOVZ,  10 10 32 LSLI,  10 SP 16 STR,
   5 (SIGACT)
   SP SP 32 ADDI, ;

: EMIT-SOURCE
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL {: tty file sdone sfail srl serr rl RD pipeok repl done :}   \ locals BEFORE the IF (frame footgun)
   STDIN? @ IF
      0 0 MOVZ,  1 $40487413 LIT64,  2 DATA BODYBUF-OFF ADDI,  NR-IOCTL SYS,
      0 tty CBZ,
      0 0 MOVZ,  1 IBUFSZ LIT64,  2 3 MOVZ,  3 $1002 LIT64,  4 0 MOVN,  5 0 MOVZ,
      NR-MMAP SYS,
      13 C-CS CSET,  13 sfail CBNZ,
      11 0 0 ADDI,  9 0 0 ADDI,
      rl LBL,
         0 0 MOVZ,  1 9 0 ADDI,
         2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  2 2 9 SUB,
         2 RD CBZ,
         NR-READ SYS,
         13 C-CS CSET,  13 sfail CBNZ,
         0 RD CBZ,
         9 9 0 ADD,  rl B,
      RD LBL,
      9 11 CMP,  C-NE pipeok BCOND,
      10 DATA ARGC-CELL LDR,  10 1 CMPI,  C-GT file BCOND,
      pipeok LBL,
      11 DATA INP-CELL STR,  9 DATA INE-CELL STR,  done B,
      tty LBL,
      9 DATA ARGC-CELL LDR,  9 1 CMPI,  C-LE repl BCOND,
      0 0 MOVZ,  1 IBUFSZ LIT64,  2 3 MOVZ,  3 $1002 LIT64,  4 0 MOVN,  5 0 MOVZ,
      NR-MMAP SYS,
      13 C-CS CSET,  13 sfail CBNZ,
      11 0 0 ADDI,
      file LBL,
      9 11 0 ADDI,
      12 DATA ARGV-CELL LDR,  12 12 8 LDR,
      0 12 0 ADDI,  1 0 MOVZ,  2 0 MOVZ,  NR-OPEN SYS,
      13 C-CS CSET,  13 sfail CBNZ,
      12 0 0 ADDI,
      srl LBL,
         0 12 0 ADDI,  1 9 0 ADDI,
         2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  2 2 9 SUB,
         2 sdone CBZ,
         NR-READ SYS,
         13 C-CS CSET,  13 serr CBNZ,
         0 sdone CBZ,
         9 9 0 ADD,  srl B,
      sdone LBL,
      0 12 0 ADDI,  NR-CLOSE SYS,
      11 DATA INP-CELL STR,  9 DATA INE-CELL STR,  done B,
      serr LBL,  0 12 0 ADDI,  NR-CLOSE SYS,
      sfail LBL,  0 74 MOVZ,  NR-EXIT SYS,
      repl LBL,
      11 LSRC @ ADR,  11 DATA INP-CELL STR,  5 SRCN @ LIT64,  11 11 5 ADD,  11 DATA INE-CELL STR,  done B,
      done LBL,
   ELSE
      11 LSRC @ ADR,  11 DATA INP-CELL STR,  5 SRCN @ LIT64,  11 11 5 ADD,  11 DATA INE-CELL STR,
   THEN ;

\ ---- control-flow JIT helpers ----
: EMIT-CF-HELPERS
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

: EMIT-LOC-FIND
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

: EMIT-KWDATA
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
   LKWQUOT @ LBL,  QUOT-KW 2 BYTES,   LKWSEMIQ @ LBL,  SEMIQ-KW 2 BYTES, ;

\ ---- compile-time keyword handlers (append JIT-emitter code at BUILD time) ----
: C-EMITW {: w :}  9 w LIT64,  LCEMIT @ BL, ;

: C-POPFLAG  $D1002273 C-EMITW  $F9400269 C-EMITW ;

: C-PUSHCP   9 CP 0 ADDI,  LCFPUSH @ BL, ;

: C-BBACK {: opc mask :}
   10 9 CP SUB,  10 10 2 ASRI,  5 mask LIT64,  10 10 5 AND,  9 opc LIT64,  9 9 10 ORR,  LCEMIT @ BL, ;

: J-IF    C-POPFLAG  C-PUSHCP  $B4000009 C-EMITW ;

: J-THEN  LCFPOP @ BL,  LPAT @ BL, ;

: J-ELSE  LCFPOP @ BL,  14 9 0 ADDI,  C-PUSHCP  $14000000 C-EMITW  9 14 0 ADDI,  LPAT @ BL, ;

\ BEGIN loops are register-resident: J-BEGIN snapshots the VS into registers
\ (Lvsnap), the back edges reconcile to that snapshot (Lvrecon) and branch on
\ x17 — never a VS register, so the reconcile reload can't clobber the flag.
: J-BEGIN  LVSNAP @ BL,  C-PUSHCP ;

: J-AGAIN  LVRECON @ BL,  LCFPOP @ BL,  $14000000 $3FFFFFF C-BBACK ;

: J-UNTILX                                 \ shared tail: reconcile + cbz x17,top
   LVRECON @ BL,
   LCFPOP @ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $B4000011 LIT64,  9 9 10 ORR,  LCEMIT @ BL, ;

: J-UNTIL  $D1002273 C-EMITW  $F9400271 C-EMITW  J-UNTILX ;   \ pop flag -> x17

: J-WHILE C-POPFLAG  C-PUSHCP  $B4000009 C-EMITW ;

: J-REPEAT LVRECON @ BL,  LCFPOP @ BL,  14 9 0 ADDI,  LCFPOP @ BL,  $14000000 $3FFFFFF C-BBACK
   12 0 MOVZ,  12 DATA VSP-CELL STR,                  \ exit path arrives from
   12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,           \ WHILE's spilled state
   12 FRALL MOVZ,  12 DATA FRFREE-CELL STR,
   9 14 0 ADDI,  LPAT @ BL, ;

: J-FRAME                                \ pop limit/start, push a loop frame
   3506446963 C-EMITW  4181721705 C-EMITW  3506446963 C-EMITW  4181721706 C-EMITW
   4181780107 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4177527177 C-EMITW  4177528202 C-EMITW  2432697707 C-EMITW  4177585803 C-EMITW ;

: J-LVOPEN                               \ open a LEAVE-chain level: LVH[LVD]=0, LVD++
   9 DATA LVD-CELL LDR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   12 0 MOVZ,  12 10 0 STR,
   9 9 1 ADDI,  9 DATA LVD-CELL STR, ;

: J-LVLEAVE                              \ chain a B placeholder on the current level
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   9 10 0 LDR,
   11 CP DBASE SUB,  11 10 0 STR,
   LCEMIT @ BL, ;

: J-DO
   J-FRAME  J-LVOPEN  C-PUSHCP ;

: J-?DO                                  \ DO, but skip the loop when limit = start
   J-FRAME  J-LVOPEN
   $EB0A013F C-EMITW                     \ cmp x9,x10  (start/limit still live)
   $54000041 C-EMITW                     \ b.ne +8 (over the skip placeholder)
   J-LVLEAVE
   C-PUSHCP ;

: J-LEAVE  J-LVLEAVE ;

: J-UNLOOP                               \ pop one loop frame, no branch
   4181780107 C-EMITW  3506439531 C-EMITW  4177585803 C-EMITW ;

: J-LOOPEND                              \ shared LOOP/+LOOP tail: pop frame, patch
   14 CP 0 ADDI,                         \ LEAVE/?DO skips to the pop point, LVD--
   4181780107 C-EMITW  3506439531 C-EMITW  4177585803 C-EMITW
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,  9 DATA LVD-CELL STR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,  9 10 0 LDR,
   LBCHAIN @ BL, ;

: J-LOOP
   4181780107 C-EMITW  3506439531 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4181722506 C-EMITW  2432697641 C-EMITW  4177527177 C-EMITW  3943301439 C-EMITW
   LCFPOP @ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000B LIT64,  9 9 10 ORR,  LCEMIT @ BL,
   J-LOOPEND ;

: J-+LOOP                                \ index += n; loop while (old-limit) and
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

: J-I
   4181780107 C-EMITW  3506439531 C-EMITW  3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4177527401 C-EMITW  2432705139 C-EMITW ;

: J-J                                    \ outer loop index: frame[LOOPSP-2]
   4181780107 C-EMITW  $D100096B C-EMITW 3548179820 C-EMITW  2434269580 C-EMITW  2333344140 C-EMITW
   4181721481 C-EMITW  4177527401 C-EMITW  2432705139 C-EMITW ;

\ >R R> R@ — the user return stack lives in a data-region stack ([x20+RSTK-OFF],
\ depth at [x20+RSP-CELL]), like the DO/LOOP frames: x25/x28 belong to the
\ compiler, and word frames on the machine stack would unbalance the epilogue.
: W-LDRX {: rt RN off :}                               \ ( rt rn off -- w ) ldr rt,[rn,#off]
   $F9400000  off 8 / 10 lshift or  RN 5 lshift or  rt or ;

: W-STRX {: rt RN off :}                               \ ( rt rn off -- w ) str rt,[rn,#off]
   $F9000000  off 8 / 10 lshift or  RN 5 lshift or  rt or ;

: J-TOR                                                \ pop data -> push RSTK
   $D1002273 C-EMITW  $F9400269 C-EMITW                \ sub x19,#8 ; ldr x9,[x19]
   10 20 RSP-CELL W-LDRX C-EMITW
   $8B0A0E8B C-EMITW                                   \ add x11,x20,x10,lsl#3
   9 11 RSTK-OFF W-STRX C-EMITW
   $9100054A C-EMITW                                   \ add x10,x10,#1
   10 20 RSP-CELL W-STRX C-EMITW ;

: J-RPOP                                               \ x9 = RSTK top, x10 = RSP-1
   10 20 RSP-CELL W-LDRX C-EMITW
   $D100054A C-EMITW                                   \ sub x10,x10,#1
   $8B0A0E8B C-EMITW                                   \ add x11,x20,x10,lsl#3
   9 11 RSTK-OFF W-LDRX C-EMITW ;

: J-RFROM  J-RPOP                                      \ pop RSTK -> push data
   10 20 RSP-CELL W-STRX C-EMITW
   $F9000269 C-EMITW  $91002273 C-EMITW ;              \ str x9,[x19] ; add x19,#8

: J-RFETCH  J-RPOP                                     \ peek RSTK -> push data
   $F9000269 C-EMITW  $91002273 C-EMITW ;

\ EXIT: emit a placeholder word holding the PREVIOUS chain offset (0 = end);
\ `;` walks the chain and patches each into `b epilogue`. RECURSE: bl back to
\ the current word's entry (PEND slot.addr) — every word has the standard
\ prologue/epilogue, so calling into the open definition is well-formed.
: J-EXIT
   9 DATA EXITH-CELL LDR,                              \ x9 = prev chain offset
   10 CP DBASE SUB,  10 DATA EXITH-CELL STR,           \ head := this placeholder
   LCEMIT @ BL, ;

: J-RECURSE
   9 DATA PEND-CELL LDR,  9 9 0 LDR,  $94000000 $3FFFFFF C-BBACK ;   \ bl entry

: J-DOES
   LBL {: dok :}
   12 DATA LOCF-CELL LDR,  12 dok CBZ,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   dok LBL,
   $1000008A C-EMITW                     \ adr x10, #+16 = D (4 words ahead)
   16 20 DOESP-CELL W-LDRX C-EMITW       \ x16 = LDOESPATCH runtime addr
   $D63F0200 C-EMITW                     \ blr x16
   J-EXIT                                \ word 4: the defining word ends here
   9 $D10043FF LIT64,  LCEMIT @ BL,      \ D: fresh prologue for the does-body
   9 $F90003FE LIT64,  LCEMIT @ BL, ;

: J-QUOT
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

: J-SEMIQUOT
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

: EMIT-DOESPATCH
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
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

\ ---- interpret-mode defining words ----
\ record defining words for the checker: append the kind token + run the hook
\ (verdict ignored — create/variable/constant always publish).
: C-DEFHOOK  LBL {: kwv klen nohk :}
   11 kwv @ ADR,  12 klen MOVZ,  LBCS @ BL,
   9 DATA HOOK-CELL LDR,  9 nohk CBZ,
   10 DATA BODYBUF-OFF ADDI,  10 G-PUSH
   10 DATA BODYLEN-CELL LDR,  10 G-PUSH
   SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
   10 G-POP
   nohk LBL, ;

: EMIT-CREATE
   LBL LBL LBL {: ncp ncpd nokind :}
   LCREATE @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,  15 SP 8 STR,
   2 3 MOVZ,  LPROT @ BL,
   LTOK @ BL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP @ BL,   \ seed "NAME " for the hook
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   CP 9 0 STR,  12 DATA TKL-CELL LDR,  12 9 16 STR,
   14 DATA CUR-CELL LDR,  14 9 40 STR,
   10 9 24 ADDI,  11 DATA TKA-CELL LDR,  12 DATA TKL-CELL LDR,
   ncp LBL,  12 ncpd CBZ,  13 11 0 LDRB,  13 10 0 STRB,
      10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  ncp B,
   ncpd LBL,
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

: C-CREATE  15 1 MOVZ,  LCREATE @ BL, ;

: C-VARIABLE  C-CREATE
   7 DATA 0 LDR,  7 7 8 ADDI,  7 DP-CHECK  7 DATA 0 STR, ;

: C-CONSTANT
   LBL LBL {: kcp kcd :}
   2 3 MOVZ,  LPROT @ BL,  LTOK @ BL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  LBCAP @ BL,   \ seed "NAME " for the hook
   15 G-POP                                             \ n -> x15 AFTER LBCAP (it clobbers x15)
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   CP 9 0 STR,  12 DATA TKL-CELL LDR,  12 9 16 STR,  14 DATA CUR-CELL LDR,  14 9 40 STR,
   10 9 24 ADDI,  11 DATA TKA-CELL LDR,  12 DATA TKL-CELL LDR,
   kcp LBL,  12 kcd CBZ,  13 11 0 LDRB,  13 10 0 STRB,
      10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  kcp B,
   kcd LBL,
   11 15 0 ADDI,  C-LIT
   9 W-RET LIT64,  LCEMIT @ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,
   NDICT NDICT 1 ADDI,  9 9 0 LDR,                      \ x9 = body start for the flush
   2 5 MOVZ,  LPROT @ BL,  LFLUSH @ BL,
   LKWCONST 8 C-DEFHOOK ;

: C-IMMEDIATE
   2 3 MOVZ,  LPROT @ BL,
   9 NDICT 0 ADDI,  9 9 1 SUBI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 16 LDR,  10 10 $100 ORRI,  10 9 16 STR,
   2 5 MOVZ,  LPROT @ BL, ;

: C-POSTPONE
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

: C-ISDQ
   12 DATA INP-CELL LDR,  12 12 1 ADDI,  13 12 0 ADDI,
   LBL LBL LBL LBL {: sl sd cl cd :}
   sl LBL,  9 12 0 LDRB,  9 $22 CMPI,  C-EQ sd BCOND,  12 12 1 ADDI,  sl B,
   sd LBL,  10 12 13 SUB,  12 12 1 ADDI,  12 DATA INP-CELL STR,
   12 DATA 0 LDR,  15 12 0 ADDI,                        \ x12 = DP, x15 = string base
   14 12 10 ADD,  14 DP-CHECK
   11 13 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 12 0 STRB,  12 12 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   12 DATA 0 STR,                                       \ allot: DP advances past the copy
   15 G-PUSH  10 G-PUSH ;

: C-ICQ
   12 DATA INP-CELL LDR,  12 12 1 ADDI,  13 12 0 ADDI,
   LBL LBL LBL LBL LBL {: sl sd capok cl cd :}
   sl LBL,  9 12 0 LDRB,  9 $22 CMPI,  C-EQ sd BCOND,  12 12 1 ADDI,  sl B,
   sd LBL,  10 12 13 SUB,  12 12 1 ADDI,  12 DATA INP-CELL STR,
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

: C-IDOTQ
   12 DATA INP-CELL LDR,  12 12 1 ADDI,  13 12 0 ADDI,
   LBL LBL {: sl sd :}
   sl LBL,  9 12 0 LDRB,  9 $22 CMPI,  C-EQ sd BCOND,  12 12 1 ADDI,  sl B,
   sd LBL,  10 12 13 SUB,  12 12 1 ADDI,  12 DATA INP-CELL STR,
   0 1 MOVZ,  1 13 0 ADDI,  2 10 0 ADDI,  NR-WRITE SYS, ;

: C-CHAR   LTOK @ BL,  9 DATA TKA-CELL LDR,  9 9 0 LDRB,  9 G-PUSH ;

: C-BCHAR  LTOK @ BL,  11 DATA TKA-CELL LDR,  11 11 0 LDRB,  LVPUSHC @ BL, ;

: C-TICK
   LBL {: tk :}
   LTOK @ BL,  9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND @ BL,
   13 tk CBZ,  11 G-PUSH  tk LBL, ;

: C-BTICK
   LBL {: bk :}
   LTOK @ BL,  9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND @ BL,
   13 bk CBZ,  C-LIT  bk LBL, ;

: C-LBRACE
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   LBL LBL
   LBL
   {: cfok xok qlok nl nd nstore nlok noti ncp ncd pl pd tsl tsd :}
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
   xok LBL,
   6 DATA LOCN-CELL LDR,
   nl LBL,
      LTOK @ BL,  0 nd CBZ,
      LBCAP @ BL,                                          \ locals reach the checker too
      0 LKWENDLOC @ ADR,  1 2 MOVZ,  LKWCMP @ BL,  0 nstore CBZ,  nd B,
      nstore LBL,
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
      11 DATA LOCN-CELL LDR,  11 11 1 ADDI,  11 DATA LOCN-CELL STR,
      nl B,
   nd LBL,
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

\ compile-mode PC-RELATIVE address push: emit `adr x9, target` then the push
\ stencil. Unlike C-LIT's absolute movz/movk, the offset survives the AOT blob
\ copy and the ASLR slide, because the target (an embedded S" body) moves WITH
\ this instruction. target in x11; CP (the emit cursor / future ADR pc) is x28.
: C-ADR
   5 11 28 SUB,                                                       \ x5 = d = target - CP
   8 $10000009 LIT64,                                                 \ ADR opcode | Rd=x9
   6 3 MOVZ,  7 5 6 AND,  7 7 29 LSLI,  8 8 7 ORR,                    \ | (d & 3) << 29
   7 5 2 LSRI,  6 $7FFFF LIT64,  7 7 6 AND,  7 7 5 LSLI,  8 8 7 ORR,  \ | ((d>>2) & 0x7FFFF) << 5
   9 8 0 ADDI,  LCEMIT @ BL,                                          \ emit the ADR word
   9 W-PUSH0 LIT64,  LCEMIT @ BL,  9 W-PUSH1 LIT64,  LCEMIT @ BL, ;

: C-SDQ
   LBL LBL LBL LBL {: sl sd cl cd :}
   12 DATA INP-CELL LDR,  12 12 1 ADDI,  13 12 0 ADDI,
   sl LBL,  9 12 0 LDRB,  9 $22 CMPI,  C-EQ sd BCOND,  12 12 1 ADDI,  sl B,
   sd LBL,  10 12 13 SUB,  16 13 0 ADDI,  12 12 1 ADDI,  12 DATA INP-CELL STR,
   11 16 0 ADDI,  12 10 1 ADDI,  LBCS @ BL,
   15 CP 0 ADDI,  9 $14000000 LIT64,  LCEMIT @ BL,
   12 CP 0 ADDI,
   11 16 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   9 15 0 ADDI,  15 10 0 ADDI,  LPAT @ BL,
   11 12 0 ADDI,  C-ADR                                \ push byte addr PC-relative (AOT/ASLR-safe)
   11 15 0 ADDI,  C-LIT ;                              \ push len (a value, absolute is fine)

: C-CQ
   LBL LBL LBL LBL LBL {: sl sd capok cl cd :}
   12 DATA INP-CELL LDR,  12 12 1 ADDI,  13 12 0 ADDI,
   sl LBL,  9 12 0 LDRB,  9 $22 CMPI,  C-EQ sd BCOND,  12 12 1 ADDI,  sl B,
   sd LBL,  10 12 13 SUB,  16 13 0 ADDI,  12 12 1 ADDI,  12 DATA INP-CELL STR,
   10 255 CMPI,  C-LE capok BCOND,  0 76 MOVZ,  NR-EXIT SYS,
   capok LBL,
   11 16 0 ADDI,  12 10 1 ADDI,  LBCS @ BL,
   15 CP 0 ADDI,  9 $14000000 LIT64,  LCEMIT @ BL,
   12 CP 0 ADDI,
   10 28 0 STRB,  28 28 1 ADDI,
   11 16 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   9 15 0 ADDI,  15 10 1 ADDI,  LPAT @ BL,
   11 12 0 ADDI,  C-ADR ;

: C-DOTQ
   LBL {: ok :}
   C-SDQ
   9 LKWTYPE @ ADR,  10 4 MOVZ,  LFIND @ BL,
   13 ok CBNZ,  0 70 MOVZ,  NR-EXIT SYS,
   ok LBL,
   C-CALL ;
variable CFSK

: CF-ENTRY {: lmainlbl kwvar kwlen hxt :}
   LBL CFSK !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 CFSK @ CBZ,
   LVSPILL @ BL,
   hxt execute  lmainlbl B,
   CFSK @ LBL, ;
s" cf-entry" s" n n n n --" TRUST

\ cfn-entry: keyword case WITHOUT the spill — loop words manage the VS
\ themselves (BEGIN snapshots it, AGAIN/REPEAT reconcile to the snapshot).
: CFN-ENTRY {: lmainlbl kwvar kwlen hxt :}
   LBL CFSK !
   0 kwvar @ ADR,  1 kwlen MOVZ,  LKWCMP @ BL,
   0 CFSK @ CBZ,
   hxt execute  lmainlbl B,
   CFSK @ LBL, ;
s" cfn-entry" s" n n n n --" TRUST
\ ---- MAIN, split into emission-ordered phases sharing label variables ----
variable LMAIN  variable LEXIT  variable LCOMPILE  variable LUNDEF
variable LEX0  variable LUN0   \ re-entrant evaluate: original-path continuations of LEXIT / LUNDEF
variable SNBL  variable SNOL   \ snapshot-loader labels (em-startup's locals group is at the 16 cap)
variable CFSK2

\ cfb-entry: branch keywords (if/until/while) with the condition on the VS —
\ a REGISTER top branches directly (no spill + memory pop); con or empty falls
\ back to the spill + pop path. hxtr gets the condition reg in x14.
: CFB-ENTRY {: lmainlbl kwvar kwlen hxtm hxtr :}
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
s" cfb-entry" s" n n n n n --" TRUST

\ cfbn-entry: like CFB-ENTRY but the register path neither spills nor saves —
\ UNTIL reconciles to the BEGIN snapshot itself; the condition reg x14 survives
\ LVDROP (which only relabels the VS, no emission).
: CFBN-ENTRY {: lmainlbl kwvar kwlen hxtm hxtr :}
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
s" cfbn-entry" s" n n n n n --" TRUST

: J-IFR  C-PUSHCP  8 $B4000000 LIT64,  9 8 14 ORR,  LCEMIT @ BL, ;

: J-WHILER  J-IFR ;

: J-UNTILR                                 \ reg flag -> x17 first: the reconcile
   8 $AA0003F1 LIT64,  7 14 16 LSLI,  9 8 7 ORR,  LCEMIT @ BL,   \ may reload into it
   J-UNTILX ;

: EM-STARTUP
   LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL LBL
   LBL LBL LBL
   {: scopy scdone rvok dvok snomag sc1 sc1d sc2 sc2d srl srn srx cwok sdl2 sdn2 sds2 :}
   LBL SNBL !  LBL SNOL !
   LANCHOR @ LBL,
   13 0 0 ADDI,  14 1 0 ADDI,  15 2 0 ADDI,
   RBASE LANCHOR @ ADR,
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   XDS SP 0 ADDI,
   0 RBASE-VA LIT64,  1 REGION LIT64,  2 3 MOVZ,  3 $1012 LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 RBASE-VA LIT64,  0 5 CMP,
   C-EQ rvok BCOND,
      0 78 MOVZ,  NR-EXIT SYS,
   rvok LBL,
   DBASE 0 0 ADDI,
   CP DBASE 0 ADDI,  5 DICT-SIZE LIT64,  CP CP 5 ADD,
   11 LNCOUNT @ ADR,  11 11 0 LDR,  NDICT 11 0 ADDI,
   9 LDICT @ ADR,  10 DBASE 0 ADDI,  12 11 0 ADDI,
   scopy LBL,
      12 scdone CBZ,
      5 9 0 LDR,  6 9 8 LDR,
      7 RBASE 5 ADD,  7 10 0 STR,
      6 6 5 SUB,  6 6 4 SUBI,  6 10 8 STR,
      5 9 16 LDR,  5 10 16 STR,
      5 9 24 LDR,  5 10 24 STR,  5 9 32 LDR,  5 10 32 STR,
      5 9 40 LDR,  5 10 40 STR,
      9 9 DREC ADDI,  10 10 DREC ADDI,  12 12 1 SUBI,  scopy B,
   scdone LBL,
   0 DATA-VA LIT64,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 $1012 LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   5 DATA-VA LIT64,  0 5 CMP,
   C-EQ dvok BCOND,
      0 78 MOVZ,  NR-EXIT SYS,
   dvok LBL,
   20 0 RBASE-CELL STR,
   DATA 0 0 ADDI,
   XDS DATA S0-CELL STR,
   13 DATA ARGC-CELL STR,  14 DATA ARGV-CELL STR,  15 DATA ENVP-CELL STR,
   5 DATA-START MOVZ,  7 DATA 5 ADD,  7 DATA DP-CELL STR,
   \ ---- AOT snapshot? (trailer at the end of our own __text). If present:
   \ restore both regions verbatim (fixed VAs keep region addresses valid),
   \ relocate engine-text call chains (the only ASLR-movers), boot WARM. ----
   24 0 MOVZ,                                       \ x24 = snapshot flag
   9 DATA RBASE-CELL LDR,  25 9 0 ADDI,             \ x25 = live text CONTENT base
   10 9 0 ADDI,  5 $1000 LIT64,  10 10 5 SUB,
   11 10 216 LDR,                                   \ S = our __text size
   12 9 11 ADD,  12 12 40 SUBI,                     \ trailer
   13 12 0 LDR,  5 SNAP-MAGIC LIT64,  13 5 CMP,  C-NE snomag BCOND,
   21 12 8 LDR,                                     \ x21 = snapshot-time text base
   15 12 16 LDR,                                    \ x15 = ndict
   6 12 24 LDR,                                     \ x6 = region payload len
   7 12 32 LDR,                                     \ x7 = data payload len
   \ corrupt/truncated trailer must never smear the regions: exit 79
   5 REGION LIT64,  6 5 CMP,  C-GT SNBL @ BCOND,
   5 DATA-SIZE LIT64,  7 5 CMP,  C-GT SNBL @ BCOND,
   5 2200 MOVZ,  15 5 CMP,  C-GT SNBL @ BCOND,
   SNOL @ B,
   SNBL @ LBL,  0 79 MOVZ,  NR-EXIT SYS,
   SNOL @ LBL,
   9 DATA ARGC-CELL LDR,  10 DATA ARGV-CELL LDR,  0 DATA ENVP-CELL LDR,
   22 11 6 SUB,  22 22 7 SUB,  22 22 40 SUBI,       \ x22 = engine text len then
   8 12 7 SUB,  8 8 6 SUB,                          \ region payload src
   13 DBASE 0 ADDI,  14 0 MOVZ,
   sc1 LBL,  14 6 CMP,  C-GE sc1d BCOND,
      3 8 14 ADD,  3 3 0 LDRB,  4 13 14 ADD,  3 4 0 STRB,
      14 14 1 ADDI,  sc1 B,
   sc1d LBL,
   8 12 7 SUB,  13 DATA 0 ADDI,  14 0 MOVZ,
   sc2 LBL,  14 7 CMP,  C-GE sc2d BCOND,
      3 8 14 ADD,  3 3 0 LDRB,  4 13 14 ADD,  3 4 0 STRB,
      14 14 1 ADDI,  sc2 B,
   sc2d LBL,
   25 DATA RBASE-CELL STR,                          \ live values over stale copies
   XDS DATA S0-CELL STR,
   9 DATA ARGC-CELL STR,  10 DATA ARGV-CELL STR,  0 DATA ENVP-CELL STR,
   NDICT 15 0 ADDI,
   CP DBASE 6 ADD,
   \ rebase seed-prim dict entries (slot.addr in the old engine text)
   9 DBASE 0 ADDI,  10 0 MOVZ,
   sdl2 LBL,  10 NDICT CMP,  C-GE sdn2 BCOND,
      13 9 0 LDR,
      13 21 CMP,  C-LT sds2 BCOND,
      14 21 22 ADD,  13 14 CMP,  C-GE sds2 BCOND,
      13 13 21 SUB,  13 13 25 ADD,  13 9 0 STR,
      sds2 LBL,  9 9 DREC ADDI,  10 10 1 ADDI,  sdl2 B,
   sdn2 LBL,
   \ relocation: movz/movk/movk x16 + blr x16 whose value sat in the OLD text
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
      13 13 21 SUB,  13 13 25 ADD,                  \ rebase into the live text
      10 9 0 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 0 ADDI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 0 STRW,
      10 9 4 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 16 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 4 STRW,
      10 9 8 LDRW,  5 $FFE0001F LIT64,  10 10 5 AND,
        14 13 32 LSRI,  5 $FFFF LIT64,  14 14 5 AND,  14 14 5 LSLI,  10 10 14 ORR,  10 9 8 STRW,
      9 9 12 ADDI,
   srn LBL,  9 9 4 ADDI,  srl B,
   srx LBL,
   2 5 MOVZ,  LPROT @ BL,                           \ region RX +
   9 DBASE 0 ADDI,  5 DICT-SIZE LIT64,  9 9 5 ADD,  LFLUSH @ BL,   \ coherent
   24 1 MOVZ,
   snomag LBL,
   9 0 MOVZ,  9 DATA HND-CELL STR,
   24 cwok CBNZ,

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
   9 0 MOVZ,  9 DATA PEND-CELL STR, ;

: EM-COMMENT
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

: EM-INTERPRET
   LBL LBL LBL LBL LBL LBL {: lnotcolon ncopy ncd lnotnum cpok ndok :}
   9 DATA TKL-CELL LDR,  9 1 CMPI,  C-NE lnotcolon BCOND,
   9 DATA TKA-CELL LDR,  9 9 0 LDRB,  9 58 CMPI,  C-NE lnotcolon BCOND,
      2 3 MOVZ,  LPROT @ BL,
      9 REGION $4000 - LIT64,  9 DBASE 9 ADD,  CP 9 CMP,  C-LT cpok BCOND,
         0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
         0 76 MOVZ,  NR-EXIT SYS,
      cpok LBL,
      9 2200 MOVZ,  NDICT 9 CMP,  C-LT ndok BCOND,      \ slot 2200 < CFSTK-OFF/48
         0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,
         0 77 MOVZ,  NR-EXIT SYS,
      ndok LBL,
      LTOK @ BL,
      9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
      9 DATA PEND-CELL STR,
      CP 9 0 STR,  12 DATA TKL-CELL LDR,  12 9 16 STR,
      14 DATA CUR-CELL LDR,  14 9 40 STR,
      10 9 24 ADDI,  11 DATA TKA-CELL LDR,  12 DATA TKL-CELL LDR,
      ncopy LBL,  12 ncd CBZ,
         13 11 0 LDRB,  13 10 0 STRB,
         10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  ncopy B,
      ncd LBL,
      5 CFSTK-OFF LIT64,  11 DBASE 5 ADD,  12 0 MOVZ,  12 11 0 STR,
      12 0 MOVZ,  12 DATA LOCN-CELL STR,  12 DATA LOCF-CELL STR,
      12 0 MOVZ,  12 DATA BODYLEN-CELL STR,
      LBCAP @ BL,             \ seed with the NAME (checker records certified sigs)
      \ capture an optional leading ( in -- out ) into the body, so the check
      \ hook sees the declared sig (CHECK! verifies the body against it)
      LBL {: nsig :}  LBL {: sigq :}  LBL {: sp1 :}  LBL {: sc2 :}  LBL {: scd :}
      11 DATA INP-CELL LDR,  12 DATA INE-CELL LDR,
      sp1 LBL,  11 12 CMP,  C-GE nsig BCOND,
         13 11 0 LDRB,  13 32 CMPI,  C-HI sigq BCOND,
         11 11 1 ADDI,  sp1 B,
      sigq LBL,  13 40 CMPI,  C-NE nsig BCOND,         \ not '(' -> no sig
      14 11 0 ADDI,  15 11 0 ADDI,                     \ x14=start x15=cursor
      sc2 LBL,  15 12 CMP,  C-GE scd BCOND,
         13 15 0 LDRB,  15 15 1 ADDI,  13 41 CMPI,  C-NE sc2 BCOND,
      scd LBL,  15 DATA INP-CELL STR,                  \ consume through ')'
      11 14 0 ADDI,  12 15 14 SUB,  LBCS @ BL,         \ append "( ... )" to body
      nsig LBL,
      12 0 MOVZ,  12 DATA VSP-CELL STR,  12 DATA SNAPSP-CELL STR,
      12 DATA EXITH-CELL STR,  12 DATA LVD-CELL STR,
      12 DATA QPATCH-CELL STR,
      12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,
      12 FRALL MOVZ,  12 DATA FRFREE-CELL STR,
      9 $D10043FF LIT64,  LCEMIT @ BL,
      9 $F90003FE LIT64,  LCEMIT @ BL,
      LMAIN @ B,
   lnotcolon LBL,
   s" create" KEEP? IF LMAIN @ LKWCREATE 6 ['] C-CREATE   CF-ENTRY THEN
   s" variable" KEEP? IF LMAIN @ LKWVAR    8 ['] C-VARIABLE CF-ENTRY THEN
   s" constant" KEEP? IF LMAIN @ LKWCONST  8 ['] C-CONSTANT CF-ENTRY THEN
   s" '" KEEP? IF LMAIN @ LKWTICK   1 ['] C-TICK     CF-ENTRY THEN
   s" char" KEEP? IF LMAIN @ LKWCHAR   4 ['] C-CHAR     CF-ENTRY THEN
   s" immediate" KEEP? IF LMAIN @ LKWIMM    9 ['] C-IMMEDIATE CF-ENTRY THEN
   LMAIN @ LKWSQ     2 ['] C-ISDQ     CF-ENTRY
   LMAIN @ LKWCQ     2 ['] C-ICQ      CF-ENTRY
   LMAIN @ LKWDOTQ   2 ['] C-IDOTQ    CF-ENTRY
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LNUM @ BL,
   12 lnotnum CBZ,  11 G-PUSH  LMAIN @ B,
   lnotnum LBL,
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND @ BL,
   13 LUNDEF @ CBZ,
   11 BLR,  LMAIN @ B, ;
s" em-interpret" s" --" TRUST

: EM-COMPILE
   LBL LBL LBL LBL LBL LBL LBL LBL {: lnotsemi notd nohook rejected notloc lmem lcnotnum notimm :}
   LCOMPILE @ LBL,
      9 DATA TKL-CELL LDR,  9 1 CMPI,  C-NE lnotsemi BCOND,
      9 DATA TKA-CELL LDR,  9 9 0 LDRB,  9 59 CMPI,  C-NE lnotsemi BCOND,
         LVSPILL @ BL,
         14 CP 0 ADDI,  9 DATA EXITH-CELL LDR,  LBCHAIN @ BL,
         12 DATA LOCF-CELL LDR,  12 notd CBZ,
            9 $910003FF LIT64,  14 12 10 LSLI,  9 9 14 ORR,  LCEMIT @ BL,
         notd LBL,
         9 $F94003FE LIT64,  LCEMIT @ BL,
         9 $910043FF LIT64,  LCEMIT @ BL,
         9 W-RET LIT64,  LCEMIT @ BL,
         11 DATA PEND-CELL LDR,  9 11 0 LDR,  10 CP 9 SUB,  10 10 4 SUBI,  10 11 8 STR,
         2 5 MOVZ,  LPROT @ BL,  LFLUSH @ BL,
         9 DATA HOOK-CELL LDR,  9 nohook CBZ,
            10 DATA BODYBUF-OFF ADDI,  10 G-PUSH
            10 DATA BODYLEN-CELL LDR,  10 G-PUSH
            SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
            10 G-POP  10 rejected CBZ,
         nohook LBL,
            NDICT NDICT 1 ADDI,
         rejected LBL,
         9 0 MOVZ,  9 DATA PEND-CELL STR,
         LMAIN @ B,
      lnotsemi LBL,
      LBCAP @ BL,
      s" if" KEEP? IF LMAIN @ LKWIF     2 ['] J-IF   ['] J-IFR    CFB-ENTRY THEN
      s" then" KEEP? IF LMAIN @ LKWTHEN   4 ['] J-THEN   CF-ENTRY THEN
      s" else" KEEP? IF LMAIN @ LKWELSE   4 ['] J-ELSE   CF-ENTRY THEN
      s" begin" KEEP? IF LMAIN @ LKWBEGIN  5 ['] J-BEGIN  CFN-ENTRY THEN
      s" until" KEEP? IF LMAIN @ LKWUNTIL  5 ['] J-UNTIL ['] J-UNTILR CFBN-ENTRY THEN
      s" again" KEEP? IF LMAIN @ LKWAGAIN  5 ['] J-AGAIN  CFN-ENTRY THEN
      s" while" KEEP? IF LMAIN @ LKWWHILE  5 ['] J-WHILE ['] J-WHILER CFB-ENTRY THEN
      s" repeat" KEEP? IF LMAIN @ LKWREPEAT 6 ['] J-REPEAT CFN-ENTRY THEN
      LMAIN @ LKWSQ     2 ['] C-SDQ    CF-ENTRY
      LMAIN @ LKWCQ     2 ['] C-CQ     CF-ENTRY
      LMAIN @ LKWDOTQ   2 ['] C-DOTQ   CF-ENTRY
      s" [']" KEEP? IF LMAIN @ LKWBTICK  3 ['] C-BTICK  CF-ENTRY THEN
      s" [char]" KEEP? IF LMAIN @ LKWBCHAR  6 ['] C-BCHAR  CF-ENTRY THEN
      s" postpone" KEEP? IF LMAIN @ LKWPOST   8 ['] C-POSTPONE CF-ENTRY THEN
      s" does>" KEEP? IF LMAIN @ LKWDOES   5 ['] J-DOES     CF-ENTRY THEN
      s" [:" KEEP? IF LMAIN @ LKWQUOT   2 ['] J-QUOT     CF-ENTRY THEN
      s" ;]" KEEP? IF LMAIN @ LKWSEMIQ  2 ['] J-SEMIQUOT CF-ENTRY THEN
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
      s" {:" KEEP? IF LMAIN @ LKWLBRACE 2 ['] C-LBRACE CF-ENTRY THEN
      LLOC-FIND @ BL,  0 0 CMPI,  C-LT notloc BCOND,
         LVRALLOC @ BL,  14 lmem CBZ,
         7 DATA LOCF-CELL LDR,  7 7 3 LSRI,  7 7 0 SUB,  7 7 1 SUBI,
         9 $F94003E0 LIT64,  9 9 14 ORR,  7 7 10 LSLI,  9 9 7 ORR,  LCEMIT @ BL,
         LVPUSHR @ BL,
         LMAIN @ B,
         lmem LBL,
         LVSPILL @ BL,
         7 DATA LOCF-CELL LDR,  7 7 3 LSRI,  7 7 0 SUB,  7 7 1 SUBI,
         9 $F94003E9 LIT64,  7 7 10 LSLI,  9 9 7 ORR,  LCEMIT @ BL,
         9 W-PUSH0 LIT64,  LCEMIT @ BL,  9 W-PUSH1 LIT64,  LCEMIT @ BL,
         LMAIN @ B,
      notloc LBL,
      9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LNUM @ BL,
      12 lcnotnum CBZ,
      LBL {: lcflt :}  2 lcflt CBNZ,  LVPUSHC @ BL,  LMAIN @ B,
      lcflt LBL,  LVPUSHF @ BL,  LMAIN @ B,
      lcnotnum LBL,
      s" +" KEEP? IF LMAIN @ LKWPLUS  1 ['] VF+ ['] E+ ['] EI+ VOPI-ENTRY THEN
      s" -" KEEP? IF LMAIN @ LKWMINUS 1 ['] VF- ['] E- ['] EI- VOPI-ENTRY THEN
      s" *" KEEP? IF LMAIN @ LKWSTAR  1 ['] VF* ['] E* VOP-ENTRY THEN
      s" and" KEEP? IF LMAIN @ LKWAND2  3 ['] FAND ['] EAND VOP-ENTRY THEN
      s" or" KEEP? IF LMAIN @ LKWOR2   2 ['] FOR2 ['] EOR2 VOP-ENTRY THEN
      s" xor" KEEP? IF LMAIN @ LKWXOR2  3 ['] FXOR2 ['] EXOR VOP-ENTRY THEN
      s" dup" KEEP? IF LMAIN @ LKWDUP2  3 1 ['] XDUP  VSHUF-ENTRY THEN
      s" drop" KEEP? IF LMAIN @ LKWDROP2 4 1 ['] XDROP VSHUF-ENTRY THEN
      s" swap" KEEP? IF LMAIN @ LKWSWAP2 4 2 ['] XSWAP VSHUF-ENTRY THEN
      s" over" KEEP? IF LMAIN @ LKWOVER2 4 2 ['] XOVER VSHUF-ENTRY THEN
      s" nip" KEEP? IF LMAIN @ LKWNIP2  3 2 ['] XNIP  VSHUF-ENTRY THEN
      s" =" KEEP? IF LMAIN @ LKWEQ2 1 0 VCMP-ENTRY THEN
      s" <>" KEEP? IF LMAIN @ LKWNE2 2 1 VCMP-ENTRY THEN
      s" <" KEEP? IF LMAIN @ LKWLT2 1 11 VCMP-ENTRY THEN
      s" >" KEEP? IF LMAIN @ LKWGT2 1 12 VCMP-ENTRY THEN
      s" <=" KEEP? IF LMAIN @ LKWLE2 2 13 VCMP-ENTRY THEN
      s" >=" KEEP? IF LMAIN @ LKWGE2 2 10 VCMP-ENTRY THEN
      s" 1+" KEEP? IF LMAIN @ LKWINC  2 ['] FU1+ ['] EU1+ VUN-ENTRY THEN
      s" 1-" KEEP? IF LMAIN @ LKWDEC  2 ['] FU1- ['] EU1- VUN-ENTRY THEN
      s" 0=" KEEP? IF LMAIN @ LKWZEQ  2 ['] FU0= ['] EU0= VUN-ENTRY THEN
      s" 0<" KEEP? IF LMAIN @ LKWZLT  2 ['] FU0< ['] EU0< VUN-ENTRY THEN
      s" negate" KEEP? IF LMAIN @ LKWNEG2 6 ['] FUNEG ['] EUNEG VUN-ENTRY THEN
      s" invert" KEEP? IF LMAIN @ LKWINV2 6 ['] FUINV ['] EUINV VUN-ENTRY THEN
      s" f+" KEEP? IF LMAIN @ LKWFPLUS  2 $1E602800 FOP-ENTRY THEN
      s" f-" KEEP? IF LMAIN @ LKWFMINUS 2 $1E603800 FOP-ENTRY THEN
      s" f*" KEEP? IF LMAIN @ LKWFSTAR  2 $1E600800 FOP-ENTRY THEN
      s" f/" KEEP? IF LMAIN @ LKWFSLASH 2 $1E601800 FOP-ENTRY THEN
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
      C-CALL  LMAIN @ B,
   LUNDEF @ LBL,
      0 2 MOVZ,  1 DATA TKA-CELL LDR,  2 DATA TKL-CELL LDR,  NR-WRITE SYS,   \ write(2, name)
      9 DATA EVALD-CELL LDR,  9 LUN0 @ CBZ,          \ inside evaluate? roll back to the frame, return err=1
         14 EVAL-FRAME LIT64,  14 DATA 14 ADD,
         9 DATA EVALD-CELL LDR,  9 9 1 SUBI,  9 DATA EVALD-CELL STR,
         CP 14 40 LDR,  NDICT 14 48 LDR,  XDS 14 32 LDR,
         9 14 56 LDR,  9 DATA DP-CELL STR,
         9 0 MOVZ,
         9 DATA RSP-CELL STR,  9 DATA HND-CELL STR,  9 DATA LOOPSP-CELL STR,
         9 DATA LVD-CELL STR,  9 DATA VSP-CELL STR,  9 DATA QPATCH-CELL STR,
         9 DATA LOCN-CELL STR,  9 DATA BODYLEN-CELL STR,  9 DATA EXITH-CELL STR,
         9 DATA PEND-CELL STR,
         9 VRALL MOVZ,  9 DATA VRFREE-CELL STR,
         9 14 0 LDR,  9 DATA INP-CELL STR,
         9 14 8 LDR,  9 DATA INE-CELL STR,
         9 1 MOVZ,  9 DATA EVALERR-CELL STR,
         9 14 24 LDR,  SP 9 0 ADDI,                  \ restore the machine SP, then return
         9 14 16 LDR,  9 BR,
      LUN0 @ LBL,
      9 DATA REPLH-CELL LDR,  9 LRDIE @ CBZ,
   LRREC @ LBL,
      \ REPL recovery (also throw's no-handler target): "?", roll back the
      \ line's compile state, reset stacks AND the machine SP, read again
      0 2 MOVZ,  1 LQNL @ ADR,  2 2 MOVZ,  NR-WRITE SYS,
      CP DATA RSAVCP-CELL LDR,
      NDICT DATA RSAVND-CELL LDR,
      9 DATA RSAVDP-CELL LDR,  9 DATA DP-CELL STR,
      9 DATA S0-CELL LDR,  XDS 9 0 ADDI,
      9 0 MOVZ,
      9 DATA RSP-CELL STR,  9 DATA HND-CELL STR,  9 DATA LOOPSP-CELL STR,
      9 DATA LVD-CELL STR,  9 DATA VSP-CELL STR,  9 DATA QPATCH-CELL STR,
      9 DATA LOCN-CELL STR,  9 DATA BODYLEN-CELL STR,  9 DATA EXITH-CELL STR,
      9 DATA PEND-CELL STR,
      9 VRALL MOVZ,  9 DATA VRFREE-CELL STR,
      9 DATA RSAVSP-CELL LDR,  SP 9 0 ADDI,
      LREAD @ B,
   LRDIE @ LBL,
      0 70 MOVZ,  NR-EXIT SYS,                       \ exit(70)
   LEXIT @ LBL,
      9 DATA EVALD-CELL LDR,  9 LEX0 @ CBZ,          \ inside evaluate? clean end-of-buffer -> return
         14 EVAL-FRAME LIT64,  14 DATA 14 ADD,
         9 DATA EVALD-CELL LDR,  9 9 1 SUBI,  9 DATA EVALD-CELL STR,
         9 14 0 LDR,  9 DATA INP-CELL STR,           \ restore outer INP/INE (defs persist)
         9 14 8 LDR,  9 DATA INE-CELL STR,
         9 0 MOVZ,  9 DATA EVALERR-CELL STR,         \ clean
         9 14 24 LDR,  SP 9 0 ADDI,                  \ restore the machine SP (no per-call drift)
         9 14 16 LDR,  9 BR,                         \ return to the evaluate caller
      LEX0 @ LBL,
      9 DATA REPLH-CELL LDR,  9 LRBYE @ CBZ,
      0 1 MOVZ,  1 LOKS @ ADR,  2 4 MOVZ,  NR-WRITE SYS,        \ " ok"
   LREAD @ LBL,
      \ save line-start compile state, then call RD-LINE ( -- a u )
      9 SP 0 ADDI,  9 DATA RSAVSP-CELL STR,
      CP DATA RSAVCP-CELL STR,
      NDICT DATA RSAVND-CELL STR,
      9 DATA DP-CELL LDR,  9 DATA RSAVDP-CELL STR,
      9 DATA REPLH-CELL LDR,  9 BLR,
      XDS XDS 8 SUBI,  10 XDS 0 LDR,
      XDS XDS 8 SUBI,  11 XDS 0 LDR,
      10 LRBYE @ CBZ,                                 \ empty = EOF
      11 DATA INP-CELL STR,  11 11 10 ADD,  11 DATA INE-CELL STR,  LMAIN @ B,
   LRBYE @ LBL,
      0 0 MOVZ,  NR-EXIT SYS, ;                     \ exit(0)
s" em-compile" s" --" TRUST

: EMIT-MAIN
   LBL LMAIN !  LBL LEXIT !  LBL LCOMPILE !  LBL LUNDEF !
   EM-STARTUP  EM-COMMENT  EM-INTERPRET  EM-COMPILE ;
s" emit-main" s" --" TRUST
variable SRCA

: EMIT-FORTH {: a u :}
   u SRCN !  a SRCA !
   ASM-INIT  0 #PL !  0 PNP !
   LBL LANCHOR !  LBL LFIND !  LBL LNUM !  LBL LDICT !  LBL LSRC !
   LBL LCEMIT !  LBL LTOK !  LBL LPROT !  LBL LFLUSH !  LBL LNCOUNT !
   LBL LBCAP !  LBL LBCS !
   LBL LCFPUSH !  LBL LCFPOP !  LBL LPAT !  LBL LKWCMP !
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
   LBL LKWQUOT !  LBL LKWSEMIQ !
   LBL LBCHAIN !  LBL LCREATE !  LBL LDOESPATCH !
   LBL LREAD !  LBL LRBYE !  LBL LRDIE !  LBL LRREC !  LBL LQNL !  LBL LOKS !
   LBL LEX0 !  LBL LUN0 !
   LBL LCRASHH !  LBL LHEX !  LBL LHDR !  LBL LTRAPH !  LBL LBPH !
   LBL LPROFH !  LBL LPROFDUMP !
   LBL LVSPILL !  LBL LVLITPUSH !  LBL LVPUSHC !
   LBL LVTOP2C !  LBL LVFOLDPUT !
   LBL LVRALLOC !  LBL LVBIT !  LBL LVRINIT !  LBL LVMOVK !  LBL LVFORCEK !  LBL LVBINPREP !  LBL LVBINIPREP !  LBL LVPUSHR !
   LBL LVPUSHF !  LBL LFRALLOC !  LBL LFFORCEK !  LBL LFBINPREP !
   LBL LKWFPLUS !  LBL LKWFMINUS !  LBL LKWFSTAR !  LBL LKWFSLASH !
   LBL LVDROP !  LBL LVSWAPX !  LBL LVNIPX !  LBL LVCOPY !
   LBL LVSNAP !  LBL LVRECON !
   LBL LKWPLUS !  LBL LKWMINUS !  LBL LKWSTAR !
   LBL LKWAND2 !  LBL LKWOR2 !  LBL LKWXOR2 !
   LBL LKWDUP2 !  LBL LKWDROP2 !  LBL LKWSWAP2 !
   LBL LKWOVER2 !  LBL LKWNIP2 !
   LBL LKWEQ2 !  LBL LKWNE2 !  LBL LKWLT2 !
   LBL LKWGT2 !  LBL LKWLE2 !  LBL LKWGE2 !
   LBL LKWINC !  LBL LKWDEC !  LBL LKWZEQ !
   LBL LKWZLT !  LBL LKWNEG2 !  LBL LKWINV2 !
   EMIT-MAIN
   EMIT-PRIMS  EMIT-PROF-PRIMS  EMIT-FP-PRIMS  EMIT-CEMIT  EMIT-BCAP  EMIT-TOK  EMIT-PROT  EMIT-FLUSH  EMIT-FIND  EMIT-NUM
   EMIT-CREATE  EMIT-DOESPATCH
   EMIT-CF-HELPERS  EMIT-LOC-FIND  EMIT-KWDATA  EMIT-FOLDKW  EMIT-SHUFKW  EMIT-CMPKW  EMIT-UNKW  EMIT-CRASH-HANDLER  EMIT-TRAPH  EMIT-HEX
   EMIT-PROFDUMP  EMIT-PROF  EMIT-JIT
   EMIT-DICT
   LSRC @ LBL,  SRCA @ SRCN @ BYTES, ;
s" emit-forth" s" n n --" TRUST
