\ habu2.f — engine-builder port, part 2 (from bootstrap/cg/forth.fs): the JIT compiler
\ emitters (literal/call/keywords/locals/strings/do-loop), the outer-interpreter
\ main loop, and EMIT-FORTH. Needs habu1.f (part 1). emit-main is split into
\ phase words sharing label VARIABLES (a giant single word would need dozens of
\ locals); emission ORDER is exactly src/cg's, so the output is byte-identical.
\ ---- compile-mode literal: emit movz/movk x9=val then the push stencil ----
: c-lit
   6 11 0 ADDI,  5 $FFFF MOVZ,
   7 6 5 AND,    7 7 5 LSLI,  8 W-MOVZ0 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK1 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK2 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   7 6 48 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 W-MOVK3 LIT64,  9 8 7 ORR,  Lcemit @ BL,
   9 W-PUSH0 LIT64,  Lcemit @ BL,  9 W-PUSH1 LIT64,  Lcemit @ BL, ;
\ ---- compile-mode CALL-or-INLINE (x11=target addr, x12=clen from FIND) ----
$28 constant INL-MAX

: c-call
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: lcall lcopy lscan lsbody lnopro linl ldone :}
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
      8 $FFFFFC1F LIT64,  10 9 8 AND,
         8 $D63F0000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
         8 $D61F0000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
      8 $D65F03C0 LIT64,  9 8 CMP,  C-EQ lcall BCOND,
      8 $1F000000 LIT64,  10 9 8 AND,  8 $10000000 LIT64,  10 8 CMP,  C-EQ lcall BCOND,
      lsbody B,
   lcopy LBL,
      15 13 0 ADDI,
   linl LBL,  15 14 CMP,  C-GE ldone BCOND,
      9 15 0 LDRW,  15 15 4 ADDI,  Lcemit @ BL,  linl B,
   lcall LBL,
      5 $FFFF MOVZ,
      7 11 5 AND,    7 7 5 LSLI,  8 $D2800010 LIT64,  9 8 7 ORR,  Lcemit @ BL,
      7 11 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2A00010 LIT64,  9 8 7 ORR,  Lcemit @ BL,
      7 11 32 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2C00010 LIT64,  9 8 7 ORR,  Lcemit @ BL,
      9 $D63F0200 LIT64,  Lcemit @ BL,
   ldone LBL, ;

\ ---- source setup: baked Lsrc or stdin ----
: emit-source
   NEWLBL NEWLBL {: rl rd :}              \ locals BEFORE the IF (frame footgun)
   STDIN? @ IF
      0 0 MOVZ,  1 IBUFSZ LIT64,  2 3 MOVZ,  3 $1002 LIT64,  4 0 MOVN,  5 0 MOVZ,
      NR-MMAP SYS,
      11 0 0 ADDI,  9 0 0 ADDI,
      rl LBL,
         0 0 MOVZ,  1 9 0 ADDI,
         2 11 0 ADDI,  5 IBUFSZ LIT64,  2 2 5 ADD,  2 2 9 SUB,
         2 rd CBZ,
         NR-READ SYS,
         0 rd CBZ,
         9 9 0 ADD,  rl B,
      rd LBL,
      INP 11 0 ADDI,  INE 9 0 ADDI,
   ELSE
      INP Lsrc @ ADR,  INE Lsrc @ ADR,  5 SRCN @ LIT64,  INE INE 5 ADD,
   THEN ;

\ ---- control-flow JIT helpers ----
: emit-cf-helpers
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: pisb pdone kno kyes kchk knf :}
   Lcfpush @ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,
      12 11 3 LSLI,  12 12 10 ADD,  12 12 8 ADDI,  9 12 0 STR,
      11 11 1 ADDI,  11 10 0 STR,  RET,
   Lcfpop @ LBL,
      5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,  11 11 1 SUBI,  11 10 0 STR,
      12 11 3 LSLI,  12 12 10 ADD,  12 12 8 ADDI,  9 12 0 LDR,  RET,
   Lpat @ LBL,
      11 9 0 LDRW,  10 CP 9 SUB,  10 10 2 ASRI,
      5 $80000000 LIT64,  13 11 5 AND,
      13 pisb CBZ,
         5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,  pdone B,
      pisb LBL,  5 $3FFFFFF LIT64,  10 10 5 AND,
      pdone LBL,  11 11 10 ORR,  11 9 0 STRW,  RET,
   Lkwcmp @ LBL,
      TKL 1 CMP,  C-NE kno BCOND,
      2 0 MOVZ,  3 $20 MOVZ,
      kchk LBL,
         2 1 CMP,  C-GE kyes BCOND,
         4 TKA 2 ADD,  4 4 0 LDRB,
         4 $41 CMPI,  C-LT knf BCOND,  4 $5A CMPI,  C-GT knf BCOND,  4 4 3 ORR,
         knf LBL,
         5 0 2 ADD,    5 5 0 LDRB,
         4 5 CMP,  C-NE kno BCOND,
         2 2 1 ADDI,  kchk B,
      kyes LBL,  0 1 MOVZ,  RET,
      kno  LBL,  0 0 MOVZ,  RET,
   Lbchain @ LBL,                                    \ patch a B-placeholder chain:
      NEWLBL NEWLBL {: bcl bcd :}                    \ x9=head offset, x14=target;
      bcl LBL,  9 bcd CBZ,                           \ clobbers x5,x10-x12
         10 DBASE 9 ADD,  11 10 0 LDRW,
         12 14 10 SUB,  12 12 2 ASRI,
         5 $3FFFFFF LIT64,  12 12 5 AND,
         5 $14000000 LIT64,  12 12 5 ORR,
         12 10 0 STRW,
         9 11 0 ADDI,  bcl B,
      bcd LBL,  RET, ;

: emit-loc-find
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: ll lmiss lhit lcmp lnext :}
   Lloc-find @ LBL,
   9 DATA LOCN-CELL LDR,  10 0 MOVZ,
   ll LBL,  10 9 CMP,  C-GE lmiss BCOND,
      12 LOC-REC MOVZ,  11 10 12 MUL,  11 11 LOCNAMES ADDI,  11 DATA 11 ADD,
      12 11 0 LDR,  12 TKL CMP,  C-NE lnext BCOND,
      13 0 MOVZ,
      lcmp LBL,  13 TKL CMP,  C-GE lhit BCOND,
         14 11 13 ADD,  14 14 8 ADDI,  14 14 0 LDRB,
         15 TKA 13 ADD,  15 15 0 LDRB,
         14 15 CMP,  C-NE lnext BCOND,
         13 13 1 ADDI,  lcmp B,
      lhit LBL,  0 10 0 ADDI,  RET,
      lnext LBL,  10 10 1 ADDI,  ll B,
   lmiss LBL,  0 0 MOVN,  RET, ;
\ keyword bytes (lower-case / literal) at known labels
create SQ-KW  115 c, 34 c,
create BCHAR-KW 91 c, 99 c, 104 c, 97 c, 114 c, 93 c,   \ [char]
create TICK-KW   39 c,
create BTICK-KW  91 c, 39 c, 93 c,
create LBRACE-KW 123 c, 58 c,
create ENDLOC-KW 58 c, 125 c,

: emit-kwdata
   Lkwif @ LBL,     s" if"     BYTES,    Lkwthen @ LBL,   s" then"   BYTES,
   Lkwelse @ LBL,   s" else"   BYTES,    Lkwbegin @ LBL,  s" begin"  BYTES,
   Lkwuntil @ LBL,  s" until"  BYTES,    Lkwagain @ LBL,  s" again"  BYTES,
   Lkwwhile @ LBL,  s" while"  BYTES,    Lkwrepeat @ LBL, s" repeat" BYTES,
   Lkwcreate @ LBL, s" create" BYTES,    Lkwvar @ LBL,    s" variable" BYTES,
   Lkwsq @ LBL,     SQ-KW 2 BYTES,
   Lkwtick @ LBL,   TICK-KW 1 BYTES,    Lkwbtick @ LBL,  BTICK-KW 3 BYTES,
   Lkwlbrace @ LBL, LBRACE-KW 2 BYTES,  Lkwendloc @ LBL, ENDLOC-KW 2 BYTES,
   Lkwconst @ LBL,  s" constant" BYTES,
   Lkwdo @ LBL,  s" do" BYTES,    Lkwloop @ LBL,  s" loop" BYTES,    Lkwi @ LBL,  s" i" BYTES,
   Lkwtor @ LBL,  s" >r" BYTES,   Lkwrfrom @ LBL,  s" r>" BYTES,   Lkwrfet @ LBL,  s" r@" BYTES,
   Lkwexit @ LBL,  s" exit" BYTES,   Lkwrec @ LBL,  s" recurse" BYTES,
   Lkwqdo @ LBL,  s" ?do" BYTES,   Lkwploop @ LBL,  s" +loop" BYTES,   Lkwj @ LBL,  s" j" BYTES,
   Lkwleave @ LBL,  s" leave" BYTES,   Lkwunloop @ LBL,  s" unloop" BYTES,
   Lkwchar @ LBL,  s" char" BYTES,   Lkwbchar @ LBL,  BCHAR-KW 6 BYTES,
   Lkwimm @ LBL,  s" immediate" BYTES,   Lkwpost @ LBL,  s" postpone" BYTES,
   Lkwcompc @ LBL,  s" compile," BYTES,
   Lkwdoes @ LBL,  s" does>" BYTES, ;

\ ---- compile-time keyword handlers (append JIT-emitter code at BUILD time) ----
: c-emitw {: w :}  9 w LIT64,  Lcemit @ BL, ;

: c-popflag  $D1002273 c-emitw  $F9400269 c-emitw ;

: c-pushcp   9 CP 0 ADDI,  Lcfpush @ BL, ;

: c-bback {: opc mask :}
   10 9 CP SUB,  10 10 2 ASRI,  5 mask LIT64,  10 10 5 AND,  9 opc LIT64,  9 9 10 ORR,  Lcemit @ BL, ;

: j-if    c-popflag  c-pushcp  $B4000009 c-emitw ;

: j-then  Lcfpop @ BL,  Lpat @ BL, ;

: j-else  Lcfpop @ BL,  14 9 0 ADDI,  c-pushcp  $14000000 c-emitw  9 14 0 ADDI,  Lpat @ BL, ;

\ BEGIN loops are register-resident: j-begin snapshots the VS into registers
\ (Lvsnap), the back edges reconcile to that snapshot (Lvrecon) and branch on
\ x17 — never a VS register, so the reconcile reload can't clobber the flag.
: j-begin  Lvsnap @ BL,  c-pushcp ;

: j-again  Lvrecon @ BL,  Lcfpop @ BL,  $14000000 $3FFFFFF c-bback ;

: j-untilx                                 \ shared tail: reconcile + cbz x17,top
   Lvrecon @ BL,
   Lcfpop @ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $B4000011 LIT64,  9 9 10 ORR,  Lcemit @ BL, ;

: j-until  $D1002273 c-emitw  $F9400271 c-emitw  j-untilx ;   \ pop flag -> x17

: j-while c-popflag  c-pushcp  $B4000009 c-emitw ;

: j-repeat Lvrecon @ BL,  Lcfpop @ BL,  14 9 0 ADDI,  Lcfpop @ BL,  $14000000 $3FFFFFF c-bback
   12 0 MOVZ,  12 DATA VSP-CELL STR,                  \ exit path arrives from
   12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,           \ WHILE's spilled state
   9 14 0 ADDI,  Lpat @ BL, ;

: j-frame                                \ pop limit/start, push a loop frame
   3506446963 c-emitw  4181721705 c-emitw  3506446963 c-emitw  4181721706 c-emitw
   4181780107 c-emitw  3548179820 c-emitw  2434269580 c-emitw  2333344140 c-emitw
   4177527177 c-emitw  4177528202 c-emitw  2432697707 c-emitw  4177585803 c-emitw ;

: j-lvopen                               \ open a LEAVE-chain level: LVH[LVD]=0, LVD++
   9 DATA LVD-CELL LDR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   12 0 MOVZ,  12 10 0 STR,
   9 9 1 ADDI,  9 DATA LVD-CELL STR, ;

: j-lvleave                              \ chain a B placeholder on the current level
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,
   9 10 0 LDR,
   11 CP DBASE SUB,  11 10 0 STR,
   Lcemit @ BL, ;

: j-do
   j-frame  j-lvopen  c-pushcp ;

: j-?do                                  \ DO, but skip the loop when limit = start
   j-frame  j-lvopen
   $EB0A013F c-emitw                     \ cmp x9,x10  (start/limit still live)
   $54000041 c-emitw                     \ b.ne +8 (over the skip placeholder)
   j-lvleave
   c-pushcp ;

: j-leave  j-lvleave ;

: j-unloop                               \ pop one loop frame, no branch
   4181780107 c-emitw  3506439531 c-emitw  4177585803 c-emitw ;

: j-loopend                              \ shared LOOP/+LOOP tail: pop frame, patch
   14 CP 0 ADDI,                         \ LEAVE/?DO skips to the pop point, LVD--
   4181780107 c-emitw  3506439531 c-emitw  4177585803 c-emitw
   9 DATA LVD-CELL LDR,  9 9 1 SUBI,  9 DATA LVD-CELL STR,
   10 9 3 LSLI,  10 10 LVH-OFF ADDI,  10 DATA 10 ADD,  9 10 0 LDR,
   Lbchain @ BL, ;

: j-loop
   4181780107 c-emitw  3506439531 c-emitw  3548179820 c-emitw  2434269580 c-emitw  2333344140 c-emitw
   4181721481 c-emitw  4181722506 c-emitw  2432697641 c-emitw  4177527177 c-emitw  3943301439 c-emitw
   Lcfpop @ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000B LIT64,  9 9 10 ORR,  Lcemit @ BL,
   j-loopend ;

: j-+loop                                \ index += n; loop while (old-limit) and
   $D1002273 c-emitw  $F9400269 c-emitw  \ (new-limit) agree in sign (ANS crossing)
   4181780107 c-emitw  3506439531 c-emitw  3548179820 c-emitw  2434269580 c-emitw  2333344140 c-emitw
   $F940018D c-emitw                     \ ldr x13,[x12]      index
   4181722506 c-emitw                    \ ldr x10,[x12,#8]   limit
   $CB0A01AF c-emitw                     \ sub x15,x13,x10    old
   $8B0901AD c-emitw                     \ add x13,x13,x9
   $F900018D c-emitw                     \ str x13,[x12]
   $CB0A01B0 c-emitw                     \ sub x16,x13,x10    new
   $CA1001EF c-emitw                     \ eor x15,x15,x16
   $F10001FF c-emitw                     \ cmp x15,#0
   Lcfpop @ BL,
   10 9 CP SUB,  10 10 2 ASRI,  5 $7FFFF LIT64,  10 10 5 AND,  10 10 5 LSLI,
   9 $5400000A LIT64,  9 9 10 ORR,  Lcemit @ BL,       \ b.ge loop-top
   j-loopend ;

: j-i
   4181780107 c-emitw  3506439531 c-emitw  3548179820 c-emitw  2434269580 c-emitw  2333344140 c-emitw
   4181721481 c-emitw  4177527401 c-emitw  2432705139 c-emitw ;

: j-j                                    \ outer loop index: frame[LOOPSP-2]
   4181780107 c-emitw  $D100096B c-emitw 3548179820 c-emitw  2434269580 c-emitw  2333344140 c-emitw
   4181721481 c-emitw  4177527401 c-emitw  2432705139 c-emitw ;

\ >R R> R@ — the user return stack lives in a data-region stack ([x20+RSTK-OFF],
\ depth at [x20+RSP-CELL]), like the DO/LOOP frames: x25/x28 belong to the
\ compiler, and word frames on the machine stack would unbalance the epilogue.
: w-ldrx {: rt rn off :}                               \ ( rt rn off -- w ) ldr rt,[rn,#off]
   $F9400000  off 8 / 10 lshift or  rn 5 lshift or  rt or ;

: w-strx {: rt rn off :}                               \ ( rt rn off -- w ) str rt,[rn,#off]
   $F9000000  off 8 / 10 lshift or  rn 5 lshift or  rt or ;

: j-tor                                                \ pop data -> push RSTK
   $D1002273 c-emitw  $F9400269 c-emitw                \ sub x19,#8 ; ldr x9,[x19]
   10 20 RSP-CELL w-ldrx c-emitw
   $8B0A0E8B c-emitw                                   \ add x11,x20,x10,lsl#3
   9 11 RSTK-OFF w-strx c-emitw
   $9100054A c-emitw                                   \ add x10,x10,#1
   10 20 RSP-CELL w-strx c-emitw ;

: j-rpop                                               \ x9 = RSTK top, x10 = RSP-1
   10 20 RSP-CELL w-ldrx c-emitw
   $D100054A c-emitw                                   \ sub x10,x10,#1
   $8B0A0E8B c-emitw                                   \ add x11,x20,x10,lsl#3
   9 11 RSTK-OFF w-ldrx c-emitw ;

: j-rfrom  j-rpop                                      \ pop RSTK -> push data
   10 20 RSP-CELL w-strx c-emitw
   $F9000269 c-emitw  $91002273 c-emitw ;              \ str x9,[x19] ; add x19,#8

: j-rfetch  j-rpop                                     \ peek RSTK -> push data
   $F9000269 c-emitw  $91002273 c-emitw ;

\ EXIT: emit a placeholder word holding the PREVIOUS chain offset (0 = end);
\ `;` walks the chain and patches each into `b epilogue`. RECURSE: bl back to
\ the current word's entry (PEND slot.addr) — every word has the standard
\ prologue/epilogue, so calling into the open definition is well-formed.
: j-exit
   9 DATA EXITH-CELL LDR,                              \ x9 = prev chain offset
   10 CP DBASE SUB,  10 DATA EXITH-CELL STR,           \ head := this placeholder
   Lcemit @ BL, ;

: j-recurse
   9 PEND 0 LDR,  $94000000 $3FFFFFF c-bback ;         \ bl entry

: j-does
   NEWLBL {: dok :}
   12 DATA LOCF-CELL LDR,  12 dok CBZ,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   dok LBL,
   $1000008A c-emitw                     \ adr x10, #+16 = D (4 words ahead)
   16 20 DOESP-CELL w-ldrx c-emitw       \ x16 = Ldoespatch runtime addr
   $D63F0200 c-emitw                     \ blr x16
   j-exit                                \ word 4: the defining word ends here
   9 $D10043FF LIT64,  Lcemit @ BL,      \ D: fresh prologue for the does-body
   9 $F90003FE LIT64,  Lcemit @ BL, ;

: emit-doespatch
   Ldoespatch @ LBL,
   SP SP 32 SUBI,  30 SP 0 STR,  10 SP 8 STR,
   2 3 MOVZ,  Lprot @ BL,                                \ region -> RW
   10 SP 8 LDR,
   11 DATA LASTC-CELL LDR,                               \ created slot
   12 11 0 LDR,  13 11 8 LDR,  12 12 13 ADD,             \ x12 = RET addr
   14 10 12 SUB,  14 14 2 ASRI,                          \ delta words (negative)
   5 $3FFFFFF LIT64,  14 14 5 AND,
   5 $14000000 LIT64,  14 14 5 ORR,                      \ b D
   14 12 0 STRW,
   12 SP 16 STR,
   2 5 MOVZ,  Lprot @ BL,                                \ region -> RX
   12 SP 16 LDR,
   12 DCCVAU,  DSB-ISH,  12 ICIVAU,  DSB-ISH,  ISB,      \ flush the patched line
   30 SP 0 LDR,  SP SP 32 ADDI,  RET, ;

\ ---- interpret-mode defining words ----
\ record defining words for the checker: append the kind token + run the hook
\ (verdict ignored — create/variable/constant always publish).
: c-defhook  NEWLBL {: kwv klen nohk :}
   11 kwv @ ADR,  12 klen MOVZ,  Lbcs @ BL,
   9 DATA HOOK-CELL LDR,  9 nohk CBZ,
   10 DATA BODYBUF-OFF ADDI,  10 g-push
   10 DATA BODYLEN-CELL LDR,  10 g-push
   SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
   10 g-pop
   nohk LBL, ;

: emit-create
   NEWLBL NEWLBL NEWLBL {: ncp ncpd nokind :}
   Lcreate @ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,  15 SP 8 STR,
   2 3 MOVZ,  Lprot @ BL,
   Ltok @ BL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  Lbcap @ BL,   \ seed "NAME " for the hook
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   CP 9 0 STR,  TKL 9 16 STR,
   14 DATA CUR-CELL LDR,  14 9 40 STR,
   10 9 24 ADDI,  11 TKA 0 ADDI,  12 TKL 0 ADDI,
   ncp LBL,  12 ncpd CBZ,  13 11 0 LDRB,  13 10 0 STRB,
      10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  ncp B,
   ncpd LBL,
   11 DATA 0 LDR,
   c-lit
   9 W-RET LIT64,  Lcemit @ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,
   9 DATA LASTC-CELL STR,
   NDICT NDICT 1 ADDI,  9 9 0 LDR,                      \ x9 = body start for the flush
   2 5 MOVZ,  Lprot @ BL,  Lflush @ BL,
   15 SP 8 LDR,  15 nokind CBZ,
   Lkwcreate 6 c-defhook
   nokind LBL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: c-create  15 1 MOVZ,  Lcreate @ BL, ;

: c-variable  c-create
   7 DATA 0 LDR,  7 7 8 ADDI,  7 DATA 0 STR, ;

: c-constant
   NEWLBL NEWLBL {: kcp kcd :}
   2 3 MOVZ,  Lprot @ BL,  Ltok @ BL,
   12 0 MOVZ,  12 DATA BODYLEN-CELL STR,  Lbcap @ BL,   \ seed "NAME " for the hook
   15 g-pop                                             \ n -> x15 AFTER Lbcap (it clobbers x15)
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   CP 9 0 STR,  TKL 9 16 STR,  14 DATA CUR-CELL LDR,  14 9 40 STR,
   10 9 24 ADDI,  11 TKA 0 ADDI,  12 TKL 0 ADDI,
   kcp LBL,  12 kcd CBZ,  13 11 0 LDRB,  13 10 0 STRB,
      10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  kcp B,
   kcd LBL,
   11 15 0 ADDI,  c-lit
   9 W-RET LIT64,  Lcemit @ BL,
   9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 0 LDR,  10 CP 10 SUB,  10 10 4 SUBI,  10 9 8 STR,
   NDICT NDICT 1 ADDI,  9 9 0 LDR,                      \ x9 = body start for the flush
   2 5 MOVZ,  Lprot @ BL,  Lflush @ BL,
   Lkwconst 8 c-defhook ;

: c-immediate
   2 3 MOVZ,  Lprot @ BL,
   9 NDICT 0 ADDI,  9 9 1 SUBI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
   10 9 16 LDR,  10 10 $100 ORRI,  10 9 16 STR,
   2 5 MOVZ,  Lprot @ BL, ;

: c-postpone
   NEWLBL NEWLBL NEWLBL {: pok pnimm pdone :}
   Ltok @ BL,  9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,
   13 pok CBNZ,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT SYS,
   pok LBL,
   14 13 2 ANDI,  14 pnimm CBZ,
      c-call  pdone B,
   pnimm LBL,
      c-lit
      9 Lkwcompc @ ADR,  10 8 MOVZ,  Lfind @ BL,
      c-call
   pdone LBL, ;

: c-isdq
   INP INP 1 ADDI,  13 INP 0 ADDI,
   NEWLBL NEWLBL NEWLBL NEWLBL {: sl sd cl cd :}
   sl LBL,  9 INP 0 LDRB,  9 $22 CMPI,  C-EQ sd BCOND,  INP INP 1 ADDI,  sl B,
   sd LBL,  10 INP 13 SUB,  INP INP 1 ADDI,
   12 DATA 0 LDR,  15 12 0 ADDI,                        \ x12 = DP, x15 = string base
   11 13 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 12 0 STRB,  12 12 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   12 DATA 0 STR,                                       \ allot: DP advances past the copy
   15 g-push  10 g-push ;

: c-char   Ltok @ BL,  9 TKA 0 LDRB,  9 g-push ;

: c-bchar  Ltok @ BL,  11 TKA 0 LDRB,  Lvpushc @ BL, ;

: c-tick
   NEWLBL {: tk :}
   Ltok @ BL,  9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,
   13 tk CBZ,  11 g-push  tk LBL, ;

: c-btick
   NEWLBL {: bk :}
   Ltok @ BL,  9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,
   13 bk CBZ,  c-lit  bk LBL, ;

: c-lbrace
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL
   NEWLBL NEWLBL
   {: cfok xok havef nl nd nstore nlok noti ncp ncd pl pd tsl tsd :}
   5 CFSTK-OFF LIT64,  10 DBASE 5 ADD,  11 10 0 LDR,  11 cfok CBZ,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   cfok LBL,
   11 DATA EXITH-CELL LDR,  11 xok CBZ,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
      0 75 MOVZ,  NR-EXIT SYS,
   xok LBL,
   12 DATA LOCF-CELL LDR,  12 havef CBNZ,
      9 $D10203FF LIT64,  Lcemit @ BL,
      9 128 MOVZ,  9 DATA LOCF-CELL STR,
   havef LBL,
   6 DATA LOCN-CELL LDR,
   nl LBL,
      Ltok @ BL,  0 nd CBZ,
      Lbcap @ BL,                                          \ locals reach the checker too
      0 Lkwendloc @ ADR,  1 2 MOVZ,  Lkwcmp @ BL,  0 nstore CBZ,  nd B,
      nstore LBL,
      11 DATA LOCN-CELL LDR,  11 16 CMPI,  C-LT nlok BCOND,
         0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
         0 75 MOVZ,  NR-EXIT SYS,
      nlok LBL,
      TKL 1 CMPI,  C-NE noti BCOND,
      13 TKA 0 LDRB,  14 $20 MOVZ,  13 13 14 ORR,  13 105 CMPI,  C-NE noti BCOND,
         0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
         0 75 MOVZ,  NR-EXIT SYS,
      noti LBL,
      11 DATA LOCN-CELL LDR,  12 LOC-REC MOVZ,  11 11 12 MUL,  11 11 LOCNAMES ADDI,  11 DATA 11 ADD,
      14 0 MOVZ,
      tsl LBL,  14 TKL CMP,  C-GE tsd BCOND,
         15 TKA 14 ADD,  15 15 0 LDRB,  15 58 CMPI,  C-EQ tsd BCOND,
         14 14 1 ADDI,  tsl B,
      tsd LBL,
      14 11 0 STR,
      12 11 8 ADDI,  13 TKA 0 ADDI,
      ncp LBL,  14 ncd CBZ,  15 13 0 LDRB, 15 12 0 STRB, 12 12 1 ADDI, 13 13 1 ADDI, 14 14 1 SUBI, ncp B,
      ncd LBL,
      11 DATA LOCN-CELL LDR,  11 11 1 ADDI,  11 DATA LOCN-CELL STR,
      nl B,
   nd LBL,
   13 DATA LOCN-CELL LDR,  13 13 1 SUBI,
   pl LBL,
      13 6 CMP,  C-LT pd BCOND,
      9 $D1002273 LIT64,  Lcemit @ BL,
      9 $F9400269 LIT64,  Lcemit @ BL,
      9 $F90003E9 LIT64,  14 13 10 LSLI,  9 9 14 ORR,  Lcemit @ BL,
      13 13 1 SUBI,  pl B,
   pd LBL, ;

: c-sdq
   NEWLBL NEWLBL NEWLBL NEWLBL {: sl sd cl cd :}
   INP INP 1 ADDI,  13 INP 0 ADDI,
   sl LBL,  9 INP 0 LDRB,  9 $22 CMPI,  C-EQ sd BCOND,  INP INP 1 ADDI,  sl B,
   sd LBL,  10 INP 13 SUB,  INP INP 1 ADDI,
   15 CP 0 ADDI,  9 $14000000 LIT64,  Lcemit @ BL,
   12 CP 0 ADDI,
   11 13 0 ADDI,  9 10 0 ADDI,
   cl LBL,  9 cd CBZ,
      14 11 0 LDRB,  14 28 0 STRB,  28 28 1 ADDI,  11 11 1 ADDI,  9 9 1 SUBI,  cl B,
   cd LBL,
   28 28 3 ADDI,  5 -4 LIT64,  28 28 5 AND,
   9 15 0 ADDI,  15 10 0 ADDI,  Lpat @ BL,
   11 12 0 ADDI,  c-lit
   11 15 0 ADDI,  c-lit ;
variable CFSK

: cf-entry {: lmainlbl kwvar kwlen hxt :}
   NEWLBL CFSK !
   0 kwvar @ ADR,  1 kwlen MOVZ,  Lkwcmp @ BL,
   0 CFSK @ CBZ,
   Lvspill @ BL,
   hxt execute  lmainlbl B,
   CFSK @ LBL, ;
s" cf-entry" s" n n n n --" trust

\ cfn-entry: keyword case WITHOUT the spill — loop words manage the VS
\ themselves (BEGIN snapshots it, AGAIN/REPEAT reconcile to the snapshot).
: cfn-entry {: lmainlbl kwvar kwlen hxt :}
   NEWLBL CFSK !
   0 kwvar @ ADR,  1 kwlen MOVZ,  Lkwcmp @ BL,
   0 CFSK @ CBZ,
   hxt execute  lmainlbl B,
   CFSK @ LBL, ;
s" cfn-entry" s" n n n n --" trust
\ ---- MAIN, split into emission-ordered phases sharing label variables ----
variable Lmain  variable Lexit  variable Lcompile  variable Lundef
variable CFSK2

\ cfb-entry: branch keywords (if/until/while) with the condition on the VS —
\ a REGISTER top branches directly (no spill + memory pop); con or empty falls
\ back to the spill + pop path. hxtr gets the condition reg in x14.
: cfb-entry {: lmainlbl kwvar kwlen hxtm hxtr :}
   NEWLBL CFSK !  NEWLBL CFSK2 !
   0 kwvar @ ADR,  1 kwlen MOVZ,  Lkwcmp @ BL,
   0 CFSK @ CBZ,
   6 DATA VSP-CELL LDR,  6 CFSK2 @ CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 CFSK2 @ CBNZ,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 LDR,
   SP SP 16 SUBI,  14 SP 8 STR,
   Lvdrop @ BL,  Lvspill @ BL,
   14 SP 8 LDR,  SP SP 16 ADDI,
   hxtr execute
   lmainlbl B,
   CFSK2 @ LBL,
   Lvspill @ BL,
   hxtm execute
   lmainlbl B,
   CFSK @ LBL, ;
s" cfb-entry" s" n n n n n --" trust

\ cfbn-entry: like cfb-entry but the register path neither spills nor saves —
\ UNTIL reconciles to the BEGIN snapshot itself; the condition reg x14 survives
\ Lvdrop (which only relabels the VS, no emission).
: cfbn-entry {: lmainlbl kwvar kwlen hxtm hxtr :}
   NEWLBL CFSK !  NEWLBL CFSK2 !
   0 kwvar @ ADR,  1 kwlen MOVZ,  Lkwcmp @ BL,
   0 CFSK @ CBZ,
   6 DATA VSP-CELL LDR,  6 CFSK2 @ CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 CFSK2 @ CBNZ,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  14 8 0 LDR,
   Lvdrop @ BL,
   hxtr execute
   lmainlbl B,
   CFSK2 @ LBL,
   Lvspill @ BL,
   hxtm execute
   lmainlbl B,
   CFSK @ LBL, ;
s" cfbn-entry" s" n n n n n --" trust

: j-ifr  c-pushcp  8 $B4000000 LIT64,  9 8 14 ORR,  Lcemit @ BL, ;

: j-whiler  j-ifr ;

: j-untilr                                 \ reg flag -> x17 first: the reconcile
   8 $AA0003F1 LIT64,  7 14 16 LSLI,  9 8 7 ORR,  Lcemit @ BL,   \ may reload into it
   j-untilx ;

: em-startup
   NEWLBL NEWLBL {: scopy scdone :}
   Lanchor @ LBL,
   RBASE Lanchor @ ADR,
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,  SP SP 2048 SUBI,
   XDS SP 0 ADDI,
   0 0 MOVZ,  1 REGION LIT64,  2 3 MOVZ,  3 $1002 LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   DBASE 0 0 ADDI,
   CP DBASE 0 ADDI,  5 DICT-SIZE LIT64,  CP CP 5 ADD,
   11 Lncount @ ADR,  11 11 0 LDR,  NDICT 11 0 ADDI,
   9 Ldict @ ADR,  10 DBASE 0 ADDI,  12 11 0 ADDI,
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
   0 0 MOVZ,  1 DATA-SIZE LIT64,  2 3 MOVZ,  3 $1002 LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,
   20 0 RBASE-CELL STR,
   DATA 0 0 ADDI,
   XDS DATA S0-CELL STR,
   5 DATA-START MOVZ,  7 DATA 5 ADD,  7 DATA DP-CELL STR,
   9 0 MOVZ,  9 DATA HND-CELL STR,
   9 0 MOVZ,  9 DATA CUR-CELL STR,
   9 1 MOVZ,  9 DATA WIDN-CELL STR,
   9 0 MOVZ,  9 DATA HOOK-CELL STR,
   9 0 MOVZ,  9 DATA LOOPSP-CELL STR,
   g-install-crash
   9 Ldoespatch @ ADR,  9 DATA DOESP-CELL STR,
   9 Lcreate @ ADR,  9 DATA CREATEP-CELL STR,
   emit-source
   PEND 0 MOVZ, ;

: em-comment
   NEWLBL NEWLBL NEWLBL {: notcom skln skpar :}
   Lmain @ LBL,
      Ltok @ BL,  0 Lexit @ CBZ,
      TKL 1 CMPI,  C-NE notcom BCOND,
      9 TKA 0 LDRB,
      9 92 CMPI,  C-EQ skln BCOND,
      9 40 CMPI,  C-NE notcom BCOND,
      skpar LBL,  INP INE CMP,  C-GE Lmain @ BCOND,
         9 INP 0 LDRB,  INP INP 1 ADDI,  9 41 CMPI,  C-NE skpar BCOND,  Lmain @ B,
      skln LBL,   INP INE CMP,  C-GE Lmain @ BCOND,
         9 INP 0 LDRB,  INP INP 1 ADDI,  9 10 CMPI,  C-NE skln BCOND,  Lmain @ B,
      notcom LBL,
      PEND Lcompile @ CBNZ, ;

: em-interpret
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: lnotcolon ncopy ncd lnotnum cpok ndok :}
   TKL 1 CMPI,  C-NE lnotcolon BCOND,
   9 TKA 0 LDRB,  9 58 CMPI,  C-NE lnotcolon BCOND,
      2 3 MOVZ,  Lprot @ BL,
      9 REGION $4000 - LIT64,  9 DBASE 9 ADD,  CP 9 CMP,  C-LT cpok BCOND,
         0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
         0 76 MOVZ,  NR-EXIT SYS,
      cpok LBL,
      9 1280 MOVZ,  NDICT 9 CMP,  C-LT ndok BCOND,      \ slot 1280 = CFSTK-OFF
         0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
         0 77 MOVZ,  NR-EXIT SYS,
      ndok LBL,
      Ltok @ BL,
      9 NDICT 0 ADDI,  10 DREC MOVZ,  9 9 10 MUL,  9 DBASE 9 ADD,
      PEND 9 0 ADDI,
      CP 9 0 STR,  TKL 9 16 STR,
      14 DATA CUR-CELL LDR,  14 9 40 STR,
      10 9 24 ADDI,  11 TKA 0 ADDI,  12 TKL 0 ADDI,
      ncopy LBL,  12 ncd CBZ,
         13 11 0 LDRB,  13 10 0 STRB,
         10 10 1 ADDI,  11 11 1 ADDI,  12 12 1 SUBI,  ncopy B,
      ncd LBL,
      5 CFSTK-OFF LIT64,  11 DBASE 5 ADD,  12 0 MOVZ,  12 11 0 STR,
      12 0 MOVZ,  12 DATA LOCN-CELL STR,  12 DATA LOCF-CELL STR,
      12 0 MOVZ,  12 DATA BODYLEN-CELL STR,
      Lbcap @ BL,             \ seed with the NAME (checker records certified sigs)
      12 0 MOVZ,  12 DATA VSP-CELL STR,  12 DATA SNAPSP-CELL STR,
      12 DATA EXITH-CELL STR,  12 DATA LVD-CELL STR,
      12 VRALL MOVZ,  12 DATA VRFREE-CELL STR,
      9 $D10043FF LIT64,  Lcemit @ BL,
      9 $F90003FE LIT64,  Lcemit @ BL,
      Lmain @ B,
   lnotcolon LBL,
   Lmain @ Lkwcreate 6 ['] c-create   cf-entry
   Lmain @ Lkwvar    8 ['] c-variable cf-entry
   Lmain @ Lkwconst  8 ['] c-constant cf-entry
   Lmain @ Lkwtick   1 ['] c-tick     cf-entry
   Lmain @ Lkwchar   4 ['] c-char     cf-entry
   Lmain @ Lkwimm    9 ['] c-immediate cf-entry
   Lmain @ Lkwsq     2 ['] c-isdq     cf-entry
   9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lnum @ BL,
   12 lnotnum CBZ,  11 g-push  Lmain @ B,
   lnotnum LBL,
   9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,
   13 Lundef @ CBZ,
   11 BLR,  Lmain @ B, ;
s" em-interpret" s" --" trust

: em-compile
   NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL NEWLBL {: lnotsemi notd nohook rejected notloc lmem lcnotnum notimm :}
   Lcompile @ LBL,
      TKL 1 CMPI,  C-NE lnotsemi BCOND,
      9 TKA 0 LDRB,  9 59 CMPI,  C-NE lnotsemi BCOND,
         Lvspill @ BL,
         14 CP 0 ADDI,  9 DATA EXITH-CELL LDR,  Lbchain @ BL,
         12 DATA LOCF-CELL LDR,  12 notd CBZ,
            9 $910003FF LIT64,  14 12 10 LSLI,  9 9 14 ORR,  Lcemit @ BL,
         notd LBL,
         9 $F94003FE LIT64,  Lcemit @ BL,
         9 $910043FF LIT64,  Lcemit @ BL,
         9 W-RET LIT64,  Lcemit @ BL,
         9 PEND 0 LDR,  10 CP 9 SUB,  10 10 4 SUBI,  10 PEND 8 STR,
         2 5 MOVZ,  Lprot @ BL,  Lflush @ BL,
         9 DATA HOOK-CELL LDR,  9 nohook CBZ,
            10 DATA BODYBUF-OFF ADDI,  10 g-push
            10 DATA BODYLEN-CELL LDR,  10 g-push
            SP SP 16 SUBI,  30 SP 0 STR,  9 BLR,  30 SP 0 LDR,  SP SP 16 ADDI,
            10 g-pop  10 rejected CBZ,
         nohook LBL,
            NDICT NDICT 1 ADDI,
         rejected LBL,
         PEND 0 MOVZ,
         Lmain @ B,
      lnotsemi LBL,
      Lbcap @ BL,
      Lmain @ Lkwif     2 ['] j-if   ['] j-ifr    cfb-entry
      Lmain @ Lkwthen   4 ['] j-then   cf-entry
      Lmain @ Lkwelse   4 ['] j-else   cf-entry
      Lmain @ Lkwbegin  5 ['] j-begin  cfn-entry
      Lmain @ Lkwuntil  5 ['] j-until ['] j-untilr cfbn-entry
      Lmain @ Lkwagain  5 ['] j-again  cfn-entry
      Lmain @ Lkwwhile  5 ['] j-while ['] j-whiler cfb-entry
      Lmain @ Lkwrepeat 6 ['] j-repeat cfn-entry
      Lmain @ Lkwsq     2 ['] c-sdq    cf-entry
      Lmain @ Lkwbtick  3 ['] c-btick  cf-entry
      Lmain @ Lkwbchar  6 ['] c-bchar  cf-entry
      Lmain @ Lkwpost   8 ['] c-postpone cf-entry
      Lmain @ Lkwdoes   5 ['] j-does     cf-entry
      Lmain @ Lkwdo     2 ['] j-do     cf-entry
      Lmain @ Lkwloop   4 ['] j-loop   cf-entry
      Lmain @ Lkwi      1 ['] j-i      cf-entry
      Lmain @ Lkwtor    2 ['] j-tor    cf-entry
      Lmain @ Lkwrfrom  2 ['] j-rfrom  cf-entry
      Lmain @ Lkwrfet   2 ['] j-rfetch cf-entry
      Lmain @ Lkwexit   4 ['] j-exit    cf-entry
      Lmain @ Lkwrec    7 ['] j-recurse cf-entry
      Lmain @ Lkwqdo    3 ['] j-?do     cf-entry
      Lmain @ Lkwploop  5 ['] j-+loop   cf-entry
      Lmain @ Lkwj      1 ['] j-j       cf-entry
      Lmain @ Lkwleave  5 ['] j-leave   cf-entry
      Lmain @ Lkwunloop 6 ['] j-unloop  cf-entry
      Lmain @ Lkwlbrace 2 ['] c-lbrace cf-entry
      Lloc-find @ BL,  0 0 CMPI,  C-LT notloc BCOND,
         Lvralloc @ BL,  14 lmem CBZ,
         9 $F94003E0 LIT64,  9 9 14 ORR,  7 0 10 LSLI,  9 9 7 ORR,  Lcemit @ BL,
         Lvpushr @ BL,
         Lmain @ B,
         lmem LBL,
         Lvspill @ BL,
         9 $F94003E9 LIT64,  14 0 10 LSLI,  9 9 14 ORR,  Lcemit @ BL,
         9 W-PUSH0 LIT64,  Lcemit @ BL,  9 W-PUSH1 LIT64,  Lcemit @ BL,
         Lmain @ B,
      notloc LBL,
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lnum @ BL,
      12 lcnotnum CBZ,  Lvpushc @ BL,  Lmain @ B,
      lcnotnum LBL,
      Lmain @ Lkwplus  1 ['] f+ ['] e+ vop-entry
      Lmain @ Lkwminus 1 ['] f- ['] e- vop-entry
      Lmain @ Lkwstar  1 ['] f* ['] e* vop-entry
      Lmain @ Lkwand2  3 ['] fand ['] eand vop-entry
      Lmain @ Lkwor2   2 ['] for2 ['] eor2 vop-entry
      Lmain @ Lkwxor2  3 ['] fxor2 ['] exor vop-entry
      Lmain @ Lkwdup2  3 1 ['] xdup  vshuf-entry
      Lmain @ Lkwdrop2 4 1 ['] xdrop vshuf-entry
      Lmain @ Lkwswap2 4 2 ['] xswap vshuf-entry
      Lmain @ Lkwover2 4 2 ['] xover vshuf-entry
      Lmain @ Lkwnip2  3 2 ['] xnip  vshuf-entry
      Lmain @ Lkweq2 1 0 vcmp-entry
      Lmain @ Lkwne2 2 1 vcmp-entry
      Lmain @ Lkwlt2 1 11 vcmp-entry
      Lmain @ Lkwgt2 1 12 vcmp-entry
      Lmain @ Lkwle2 2 13 vcmp-entry
      Lmain @ Lkwge2 2 10 vcmp-entry
      Lmain @ Lkwinc  2 ['] fu1+ ['] eu1+ vun-entry
      Lmain @ Lkwdec  2 ['] fu1- ['] eu1- vun-entry
      Lmain @ Lkwzeq  2 ['] fu0= ['] eu0= vun-entry
      Lmain @ Lkwzlt  2 ['] fu0< ['] eu0< vun-entry
      Lmain @ Lkwneg2 6 ['] funeg ['] euneg vun-entry
      Lmain @ Lkwinv2 6 ['] fuinv ['] euinv vun-entry
      Lvspill @ BL,
      9 TKA 0 ADDI,  10 TKL 0 ADDI,  Lfind @ BL,
      13 Lundef @ CBZ,
      14 13 2 ANDI,  14 notimm CBZ,
         SP SP 16 SUBI,  30 SP 0 STR,  11 SP 8 STR,
         2 5 MOVZ,  Lprot @ BL,
         11 SP 8 LDR,  11 BLR,
         2 3 MOVZ,  Lprot @ BL,
         30 SP 0 LDR,  SP SP 16 ADDI,
         Lmain @ B,
      notimm LBL,
      c-call  Lmain @ B,
   Lundef @ LBL,
      0 2 MOVZ,  1 TKA 0 ADDI,  2 TKL 0 ADDI,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT SYS,
   Lexit @ LBL,
      0 0 MOVZ,  NR-EXIT SYS, ;
s" em-compile" s" --" trust

: emit-main
   NEWLBL Lmain !  NEWLBL Lexit !  NEWLBL Lcompile !  NEWLBL Lundef !
   em-startup  em-comment  em-interpret  em-compile ;
s" emit-main" s" --" trust
variable SRCA

: EMIT-FORTH {: a u :}
   u SRCN !  a SRCA !
   ASM-INIT  0 #PL !  0 PNP !
   NEWLBL Lanchor !  NEWLBL Lfind !  NEWLBL Lnum !  NEWLBL Ldict !  NEWLBL Lsrc !
   NEWLBL Lcemit !  NEWLBL Ltok !  NEWLBL Lprot !  NEWLBL Lflush !  NEWLBL Lncount !
   NEWLBL Lbcap !  NEWLBL Lbcs !
   NEWLBL Lcfpush !  NEWLBL Lcfpop !  NEWLBL Lpat !  NEWLBL Lkwcmp !
   NEWLBL Lkwif !  NEWLBL Lkwthen !  NEWLBL Lkwelse !  NEWLBL Lkwbegin !
   NEWLBL Lkwuntil !  NEWLBL Lkwagain !  NEWLBL Lkwwhile !  NEWLBL Lkwrepeat !
   NEWLBL Lkwcreate !  NEWLBL Lkwvar !  NEWLBL Lkwsq !
   NEWLBL Lkwtick !  NEWLBL Lkwbtick !
   NEWLBL Lkwlbrace !  NEWLBL Lkwendloc !  NEWLBL Lloc-find !  NEWLBL Lkwconst !
   NEWLBL Lkwdo !  NEWLBL Lkwloop !  NEWLBL Lkwi !
   NEWLBL Lkwtor !  NEWLBL Lkwrfrom !  NEWLBL Lkwrfet !
   NEWLBL Lkwexit !  NEWLBL Lkwrec !
   NEWLBL Lkwqdo !  NEWLBL Lkwploop !  NEWLBL Lkwj !  NEWLBL Lkwleave !  NEWLBL Lkwunloop !
   NEWLBL Lkwchar !  NEWLBL Lkwbchar !
   NEWLBL Lkwimm !  NEWLBL Lkwpost !  NEWLBL Lkwcompc !  NEWLBL Lkwdoes !
   NEWLBL Lbchain !  NEWLBL Lcreate !  NEWLBL Ldoespatch !
   NEWLBL Lcrashh !  NEWLBL Lhex !  NEWLBL Lhdr !
   NEWLBL Lprofh !  NEWLBL Lprofdump !
   NEWLBL Lvspill !  NEWLBL Lvlitpush !  NEWLBL Lvpushc !
   NEWLBL Lvtop2c !  NEWLBL Lvfoldput !
   NEWLBL Lvralloc !  NEWLBL Lvmovk !  NEWLBL Lvforcek !  NEWLBL Lvbinprep !  NEWLBL Lvpushr !
   NEWLBL Lvdrop !  NEWLBL Lvswapx !  NEWLBL Lvnipx !  NEWLBL Lvcopy !
   NEWLBL Lvsnap !  NEWLBL Lvrecon !
   NEWLBL Lkwplus !  NEWLBL Lkwminus !  NEWLBL Lkwstar !
   NEWLBL Lkwand2 !  NEWLBL Lkwor2 !  NEWLBL Lkwxor2 !
   NEWLBL Lkwdup2 !  NEWLBL Lkwdrop2 !  NEWLBL Lkwswap2 !
   NEWLBL Lkwover2 !  NEWLBL Lkwnip2 !
   NEWLBL Lkweq2 !  NEWLBL Lkwne2 !  NEWLBL Lkwlt2 !
   NEWLBL Lkwgt2 !  NEWLBL Lkwle2 !  NEWLBL Lkwge2 !
   NEWLBL Lkwinc !  NEWLBL Lkwdec !  NEWLBL Lkwzeq !
   NEWLBL Lkwzlt !  NEWLBL Lkwneg2 !  NEWLBL Lkwinv2 !
   emit-main
   emit-prims  emit-prof-prims  emit-fp-prims  emit-cemit  emit-bcap  emit-tok  emit-prot  emit-flush  emit-find  emit-num
   emit-create  emit-doespatch
   emit-cf-helpers  emit-loc-find  emit-kwdata  emit-foldkw  emit-shufkw  emit-cmpkw  emit-unkw  emit-crash-handler  emit-hex
   emit-profdump  emit-prof  emit-vsjit
   emit-dict
   Lsrc @ LBL,  SRCA @ SRCN @ BYTES, ;
s" emit-forth" s" n n --" trust
