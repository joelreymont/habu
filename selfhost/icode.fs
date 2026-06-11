\ icode.fs — minimal single-pass assembler in the STANDALONE's Forth: emit ARM64
\ words (asm.fs encoders) into a code buffer, define labels, and resolve B/CBZ/CBNZ
\ branches (backward immediately, forward via backpatch). Next codegen-port step
\ after the encoders. CP counts WORDS; deltas are word-relative (ARM64 PC-relative).
create CODE 131072 allot   variable CP
: ARESET 0 CP ! ;
: CW@ {: w :}  CODE w 4 * + ;                      \ byte addr of word w
\ NB: the standalone mis-reads a SECOND {: :} locals group, so these use a variable
\ for the byte pointer instead of a 2nd local (cf. VAR-OF / BR-EMIT bugs).
variable EP
: EMITW {: u :}  CP @ CW@ EP !                       \ store u LE at CODE[CP], CP++
   u 255 and EP @ c!  u 8 rshift 255 and EP @ 1 + c!  u 16 rshift 255 and EP @ 2 + c!  u 24 rshift 255 and EP @ 3 + c!
   CP @ 1 + CP ! ;
: PATCH {: u w :}  w CW@ EP !                        \ OR u into the word already at w (delta bits)
   u 255 and EP @ c@ or EP @ c!  u 8 rshift 255 and EP @ 1 + c@ or EP @ 1 + c!
   u 16 rshift 255 and EP @ 2 + c@ or EP @ 2 + c!  u 24 rshift 255 and EP @ 3 + c@ or EP @ 3 + c! ;
\ labels: LBLP[id] = defining word pos, or -1 if pending (1024 — engine-builder sized)
create LBLP 1024 cells allot   variable NLBL
\ fixups: site word-pos, target label, kind (0=B26, 1=cond/CBZ 19-bit, 2=ADR)
create FXS 2048 cells allot   create FXL 2048 cells allot   create FXK 2048 cells allot   variable NFX
: ASM-INIT  ARESET  0 NLBL !  0 NFX !  0 BEGIN dup cells LBLP + -1 swap ! 1 + dup 1023 > UNTIL drop ;
: NEWLBL?  NLBL @ 1023 > IF s" icode: out of labels" 72 die THEN ;
: FX?      NFX @ 2047 > IF s" icode: out of fixups" 72 die THEN ;
: NEWLBL  NEWLBL?  NLBL @ dup 1 + NLBL ! ;           \ -- id
: FX+ {: site lbl kind :}  FX?                       \ record a forward fixup
   site NFX @ cells FXS + !  lbl NFX @ cells FXL + !  kind NFX @ cells FXK + !  NFX @ 1 + NFX ! ;
\ encode a word delta into the branch word for a kind
: D26  16777215 and ;                                \ B/BL: bits 0..25
: D19  524287 and 5 lshift ;                         \ cond/CBZ: bits 5..23
\ emit a branch (base already encoded with delta=0) to a label; resolve or defer
variable BBASE  variable BKIND
: BR-EMIT {: lbl :}                                  \ BBASE/BKIND set; emits + records if fwd
   lbl cells LBLP + @  dup 0 < IF                     \ pos on stack (0< isn't a standalone prim)
     drop  CP @ lbl BKIND @ FX+  BBASE @ EMITW
   ELSE  CP @ -  BKIND @ 0= IF D26 ELSE D19 THEN  BBASE @ or EMITW  THEN ;
: B,    {: lbl :}  335544320  BBASE !  0 BKIND !  lbl BR-EMIT ;
: BL,   {: lbl :}  2483027968 BBASE !  0 BKIND !  lbl BR-EMIT ;
: BCOND, {: cond lbl :}  1409286144 cond or BBASE !  1 BKIND !  lbl BR-EMIT ;
: CBZ,  {: rt lbl :}  3019898880 rt or BBASE !  1 BKIND !  lbl BR-EMIT ;
: CBNZ, {: rt lbl :}  3036676096 rt or BBASE !  1 BKIND !  lbl BR-EMIT ;
\ adr rd, label: PC-relative address (kind-2 fixup when forward)
: ADR, {: rd lbl :}
   lbl cells LBLP + @ dup 0 < IF
     drop  CP @ lbl 2 FX+  rd 0 ENC-ADR EMITW
   ELSE  CP @ - 4 *  rd swap ENC-ADR EMITW  THEN ;
\ define a label here; backpatch all pending fixups that target it
variable LBI
: LBL, {: lbl :}  CP @ lbl cells LBLP + !
   0 LBI ! BEGIN LBI @ NFX @ < WHILE
     LBI @ cells FXL + @ lbl = IF
       CP @ LBI @ cells FXS + @ -                    \ delta = here - site (words)
       LBI @ cells FXK + @ 0 = IF D26 ELSE
         LBI @ cells FXK + @ 1 = IF D19 ELSE 4 * ENC-ADRD THEN THEN
       LBI @ cells FXS + @ PATCH
     THEN
     LBI @ 1 + LBI !
   REPEAT ;
\ --- data layer ---
: DCQ, {: x :}  x $FFFFFFFF and EMITW  x 32 rshift EMITW ;   \ one 64-bit cell, LE
: DLBL, {: lbl :}                                            \ cell = label's byte offset
   lbl cells LBLP + @ dup 0 < IF s" icode: DLBL forward ref" 72 die THEN  4 * DCQ, ;
variable BYP
: BYTES, {: a u :}  CP @ 4 * CODE + BYP !                    \ raw bytes, zero-padded to 4
   0 BEGIN dup u < WHILE  dup a + c@  BYP @ c!  BYP @ 1 + BYP !  1 + REPEAT drop
   BEGIN BYP @ CODE - 3 and 0 <> WHILE  0 BYP @ c!  BYP @ 1 + BYP !  REPEAT
   BYP @ CODE - 4 / CP ! ;
\ --- 64-bit constant synthesis: minimal MOVZ/MOVN + MOVK chain (byte-parity with
\ caf's LIT-Z/LIT-N in src/cg/asm.fs — the stage2 fixpoint depends on it) ---
variable LCH  variable LFI  variable LCI
: CHUNK16 {: x n :}  x n 16 * rshift $FFFF and ;
: NZC {: x :}  0 LCH !  0 BEGIN dup 4 < WHILE
     x over CHUNK16 0 <> IF LCH @ 1 + LCH ! THEN  1 + REPEAT drop  LCH @ ;
: NFC {: x :}  0 LCH !  0 BEGIN dup 4 < WHILE
     x over CHUNK16 $FFFF <> IF LCH @ 1 + LCH ! THEN  1 + REPEAT drop  LCH @ ;
: MAX1 {: n :}  n 1 < IF 1 ELSE n THEN ;
: 1STNZ {: x :}  -1 LFI !  0 BEGIN dup 4 < WHILE
     LFI @ 0 < IF x over CHUNK16 0 <> IF dup LFI ! THEN THEN  1 + REPEAT drop
   LFI @ 0 < IF 0 ELSE LFI @ THEN ;
: 1STNF {: x :}  -1 LFI !  0 BEGIN dup 4 < WHILE
     LFI @ 0 < IF x over CHUNK16 $FFFF <> IF dup LFI ! THEN THEN  1 + REPEAT drop
   LFI @ 0 < IF 0 ELSE LFI @ THEN ;
: LITZ {: rd x :}  x 1STNZ LFI !
   rd  x LFI @ CHUNK16  LFI @ MOVZHW EMITW
   0 LCI ! BEGIN LCI @ 4 < WHILE
     LCI @ LFI @ <> IF
       x LCI @ CHUNK16 LCH !
       LCH @ 0 <> IF rd LCH @ LCI @ MOVKHW EMITW THEN THEN
     LCI @ 1 + LCI ! REPEAT ;
: LITN {: rd x :}  x 1STNF LFI !
   rd  x LFI @ CHUNK16 invert $FFFF and  LFI @ MOVNHW EMITW
   0 LCI ! BEGIN LCI @ 4 < WHILE
     LCI @ LFI @ <> IF
       x LCI @ CHUNK16 LCH !
       LCH @ $FFFF <> IF rd LCH @ LCI @ MOVKHW EMITW THEN THEN
     LCI @ 1 + LCI ! REPEAT ;
: LIT64, {: rd x :}  x NFC MAX1  x NZC MAX1  < IF rd x LITN ELSE rd x LITZ THEN ;
: ASM-LEN  CP @ 4 * ;
