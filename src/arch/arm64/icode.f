\ icode.fs — minimal single-pass assembler in the STANDALONE's Forth: emit ARM64
\ words (asm.fs encoders) into a code buffer, define labels, and resolve B/CBZ/CBNZ
\ branches (backward immediately, forward via backpatch). Next codegen-port step
\ after the encoders. CP counts WORDS; deltas are word-relative (ARM64 PC-relative).
$80000 constant CODE-CAP-BYTES
131071 constant CODE-CAP-WORDS
$1002 constant ICODE-MAP-PRIVATE-ANON
2048 constant ICODE-TAB-CELLS
4 constant ICODE-TAB-COUNT
ICODE-TAB-CELLS ICODE-TAB-COUNT * cells constant ICODE-TAB-BYTES
variable CODE-A
variable ICODE-TAB-A
variable CP

: CODE-ALLOC ( -- n )
   0 CODE-CAP-BYTES 3 ICODE-MAP-PRIVATE-ANON -1 0 mmap
   dup 0 < IF s" icode: code mmap failed" 72 die THEN ;

: CODE ( -- ptr u8 )
   CODE-A @ 0= IF CODE-ALLOC CODE-A ! THEN
   CODE-A @ ;
s" CODE" s" -- ptr u8" TRUST

: ICODE-TAB-ALLOC ( -- n )
   0 ICODE-TAB-BYTES 3 ICODE-MAP-PRIVATE-ANON -1 0 mmap
   dup 0 < IF s" icode: table mmap failed" 72 die THEN ;

: ICODE-TABS ( -- ptr n )
   ICODE-TAB-A @ 0= IF ICODE-TAB-ALLOC ICODE-TAB-A ! THEN
   ICODE-TAB-A @ ;
s" ICODE-TABS" s" -- ptr n" TRUST

: ICODE-TAB ( n -- ptr n )
   ICODE-TABS swap ICODE-TAB-CELLS * cells + ;

: ARESET ( -- )
   CODE drop
   ICODE-TABS drop
   0 CP ! ;

: CW@ ( n -- ptr u8 ) {: w :}  CODE w 4 * + ;                      \ byte addr of word w

: CP? ( n -- ) {: n :}  CP @ n + CODE-CAP-WORDS > IF s" icode: code buffer overflow" 72 die THEN ;
\ NB: the standalone mis-reads a SECOND {: :} locals group, so these use a variable
\ for the byte pointer instead of a 2nd local (cf. VAR-OF / BR-EMIT bugs).
variable EP
: EP@ ( -- ptr u8 ) EP 0 ptr-field @ ;

: EMITW ( n -- ) {: u :}  1 CP?  CP @ CW@ EP !                \ store u LE at CODE[CP], CP++
   u 255 and EP@ c!  u 8 rshift 255 and EP@ 1 + c!
   u 16 rshift 255 and EP@ 2 + c!  u 24 rshift 255 and EP@ 3 + c!
   CP @ 1 + CP ! ;

: PATCH ( n n -- ) {: u w :}  w CW@ EP !                        \ OR u into the word already at w (delta bits)
   u 255 and EP@ c@ or EP@ c!  u 8 rshift 255 and EP@ 1 + c@ or EP@ 1 + c!
   u 16 rshift 255 and EP@ 2 + c@ or EP@ 2 + c!  u 24 rshift 255 and EP@ 3 + c@ or EP@ 3 + c! ;
\ labels: LBLP[id] = defining word pos, or -1 if pending.
2048 constant LBL-CAP
\ Keep the historical table names as accessors so emitter code stays readable.
: LBLP ( -- ptr n ) 0 ICODE-TAB ;
variable NLBL
\ fixups: site word-pos, target label, kind (0=B26, 1=cond/CBZ 19-bit, 2=ADR)
: FXS ( -- ptr n ) 1 ICODE-TAB ;
: FXL ( -- ptr n ) 2 ICODE-TAB ;
: FXK ( -- ptr n ) 3 ICODE-TAB ;
variable NFX

: ASM-INIT ( -- )  ARESET  0 NLBL !  0 NFX !  0 BEGIN dup cells LBLP + -1 swap ! 1 + dup LBL-CAP 1- > UNTIL drop ;

: ?LBL ( -- )  NLBL @ LBL-CAP 1- > IF s" icode: out of labels" 72 die THEN ;

: FX? ( -- )  NFX @ 2047 > IF s" icode: out of fixups" 72 die THEN ;

: LBL ( -- n )  ?LBL  NLBL @ dup 1 + NLBL ! ;

: FX+ ( n n n -- ) {: site lbl kind :}  FX?                       \ record a forward fixup
   site NFX @ cells FXS + !  lbl NFX @ cells FXL + !  kind NFX @ cells FXK + !  NFX @ 1 + NFX ! ;

\ encode a word delta into the branch word for a kind
: D26 ( n -- n )  $3FFFFFF and ;                               \ B/BL: bits 0..25 (26-bit field)

: D19 ( n -- n )  524287 and 5 lshift ;                         \ cond/CBZ: bits 5..23
\ emit a branch (base already encoded with delta=0) to a label; resolve or defer
variable BBASE  variable BKIND

: BR-EMIT ( n -- ) {: lbl :}                                  \ BBASE/BKIND set; emits + records if fwd
   lbl cells LBLP + @  dup 0 < IF                     \ pos on stack (0< isn't a standalone prim)
     drop  CP @ lbl BKIND @ FX+  BBASE @ EMITW
   ELSE  CP @ -  BKIND @ 0= IF D26 ELSE D19 THEN  BBASE @ or EMITW  THEN ;

: B, ( n -- ) {: lbl :}  335544320  BBASE !  0 BKIND !  lbl BR-EMIT ;

: BL, ( n -- ) {: lbl :}  2483027968 BBASE !  0 BKIND !  lbl BR-EMIT ;

: BCOND, ( n n -- ) {: cond lbl :}  1409286144 cond or BBASE !  1 BKIND !  lbl BR-EMIT ;

: CBZ, ( n n -- ) {: rt lbl :}  3019898880 rt or BBASE !  1 BKIND !  lbl BR-EMIT ;

: CBNZ, ( n n -- ) {: rt lbl :}  3036676096 rt or BBASE !  1 BKIND !  lbl BR-EMIT ;

\ adr rd, label: PC-relative address (kind-2 fixup when forward)
: ADR, ( n n -- ) {: RD lbl :}
   lbl cells LBLP + @ dup 0 < IF
     drop  CP @ lbl 2 FX+  RD 0 ENC-ADR EMITW
   ELSE  CP @ - 4 *  RD swap ENC-ADR EMITW  THEN ;
\ define a label here; backpatch all pending fixups that target it
variable LBI

: LBL, ( n -- ) {: lbl :}  CP @ lbl cells LBLP + !
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
: DCQ, ( n -- ) {: x :}  x $FFFFFFFF and EMITW  x 32 rshift EMITW ;   \ one 64-bit cell, LE

: DLBL, ( n -- ) {: lbl :}                                            \ cell = label's byte offset
   lbl cells LBLP + @ dup 0 < IF s" icode: DLBL forward ref" 72 die THEN  4 * DCQ, ;
variable BYP
variable BYA
variable BYU
: BYP@ ( -- ptr u8 ) BYP 0 ptr-field @ ;
: BYA@ ( -- ptr u8 ) BYA 0 ptr-field @ ;

: BYTES-ARGS ( ptr u8 n -- )
   BYU !  BYA ! ;

: BYTES-CAP ( -- )
   BYU @ 3 + 4 / CP?  CP @ 4 * CODE + BYP ! ;

: BYTES-COPY ( -- )
   0 BEGIN dup BYU @ < WHILE
      dup BYA@ + c@  BYP@ c!  BYP@ 1 + BYP !  1 +
   REPEAT drop ;

: BYTES-PAD ( -- )
   BEGIN BYP@ CODE - 3 and 0 <> WHILE  0 BYP@ c!  BYP@ 1 + BYP !  REPEAT ;

: BYTES, ( ptr u8 n -- )
   BYTES-ARGS
   BYTES-CAP
   BYTES-COPY
   BYTES-PAD
   BYP@ CODE - 4 / CP ! ;
\ --- 64-bit constant synthesis: minimal MOVZ/MOVN + MOVK chain. The stage2
\ fixpoint depends on this exact encoding policy. ---
variable LCH  variable LFI  variable LCI

: CHUNK16 ( n n -- n ) {: x n :}  x n 16 * rshift $FFFF and ;

: NZC ( n -- n ) {: x :}  0 LCH !  0 BEGIN dup 4 < WHILE
     x over CHUNK16 0 <> IF LCH @ 1 + LCH ! THEN  1 + REPEAT drop  LCH @ ;

: NFC ( n -- n ) {: x :}  0 LCH !  0 BEGIN dup 4 < WHILE
     x over CHUNK16 $FFFF <> IF LCH @ 1 + LCH ! THEN  1 + REPEAT drop  LCH @ ;

: MAX1 ( n -- n ) {: n :}  n 1 < IF 1 ELSE n THEN ;

: 1STNZ ( n -- n ) {: x :}  -1 LFI !  0 BEGIN dup 4 < WHILE
     LFI @ 0 < IF x over CHUNK16 0 <> IF dup LFI ! THEN THEN  1 + REPEAT drop
   LFI @ 0 < IF 0 ELSE LFI @ THEN ;

: 1STNF ( n -- n ) {: x :}  -1 LFI !  0 BEGIN dup 4 < WHILE
     LFI @ 0 < IF x over CHUNK16 $FFFF <> IF dup LFI ! THEN THEN  1 + REPEAT drop
   LFI @ 0 < IF 0 ELSE LFI @ THEN ;

: LITZ ( n n -- ) {: RD x :}  x 1STNZ LFI !
   RD  x LFI @ CHUNK16  LFI @ MOVZHW EMITW
   0 LCI ! BEGIN LCI @ 4 < WHILE
     LCI @ LFI @ <> IF
       x LCI @ CHUNK16 LCH !
       LCH @ 0 <> IF RD LCH @ LCI @ MOVKHW EMITW THEN THEN
     LCI @ 1 + LCI ! REPEAT ;

: LITN ( n n -- ) {: RD x :}  x 1STNF LFI !
   RD  x LFI @ CHUNK16 invert $FFFF and  LFI @ MOVNHW EMITW
   0 LCI ! BEGIN LCI @ 4 < WHILE
     LCI @ LFI @ <> IF
       x LCI @ CHUNK16 LCH !
       LCH @ $FFFF <> IF RD LCH @ LCI @ MOVKHW EMITW THEN THEN
     LCI @ 1 + LCI ! REPEAT ;

: LIT64, ( n n -- ) {: RD x :}  x NFC MAX1  x NZC MAX1  < IF RD x LITN ELSE RD x LITZ THEN ;

: ASM-LEN ( -- n )  CP @ 4 * ;
