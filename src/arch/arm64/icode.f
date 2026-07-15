\ icode.fs — minimal single-pass assembler in the STANDALONE's Forth: emit ARM64
\ words (asm.fs encoders) into a code buffer, define labels, and resolve B/CBZ/CBNZ
\ branches (backward immediately, forward via backpatch). Next codegen-port step
\ after the encoders. ASM-CP counts WORDS; deltas are word-relative (ARM64 PC-relative).
$1ff000 constant CODE-CAP-BYTES  \ MPAGE-CODE-OFF: full 2 MB executable window
CODE-CAP-BYTES 4 / constant CODE-CAP-WORDS  \ derived: guard can never drift from the mmap
$1002 constant ICODE-MAP-PRIVATE-ANON
$1000 constant ICODE-TAB-CELLS
$5 constant ICODE-TAB-COUNT
ICODE-TAB-CELLS ICODE-TAB-COUNT * cells constant ICODE-TAB-BYTES
72 constant ICODE-EXIT-RC
variable CODE-A
variable ICODE-TAB-A
variable ASM-CP
variable I-SITE
variable I-LBL
variable I-KIND
variable I-COND
variable I-RD
variable I-X
variable I-N
variable I-W

: CODE-ALLOC ( -- n )
   0 CODE-CAP-BYTES 3 ICODE-MAP-PRIVATE-ANON -1 0 mmap
   dup 0 < if s" icode: code mmap failed" ICODE-EXIT-RC die then ;

: CODE ( -- ptr u8 )
   CODE-A @ 0= IF CODE-ALLOC CODE-A ! THEN
   CODE-A @ ;
s" CODE" s" -- ptr u8" TRUST

: ICODE-TAB-ALLOC ( -- n )
   0 ICODE-TAB-BYTES 3 ICODE-MAP-PRIVATE-ANON -1 0 mmap
   dup 0 < if s" icode: table mmap failed" ICODE-EXIT-RC die then ;

: ICODE-TABS ( -- ptr n )
   ICODE-TAB-A @ 0= IF ICODE-TAB-ALLOC ICODE-TAB-A ! THEN
   ICODE-TAB-A @ ;
s" ICODE-TABS" s" -- ptr n" TRUST

: ICODE-TAB ( n -- ptr n )
   ICODE-TABS swap ICODE-TAB-CELLS * cells + ;

: ARESET ( -- )
   CODE drop
   ICODE-TABS drop
   0 ASM-CP ! ;

: CODE-BYTE+ ( ptr u8 n -- ptr u8 ) + ;

: CW@ ( n -- ptr u8 )
   $4 * CODE swap CODE-BYTE+ ;

: ASM-CP? ( n -- )
   ASM-CP @ + CODE-CAP-WORDS > if s" icode: code buffer overflow" ICODE-EXIT-RC die then ;
\ Keep stage-source words local-free: the Gforth recovery compiler must check
\ this file before the native checker is available.
variable EP
: EP@ ( -- ptr u8 ) EP 0 ptr-field @ ;

: EMITW ( n -- )
   I-W !
   1 ASM-CP?  ASM-CP @ CW@ EP !        \ store u LE at CODE[ASM-CP], ASM-CP++
   I-W @ $FF and EP@ c!  I-W @ $8 rshift $FF and EP@ 1 CODE-BYTE+ c!
   I-W @ $10 rshift $FF and EP@ 2 CODE-BYTE+ c!
   I-W @ $18 rshift $FF and EP@ 3 CODE-BYTE+ c!
   ASM-CP @ 1 + ASM-CP ! ;

: PATCH ( n n -- )
   I-N ! I-W !
   I-N @ CW@ EP !                        \ OR u into the word already at w (delta bits)
   I-W @ $FF and EP@ c@ or EP@ c!
   I-W @ $8 rshift $FF and EP@ 1 CODE-BYTE+ c@ or EP@ 1 CODE-BYTE+ c!
   I-W @ $10 rshift $FF and EP@ 2 CODE-BYTE+ c@ or EP@ 2 CODE-BYTE+ c!
   I-W @ $18 rshift $FF and EP@ 3 CODE-BYTE+ c@ or EP@ 3 CODE-BYTE+ c! ;
\ labels: LBLP[id] = defining word pos, or -1 while pending.
ICODE-TAB-CELLS constant LBL-CAP
\ Keep the historical table names as accessors so emitter code stays readable.
: LBLP ( -- ptr n ) 0 ICODE-TAB ;
variable NLBL
\ fixups: site word-pos, next slot, kind (0=B26, 1=cond/CBZ 19-bit, 2=ADR),
\ and one pending-chain head per label. Target identity is the owning FXH chain.
: FXS ( -- ptr n ) 1 ICODE-TAB ;
: FXN ( -- ptr n ) 2 ICODE-TAB ;
: FXK ( -- ptr n ) 3 ICODE-TAB ;
: FXH ( -- ptr n ) 4 ICODE-TAB ;
variable NFX
variable FX-FREE
variable FX-NEW

: FX-INIT ( -- )
   -1 FX-FREE !
   0 FX-NEW ! ;

: ASM-INIT ( -- )
   ARESET
   0 NLBL !
   0 NFX !
   FX-INIT
   0 begin
      dup cells LBLP + -1 swap !
      dup cells FXH + -1 swap !
      1 + dup LBL-CAP 1 - >
   until drop ;

: ?LBL ( -- )  NLBL @ LBL-CAP 1- > if s" icode: out of labels" ICODE-EXIT-RC die then ;

: FX-FREE-BAD? ( -- bool )
   FX-NEW @ 0 <
   FX-NEW @ ICODE-TAB-CELLS > or
   FX-FREE @ -1 < or
   FX-FREE @ 0 >= if
      FX-FREE @ FX-NEW @ >= or
   else
      FX-NEW @ ICODE-TAB-CELLS >=
      NFX @ ICODE-TAB-CELLS < and or
   then ;

: FX? ( -- )
   NFX @ 0 < if s" icode: fixup count corrupt" ICODE-EXIT-RC die then
   NFX @ ICODE-TAB-CELLS 1 - > if
      s" icode: out of fixups" ICODE-EXIT-RC die
   then
   FX-FREE-BAD? if s" icode: fixup free list corrupt" ICODE-EXIT-RC die then ;

: LBL ( -- label )  ?LBL  NLBL @ dup 1 + NLBL !  >LABEL ;

: LABEL@ ( ptr n -- label ) @ >LABEL ;

: LABEL! ( label ptr n -- ) swap LABEL>N swap ! ;

: LBL-BOUND? ( label -- bool )
   LABEL>N cells LBLP + @ 0 >= ;

: ?LBL-UNBOUND ( label -- )
   LBL-BOUND? if s" icode: label redefined" ICODE-EXIT-RC die then ;

: FX-TAKE ( -- n )
   FX?
   FX-FREE @ 0 >= if
      FX-FREE @ dup cells FXN + @ FX-FREE !
   else
      FX-NEW @ dup 1 + FX-NEW !
   then
   NFX @ 1 + NFX ! ;

: FX-PUT ( n -- )
   dup FX-FREE @ swap cells FXN + !
   FX-FREE !
   NFX @ 1 - NFX ! ;

: FX-LINK ( n -- )
   I-W !
   I-LBL @ cells FXH + @ I-W @ cells FXN + !
   I-W @ I-LBL @ cells FXH + ! ;

: FX+ ( n label n -- )
   I-KIND ! LABEL>N I-LBL ! I-SITE !
   FX-TAKE I-W !
   I-SITE @ I-W @ cells FXS + !
   I-KIND @ I-W @ cells FXK + !
   I-W @ FX-LINK ;

\ encode a word delta into the branch word for a kind
: D26 ( n -- n )  $3FFFFFF and ;                               \ B/BL: bits 0..25 (26-bit field)

: D19 ( n -- n )  $7FFFF and 5 lshift ;                         \ cond/CBZ: bits 5..23
\ emit a branch (base already encoded with delta=0) to a label; resolve or defer
variable BBASE  variable BKIND

: BR-EMIT ( label -- )
   LABEL>N I-LBL !                    \ BBASE/BKIND set; emits + records if fwd
   I-LBL @ cells LBLP + @  dup 0 < IF              \ pos on stack (0< isn't a standalone prim)
     drop  ASM-CP @ I-LBL @ >LABEL BKIND @ FX+  BBASE @ EMITW
   ELSE  ASM-CP @ -  BKIND @ 0= IF D26 ELSE D19 THEN  BBASE @ or EMITW  THEN ;

: B, ( label -- )  $14000000  BBASE !  0 BKIND !  BR-EMIT ;

: BL, ( label -- )  $94000000 BBASE !  0 BKIND !  BR-EMIT ;

: BCOND, ( n label -- )
   LABEL>N I-LBL ! I-COND !  $54000000 I-COND @ or BBASE !  1 BKIND !  I-LBL @ >LABEL BR-EMIT ;

: CBZ, ( n label -- )
   LABEL>N I-LBL ! I-RD !  $B4000000 I-RD @ or BBASE !  1 BKIND !  I-LBL @ >LABEL BR-EMIT ;

: CBNZ, ( n label -- )
   LABEL>N I-LBL ! I-RD !  $B5000000 I-RD @ or BBASE !  1 BKIND !  I-LBL @ >LABEL BR-EMIT ;

\ adr rd, label: PC-relative address (kind-2 fixup when forward)
: ADR, ( n label -- )
   LABEL>N I-LBL ! I-RD !
   I-LBL @ cells LBLP + @ dup 0 < IF
     drop  ASM-CP @ I-LBL @ >LABEL 2 FX+  I-RD @ 0 ENC-ADR EMITW
   ELSE  ASM-CP @ - $4 *  I-RD @ swap ENC-ADR EMITW  THEN ;
\ define a label here; backpatch and reclaim only that label's pending chain
variable LBI

: FX-PATCH ( -- )
   ASM-CP @ LBI @ cells FXS + @ -                \ delta = here - site (words)
   LBI @ cells FXK + @ 0 = if D26 else
      LBI @ cells FXK + @ 1 = if D19 else $4 * ENC-ADRD then
   then
   LBI @ cells FXS + @ PATCH ;

: LBL, ( label -- )
   dup ?LBL-UNBOUND LABEL>N I-LBL !
   I-LBL @ cells FXH + @ LBI !
   -1 I-LBL @ cells FXH + !
   ASM-CP @ I-LBL @ cells LBLP + !
   begin LBI @ 0 >= while
      FX-PATCH
      LBI @ cells FXN + @ I-N !
      LBI @ FX-PUT
      I-N @ LBI !
   repeat ;

\ --- data layer ---
: DCQ, ( n -- )
   dup $FFFFFFFF and EMITW  $20 rshift EMITW ;   \ one 64-bit cell, LE

: DLBL, ( label -- )                                  \ cell = label's byte offset
   LABEL>N cells LBLP + @ dup 0 < if s" icode: DLBL forward ref" ICODE-EXIT-RC die then  $4 * DCQ, ;
variable BYP
variable BYA
variable BYU
: BYP@ ( -- ptr u8 ) BYP 0 ptr-field @ ;
: BYA@ ( -- ptr u8 ) BYA 0 ptr-field @ ;

: BYTES-ARGS ( ptr u8 n -- )
   BYU !  BYA ! ;

: BYTES-CAP ( -- )
   BYU @ 3 + 4 / ASM-CP?  ASM-CP @ $4 * CODE swap CODE-BYTE+ BYP ! ;

: BYTES-COPY ( -- )
   0 BEGIN dup BYU @ < WHILE
      dup BYA@ swap CODE-BYTE+ c@  BYP@ c!  BYP@ 1 CODE-BYTE+ BYP !  1 +
   REPEAT drop ;

: BYTES-PAD ( -- )
   BEGIN BYP@ CODE - 3 and 0 <> WHILE  0 BYP@ c!  BYP@ 1 CODE-BYTE+ BYP !  REPEAT ;

: BYTES, ( ptr u8 n -- )
   BYTES-ARGS
   BYTES-CAP
   BYTES-COPY
   BYTES-PAD
   BYP@ CODE - 4 / ASM-CP ! ;
\ --- 64-bit constant synthesis: minimal MOVZ/MOVN + MOVK chain. The stage2
\ fixpoint depends on this exact encoding policy. ---
variable LIT-CH  variable LFI  variable LCI

: CHUNK16 ( n n -- n )  $10 * rshift $FFFF and ;

: NZC ( n -- n )
   I-X !  0 LIT-CH !  0 BEGIN dup 4 < WHILE
     I-X @ over CHUNK16 0 <> IF LIT-CH @ 1 + LIT-CH ! THEN  1 + REPEAT drop  LIT-CH @ ;

: NFC ( n -- n )
   I-X !  0 LIT-CH !  0 BEGIN dup 4 < WHILE
     I-X @ over CHUNK16 $FFFF <> IF LIT-CH @ 1 + LIT-CH ! THEN  1 + REPEAT drop  LIT-CH @ ;

: MAX1 ( n -- n )  dup 1 < IF drop 1 THEN ;

: 1STNZ ( n -- n )
   I-X !  -1 LFI !  0 BEGIN dup 4 < WHILE
     LFI @ 0 < IF I-X @ over CHUNK16 0 <> IF dup LFI ! THEN THEN  1 + REPEAT drop
   LFI @ 0 < IF 0 ELSE LFI @ THEN ;

: 1STNF ( n -- n )
   I-X !  -1 LFI !  0 BEGIN dup 4 < WHILE
     LFI @ 0 < IF I-X @ over CHUNK16 $FFFF <> IF dup LFI ! THEN THEN  1 + REPEAT drop
   LFI @ 0 < IF 0 ELSE LFI @ THEN ;

: LITZ ( n n -- )
   I-X ! I-RD !  I-X @ 1STNZ LFI !
   I-RD @  I-X @ LFI @ CHUNK16  LFI @ MOVZHW EMITW
   0 LCI ! BEGIN LCI @ 4 < WHILE
     LCI @ LFI @ <> IF
      I-X @ LCI @ CHUNK16 LIT-CH !
      LIT-CH @ 0 <> IF I-RD @ LIT-CH @ LCI @ MOVKHW EMITW THEN THEN
     LCI @ 1 + LCI ! REPEAT ;

: LITN ( n n -- )
   I-X ! I-RD !  I-X @ 1STNF LFI !
   I-RD @  I-X @ LFI @ CHUNK16 invert $FFFF and  LFI @ MOVNHW EMITW
   0 LCI ! BEGIN LCI @ 4 < WHILE
     LCI @ LFI @ <> IF
      I-X @ LCI @ CHUNK16 LIT-CH !
      LIT-CH @ $FFFF <> IF I-RD @ LIT-CH @ LCI @ MOVKHW EMITW THEN THEN
     LCI @ 1 + LCI ! REPEAT ;

: LIT64, ( n n -- )
   I-X ! I-RD !
   I-X @ NFC MAX1  I-X @ NZC MAX1  < IF I-RD @ I-X @ LITN ELSE I-RD @ I-X @ LITZ THEN ;

: ASM-LEN ( -- n )  ASM-CP @ $4 * ;
