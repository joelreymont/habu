\ icode.f — A64ICODE, the ARM64 single-pass assembler: EMITW appends asm.f
\ encodings to the CODE window; labels resolve B/BL, B.cond, CBZ/CBNZ, ADR and
\ LOFF, sites, backward at once and forward by backpatch. ASM-CP counts words and
\ branch deltas are in words.
\ No locals: the Gforth recovery compiler checks this file before the native
\ checker exists.
require src/arch/arm64/asm.f
require src/core/layout-buffer.f
require src/core/dynamic-storage.f
package A64ICODE
using A64ASM
public

\ ---- the window this assembler emits into ------------------------------------
\ An image is three parts emitted in this order (src/habu/habu2.f
\ ENGINE-EMIT:FORTH); the order keeps the first bound true.
\ 1. Engine code. The baked-source label LSRC follows it (habu2.f
\    EMIT-SOURCE-BYTES) and the startup region reaches LSRC by ADR
\    (C-SOURCE-BAKED, C-SOURCE-APPEND-LSRC under EM-STARTUP), so engine code is
\    bounded by the 21-bit ADR byte field, ADR-HI; past it ?ADR dies `icode: adr
\    out of reach`.
\ 2. Baked source, the whole of src/habu/stage2.f READ-SRC: refused at emission
\    by CODE-CAP-WORDS. The boot arena maps IBUFSZ plus the source's length.
\ 3. AOT payload (EMIT-AOT-SEED). No PC-relative field addresses it (habu2.f
\    TADR, adds the text base to a LOFF, offset), so it has one aggregate budget,
\    AOT-SECTION-CAP, which src/habu/aot-decl.f AOT-SECTION enforces.
\ This file loads before layout.f and habu2.f, so the terms are spelled out here.
\ CODE is a demand-paged anonymous mapping taken on first emit and nothing walks
\ the span, so the window costs reserved address space only. A B/BL across the
\ window must fit REL26-HI words (128 MiB).
\
\   AOT-SECTION-CAP >= AOT-BUF:AOT-BLOB-CAP (= CODE-BAND:BYTES)
\                      + 136 header + 272 section table + 40 scalars
\                      + AOT-FILE:CLOSURE-CAP
\
\ A full-band blob plus aot-file.f's framing is the largest artifact. Below the
\ bound AOT-BLOB-CAP and the site buffer (AOT-DSITE-MAX * 4) are unreachable and
\ the per-section refusal is dead. test/aot-data-sites.f "the section budget
\ admits a full code band" pins it; at CODE-BAND:BYTES 30,404,608 the minimum is
\ 30,536,128.
$1E00000 constant AOT-SECTION-CAP  \ aggregate payload budget, including framing/alignment
$2300000 constant CODE-CAP-BYTES   \ ADR-HI ($100000) + SOURCE-ARENA-CAP ($400000) + AOT-SECTION-CAP
CODE-CAP-BYTES 4 / constant CODE-CAP-WORDS
\ The fixup tables FXS, FXN and FXK hold $2000 rows each: the native recovery
\ build's identity shadow branches pass 4096 labels, and their pending fixups
\ share these tables. LBLP and FXH grow on demand.
$2000 constant ICODE-TAB-CELLS
$3 constant ICODE-TAB-COUNT
ICODE-TAB-CELLS ICODE-TAB-COUNT * cells constant ICODE-TAB-BYTES
72 constant ICODE-EXIT-RC
PTR-VARIABLE CODE-A
PTR-VARIABLE ICODE-TAB-A
variable ASM-CP
variable I-SITE
variable I-LBL
variable I-KIND
variable I-COND
variable I-RD
variable I-X
variable I-N
variable I-W

: CODE-ALLOC ( -- ptr u8 )
   CODE-CAP-BYTES map-anon 0 <> if
      drop s" icode: code mmap failed" ICODE-EXIT-RC die then ;

: CODE ( -- ptr u8 )
   CODE-A @ NULL-PTR = IF CODE-ALLOC CODE-A ! THEN
   CODE-A @ ;

: ICODE-TAB-ALLOC ( -- ptr n )
   ICODE-TAB-BYTES map-anon 0 <> if
      drop s" icode: table mmap failed" ICODE-EXIT-RC die then ;

: ICODE-TABS ( -- ptr n )
   ICODE-TAB-A @ NULL-PTR = IF ICODE-TAB-ALLOC ICODE-TAB-A ! THEN
   ICODE-TAB-A @ ;

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
PTR-VARIABLE EP
: EP@ ( -- ptr u8 ) EP @ ;

: EMITW ( n -- )
   I-W !
   1 ASM-CP?  ASM-CP @ CW@ EP !
   I-W @ $FF and EP@ c!  I-W @ $8 rshift $FF and EP@ 1 CODE-BYTE+ c!
   I-W @ $10 rshift $FF and EP@ 2 CODE-BYTE+ c!
   I-W @ $18 rshift $FF and EP@ 3 CODE-BYTE+ c!
   ASM-CP @ 1 + ASM-CP ! ;

: PATCH ( n n -- )
   I-N ! I-W !
   I-N @ CW@ EP !
   I-W @ $FF and EP@ c@ or EP@ c!
   I-W @ $8 rshift $FF and EP@ 1 CODE-BYTE+ c@ or EP@ 1 CODE-BYTE+ c!
   I-W @ $10 rshift $FF and EP@ 2 CODE-BYTE+ c@ or EP@ 2 CODE-BYTE+ c!
   I-W @ $18 rshift $FF and EP@ 3 CODE-BYTE+ c@ or EP@ 3 CODE-BYTE+ c! ;
\ LBLP[label] is the label's word position, or -1 while unbound.
DYNAMIC-BUFFER LBLP n
variable NLBL
\ Fixup rows: FXS site word position, FXN next slot (pending chain or free list),
\ FXK kind; FXH[label] heads that label's pending chain, the target's only
\ record. A kind is validated before any table or code mutation.
0 constant FX-B26                              \ B/BL: 26-bit word delta
1 constant FX-B19                              \ cond/CBZ/CBNZ: 19-bit word delta
2 constant FX-ADR                              \ ADR: 21-bit byte delta
3 constant FX-LOFF                             \ movz/movk pair: label byte offset
: FXS ( -- ptr n ) 0 ICODE-TAB ;
: FXN ( -- ptr n ) 1 ICODE-TAB ;
: FXK ( -- ptr n ) 2 ICODE-TAB ;
DYNAMIC-BUFFER FXH n
variable NFX
variable FX-FREE
variable FX-NEW

: FX-INIT ( -- )
   -1 FX-FREE !
   0 FX-NEW ! ;

: ASM-INIT ( -- )
   ARESET
   LBLP-RELEASE FXH-RELEASE
   0 NLBL !
   0 NFX !
   FX-INIT ;

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

: LBL ( -- label )
   NLBL @ dup 1 + LBLP-RESERVE
   dup 1 + FXH-RESERVE
   dup LBLP -1 swap !
   dup FXH -1 swap !
   dup 1 + NLBL ! >LABEL ;

: LABEL@ ( ptr n -- label ) @ >LABEL ;

: LABEL! ( label ptr n -- ) swap LABEL>N swap ! ;

: LBL-BOUND? ( label -- bool )
   LABEL>N LBLP @ 0 >= ;

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
   I-LBL @ FXH @ I-W @ cells FXN + !
   I-W @ I-LBL @ FXH ! ;

: FX-KIND-OK? ( n -- bool )
   dup FX-B26 =
   over FX-B19 = or
   over FX-ADR = or
   swap FX-LOFF = or ;

: ?FX-KIND ( -- )
   I-KIND @ FX-KIND-OK? if exit then
   s" icode: invalid fixup kind" ICODE-EXIT-RC die ;

: FX+ ( n label n -- )
   I-KIND ! LABEL>N I-LBL ! I-SITE !
   ?FX-KIND
   FX-TAKE I-W !
   I-SITE @ I-W @ cells FXS + !
   I-KIND @ I-W @ cells FXK + !
   I-W @ FX-LINK ;

: D26 ( n -- n )  $3FFFFFF and ;                               \ B/BL: bits 0..25 (26-bit field)

: D19 ( n -- n )  $7FFFF and 5 lshift ;                         \ cond/CBZ: bits 5..23
\ A delta d is in reach iff LO <= d < HI. The bounds match bootstrap/cg/asm.fs
\ (?REL26, ?REL19, ENC-ADR) so the native assembler refuses where the seed does.
\ The code window overshoots REL19 and ADR reach, so every emit and patch site
\ checks a delta before masking it; the mask would wrap to a wrong target.
      -33554432 constant REL26-LO   \ -2^25 words, inclusive  (B/BL 26-bit field)
       33554432 constant REL26-HI   \  2^25 words, exclusive
        -262144 constant REL19-LO   \ -2^18 words, inclusive  (cond/CBZ/CBNZ 19-bit field)
         262144 constant REL19-HI   \  2^18 words, exclusive
       -1048576 constant ADR-LO     \ -2^20 bytes, inclusive  (ADR 21-bit field)
        1048576 constant ADR-HI     \  2^20 bytes, exclusive

: REL26-OK? ( n -- bool )  dup REL26-LO >=  swap REL26-HI <  and ;
: REL19-OK? ( n -- bool )  dup REL19-LO >=  swap REL19-HI <  and ;
: ADR-OK?   ( n -- bool )  dup ADR-LO   >=  swap ADR-HI   <  and ;

\ ---- what a reach refusal names ----------------------------------------------
\ A reach refusal prints site, target, delta and limit in bytes as one message.
\ Every caller of ?REL26, ?REL19 and ?ADR sets I-RSITE and I-RTARGET just before
\ the check: BR-EMIT and ADR, from ASM-CP and LBLP, FX-PATCH from FXS and ASM-CP.
variable I-RSITE                    \ byte offset of the instruction under assembly
variable I-RTARGET                  \ byte offset it has to reach
variable I-RDELTA                   \ signed byte distance it would need
variable I-RLIMIT                   \ the field's exclusive bound, in bytes

\ Both build texts load this file before src/lib/fmt.f (tools/bootstrap.sh
\ SRC_COMMON, tools/build-fixpoint.f BF-APPEND-COMMON), so it renders its own
\ decimals.
$14 constant IDEC-CAP               \ -9223372036854775808: nineteen digits and a sign
$30 constant IDEC-ZERO
$2D constant IDEC-MINUS
create IDEC-BUF IDEC-CAP allot
variable IDEC-V                     \ magnitude left to render, never negative
variable IDEC-CY                    \ 1 while a negative value's borrowed +1 is pending
variable IDEC-NEG
variable IDEC-D
variable IDEC-N                     \ fill cursor: digits land backwards from the end

: IDEC-PUT ( n -- )
   IDEC-N @ 1 - IDEC-N !
   IDEC-BUF IDEC-N @ CODE-BYTE+ c! ;

\ MIN-N has no positive twin: a negative v renders from -(v+1) with a pending +1
\ carry, so `/` and `mod` see only non-negative values and the recovery seed and
\ the native engine render alike.
: IDEC-DIGIT ( -- )
   IDEC-V @ 10 mod IDEC-CY @ + IDEC-D !
   IDEC-D @ 10 = IF 0 IDEC-D ! ELSE 0 IDEC-CY ! THEN
   IDEC-D @ IDEC-ZERO + IDEC-PUT
   IDEC-V @ 10 / IDEC-V ! ;

: IDEC-SET ( n -- )
   IDEC-V !  IDEC-CAP IDEC-N !
   IDEC-V @ 0 < IF  0 IDEC-V @ 1 + -  IDEC-V !  1 IDEC-CY !  1 IDEC-NEG !
   ELSE  0 IDEC-CY !  0 IDEC-NEG !  THEN ;

: IDEC$ ( n -- ptr u8 n )
   IDEC-SET
   BEGIN IDEC-DIGIT  IDEC-V @ 0 = IDEC-CY @ 0 = and UNTIL
   IDEC-NEG @ 0 <> IF IDEC-MINUS IDEC-PUT THEN
   IDEC-BUF IDEC-N @ CODE-BYTE+  IDEC-CAP IDEC-N @ - ;

\ `die` takes one span, so the line is composed first; separate writes would
\ interleave with other stderr output. Outgrowing IMSG-CAP is a refusal, not a
\ truncated line.
$20 constant IMSG-PREFIX-CAP        \ `icode: cond branch out of reach` is the longest
$8 constant IMSG-LABEL-CAP          \ ` target=` is the longest field label
4 constant IMSG-FIELDS
IMSG-PREFIX-CAP IMSG-FIELDS IMSG-LABEL-CAP IDEC-CAP + * + constant IMSG-CAP
create IMSG-BUF IMSG-CAP allot
variable IMSG-N
PTR-VARIABLE IMSG-A
variable IMSG-U
variable IMSG-V

: IMSG-A@ ( -- ptr u8 ) IMSG-A @ ;

: IMSG+ ( ptr u8 n -- )
   IMSG-U ! IMSG-A !
   \ Any source can call IMSG+, so the length is compared with the room left: in
   \ a sum, a length near the maximum cell wraps back under the capacity.
   IMSG-U @ 0 <  IMSG-U @ IMSG-CAP IMSG-N @ - >  or if
      s" icode: a reach refusal outgrew its own message buffer" ICODE-EXIT-RC die
   then
   0 BEGIN dup IMSG-U @ < WHILE
      dup IMSG-A@ swap CODE-BYTE+ c@  IMSG-BUF IMSG-N @ CODE-BYTE+ c!
      IMSG-N @ 1 + IMSG-N !
      1 +
   REPEAT drop ;

: IMSG-FIELD ( ptr u8 n n -- )
   IMSG-V !  IMSG+  IMSG-V @ IDEC$ IMSG+ ;

: REACH-DIE ( ptr u8 n -- )
   0 IMSG-N !  IMSG+
   s"  site="   I-RSITE   @ IMSG-FIELD
   s"  target=" I-RTARGET @ IMSG-FIELD
   s"  delta="  I-RDELTA  @ IMSG-FIELD
   s"  limit="  I-RLIMIT  @ IMSG-FIELD
   IMSG-BUF IMSG-N @ ICODE-EXIT-RC die ;

: ?REL26 ( n -- n )  dup REL26-OK? if exit then
   dup $4 * I-RDELTA !  REL26-HI $4 * I-RLIMIT !
   s" icode: branch out of reach" REACH-DIE ;

: ?REL19 ( n -- n )  dup REL19-OK? if exit then
   dup $4 * I-RDELTA !  REL19-HI $4 * I-RLIMIT !
   s" icode: cond branch out of reach" REACH-DIE ;

: ?ADR ( n -- n )  dup ADR-OK? if exit then
   dup I-RDELTA !  ADR-HI I-RLIMIT !
   s" icode: adr out of reach" REACH-DIE ;
\ LOFF, carries a label's non-negative byte offset from the code base in the two
\ 16-bit lanes of a movz/movk pair, not a delta: the reader adds the runtime code
\ base, so a label past ADR reach is still addressable. The window must fit
\ under LOFF-HI for the form to cover the buffer.
$10 constant MOVW-LANE-BITS         \ one movz/movk immediate lane
2 constant LOFF-LANES               \ the pair LOFF, emits
0 constant LOFF-LO                  \ inclusive: a code-buffer byte offset is never negative
1 LOFF-LANES MOVW-LANE-BITS * lshift constant LOFF-HI   \ exclusive: 2^32 bytes

: LOFF-OK? ( n -- bool )  dup LOFF-LO >=  swap LOFF-HI <  and ;

: ?LOFF ( n -- n )  dup LOFF-OK? if exit then  s" icode: label offset out of reach" ICODE-EXIT-RC die ;
\ BR-EMIT expects BBASE, the branch encoded with a zero delta, and BKIND.
variable BBASE  variable BKIND

: BR-EMIT ( label -- )
   LABEL>N I-LBL !
   I-LBL @ LBLP @  dup 0 < IF              \ pos on stack (0< isn't a standalone prim)
     drop  ASM-CP @ I-LBL @ >LABEL BKIND @ FX+  BBASE @ EMITW
   ELSE  dup $4 * I-RTARGET !  ASM-CP @ $4 * I-RSITE !
     ASM-CP @ -  BKIND @ FX-B26 = IF ?REL26 D26 ELSE ?REL19 D19 THEN  BBASE @ or EMITW  THEN ;

\ A branch base word is its asm.f encoder applied to a zero delta, so ENC-BCOND
\ and ENC-CBZ/ENC-CBNZ own the bit layout and screen the condition and register
\ before this layer folds the delta in.
: B, ( label -- )  0 ENC-B  BBASE !  FX-B26 BKIND !  BR-EMIT ;

: BL, ( label -- )  0 ENC-BL BBASE !  FX-B26 BKIND !  BR-EMIT ;

: BCOND, ( n label -- )
   LABEL>N I-LBL ! I-COND !  0 I-COND @ ENC-BCOND BBASE !  FX-B19 BKIND !  I-LBL @ >LABEL BR-EMIT ;

: CBZ, ( n label -- )
   LABEL>N I-LBL ! I-RD !  I-RD @ 0 ENC-CBZ BBASE !  FX-B19 BKIND !  I-LBL @ >LABEL BR-EMIT ;

: CBNZ, ( n label -- )
   LABEL>N I-LBL ! I-RD !  I-RD @ 0 ENC-CBNZ BBASE !  FX-B19 BKIND !  I-LBL @ >LABEL BR-EMIT ;

: ADR, ( n label -- )
   LABEL>N I-LBL ! I-RD !
   I-LBL @ LBLP @ dup 0 < IF
     drop  ASM-CP @ I-LBL @ >LABEL FX-ADR FX+  I-RD @ 0 ENC-ADR EMITW
   ELSE  dup $4 * I-RTARGET !  ASM-CP @ $4 * I-RSITE !
     ASM-CP @ - $4 *  ?ADR  I-RD @ swap ENC-ADR EMITW  THEN ;

\ LOFF, is two words on either side of the label: a forward site reserves both
\ lanes before the offset is known, and a site's width may not depend on where
\ it was emitted. The base words come from MOVZHW and MOVKHW, as ADR,'s from
\ ENC-ADR.
: LOFF, ( n label -- )
   LABEL>N I-LBL ! I-RD !
   I-LBL @ LBLP @ dup 0 < IF
     drop  ASM-CP @ I-LBL @ >LABEL FX-LOFF FX+
     I-RD @ 0 0 MOVZHW EMITW  I-RD @ 0 1 MOVKHW EMITW
   ELSE  $4 * ?LOFF I-X !
     I-RD @ I-X @ $FFFF and 0 MOVZHW EMITW
     I-RD @ I-X @ MOVW-LANE-BITS rshift $FFFF and 1 MOVKHW EMITW  THEN ;
variable LBI

: FX-ENC ( n n -- n )                            \ ( delta kind -- patchbits )
   dup FX-B26 = if drop ?REL26 D26 exit then
   dup FX-B19 = if drop ?REL19 D19 exit then
   dup FX-ADR = if drop $4 * ?ADR ENC-ADRD exit then
   drop s" icode: invalid fixup kind" ICODE-EXIT-RC die ;

\ A reach die precedes the current fixup's PATCH, so the failing site word and
\ its slots stay untouched. A chain is not atomic: LBL, clears FXH first, and
\ fixups patched before the failing one keep their deltas and are freed.
\ FX-LOFF binds from ASM-CP alone and patches two words, both after ?LOFF.
: FX-LOFF-PATCH ( -- )
   ASM-CP @ $4 * ?LOFF I-X !
   0 I-X @ $FFFF and 0 MOVZHW  LBI @ cells FXS + @ PATCH
   0 I-X @ MOVW-LANE-BITS rshift $FFFF and 1 MOVKHW  LBI @ cells FXS + @ 1 + PATCH ;

: FX-PATCH ( -- )
   LBI @ cells FXK + @ FX-LOFF = if FX-LOFF-PATCH exit then
   LBI @ cells FXS + @ $4 * I-RSITE !
   ASM-CP @ $4 * I-RTARGET !
   ASM-CP @ LBI @ cells FXS + @ -                \ delta = here - site (words)
   LBI @ cells FXK + @ FX-ENC
   LBI @ cells FXS + @ PATCH ;

: LBL, ( label -- )
   dup ?LBL-UNBOUND LABEL>N I-LBL !
   I-LBL @ FXH @ LBI !
   -1 I-LBL @ FXH !
   ASM-CP @ I-LBL @ LBLP !
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
   LABEL>N LBLP @ dup 0 < if s" icode: DLBL forward ref" ICODE-EXIT-RC die then  $4 * DCQ, ;
PTR-VARIABLE BYP
PTR-VARIABLE BYA
variable BYU
: BYP@ ( -- ptr u8 ) BYP @ ;
: BYA@ ( -- ptr u8 ) BYA @ ;

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

\ Each call is its own run: the bytes, then NUL pad to the next word. A span
\ written as one, such as a message and its newline, is one call.
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

;using
;package
