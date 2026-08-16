\ icode.fs — minimal single-pass assembler in the STANDALONE's Forth: emit ARM64
\ words (asm.fs encoders) into a code buffer, define labels, and resolve B/CBZ/CBNZ
\ branches (backward immediately, forward via backpatch). Next codegen-port step
\ after the encoders. ASM-CP counts WORDS; deltas are word-relative (ARM64 PC-relative).
\ The encoders are package A64ASM's public surface, imported here once and called
\ bare, which is what this layer did when they were global names.
using A64ASM

\ ---- the window this assembler emits into ------------------------------------
\ THE WINDOW IS NOT THIS FILE'S NUMBER TO PICK. It holds one emitted image, and
\ an image is THREE parts, each already bounded by a file that is not this one.
\ The parts are emitted in that order (src/habu/habu2.f ENGINE-EMIT:FORTH) and
\ the order is load-bearing: it is what keeps the first bound below true.
\
\ THE ENGINE CODE PART IS BOUNDED BY THIS FILE'S OWN ADR REACH. The baked-source
\ label LSRC is emitted directly after the engine code (src/habu/habu2.f
\ EMIT-SOURCE-BYTES) and the boot path reaches it with an ADR planted in the
\ startup region (C-SOURCE-BAKED and C-SOURCE-APPEND-LSRC, both under
\ EM-STARTUP - the FIRST region). Every ENGINE code byte lies between that site
\ and that label, so engine code past the 21-bit signed ADR byte field cannot
\ address its own baked source: the deferred patch runs ?ADR and dies `icode: adr
\ out of reach` before any word is written. ADR-HI below is that field's
\ exclusive bound, so it is exactly this part's allowance.
\
\ THE BAKED-SOURCE PART IS BOUNDED BY THE BOOT SOURCE ARENA. The same boot path
\ copies LSRC into the source arena a byte at a time and refuses at IBUFSZ
\ (src/habu/layout.f SOURCE-ARENA-CAP) with `hb: source prefix buffer full`,
\ rc 74. A window able to assemble more baked source than the arena can hold
\ would only produce engines that die on their first boot, so IBUFSZ is exactly
\ that part's allowance.
\
\ THE AOT PAYLOAD PART IS BOUNDED BY ITS OWN CAPTURE BUFFERS. It is emitted last
\ (EMIT-AOT-SEED) and carries the compiled blob, the dictionary records, the
\ relocation tables, the name pool and the captured DATA window - megabytes once
\ the compiler chain is captured, and NOT bounded by any reach, because nothing
\ addresses it with a PC-relative field: every reference into it goes through
\ habu2.f's TADR,, which materialises the label's offset with LOFF, below and
\ adds the text base the boot recorded. Its allowance is the sum of the capture
\ buffers' own caps, each of which fails closed on overflow at capture time.
\
\ This file loads before layout.f and habu2.f, so all three terms are spelled out
\ here. test/icode-fixup-test.f holds the window against ADR-HI and IBUFSZ, and
\ habu2.f derives the third from the buffers themselves and dies at load if it
\ disagrees with the constant below - the same shape src/habu/rt.f uses to hold
\ mnem.f's XDS against layout.f's ENGINE-GPR:DSTACK.
\
\ WHAT PAYS FOR IT IS NOTHING THAT IS TOUCHED. CODE is one demand-paged
\ anonymous mapping taken on first emit (CODE-ALLOC), by build drivers only, and
\ nothing walks the span: the image writers copy CODELEN bytes and pad to a
\ content-rounded TEXTSZ, and ARESET rewinds a cursor rather than clearing a
\ buffer. What grows is reserved address space in a build process.
\
\ THE ARCHITECTURAL CEILING IS FAR ABOVE THIS. The window is one contiguous run
\ of code, so a B/BL between its ends must fit the 26-bit signed word field:
\ REL26-HI words, 128 MiB, twenty-five times this window. Past that a single-pass
\ assembler with no veneers could not branch across its own buffer.
\
\ WHY IT MOVED (2026-08-14, dot habu-lift-the-image-42b2b9f4). The old window was
\ $1ff000, chosen as a round 2 MB rather than derived, and the tree had grown into
\ it: the stdin maker engine emitted 2,038,580 bytes of __text against a
\ 2,093,056-byte window, 2.6% clear, and the widening lane's stage build died on
\ this guard. The window was refusing images the boot arena would have accepted,
\ which is the wrong file refusing first.
\
\ WHY IT GREW A THIRD TERM (2026-08-14, dot habu-reach-the-seed-d1326596). The
\ two-term window was measured against an image whose AOT payload was 30 KiB. At
\ the compiler chain's scale the payload is megabytes, and it was emitted BETWEEN
\ the engine code and the baked source, so it counted against the ADR term: the
\ build died `icode: adr out of reach` at 29 sites with the engine code a tenth
\ of its allowance. Moving the payload after the source and addressing it with
\ TADR, took it out of every reach; it is a part of the image, so it is a term of
\ the window, and this is that term.
$5C0000 constant AOT-SECTION-CAP  \ src/habu/aot-decl.f AOT-SECTION:BYTES: the capture buffers ($5BC801) rounded to the 64 KiB grain
$EC0000 constant CODE-CAP-BYTES   \ ADR-HI ($100000) + IBUFSZ ($800000) + AOT-SECTION-CAP ($5C0000)
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

\ CODE and ICODE-TABS refine successful anonymous mappings into the byte-code
\ buffer and numeric label/fixup tables; syscall-result provenance is not inferred.
\ Retirement: habu-builder-trust-rows-c5d41af6.
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
\ fixups: site word-pos, next slot, kind (FX-B26 / FX-B19 / FX-ADR / FX-LOFF),
\ and one pending-chain head per label. Target identity is the owning FXH chain.
\ The kind selects how a resolved label is encoded into the branch/ADR/movw site.
\ It is validated before any fixup-chain, free-list, NFX, or code mutation so a
\ corrupt or internal invalid kind fails closed instead of silently patching ADR.
\ Three kinds carry a DELTA from the site; FX-LOFF carries the label's own byte
\ offset from the code base, so it is the one kind whose site position does not
\ enter its value.
0 constant FX-B26                              \ B/BL: 26-bit word delta
1 constant FX-B19                              \ cond/CBZ/CBNZ: 19-bit word delta
2 constant FX-ADR                              \ ADR: 21-bit byte delta
3 constant FX-LOFF                             \ movz/movk pair: label byte offset
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
   ?FX-KIND                                     \ reject invalid kind before any mutation
   FX-TAKE I-W !
   I-SITE @ I-W @ cells FXS + !
   I-KIND @ I-W @ cells FXK + !
   I-W @ FX-LINK ;

\ encode a word delta into the branch word for a kind
: D26 ( n -- n )  $3FFFFFF and ;                               \ B/BL: bits 0..25 (26-bit field)

: D19 ( n -- n )  $7FFFF and 5 lshift ;                         \ cond/CBZ: bits 5..23
\ signed-reach bounds: a relocation delta d is in range iff LO <= d < HI (LO
\ inclusive, HI exclusive — a two's-complement signed field). These mirror the
\ Gforth seed generator's within-based rejection in bootstrap/cg/asm.fs exactly
\ (?REL26/?REL19 on word deltas, ENC-ADR on the byte delta), so the native
\ single-pass assembler fails closed at the identical boundary the trusted seed
\ already rejects. The code window (CODE-CAP-BYTES) overshoots REL19/ADR reach —
\ it always did, and now that the window is ADR-HI + IBUFSZ it overshoots ADR by
\ exactly the boot source arena — so a forward or backward BCOND/CBZ/CBNZ/ADR,
\ and a large enough B/BL, must be range-checked before its delta is masked, or
\ the mask silently wraps to the wrong target. Every emit and patch site
\ validates before code mutation.
      -33554432 constant REL26-LO   \ -2^25 words, inclusive  (B/BL 26-bit field)
       33554432 constant REL26-HI   \  2^25 words, exclusive
        -262144 constant REL19-LO   \ -2^18 words, inclusive  (cond/CBZ/CBNZ 19-bit field)
         262144 constant REL19-HI   \  2^18 words, exclusive
       -1048576 constant ADR-LO     \ -2^20 bytes, inclusive  (ADR 21-bit field)
        1048576 constant ADR-HI     \  2^20 bytes, exclusive

: REL26-OK? ( n -- bool )  dup REL26-LO >=  swap REL26-HI <  and ;   \ B/BL word delta in reach?
: REL19-OK? ( n -- bool )  dup REL19-LO >=  swap REL19-HI <  and ;   \ cond/CBZ/CBNZ word delta in reach?
: ADR-OK?   ( n -- bool )  dup ADR-LO   >=  swap ADR-HI   <  and ;   \ ADR byte delta in reach?

: ?REL26 ( n -- n )  dup REL26-OK? if exit then  s" icode: branch out of reach" ICODE-EXIT-RC die ;

: ?REL19 ( n -- n )  dup REL19-OK? if exit then  s" icode: cond branch out of reach" ICODE-EXIT-RC die ;

: ?ADR ( n -- n )  dup ADR-OK? if exit then  s" icode: adr out of reach" ICODE-EXIT-RC die ;
\ label-offset bound: LOFF, below carries a label's byte offset from the code
\ base — a NON-NEGATIVE absolute position, not a signed delta around a site — in
\ the two 16-bit immediate lanes of a movz/movk pair. So its bound is that pair's
\ own capacity, and the distance from the reading site does not enter it at all.
\ That is the whole point of the form: the reader adds the offset to the code
\ base it holds at runtime, so a label the 21-bit ADR field cannot reach is still
\ addressable. The window must fit inside this field for the form to be total
\ over the buffer, and test/icode-fixup-test.f holds the two against each other.
$10 constant MOVW-LANE-BITS         \ one movz/movk immediate lane
2 constant LOFF-LANES               \ the pair LOFF, emits
0 constant LOFF-LO                  \ inclusive: a code-buffer byte offset is never negative
1 LOFF-LANES MOVW-LANE-BITS * lshift constant LOFF-HI   \ exclusive: 2^32 bytes

: LOFF-OK? ( n -- bool )  dup LOFF-LO >=  swap LOFF-HI <  and ;   \ label byte offset representable?

: ?LOFF ( n -- n )  dup LOFF-OK? if exit then  s" icode: label offset out of reach" ICODE-EXIT-RC die ;
\ emit a branch (base already encoded with delta=0) to a label; resolve or defer
variable BBASE  variable BKIND

: BR-EMIT ( label -- )
   LABEL>N I-LBL !                    \ BBASE/BKIND set; emits + records if fwd
   I-LBL @ cells LBLP + @  dup 0 < IF              \ pos on stack (0< isn't a standalone prim)
     drop  ASM-CP @ I-LBL @ >LABEL BKIND @ FX+  BBASE @ EMITW
   ELSE  ASM-CP @ -  BKIND @ FX-B26 = IF ?REL26 D26 ELSE ?REL19 D19 THEN  BBASE @ or EMITW  THEN ;

\ The base word of a branch is the asm.fs encoder applied to a zero delta —
\ the same shape ADR, below already uses. Going through the encoder keeps one
\ authority for each opcode's bit layout and for its operand refusals: the
\ condition code and the compare-and-branch register are bounded and screened
\ for the reserved register by ENC-BCOND and ENC-CBZ/ENC-CBNZ, before the
\ delta this layer computes is folded in.
: B, ( label -- )  0 ENC-B  BBASE !  FX-B26 BKIND !  BR-EMIT ;

: BL, ( label -- )  0 ENC-BL BBASE !  FX-B26 BKIND !  BR-EMIT ;

: BCOND, ( n label -- )
   LABEL>N I-LBL ! I-COND !  0 I-COND @ ENC-BCOND BBASE !  FX-B19 BKIND !  I-LBL @ >LABEL BR-EMIT ;

: CBZ, ( n label -- )
   LABEL>N I-LBL ! I-RD !  I-RD @ 0 ENC-CBZ BBASE !  FX-B19 BKIND !  I-LBL @ >LABEL BR-EMIT ;

: CBNZ, ( n label -- )
   LABEL>N I-LBL ! I-RD !  I-RD @ 0 ENC-CBNZ BBASE !  FX-B19 BKIND !  I-LBL @ >LABEL BR-EMIT ;

\ adr rd, label: PC-relative address (FX-ADR fixup when forward)
: ADR, ( n label -- )
   LABEL>N I-LBL ! I-RD !
   I-LBL @ cells LBLP + @ dup 0 < IF
     drop  ASM-CP @ I-LBL @ >LABEL FX-ADR FX+  I-RD @ 0 ENC-ADR EMITW
   ELSE  ASM-CP @ - $4 *  ?ADR  I-RD @ swap ENC-ADR EMITW  THEN ;

\ movz rd, #lo16 / movk rd, #hi16, lsl 16: rd = the label's byte offset from the
\ code base (FX-LOFF fixup when forward). The caller adds the base the running
\ image holds, which is what makes this the reach-free companion to ADR,.
\ The pair is ALWAYS two words wide, whichever side of the label the site is on,
\ because a forward site has to reserve the lanes before the offset is known and
\ a site's width may not depend on where it was emitted.
\ The zero-immediate base words come from the move-wide encoders for the same
\ reason ADR,'s does: one authority for the field layout, and the destination
\ register takes that encoder's own refusals before this layer folds a lane in.
: LOFF, ( n label -- )
   LABEL>N I-LBL ! I-RD !
   I-LBL @ cells LBLP + @ dup 0 < IF
     drop  ASM-CP @ I-LBL @ >LABEL FX-LOFF FX+
     I-RD @ 0 0 MOVZHW EMITW  I-RD @ 0 1 MOVKHW EMITW
   ELSE  $4 * ?LOFF I-X !
     I-RD @ I-X @ $FFFF and 0 MOVZHW EMITW
     I-RD @ I-X @ MOVW-LANE-BITS rshift $FFFF and 1 MOVKHW EMITW  THEN ;
\ define a label here; backpatch and reclaim only that label's pending chain
variable LBI

: FX-ENC ( n n -- n )                            \ ( delta kind -- patchbits )
   dup FX-B26 = if drop ?REL26 D26 exit then
   dup FX-B19 = if drop ?REL19 D19 exit then
   dup FX-ADR = if drop $4 * ?ADR ENC-ADRD exit then
   drop s" icode: invalid fixup kind" ICODE-EXIT-RC die ;

\ Deferred-patch reach is validated in FX-ENC before it returns any patch bits,
\ so a failed patch dies before PATCH writes the site word: the failing fixup's
\ code word and its FXS/FXK slot stay untouched (fully atomic for one fixup).
\ A label with a multi-fixup pending chain is not atomic across the whole chain:
\ LBL, binds the label and clears its FXH head, then walks the chain patching
\ from the head, so fixups patched before an out-of-reach one keep their new
\ deltas and are freed. The invariant is per-fixup: the die always precedes the
\ current fixup's PATCH write, never mid-write.
\ FX-LOFF is the one kind whose value is not a delta, so it binds from ASM-CP
\ alone and writes TWO site words. Validated before either PATCH, exactly like
\ the single-word kinds: the die precedes the first lane's write.
: FX-LOFF-PATCH ( -- )
   ASM-CP @ $4 * ?LOFF I-X !                     \ the label's byte offset from the code base
   0 I-X @ $FFFF and 0 MOVZHW  LBI @ cells FXS + @ PATCH
   0 I-X @ MOVW-LANE-BITS rshift $FFFF and 1 MOVKHW  LBI @ cells FXS + @ 1 + PATCH ;

: FX-PATCH ( -- )
   LBI @ cells FXK + @ FX-LOFF = if FX-LOFF-PATCH exit then
   ASM-CP @ LBI @ cells FXS + @ -                \ delta = here - site (words)
   LBI @ cells FXK + @ FX-ENC                    \ delta -> patch bits by validated kind (or die)
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

;using
