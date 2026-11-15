\ icode.f — A64ICODE, the ARM64 single-pass assembler: EMITW appends asm.f
\ encodings to the CODE window; labels resolve B/BL, B.cond, CBZ/CBNZ, ADR and
\ LOFF, sites, backward at once and forward by backpatch. ASM-CP counts words;
\ a branch or ADR distance reaches its asm.f encoder in bytes. Every label
\ reference, DLBL, cells included, and every BYTES, run is recorded until
\ ARESET; CUT removes code and re-encodes the references that stay.
require src/arch/arm64/asm.f
require src/core/layout-buffer.f
require src/core/dynamic-storage.f
require src/habu/layout.f
package A64ICODE
using A64ASM
public

\ ---- the window this assembler emits into ------------------------------------
\ ADR, reaches ADR-HI (1 MiB) from its site; past it FX-REACH dies `icode: adr out of reach`.
\ CODE is a demand-paged anonymous mapping taken on first use and returned by
\ ASM-RELEASE; nothing walks the span, so the window costs reserved address
\ space only. A B/BL across the window must fit REL26-HI words (128 MiB).
$1E00000 constant AOT-SECTION-CAP  \ aggregate payload budget, including framing/alignment
ADR-HI SOURCE-ARENA-CAP + AOT-SECTION-CAP + constant CODE-CAP-BYTES
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

private

\ One record per label reference: REF-SITES its first word, REF-LABELS LABEL>N
\ of its label, REF-KINDS its kind. ARESET empties them with the code buffer.
DYNAMIC-BUFFER REF-SITES n
DYNAMIC-BUFFER REF-LABELS n
DYNAMIC-BUFFER REF-KINDS n
variable NREF
\ One data span per BYTES, run: SPAN-FIRSTS its first word, SPAN-ENDS the word
\ after its padding. Control never passes from a span's words into code.
DYNAMIC-BUFFER SPAN-FIRSTS n
DYNAMIC-BUFFER SPAN-ENDS n
variable NSPAN
4 constant FX-CELL                  \ DLBL,: the label's byte offset as one cell; never pending
\ CUT's map: the new position of each word from CUT-FROM up to ASM-CP, -1 for
\ a cut word; ASM-CP's own cell holds the new end.
DYNAMIC-BUFFER CUT-MAP n

public

\ Stack only: BR-EMIT reads BKIND and BBASE after it. Callers add records in
\ ascending site order (REFS).
: REF+ ( n label n -- )                          \ ( site label kind -- )
   NREF @ 1 + REF-SITES-RESERVE
   NREF @ 1 + REF-LABELS-RESERVE
   NREF @ 1 + REF-KINDS-RESERVE
   NREF @ REF-KINDS !
   LABEL>N NREF @ REF-LABELS !
   NREF @ REF-SITES !
   NREF @ 1 + NREF ! ;

\ Callers add spans in ascending order, as references.
: DATA-SPAN+ ( n n -- )                          \ ( first end -- )
   NSPAN @ 1 + SPAN-FIRSTS-RESERVE
   NSPAN @ 1 + SPAN-ENDS-RESERVE
   NSPAN @ SPAN-ENDS !
   NSPAN @ SPAN-FIRSTS !
   NSPAN @ 1 + NSPAN ! ;

: ARESET ( -- )
   CODE drop
   ICODE-TABS drop
   0 ASM-CP !
   0 NREF !  0 NSPAN ! ;

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
\ Fixup rows: FXS site word, FXN next slot (pending chain or free list), FXK
\ kind; FXH[label] heads its pending chain. A kind is checked before any change.
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

private

: ICODE-UNMAP ( ptr a n -- )
   munmap 0 <> if s" icode: unmap failed" ICODE-EXIT-RC die then ;

: BUFFERS-RELEASE ( -- )
   LBLP-RELEASE FXH-RELEASE
   REF-SITES-RELEASE REF-LABELS-RELEASE REF-KINDS-RELEASE CUT-MAP-RELEASE
   SPAN-FIRSTS-RELEASE SPAN-ENDS-RELEASE
   0 NREF !  0 NSPAN !  0 NLBL !  0 NFX !  FX-INIT ;

public

: ASM-INIT ( -- ) ARESET BUFFERS-RELEASE ;

\ Unmaps the window and the tables the assembler fills; the next emit maps again.
: ASM-RELEASE ( -- )
   CODE-A @ NULL-PTR <> if
      CODE-A @ CODE-CAP-BYTES ICODE-UNMAP  NULL-PTR CODE-A !
   then
   ICODE-TAB-A @ NULL-PTR <> if
      ICODE-TAB-A @ ICODE-TAB-BYTES ICODE-UNMAP  NULL-PTR ICODE-TAB-A !
   then
   0 ASM-CP !  BUFFERS-RELEASE ;

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

\ Records ascend by site: each is appended at ASM-CP or, by an adopting link, in
\ the object's order, and CUT keeps their order below its new end.
: REFS ( -- n ) NREF @ ;
: REF-SITE ( n -- n ) REF-SITES @ ;
: REF-LABEL ( n -- label ) REF-LABELS @ >LABEL ;
: REF-KIND ( n -- n ) REF-KINDS @ ;

\ Data spans ascend and never overlap.
: DATA-SPANS ( -- n ) NSPAN @ ;
: DATA-SPAN-FIRST ( n -- n ) SPAN-FIRSTS @ ;
: DATA-SPAN-END ( n -- n ) SPAN-ENDS @ ;

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

\ ---- what a reach refusal names ----------------------------------------------
\ BR-EMIT, ADR, and FX-FILL set I-RSITE and I-RTARGET before FX-REACH, which
\ prints them in bytes when it refuses; FX-CLEAR's fixed distances always fit.
variable I-RSITE                    \ byte offset of the instruction under assembly
variable I-RTARGET                  \ byte offset it has to reach
variable I-RDELTA                   \ signed byte distance it would need
variable I-RLIMIT                   \ the field's exclusive bound, in bytes

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

private

variable I-RKIND                    \ the fixup kind whose field must hold I-RDELTA
variable I-RBITS                    \ the field bits FX-BITS packs for I-RDELTA

\ I-RKIND's encoder's word for I-RDELTA xor its word for 0, into I-RBITS: catch
\ admits only a stack-preserving word. b.cond, cbz and cbnz share one field.
: FX-BITS ( -- )
   I-RKIND @ FX-B26 = if I-RDELTA @ ENC-B  0 ENC-B xor I-RBITS ! exit then
   I-RKIND @ FX-B19 = if 0 I-RDELTA @ ENC-CBZ  0 0 ENC-CBZ xor I-RBITS ! exit then
   I-RKIND @ FX-ADR = if 0 I-RDELTA @ ENC-ADR  0 0 ENC-ADR xor I-RBITS ! exit then
   s" icode: invalid fixup kind" ICODE-EXIT-RC die ;


\ A distance the field cannot hold dies with REACH-DIE's report: E-A64EMIT-REACH
\ must not escape, since LBL, clears FXH before walking the chain FX-PATCH patches.
: FX-REACH ( n n -- n )                          \ ( bytes kind -- bits )
   I-RKIND !  I-RDELTA !
   ['] FX-BITS catch  dup 0= if drop I-RBITS @ exit then
   dup E-A64EMIT-REACH <> if throw then  drop
   I-RKIND @ FX-B26 = if
      REL26-HI $4 * I-RLIMIT !  s" icode: branch out of reach" REACH-DIE
   then
   I-RKIND @ FX-B19 = if
      REL19-HI $4 * I-RLIMIT !  s" icode: cond branch out of reach" REACH-DIE
   then
   ADR-HI I-RLIMIT !  s" icode: adr out of reach" REACH-DIE ;

public

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
   ASM-CP @ over BKIND @ REF+
   LABEL>N I-LBL !
   I-LBL @ LBLP @  dup 0 < IF              \ pos on stack (0< isn't a standalone prim)
     drop  ASM-CP @ I-LBL @ >LABEL BKIND @ FX+  BBASE @ EMITW
   ELSE  dup $4 * I-RTARGET !  ASM-CP @ $4 * I-RSITE !
     ASM-CP @ - $4 *  BKIND @ FX-REACH  BBASE @ or EMITW  THEN ;

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
   ASM-CP @ over FX-ADR REF+
   LABEL>N I-LBL ! I-RD !
   I-LBL @ LBLP @ dup 0 < IF
     drop  ASM-CP @ I-LBL @ >LABEL FX-ADR FX+  I-RD @ 0 ENC-ADR EMITW
   ELSE  dup $4 * I-RTARGET !  ASM-CP @ $4 * I-RSITE !
     ASM-CP @ - $4 *  FX-ADR FX-REACH  I-RD @ 0 ENC-ADR or EMITW  THEN ;


\ LOFF, is two words on either side of the label: a forward site reserves both
\ lanes before the offset is known, and a site's width may not depend on where
\ it was emitted. The base words come from MOVZHW and MOVKHW, as ADR,'s from
\ ENC-ADR.
: LOFF, ( n label -- )
   ASM-CP @ over FX-LOFF REF+
   LABEL>N I-LBL ! I-RD !
   I-LBL @ LBLP @ dup 0 < IF
     drop  ASM-CP @ I-LBL @ >LABEL FX-LOFF FX+
     I-RD @ 0 0 MOVZHW EMITW  I-RD @ 0 1 MOVKHW EMITW
   ELSE  $4 * ?LOFF I-X !
     I-RD @ I-X @ $FFFF and 0 MOVZHW EMITW
     I-RD @ I-X @ MOVW-LANE-BITS rshift $FFFF and 1 MOVKHW EMITW  THEN ;
variable LBI

private

variable FILL-KIND
variable FILL-SITE                  \ word position of the site's first word
variable FILL-TARGET                \ word position the site names

\ An FX-LOFF field holds the target's byte offset, not a delta, and both words
\ are patched after ?LOFF.
: FILL-LOFF ( -- )
   FILL-TARGET @ $4 * ?LOFF I-X !
   0 I-X @ $FFFF and 0 MOVZHW  FILL-SITE @ PATCH
   0 I-X @ MOVW-LANE-BITS rshift $FFFF and 1 MOVKHW  FILL-SITE @ 1 + PATCH ;

\ The target's byte offset, low 32 bits first, as DCQ, lays a cell.
: FILL-CELL ( -- )
   FILL-TARGET @ $4 * dup $FFFFFFFF and FILL-SITE @ PATCH
   $20 rshift FILL-SITE @ 1 + PATCH ;

\ A reach die precedes PATCH, so the failing site's words stay untouched.
: FX-FILL ( n n n -- )                           \ ( kind site target -- ), zero field
   FILL-TARGET ! FILL-SITE ! FILL-KIND !
   FILL-KIND @ FX-LOFF = if FILL-LOFF exit then
   FILL-KIND @ FX-CELL = if FILL-CELL exit then
   FILL-SITE @ $4 * I-RSITE !
   FILL-TARGET @ $4 * I-RTARGET !
   FILL-TARGET @ FILL-SITE @ - $4 *              \ bytes from the site to the target
   FILL-KIND @ FX-REACH
   FILL-SITE @ PATCH ;

: UNPATCH ( n n -- )                             \ ( mask pos -- ): clears the mask's bits
   I-N ! invert I-W !
   I-N @ CW@ EP !
   I-W @ $FF and EP@ c@ and EP@ c!
   I-W @ $8 rshift $FF and EP@ 1 CODE-BYTE+ c@ and EP@ 1 CODE-BYTE+ c!
   I-W @ $10 rshift $FF and EP@ 2 CODE-BYTE+ c@ and EP@ 2 CODE-BYTE+ c!
   I-W @ $18 rshift $FF and EP@ 3 CODE-BYTE+ c@ and EP@ 3 CODE-BYTE+ c! ;

\ A field's mask is its encoder's all-ones word xor its word for zero, as
\ FX-BITS derives bits: -4 bytes fills a branch field and -1 byte ADR's.
: FX-CLEAR ( n n -- )                            \ ( kind site -- )
   FILL-SITE ! FILL-KIND !
   FILL-KIND @ FX-LOFF = if
      0 $FFFF 0 MOVZHW  0 0 0 MOVZHW xor  FILL-SITE @ UNPATCH
      0 $FFFF 1 MOVKHW  0 0 1 MOVKHW xor  FILL-SITE @ 1 + UNPATCH  exit
   then
   FILL-KIND @ FX-CELL = if
      $FFFFFFFF FILL-SITE @ UNPATCH  $FFFFFFFF FILL-SITE @ 1 + UNPATCH  exit
   then
   FILL-KIND @ FX-ADR = if -1 FX-ADR FX-REACH FILL-SITE @ UNPATCH exit then
   -4 FILL-KIND @ FX-REACH FILL-SITE @ UNPATCH ;

public

\ A chain is not atomic: LBL, clears FXH first, and fixups patched before a
\ failing one keep their deltas and are freed.
: FX-PATCH ( -- )
   LBI @ cells FXK + @  LBI @ cells FXS + @  ASM-CP @  FX-FILL ;


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

private

variable CUT-FROM                   \ first word position of the region CUT edits
variable CUT-N                      \ kept range count
variable CUT-I
variable CUT-J
variable CUT-W
variable CUT-NEW                    \ next new word position
PTR-VARIABLE CUT-RANGES
PTR-VARIABLE CUT-SRC
: RANGES@ ( -- ptr n ) CUT-RANGES @ ;
: CUT-SRC@ ( -- ptr u8 ) CUT-SRC @ ;
: RANGE-START ( n -- n ) 2 * cells RANGES@ swap + @ ;
: RANGE-END ( n -- n ) 2 * 1 + cells RANGES@ swap + @ ;
: MAP-CELL ( n -- ptr n ) CUT-FROM @ - CUT-MAP ;
: REF-POS ( n -- n ) REF-LABELS @ LBLP @ ;      \ record's label position, -1 unbound

\ A position after the cut: itself outside [CUT-FROM, ASM-CP], else its map
\ cell, so an unbound label's -1 stays -1.
: >MOVED ( n -- n )
   dup CUT-FROM @ < if exit then
   dup ASM-CP @ > if exit then
   MAP-CELL @ ;

: SITE-KEPT? ( n -- bool ) >MOVED 0 >= ;

\ A range may start where the previous one ends.
: ?RANGES ( -- )
   CUT-N @ 0 <  CUT-FROM @ 0 < or if s" icode: cut ranges malformed" ICODE-EXIT-RC die then
   CUT-FROM @ CUT-W !                            \ the previous range's end
   0 CUT-I !
   BEGIN CUT-I @ CUT-N @ < WHILE
      CUT-I @ RANGE-START CUT-W @ <
      CUT-I @ RANGE-START CUT-I @ RANGE-END >= or
      if s" icode: cut ranges malformed" ICODE-EXIT-RC die then
      CUT-I @ RANGE-END CUT-W !
      CUT-I @ 1 + CUT-I !
   REPEAT
   CUT-W @ ASM-CP @ > if s" icode: cut ranges malformed" ICODE-EXIT-RC die then ;


: MAP-BUILD ( -- )
   ASM-CP @ CUT-FROM @ - 1 + CUT-MAP-RESERVE
   CUT-FROM @ CUT-W !
   BEGIN CUT-W @ ASM-CP @ < WHILE  -1 CUT-W @ MAP-CELL !  CUT-W @ 1 + CUT-W !  REPEAT
   CUT-FROM @ CUT-NEW !
   0 CUT-I !
   BEGIN CUT-I @ CUT-N @ < WHILE
      CUT-I @ RANGE-START CUT-W !
      BEGIN CUT-W @ CUT-I @ RANGE-END < WHILE
         CUT-NEW @ CUT-W @ MAP-CELL !
         CUT-NEW @ 1 + CUT-NEW !  CUT-W @ 1 + CUT-W !
      REPEAT
      CUT-I @ 1 + CUT-I !
   REPEAT
   CUT-NEW @ ASM-CP @ MAP-CELL ! ;


\ A LOFF pair (movz at the site, movk after it) and a DLBL, cell span two words.
: TWO-WORD? ( n -- bool ) REF-KINDS @ dup FX-LOFF = swap FX-CELL = or ;

\ A site's two words are split when only one is kept: kept words stay adjacent.
: PAIR-SPLIT? ( n -- bool ) dup SITE-KEPT? swap 1 + SITE-KEPT? xor ;

\ Refuses a pending reference sited in the region (FXS holds its old site), a
\ kept reference naming cut code and a cut that splits a two-word reference.
: ?REFS ( -- )
   0 CUT-I !
   BEGIN CUT-I @ NREF @ < WHILE
      CUT-I @ REF-SITES @ CUT-FROM @ >=  CUT-I @ REF-POS 0 <  and if
         s" icode: a pending reference lies in the cut region" ICODE-EXIT-RC die
      then
      CUT-I @ REF-SITES @ SITE-KEPT?  CUT-I @ REF-POS 0 >=  and
      CUT-I @ REF-POS >MOVED 0 <  and if
         s" icode: a kept reference names cut code" ICODE-EXIT-RC die
      then
      CUT-I @ TWO-WORD?  CUT-I @ REF-SITES @ PAIR-SPLIT?  and if
         s" icode: a cut splits a two-word reference" ICODE-EXIT-RC die
      then
      CUT-I @ 1 + CUT-I !
   REPEAT ;

: SPAN-LAST ( n -- n ) SPAN-ENDS @ 1 - ;

\ A cut keeps a span whole or removes it whole: its first and last words are
\ both kept, the same distance apart, or both cut.
: SPAN-SPLIT? ( n -- bool )
   {: k:n :}
   k SPAN-FIRSTS @ {: a:n :}
   k SPAN-LAST {: z:n :}
   a SITE-KEPT? z SITE-KEPT? xor if true exit then
   a SITE-KEPT? 0= if false exit then
   z >MOVED  a >MOVED -  z a -  <> ;

: ?SPANS ( -- )
   NSPAN @ 0 ?do
      i SPAN-SPLIT? if
         s" icode: a cut splits a data span" ICODE-EXIT-RC die
      then
   loop ;


: WORD-MOVE ( n n -- )                           \ ( from to -- ) word positions
   CW@ EP !  CW@ CUT-SRC !
   CUT-SRC@ c@ EP@ c!
   CUT-SRC@ 1 CODE-BYTE+ c@ EP@ 1 CODE-BYTE+ c!
   CUT-SRC@ 2 CODE-BYTE+ c@ EP@ 2 CODE-BYTE+ c!
   CUT-SRC@ 3 CODE-BYTE+ c@ EP@ 3 CODE-BYTE+ c! ;

\ Ascending order: every destination is at or below its source, so no word is
\ overwritten before it is read.
: CODE-MOVE ( -- )
   0 CUT-I !
   BEGIN CUT-I @ CUT-N @ < WHILE
      CUT-I @ RANGE-START CUT-W !
      BEGIN CUT-W @ CUT-I @ RANGE-END < WHILE
         CUT-W @ CUT-W @ MAP-CELL @ WORD-MOVE
         CUT-W @ 1 + CUT-W !
      REPEAT
      CUT-I @ 1 + CUT-I !
   REPEAT ;

\ Runs before LABEL-MOVE: REF-POS still reads a label's old position.
: REF-MOVE ( -- )
   0 CUT-I !  0 CUT-J !
   BEGIN CUT-I @ NREF @ < WHILE
      CUT-I @ REF-SITES @ SITE-KEPT? if
         CUT-I @ REF-SITES @ >MOVED CUT-J @ REF-SITES !
         CUT-I @ REF-LABELS @ CUT-J @ REF-LABELS !
         CUT-I @ REF-KINDS @ CUT-J @ REF-KINDS !
         CUT-J @ REF-POS 0 >= if
            CUT-J @ REF-KINDS @ CUT-J @ REF-SITES @ FX-CLEAR
            CUT-J @ REF-KINDS @ CUT-J @ REF-SITES @ CUT-J @ REF-POS >MOVED FX-FILL
         then
         CUT-J @ 1 + CUT-J !
      then
      CUT-I @ 1 + CUT-I !
   REPEAT
   CUT-J @ NREF ! ;

: SPAN-MOVE ( -- )
   0 CUT-J !
   NSPAN @ 0 ?do
      i SPAN-FIRSTS @ SITE-KEPT? if
         i SPAN-FIRSTS @ >MOVED CUT-J @ SPAN-FIRSTS !
         i SPAN-LAST >MOVED 1 + CUT-J @ SPAN-ENDS !
         CUT-J @ 1 + CUT-J !
      then
   loop
   CUT-J @ NSPAN ! ;

: LABEL-MOVE ( -- )
   0 CUT-I !
   BEGIN CUT-I @ NLBL @ < WHILE
      CUT-I @ LBLP @ >MOVED CUT-I @ LBLP !
      CUT-I @ 1 + CUT-I !
   REPEAT ;

public

\ CUT ( ranges count from -- ) keeps only the ranges' words of [from, ASM-CP),
\ moved down with their references and data spans; ranges: count ascending
\ [start, end) pairs.
: CUT ( ptr n n n -- )
   CUT-FROM ! CUT-N ! CUT-RANGES !
   ?RANGES  MAP-BUILD  ?REFS  ?SPANS
   CODE-MOVE  REF-MOVE  SPAN-MOVE  LABEL-MOVE
   ASM-CP @ MAP-CELL @ ASM-CP ! ;

\ --- data layer ---
: DCQ, ( n -- )
   dup $FFFFFFFF and EMITW  $20 rshift EMITW ;   \ one 64-bit cell, LE

: DLBL, ( label -- )                                  \ cell = label's byte offset
   dup LABEL>N LBLP @ 0 < if s" icode: DLBL forward ref" ICODE-EXIT-RC die then
   ASM-CP @ over FX-CELL REF+
   LABEL>N LBLP @ $4 * DCQ, ;
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

\ Each call is its own run, recorded as a data span: the bytes, then NUL pad to
\ the next word. A span written as one, such as a message and its newline, is
\ one call.
: BYTES, ( ptr u8 n -- )
   BYTES-ARGS
   BYTES-CAP
   BYTES-COPY
   BYTES-PAD
   ASM-CP @  BYP@ CODE - 4 /  dup ASM-CP !
   2dup < if DATA-SPAN+ else 2drop then ;
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
