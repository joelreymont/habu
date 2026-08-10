\ addrmap-set.f - the address-literal map primitive's contract.
\
\ `addrmap-set ( addr -- )` is the publisher's half of the address-literal map
\ (src/habu/layout.f SNAP-RELOC:ADDRMAP-OFF): it records the region word a
\ four-instruction MOVZ/MOVK chain starts in, so the two relocation passes can
\ find that chain again without ever decoding region bytes. The engine's own
\ C-CODE-ADDR writes the same map from inside the compiler; this primitive is how
\ a compiler written in Habu writes it, and src/compiler/native/publish.f is its
\ one caller.
\
\ WHAT THIS SUITE HAS TO SHOW, AND WHY EACH PART IS NOT ALREADY SHOWN ELSEWHERE.
\
\   1. That it sets THE bit and not A bit. The map is indexed by region byte
\      offset, and the index arithmetic - shift by five for the byte, mask the
\      word index by seven for the bit - is the whole primitive. An off-by-one in
\      either half sets a bit the relocation pass will read as a chain start that
\      is not one, which fails the image closed at ADDRMAP-RC and reports nothing
\      about the arithmetic. So the check is on the exact bit AND on its
\      neighbours in the same byte and in the byte on either side.
\   2. That it is idempotent. The publisher may record a site the engine already
\      recorded, and a map bit is a set membership, not a count.
\   3. That it writes the ADDRESS map and not the CALL map. The two bands sit
\      next to each other, are the same shape, and are indexed the same way, so a
\      wrong band constant produces a primitive that passes every bit-arithmetic
\      question above and silently relocates call sites as address chains. The
\      call-map bit under the same address must stay clear.
\   4. That an address it cannot index is a refusal and not a wild write. An
\      address below the region, at or past its end, or not on an instruction
\      boundary computes an offset that indexes past the map and into whatever
\      DATA band follows it. The engine exits SEAL-VIOLATION, which no in-process
\      catch can see, so those four cases run in child processes.
\
\ The positive cases address region words that are FREE code space - at and above
\ the code pointer, which no routine occupies - so the suite never marks a word
\ of a live routine, and it clears every bit it set before it returns.

require lib/errors.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f

package ADDRMAP-TEST

private

$800 constant CAP
10000 constant TIMEOUT-MS
ENGINE-ERROR:SEAL-VIOLATION constant VIOLATION-RC

create OUT CAP allot
create ERR CAP allot

\ ---- the boundary ------------------------------------------------------------
\ The primitive is admitted only through a trusted boundary (src/core/checker.f
\ PRIM-TRUSTED-ONLY!), so a suite that exercises it declares one, exactly as
\ src/compiler/native/publish.f does. It chooses nothing: the address is computed
\ by the checked words below.
TRUSTED: MARK ( n -- )
   addrmap-set ;

TRUSTED: DATA-A ( -- ptr u8 )
   data-base ;

TRUSTED: REGION-BASE ( -- n )
   dbase@ ;

\ ---- reading the two bands ---------------------------------------------------
\ Read exactly the way the primitive writes: the region byte offset of the
\ address, its map byte at offset >> 5, and its bit at (offset >> 2) & 7. Stated
\ once and given each band's base, so a reader cannot drift from the other.
: BAND-BIT@ ( n n -- n ) {: base:n at:n :}
   at REGION-BASE - {: off:n :}
   DATA-A base + off 5 rshift + c@
   off 2 rshift 7 and rshift 1 and ;

: ADDR-BIT@ ( n -- n )
   SNAP-RELOC:ADDRMAP-OFF swap BAND-BIT@ ;

: CALL-BIT@ ( n -- n )
   SNAP-RELOC:CALLMAP-OFF swap BAND-BIT@ ;

\ Clearing is not a primitive - nothing in the engine clears one bit of this band
\ - so the suite writes the byte back through the same DATA cast it reads with,
\ which is the only way to leave the band as it found it.
: ADDR-BIT-CLEAR ( n -- ) {: at:n :}
   at REGION-BASE - {: off:n :}
   DATA-A SNAP-RELOC:ADDRMAP-OFF + off 5 rshift + {: p:ptr :}
   p c@  1 off 2 rshift 7 and lshift  invert and  p c! ;

\ ---- the free words this suite is allowed to mark ----------------------------
\ The code pointer is the first free instruction slot, and everything above it is
\ unclaimed until something publishes there. Sixteen words is two map bytes plus
\ change, which is what question 1 needs to see a byte boundary crossed.
16 constant SPAN-WORDS

: SLOT ( n -- n ) {: i:n :}
   cp@ i 4 * + ;

: SPAN-CLEAR ( -- )
   SPAN-WORDS 0 ?do i SLOT ADDR-BIT-CLEAR loop ;

\ Every word of the span except the one named must read clear. This is the
\ neighbour question, and it is asked over a span wide enough to cover the byte
\ the bit lives in and the bytes on both sides of it.
: ONLY-BIT-SET ( n -- ) {: k:n :}
   SPAN-WORDS 0 ?do
      i SLOT ADDR-BIT@  i k = if 1 else 0 then  T=
   loop ;

\ ---- 1. it sets the bit it names, and only that one --------------------------
\ Word 0 is the first bit of a map byte, word 7 the last, word 8 the first bit of
\ the NEXT byte and word 9 an ordinary one inside it. Those four separate a byte
\ index computed with the wrong shift from one computed right, and a bit index
\ masked with the wrong width from one masked right.
: ONE-SITE ( n -- ) {: k:n :}
   SPAN-CLEAR
   k SLOT MARK
   k ONLY-BIT-SET
   SPAN-CLEAR ;

: TEST-EXACT ( -- )
   s" the first word of a map byte sets that byte's first bit" T-LABEL
   0 ONE-SITE
   s" the last word of a map byte sets that byte's last bit" T-LABEL
   7 ONE-SITE
   s" the next word sets the first bit of the next byte" T-LABEL
   8 ONE-SITE
   s" a word inside the second byte sets its own bit" T-LABEL
   9 ONE-SITE ;

\ ---- 2. recording a site twice records it once -------------------------------
: TEST-IDEMPOTENT ( -- )
   s" marking the same site twice leaves one bit set" T-LABEL
   SPAN-CLEAR
   3 SLOT MARK
   3 SLOT MARK
   3 ONLY-BIT-SET
   SPAN-CLEAR ;

\ ---- 3. it writes the address band and not the call band ---------------------
\ The call bit under the same address is read BEFORE and AFTER, because free code
\ space is not guaranteed to have a clear call bit - code-publish clears the map
\ over a span when it writes one, and nothing clears it above the pointer. What
\ the claim needs is that this primitive did not CHANGE it.
: TEST-BAND ( -- )
   s" the call map under the same address is untouched" T-LABEL
   SPAN-CLEAR
   5 SLOT CALL-BIT@ {: before:n :}
   5 SLOT MARK
   5 SLOT ADDR-BIT@ 1 T=
   5 SLOT CALL-BIT@ before T=
   SPAN-CLEAR ;

\ ---- 4. an address it cannot index is a refusal ------------------------------
\ These exit the engine, so each runs as a child program. The child declares its
\ own boundary because the primitive is trusted-only there too - which is itself
\ part of the claim: the guard being tested is the ENGINE's, reached through a
\ legitimate boundary, not the checker's refusal of an unwrapped call.
: CAPTURE ( ptr u8 n -- len len outcome ) {: src:ptr u:n :}
   src u OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN ;

: EXPECT ( ptr u8 n n -- ) {: src:ptr u:n want:n :}
   src u CAPTURE want T-OUTCOME-EXITED=
   LEN>N drop
   LEN>N drop ;

: REJECTS ( ptr u8 n -- )
   VIOLATION-RC EXPECT ;

: ACCEPTS ( ptr u8 n -- )
   0 EXPECT ;

: TEST-BOUNDS ( -- )
   s" an address below the region is refused" T-LABEL
   s" TRUSTED: AM ( n -- ) addrmap-set ; dbase@ 4 - AM" REJECTS
   s" an address at the region end is refused" T-LABEL
   s" TRUSTED: AM ( n -- ) addrmap-set ; dbase@ REGION + AM" REJECTS
   s" an address past the region end is refused" T-LABEL
   s" TRUSTED: AM ( n -- ) addrmap-set ; dbase@ REGION + $1000 + AM" REJECTS
   s" an address that is not a whole instruction is refused" T-LABEL
   s" TRUSTED: AM ( n -- ) addrmap-set ; cp@ 2 + AM" REJECTS
   s" the last word of the region is accepted" T-LABEL
   s" TRUSTED: AM ( n -- ) addrmap-set ; dbase@ REGION + 4 - AM" ACCEPTS ;

public

: RUN ( -- )
   T-RESET
   TEST-EXACT
   TEST-IDEMPOTENT
   TEST-BAND
   TEST-BOUNDS
   T-REPORT
   s" addrmap-set: ok" type cr ;

;package

ADDRMAP-TEST:RUN
